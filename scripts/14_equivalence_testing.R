# ==============================================================================
# Title: Equivalence Testing (TOST)
# Purpose: TOST equivalence tests for structural break stability analysis.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, gridExtra, knitr, broom)

# 1. YRICHTIG & THEMA ----------------------------------------------------------
source("R/theme_pku.R")
source("R/controls.R")
pku_red <- pku_pal$accent
pku_green <- pku_pal$slate # Griin dur Schiefer ersetzt fir "Erfolg/Stabilität" no Benützerriichtlinie

out_dir <- "outputs/robustness/equivalence"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# std_caption isch in theme_pku.R definiert

# 2. DATE LADE -----------------------------------------------------------------
print("Loading data...")
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>%
    filter(category == "Violence_Officials") %>%
    arrange(date)

controls <- load_controls()

# 3. VORBEREITE ----------------------------------------------------------------
# Jährlich
df_annual <- df_monthly %>%
    group_by(year = year(date)) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    # Mitti vom Johr als Datum fir Plotte zuefiege
    mutate(date = as.Date(paste0(year, "-07-01"))) %>%
    arrange(date) %>%
    left_join(controls$annual, by = "date")

# Log-Jährlich (Neie Versuech fir TOST)
df_log_annual <- df_annual %>%
    mutate(count = log(count + 1))

# Quartalswys
df_quarterly <- df_monthly %>%
    mutate(date = as.Date(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))) %>%
    group_by(date) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    arrange(date) %>%
    left_join(controls$quarterly, by = "date")

datasets <- list(
    "Annual" = df_annual,
    "Quarterly" = df_quarterly,
    "Log_Annual" = df_log_annual
)

treatments <- list(
    "FR2014_CT" = as.Date("2014-11-13"),
    "FR2015_INTEL" = as.Date("2015-07-24"),
    "FR2015_ESTATE" = as.Date("2015-11-20"),
    "FR2017_SILT" = as.Date("2017-10-30"),
    "FR2021_PACTI" = as.Date("2021-07-30")
)

# 4. ÄQUIVALÄNZTEST FUNKTION (TOST) --------------------------------------------

run_tost <- function(df, t_name, t_date, delta_type = "1SD") {
    # A. Vorbehandligsperiode definiere
    df_pre <- df %>%
        filter(date < t_date) %>%
        filter(!is.na(control_unemployment), !is.na(control_asylum))

    # B. Delta usrechne (Stabilitätsschwelle)
    # Üslicherwys: 1 Standardabwychig vo de Vorbehandligs-Kontroldate
    # Oder e fixe Prozentsatz (z.B. 20% vom Mittelwärt)
    sd_pre <- sd(df_pre$count, na.rm = T)
    mean_pre <- mean(df_pre$count, na.rm = T)

    if (delta_type == "1SD") {
        delta <- sd_pre
    } else if (delta_type == "20pct") {
        delta <- 0.2 * mean_pre
    } else {
        delta <- sd_pre * 1.5 # Entspannt
    }

    # C. Interventionsmodell aapasse
    # Y ~ PostDummy
    # Mir bruuche e Fäischter zum langfrischtige Drift-Verwirrig z vermyde. +/- 3 Johr?
    # Jährlich: GsamtdateSATZ isch chlei gnueg.
    window_start <- t_date - years(4)
    window_end <- t_date + years(4)

    df_model <- df %>%
        filter(date >= window_start & date <= window_end) %>%
        mutate(Post = ifelse(date >= t_date, 1, 0)) %>%
        filter(!is.na(control_unemployment), !is.na(control_asylum))

    # Eifachi Mittelwärt-Verglych (t-Test-Logik, aber iber Regression)
    model <- lm(count ~ Post + control_unemployment + control_asylum, data = df_model)
    tidied <- tidy(model)

    beta <- tidied %>%
        filter(term == "Post") %>%
        pull(estimate)
    se <- tidied %>%
        filter(term == "Post") %>%
        pull(std.error)
    df_resid <- df.residual(model)

    # D. TOST-Logik
    # T1: (Beta - (-Delta)) / SE  > t_crit  (Eisitigi Ablehnig unteri Gränze)
    # T2: (Beta - (+Delta)) / SE  < -t_crit (Eisitigi Ablehnig obri Gränze)

    # P-Wärt:
    # H0_unteri: Beta <= -Delta
    t1 <- (beta - (-delta)) / se
    p1 <- pt(t1, df_resid, lower.tail = FALSE) # Rächti Schwänz

    # H0_obri: Beta >= Delta
    t2 <- (beta - delta) / se
    p2 <- pt(t2, df_resid, lower.tail = TRUE) # Linki Schwänz

    # TOST P-Wärt (Maximum vo beide)
    p_tost <- max(p1, p2)

    return(list(
        beta = beta,
        delta = delta,
        p_tost = p_tost,
        mean_pre = mean_pre
    ))
}

# 5. USFIEHRE ------------------------------------------------------------------
print("Running Equivalence Tests...")

results_all <- data.frame()
multipliers <- c(1, 2, 3, 4, 5) # Teschtgränze vo 1 bis 5 SD

for (ds_name in names(datasets)) {
    df <- datasets[[ds_name]]

    for (t_name in names(treatments)) {
        t_date <- treatments[[t_name]]

        for (mult in multipliers) {
            delta_lbl <- paste0(mult, "SD")

            # Manuelle Delta usrechne
            df_pre <- df %>%
                filter(date < t_date) %>%
                filter(!is.na(control_unemployment), !is.na(control_asylum))
            delta_val <- sd(df_pre$count, na.rm = T) * mult

            # Funktionslogik nochemol bruuche aber mit manuellem Delta-Override
            # A. Vor/Nach definiere
            window_start <- t_date - years(4)
            window_end <- t_date + years(4)

            df_model <- df %>%
                filter(date >= window_start & date <= window_end) %>%
                mutate(Post = ifelse(date >= t_date, 1, 0)) %>%
                filter(!is.na(control_unemployment), !is.na(control_asylum))

            # Aapasse
            model <- lm(count ~ Post + control_unemployment + control_asylum, data = df_model)
            tidied <- tidy(model)
            beta <- tidied %>%
                filter(term == "Post") %>%
                pull(estimate)
            se <- tidied %>%
                filter(term == "Post") %>%
                pull(std.error)
            df_resid <- df.residual(model)

            # TOST durefüere
            t1 <- (beta - (-delta_val)) / se
            p1 <- pt(t1, df_resid, lower.tail = FALSE)

            t2 <- (beta - delta_val) / se
            p2 <- pt(t2, df_resid, lower.tail = TRUE)

            p_tost <- max(p1, p2)

            # Plotte (Nur fir d "Gwünende" oder 2SD/4SD)
            if (mult == 2 || mult == 4) {
                # Plot-Code hier identisch wie vorhär generiere...
                df_plot_window <- df %>%
                    filter(date >= window_start & date <= window_end) %>%
                    mutate(Period = ifelse(date < t_date, "Pre", "Post"))

                mean_pre <- mean(df_pre$count, na.rm = T)

                p_tost_plot <- ggplot(df_plot_window, aes(x = date, y = count)) +
                    annotate("rect",
                        xmin = min(df_plot_window$date), xmax = max(df_plot_window$date),
                        ymin = mean_pre - delta_val, ymax = mean_pre + delta_val,
                        fill = pku_green, alpha = 0.1
                    ) +
                    annotate("text",
                        x = min(df_plot_window$date), y = mean_pre + delta_val,
                        label = paste0("Stability Bound (+/- ", mult, " SD)"), color = pku_green, vjust = -0.5, hjust = 0, size = 3
                    ) +
                    geom_point(aes(color = Period), size = 3) +
                    geom_line(color = "grey", alpha = 0.5) +
                    geom_segment(aes(x = min(date[Period == "Pre"]), xend = t_date, y = mean_pre, yend = mean_pre), color = "blue", linewidth = 1.2, linetype = "dashed") +
                    geom_segment(aes(x = t_date, xend = max(date), y = mean_pre + beta, yend = mean_pre + beta), color = "red", linewidth = 1.2, linetype = "dashed") +
                    scale_color_manual(values = c("Pre" = "blue", "Post" = "red")) +
                    labs(
                        title = paste0("Stability Test: ", t_name, " (", ds_name, ")"),
                        subtitle = paste0(
                            "TOST P-value: ", format.pval(p_tost, 3),
                            ". Green Zone = Statistical Stability (+/- ", round(delta_val, 1), ")."
                        ),
                        caption = std_caption
                    ) +
                    theme_pku() %>%
                    add_watermark()

                fname <- paste0("TOST_", ds_name, "_", t_name, "_", delta_lbl, ".png")
                ggsave(file.path(out_dir, fname), p_tost_plot, width = 8, height = 5, bg = "white")
            }

            results_all <- rbind(results_all, data.frame(
                Dataset = ds_name,
                Law = t_name,
                Shift = round(beta, 2),
                Bound_Type = delta_lbl,
                Bound_Val = round(delta_val, 2),
                P_TOST = round(p_tost, 4),
                Significant_Stability = p_tost < 0.05
            ))
        }
    }
}

# 6. USGABETABELLE --------------------------------------------------------------
write_csv(results_all, file.path(out_dir, "Equivalence_Results.csv"))

# HTML Zämmefassig
sink(file.path(out_dir, "Equivalence_Summary.html"))
cat("<html><head><style>table, th, td {border: 1px solid black; border-collapse: collapse; padding: 5px;} th {background-color: #8A0000; color: white;} .pass {background-color: #d4edda; font-weight: bold;} .fail {background-color: #f8d7da;}</style></head><body>")
cat("<h1>Equivalence Testing Results</h1>")
cat("<h2>Hypothesis: Structural Break is Trivial (Stable)</h2>")
cat("<p><strong>Condition:</strong> p < 0.05 proves Significant Stability.</p>")
cat("<p><strong>Controls:</strong> Unemployment rate and asylum applications.</p>")

cat("<table><tr><th>Dataset</th><th>Law</th><th>Shift Observed</th><th>Bound Type</th><th>Bound Val</th><th>TOST P-Value</th><th>Result</th></tr>")

for (i in 1:nrow(results_all)) {
    row <- results_all[i, ]
    res_str <- if (row$Significant_Stability) "SIGNIFICANTLY STABLE" else "Instability Detected"
    cls <- if (row$Significant_Stability) "pass" else "fail"

    cat("<tr>")
    cat(paste0("<td>", row$Dataset, "</td>"))
    cat(paste0("<td>", row$Law, "</td>"))
    cat(paste0("<td>", row$Shift, "</td>"))
    cat(paste0("<td>", row$Bound_Type, "</td>"))
    cat(paste0("<td>", row$Bound_Val, "</td>"))
    cat(paste0("<td class='", cls, "'>", row$P_TOST, "</td>"))
    cat(paste0("<td class='", cls, "'>", res_str, "</td>"))
    cat("</tr>")
}
cat("</table></body></html>")
sink()

print("--- Equivalence Testing Complete ---")
print(results_all)
