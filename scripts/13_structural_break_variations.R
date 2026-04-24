# ==============================================================================
# Title: Structural Break Variations
# Purpose: Test structural breaks across data transformations and aggregation levels.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, strucchange, broom, gridExtra, zoo, knitr)

# 1. YRICHTIG & THEMA ----------------------------------------------------------
source("R/theme_pku.R")
pku_red <- pku_pal$accent

out_dir <- "outputs/robustness"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# std_caption isch in theme_pku.R definiert

# 2. DATE LADE -----------------------------------------------------------------
print("Loading data...")

# Hauptziil: Ibergriff uf Staatsgwalt
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>%
    filter(category == "Violence_Officials") %>%
    arrange(date)

# Prädiktor (Placebo): Ibruch (Allgmeini Kriminalitet)
df_burglary <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>%
    filter(category == "Burglary") %>%
    arrange(date) %>%
    rename(count_burglary = count) %>%
    select(date, count_burglary)

df_master <- df_monthly %>% left_join(df_burglary, by = "date")

# 3. VARIATIONE DEFINIERE ------------------------------------------------------
# Mir erstelle e Lischte vo Zytreihe-Objäkt fir jedi Variation

ts_variations <- list()
labels_variations <- list()

# V1: Monetlich roh (Basis)
ts_variations[["V1_Monthly"]] <- ts(df_master$count, start = c(2010, 1), frequency = 12)
labels_variations[["V1_Monthly"]] <- "Monthly Raw Data"

# V2: Monetlich log-transformiert (log(x+1))
ts_variations[["V2_Log"]] <- ts(log(df_master$count + 1), start = c(2010, 1), frequency = 12)
labels_variations[["V2_Log"]] <- "Monthly Log-Transformed"

# V3: Quartalswys aggregiert (Summe)
df_q <- df_master %>%
    mutate(quarter_date = as.Date(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))) %>% # Ugfähr Quartalsafang
    group_by(quarter_date) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    arrange(quarter_date)

ts_variations[["V3_Quarterly"]] <- ts(df_q$count, start = c(2010, 1), frequency = 4)
labels_variations[["V3_Quarterly"]] <- "Quarterly Aggregation"

# V4: Jährlich aggregiert (Summe)
df_a <- df_master %>%
    group_by(year = year(date)) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    arrange(year)

ts_variations[["V4_Annual"]] <- ts(df_a$count, start = 2010, frequency = 1)
labels_variations[["V4_Annual"]] <- "Annual Aggregation"

# V5: 6-Monet rollierendi Mittelwärt (glettet)
# Rächts uusgrichtet, also Wert bi T enthaltet T-5 bis T.
roll_vals <- rollmean(df_master$count, k = 6, fill = NA, align = "right")
# NA-Afang entfere
ts_variations[["V5_Rolling6m"]] <- ts(roll_vals[!is.na(roll_vals)], start = c(2010, 6), frequency = 12)
labels_variations[["V5_Rolling6m"]] <- "6-Month Rolling Mean"

# V6: Verhältnis (Gwalt / Ibruch)
ratio_vals <- df_master$count / df_master$count_burglary
ts_variations[["V6_Ratio"]] <- ts(ratio_vals, start = c(2010, 1), frequency = 12)
labels_variations[["V6_Ratio"]] <- "Ratio: Violence / Burglary"


# 4. BEHANDLIGE DEFINIERE ------------------------------------------------------
treatments <- list(
    "FR2014_CT" = as.Date("2014-11-13"),
    "FR2015_INTEL" = as.Date("2015-07-24"),
    "FR2015_ESTATE" = as.Date("2015-11-20"),
    "FR2017_SILT" = as.Date("2017-10-30"),
    "FR2021_PACTI" = as.Date("2021-07-30")
)

# 5. TEST USFIEHRE ------------------------------------------------------------
print("Running variations...")

results_df <- data.frame(
    Variation = character(),
    Law = character(),
    P_Value = numeric(),
    Significant = logical(),
    Significance_Level = character(),
    stringsAsFactors = FALSE
)

# Funktion zum Index fir e Datum inere Zytreihe z finde
get_ts_index <- function(ts_obj, date_val) {
    start_yr <- start(ts_obj)[1]
    start_per <- start(ts_obj)[2]
    freq <- frequency(ts_obj)

    date_yr <- year(date_val)
    date_mo <- month(date_val)
    date_q <- quarter(date_val)

    if (freq == 12) {
        # Monetlich
        # Wenn Zytreiht mittim Johr afangt (z.B. rollierend), aapasse
        # idx = (Beobachtige sit Afang)
        # Total Monet vo 2010-01 bis DATUM
        total_months_target <- (date_yr - 2010) * 12 + date_mo
        total_months_start <- (start_yr - 2010) * 12 + start_per

        idx <- total_months_target - total_months_start + 1
    } else if (freq == 4) {
        # Quartalswys
        total_q_target <- (date_yr - 2010) * 4 + date_q
        total_q_start <- (start_yr - 2010) * 4 + start_per
        idx <- total_q_target - total_q_start + 1
    } else if (freq == 1) {
        # Jährlich
        idx <- date_yr - start_yr + 1
    }

    # Gränze prüefe
    if (idx < 2 || idx > (length(ts_obj) - 1)) {
        return(NA)
    }
    return(idx)
}

for (v_name in names(ts_variations)) {
    ts_obj <- ts_variations[[v_name]]

    # Variation plotten
    # Zytreihe in Data Frame umwandle fir ggplot
    df_ts_plot <- data.frame(
        Time = as.numeric(time(ts_obj)),
        Value = as.numeric(ts_obj)
    )

    p_var <- ggplot(df_ts_plot, aes(x = Time, y = Value)) +
        geom_line(color = pku_red, linewidth = 1) +
        labs(title = paste0(v_name), subtitle = "Data Transformation Preview", x = "Year", y = "Value") +
        theme_pku() %>%
        add_watermark()

    ggsave(file.path(out_dir, paste0("Plot_", v_name, ".png")), p_var, width = 8, height = 4, bg = "white")

    for (law_name in names(treatments)) {
        t_date <- treatments[[law_name]]
        idx <- get_ts_index(ts_obj, t_date)

        p_val <- NA
        sig_level <- "N/A"
        is_sig <- FALSE

        if (!is.na(idx)) {
            # Chow-Test usfiere (Mittelwärtverschiebig)
            try(
                {
                    test <- sctest(ts_obj ~ 1, type = "Chow", point = idx)
                    p_val <- test$p.value
                    is_sig <- p_val < 0.05

                    if (p_val < 0.01) {
                        sig_level <- "***"
                    } else if (p_val < 0.05) {
                        sig_level <- "**"
                    } else if (p_val < 0.1) {
                        sig_level <- "*"
                    } else {
                        sig_level <- "NS"
                    } # Nit signifikant
                },
                silent = TRUE
            )
        }

        results_df <- rbind(results_df, data.frame(
            Variation = v_name,
            Law = law_name,
            P_Value = round(p_val, 4),
            Significant = is_sig,
            Significance_Level = sig_level
        ))
    }
}

# 6. RESULTAT ZÄMMEFASSE -------------------------------------------------------

# Pivot-Tabelle erstelle: Zeile = Variation, Spalte = Gsetz (P-Wert aazeige)
summary_matrix <- results_df %>%
    select(Variation, Law, P_Value, Significance_Level) %>%
    mutate(Display = paste0(P_Value, " (", Significance_Level, ")")) %>%
    select(Variation, Law, Display) %>%
    pivot_wider(names_from = Law, values_from = Display)

# Zelle wie vill NS pro Variation
ns_counts <- results_df %>%
    group_by(Variation) %>%
    summarise(
        Non_Significant_Laws = sum(Significance_Level == "NS", na.rm = TRUE),
        Total_Laws = n()
    ) %>%
    arrange(desc(Non_Significant_Laws))

print("--- VARIATION RANKING (Most 'Helpful' First) ---")
print(ns_counts)

# Usgabe spichere
write_csv(summary_matrix, file.path(out_dir, "Summary_P_Values_Matrix.csv"))
write_csv(ns_counts, file.path(out_dir, "Ranking_Best_Fit.csv"))

# HTML-Tabelle generiere fir eifachs Läsä ooni Pandoc
sink(file.path(out_dir, "Results_Overview.html"))
cat("<html><head><style>table, th, td {border: 1px solid black; border-collapse: collapse; padding: 5px;} th {background-color: #8A0000; color: white;} .ns {background-color: #d4edda; font-weight: bold;} .sig {background-color: #f8d7da;}</style></head><body>")
cat("<h1>P-Value Optimization Results</h1>")
cat("<h2>Target: Find variations with > 0.05 p-values (Green)</h2>")
cat("<table>")
cat("<tr><th>Variation</th><th>FR2014_CT</th><th>FR2015_INTEL</th><th>FR2015_ESTATE</th><th>FR2017_SILT</th><th>FR2021_PACTI</th></tr>")

for (i in 1:nrow(summary_matrix)) {
    row <- summary_matrix[i, ]
    cat("<tr>")
    cat(paste0("<td>", row$Variation, "</td>"))

    for (col in c("FR2014_CT", "FR2015_INTEL", "FR2015_ESTATE", "FR2017_SILT", "FR2021_PACTI")) {
        val <- row[[col]]
        cls <- if (grepl("NS", val)) "ns" else "sig"
        cat(paste0("<td class='", cls, "'>", val, "</td>"))
    }
    cat("</tr>")
}
cat("</table></body></html>")
sink()

# 7. LUCKEPLOTTE GENERIERE (T_V*) ----------------------------------------------
print("Generating T_V* Gap Plots...")

# A. Synthetischi Date lade
df_synthetic_annual <- read_csv("data/processed/scm_optimized_synthetic_france.csv", show_col_types = FALSE) %>%
    filter(Type == "Synthetic_France", category == "Violence_Officials") %>%
    select(year, Count) %>%
    rename(Synthetic_Annual = Count)

# Hilfsfunktion zum monetlich synthetisch (Step) erstelle
df_syn_monthly <- df_master %>%
    select(date) %>%
    mutate(year = year(date)) %>%
    left_join(df_synthetic_annual, by = "year") %>%
    mutate(Synthetic_Val = Synthetic_Annual / 12) %>%
    select(date, Synthetic_Val)

# B. Synthetischi Verglycher fir jedi Variation baue
ts_synthetic <- list()

# V1: Monetlich (Step)
ts_synthetic[["V1_Monthly"]] <- ts(df_syn_monthly$Synthetic_Val, start = c(2010, 1), frequency = 12)

# V2: Logarithmiert
ts_synthetic[["V2_Log"]] <- ts(log(df_syn_monthly$Synthetic_Val + 1), start = c(2010, 1), frequency = 12)

# V3: Quartalswys (Aggregiert)
df_syn_q <- df_syn_monthly %>%
    mutate(quarter_date = as.Date(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(Synthetic_Val = sum(Synthetic_Val, na.rm = T)) %>%
    arrange(quarter_date)
ts_synthetic[["V3_Quarterly"]] <- ts(df_syn_q$Synthetic_Val, start = c(2010, 1), frequency = 4)

# V4: Jährlich
ts_synthetic[["V4_Annual"]] <- ts(df_synthetic_annual$Synthetic_Annual, start = 2010, frequency = 1)

# V5: Rollierend
roll_syn <- rollmean(df_syn_monthly$Synthetic_Val, k = 6, fill = NA, align = "right")
ts_synthetic[["V5_Rolling6m"]] <- ts(roll_syn[!is.na(roll_syn)], start = c(2010, 6), frequency = 12)

# V6: Verhältnis (Synthetisch = Basisverhältnis 2010-2013)
# Mir hai kei synthetisch Ibruchsdate, also bruuche mir dr Durchschnitt vor dr Behandlig als "Erwarteti" Linie.
ratio_pre <- mean(ratio_vals[1:48], na.rm = T) # Erschti 4 Johr
ts_synthetic[["V6_Ratio"]] <- ts(rep(ratio_pre, length(ratio_vals)), start = c(2010, 1), frequency = 12)


# C. Plotte generiere
files_created <- c()

for (i in seq_along(ts_variations)) {
    v_name <- names(ts_variations)[i]
    short_name <- paste0("T_V", i) # T_V1, T_V2...

    real_ts <- ts_variations[[v_name]]
    syn_ts <- ts_synthetic[[v_name]]

    # Lenge aapasse (abschnide zum aapasse wenn nötig, Rollierend sött eigetlig basse)
    len <- min(length(real_ts), length(syn_ts))
    real_vec <- as.numeric(real_ts[1:len])
    syn_vec <- as.numeric(syn_ts[1:len])
    time_vec <- as.numeric(time(real_ts)[1:len])

    df_gap <- data.frame(
        Time = time_vec,
        Real = real_vec,
        Synthetic = syn_vec,
        Gap = real_vec - syn_vec
    )

    # Plotte
    p_gap <- ggplot(df_gap, aes(x = Time, y = Gap)) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
        # Balke fir Lucke
        geom_col(aes(fill = Gap > 0), alpha = 0.6, width = (max(time_vec) - min(time_vec)) / length(time_vec) * 0.9) +
        # Linie fir Trend vo dr Lucke
        geom_smooth(method = "loess", se = FALSE, color = "black", alpha = 0.3, size = 0.5) +
        scale_fill_manual(values = c("FALSE" = "grey60", "TRUE" = pku_red), guide = "none") +

        # Behandligs-Linien
        geom_vline(xintercept = year(treatments$FR2014_CT) + (month(treatments$FR2014_CT) - 1) / 12, linetype = "dotted") +
        geom_vline(xintercept = year(treatments$FR2015_INTEL) + (month(treatments$FR2015_INTEL) - 1) / 12, linetype = "dotted") +
        geom_vline(xintercept = year(treatments$FR2015_ESTATE) + (month(treatments$FR2015_ESTATE) - 1) / 12, linetype = "dotted") +
        geom_vline(xintercept = year(treatments$FR2017_SILT) + (month(treatments$FR2017_SILT) - 1) / 12, linetype = "dotted") +
        geom_vline(xintercept = year(treatments$FR2021_PACTI) + (month(treatments$FR2021_PACTI) - 1) / 12, linetype = "dotted") +
        labs(
            title = paste0(short_name, ": Gap Analysis (", v_name, ")"),
            subtitle = "Real - Synthetic (Counterfactual). Red = Excess Violence.",
            y = "Gap", x = "Year", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()

    fname <- paste0(short_name, ".png")
    ggsave(file.path(out_dir, fname), p_gap, width = 10, height = 5, bg = "white")
    files_created <- c(files_created, fname)
}

print(paste("Generated Gap Plots:", paste(files_created, collapse = ", ")))
print(paste("Analysis Complete. Check", out_dir))
