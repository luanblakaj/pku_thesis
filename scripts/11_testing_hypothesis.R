# ==============================================================================
# Title: Hypothesis Testing and Structural Break Analysis
# Purpose: Formal structural break tests and treatment impact visualization.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, strucchange, broom, gridExtra, scales, zoo)

# 1. YRICHTIG & THEMA ----------------------------------------------------------
source("R/theme_pku.R")
# Alias fir Kompatibilitet
pku_red <- pku_pal$accent
pku_grey <- pku_pal$text_sec
pku_light <- pku_pal$grid

out_dir <- "outputs/tests"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# std_caption isch in theme_pku.R definiert

# 2. DATE LADE -----------------------------------------------------------------
print("Loading data...")

# A. Monetlich echt Date (Ziilvariable)
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) # Aapassig a d Studieperiode

# Hauptergebnis: Violence_Officials
df_target <- df_monthly %>%
    filter(category == "Violence_Officials") %>%
    arrange(date)

# B. Jehrlich synthetisch Date (Dr Counterfactual)
df_synthetic_annual <- read_csv("data/processed/scm_optimized_synthetic_france.csv", show_col_types = FALSE) %>%
    filter(Type == "Synthetic_France", category == "Violence_Officials") %>%
    dplyr::select(year, Count) %>%
    rename(Synthetic_Annual = Count) %>%
    mutate(Synthetic_Monthly_Avg = Synthetic_Annual / 12)

# Monetlichi Step-Version vom Synthetic fir Plotte/Vergleich erstelle
df_synthetic_monthly_step <- df_target %>%
    dplyr::select(date) %>%
    mutate(year = year(date)) %>%
    left_join(df_synthetic_annual, by = "year") %>%
    dplyr::select(date, Synthetic_Monthly_Avg)

# 3. BEHANDLIGSDATUM DEFINIERE -------------------------------------------------
treatments <- list(
    "FR2014_CT" = list(date = as.Date("2014-11-13"), name = "FR2014_CT", file_suffix = "FR2014_CT"),
    "FR2015_INTEL" = list(date = as.Date("2015-07-24"), name = "FR2015_INTEL", file_suffix = "FR2015_INTEL"),
    "FR2015_ESTATE" = list(date = as.Date("2015-11-20"), name = "FR2015_ESTATE", file_suffix = "FR2015_ESTATE"),
    "FR2017_SILT" = list(date = as.Date("2017-10-30"), name = "FR2017_SILT", file_suffix = "FR2017_SILT"),
    "FR2021_PTR" = list(date = as.Date("2021-07-30"), name = "FR2021_PTR", file_suffix = "FR2021_PTR")
)

# 4. CHART GENERIERIG: EINZELNI BEHANDLIGS-ZOOM --------------------------------
print("Generating Treatment Zoom Charts...")

# Hilfsfunktion zum Legend useläse
get_legend <- function(myggplot) {
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

generate_zoom_plot <- function(treatment_info, window_months = 24, minimal = FALSE) {
    t_date <- treatment_info$date
    t_name <- treatment_info$name

    start_date <- t_date %m-% months(window_months)
    end_date <- t_date %m+% months(window_months)

    # Date filtere
    plot_data <- df_target %>%
        filter(date >= start_date & date <= end_date) %>%
        left_join(df_synthetic_monthly_step, by = "date")

    # Eifachi Statischtik fir d Bschriibig usrechne
    avg_pre <- mean(plot_data$count[plot_data$date < t_date], na.rm = TRUE)
    avg_post <- mean(plot_data$count[plot_data$date >= t_date], na.rm = TRUE)
    delta_pct <- (avg_post - avg_pre) / avg_pre * 100

    # Formatierig fir minimal vs standard
    date_fmt <- if (minimal) "%b '%y" else "%b %Y"
    date_brk <- if (minimal) "1 year" else "6 months"
    cap_txt <- if (minimal) NULL else std_caption

    # Titel- und Untertitel-Logik
    if (minimal) {
        plot_title <- t_name
        plot_subtitle <- paste0("Change: ", round(delta_pct, 1), "%")
    } else {
        plot_title <- paste("Impact Assessment:", t_name)
        plot_subtitle <- paste0("Window: +/- ", window_months, " months. Change: ", round(delta_pct, 1), "%")
    }

    p <- ggplot(plot_data, aes(x = date)) +
        # Schattierig fir Post-Behandlig
        annotate("rect",
            xmin = t_date, xmax = end_date, ymin = -Inf, ymax = Inf,
            fill = pku_red, alpha = 0.05
        ) +

        # Synthetischi Step-Linie
        geom_step(aes(y = Synthetic_Monthly_Avg, color = "Counterfactual (SCM)"), linewidth = 1.2, linetype = "solid") +

        # Echti Date-Linie
        geom_line(aes(y = count, color = "Observed France"), linewidth = 1) +
        geom_point(aes(y = count, color = "Observed France"), size = 1.5) +

        # Behandligs-Linie
        geom_vline(xintercept = t_date, linetype = "dashed", color = pku_red, linewidth = 0.8) +
        annotate("text",
            x = t_date, y = max(plot_data$count) * 1.02,
            label = "Treatment",
            angle = 0, vjust = -0.5, fontface = "bold"
        ) +
        scale_color_manual(values = c("Observed France" = pku_red, "Counterfactual (SCM)" = pku_grey)) +
        scale_x_date(date_labels = date_fmt, date_breaks = date_brk) +
        labs(
            title = plot_title,
            subtitle = plot_subtitle,
            y = if (minimal) "Monthly Incidents" else "Monthly Incidents (Assaults on State Authority)",
            x = "",
            caption = cap_txt
        ) +
        theme_pku() +
        theme(legend.position = "bottom")

    if (minimal) {
        p <- p + theme(legend.position = "none")
    } else {
        p <- p %>% add_watermark()
    }

    return(p)
}

plot_list <- list()
# Variable zum Legend spichere
shared_legend <- NULL

for (key in names(treatments)) {
    # Voll Version fir einzelni Datei
    p_zoom <- generate_zoom_plot(treatments[[key]], minimal = FALSE)
    filename <- paste0("Hypothesis_Test_", key, ".png")
    ggsave(file.path(out_dir, filename), p_zoom, width = 10, height = 6, bg = "white")
    print(paste("Saved:", filename))

    # Legend usm erschte Plot useläse, falls no nit gmacht
    if (is.null(shared_legend)) {
        shared_legend <- get_legend(p_zoom)
    }

    # Minimal Version fir Panel
    p_zoom_min <- generate_zoom_plot(treatments[[key]], minimal = TRUE)
    plot_list[[key]] <- p_zoom_min
}

# Legend as 6. Elemänt zuefiege
plot_list[["Legend"]] <- shared_legend

# Multi-Panel Zämmefassig spichere
print("Generating Multi-Panel Summary Plot...")

# Header Grob mit korrektem Styling erstelle
title_grob <- grid::textGrob("Impact Assessments", gp = grid::gpar(fontsize = 22, fontface = "bold", col = pku_red, fontfamily = "Times New Roman"))
subtitle_grob <- grid::textGrob("Window: +/- 24 months", gp = grid::gpar(fontsize = 14, col = pku_grey, fontfamily = "Times New Roman"))
header_grob <- gridExtra::arrangeGrob(title_grob, subtitle_grob, nrow = 2, heights = grid::unit(c(1.5, 1), "lines"))

p_combined <- gridExtra::arrangeGrob(
    grobs = plot_list,
    ncol = 2,
    top = header_grob,
    bottom = grid::textGrob(std_caption, gp = grid::gpar(fontsize = 12, fontfamily = "Times New Roman", col = pku_grey))
)

ggsave(file.path(out_dir, "Hypothesis_Test_ALL_PANELS.png"), p_combined, width = 12, height = 14, bg = "white")
print("Saved: Hypothesis_Test_ALL_PANELS.png")

# 5. CHART GENERIERIG: IMPAKT VERGLEICH (LUCKENANALYSE) ------------------------
print("Generating Gap Analysis...")

df_gap <- df_target %>%
    left_join(df_synthetic_monthly_step, by = "date") %>%
    mutate(
        Gap = count - Synthetic_Monthly_Avg,
        Gap_Smooth = rollmean(Gap, k = 6, fill = NA, align = "right")
    )

p_gap <- ggplot(df_gap, aes(x = date, y = Gap)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_col(aes(fill = Gap > 0), width = 20, alpha = 0.6) +
    geom_line(aes(y = Gap_Smooth), color = "black", alpha = 0.4) +
    scale_fill_manual(values = c("FALSE" = "grey60", "TRUE" = pku_red), labels = c("Below Synthetic", "Above Synthetic")) +

    # Behandligs-Linien zuefiege
    geom_vline(xintercept = treatments$FR2014_CT$date, linetype = "dotted", color = "black") +
    annotate("text", x = treatments$FR2014_CT$date, y = max(df_gap$Gap, na.rm = T), label = "FR2014_CT", angle = 90, vjust = -0.5, size = 3) +
    geom_vline(xintercept = treatments$FR2015_INTEL$date, linetype = "dotted", color = "black") +
    annotate("text", x = treatments$FR2015_INTEL$date, y = max(df_gap$Gap, na.rm = T), label = "FR2015_INTEL", angle = 90, vjust = -0.5, size = 3) +
    geom_vline(xintercept = treatments$FR2015_ESTATE$date, linetype = "dotted", color = "black") +
    annotate("text", x = treatments$FR2015_ESTATE$date, y = max(df_gap$Gap, na.rm = T) * 0.9, label = "FR2015_ESTATE", angle = 90, vjust = -0.5, size = 3) +
    geom_vline(xintercept = treatments$FR2017_SILT$date, linetype = "dotted", color = "black") +
    annotate("text", x = treatments$FR2017_SILT$date, y = max(df_gap$Gap, na.rm = T), label = "FR2017_SILT", angle = 90, vjust = -0.5, size = 3) +
    geom_vline(xintercept = treatments$FR2021_PTR$date, linetype = "dotted", color = "black") +
    annotate("text", x = treatments$FR2021_PTR$date, y = max(df_gap$Gap, na.rm = T), label = "FR2021_PTR", angle = 90, vjust = -0.5, size = 3) +
    labs(
        title = "Structural Gap Analysis: Real vs Synthetic France",
        subtitle = "Positive bars (Red) indicate monthly violence exceeding the counterfactual model.",
        y = "Excess Incidents (Real - Synthetic)",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(out_dir, "Impact_vs_Synth_Gap.png"), p_gap, width = 12, height = 6, bg = "white")


# 6. STATISTISCHI HYPOTHESETEST (CHOW-TEST) ------------------------------------
print("Running Statistical Tests...")

# Usgab-Text formatiere
results_text <- c("statistical_results.txt")
sink(file.path(out_dir, "statistical_results.txt"))

cat("====================================================================\n")
cat("HYPOTHESIS TESTING: STRUCTURAL BREAKS IN MONTHLY VIOLENCE\n")
cat("Method: Chow Test (F-statistic) on Time Series\n")
cat("Outcome Variable: Monthly Assaults on State Authority\n")
cat("====================================================================\n\n")

run_chow_test <- function(t_date, label) {
    # Beobachtigsindex zum Datum usrechne
    # Aafang 2010-01.
    # index = (Johr - 2010)*12 + Monet
    yr <- year(t_date)
    mo <- month(t_date)

    break_month <- as.Date(paste0(yr, "-", sprintf("%02d", mo), "-01"))
    break_point <- match(break_month, df_target$date)

    if (is.na(break_point)) {
        cat(paste("Breakpoint not found for:", label, "\n"))
        return()
    }

    # Fstats usfiere
    # Mir teste, ob sich d Regressionskoeffiziente an dem Punkt ändere.
    # Eifachs Modell: y ~ t (Trendverschiebig) oder y ~ 1 (Mittelwärtverschiebig)
    # Mir teste fir Mittelwärtverschiebig (y ~ 1), also "Strukturbruch in dr Frequänz"

    # Dr Chow-Test iber Fstats isch rollend, sctest isch fir e spezifische Punkt.
    # Mir bruuche sctest (Structural Change Test) fir e bekannte Bruchpunkt.

    test_result <- sctest(
        count ~ 1,
        data = df_target,
        type = "Chow",
        point = break_point
    )

    cat(paste("--- TEST:", label, paste0("(", t_date, ")"), "---\n"))
    cat(paste("Breakpoint Index:", break_point, "\n"))
    cat(paste("F-statistic:", round(test_result$statistic, 4), "\n"))
    cat(paste("p-value:", format.pval(test_result$p.value, digits = 4), "\n"))

    sig <- if (test_result$p.value < 0.01) "*** (p<0.01)" else if (test_result$p.value < 0.05) "** (p<0.05)" else if (test_result$p.value < 0.1) "* (p<0.1)" else "Not Significant"

    cat(paste("Result:", sig, "\n"))
    cat("\n")
}

for (key in names(treatments)) {
    run_chow_test(treatments[[key]]$date, treatments[[key]]$name)
}

cat("====================================================================\n")
cat("INTERPRETATION GUIDE:\n")
cat("- Null Hypothesis (H0): No structural change at the specified date.\n")
cat("- significant p-value (<0.05) implies we reject H0 -> A structural break occurred.\n")
cat("====================================================================\n")

sink()
print("Statistical results saved to outputs/tests/statistical_results.txt")

print("--- 11_testing_hypothesis.R COMPLETE ---")
