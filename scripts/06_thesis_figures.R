# ==============================================================================
# Title: Core Thesis Figures
# Purpose: Generate primary publication-quality figures for the thesis.
# Author: Luan Blakaj 齐凯
# ==============================================================================

# 1. YRICHTIG & THEME-DEFINITION ------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, extrafont, gridExtra, scales, zoo)

# Versueche Schrifte z lade (Times New Roman). Fälls nit goot, Standard-Serif.
# loadfonts(device = "pdf", quiet = TRUE)

source("R/theme_pku.R")
# Alias fir Kompatibilitet mit em bsthende Code
pku_red <- pku_pal$accent
pku_grey <- pku_pal$text_sec
pku_light <- pku_pal$grid

# Usgab-Verzeichnis sicherstelle
if (!dir.exists("outputs/figures_final")) dir.create("outputs/figures_final", recursive = TRUE)

# 2. DATE LADE -----------------------------------------------------------------
# Monetligi national Date
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE)
df_monthly_labeled <- df_monthly %>%
    mutate(category_label = category_display(category))

# Synthetischi Kontroll-Date (järlich)
df_scm <- read_csv("data/processed/scm_master_dataset_annual.csv", show_col_types = FALSE)

# Departemänt-Date
df_dept <- read_csv("data/processed/departmental_panel_2010_2022_FULL.csv", show_col_types = FALSE)

# 3. VORVERARBETIG FIR PLOT --------------------------------------------------

# Gsetz-Datume fir vertikali Linie definiere
law_dates <- as.Date(c("2015-11-13", "2017-10-30"))
law_labels <- c("State of Emergency", "Loi SILT")

# Aggregiere fir "Politischi Gwalt" (Agriff uf Staatsgwalt + Brändstiftige + Agriff uf d Fundamäntalintärässe vo dr Nation)
df_monthly_agg <- df_monthly %>%
    filter(category %in% c("Violence_Officials", "Incendies", "Terrorism_AIFN")) %>%
    group_by(date) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Political Violence")

# 4. VISUALISIERIGS-SERIE ------------------------------------------------------

print("Generating Figures...")

# --- FIG 1: Monetligi Trends mit gsetzlige Markierige ---
p1 <- ggplot(df_monthly_agg, aes(x = date, y = count)) +
    geom_line(color = pku_red, linewidth = 0.8) +
    geom_smooth(method = "loess", color = "black", linetype = "dashed", alpha = 0.2, span = 0.2) +
    geom_vline(xintercept = law_dates, linetype = "dotted", color = "black") +
    annotate("text",
        x = law_dates, y = max(df_monthly_agg$count) * 0.95,
        label = law_labels, angle = 90, vjust = -0.5, size = 3
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
        title = "Evolution of Political Violence in France (01.2010-08.2022)",
        y = "Monthly Incidents", x = "",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave("outputs/figures_final/Fig1_Overall_Trend_Laws.png", p1, width = 8, height = 5, bg = "white")


# --- FIG 2: Placebo-Test (Gwalt vs. Ybruch) ---
df_placebo <- df_monthly %>%
    filter(category %in% c("Violence_Officials", "Burglary")) %>%
    group_by(category) %>%
    arrange(date) %>%
    mutate(index = (count / first(count)) * 100) %>%
    ungroup() %>%
    mutate(category_label = category_display(category))

p2 <- ggplot(df_placebo, aes(x = date, y = index, color = category_label)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Burglary" = "grey60", "Assaults on State Authority" = pku_red)) +
    labs(
        title = "Figure 2: The Specificity of Political Violence",
        subtitle = "Comparing Assaults on State Authority (Red) vs. Burglary (Grey). Indexed 2010=100.",
        y = "Index (Jan 2010 = 100)", x = ""
    ) +
    theme_pku() %>%
    add_watermark()

ggsave("outputs/figures_final/Fig2_Placebo_Test.png", p2, width = 8, height = 5, bg = "white")


# --- FIG 3: Rüümligi Verlagrig (Paris vs. Peripherii) ---
df_spatial <- df_dept %>%
    filter(category == "Violence_Officials") %>%
    mutate(Region = ifelse(dept == "75", "Paris (Capital)", "Rest of France")) %>%
    group_by(date, Region) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(df_spatial, aes(x = date, y = count, color = Region)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Paris (Capital)" = "black", "Rest of France" = pku_red)) +
    facet_wrap(~Region, scales = "free_y", ncol = 1) +
    labs(
        title = "Figure 3: Spatial Displacement Analysis",
        subtitle = "Does violence drop in Paris but rise elsewhere? (Note different Y-axis scales)",
        y = "Monthly Incidents", x = ""
    ) +
    theme_pku() %>%
    add_watermark()

ggsave("outputs/figures_final/Fig3_Paris_vs_Rest.png", p3, width = 7, height = 7, bg = "white")


# --- FIG 4: Saisonalitets-Heatmap ---
df_heatmap <- df_monthly %>%
    filter(category == "Violence_Officials") %>%
    mutate(
        Year = factor(year(date)),
        Month = month(date, label = TRUE, abbr = TRUE)
    )

p4 <- ggplot(df_heatmap, aes(x = Month, y = Year, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = pku_red) +
    labs(
        title = "Figure 4: Seasonality of Assaults on State Authority",
        subtitle = "Heatmap of monthly intensity.",
        fill = "Incidents"
    ) +
    theme_pku() +
    theme(legend.position = "right") %>%
    add_watermark()

ggsave("outputs/figures_final/Fig4_Seasonality_Heatmap.png", p4, width = 6, height = 8, bg = "white")


# --- FIG 5: Synthetischi Luggi (Järlich) ---

# Luege, ob d optimiert Datei existiert
if (file.exists("data/processed/scm_optimized_synthetic_france.csv")) {
    df_synth_res <- read_csv("data/processed/scm_optimized_synthetic_france.csv", show_col_types = FALSE)

    # D Luggi plotte
    p5 <- ggplot(df_synth_res, aes(x = year)) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        geom_line(aes(y = France - Synthetic), color = pku_red, linewidth = 1) +
        geom_ribbon(aes(ymin = 0, ymax = France - Synthetic), fill = pku_red, alpha = 0.2) +
        geom_vline(xintercept = 2017, linetype = "dashed") +
        annotate("text",
            x = 2017.2, y = max(df_synth_res$France - df_synth_res$Synthetic) * 0.8,
            label = "Loi SILT", hjust = 0, family = "serif"
        ) +
        labs(
            title = "Figure 5: The 'Treatment Effect' (Gap Analysis)",
            subtitle = "Difference between Real France and Synthetic Counterfactual (Annual)",
            y = "Excess Violence (Real - Synthetic)", x = "Year"
        ) +
        theme_pku() %>%
        add_watermark()

    ggsave("outputs/figures_final/Fig5_SCM_Gap.png", p5, width = 8, height = 5, bg = "white")
}


# --- FIG 6: Kategorii-Ufbruch (Chlini Viilfach) ---
p6 <- ggplot(df_monthly_labeled, aes(x = date, y = count)) +
    geom_area(fill = pku_red, alpha = 0.6) +
    facet_wrap(~category_label, scales = "free_y", ncol = 2) +
    labs(
        title = "Figure 6: Breakdown by Violence Type",
        subtitle = "Trends across different categories of insecurity.",
        y = "Count", x = ""
    ) +
    theme_pku() +
    theme(strip.background = element_rect(fill = "grey90", color = NA)) %>%
    add_watermark()

ggsave("outputs/figures_final/Fig6_Category_Breakdown.png", p6, width = 10, height = 8, bg = "white")


print("--- Figure Generation Complete ---")
print("All figures saved in 'outputs/figures_final/'.")
