# ==============================================================================
# Title: Extended Thesis Figures
# Purpose: Generate supplementary figures for event studies, spatial analysis, and SCM.
# Author: Luan Blakaj 齐凯
# ==============================================================================

# 1. YRICHTIG & THEME-DEFINITION ------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, extrafont, gridExtra, scales, zoo, patchwork)

# Versueche Schrifte z lade. Fälls nit goot, Standard-Serif.
# loadfonts(device = "pdf", quiet = TRUE)

source("R/theme_pku.R")
# Alias fir Kompatibilitet
pku_red <- pku_pal$accent
pku_grey <- pku_pal$text_sec
pku_light <- pku_pal$grid

# Standard-Bildtext isch jetz definiert in R/theme_pku.R

# Usgab-Verzeichnis sicherstelle
if (!dir.exists("outputs/figures_final")) dir.create("outputs/figures_final", recursive = TRUE)

# 2. DATE LADE -----------------------------------------------------------------
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE)
df_monthly_labeled <- df_monthly %>%
    mutate(category_label = category_display(category))
df_scm <- read_csv("data/processed/scm_master_dataset_annual.csv", show_col_types = FALSE)
df_dept <- read_csv("data/processed/departmental_panel_2010_2022_FULL.csv", show_col_types = FALSE)

# 3. VORVERARBETIG ------------------------------------------------------------

# Gsetz-Datume definiere
law_dates <- as.Date(c("2015-11-13", "2017-10-30", "2021-08-24"))
law_labels <- c("State of Emergency", "Loi SILT", "Separatism Law")

# Aggregiere fir "Politischi Gwalt" (Agriff uf Staatsgwalt + Brändstiftige + Agriff uf d Fundamäntalintärässe vo dr Nation)
df_monthly_agg <- df_monthly %>%
    filter(category %in% c("Violence_Officials", "Incendies", "Terrorism_AIFN")) %>%
    group_by(date) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Political Violence")

# 4. VISUALISIERIGS-SERIE ------------------------------------------------------
print("Generating Figures...")

# --- GRUPPE A: GsamTTREND ---

# Fig A1: Langfrischtige Trend (Alli Gsetz)
p_a1 <- ggplot(df_monthly_agg, aes(x = date, y = count)) +
    geom_line(color = pku_red, linewidth = 0.8) +
    geom_smooth(method = "loess", color = "black", linetype = "dashed", alpha = 0.2, span = 0.2) +
    geom_vline(xintercept = law_dates, linetype = "dotted", color = "black") +
    annotate("text",
        x = law_dates, y = max(df_monthly_agg$count) * 0.95,
        label = law_labels, angle = 90, vjust = -0.5, size = 3
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
        title = "Figure 1: Evolution of Political Violence (2010-2025)",
        subtitle = "Aggregated Monthly Count: Assaults on State Authority + Incendiary Attacks + Attacks on the Fundamental Interests of the Nation",
        y = "Monthly Incidents", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()
ggsave("outputs/figures_final/FigA1_Overall_Trend.png", p_a1, width = 9, height = 6, bg = "white")

# --- GRUPPE B: EVENT-STUDIE (Zoomed In) ---

# Funktion fir d Zoom-Ansicht
plot_zoom <- function(data, center_date, window_months, title_text) {
    start_date <- center_date - months(window_months)
    end_date <- center_date + months(window_months)

    ggplot(data, aes(x = date, y = count)) +
        geom_rect(aes(xmin = center_date, xmax = end_date, ymin = -Inf, ymax = Inf), fill = "#f7dede", alpha = 0.35) +
        geom_line(color = pku_red, linewidth = 1) +
        geom_point(size = 1, color = "black") +
        geom_vline(xintercept = center_date, linetype = "dashed") +
        scale_x_date(date_labels = "%b %Y", limits = c(start_date, end_date)) +
        labs(
            title = title_text,
            subtitle = paste0("+/- ", window_months, " Month Window"),
            y = "Incidents", x = "", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
}

# Fig B0: Zoom uf 2014 Anti-Terrorismus
p_b0 <- plot_zoom(df_monthly_agg, as.Date("2014-11-13"), 24, "Impact of FR2014_CT")
ggsave("outputs/figures_final/FigB0_Zoom_CT2014.png", p_b0, width = 8, height = 5, bg = "white")

# Fig B1: Zoom uf 2015 Notstand
p_b1 <- plot_zoom(df_monthly_agg, as.Date("2015-11-13"), 24, "Impact of FR2015_ESTATE")
ggsave("outputs/figures_final/FigB1_Zoom_Emergency2015.png", p_b1, width = 8, height = 5, bg = "white")

# Fig B1b: Zoom uf 2015 Nochrichte-Gsetz
p_b1b <- plot_zoom(df_monthly_agg, as.Date("2015-07-24"), 24, "Impact of FR2015_INTEL")
ggsave("outputs/figures_final/FigB1b_Zoom_Intel2015.png", p_b1b, width = 8, height = 5, bg = "white")

# Fig B2: Zoom uf 2017 SILT
p_b2 <- plot_zoom(df_monthly_agg, as.Date("2017-10-30"), 24, "Impact of FR2017_SILT")
ggsave("outputs/figures_final/FigB2_Zoom_SILT2017.png", p_b2, width = 8, height = 5, bg = "white")

# Fig B3: Zoom uf 2021
p_b3 <- plot_zoom(df_monthly_agg, as.Date("2021-08-24"), 24, "Impact of FR2021_PTR")
ggsave("outputs/figures_final/FigB3_Zoom_Separatism2021.png", p_b3, width = 8, height = 5, bg = "white")


# --- GRUPPE C: RÜÜMLIGI UFTAILIG (Paris vs Peripherii) ---

# Date vorberaite
df_spatial_paris <- df_dept %>%
    filter(category == "Violence_Officials") %>%
    mutate(Region = case_when(
        dept == "75" ~ "Paris (75)",
        dept %in% c("92", "93", "94") ~ "Petite Couronne (Suburbs)",
        TRUE ~ "Rest of France"
    )) %>%
    group_by(date, Region) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

# Fig C1: Paris vs Räscht (Absoluti Zaale)
p_c1 <- ggplot(df_spatial_paris, aes(x = date, y = count, color = Region)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Paris (75)" = "black", "Petite Couronne (Suburbs)" = pku_red, "Rest of France" = "grey70")) +
    labs(
        title = "Figure 3a: Spatial Displacement (Absolute Counts)",
        subtitle = "Comparing Capital, Suburbs, and Provinces (SSMSI Data)",
        y = "Monthly Incidents", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()
ggsave("outputs/figures_final/FigC1_Spatial_Absolute.png", p_c1, width = 9, height = 6, bg = "white")

# Fig C2: Paris vs Räscht (Indexiert 2010=100)
p_c2 <- df_spatial_paris %>%
    group_by(Region) %>%
    arrange(date) %>%
    mutate(index = (count / mean(count[year(date) == 2010], na.rm = TRUE)) * 100) %>%
    ggplot(aes(x = date, y = index, color = Region)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Paris (75)" = "black", "Petite Couronne (Suburbs)" = pku_red, "Rest of France" = "grey70")) +
    labs(
        title = "Figure 3b: Spatial Displacement (Indexed Growth)",
        subtitle = "Relative growth rates (2010 = 100). Did suburbs grow faster?",
        y = "Index (2010 Avg = 100)", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()
ggsave("outputs/figures_final/FigC2_Spatial_Indexed.png", p_c2, width = 9, height = 6, bg = "white")

# Fig C3: Rüümligi Konzentratioon (Paris vs Vorort)
p_c3 <- df_spatial_paris %>%
    filter(Region != "Rest of France") %>%
    ggplot(aes(x = date, y = count, fill = Region)) +
    geom_area(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = c("Paris (75)" = "black", "Petite Couronne (Suburbs)" = pku_red)) +
    labs(
        title = "Figure 3c: Spatial Concentration (Share of Violence)",
        subtitle = "Share of incidents: Paris City Center vs. Immediate Suburbs",
        y = "Proportion", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()
ggsave("outputs/figures_final/FigC3_Spatial_Share.png", p_c3, width = 9, height = 6, bg = "white")


# --- GRUPPE D: ROBUSTHET & MECHANISME ---

# Fig D1: Placebo (Gwalt vs Ybruch)
df_placebo <- df_monthly %>%
    filter(category %in% c("Violence_Officials", "Burglary")) %>%
    group_by(category) %>%
    arrange(date) %>%
    mutate(index = (count / first(count)) * 100) %>%
    ungroup() %>%
    mutate(category_label = category_display(category))

p_d1 <- ggplot(df_placebo, aes(x = date, y = index, color = category_label)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("Burglary" = "grey60", "Assaults on State Authority" = pku_red)) +
    labs(
        title = "Figure 4: The Specificity of Political Violence",
        subtitle = "Comparing Assaults on State Authority (Red) vs. Burglary (Grey). Indexed.",
        y = "Index", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()
ggsave("outputs/figures_final/FigD1_Placebo_Test.png", p_d1, width = 8, height = 5, bg = "white")

# Fig D2: Granular Ufbruch
p_d2 <- ggplot(df_monthly_labeled, aes(x = date, y = count)) +
    geom_line(color = pku_red) +
    facet_wrap(~category_label, scales = "free_y", ncol = 2) +
    labs(
        title = "Figure 5: Granular Breakdown by Category",
        subtitle = "Distinct trends across violence types.",
        y = "Count", x = "", caption = std_caption
    ) +
    theme_pku() +
    theme(strip.background = element_rect(fill = "grey95", color = NA)) %>%
    add_watermark()
ggsave("outputs/figures_final/FigD2_Category_Breakdown.png", p_d2, width = 10, height = 10, bg = "white")

# Fig D3: Saisonalitet
df_heatmap <- df_monthly %>%
    filter(category == "Violence_Officials") %>%
    mutate(Year = factor(year(date)), Month = month(date, label = TRUE, abbr = TRUE))

p_d3 <- ggplot(df_heatmap, aes(x = Month, y = Year, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = pku_red) +
    labs(title = "Figure 6: Seasonality Heatmap", subtitle = "Violence intensity by month/year", caption = std_caption) +
    theme_pku() %>%
    add_watermark()
ggsave("outputs/figures_final/FigD3_Seasonality.png", p_d3, width = 7, height = 8, bg = "white")

# --- GRUPPE E: SYNTHETISCHI KONTROLL-LUGGI ---
if (file.exists("data/processed/scm_optimized_synthetic_france.csv")) {
    df_synth_res <- read_csv("data/processed/scm_optimized_synthetic_france.csv", show_col_types = FALSE) %>%
        # Korrektur: Pivot zue breitem Format fir d Luggi z berächne
        pivot_wider(names_from = Type, values_from = Count)

    p_e1 <- ggplot(df_synth_res, aes(x = year)) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        geom_line(aes(y = France_Real - Synthetic_France), color = pku_red, linewidth = 1) +
        geom_ribbon(aes(ymin = 0, ymax = France_Real - Synthetic_France), fill = pku_red, alpha = 0.2) +
        geom_vline(xintercept = 2017, linetype = "dashed") +
        labs(
            title = "Figure 7: SCM Gap Analysis (Treatment Effect)",
            subtitle = "Difference between Real France and Synthetic Counterfactual (Annual)",
            y = "Excess Violence", x = "", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    ggsave("outputs/figures_final/FigE1_SCM_Gap.png", p_e1, width = 8, height = 5, bg = "white")
}

print("--- Extended Figure Generation Complete ---")
print("Check 'outputs/figures_final/' for figures.")
