# ==============================================================================
# Title: Spatial Diagnostics
# Purpose: Descriptive spatial dispersion, target-shift, and core-periphery analysis.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (grepl("scripts$", getwd())) {
    setwd("..")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, readxl, maps, sf)

source("R/theme_pku.R")

# ------------------------------------------------------------------------------
# 0) CONFIG
# ------------------------------------------------------------------------------
out_fig <- "outputs/figures_final"
out_tab <- "outputs/figures_additional"
if (!dir.exists(out_fig)) dir.create(out_fig, recursive = TRUE)
if (!dir.exists(out_tab)) dir.create(out_tab, recursive = TRUE)

file_dept <- "data/processed/departmental_panel_2010_2022_FULL.csv"
file_serieschrono <- "data/serieschrono-datagouv.csv"
file_tableaux <- "data/tableaux-4001-ts.xlsx"
file_gtd <- "data/globalterrorismdb_FR_20102021.xlsx"

if (!file.exists(file_dept)) stop("Missing file: data/processed/departmental_panel_2010_2022_FULL.csv")
if (!file.exists(file_serieschrono)) stop("Missing file: data/serieschrono-datagouv.csv")
if (!file.exists(file_tableaux)) stop("Missing file: data/tableaux-4001-ts.xlsx")

focal_outcome <- "Violence_Officials"
analysis_start <- as.Date("2010-01-01")
analysis_end <- as.Date("2022-12-31")

treat_dates <- tibble(
    when = as.Date(c("2017-10-30", "2021-07-30")),
    label = c("FR2017_SILT", "FR2021_PTR")
)

hard_proxy_categories <- c("Violence_Officials", "Terrorism_AIFN", "Terrorism_Explosives")
soft_proxy_categories <- c("Incendies", "Destruction_Other")

# ------------------------------------------------------------------------------
# 1) HELPERS
# ------------------------------------------------------------------------------
save_plot <- function(plot_obj, file_name, width = 10.5, height = 6) {
    suppressWarnings(
        ggsave(
            filename = file.path(out_fig, file_name),
            plot = plot_obj,
            width = width,
            height = height,
            bg = "white"
        )
    )
}

to_quarter_start <- function(date_vec) {
    as.Date(paste0(year(date_vec), "-", sprintf("%02d", 3 * quarter(date_vec) - 2), "-01"))
}

normalize_text <- function(x) {
    x %>%
        str_to_lower() %>%
        iconv(from = "latin1", to = "ASCII//TRANSLIT") %>%
        str_replace_all("[^a-z]", "")
}

gini_coef <- function(x) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    n <- length(x)
    if (n == 0) return(NA_real_)
    sx <- sum(x, na.rm = TRUE)
    if (sx <= 0) return(0)
    x <- sort(x)
    i <- seq_len(n)
    (2 * sum(i * x) / (n * sx)) - (n + 1) / n
}

add_treatment_lines <- function() {
    geom_vline(data = treat_dates, aes(xintercept = when), linetype = "dashed", color = "black", linewidth = 0.6)
}

build_dept_centroids <- function(serieschrono_path) {
    france_map <- map_data("france")
    centroids_map <- france_map %>%
        group_by(region) %>%
        summarise(
            lon = mean(range(long, na.rm = TRUE)),
            lat = mean(range(lat, na.rm = TRUE)),
            .groups = "drop"
        ) %>%
        mutate(region_clean = normalize_text(region))

    sc_raw <- read_delim(
        serieschrono_path,
        delim = ";",
        locale = locale(encoding = "latin1"),
        show_col_types = FALSE
    )

    zone_col <- names(sc_raw)[str_detect(tolower(names(sc_raw)), "zone_geographique")]
    if (length(zone_col) == 0) {
        return(tibble(dept = character(), lon = numeric(), lat = numeric(), x_m = numeric(), y_m = numeric()))
    }

    dept_names <- sc_raw %>%
        transmute(zone = .data[[zone_col[1]]]) %>%
        filter(str_detect(zone, "^[0-9A-Z]{2,3}-")) %>%
        transmute(
            dept = str_extract(zone, "^[0-9A-Z]{2,3}"),
            dept_name = str_replace(zone, "^[0-9A-Z]{2,3}-", ""),
            dept_clean = normalize_text(dept_name)
        ) %>%
        distinct(dept, dept_clean)

    centroid_tbl <- dept_names %>%
        left_join(centroids_map, by = c("dept_clean" = "region_clean")) %>%
        filter(!is.na(lon), !is.na(lat)) %>%
        distinct(dept, lon, lat)

    if (nrow(centroid_tbl) == 0) return(tibble(dept = character(), lon = numeric(), lat = numeric(), x_m = numeric(), y_m = numeric()))

    pts_sf <- st_as_sf(centroid_tbl, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
        st_transform(2154)
    coords <- st_coordinates(pts_sf)

    centroid_tbl %>%
        mutate(
            x_m = coords[, 1],
            y_m = coords[, 2]
        )
}

# ------------------------------------------------------------------------------
# 2) LOAD DATA + OVERLAP DIAGNOSTIC
# ------------------------------------------------------------------------------
df_dept <- read_csv(file_dept, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= analysis_start, date <= analysis_end)

sc_raw <- read_delim(
    file_serieschrono,
    delim = ";",
    locale = locale(encoding = "latin1"),
    show_col_types = FALSE
)

month_cols_tableaux <- names(read_excel(file_tableaux, sheet = 1, n_max = 1))
month_cols_tableaux <- month_cols_tableaux[str_detect(month_cols_tableaux, "^_\\d{4}_\\d{2}$")]

overlap_diag <- tibble(
    source = c("processed_dept_panel", "serieschrono_raw", "tableaux_4001_raw"),
    min_date = c(
        min(df_dept$date, na.rm = TRUE),
        as.Date(paste0(min(str_extract(sc_raw$Unite_temps, "\\d{4}M\\d{2}"), na.rm = TRUE) %>% str_replace("M", "-"), "-01")),
        as.Date(paste0(str_extract(max(month_cols_tableaux), "\\d{4}_\\d{2}") %>% str_replace("_", "-"), "-01"))
    ),
    max_date = c(
        max(df_dept$date, na.rm = TRUE),
        as.Date(paste0(max(str_extract(sc_raw$Unite_temps, "\\d{4}M\\d{2}"), na.rm = TRUE) %>% str_replace("M", "-"), "-01")),
        as.Date(paste0(str_extract(min(month_cols_tableaux), "\\d{4}_\\d{2}") %>% str_replace("_", "-"), "-01"))
    ),
    note = c(
        "Harmonized panel used for analysis (same cleaning lineage as existing spatial figures).",
        "Raw SSMSI series (can overlap by period/indicator).",
        "Raw Etat 4001 workbook (department sheets)."
    )
)

write_csv(overlap_diag, file.path(out_tab, "Table6_SSMSI_Overlap_Diagnostic.csv"))

# ------------------------------------------------------------------------------
# 3) A) DISPERSION WITHOUT KDE (2010-2022)
# ------------------------------------------------------------------------------
df_disp_base <- df_dept %>%
    filter(category == focal_outcome) %>%
    group_by(date) %>%
    mutate(
        total_t = sum(count, na.rm = TRUE),
        s_it = if_else(total_t > 0, count / total_t, 0)
    ) %>%
    ungroup()

df_disp_monthly <- df_disp_base %>%
    group_by(date) %>%
    summarise(
        total_t = first(total_t),
        HHI_t = sum(s_it^2, na.rm = TRUE),
        N_eff_t = if_else(HHI_t > 0, 1 / HHI_t, NA_real_),
        Entropy_t = -sum(if_else(s_it > 0, s_it * log(s_it), 0), na.rm = TRUE),
        Gini_t = gini_coef(count),
        .groups = "drop"
    )

centroids_tbl <- build_dept_centroids(file_serieschrono)
write_csv(centroids_tbl, file.path(out_tab, "Table6_Dept_Centroids_Used.csv"))

df_spread_monthly <- df_disp_base %>%
    inner_join(centroids_tbl, by = "dept") %>%
    group_by(date) %>%
    mutate(
        mapped_total_t = sum(count, na.rm = TRUE),
        s_map = if_else(mapped_total_t > 0, count / mapped_total_t, 0),
        xbar = sum(s_map * x_m, na.rm = TRUE),
        ybar = sum(s_map * y_m, na.rm = TRUE)
    ) %>%
    summarise(
        SD_m_t = sqrt(sum(s_map * ((x_m - xbar)^2 + (y_m - ybar)^2), na.rm = TRUE)),
        mapped_total_t = first(mapped_total_t),
        .groups = "drop"
    ) %>%
    left_join(df_disp_monthly %>% select(date, total_t), by = "date") %>%
    mutate(
        SD_km_t = SD_m_t / 1000,
        mapped_share_of_total = if_else(total_t > 0, mapped_total_t / total_t, NA_real_)
    )

disp_monthly <- df_disp_monthly %>%
    left_join(df_spread_monthly %>% select(date, SD_km_t, mapped_share_of_total), by = "date")

disp_quarterly <- disp_monthly %>%
    mutate(q_date = to_quarter_start(date)) %>%
    group_by(q_date) %>%
    summarise(
        HHI_t = mean(HHI_t, na.rm = TRUE),
        N_eff_t = mean(N_eff_t, na.rm = TRUE),
        Entropy_t = mean(Entropy_t, na.rm = TRUE),
        Gini_t = mean(Gini_t, na.rm = TRUE),
        SD_km_t = mean(SD_km_t, na.rm = TRUE),
        mapped_share_of_total = mean(mapped_share_of_total, na.rm = TRUE),
        .groups = "drop"
    )

write_csv(disp_monthly, file.path(out_tab, "Table6_Dispersion_Indices_Monthly.csv"))
write_csv(disp_quarterly, file.path(out_tab, "Table6_Dispersion_Indices_Quarterly.csv"))

p_hhi <- ggplot(disp_quarterly, aes(x = q_date, y = HHI_t)) +
    geom_line(color = pku_pal$accent, linewidth = 1.1) +
    add_treatment_lines() +
    labs(
        title = "Departmental Concentration (HHI), 2010-2022",
        subtitle = "Quarterly-smoothed descriptive concentration index of the focal outcome.",
        x = "",
        y = "HHI",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

save_plot(p_hhi, "Fig6_Dispersion_HHI.png", width = 10.5, height = 5.8)
save_plot(p_hhi, "Fig_New_B5_Dispersion_HHI.png", width = 10.5, height = 5.8)

p_sd <- ggplot(disp_quarterly, aes(x = q_date, y = SD_km_t)) +
    geom_line(color = pku_pal$navy, linewidth = 1.1) +
    add_treatment_lines() +
    labs(
        title = "Spatial Spread (Weighted SD), 2010-2022",
        subtitle = "Quarterly-smoothed dispersion radius using department centroids (EPSG:2154, km).",
        x = "",
        y = "Weighted Spatial SD (km)",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

save_plot(p_sd, "Fig6_Dispersion_SD.png", width = 10.5, height = 5.8)
save_plot(p_sd, "Fig_New_B6_Dispersion_SD.png", width = 10.5, height = 5.8)

# ------------------------------------------------------------------------------
# 4) B) TARGET SHIFT WITH LIMITED EVENT DATA
# ------------------------------------------------------------------------------
available_categories <- sort(unique(df_dept$category))
can_run_b1 <- length(intersect(hard_proxy_categories, available_categories)) > 0 &&
    length(intersect(soft_proxy_categories, available_categories)) > 0

if (can_run_b1) {
    df_target_base <- df_dept %>%
        filter(category %in% c(hard_proxy_categories, soft_proxy_categories)) %>%
        mutate(target_class = if_else(category %in% hard_proxy_categories, "Hard", "Soft"))

    # National
    target_monthly <- df_target_base %>%
        group_by(date, target_class) %>%
        summarise(y = sum(count, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = target_class, values_from = y, values_fill = 0) %>%
        mutate(
            TSR_t = if_else((Hard + Soft) > 0, Hard / (Hard + Soft), NA_real_),
            R_t = if_else(Soft > 0, Hard / Soft, NA_real_)
        )

    target_quarterly <- target_monthly %>%
        mutate(q_date = to_quarter_start(date)) %>%
        group_by(q_date) %>%
        summarise(
            Hard = sum(Hard, na.rm = TRUE),
            Soft = sum(Soft, na.rm = TRUE),
            TSR_t = if_else((Hard + Soft) > 0, Hard / (Hard + Soft), NA_real_),
            R_t = if_else(Soft > 0, Hard / Soft, NA_real_),
            .groups = "drop"
        )

    write_csv(target_monthly, file.path(out_tab, "Table6_TargetShift_SSMSI_Monthly.csv"))
    write_csv(target_quarterly, file.path(out_tab, "Table6_TargetShift_SSMSI_Quarterly.csv"))

    p_tsr <- ggplot(target_quarterly, aes(x = q_date, y = TSR_t)) +
        geom_line(color = pku_pal$accent, linewidth = 1.1) +
        add_treatment_lines() +
        labs(
            title = "Target-Shift Ratio (SSMSI Proxy), 2010-2022",
            subtitle = "Hard / (Hard + Soft) using category-based SSMSI composition proxy (descriptive).",
            x = "",
            y = "TSR",
            caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()

    save_plot(p_tsr, "Fig6_TargetShift_TSR.png", width = 10.5, height = 5.8)

    # Regional split (Paris, Petite Couronne, Rest)
    target_region_monthly <- df_target_base %>%
        mutate(
            region = case_when(
                dept == "75" ~ "Paris (75)",
                dept %in% c("92", "93", "94") ~ "Petite Couronne (92/93/94)",
                TRUE ~ "Rest of France"
            )
        ) %>%
        group_by(date, region, target_class) %>%
        summarise(y = sum(count, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = target_class, values_from = y, values_fill = 0) %>%
        mutate(TSR_t = if_else((Hard + Soft) > 0, Hard / (Hard + Soft), NA_real_))

    target_region_quarterly <- target_region_monthly %>%
        mutate(q_date = to_quarter_start(date)) %>%
        group_by(q_date, region) %>%
        summarise(
            Hard = sum(Hard, na.rm = TRUE),
            Soft = sum(Soft, na.rm = TRUE),
            TSR_t = if_else((Hard + Soft) > 0, Hard / (Hard + Soft), NA_real_),
            .groups = "drop"
        )

    write_csv(target_region_monthly, file.path(out_tab, "Table6_TargetShift_SSMSI_Region_Monthly.csv"))
    write_csv(target_region_quarterly, file.path(out_tab, "Table6_TargetShift_SSMSI_Region_Quarterly.csv"))

    p_tsr_region <- ggplot(target_region_quarterly, aes(x = q_date, y = TSR_t, color = region)) +
        geom_line(linewidth = 1.0) +
        add_treatment_lines() +
        scale_color_manual(values = c(
            "Paris (75)" = "black",
            "Petite Couronne (92/93/94)" = pku_pal$accent,
            "Rest of France" = pku_pal$mono[2]
        )) +
        labs(
            title = "Core-Periphery Target Balance (TSR Proxy)",
            subtitle = "Descriptive regional split: Paris, Petite Couronne, and Rest of France.",
            x = "",
            y = "TSR",
            color = "",
            caption = std_caption
        ) +
        theme_pku() +
        theme(legend.position = "bottom") %>%
        add_watermark()

    save_plot(p_tsr_region, "Fig6_TargetShift_TSR_Regions.png", width = 11, height = 6)

    target_strategy_note <- "B1 (SSMSI composition proxy) executed."
} else {
    # B2 fallback: GTD target bins (annual)
    if (!file.exists(file_gtd)) stop("B1 unavailable and GTD fallback file missing.")

    gtd <- read_excel(file_gtd) %>%
        mutate(
            year = as.integer(iyear),
            target = as.character(targtype1_txt)
        ) %>%
        filter(year >= 2010, year <= 2022)

    hard_bins <- c(
        "Government (General)", "Police", "Military", "Government (Diplomatic)",
        "Utilities", "Airports & Aircraft", "Transportation"
    )
    soft_bins <- c(
        "Private Citizens & Property", "Business", "Educational Institution",
        "Religious Figures/Institutions", "Tourists", "Journalists & Media", "NGO"
    )

    gtd_year <- gtd %>%
        mutate(
            target_class = case_when(
                target %in% hard_bins ~ "Hard",
                target %in% soft_bins ~ "Soft",
                TRUE ~ "Other"
            )
        ) %>%
        filter(target_class %in% c("Hard", "Soft")) %>%
        count(year, target_class, name = "n") %>%
        tidyr::pivot_wider(names_from = target_class, values_from = n, values_fill = 0) %>%
        mutate(
            TSR_year = if_else((Hard + Soft) > 0, Hard / (Hard + Soft), NA_real_)
        )

    gtd_pre_post <- gtd_year %>%
        mutate(period = if_else(year <= 2016, "Pre-SILT (2010-2016)", "Post-SILT (2017-2022)")) %>%
        group_by(period) %>%
        summarise(
            Hard = sum(Hard, na.rm = TRUE),
            Soft = sum(Soft, na.rm = TRUE),
            TSR = if_else((Hard + Soft) > 0, Hard / (Hard + Soft), NA_real_),
            .groups = "drop"
        )

    write_csv(gtd_year, file.path(out_tab, "Table6_TargetShift_GTD_Annual.csv"))
    write_csv(gtd_pre_post, file.path(out_tab, "Table6_TargetShift_GTD_PrePost.csv"))

    p_gtd <- ggplot(gtd_year, aes(x = year, y = TSR_year)) +
        geom_line(color = pku_pal$accent, linewidth = 1.1) +
        geom_vline(xintercept = c(2017, 2021), linetype = "dashed", color = "black", linewidth = 0.6) +
        scale_x_continuous(breaks = seq(2010, 2022, by = 1)) +
        labs(
            title = "Target-Shift Ratio (GTD Annual, Fallback)",
            subtitle = "Sparse annual corroborative pattern only; descriptive interpretation.",
            x = "Year",
            y = "TSR",
            caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()

    save_plot(p_gtd, "Fig6_TargetShift_GTD_Annual.png", width = 10.5, height = 5.8)
    target_strategy_note <- "B2 (GTD fallback) executed."
}

# ------------------------------------------------------------------------------
# 5) C) UPDATE DONUT RATIO FIGURE (FR2017_SILT + FR2021_PTR)
# ------------------------------------------------------------------------------
df_donut_q <- df_dept %>%
    filter(category == focal_outcome) %>%
    mutate(
        zone = case_when(
            dept == "75" ~ "Center (75)",
            dept %in% c("92", "93", "94") ~ "Ring (92-94)",
            TRUE ~ "Other"
        )
    ) %>%
    filter(zone != "Other") %>%
    group_by(date, zone) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(q_date = to_quarter_start(date)) %>%
    group_by(q_date, zone) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = zone, values_from = count, values_fill = 0) %>%
    mutate(
        Ratio = if_else(`Center (75)` > 0, `Ring (92-94)` / `Center (75)`, NA_real_),
        year = year(q_date)
    )

mean_pre <- df_donut_q %>% filter(year >= 2010, year <= 2016) %>% summarise(v = mean(Ratio, na.rm = TRUE)) %>% pull(v)
mean_post <- df_donut_q %>% filter(year >= 2017, year <= 2022) %>% summarise(v = mean(Ratio, na.rm = TRUE)) %>% pull(v)

write_csv(df_donut_q, file.path(out_tab, "Table6_DonutRatio_Quarterly.csv"))

p_donut <- ggplot(df_donut_q, aes(x = q_date, y = Ratio)) +
    geom_line(color = "black", linewidth = 1.0) +
    geom_smooth(method = "loess", se = FALSE, color = pku_pal$accent, linewidth = 0.9, span = 0.25) +
    geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +
    add_treatment_lines() +
    geom_segment(
        aes(x = as.Date("2010-01-01"), xend = as.Date("2016-12-31"), y = mean_pre, yend = mean_pre),
        color = pku_pal$navy, linewidth = 1.0, linetype = "dotdash"
    ) +
    geom_segment(
        aes(x = as.Date("2017-01-01"), xend = max(q_date, na.rm = TRUE), y = mean_post, yend = mean_post),
        color = pku_pal$accent, linewidth = 1.0, linetype = "dotdash"
    ) +
    labs(
        title = "Donut Ratio (Ring / Center), Quarterly",
        subtitle = "Core-periphery balance with SILT/PTR markers and pre/post mean segments.",
        x = "",
        y = "Ratio: Ring (92-94) / Center (75)",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

# overwrite existing figure requested + save chapter-specific alias
save_plot(p_donut, "Fig_New_B3_DonutRatio.png", width = 10, height = 6)
save_plot(p_donut, "Fig6_DonutRatio_Updated.png", width = 10, height = 6)

# ------------------------------------------------------------------------------
# 6) NOTES OUTPUT
# ------------------------------------------------------------------------------
notes <- c(
    "# Chapter 6 Spatial Diagnostics (Script 22)",
    "",
    "All outputs are descriptive/corroborative diagnostics.",
    "No causal claim is made from concentration/dispersion or target-shift charts.",
    "",
    paste0("Target-shift strategy: ", target_strategy_note),
    "",
    "Primary terminology used: dispersion, concentration, core-periphery balance, substitution channel, corroborative."
)
writeLines(notes, file.path(out_tab, "Notes_Chapter6_SpatialDiagnostics.md"))

print("------------------------------------------------------------")
print("22_addition_spatial.R complete.")
print(paste("Figures saved to:", out_fig))
print(paste("Tables/notes saved to:", out_tab))
print("------------------------------------------------------------")
