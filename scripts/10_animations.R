# ==============================================================================
# Title: Animated Spatial Visualizations
# Purpose: Generate animated displacement maps and center-of-gravity trajectories.
# Author: Luan Blakaj 齐凯
# ==============================================================================

# 1. YRICHTIG -----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,
    lubridate,
    readxl,
    gganimate,
    gifski,
    transformr,
    maps,
    scales
)

source("R/theme_pku.R")
pku_red <- pku_pal$accent

output_dir <- "outputs/animation"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. HILFSFUNKTIONE -----------------------------------------------------------
normalize_text <- function(x) {
    x %>%
        str_to_lower() %>%
        iconv(from = "latin1", to = "ASCII//TRANSLIT") %>%
        str_replace_all("[^a-z]", "")
}

find_col <- function(df, candidates) {
    name_map <- setNames(names(df), tolower(names(df)))
    for (candidate in candidates) {
        if (candidate %in% names(name_map)) {
            return(name_map[[candidate]])
        }
    }
    NA_character_
}

build_france_map <- function() {
    france_map <- map_data("france")
    france_centroids <- france_map %>%
        group_by(region) %>%
        summarise(
            lon = mean(range(long, na.rm = TRUE)),
            lat = mean(range(lat, na.rm = TRUE)),
            .groups = "drop"
        ) %>%
        mutate(region_clean = normalize_text(region))

    list(outline = france_map, centroids = france_centroids)
}

map_assets <- build_france_map()

make_heat_animation <- function(df, year_col, lon_col, lat_col, weight_col, title, out_file) {
    df_plot <- df %>%
        filter(!is.na(.data[[year_col]]), !is.na(.data[[lon_col]]), !is.na(.data[[lat_col]]))

    p <- ggplot() +
        geom_polygon(
            data = map_assets$outline,
            aes(x = long, y = lat, group = group),
            fill = "grey95",
            color = "white"
        ) +
        stat_density_2d(
            data = df_plot,
            aes(
                x = .data[[lon_col]],
                y = .data[[lat_col]],
                weight = .data[[weight_col]],
                fill = after_stat(level)
            ),
            geom = "polygon",
            color = NA,
            alpha = 0.6,
            contour = TRUE
        ) +
        scale_fill_gradientn(
            colors = c("#2b0b3f", "#5b2a86", "#9b4f96", "#d26b6b", "#f4a261", "#f6c667"),
            guide = "none"
        ) +
        coord_fixed(1.3) +
        labs(title = title, subtitle = "Year: {frame_time}", x = "", y = "") +
        theme_void(base_family = "Times New Roman") %>%
        add_watermark()

    anim <- p + transition_time(.data[[year_col]]) + ease_aes("linear")

    animate(
        anim,
        nframes = 120,
        fps = 10,
        width = 800,
        height = 800,
        renderer = gifski_renderer(file.path(output_dir, out_file))
    )
}

make_center_map <- function(df, year_col, lon_col, lat_col, weight_col, title, out_file) {
    centers <- df %>%
        filter(!is.na(.data[[year_col]]), !is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) %>%
        group_by(.data[[year_col]]) %>%
        summarise(
            lon = weighted.mean(.data[[lon_col]], w = .data[[weight_col]], na.rm = TRUE),
            lat = weighted.mean(.data[[lat_col]], w = .data[[weight_col]], na.rm = TRUE),
            .groups = "drop"
        ) %>%
        arrange(.data[[year_col]])

    p <- ggplot() +
        geom_polygon(
            data = map_assets$outline,
            aes(x = long, y = lat, group = group),
            fill = "grey95",
            color = "white"
        ) +
        geom_path(
            data = centers,
            aes(x = lon, y = lat),
            color = pku_red,
            linewidth = 1
        ) +
        geom_point(
            data = centers,
            aes(x = lon, y = lat),
            color = pku_red,
            size = 2
        ) +
        geom_text(
            data = centers,
            aes(x = lon, y = lat, label = .data[[year_col]]),
            hjust = -0.2,
            vjust = -0.5,
            size = 3,
            family = "Times New Roman"
        ) +
        coord_fixed(1.3) +
        labs(title = title, subtitle = "Spatial trajectory of the mean event location", x = "", y = "") +
        theme_void(base_family = "Times New Roman") %>%
        add_watermark()

    ggsave(file.path(output_dir, out_file), p, width = 8, height = 8, bg = "white")
}

# 3. SERIESCHRONO DATE (DATAGOUV) ---------------------------------------------
series_raw <- read_delim(
    "data/serieschrono-datagouv.csv",
    delim = ";",
    locale = locale(encoding = "latin1"),
    show_col_types = FALSE
)

series_df <- series_raw %>%
    rename(
        value = `Valeurs`,
        time_unit = `Unite_temps`,
        zone = `Zone_geographique`
    ) %>%
    mutate(
        value = as.numeric(str_replace_all(value, ",", ".")),
        year = as.integer(str_sub(time_unit, 1, 4)),
        month = as.integer(str_sub(time_unit, 6, 7)),
        dept_name = str_replace(zone, "^[0-9A-Z]{2}-", ""),
        dept_clean = normalize_text(dept_name)
    ) %>%
    filter(str_detect(zone, "^[0-9A-Z]{2}-")) %>%
    left_join(map_assets$centroids, by = c("dept_clean" = "region_clean")) %>%
    filter(!is.na(lat), !is.na(lon))

series_yearly <- series_df %>%
    group_by(year, dept_clean, lon, lat) %>%
    summarise(weight = sum(value, na.rm = TRUE), .groups = "drop")

make_heat_animation(
    series_yearly,
    year_col = "year",
    lon_col = "lon",
    lat_col = "lat",
    weight_col = "weight",
    title = "Serieschrono (SSMSI): Department-Level Incidents",
    out_file = "serieschrono_animation.gif"
)

make_center_map(
    series_yearly,
    year_col = "year",
    lon_col = "lon",
    lat_col = "lat",
    weight_col = "weight",
    title = "Serieschrono Center of Gravity",
    out_file = "serieschrono_center_trajectory.png"
)

# 4. ACLED DATE ----------------------------------------------------------------
acled_raw <- read_delim(
    "data/ACLED Data_2026-01-31.csv",
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
)

names(acled_raw) <- str_replace(names(acled_raw), "^\\ufeff", "")

acled_df <- acled_raw %>%
    mutate(
        event_date = as.Date(event_date),
        year = year(event_date),
        lon = as.numeric(longitude),
        lat = as.numeric(latitude)
    ) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(year)) %>%
    mutate(weight = 1)

make_heat_animation(
    acled_df,
    year_col = "year",
    lon_col = "lon",
    lat_col = "lat",
    weight_col = "weight",
    title = "ACLED: Event Locations by Year",
    out_file = "acled_animation.gif"
)

make_center_map(
    acled_df,
    year_col = "year",
    lon_col = "lon",
    lat_col = "lat",
    weight_col = "weight",
    title = "ACLED Center of Gravity",
    out_file = "acled_center_trajectory.png"
)

# 5. GTD DATE ------------------------------------------------------------------
gtd_raw <- read_excel("data/globalterrorismdb_FR_20102021.xlsx")

names(gtd_raw) <- tolower(names(gtd_raw))

lat_col <- find_col(gtd_raw, c("latitude", "lat"))
lon_col <- find_col(gtd_raw, c("longitude", "lon"))
year_col <- find_col(gtd_raw, c("iyear", "year"))

if (is.na(lat_col) || is.na(lon_col) || is.na(year_col)) {
    stop("GTD dataset is missing latitude/longitude/year columns.")
}

gtd_df <- gtd_raw %>%
    transmute(
        year = as.integer(.data[[year_col]]),
        lat = as.numeric(.data[[lat_col]]),
        lon = as.numeric(.data[[lon_col]]),
        weight = 1
    ) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(year))

make_heat_animation(
    gtd_df,
    year_col = "year",
    lon_col = "lon",
    lat_col = "lat",
    weight_col = "weight",
    title = "GTD: Terrorism Events by Year",
    out_file = "gtd_animation.gif"
)

make_center_map(
    gtd_df,
    year_col = "year",
    lon_col = "lon",
    lat_col = "lat",
    weight_col = "weight",
    title = "GTD Center of Gravity",
    out_file = "gtd_center_trajectory.png"
)

print("--- Animation outputs saved to outputs/animation ---")
