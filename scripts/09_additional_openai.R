# ==============================================================================
# Title: Additional Supplementary Figures
# Purpose: Generate extended figures for trends, departments, event studies, and ACLED.
# Author: Luan Blakaj 齐凯
# ==============================================================================

# 1. SETUP --------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, scales, zoo, patchwork, maps)

source("R/theme_pku.R")
# Alias for compatibility
pku_red <- pku_pal$accent
pku_grey <- pku_pal$text_sec
pku_light <- pku_pal$grid

# std_caption defined in theme_pku.R

# Output directory
output_dir <- "outputs/figures_additional"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. LOAD DATA ----------------------------------------------------------------
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
df_monthly_labeled <- df_monthly %>%
    mutate(category_label = category_display(category))

df_dept <- read_csv("data/processed/departmental_panel_2010_2022_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date))

df_scm <- read_csv("data/processed/scm_master_dataset_annual.csv", show_col_types = FALSE) %>%
    mutate(year = as.integer(year))

acled_raw <- read_delim(
    "data/ACLED Data_2026-01-31.csv",
    delim = ";",
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
)

names(acled_raw) <- str_replace(names(acled_raw), "^\\ufeff", "")

df_acled <- acled_raw %>%
    mutate(
        event_date = as.Date(event_date),
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude),
        fatalities = as.numeric(fatalities)
    ) %>%
    filter(!is.na(event_date))

# 3. DERIVED DATA --------------------------------------------------------------

law_dates <- as.Date(c("2015-11-13", "2017-10-30", "2021-08-24"))

# Aggregate political violence
political_categories <- c("Violence_Officials", "Incendies", "Terrorism_AIFN")

df_political_monthly <- df_monthly %>%
    filter(category %in% political_categories) %>%
    group_by(date) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    arrange(date)

# Pre/Post summary window
pre_period <- interval(as.Date("2010-01-01"), as.Date("2015-12-31"))
post_period <- interval(as.Date("2018-01-01"), as.Date("2024-12-31"))

df_pre_post <- df_monthly %>%
    mutate(
        category_label = category_display(category),
        period = case_when(
            date %within% pre_period ~ "Pre (2010-2015)",
            date %within% post_period ~ "Post (2018-2024)",
            TRUE ~ "Other"
        )
    ) %>%
    filter(period != "Other") %>%
    group_by(category_label, period) %>%
    summarise(avg_monthly = mean(count, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = avg_monthly) %>%
    mutate(pct_change = (`Post (2018-2024)` / `Pre (2010-2015)` - 1) * 100) %>%
    rename(category = category_label)

write_csv(df_pre_post, file.path(output_dir, "Table1_Pre_Post_Averages.csv"))

# Department totals (Violence_Officials)
df_dept_totals <- df_dept %>%
    filter(category == "Violence_Officials") %>%
    group_by(dept) %>%
    summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total))

write_csv(df_dept_totals, file.path(output_dir, "Table2_Department_Totals.csv"))

# ACLED event type summary
acled_event_summary <- df_acled %>%
    count(event_type, sort = TRUE)

write_csv(acled_event_summary, file.path(output_dir, "Table3_ACLED_Event_Types.csv"))

# 4. FIGURES ------------------------------------------------------------------
print("Generating additional figures...")

# Fig E1: Category trends
p_a1 <- df_monthly_labeled %>%
    ggplot(aes(x = date, y = count, color = category_label)) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    labs(
        title = "Figure E1: Monthly Trends by Category",
        subtitle = "All recorded security categories (2010-2025)",
        y = "Monthly Incidents", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE1_Category_Trends.png"), p_a1, width = 10, height = 6, bg = "white")

# Fig E2: Political violence rolling mean
p_a2 <- df_political_monthly %>%
    mutate(roll_12 = zoo::rollmean(count, 12, fill = NA, align = "right")) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = count), color = "grey70", linewidth = 0.5) +
    geom_line(aes(y = roll_12), color = pku_red, linewidth = 1) +
    geom_vline(xintercept = law_dates, linetype = "dotted") +
    labs(
        title = "Figure E2: Political Violence (12-Month Rolling Mean)",
        subtitle = "Smoothing highlights structural shifts rather than monthly noise.",
        y = "Incidents", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE2_Political_Rolling12.png"), p_a2, width = 9, height = 5, bg = "white")

# Fig E3: Source overlap
p_a3 <- df_monthly %>%
    filter(category == "Violence_Officials") %>%
    ggplot(aes(x = date, y = count, color = source)) +
    geom_line(linewidth = 0.8) +
    labs(
        title = "Figure E3: Overlap Between Etat 4001 and SSMSI",
        subtitle = "Validating continuity in the source switch (Assaults on State Authority)",
        y = "Monthly Incidents", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE3_Source_Overlap.png"), p_a3, width = 9, height = 5, bg = "white")

# Fig E4: Pre/Post bar
p_a4 <- df_pre_post %>%
    ggplot(aes(x = reorder(category, pct_change), y = pct_change, fill = pct_change > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = pku_red, "FALSE" = "grey70")) +
    labs(
        title = "Figure E4: Average Monthly Change (Pre vs Post)",
        subtitle = "Comparing 2010-2015 vs 2018-2024 (percent change)",
        y = "% Change", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE4_Pre_Post_Change.png"), p_a4, width = 8, height = 6, bg = "white")

# Fig E5: Year-over-year change
p_a5 <- df_political_monthly %>%
    mutate(yoy = (count / lag(count, 12) - 1) * 100) %>%
    ggplot(aes(x = date, y = yoy)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_line(color = pku_red, linewidth = 0.8) +
    labs(
        title = "Figure E5: Year-over-Year Change in Political Violence",
        subtitle = "Highlights acceleration or deceleration in the trend.",
        y = "% Change vs Prior Year", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE5_YoY_Change.png"), p_a5, width = 9, height = 5, bg = "white")

# Fig E6: Cumulative count
p_a6 <- df_political_monthly %>%
    mutate(cumulative = cumsum(count)) %>%
    ggplot(aes(x = date, y = cumulative)) +
    geom_line(color = pku_red, linewidth = 1) +
    labs(
        title = "Figure E6: Cumulative Political Violence",
        subtitle = "Long-term accumulation of incidents",
        y = "Cumulative Incidents", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE6_Cumulative.png"), p_a6, width = 9, height = 5, bg = "white")

# Fig E7: Monthly distribution
p_a7 <- df_political_monthly %>%
    ggplot(aes(x = count)) +
    geom_histogram(fill = pku_red, alpha = 0.7, bins = 30) +
    labs(
        title = "Figure E7: Distribution of Monthly Political Violence",
        subtitle = "Shows skew and outlier months",
        y = "Number of Months", x = "Monthly Incidents", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE7_Monthly_Distribution.png"), p_a7, width = 8, height = 5, bg = "white")

# Fig E8: Seasonal profile
p_a8 <- df_political_monthly %>%
    mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
    group_by(month) %>%
    summarise(avg = mean(count, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = month, y = avg, group = 1)) +
    geom_line(color = pku_red, linewidth = 1) +
    geom_point(color = "black", size = 2) +
    labs(
        title = "Average Seasonal Cycle",
        y = "Average Monthly Incidents", x = "Month", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE8_Seasonal_Profile.png"), p_a8, width = 8, height = 5, bg = "white")

# Fig E9: Category by month heatmap
p_a9 <- df_monthly %>%
    mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
    mutate(category_label = category_display(category)) %>%
    group_by(category_label, month) %>%
    summarise(avg = mean(count, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = month, y = category_label, fill = avg)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = pku_red) +
    labs(
        title = "Seasonal Heatmap by Category",
        y = "Category", x = "Month", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigE9_Category_Seasonality.png"), p_a9, width = 9, height = 5, bg = "white")

# Fig F1: Top departments
p_b1 <- df_dept_totals %>%
    slice_max(order_by = total, n = 15) %>%
    ggplot(aes(x = reorder(dept, total), y = total)) +
    geom_col(fill = pku_red) +
    coord_flip() +
    labs(
        title = "Figure F1: Top Departments by Total Assaults on State Authority",
        subtitle = "2010-2022 totals",
        y = "Total Incidents", x = "Department", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigF1_Top_Departments.png"), p_b1, width = 8, height = 6, bg = "white")

# Fig F2: Department share in 2022
p_b2 <- df_dept %>%
    filter(category == "Violence_Officials", year(date) == 2022) %>%
    group_by(dept) %>%
    summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(share = total / sum(total, na.rm = TRUE)) %>%
    arrange(desc(share)) %>%
    slice_max(order_by = share, n = 12) %>%
    ggplot(aes(x = reorder(dept, share), y = share)) +
    geom_col(fill = pku_red) +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(
        title = "Figure F2: Department Share of Violence (2022)",
        subtitle = "Concentration of incidents in the latest year",
        y = "Share of National Total", x = "Department", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigF2_Department_Share_2022.png"), p_b2, width = 8, height = 6, bg = "white")

# Fig F3: Trend slope change
slope_calc <- function(df) {
    model <- lm(count ~ time_index, data = df)
    coef(model)[2]
}

df_dept_slopes <- df_dept %>%
    filter(category == "Violence_Officials") %>%
    mutate(year = year(date), time_index = as.numeric(date)) %>%
    mutate(period = case_when(
        year <= 2015 ~ "Pre",
        year >= 2018 ~ "Post",
        TRUE ~ "Other"
    )) %>%
    filter(period != "Other") %>%
    group_by(dept, period) %>%
    summarise(slope = slope_calc(pick(everything())), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = slope) %>%
    mutate(delta = Post - Pre) %>%
    arrange(desc(delta))

p_b3 <- df_dept_slopes %>%
    slice_max(order_by = abs(delta), n = 15) %>%
    ggplot(aes(x = reorder(dept, delta), y = delta)) +
    geom_col(fill = pku_red) +
    coord_flip() +
    labs(
        title = "Figure F3: Shift in Departmental Trends",
        subtitle = "Difference in slope (Post 2018 vs Pre 2015)",
        y = "Slope Change", x = "Department", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigF3_Department_Slope_Change.png"), p_b3, width = 8, height = 6, bg = "white")

# Fig F4: Paris vs Suburbs indexed to 2010 average
p_b4 <- df_dept %>%
    filter(category == "Violence_Officials") %>%
    mutate(region = case_when(
        dept == "75" ~ "Paris (75)",
        dept %in% c("92", "93", "94") ~ "Petite Couronne",
        TRUE ~ "Rest of France"
    )) %>%
    group_by(date, region) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    group_by(region) %>%
    mutate(index = (count / mean(count[year(date) == 2010], na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    ggplot(aes(x = date, y = index, color = region)) +
    geom_line(linewidth = 0.8) +
    labs(
        title = "Figure F4: Paris vs Suburbs (Indexed)",
        subtitle = "2010 average = 100 for each region",
        y = "Index", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigF4_Paris_Suburbs_Indexed.png"), p_b4, width = 9, height = 5, bg = "white")

# Event study helper
build_event_study <- function(center_date, window_months) {
    df_political_monthly %>%
        mutate(month_rel = (year(date) - year(center_date)) * 12 + (month(date) - month(center_date))) %>%
        filter(month_rel >= -window_months, month_rel <= window_months) %>%
        group_by(month_rel) %>%
        summarise(avg = mean(count, na.rm = TRUE), .groups = "drop")
}

# Fig G1/G2/G3: Event studies
p_c1 <- build_event_study(as.Date("2015-11-13"), 24) %>%
    ggplot(aes(x = month_rel, y = avg)) +
    geom_line(color = pku_red, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
        title = "Figure G1: Event Study around 2015 Emergency Law",
        subtitle = "Average monthly incidents relative to policy start",
        y = "Average Incidents", x = "Months from Event", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigG1_Event_Study_2015.png"), p_c1, width = 8, height = 5, bg = "white")

p_c2 <- build_event_study(as.Date("2017-10-30"), 24) %>%
    ggplot(aes(x = month_rel, y = avg)) +
    geom_line(color = pku_red, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
        title = "Figure G2: Event Study around Loi SILT (2017)",
        subtitle = "Average monthly incidents relative to policy start",
        y = "Average Incidents", x = "Months from Event", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigG2_Event_Study_2017.png"), p_c2, width = 8, height = 5, bg = "white")

p_c3 <- build_event_study(as.Date("2021-08-24"), 24) %>%
    ggplot(aes(x = month_rel, y = avg)) +
    geom_line(color = pku_red, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
        title = "Figure G3: Event Study around Separatism Law (2021)",
        subtitle = "Average monthly incidents relative to policy start",
        y = "Average Incidents", x = "Months from Event", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigG3_Event_Study_2021.png"), p_c3, width = 8, height = 5, bg = "white")

# Fig H1: SCM country trends
p_d1 <- df_scm %>%
    filter(category_standardized %in% c("Violence_Officials", "Burglary", "Terrorism")) %>%
    mutate(category_label = category_display(category_standardized)) %>%
    ggplot(aes(x = year, y = count, color = country)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~category_label, scales = "free_y") +
    labs(
        title = "Figure H1: Donor Pool Trends (Annual)",
        subtitle = "France vs donor countries for key categories",
        y = "Annual Count", x = "Year", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigH1_Donor_Trends.png"), p_d1, width = 10, height = 6, bg = "white")

# Fig H2: SCM indexed trends
p_d2 <- df_scm %>%
    filter(category_standardized == "Violence_Officials") %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(index = (count / first(count)) * 100) %>%
    ungroup() %>%
    ggplot(aes(x = year, y = index, color = country)) +
    geom_line(linewidth = 0.8) +
    labs(
        title = "Figure H2: Indexed Trends (Assaults on State Authority)",
        subtitle = "2010 = 100 for each country",
        y = "Index", x = "Year", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigH2_Indexed_SCM.png"), p_d2, width = 9, height = 5, bg = "white")

# Fig H3: SCM gap (if optimized file exists)
if (file.exists("data/processed/scm_optimized_synthetic_france.csv")) {
    df_synth <- read_csv("data/processed/scm_optimized_synthetic_france.csv", show_col_types = FALSE) %>%
        pivot_wider(names_from = Type, values_from = Count)

    p_d3 <- df_synth %>%
        mutate(gap = France_Real - Synthetic_France) %>%
        ggplot(aes(x = year, y = gap)) +
        geom_col(fill = pku_red) +
        geom_hline(yintercept = 0, color = "grey40") +
        labs(
            title = "Figure H3: Synthetic Control Gap (Annual)",
            subtitle = "Positive values indicate excess incidents in France",
            y = "France - Synthetic", x = "Year", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()

    ggsave(file.path(output_dir, "FigH3_SCM_Gap_Bars.png"), p_d3, width = 9, height = 5, bg = "white")
}

# Fig I1: ACLED point map
france_map <- map_data("france")

p_e1 <- ggplot() +
    geom_polygon(data = france_map, aes(x = long, y = lat, group = group), fill = "grey95", color = "white") +
    geom_point(data = df_acled, aes(x = longitude, y = latitude), color = pku_red, alpha = 0.4, size = 0.8) +
    coord_fixed(1.3) +
    labs(
        title = "ACLED Event Locations (2020-2025)",
        subtitle = "Point map of political violence and demonstrations",
        x = "", y = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigI1_ACLED_Point_Map.png"), p_e1, width = 7, height = 7, bg = "white")

# Fig I2: ACLED map by event type
p_e2 <- ggplot() +
    geom_polygon(data = france_map, aes(x = long, y = lat, group = group), fill = "grey95", color = "white") +
    geom_point(data = df_acled, aes(x = longitude, y = latitude, color = event_type), alpha = 0.5, size = 0.9) +
    coord_fixed(1.3) +
    labs(
        title = "Figure I2: ACLED Events by Type",
        subtitle = "Spatial clustering of event categories",
        x = "", y = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigI2_ACLED_Event_Types.png"), p_e2, width = 8, height = 7, bg = "white")

# Fig I3: ACLED density bins
p_e3 <- ggplot(df_acled, aes(x = longitude, y = latitude)) +
    stat_bin2d(bins = 40) +
    scale_fill_gradient(low = "white", high = pku_red) +
    coord_fixed(1.3) +
    labs(
        title = "Figure I3: ACLED Spatial Density",
        subtitle = "2D bin counts across France",
        x = "", y = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigI3_ACLED_Density.png"), p_e3, width = 7, height = 7, bg = "white")

# Fig I4: ACLED monthly trend
p_e4 <- df_acled %>%
    mutate(month = floor_date(event_date, "month")) %>%
    count(month) %>%
    ggplot(aes(x = month, y = n)) +
    geom_line(color = pku_red, linewidth = 1) +
    labs(
        title = "Figure I4: ACLED Monthly Event Count",
        subtitle = "High-frequency view of political violence + demonstrations",
        y = "Events", x = "", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigI4_ACLED_Monthly_Trend.png"), p_e4, width = 9, height = 5, bg = "white")

# Fig I5: ACLED fatalities distribution
p_e5 <- df_acled %>%
    filter(!is.na(fatalities)) %>%
    ggplot(aes(x = fatalities)) +
    geom_histogram(fill = pku_red, bins = 30, alpha = 0.7) +
    scale_x_continuous(breaks = pretty_breaks()) +
    labs(
        title = "Figure I5: ACLED Fatalities Distribution",
        subtitle = "Most events have zero fatalities",
        y = "Event Count", x = "Fatalities", caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

ggsave(file.path(output_dir, "FigI5_ACLED_Fatalities.png"), p_e5, width = 8, height = 5, bg = "white")

print("--- Additional graphics complete ---")
print("Check outputs/figures_additional for new figures and tables.")
