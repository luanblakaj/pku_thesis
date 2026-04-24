# ==============================================================================
# Title: Full SCM Pipeline
# Purpose: Consolidated pipeline for data cleaning, SCM optimization, and visualization.
# Author: Luan Blakaj 齐凯
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. SETUP & PACKAGES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
source("R/theme_pku.R")
pacman::p_load(tidyverse, readxl, stringr, lubridate, tidyr)


to_long <- function(df, country) {
    df %>%
        rename(name_raw = Name) %>%
        mutate(across(-name_raw, as.character)) %>%
        pivot_longer(cols = -name_raw, names_to = "year", values_to = "count") %>%
        mutate(
            year = as.integer(str_extract(year, "\\d{4}")),
            country = country,
            count = as.integer(str_remove_all(count, "[^0-9]"))
        )
}


pick_category <- function(long_df, pattern, category_std) {
    long_df %>%
        filter(str_detect(name_raw, regex(pattern, ignore_case = TRUE))) %>%
        group_by(year, country) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(year, country, category_standardized = category_std, count)
}


pick_austria_violence_officials <- function(long_df) {
    widerstand <- long_df %>% filter(str_detect(name_raw, regex("Widerstand", ignore_case = TRUE)))
    if (nrow(widerstand) > 0) {
        widerstand %>%
            group_by(year, country) %>%
            slice(1) %>%
            ungroup() %>%
            transmute(year, country, category_standardized = "Violence_Officials", count)
    } else {
        long_df %>%
            filter(str_detect(name_raw, regex("ABSICHTLICHE\\s+SCHWERE\\s+K[ÖO]RPERVERLETZUNG\\s*§\\s*87", ignore_case = TRUE))) %>%
            group_by(year, country) %>%
            slice(1) %>%
            ungroup() %>%
            transmute(year, country, category_standardized = "Violence_Officials", count)
    }
}


# ------------------------------------------------------------------------------
# 1. FRANCE DATA (CLEAN & AGGREGATE)
# ------------------------------------------------------------------------------
print("--- STEP 1: Processing France Data ---")

df_france_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE)

df_france_annual <- df_france_monthly %>%
    mutate(year = year(date)) %>%
    filter(year < 2025) %>%
    group_by(year, category) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(country = "France") %>%
    mutate(category_standardized = case_when(
        category == "Violence_Officials" ~ "Violence_Officials",
        category == "Incendies" ~ "Incendies",
        category == "Destruction_Other" ~ "Destruction_Other",
        category == "Burglary" ~ "Burglary",
        category == "Vehicle_Theft" ~ "Vehicle_Theft",
        category == "Terrorism_AIFN" ~ "Terrorism",
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(category_standardized)) %>%
    dplyr::select(year, country, category_standardized, count)

print(paste("France Data Loaded:", nrow(df_france_annual), "rows (Annual)."))


# ------------------------------------------------------------------------------
# 2. DONOR DATA (READ & STANDARDIZE)
# ------------------------------------------------------------------------------
print("--- STEP 2: Processing Donor Data ---")

file_path_donors <- "data/Donor_Data.xlsx"
df_at <- read_excel(file_path_donors, sheet = "Austria")
df_uk <- read_excel(file_path_donors, sheet = "England")
df_es <- read_excel(file_path_donors, sheet = "Sheet3") # Spain

long_at <- to_long(df_at, "Austria")
long_uk <- to_long(df_uk, "UK")
long_es <- to_long(df_es, "Spain")

# Spain
out_es <- bind_rows(
    pick_category(long_es, "^\\s*22\\.2\\s+Atentados", "Violence_Officials"),
    pick_category(long_es, "^\\s*22\\.7\\s+Organizaciones\\s+y\\s+grupos\\s+terroristas", "Terrorism"),
    pick_category(long_es, "^\\s*17\\.2\\s+Incend", "Incendies"),
    pick_category(long_es, "^\\s*13\\.9\\s+Da[ñn]os", "Destruction_Other"),
    pick_category(long_es, "^\\s*13\\.2\\s+Robos", "Burglary"),
    pick_category(long_es, "^\\s*13\\.4\\s+Robo\\s+y\\s+hurto", "Vehicle_Theft")
)

# Austria
out_at <- bind_rows(
    pick_austria_violence_officials(long_at),
    pick_category(long_at, "BRANDSTIFTUNG\\s*§\\s*169", "Incendies"),
    pick_category(long_at, "SACHBESCH[ÄA]DIGUNG\\s*§\\s*125", "Destruction_Other"),
    pick_category(long_at, "DIEBSTAHL\\s+DURCH\\s+EINBRUCH", "Burglary"),
    pick_category(long_at, "UNBEFUGTER\\s+GEBRAUCH\\s+VON\\s+FAHRZEUGEN", "Vehicle_Theft"),
    # Map Public Safety to Terrorism
    pick_category(long_at, "VORS[ÄA]TZLICHE\\s+GEMEINGEF[ÄA]HRDUNG\\s*§\\s*176", "Terrorism")
)

# UK
out_uk <- bind_rows(
    pick_category(long_uk, "Assault.*on\\s+constable", "Violence_Officials"),
    pick_category(long_uk, "^\\s*Arson\\s*$", "Incendies"),
    pick_category(long_uk, "^\\s*Criminal\\s+Damage\\s*$", "Destruction_Other"),
    pick_category(long_uk, "^\\s*Burglary\\s*$", "Burglary"),
    pick_category(long_uk, "^\\s*Theft\\s+or\\s+unauthorised\\s+taking\\s+of\\s+a\\s+motor\\s+vehicle\\s*$", "Vehicle_Theft")
)

df_donors_clean <- bind_rows(out_es, out_at, out_uk) %>%
    arrange(year, country, category_standardized)

print(paste("Donor Data Loaded:", nrow(df_donors_clean), "rows."))


# ------------------------------------------------------------------------------
# 3. MERGE & TRANSFORMATION
# ------------------------------------------------------------------------------
print("--- STEP 3: Merging & Log Transformation ---")

df_master <- bind_rows(df_france_annual, df_donors_clean)


df_log <- df_master %>%
    mutate(log_count = log(count + 1))


# ------------------------------------------------------------------------------
# 4. Optimization
# ------------------------------------------------------------------------------
print("--- STEP 4: Optimization (Log-Linear) ---")

TRAIN_START <- 2010
TRAIN_END <- 2015

create_synthetic_control <- function(category_name) {
    print(paste("Optimizing for:", category_name))


    df_wide <- df_log %>%
        filter(category_standardized == category_name) %>%
        dplyr::select(year, country, log_count) %>%
        pivot_wider(names_from = country, values_from = log_count) %>%
        filter(year >= TRAIN_START & year <= 2024) %>%
        tidyr::fill(everything(), .direction = "up")

    available_donors <- setdiff(names(df_wide), c("year", "France"))

    if (length(available_donors) == 0 || !"France" %in% names(df_wide)) {
        print("Skipping: Insufficient data.")
        return(NULL)
    }


    df_train <- df_wide %>% filter(year <= TRAIN_END)
    donors_formula <- paste(available_donors, collapse = " + ")
    formula_str <- paste("France ~", donors_formula)
    print(paste("Formula:", formula_str))

    model <- lm(as.formula(formula_str), data = df_train)


    df_wide$Synthetic_France_Log <- predict(model, newdata = df_wide)


    mean_real <- mean(df_wide$France[df_wide$year <= TRAIN_END], na.rm = TRUE)
    mean_syn <- mean(df_wide$Synthetic_France_Log[df_wide$year <= TRAIN_END], na.rm = TRUE)
    df_wide$Synthetic_France_Log <- df_wide$Synthetic_France_Log + (mean_real - mean_syn)


    df_wide <- df_wide %>%
        mutate(
            France_Real = exp(France) - 1,
            Synthetic_France = exp(Synthetic_France_Log) - 1
        )


    df_plot <- df_wide %>%
        dplyr::select(year, France_Real, Synthetic_France) %>%
        pivot_longer(cols = c("France_Real", "Synthetic_France"), names_to = "Type", values_to = "Count") %>%
        mutate(category = category_name)

    return(df_plot)
}


categories <- c("Violence_Officials", "Incendies", "Terrorism", "Burglary", "Vehicle_Theft", "Destruction_Other")
results_list <- list()

for (cat in categories) {
    check <- df_master %>% filter(category_standardized == cat)
    if ("France" %in% check$country && n_distinct(check$country) > 1) {
        results_list[[cat]] <- create_synthetic_control(cat)
    }
}
df_synthetic_final <- bind_rows(results_list)


# ------------------------------------------------------------------------------
# 5. HYBRID VISUALIZATION
# ------------------------------------------------------------------------------
print("--- STEP 5: Hybrid Visualization ---")


df_monthly_view <- df_france_monthly %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>%
    mutate(category = case_when(
        category == "Violence_Officials" ~ "Violence_Officials",
        category == "Incendies" ~ "Incendies",
        category == "Destruction_Other" ~ "Destruction_Other",
        category == "Burglary" ~ "Burglary",
        category == "Vehicle_Theft" ~ "Vehicle_Theft",
        category == "Terrorism_AIFN" ~ "Terrorism",
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(category), category %in% categories) %>%
    mutate(category_label = category_display(category)) %>%
    rename(France_Real = count)

df_synthetic_view <- df_synthetic_final %>%
    filter(Type == "Synthetic_France") %>%
    mutate(
        date = as.Date(paste0(year, "-01-01")),
        Synthetic_Monthly_Avg = Count / 12
    ) %>%
    mutate(category_label = category_display(category))

plot_hybrid <- ggplot() +
    geom_line(data = df_monthly_view, aes(x = date, y = France_Real, color = "France (Monthly Real)"), alpha = 0.6, linewidth = 0.8) +
    geom_step(data = df_synthetic_view, aes(x = date, y = Synthetic_Monthly_Avg, color = "Synthetic (Monthly Avg)"), linewidth = 1.2) +
    facet_wrap(~category_label, scales = "free_y") +
    theme_pku() +
    scale_color_manual(values = c("France (Monthly Real)" = pku_pal$accent, "Synthetic (Monthly Avg)" = pku_pal$navy))

plot_hybrid <- plot_hybrid +
    labs(
        title = "Hybrid SCM: Monthly Reality vs. Annual Synthetic Trend",
        subtitle = "Synthetic line = Annual Optimized Prediction / 12 months.",
        y = "Monthly Count", x = "Date",
        caption = std_caption
    ) +
    theme(plot.title.position = "plot")

ggsave("outputs/MASTER_SCM_PLOT.png", plot_hybrid, width = 10, height = 7, bg = "white")

# ------------------------------------------------------------------------------
# 6. Single-Outcome SCM
# ------------------------------------------------------------------------------
print("--- STEP 6: Single-Outcome SCM Plots ---")

df_single_real <- df_monthly_view %>%
    dplyr::filter(category == "Violence_Officials") %>%
    dplyr::select(date, France_Real) %>%
    dplyr::arrange(date)

df_single_syn <- df_synthetic_final %>%
    dplyr::filter(Type == "Synthetic_France", category == "Violence_Officials") %>%
    dplyr::select(year, Count) %>%
    dplyr::rename(Synthetic_Annual = Count)

df_single <- df_single_real %>%
    dplyr::mutate(year = year(date)) %>%
    dplyr::left_join(df_single_syn, by = "year") %>%
    dplyr::mutate(Synthetic_Monthly_Avg = Synthetic_Annual / 12) %>%
    dplyr::select(date, France_Real, Synthetic_Monthly_Avg) %>%
    dplyr::arrange(date)

# A. Raw levels (monthly vs synthetic step)
p_single <- ggplot(df_single, aes(x = date)) +
    geom_line(aes(y = France_Real, color = "France (Monthly Real)"), alpha = 0.7, linewidth = 0.9) +
    geom_step(aes(y = Synthetic_Monthly_Avg, color = "Synthetic (Monthly Avg)"), linewidth = 1.1, linetype = "solid") +
    scale_color_manual(values = c("France (Monthly Real)" = pku_pal$accent, "Synthetic (Monthly Avg)" = pku_pal$navy)) +
    labs(
        title = "France vs Synthetic (Assaults on State Authority)",
        subtitle = "Absolute levels: observed monthly series vs annual synthetic (monthly average).",
        y = "Monthly Count", x = "Date",
        caption = std_caption
    ) +
    theme_pku() +
    theme(plot.title.position = "plot")

ggsave("outputs/single-outcome_SCM.png", p_single, width = 10, height = 6, bg = "white")

# B. Indexed trajectories (2010 = 100)
base_real <- mean(df_single$France_Real[year(df_single$date) == 2010], na.rm = TRUE)
base_syn <- mean(df_single$Synthetic_Monthly_Avg[year(df_single$date) == 2010], na.rm = TRUE)
if (is.na(base_real) || base_real == 0) {
    base_real <- mean(df_single$France_Real, na.rm = TRUE)
}
if (is.na(base_syn) || base_syn == 0) {
    base_syn <- mean(df_single$Synthetic_Monthly_Avg, na.rm = TRUE)
}

df_single_index <- df_single %>%
    mutate(
        France_Index = (France_Real / base_real) * 100,
        Synthetic_Index = (Synthetic_Monthly_Avg / base_syn) * 100
    )

p_single_index <- ggplot(df_single_index, aes(x = date)) +
    geom_line(aes(y = France_Index, color = "France (Indexed)"), alpha = 0.8, linewidth = 0.9) +
    geom_line(aes(y = Synthetic_Index, color = "Synthetic (Indexed)"), linewidth = 1.1) +
    scale_color_manual(values = c("France (Indexed)" = pku_pal$accent, "Synthetic (Indexed)" = pku_pal$navy)) +
    labs(
        title = "France vs Synthetic (Indexed, 2010 = 100)",
        subtitle = "Trajectory comparison with baseline normalization.",
        y = "Index (2010 = 100)", x = "Date",
        caption = std_caption
    ) +
    theme_pku() +
    theme(plot.title.position = "plot")

ggsave("outputs/single-outcome_SCM_indexed.png", p_single_index, width = 10, height = 6, bg = "white")

# C. Smoothed trajectories (12-month moving average)
df_single_smooth <- df_single %>%
    mutate(
        France_Roll12 = as.numeric(stats::filter(France_Real, rep(1 / 12, 12), sides = 1)),
        Synthetic_Roll12 = as.numeric(stats::filter(Synthetic_Monthly_Avg, rep(1 / 12, 12), sides = 1))
    ) %>%
    filter(!is.na(France_Roll12), !is.na(Synthetic_Roll12))

p_single_smooth <- ggplot(df_single_smooth, aes(x = date)) +
    geom_line(aes(y = France_Roll12, color = "France (12m MA)"), linewidth = 1.0) +
    geom_line(aes(y = Synthetic_Roll12, color = "Synthetic (12m MA)"), linewidth = 1.1) +
    scale_color_manual(values = c("France (12m MA)" = pku_pal$accent, "Synthetic (12m MA)" = pku_pal$navy)) +
    labs(
        title = "France vs Synthetic (12-Month Moving Average)",
        subtitle = "Smoothed absolute levels for the focal outcome.",
        y = "Monthly Count (12m MA)", x = "Date",
        caption = std_caption
    ) +
    theme_pku() +
    theme(plot.title.position = "plot")

ggsave("outputs/single-outcome_SCM_rolling12.png", p_single_smooth, width = 10, height = 6, bg = "white")

print("--- PIPELINE COMPLETE ---")
print("See 'outputs/MASTER_SCM_PLOT.png'.")
