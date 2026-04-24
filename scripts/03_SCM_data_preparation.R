# ==============================================================================
# Title: SCM Data Preparation (Annual Aggregation)
# Purpose: Aggregate French monthly data to annual, merge with donors, normalize.
# Author: Luan Blakaj 齐凯
# ==============================================================================

library(tidyverse)
library(readxl)
source("R/theme_pku.R")

# 1. Load Data ----------------------------------------------------------------
# French Monthly Data (Processed in Step 1)
df_france_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE)

# Donor Annual Data
df_donors <- read_csv("data/processed/standardized_counts_donor.csv", show_col_types = FALSE)

# 2. Aggregate France to Annual -----------------------------------------------
# We need to sum the monthly counts into yearly counts to compare with Spain/UK.

df_france_annual <- df_france_monthly %>%
    # A. DE-DUPLICATION (Handle Overlap Step)
    # If 2016-2022 exists in both sources, we average them to avoid double counting.
    group_by(date, category) %>%
    summarise(count = mean(count, na.rm = TRUE), .groups = "drop") %>%
    # B. ANNUAL AGGREGATION & EXTRAPOLATION
    mutate(year = year(date)) %>%
    # Keep 2010-2022
    filter(year >= 2010 & year <= 2022) %>%
    group_by(year, category) %>%
    summarise(
        total_count = sum(count, na.rm = TRUE),
        months_present = n_distinct(month(date)),
        .groups = "drop"
    ) %>%
    # C. EXTRAPOLATION LOGIC
    mutate(
        count = if_else(
            months_present < 12 & months_present > 0,
            total_count * (12 / months_present), # Scale up
            total_count
        )
    ) %>%
    mutate(country = "France") %>%
    # Standardize category names
    mutate(category_standardized = case_when(
        category == "Violence_Officials" ~ "Violence_Officials",
        category == "Incendies" ~ "Incendies", # Matching your donor file name
        category == "Destruction_Other" ~ "Destruction_Other",
        category == "Burglary" ~ "Burglary",
        category == "Vehicle_Theft" ~ "Vehicle_Theft",
        category == "Terrorism_AIFN" ~ "Terrorism", # Matching Spain's "Terrorism"
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(category_standardized)) %>%
    select(year, country, category_standardized, count)

# 3. Merge Datasets -----------------------------------------------------------

df_scm_master <- bind_rows(df_france_annual, df_donors)

# 4. Normalizations -----------------------------------------------------------

# A. Indexing (2010 = 100)
df_indexed <- df_scm_master %>%
    group_by(country, category_standardized) %>%
    arrange(year) %>%
    mutate(
        base_val = count[year == min(year)], # Value in first year (usually 2010)
        index_100 = (count / base_val) * 100
    ) %>%
    ungroup()

# B. Z-Score (Mean = 0, SD = 1)
df_zscore <- df_scm_master %>%
    group_by(country, category_standardized) %>%
    mutate(
        z_score = (count - mean(count, na.rm = TRUE)) / sd(count, na.rm = TRUE)
    ) %>%
    ungroup()

# 5. Visualization ------------------------------------------------------------

plot_indexed <- df_indexed %>%
    filter(category_standardized %in% c("Violence_Officials", "Incendies", "Terrorism", "Burglary", "Vehicle_Theft")) %>%
    mutate(category_label = category_display(category_standardized)) %>%
    ggplot(aes(x = year, y = index_100, color = country)) +
    geom_line(linewidth = 1) +
    facet_wrap(~category_label, scales = "free_y") +
    theme_pku() +
    scale_color_manual(values = c(
        "France" = pku_pal$accent,
        "Spain" = pku_pal$navy,
        "UK" = pku_pal$slate,
        "Austria" = pku_pal$mono[2]
    )) +
    labs(
        title = "Indexed Trends (2010 = 100)",
        subtitle = "Relative growth comparison. All lines start at 100.",
        y = "Index Value", x = "Year"
    ) %>%
    add_watermark()

plot_zscore <- df_zscore %>%
    filter(category_standardized %in% c("Violence_Officials", "Incendies", "Terrorism", "Burglary", "Vehicle_Theft")) %>%
    mutate(category_label = category_display(category_standardized)) %>%
    ggplot(aes(x = year, y = z_score, color = country)) +
    geom_line(linewidth = 1) +
    facet_wrap(~category_label, scales = "free_y") +
    theme_pku() +
    scale_color_manual(values = c(
        "France" = pku_pal$accent,
        "Spain" = pku_pal$navy,
        "UK" = pku_pal$slate,
        "Austria" = pku_pal$mono[2]
    )) +
    labs(
        title = "Z-Score Trends (Std Dev from Mean)",
        subtitle = "Correlated movements check. 0 = Average for that country.",
        y = "Standard Deviations", x = "Year"
    ) %>%
    add_watermark()

# 6. Save Outputs -------------------------------------------------------------
write_csv(df_scm_master, "data/processed/scm_master_dataset_annual.csv")
write_csv(df_indexed, "data/processed/scm_master_dataset_indexed.csv")
write_csv(df_zscore, "data/processed/scm_master_dataset_zscore.csv")

ggsave("outputs/scm_trends_indexed.png", plot_indexed, width = 12, height = 8, bg = "white")
ggsave("outputs/scm_trends_zscore.png", plot_zscore, width = 12, height = 8, bg = "white")

print("--- SCM Preparation Complete ---")
print("Files Saved:")
print("1. data/processed/scm_master_dataset_annual.csv")
print("2. data/processed/scm_master_dataset_indexed.csv")
print("3. data/processed/scm_master_dataset_zscore.csv")
print("Plots Saved:")
print("1. outputs/scm_trends_indexed.png")
print("2. outputs/scm_trends_zscore.png")
