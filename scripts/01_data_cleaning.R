# ==============================================================================
# Title: Data Cleaning and Donor Integration
# Purpose: Clean French security data, standardize categories, generate diagnostic plots.
# Author: Luan Blakaj 齐凯
# ==============================================================================

library(tidyverse)
library(readxl)
source("R/theme_pku.R")

# 1. DATE LADE ----------------------------------------------------------------
# Französeschi Mönetlichi Date (us Schritt 1)
df_france_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE)

# Donor-Johresdate
df_donors <- read_csv("data/processed/standardized_counts_donor.csv", show_col_types = FALSE)

# 2. Frankrych uff Johresebeni aggregiere -----------------------------------------------

df_france_annual <- df_france_monthly %>%
  mutate(year = year(date)) %>%
  # Nur vollständigi Johr filtere (2010-2024). 2025 isch unvollständig.
  filter(year < 2025) %>%
  group_by(year, category) %>%
  summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(country = "France") %>%
  # Standardize Category Names to match your Donor File exactly
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

# 3.  Datasets Zämmesetze -----------------------------------------------------------

df_scm_master <- bind_rows(df_france_annual, df_donors)

# 4. Normalisiere (Index to Base Year) ----------------------------------------

df_scm_normalized <- df_scm_master %>%
  group_by(country, category_standardized) %>%
  arrange(year) %>%
  mutate(
    # Luege ob 2010 existiert, otherwise use the first available year as base
    base_value = first(count),
    index_100 = (count / base_value) * 100
  ) %>%
  ungroup()

# 5. Diagnostic Quality Check --------------------------------------------------

plot_trends_indexed <- df_scm_normalized %>%
  filter(category_standardized %in% c("Violence_Officials", "Incendies", "Terrorism")) %>%
  mutate(category_label = category_display(category_standardized)) %>%
  ggplot(aes(x = year, y = index_100, color = country)) +
  geom_line(size = 1) +
  facet_wrap(~category_label, scales = "free_y") +
  theme_pku() +
  scale_color_manual(values = c(
    "France" = pku_pal$accent,
    "Spain" = pku_pal$navy,
    "UK" = pku_pal$slate,
    "Austria" = pku_pal$mono[2]
  )) +
  labs(
    title = "Indexed Trends (Base Year = 100)",
    subtitle = "Comparing Relative Growth (Normalized for Scale Differences)",
    y = "Index (100 = Start)", x = "Year"
  ) %>%
  add_watermark()

# 6. Scaling Factor ------------------------------------------------------------

scaling_factors <- df_scm_master %>%
  group_by(country, category_standardized) %>%
  summarise(mean_val = mean(count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = country, values_from = mean_val) %>%
  mutate(
    scale_Spain = France / Spain,
    scale_UK = France / UK,
    scale_Austria = France / Austria
  )

print("--- Scaling Factors (Multipliers to make raw counts match France) ---")
print(scaling_factors)

# Save Output
write_csv(df_scm_master, "data/processed/scm_master_dataset_annual.csv")
write_csv(df_scm_normalized, "data/processed/scm_master_dataset_annual_indexed.csv")
ggsave("outputs/scm_indexed_trends_check.png", plot_trends_indexed, width = 10, height = 6, bg = "white")

print("--- SCM Preparation Complete ---")
print("1. Master Dataset saved: data/processed/scm_master_dataset_annual.csv")
print("2. Indexed Dataset saved: data/processed/scm_master_dataset_annual_indexed.csv")
print("3. outputs/scm_indexed_trends_check.png")
