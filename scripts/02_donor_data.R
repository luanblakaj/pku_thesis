# ==============================================================================
# Title: Donor Data Standardization
# Purpose: Parse and standardize crime data from Austria, UK, and Spain.
# Author: Luan Blakaj 齐凯
# ==============================================================================

# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

file_path <- "data/Donor_Data.xlsx"

# Helper: pivot wide (years) -> long and clean types
to_long <- function(df, country) {
    df %>%
        rename(name_raw = Name) %>%
        # FORCE all columns to character first to avoid type mismatch
        mutate(across(-name_raw, as.character)) %>%
        pivot_longer(
            cols = -name_raw,
            names_to = "year",
            values_to = "count"
        ) %>%
        mutate(
            # Extract just the numeric year (in case of footnotes like "2010 (a)")
            year = as.integer(str_extract(year, "\\d{4}")),
            country = country,
            # Clean count: remove non-numeric chars except digits, then parse
            count = as.integer(str_remove_all(count, "[^0-9]"))
        )
}

# Helper: find one row by regex pattern (first match), then tag with standardized category
pick_category <- function(long_df, pattern, category_std) {
    long_df %>%
        filter(str_detect(name_raw, regex(pattern, ignore_case = TRUE))) %>%
        group_by(year, country) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(year, country, category_standardized = category_std, count)
}

# Helper: for Austria Violence_Officials rule
pick_austria_violence_officials <- function(long_df) {
    widerstand <- long_df %>%
        filter(str_detect(name_raw, regex("Widerstand", ignore_case = TRUE)))

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

# ---- Read sheets ----
df_at <- read_excel(file_path, sheet = "Austria")
df_uk <- read_excel(file_path, sheet = "England")
df_es <- read_excel(file_path, sheet = "Sheet3") # Spain

long_at <- to_long(df_at, "Austria")
long_uk <- to_long(df_uk, "UK")
long_es <- to_long(df_es, "Spain")

# ---- Build standardized outputs ----

# Spain mappings (yearly)
out_es <- bind_rows(
    pick_category(long_es, "^\\s*22\\.2\\s+Atentados", "Violence_Officials"),
    # Terrorism row may or may not exist in your sheet; this will just return 0 rows if missing
    pick_category(long_es, "^\\s*22\\.7\\s+Organizaciones\\s+y\\s+grupos\\s+terroristas", "Terrorism"),
    pick_category(long_es, "^\\s*17\\.2\\s+Incend", "Incendies"),
    pick_category(long_es, "^\\s*13\\.9\\s+Da[ñn]os", "Destruction_Other"),
    pick_category(long_es, "^\\s*13\\.2\\s+Robos", "Burglary"),
    pick_category(long_es, "^\\s*13\\.4\\s+Robo\\s+y\\s+hurto", "Vehicle_Theft")
)

# Austria mappings (yearly)
out_at <- bind_rows(
    pick_austria_violence_officials(long_at),
    pick_category(long_at, "BRANDSTIFTUNG\\s*§\\s*169", "Incendies"),
    pick_category(long_at, "SACHBESCH[ÄA]DIGUNG\\s*§\\s*125", "Destruction_Other"),
    pick_category(long_at, "DIEBSTAHL\\s+DURCH\\s+EINBRUCH", "Burglary"),
    pick_category(long_at, "UNBEFUGTER\\s+GEBRAUCH\\s+VON\\s+FAHRZEUGEN", "Vehicle_Theft"),
    # Map Public Safety to Terrorism
    pick_category(long_at, "VORS[ÄA]TZLICHE\\s+GEMEINGEF[ÄA]HRDUNG\\s*§\\s*176", "Terrorism")
)

# UK mappings (annual or quarterly in source, but your sheet is annual totals by year)
out_uk <- bind_rows(
    # Updated regex to be more flexible/robust
    pick_category(long_uk, "Assault.*on\\s+constable", "Violence_Officials"),
    pick_category(long_uk, "^\\s*Arson\\s*$", "Incendies"),
    pick_category(long_uk, "^\\s*Criminal\\s+Damage\\s*$", "Destruction_Other"),
    pick_category(long_uk, "^\\s*Burglary\\s*$", "Burglary"),
    pick_category(long_uk, "^\\s*Theft\\s+or\\s+unauthorised\\s+taking\\s+of\\s+a\\s+motor\\s+vehicle\\s*$", "Vehicle_Theft")
)

# Combine and order
final <- bind_rows(out_es, out_at, out_uk) %>%
    arrange(year, country, category_standardized)

# Verify no duplicates
dups <- final %>%
    count(year, country, category_standardized) %>%
    filter(n > 1)

if (nrow(dups) > 0) {
    warning("Found duplicates in standardized output. Inspect `dups` object.")
}

# Write CSV
write.csv(final, "data/processed/standardized_counts_donor.csv", row.names = FALSE)

print(final)
