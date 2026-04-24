# ==============================================================================
# Title: Data Inspection Utility
# Purpose: Quick inspection of raw GTD and DataGouv data files.
# Author: Luan Blakaj 齐凯
# ==============================================================================

library(readxl)
library(tidyverse)

# Inspect GTD
print("--- GTD DATA ---")
gtd_path <- "data/globalterrorismdb_FR_20102021.xlsx"
if (file.exists(gtd_path)) {
    df_gtd <- read_excel(gtd_path, n_max = 5)
    print(glimpse(df_gtd))
} else {
    print("GTD file not found")
}

# Inspect Datagouv
print("--- DATAGOUV DATA ---")
dg_path <- "data/serieschrono-datagouv.csv"
if (file.exists(dg_path)) {
    # Read just the first few lines to avoid huge load
    df_dg <- read_delim(dg_path, delim = ";", n_max = 5, show_col_types = FALSE)
    print(glimpse(df_dg))
} else {
    print("Datagouv file not found")
}
