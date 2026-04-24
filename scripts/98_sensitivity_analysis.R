# ==============================================================================
# Title: TOST Sensitivity Analysis
# Purpose: Test TOST stability at 3.5 SD bound across all treatment dates.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, broom)
source("R/controls.R")

# 1. LOAD DATA -----------------------------------------------------------------
print("Loading data...")
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>%
    filter(category == "Violence_Officials")

controls <- load_controls()

df_quarterly <- df_monthly %>%
    mutate(quarter_date = as.Date(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    arrange(quarter_date) %>%
    rename(date = quarter_date) %>%
    left_join(controls$quarterly, by = "date")

# 2. CONFIG --------------------------------------------------------------------
treatments <- list(
    "FR2014_CT" = list(date = as.Date("2014-11-13"), name = "2014 Counter-Terrorism Law"),
    "FR2015_INTEL" = list(date = as.Date("2015-07-24"), name = "2015 Intelligence Act"),
    "FR2015_ESTATE" = list(date = as.Date("2015-11-20"), name = "2015 State of Emergency"),
    "FR2017_SILT" = list(date = as.Date("2017-10-30"), name = "2017 Internal Security Act"),
    "FR2021_PACTI" = list(date = as.Date("2021-07-30"), name = "2021 PACTI Law")
)

TEST_BOUND_SD <- 3.5 # The requested test value
print(paste("Running TOST with Stability Bound:", TEST_BOUND_SD, "SD"))

# 3. RUN TEST ------------------------------------------------------------------
results_tost <- data.frame()

for (code in names(treatments)) {
    t_date <- treatments[[code]]$date
    t_name <- treatments[[code]]$name

    # Window: +/- 4 Years
    window_start <- t_date - years(4)
    window_end <- t_date + years(4)

    # Stats
    df_pre <- df_quarterly %>%
        filter(date < t_date) %>%
        filter(!is.na(control_unemployment), !is.na(control_asylum))
    delta_val <- sd(df_pre$count, na.rm = T) * TEST_BOUND_SD

    # Model
    df_model <- df_quarterly %>%
        filter(date >= window_start & date <= window_end) %>%
        mutate(Post = ifelse(date >= t_date, 1, 0)) %>%
        filter(!is.na(control_unemployment), !is.na(control_asylum))

    model <- lm(count ~ Post + control_unemployment + control_asylum, data = df_model)
    res_tidy <- tidy(model)

    beta <- res_tidy %>%
        filter(term == "Post") %>%
        pull(estimate)
    se <- res_tidy %>%
        filter(term == "Post") %>%
        pull(std.error)
    df_df <- df.residual(model)

    # TOST
    t1 <- (beta - (-delta_val)) / se
    p1 <- pt(t1, df_df, lower.tail = FALSE)
    t2 <- (beta - delta_val) / se
    p2 <- pt(t2, df_df, lower.tail = TRUE)
    p_tost <- max(p1, p2)

    results_tost <- rbind(results_tost, data.frame(
        Law = code,
        Bound_SD = TEST_BOUND_SD,
        P_Value = round(p_tost, 5),
        Significant_Stability = p_tost < 0.05
    ))
}

print("--- RESULTS (3.5 SD) ---")
print(results_tost)
