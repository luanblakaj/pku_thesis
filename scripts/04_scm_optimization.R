# ==============================================================================
# Title: Advanced SCM Fitting
# Purpose: Apply log-transformation and optimal weighting for synthetic control.
# Author: Luan Blakaj 齐凯
# ==============================================================================

library(tidyverse)
library(readxl)
source("R/theme_pku.R")

# 1. Load Data ----------------------------------------------------------------
df_master <- read_csv("data/processed/scm_master_dataset_annual.csv", show_col_types = FALSE)

TRAIN_START <- 2010
TRAIN_END <- 2015

# 2. Transformation: Log-Scale ------------------------------------------------
df_log <- df_master %>%
    mutate(log_count = log(count + 1))

# 3. Optimization: Finding the "Synthetic France" Weights ---------------------


create_synthetic_control <- function(category_name) {
    print(paste("Optimizing for:", category_name))

    df_wide <- df_log %>%
        filter(category_standardized == category_name) %>%
        select(year, country, log_count) %>%
        pivot_wider(names_from = country, values_from = log_count) %>%
        filter(year >= TRAIN_START & year <= 2024) %>%
        fill(everything(), .direction = "up")

    available_donors <- setdiff(names(df_wide), c("year", "France"))

    if (length(available_donors) == 0 || !"France" %in% names(df_wide)) {
        print("Skipping: Insufficient data for this category.")
        return(NULL)
    }

    df_train <- df_wide %>% filter(year <= TRAIN_END)

    donors_formula <- paste(available_donors, collapse = " + ")
    formula_str <- paste("France ~", donors_formula)

    print(paste("Formula:", formula_str))

    model <- lm(as.formula(formula_str), data = df_train)

    df_wide$Synthetic_France_Log <- predict(model, newdata = df_wide)


    train_indices <- which(df_wide$year <= TRAIN_END)

    mean_real <- mean(df_wide$France[train_indices], na.rm = TRUE)
    mean_syn <- mean(df_wide$Synthetic_France_Log[train_indices], na.rm = TRUE)

    gap <- mean_real - mean_syn
    df_wide$Synthetic_France_Log <- df_wide$Synthetic_France_Log + gap

    df_wide <- df_wide %>%
        mutate(
            France_Real = exp(France) - 1,
            Synthetic_France = exp(Synthetic_France_Log) - 1
        )

    df_plot <- df_wide %>%
        select(year, France_Real, Synthetic_France) %>%
        pivot_longer(cols = c("France_Real", "Synthetic_France"), names_to = "Type", values_to = "Count") %>%
        mutate(category = category_name)

    return(df_plot)
}

# 4. Run Optimization for Key Categories --------------------------------------
categories_to_fit <- c("Violence_Officials", "Incendies", "Terrorism", "Burglary", "Vehicle_Theft", "Destruction_Other")


results_list <- list()

for (cat in categories_to_fit) {
    # Check if category exists in France AND at least one Donor
    check <- df_master %>% filter(category_standardized == cat)
    if ("France" %in% check$country && n_distinct(check$country) > 1) {
        results_list[[cat]] <- create_synthetic_control(cat)
    }
}

df_synthetic_final <- bind_rows(results_list)


df_monthly_france <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>% # Match the annual window
    mutate(category = case_when(
        category == "Violence_Officials" ~ "Violence_Officials",
        category == "Incendies" ~ "Incendies",
        category == "Destruction_Other" ~ "Destruction_Other",
        category == "Burglary" ~ "Burglary",
        category == "Vehicle_Theft" ~ "Vehicle_Theft",
        category == "Terrorism_AIFN" ~ "Terrorism",
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(category)) %>%
    mutate(category_label = category_display(category)) %>%
    select(date, category, count) %>%
    rename(France_Real = count)

df_synthetic_monthly_view <- df_synthetic_final %>%
    filter(Type == "Synthetic_France") %>% # Filter for the synthetic rows
    mutate(
        # Create a date for the step plot (start of year)
        date = as.Date(paste0(year, "-01-01")),
        # Normalize Annual -> Monthly Average
        Synthetic_Monthly_Avg = Count / 12 # Use 'Count' column
    ) %>%
    mutate(category_label = category_display(category))


print("--- DIAGNOSTICS: Violence_Officials Trend ---")
df_donors_log <- df_log %>%
    filter(category_standardized == "Violence_Officials") %>%
    group_by(country) %>%
    summarise(
        Growth_2016_2022 = (last(count) - count[year == 2016]) / count[year == 2016] * 100
    )
print(df_donors_log)
print("---------------------------------------------")

plot_hybrid <- ggplot() +
    geom_line(data = df_monthly_france, aes(x = date, y = France_Real, color = "France (Monthly Real)"), alpha = 0.6, size = 0.8) +

    geom_step(data = df_synthetic_monthly_view, aes(x = date, y = Synthetic_Monthly_Avg, color = "Synthetic (Monthly Avg)"), size = 1.2) +

    facet_wrap(~category_label, scales = "free_y") +
    theme_pku() +
    scale_color_manual(values = c("France (Monthly Real)" = pku_pal$accent, "Synthetic (Monthly Avg)" = pku_pal$navy)) +
    labs(
        title = "Hybrid SCM: Monthly Reality vs. Annual Synthetic Trend",
        subtitle = "Synthetic line = Annual Optimized Prediction / 12 months.",
        y = "Monthly Count", x = "Date"
    ) %>%
    add_watermark()

#Save ---------------------------------------------------------------------
write_csv(df_synthetic_final, "data/processed/scm_optimized_synthetic_france.csv")
ggsave("outputs/scm_optimized_fit.png", plot_hybrid, width = 14, height = 9, bg = "white")

print("--- Optimization Complete ---")
print("Diagnostics for Violence increase printed above.")
print("See 'outputs/scm_optimized_fit.png' for the Hybrid Plot.")
