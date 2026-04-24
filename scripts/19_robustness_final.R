# ==============================================================================
# Title: Final Robustness Suite
# Purpose: Comprehensive robustness checks for SCM results.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (grepl("scripts$", getwd())) {
    setwd("..")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, patchwork, broom, readxl)

source("R/theme_pku.R")
source("R/controls.R")

# ------------------------------------------------------------------------------
# 0) KONFIG
# ------------------------------------------------------------------------------
out_dir <- "outputs/robustness"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

data_path <- "data/processed/scm_master_dataset_annual.csv"
if (!file.exists(data_path)) stop("Missing required file: data/processed/scm_master_dataset_annual.csv")

df_scm <- read_csv(data_path, show_col_types = FALSE) %>%
    mutate(
        year = as.integer(year),
        count = as.numeric(count)
    ) %>%
    filter(year >= 2010, year <= 2022)

analysis_years <- sort(unique(df_scm$year))
countries <- sort(unique(df_scm$country))

treated_unit <- "France"
focal_outcome <- "Violence_Officials"
treatment_year <- 2015
placebo_years <- c(2011, 2012, 2013, 2014)

# ------------------------------------------------------------------------------
# 1) HILF
# ------------------------------------------------------------------------------
pretty_outcome <- function(x) {
    category_display(x)
}

fit_log_scm <- function(df, outcome, treated, t0, years = analysis_years, exclude_donors = character()) {
    df_cat <- df %>%
        filter(category_standardized == outcome, year %in% years) %>%
        transmute(
            year = as.integer(year),
            country,
            log_count = log(count + 1)
        )

    if (!(treated %in% unique(df_cat$country))) return(NULL)

    wide <- df_cat %>%
        pivot_wider(names_from = country, values_from = log_count) %>%
        complete(year = years) %>%
        arrange(year)

    if (!(treated %in% names(wide))) return(NULL)

    donor_cols <- setdiff(names(wide), c("year", treated, exclude_donors))
    if (length(donor_cols) == 0) return(NULL)

    wide <- wide %>%
        select(year, all_of(c(treated, donor_cols))) %>%
        tidyr::fill(all_of(c(treated, donor_cols)), .direction = "downup")

    df_train <- wide %>% filter(year < t0)
    if (nrow(df_train) == 0) return(NULL)

    model_formula <- as.formula(
        paste0(
            "`", treated, "` ~ ",
            paste0("`", donor_cols, "`", collapse = " + ")
        )
    )

    model <- suppressWarnings(lm(model_formula, data = df_train))
    pred_log <- suppressWarnings(as.numeric(predict(model, newdata = wide)))
    if (all(is.na(pred_log))) return(NULL)

    pre_idx <- wide$year < t0
    shift <- mean(wide[[treated]][pre_idx], na.rm = TRUE) - mean(pred_log[pre_idx], na.rm = TRUE)
    synthetic_log <- pred_log + shift

    real <- exp(wide[[treated]]) - 1
    synthetic <- exp(synthetic_log) - 1
    gap <- real - synthetic

    pre_rmspe <- sqrt(mean(gap[pre_idx]^2, na.rm = TRUE))
    post_rmspe <- sqrt(mean(gap[!pre_idx]^2, na.rm = TRUE))
    rho <- ifelse(is.na(pre_rmspe) || pre_rmspe == 0, NA_real_, post_rmspe / pre_rmspe)

    donor_weights <- broom::tidy(model) %>%
        filter(term != "(Intercept)") %>%
        transmute(
            donor = str_replace_all(term, "`", ""),
            weight = estimate
        ) %>%
        right_join(tibble(donor = donor_cols), by = "donor") %>%
        mutate(
            weight = replace_na(weight, 0),
            abs_weight = abs(weight)
        ) %>%
        arrange(desc(abs_weight))

    series <- tibble(
        year = wide$year,
        treated = treated,
        outcome = outcome,
        treatment_year = t0,
        real = real,
        synthetic = synthetic,
        gap = gap,
        period = if_else(year < t0, "Pre", "Post")
    )

    list(
        series = series,
        donor_weights = donor_weights,
        pre_rmspe = pre_rmspe,
        post_rmspe = post_rmspe,
        rho = rho
    )
}

save_plot <- function(plot_obj, file_name, width = 11, height = 7) {
    suppressWarnings(
        ggsave(
            filename = file.path(out_dir, file_name),
            plot = plot_obj,
            width = width,
            height = height,
            bg = "white"
        )
    )
}

# ------------------------------------------------------------------------------
# 2) ROBUSTHEIT 1: IM-ROOM-PLACEBO-TESCHT (PERMUTATION)
# ------------------------------------------------------------------------------
space_tbl <- tibble(unit = countries) %>%
    mutate(fit = map(unit, ~ fit_log_scm(df_scm, focal_outcome, treated = .x, t0 = treatment_year))) %>%
    filter(map_lgl(fit, ~ !is.null(.x)))

space_series <- space_tbl %>%
    transmute(unit, series = map(fit, "series")) %>%
    unnest(series)

space_metrics <- space_tbl %>%
    transmute(
        treated = unit,
        pre_rmspe = map_dbl(fit, "pre_rmspe"),
        post_rmspe = map_dbl(fit, "post_rmspe"),
        rho = map_dbl(fit, "rho")
    ) %>%
    mutate(is_france = treated == treated_unit)

rho_france <- space_metrics %>% filter(treated == treated_unit) %>% pull(rho)
n_donors <- sum(space_metrics$treated != treated_unit)
p_randomization <- (1 + sum(space_metrics$treated != treated_unit & space_metrics$rho >= rho_france, na.rm = TRUE)) / (1 + n_donors)

write_csv(space_metrics, file.path(out_dir, "R1_placebo_space_metrics.csv"))

p_space_gap <- ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_line(
        data = space_series %>% filter(unit != treated_unit),
        aes(x = year, y = gap, group = unit),
        color = "grey70", linewidth = 0.8, alpha = 0.9
    ) +
    geom_line(
        data = space_series %>% filter(unit == treated_unit),
        aes(x = year, y = gap),
        color = pku_pal$accent, linewidth = 1.4
    ) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    labs(
        title = "In-Space Placebo Test: Gap Trajectories",
        subtitle = paste0(
            "Outcome: ", pretty_outcome(focal_outcome),
            " | Treatment year = ", treatment_year,
            " | Randomization p-value = ", formatC(p_randomization, digits = 3, format = "f")
        ),
        x = "Year",
        y = "Gap (Real - Synthetic)",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

save_plot(p_space_gap, "R1_placebo_spaghetti_gap.png", width = 11, height = 6.5)

p_space_rho <- space_metrics %>%
    ggplot(aes(x = rho, y = fct_reorder(treated, rho), color = is_france)) +
    geom_vline(xintercept = rho_france, linetype = "dashed", color = pku_pal$accent) +
    geom_point(size = 3) +
    geom_text(
        aes(label = round(rho, 2)),
        nudge_x = 0.08, hjust = 0, size = 3.5,
        family = "Times New Roman", show.legend = FALSE
    ) +
    scale_color_manual(values = c("TRUE" = pku_pal$accent, "FALSE" = "grey55"), labels = c("FALSE" = "Placebo Unit", "TRUE" = "France")) +
    labs(
        title = "RMSPE Ratio Distribution (Dotplot)",
        subtitle = "Ratio = Post-RMSPE / Pre-RMSPE. Dashed line marks France.",
        x = "RMSPE Ratio",
        y = "",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_space_rho, "R1_placebo_rho_dotplot.png", width = 10, height = 5.5)

# ------------------------------------------------------------------------------
# 3) ROBUSTHEIT 2: LAAN-EIS-WEG SPÄNDER-SENSITIVITÄT
# ------------------------------------------------------------------------------
fit_baseline <- fit_log_scm(df_scm, focal_outcome, treated = treated_unit, t0 = treatment_year)
if (is.null(fit_baseline)) stop("Baseline SCM fit failed for France.")

n_top_donors <- min(3, nrow(fit_baseline$donor_weights))
top_donors <- fit_baseline$donor_weights %>%
    arrange(desc(abs_weight)) %>%
    slice_head(n = n_top_donors) %>%
    pull(donor)

loo_tbl <- tibble(excluded_donor = top_donors) %>%
    mutate(
        fit = map(excluded_donor, ~ fit_log_scm(
            df_scm, focal_outcome,
            treated = treated_unit, t0 = treatment_year,
            exclude_donors = .x
        ))
    ) %>%
    filter(map_lgl(fit, ~ !is.null(.x)))

write_csv(fit_baseline$donor_weights, file.path(out_dir, "R2_baseline_donor_weights.csv"))

loo_series <- bind_rows(
    fit_baseline$series %>%
        transmute(year, series = "France (Observed)", value = real),
    fit_baseline$series %>%
        transmute(year, series = "Synthetic (Baseline)", value = synthetic),
    loo_tbl %>%
        transmute(series = paste0("Synthetic (w/o ", excluded_donor, ")"), tmp = map(fit, "series")) %>%
        unnest(tmp) %>%
        transmute(year, series, value = synthetic)
)

series_levels <- unique(loo_series$series)
loo_colors <- setNames(rep("grey65", length(series_levels)), series_levels)
loo_colors["France (Observed)"] <- pku_pal$accent
loo_colors["Synthetic (Baseline)"] <- pku_pal$navy

loo_linetypes <- setNames(rep("solid", length(series_levels)), series_levels)
loo_linetypes[grepl("^Synthetic \\(w/o", names(loo_linetypes))] <- "dashed"

p_loo <- ggplot(loo_series, aes(x = year, y = value, color = series, linetype = series)) +
    geom_line(linewidth = 1.05) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black") +
    scale_color_manual(values = loo_colors) +
    scale_linetype_manual(values = loo_linetypes) +
    labs(
        title = "Leave-One-Out Donor Sensitivity",
        subtitle = paste0(
            "Top baseline donors removed one-by-one: ",
            paste(top_donors, collapse = ", ")
        ),
        x = "Year",
        y = "Annual Count",
        color = "",
        linetype = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_loo, "R2_leave_one_out_paths.png", width = 11, height = 6.5)

# ------------------------------------------------------------------------------
# 4) ROBUSTHEIT 3: IN-ZIT-PLACEBO (FALSCHI BEHANDLIGSJOHR)
# ------------------------------------------------------------------------------
time_tbl <- tibble(t0 = c(placebo_years, treatment_year)) %>%
    mutate(
        fit = map(t0, ~ fit_log_scm(df_scm, focal_outcome, treated = treated_unit, t0 = .x)),
        label = if_else(
            t0 == treatment_year,
            paste0("Real treatment (T0 = ", t0, ")"),
            paste0("Placebo treatment (T0 = ", t0, ")")
        )
    ) %>%
    filter(map_lgl(fit, ~ !is.null(.x)))

time_metrics <- time_tbl %>%
    transmute(
        treatment_year = t0,
        pre_rmspe = map_dbl(fit, "pre_rmspe"),
        post_rmspe = map_dbl(fit, "post_rmspe"),
        rho = map_dbl(fit, "rho")
    )

write_csv(time_metrics, file.path(out_dir, "R3_in_time_placebo_metrics.csv"))

time_series <- time_tbl %>%
    transmute(t0, label, series = map(fit, "series")) %>%
    unnest(series) %>%
    mutate(is_real = t0 == treatment_year)

p_time <- ggplot(time_series, aes(x = year, y = gap, color = is_real)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_line(linewidth = 0.95) +
    geom_vline(
        data = time_series %>% distinct(t0, label, is_real),
        aes(xintercept = t0, color = is_real),
        linetype = "dashed", show.legend = FALSE
    ) +
    facet_wrap(~label, ncol = 3) +
    scale_color_manual(values = c("TRUE" = pku_pal$accent, "FALSE" = "grey55"), guide = "none") +
    labs(
        title = "In-Time Placebo Test: Gap Panels",
        subtitle = "False treatment dates in the pre-period vs real treatment year",
        x = "Year",
        y = "Gap (Real - Synthetic)",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

save_plot(p_time, "R3_in_time_placebo_panels.png", width = 12, height = 7.5)

# ------------------------------------------------------------------------------
# 5) ROBUSTHEIT 4: ERGEBNIS-PLACEBOS (IIBRUCH + FAARZIGDIEBSTAHL)
# ------------------------------------------------------------------------------
outcome_placebos <- c("Burglary", "Vehicle_Theft")

outcome_tbl <- tibble(outcome = outcome_placebos) %>%
    mutate(fit = map(outcome, ~ fit_log_scm(df_scm, .x, treated = treated_unit, t0 = treatment_year))) %>%
    filter(map_lgl(fit, ~ !is.null(.x)))

outcome_metrics <- outcome_tbl %>%
    transmute(
        outcome,
        pre_rmspe = map_dbl(fit, "pre_rmspe"),
        post_rmspe = map_dbl(fit, "post_rmspe"),
        rho = map_dbl(fit, "rho")
    )
write_csv(outcome_metrics, file.path(out_dir, "R4_outcome_placebo_metrics.csv"))

# Placebo-im-Room p-Wärt für jedes Negativ-Kontrolle-Ergebnis
outcome_perm <- map_dfr(outcome_placebos, function(outcome_name) {
    tmp <- tibble(unit = countries) %>%
        mutate(fit = map(unit, ~ fit_log_scm(df_scm, outcome_name, treated = .x, t0 = treatment_year))) %>%
        filter(map_lgl(fit, ~ !is.null(.x)))

    m <- tmp %>%
        transmute(
            treated = unit,
            rho = map_dbl(fit, "rho")
        )

    rho_fr <- m %>% filter(treated == treated_unit) %>% pull(rho)
    n_d <- sum(m$treated != treated_unit)
    p_val <- (1 + sum(m$treated != treated_unit & m$rho >= rho_fr, na.rm = TRUE)) / (1 + n_d)

    tibble(
        outcome = outcome_name,
        france_rho = rho_fr,
        randomization_p = p_val
    )
})

write_csv(outcome_perm, file.path(out_dir, "R4_outcome_placebo_permutation_pvalues.csv"))

outcome_series <- outcome_tbl %>%
    transmute(
        outcome,
        series = map(fit, ~ .x$series %>% select(year, real, synthetic, gap))
    ) %>%
    unnest(series) %>%
    mutate(outcome_label = pretty_outcome(outcome))

outcome_paths <- outcome_series %>%
    select(year, outcome_label, real, synthetic) %>%
    pivot_longer(cols = c(real, synthetic), names_to = "series_type", values_to = "value") %>%
    mutate(
        series_type = recode(
            series_type,
            "real" = "France (Observed)",
            "synthetic" = "Synthetic France"
        )
    )

p_outcome_paths <- ggplot(outcome_paths, aes(x = year, y = value, color = series_type)) +
    geom_line(linewidth = 1.05) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black") +
    facet_wrap(~outcome_label, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("France (Observed)" = pku_pal$accent, "Synthetic France" = pku_pal$navy)) +
    labs(
        title = "Outcome Placebo SCM: France vs Synthetic",
        subtitle = "Negative-control outcomes: Burglary and Vehicle Theft",
        x = "Year",
        y = "Annual Count",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_outcome_paths, "R4_outcome_placebo_paths.png", width = 11, height = 6.8)

p_outcome_gaps <- ggplot(outcome_series, aes(x = year, y = gap)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_col(aes(fill = gap > 0), alpha = 0.65, width = 0.7, show.legend = FALSE) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    facet_wrap(~outcome_label, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = c("TRUE" = pku_pal$accent, "FALSE" = "grey70")) +
    labs(
        title = "Outcome Placebo SCM: Gap Plots",
        subtitle = "Gap = France - Synthetic. Positive bars indicate excess incidents.",
        x = "Year",
        y = "Gap (Real - Synthetic)",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

save_plot(p_outcome_gaps, "R4_outcome_placebo_gaps.png", width = 11, height = 6.8)

# Negativ-Kontrolle-Differänzlucke: D_t = Lück_Agriff - Lück_Iibruch
gap_assault <- fit_baseline$series %>% select(year, gap_assault = gap)
gap_burglary <- outcome_tbl %>%
    filter(outcome == "Burglary") %>%
    mutate(series = map(fit, ~ .x$series %>% select(year, gap))) %>%
    unnest(series) %>%
    select(year, gap_burglary = gap)

df_dt <- gap_assault %>%
    left_join(gap_burglary, by = "year") %>%
    mutate(D_t = gap_assault - gap_burglary)

write_csv(df_dt, file.path(out_dir, "R4_negative_control_difference_gap.csv"))

p_dt <- ggplot(df_dt, aes(x = year, y = D_t)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_col(fill = pku_pal$accent, alpha = 0.7, width = 0.7) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    labs(
        title = "Negative-Control Difference Gap",
        subtitle = "D_t = Gap_assault - Gap_burglary",
        x = "Year",
        y = "Difference Gap",
        caption = std_caption
    ) +
    theme_pku() %>%
    add_watermark()

save_plot(p_dt, "R4_negative_control_Dt.png", width = 10.5, height = 5.5)

# ------------------------------------------------------------------------------
# 6) ROBUSTHEIT 5: KONTEXTUELLI KOVARIATE-IBERLAGERIG (NU FRANKRICH-KONTROLLE)
# ------------------------------------------------------------------------------
controls_annual <- load_controls()$annual %>%
    mutate(year = year(date)) %>%
    select(year, control_unemployment, control_asylum)

df_context <- fit_baseline$series %>%
    select(year, gap) %>%
    rename(gap_assault = gap) %>%
    left_join(controls_annual, by = "year") %>%
    mutate(
        z_gap = as.numeric(scale(gap_assault)),
        z_unemployment = as.numeric(scale(control_unemployment)),
        z_asylum = as.numeric(scale(log1p(control_asylum)))
    ) %>%
    select(year, z_gap, z_unemployment, z_asylum) %>%
    pivot_longer(
        cols = c(z_gap, z_unemployment, z_asylum),
        names_to = "series",
        values_to = "z_value"
    ) %>%
    mutate(
        series = recode(
            series,
            "z_gap" = "SCM Gap (Assaults), z-score",
            "z_unemployment" = "Unemployment (France), z-score",
            "z_asylum" = "Asylum Applications (France, log-z)"
        )
    )

write_csv(df_context, file.path(out_dir, "R5_contextual_overlay_data.csv"))

p_context <- ggplot(df_context, aes(x = year, y = z_value, color = series)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_line(linewidth = 1.0) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    scale_color_manual(values = c(
        "SCM Gap (Assaults), z-score" = pku_pal$accent,
        "Unemployment (France), z-score" = pku_pal$navy,
        "Asylum Applications (France, log-z)" = pku_pal$mono[2]
    )) +
    labs(
        title = "Contextual Covariate Overlay (France Only)",
        subtitle = "Not a full covariate SCM: donor-side covariates are unavailable in this dataset.",
        x = "Year",
        y = "Standardized Value (z-score)",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_context, "R5_contextual_controls_overlay.png", width = 11, height = 6.2)

# ------------------------------------------------------------------------------
# 7) ROBUSTHEIT 6: MULTI-ERGEBNIS HETEROGENITETS-PANEL
# ------------------------------------------------------------------------------
heterogeneity_outcomes <- c("Violence_Officials", "Terrorism", "Incendies", "Burglary", "Vehicle_Theft")

heterogeneity_tbl <- tibble(outcome = heterogeneity_outcomes) %>%
    mutate(fit = map(outcome, ~ fit_log_scm(df_scm, .x, treated = treated_unit, t0 = treatment_year))) %>%
    filter(map_lgl(fit, ~ !is.null(.x)))

heterogeneity_metrics <- heterogeneity_tbl %>%
    transmute(
        outcome,
        pre_rmspe = map_dbl(fit, "pre_rmspe"),
        post_rmspe = map_dbl(fit, "post_rmspe"),
        rho = map_dbl(fit, "rho")
    )
write_csv(heterogeneity_metrics, file.path(out_dir, "R6_heterogeneity_metrics.csv"))

heterogeneity_series <- heterogeneity_tbl %>%
    transmute(
        outcome,
        series = map(fit, ~ .x$series %>% select(year, real, synthetic, gap))
    ) %>%
    unnest(series) %>%
    mutate(outcome_label = pretty_outcome(outcome))

heterogeneity_paths <- heterogeneity_series %>%
    select(year, outcome_label, real, synthetic) %>%
    pivot_longer(cols = c(real, synthetic), names_to = "series_type", values_to = "value") %>%
    mutate(
        series_type = recode(
            series_type,
            "real" = "France (Observed)",
            "synthetic" = "Synthetic France"
        )
    )

p_heterogeneity <- ggplot(heterogeneity_paths, aes(x = year, y = value, color = series_type)) +
    geom_line(linewidth = 0.95) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black") +
    facet_wrap(~outcome_label, scales = "free_y", ncol = 3) +
    scale_color_manual(values = c("France (Observed)" = pku_pal$accent, "Synthetic France" = pku_pal$navy)) +
    labs(
        title = "Outcome Heterogeneity Panel: France vs Synthetic",
        subtitle = "Focal outcome, related categories, and negative controls",
        x = "Year",
        y = "Annual Count",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_heterogeneity, "R6_heterogeneity_paths_panel.png", width = 12, height = 8.2)

# ------------------------------------------------------------------------------
# 8) FORMLE + ERGEBNIS-NOTIZ (KAPITEL-FERTIG)
# ------------------------------------------------------------------------------
summary_lines <- c(
    "# Robustness Suite (Section 5.4)",
    "",
    paste0("Generated on: ", Sys.Date()),
    paste0("Treatment year used in SCM robustness: T0 = ", treatment_year),
    "",
    "## Core formulas",
    "Gap_j,t = Y_j,t - Yhat_j,t^SCM",
    "RMSPE_pre_j = sqrt( (1/(T0-1)) * sum_{t<T0}(Gap_j,t^2) )",
    "RMSPE_post_j = sqrt( (1/(T-T0+1)) * sum_{t>=T0}(Gap_j,t^2) )",
    "rho_j = RMSPE_post_j / RMSPE_pre_j",
    "p_randomization = (1 + #{j: rho_j >= rho_France}) / (1 + N)",
    "",
    "## In-space placebo (focal outcome)",
    paste0("France rho: ", round(rho_france, 4)),
    paste0("Randomization p-value: ", round(p_randomization, 4)),
    "",
    "## Outcome placebo permutation p-values",
    paste0(
        outcome_perm$outcome, ": p = ", formatC(outcome_perm$randomization_p, digits = 4, format = "f"),
        collapse = "\n"
    ),
    "",
    "## Notes",
    "- Covariate augmentation is presented as contextual overlay only because covariates are available for France but not donors.",
    "- All figures follow theme_pku style and include global footer/watermark."
)

writeLines(summary_lines, file.path(out_dir, "robustness_summary_5_4.md"))

# ------------------------------------------------------------------------------
# 9) FOALKALISERIE FER KAPITELTABÄLLE SPICHERE
# ------------------------------------------------------------------------------
write_csv(fit_baseline$series, file.path(out_dir, "R0_focal_gap_series.csv"))

print("------------------------------------------------------------")
print("19_robustness_final.R complete.")
print(paste("Outputs saved to:", out_dir))
print("------------------------------------------------------------")
