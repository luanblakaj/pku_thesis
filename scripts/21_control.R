# ==============================================================================
# Title: Donor-Inclusive Control Robustness
# Purpose: SCM robustness with donor-inclusive unemployment and asylum controls.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (grepl("scripts$", getwd())) {
    setwd("..")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, broom)

source("R/theme_pku.R")

# ------------------------------------------------------------------------------
# 0) KONFIG
# ------------------------------------------------------------------------------
out_dir <- "outputs/robustness/controls"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

file_unemp_raw <- "data/une_rt_m_page_linear_2_0.csv"
file_asylum_raw <- "data/migr_asyappctzm_page_linear_2_0.csv"
file_scm <- "data/processed/scm_master_dataset_annual.csv"
file_monthly <- "data/processed/national_security_series_2010_2025_FULL.csv"

if (!file.exists(file_unemp_raw)) stop("Missing file: data/une_rt_m_page_linear_2_0.csv")
if (!file.exists(file_asylum_raw)) stop("Missing file: data/migr_asyappctzm_page_linear_2_0.csv")
if (!file.exists(file_scm)) stop("Missing file: data/processed/scm_master_dataset_annual.csv")
if (!file.exists(file_monthly)) stop("Missing file: data/processed/national_security_series_2010_2025_FULL.csv")

country_map <- c(
    "AT" = "Austria",
    "ES" = "Spain",
    "FR" = "France",
    "UK" = "UK"
)

focal_outcome <- "Violence_Officials"
treated_unit <- "France"
treatment_year <- 2015
t0_month <- as.Date("2015-11-01")
analysis_start <- as.Date("2010-01-01")
analysis_end <- as.Date("2022-12-01")

# ------------------------------------------------------------------------------
# 1) HILF
# ------------------------------------------------------------------------------
save_plot <- function(plot_obj, file_name, width = 11, height = 6.5) {
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

month_index <- function(dates, origin_date = min(dates, na.rm = TRUE)) {
    (year(dates) - year(origin_date)) * 12 + (month(dates) - month(origin_date)) + 1
}

load_control_raw <- function(path, value_name, country_map) {
    read_csv(path, show_col_types = FALSE) %>%
        transmute(
            geo = as.character(geo),
            country = recode(geo, !!!country_map),
            date = as.Date(paste0(TIME_PERIOD, "-01")),
            value = as.numeric(OBS_VALUE)
        ) %>%
        filter(!is.na(country), !is.na(date)) %>%
        group_by(country, date) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        rename(!!value_name := value)
}

fit_log_scm_annual <- function(df_focal, treated = "France", t0 = 2015, years = NULL) {
    # df_focal: Johr, Land, Aazahl
    if (is.null(years)) years <- sort(unique(df_focal$year))

    df_log <- df_focal %>%
        mutate(
            year = as.integer(year),
            log_count = log(as.numeric(count) + 1)
        ) %>%
        select(year, country, log_count)

    wide <- df_log %>%
        pivot_wider(names_from = country, values_from = log_count) %>%
        complete(year = years) %>%
        arrange(year)

    if (!(treated %in% names(wide))) return(NULL)

    donor_cols <- setdiff(names(wide), c("year", treated))
    if (length(donor_cols) == 0) return(NULL)

    wide <- wide %>%
        select(year, all_of(c(treated, donor_cols))) %>%
        tidyr::fill(all_of(c(treated, donor_cols)), .direction = "downup")

    df_train <- wide %>% filter(year < t0)
    if (nrow(df_train) < 2) return(NULL)

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

    real_count <- exp(wide[[treated]]) - 1
    synthetic_count <- exp(synthetic_log) - 1
    gap_count <- real_count - synthetic_count

    list(
        model = model,
        donors = donor_cols,
        series = tibble(
            year = wide$year,
            real_count = real_count,
            synthetic_count = synthetic_count,
            gap_count = gap_count,
            real_log = wide[[treated]],
            synthetic_log = synthetic_log,
            gap_log = wide[[treated]] - synthetic_log
        )
    )
}

fit_scm_value <- function(df_value, treated = "France", t0 = 2015, time_col = "date", log_transform = FALSE) {
    # df_value: Zit-Spalt, Land, Wärt
    t_sym <- rlang::sym(time_col)

    df_use <- df_value %>%
        transmute(
            time = !!t_sym,
            country,
            value_raw = as.numeric(value)
        )

    df_use <- if (log_transform) {
        df_use %>% mutate(value_model = log(value_raw + 1))
    } else {
        df_use %>% mutate(value_model = value_raw)
    }

    # Vollständigi monetligi Seqänz fer Robustheit ergänze
    if (inherits(df_use$time, "Date")) {
        full_time <- seq(min(df_use$time, na.rm = TRUE), max(df_use$time, na.rm = TRUE), by = "month")
    } else {
        full_time <- sort(unique(df_use$time))
    }

    wide <- df_use %>%
        select(time, country, value_model) %>%
        pivot_wider(names_from = country, values_from = value_model) %>%
        complete(time = full_time) %>%
        arrange(time)

    if (!(treated %in% names(wide))) return(NULL)

    donor_cols <- setdiff(names(wide), c("time", treated))
    if (length(donor_cols) == 0) return(NULL)

    # Fählendi Spänder- und Behandligswärt usfülle
    wide <- wide %>%
        select(time, all_of(c(treated, donor_cols))) %>%
        tidyr::fill(all_of(c(treated, donor_cols)), .direction = "downup")

    train_idx <- wide$time < t0
    df_train <- wide[train_idx, , drop = FALSE]
    if (nrow(df_train) < 12) return(NULL)

    model_formula <- as.formula(
        paste0(
            "`", treated, "` ~ ",
            paste0("`", donor_cols, "`", collapse = " + ")
        )
    )

    model <- suppressWarnings(lm(model_formula, data = df_train))
    pred_model <- suppressWarnings(as.numeric(predict(model, newdata = wide)))
    if (all(is.na(pred_model))) return(NULL)

    shift <- mean(wide[[treated]][train_idx], na.rm = TRUE) - mean(pred_model[train_idx], na.rm = TRUE)
    syn_model <- pred_model + shift

    if (log_transform) {
        real_value <- exp(wide[[treated]]) - 1
        syn_value <- exp(syn_model) - 1
    } else {
        real_value <- wide[[treated]]
        syn_value <- syn_model
    }

    list(
        model = model,
        donors = donor_cols,
        series = tibble(
            date = as.Date(wide$time),
            real_value = as.numeric(real_value),
            synthetic_value = as.numeric(syn_value),
            gap_value = as.numeric(real_value - syn_value),
            real_model = as.numeric(wide[[treated]]),
            synthetic_model = as.numeric(syn_model),
            gap_model = as.numeric(wide[[treated]] - syn_model)
        )
    )
}

fit_scm_on_residual <- function(df_resid, treated = "France", t0 = 2015, years = NULL) {
    # df_resid: Johr, Land, Residuum
    if (is.null(years)) years <- sort(unique(df_resid$year))

    wide <- df_resid %>%
        mutate(year = as.integer(year), resid = as.numeric(resid)) %>%
        select(year, country, resid) %>%
        pivot_wider(names_from = country, values_from = resid) %>%
        complete(year = years) %>%
        arrange(year)

    if (!(treated %in% names(wide))) return(NULL)

    donor_cols <- setdiff(names(wide), c("year", treated))
    if (length(donor_cols) == 0) return(NULL)

    wide <- wide %>%
        select(year, all_of(c(treated, donor_cols))) %>%
        tidyr::fill(all_of(c(treated, donor_cols)), .direction = "downup")

    df_train <- wide %>% filter(year < t0)
    if (nrow(df_train) < 2) return(NULL)

    model_formula <- as.formula(
        paste0(
            "`", treated, "` ~ ",
            paste0("`", donor_cols, "`", collapse = " + ")
        )
    )

    model <- suppressWarnings(lm(model_formula, data = df_train))
    pred <- suppressWarnings(as.numeric(predict(model, newdata = wide)))
    if (all(is.na(pred))) return(NULL)

    shift <- mean(wide[[treated]][wide$year < t0], na.rm = TRUE) - mean(pred[wide$year < t0], na.rm = TRUE)
    syn <- pred + shift

    list(
        model = model,
        series = tibble(
            year = wide$year,
            real_resid = wide[[treated]],
            synthetic_resid = syn,
            gap_resid = wide[[treated]] - syn
        )
    )
}

# ------------------------------------------------------------------------------
# 2) ROO-KONTROLLEDATEIE LADE + BERENIGE (ALLI SCM-LÄNDER)
# ------------------------------------------------------------------------------
controls_unemp <- load_control_raw(file_unemp_raw, "control_unemployment", country_map)
controls_asylum <- load_control_raw(file_asylum_raw, "control_asylum", country_map)

controls_monthly_raw <- full_join(controls_unemp, controls_asylum, by = c("country", "date")) %>%
    filter(date >= analysis_start, date <= analysis_end) %>%
    arrange(country, date)

imputation_log <- controls_monthly_raw %>%
    group_by(country) %>%
    summarise(
        missing_unemp_before = sum(is.na(control_unemployment)),
        missing_asylum_before = sum(is.na(control_asylum)),
        .groups = "drop"
    )

controls_monthly <- controls_monthly_raw %>%
    group_by(country) %>%
    arrange(date, .by_group = TRUE) %>%
    tidyr::fill(control_unemployment, control_asylum, .direction = "downup") %>%
    ungroup()

imputation_log <- imputation_log %>%
    left_join(
        controls_monthly %>%
            group_by(country) %>%
            summarise(
                missing_unemp_after = sum(is.na(control_unemployment)),
                missing_asylum_after = sum(is.na(control_asylum)),
                .groups = "drop"
            ),
        by = "country"
    )

controls_annual <- controls_monthly %>%
    mutate(year = year(date)) %>%
    group_by(country, year) %>%
    summarise(
        control_unemployment = mean(control_unemployment, na.rm = TRUE),
        control_asylum = sum(control_asylum, na.rm = TRUE),
        n_month = n_distinct(month(date)),
        .groups = "drop"
    )

coverage_monthly <- controls_monthly_raw %>%
    group_by(country) %>%
    summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE),
        n_month = n(),
        missing_unemp = sum(is.na(control_unemployment)),
        missing_asylum = sum(is.na(control_asylum)),
        .groups = "drop"
    )

write_csv(controls_monthly_raw, file.path(out_dir, "21_controls_clean_monthly_raw.csv"))
write_csv(controls_monthly, file.path(out_dir, "21_controls_clean_monthly_filled.csv"))
write_csv(controls_annual, file.path(out_dir, "21_controls_clean_annual.csv"))
write_csv(coverage_monthly, file.path(out_dir, "21_controls_coverage_monthly.csv"))
write_csv(imputation_log, file.path(out_dir, "21_controls_imputation_log.csv"))

# ------------------------------------------------------------------------------
# 3) BASISLINIE-ERGEBNIS SCM (JOHRLIG)
# ------------------------------------------------------------------------------
df_scm <- read_csv(file_scm, show_col_types = FALSE) %>%
    mutate(
        year = as.integer(year),
        count = as.numeric(count)
    ) %>%
    filter(
        category_standardized == focal_outcome,
        year >= 2010,
        year <= 2022
    ) %>%
    select(year, country, count)

analysis_years <- sort(unique(df_scm$year))
fit_outcome_base <- fit_log_scm_annual(df_scm, treated = treated_unit, t0 = treatment_year, years = analysis_years)
if (is.null(fit_outcome_base)) stop("Baseline outcome SCM failed")

write_csv(fit_outcome_base$series, file.path(out_dir, "21_focal_scm_gap_series_annual.csv"))
write_csv(tidy(fit_outcome_base$model), file.path(out_dir, "21_focal_scm_model_weights.csv"))

# ------------------------------------------------------------------------------
# 4) DONOR-INCLUSIVE CONTROL GAPS (MONTHLY SCM FOR CONTROLS)
# ------------------------------------------------------------------------------
control_unemp_long <- controls_monthly %>%
    select(date, country, value = control_unemployment)

control_asylum_long <- controls_monthly %>%
    select(date, country, value = control_asylum)

fit_unemp <- fit_scm_value(
    control_unemp_long,
    treated = treated_unit,
    t0 = t0_month,
    time_col = "date",
    log_transform = FALSE
)

fit_asylum <- fit_scm_value(
    control_asylum_long,
    treated = treated_unit,
    t0 = t0_month,
    time_col = "date",
    log_transform = TRUE
)

if (is.null(fit_unemp) || is.null(fit_asylum)) {
    stop("Control SCM fit failed (unemployment/asylum)")
}

write_csv(fit_unemp$series, file.path(out_dir, "21_unemployment_scm_series_monthly.csv"))
write_csv(fit_asylum$series, file.path(out_dir, "21_asylum_scm_series_monthly.csv"))
write_csv(tidy(fit_unemp$model), file.path(out_dir, "21_unemployment_scm_weights.csv"))
write_csv(tidy(fit_asylum$model), file.path(out_dir, "21_asylum_scm_weights.csv"))

# ------------------------------------------------------------------------------
# 5) R5-LIKE OVERLAY NOW USING DONOR-ADJUSTED CONTROL GAPS
# ------------------------------------------------------------------------------
df_monthly_focal <- read_csv(file_monthly, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(
        category == focal_outcome,
        year(date) >= 2010,
        year(date) <= 2022
    ) %>%
    select(date, count) %>%
    arrange(date)

monthly_gap <- df_monthly_focal %>%
    mutate(year = year(date)) %>%
    left_join(
        fit_outcome_base$series %>% select(year, synthetic_count),
        by = "year"
    ) %>%
    mutate(
        synthetic_monthly = synthetic_count / 12,
        gap_outcome = count - synthetic_monthly
    ) %>%
    select(date, gap_outcome)

context_monthly <- monthly_gap %>%
    left_join(
        fit_unemp$series %>% select(date, gap_unemployment = gap_value),
        by = "date"
    ) %>%
    left_join(
        fit_asylum$series %>% select(date, gap_asylum = gap_model),
        by = "date"
    ) %>%
    filter(!is.na(gap_unemployment), !is.na(gap_asylum)) %>%
    mutate(
        year = year(date)
    )

context_annual <- context_monthly %>%
    group_by(year) %>%
    summarise(
        gap_outcome = mean(gap_outcome, na.rm = TRUE),
        gap_unemployment = mean(gap_unemployment, na.rm = TRUE),
        gap_asylum = mean(gap_asylum, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        z_gap_outcome = as.numeric(scale(gap_outcome)),
        z_gap_unemployment = as.numeric(scale(gap_unemployment)),
        z_gap_asylum = as.numeric(scale(gap_asylum))
    ) %>%
    select(year, z_gap_outcome, z_gap_unemployment, z_gap_asylum) %>%
    pivot_longer(
        cols = c(z_gap_outcome, z_gap_unemployment, z_gap_asylum),
        names_to = "series",
        values_to = "z_value"
    ) %>%
    mutate(
        series = recode(
            series,
            "z_gap_outcome" = "SCM Outcome Gap (France - Synthetic)",
            "z_gap_unemployment" = "Unemployment Gap (France - Synthetic)",
            "z_gap_asylum" = "Asylum Gap (France - Synthetic, log scale)"
        )
    )

write_csv(context_monthly, file.path(out_dir, "21_context_gaps_monthly.csv"))
write_csv(context_annual, file.path(out_dir, "21_R5_contextual_overlay_data.csv"))

p_context <- ggplot(context_annual, aes(x = year, y = z_value, color = series)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_line(linewidth = 1.0) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    scale_color_manual(values = c(
        "SCM Outcome Gap (France - Synthetic)" = pku_pal$accent,
        "Unemployment Gap (France - Synthetic)" = pku_pal$navy,
        "Asylum Gap (France - Synthetic, log scale)" = pku_pal$mono[2]
    )) +
    labs(
        title = "Contextual Overlay with Donor-Adjusted Control Gaps",
        subtitle = "All series are France minus synthetic donor counterfactuals",
        x = "Year",
        y = "Standardized Value (z-score)",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_context, "21_R5_contextual_controls_overlay.png", width = 11, height = 6.2)

# ------------------------------------------------------------------------------
# 6) C-LIKE ITS ON GAP USING DONOR-ADJUSTED CONTROL GAPS
# ------------------------------------------------------------------------------
model_df <- context_monthly %>%
    mutate(
        t = month_index(date),
        rel_t = month_index(date, t0_month) - 1,
        post = as.integer(date >= t0_month),
        t_post = if_else(rel_t >= 0, rel_t, 0),
        z_unemployment_gap = as.numeric(scale(gap_unemployment)),
        z_asylum_gap = as.numeric(scale(gap_asylum))
    )

model_plain <- lm(gap_outcome ~ t + post + t_post, data = model_df)
model_cov <- lm(gap_outcome ~ t + post + t_post + z_unemployment_gap + z_asylum_gap, data = model_df)

coef_tbl <- bind_rows(
    tidy(model_plain, conf.int = TRUE) %>% mutate(spec = "ITS on gap (no controls)"),
    tidy(model_cov, conf.int = TRUE) %>% mutate(spec = "ITS on gap + synthetic control gaps")
) %>%
    filter(term %in% c("post", "t_post", "z_unemployment_gap", "z_asylum_gap")) %>%
    mutate(term = recode(
        term,
        "post" = "Post-level shift",
        "t_post" = "Post-trend shift",
        "z_unemployment_gap" = "Unemployment gap z-score",
        "z_asylum_gap" = "Asylum gap z-score"
    ))

fit_tbl <- bind_rows(
    glance(model_plain) %>% mutate(spec = "ITS on gap (no controls)"),
    glance(model_cov) %>% mutate(spec = "ITS on gap + synthetic control gaps")
) %>%
    select(spec, r.squared, adj.r.squared, sigma, AIC, BIC, nobs)

write_csv(model_df, file.path(out_dir, "21_gap_monthly_with_controls.csv"))
write_csv(coef_tbl, file.path(out_dir, "21_C_gap_its_covariate_coefficients.csv"))
write_csv(fit_tbl, file.path(out_dir, "21_C_gap_its_model_fit_stats.csv"))

p_coef <- ggplot(coef_tbl, aes(x = term, y = estimate, color = spec)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_point(position = position_dodge(width = 0.6), size = 2.6) +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        position = position_dodge(width = 0.6),
        width = 0.12,
        linewidth = 0.75
    ) +
    scale_color_manual(values = c(
        "ITS on gap (no controls)" = pku_pal$mono[2],
        "ITS on gap + synthetic control gaps" = pku_pal$accent
    )) +
    labs(
        title = "ITS on SCM Outcome Gap with Donor-Adjusted Controls",
        subtitle = "Controls enter as France-minus-synthetic gaps (monthly)",
        x = "",
        y = "Coefficient Estimate (95% CI)",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_coef, "21_C_gap_its_covariate_coefficients.png", width = 11, height = 6.2)

# ------------------------------------------------------------------------------
# 7) DONOR-INCLUSIVE COVARIATE-AUGMENTED SCM (RESIDUALIZATION)
# ------------------------------------------------------------------------------
panel_controls <- df_scm %>%
    left_join(
        controls_annual %>%
            filter(year >= 2010, year <= 2022) %>%
            select(country, year, control_unemployment, control_asylum),
        by = c("country", "year")
    ) %>%
    group_by(country) %>%
    arrange(year, .by_group = TRUE) %>%
    tidyr::fill(control_unemployment, control_asylum, .direction = "downup") %>%
    ungroup() %>%
    mutate(
        log_count = log(count + 1),
        log_asylum = log1p(control_asylum),
        year_index = year - min(year, na.rm = TRUE)
    )

pre_panel <- panel_controls %>% filter(year < treatment_year)
resid_model <- lm(log_count ~ country + year_index + control_unemployment + log_asylum, data = pre_panel)

panel_controls <- panel_controls %>%
    mutate(
        fitted_pre = as.numeric(predict(resid_model, newdata = panel_controls)),
        resid = log_count - fitted_pre
    )

fit_resid <- fit_scm_on_residual(
    panel_controls %>% select(year, country, resid),
    treated = treated_unit,
    t0 = treatment_year,
    years = analysis_years
)

if (is.null(fit_resid)) stop("Residualized SCM fit failed")

gap_compare <- fit_outcome_base$series %>%
    select(year, gap_log) %>%
    rename(gap = gap_log) %>%
    mutate(series = "Baseline SCM gap (log)") %>%
    bind_rows(
        fit_resid$series %>%
            select(year, gap = gap_resid) %>%
            mutate(series = "Residualized SCM gap (donor controls)")
    )

write_csv(panel_controls, file.path(out_dir, "21_panel_with_controls_and_residuals.csv"))
write_csv(tidy(resid_model, conf.int = TRUE), file.path(out_dir, "21_residualization_model_coefficients.csv"))
write_csv(gap_compare, file.path(out_dir, "21_gap_log_vs_residualized_comparison.csv"))

p_resid <- ggplot(gap_compare, aes(x = year, y = gap, color = series)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    scale_color_manual(values = c(
        "Baseline SCM gap (log)" = pku_pal$accent,
        "Residualized SCM gap (donor controls)" = pku_pal$navy
    )) +
    labs(
        title = "Baseline vs Residualized SCM Gap",
        subtitle = "Residualization uses donor-inclusive unemployment and asylum controls",
        x = "Year",
        y = "Gap",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_resid, "21_residualized_scm_gap_comparison.png", width = 11, height = 6)

# ------------------------------------------------------------------------------
# 8) SUMMARY NOTE
# ------------------------------------------------------------------------------
post_plain <- coef_tbl %>% filter(spec == "ITS on gap (no controls)", term == "Post-level shift")
post_cov <- coef_tbl %>% filter(spec == "ITS on gap + synthetic control gaps", term == "Post-level shift")
tpost_plain <- coef_tbl %>% filter(spec == "ITS on gap (no controls)", term == "Post-trend shift")
tpost_cov <- coef_tbl %>% filter(spec == "ITS on gap + synthetic control gaps", term == "Post-trend shift")

summary_lines <- c(
    "# 21_control.R summary (donor-inclusive controls)",
    "",
    paste0("Generated on: ", Sys.Date()),
    "",
    "Controls now used across all SCM countries (Austria, Spain, France, UK).",
    "Core outputs are donor-adjusted (France minus synthetic controls), not France-only.",
    "",
    "Reproduced outputs with donor-inclusive controls:",
    "- 21_R5_contextual_controls_overlay.png",
    "- 21_C_gap_its_covariate_coefficients.png",
    "",
    paste0(
        "Post-level shift (no controls): ", round(post_plain$estimate, 3),
        " | p=", formatC(post_plain$p.value, digits = 3, format = "f")
    ),
    paste0(
        "Post-level shift (with donor-adjusted controls): ", round(post_cov$estimate, 3),
        " | p=", formatC(post_cov$p.value, digits = 3, format = "f")
    ),
    paste0(
        "Post-trend shift (no controls): ", round(tpost_plain$estimate, 3),
        " | p=", formatC(tpost_plain$p.value, digits = 3, format = "f")
    ),
    paste0(
        "Post-trend shift (with donor-adjusted controls): ", round(tpost_cov$estimate, 3),
        " | p=", formatC(tpost_cov$p.value, digits = 3, format = "f")
    ),
    "",
    "Additional check:",
    "- 21_residualized_scm_gap_comparison.png"
)

writeLines(summary_lines, file.path(out_dir, "21_control_summary.md"))

print("------------------------------------------------------------")
print("21_control.R complete.")
print(paste("Outputs saved to:", out_dir))
print("------------------------------------------------------------")
