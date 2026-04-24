# ==============================================================================
# Title: Additional Robustness Checks
# Purpose: Pre-fit screening, shock exclusion, and covariate-augmented ITS.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (grepl("scripts$", getwd())) {
    setwd("..")
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, broom, patchwork)

source("R/theme_pku.R")
source("R/controls.R")

# ------------------------------------------------------------------------------
# 0) KONFIG
# ------------------------------------------------------------------------------
out_dir <- "outputs/robustness/additional"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

treated_unit <- "France"
focal_outcome <- "Violence_Officials"
treatment_year <- 2015
t0_month <- as.Date("2015-11-01")

shock_start <- as.Date("2018-10-01") # 2018Q4
shock_end <- as.Date("2019-12-01") # 2019Q4

fit_screen_c_grid <- c(5, 2, 1.5)
fit_screen_c_primary <- 2
tost_bound_sd <- 3.5
tost_window_years <- 4

treatments <- tibble(
    law_code = c("FR2014_CT", "FR2015_INTEL", "FR2015_ESTATE", "FR2017_SILT", "FR2021_PTR"),
    law_date = as.Date(c("2014-11-13", "2015-07-24", "2015-11-20", "2017-10-30", "2021-07-30"))
)

annual_path <- "data/processed/scm_master_dataset_annual.csv"
monthly_path <- "data/processed/national_security_series_2010_2025_FULL.csv"
if (!file.exists(annual_path)) stop("Missing data/processed/scm_master_dataset_annual.csv")
if (!file.exists(monthly_path)) stop("Missing data/processed/national_security_series_2010_2025_FULL.csv")

df_scm <- read_csv(annual_path, show_col_types = FALSE) %>%
    mutate(
        year = as.integer(year),
        count = as.numeric(count)
    ) %>%
    filter(year >= 2010, year <= 2022)

analysis_years <- sort(unique(df_scm$year))

df_monthly_focal <- read_csv(monthly_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010, year(date) <= 2022, category == focal_outcome) %>%
    select(date, count) %>%
    arrange(date)

controls_monthly <- load_controls()$monthly %>%
    mutate(date = as.Date(date))

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

fit_log_scm_panel <- function(df_panel, treated = "France", t0 = 2015, years = NULL, fill_treated = TRUE) {
    df_panel <- df_panel %>%
        mutate(
            year = as.integer(year),
            count = as.numeric(count),
            log_count = log(count + 1)
        ) %>%
        select(year, country, log_count)

    if (is.null(years)) years <- sort(unique(df_panel$year))

    wide <- df_panel %>%
        pivot_wider(names_from = country, values_from = log_count) %>%
        complete(year = years) %>%
        arrange(year)

    if (!(treated %in% names(wide))) return(NULL)

    donor_cols <- setdiff(names(wide), c("year", treated))
    if (length(donor_cols) == 0) return(NULL)

    wide[donor_cols] <- wide %>%
        select(all_of(donor_cols)) %>%
        tidyr::fill(everything(), .direction = "downup")

    if (fill_treated) {
        wide[[treated]] <- tidyr::fill(tibble(val = wide[[treated]]), val, .direction = "downup")$val
    }

    wide_use <- wide %>%
        filter(!is.na(.data[[treated]]))

    df_train <- wide_use %>% filter(year < t0)
    if (nrow(df_train) < 2) return(NULL)

    model_formula <- as.formula(
        paste0(
            "`", treated, "` ~ ",
            paste0("`", donor_cols, "`", collapse = " + ")
        )
    )

    model <- suppressWarnings(lm(model_formula, data = df_train))
    pred_log <- suppressWarnings(as.numeric(predict(model, newdata = wide_use)))
    if (all(is.na(pred_log))) return(NULL)

    pre_idx <- wide_use$year < t0
    if (sum(pre_idx, na.rm = TRUE) == 0) return(NULL)

    shift <- mean(wide_use[[treated]][pre_idx], na.rm = TRUE) - mean(pred_log[pre_idx], na.rm = TRUE)
    synthetic_log <- pred_log + shift

    real <- exp(wide_use[[treated]]) - 1
    synthetic <- exp(synthetic_log) - 1
    gap <- real - synthetic

    pre_rmspe <- sqrt(mean(gap[pre_idx]^2, na.rm = TRUE))
    post_rmspe <- sqrt(mean(gap[!pre_idx]^2, na.rm = TRUE))
    rho <- ifelse(is.na(pre_rmspe) || pre_rmspe == 0, NA_real_, post_rmspe / pre_rmspe)

    series <- tibble(
        year = wide_use$year,
        real = real,
        synthetic = synthetic,
        gap = gap,
        period = if_else(year < t0, "Pre", "Post")
    )

    list(
        model = model,
        donor_cols = donor_cols,
        series = series,
        pre_rmspe = pre_rmspe,
        post_rmspe = post_rmspe,
        rho = rho
    )
}

build_france_annual_from_monthly <- function(df_monthly) {
    df_monthly %>%
        mutate(year = year(date)) %>%
        group_by(year) %>%
        summarise(
            total = sum(count, na.rm = TRUE),
            months_present = n_distinct(month(date)),
            .groups = "drop"
        ) %>%
        mutate(
            count = if_else(
                months_present > 0,
                total * (12 / months_present),
                NA_real_
            )
        ) %>%
        select(year, count)
}

build_monthly_gap <- function(df_monthly, fit_obj, spec_label) {
    if (is.null(fit_obj)) return(tibble())

    syn_ann <- fit_obj$series %>%
        select(year, synthetic)

    df_monthly %>%
        mutate(year = year(date)) %>%
        left_join(syn_ann, by = "year") %>%
        mutate(
            synthetic_monthly = synthetic / 12,
            gap = count - synthetic_monthly,
            spec = spec_label
        ) %>%
        filter(!is.na(gap)) %>%
        select(date, year, count, synthetic_monthly, gap, spec)
}

run_its_gap <- function(df_gap, treatment_date) {
    t0 <- as.Date(paste0(year(treatment_date), "-", sprintf("%02d", month(treatment_date)), "-01"))
    if (sum(df_gap$date < t0) < 12 || sum(df_gap$date >= t0) < 12) return(NULL)

    origin <- min(df_gap$date, na.rm = TRUE)
    model_df <- df_gap %>%
        mutate(
            t = month_index(date, origin),
            rel_t = month_index(date, t0) - 1,
            post = as.integer(date >= t0),
            t_post = if_else(rel_t >= 0, rel_t, 0)
        )

    model <- lm(gap ~ t + post + t_post, data = model_df)
    out <- broom::tidy(model, conf.int = TRUE) %>%
        filter(term %in% c("post", "t_post")) %>%
        mutate(
            break_date = t0,
            n_obs = nrow(model_df)
        )
    out
}

run_tost_gap_quarterly <- function(df_gap, treatment_date, bound_sd = 3.5, window_years = 4) {
    t0 <- as.Date(paste0(year(treatment_date), "-", sprintf("%02d", month(treatment_date)), "-01"))

    df_q <- df_gap %>%
        mutate(q_date = as.Date(paste0(year(date), "-", sprintf("%02d", 3 * quarter(date) - 2), "-01"))) %>%
        group_by(q_date) %>%
        summarise(gap = sum(gap, na.rm = TRUE), .groups = "drop") %>%
        arrange(q_date)

    pre <- df_q %>% filter(q_date < t0)
    if (nrow(pre) < 8) return(NULL)

    delta <- sd(pre$gap, na.rm = TRUE) * bound_sd

    window <- df_q %>%
        filter(q_date >= (t0 - years(window_years)), q_date <= (t0 + years(window_years))) %>%
        mutate(Post = as.integer(q_date >= t0))

    if (nrow(window) < 12 || dplyr::n_distinct(window$Post) < 2) return(NULL)

    model <- lm(gap ~ Post, data = window)
    beta <- coef(summary(model))["Post", "Estimate"]
    se <- coef(summary(model))["Post", "Std. Error"]
    df_df <- df.residual(model)

    t1 <- (beta - (-delta)) / se
    p1 <- pt(t1, df_df, lower.tail = FALSE)
    t2 <- (beta - delta) / se
    p2 <- pt(t2, df_df, lower.tail = TRUE)
    p_tost <- max(p1, p2)

    tibble(
        break_date = t0,
        n_obs = nrow(window),
        beta_post = beta,
        se_post = se,
        delta = delta,
        p_tost = p_tost,
        stable = p_tost < 0.05
    )
}

# ------------------------------------------------------------------------------
# 2) A) VOR-AAPASSIGS-QUALITETSSCREANI FER PLACEBO-FOLGERIG
# ------------------------------------------------------------------------------
df_panel_focal <- df_scm %>%
    filter(category_standardized == focal_outcome) %>%
    select(year, country, count)

placebo_tbl <- tibble(unit = sort(unique(df_panel_focal$country))) %>%
    mutate(
        fit = map(unit, ~ fit_log_scm_panel(
            df_panel_focal,
            treated = .x,
            t0 = treatment_year,
            years = analysis_years,
            fill_treated = TRUE
        ))
    ) %>%
    filter(map_lgl(fit, ~ !is.null(.x)))

placebo_metrics <- placebo_tbl %>%
    transmute(
        treated = unit,
        pre_rmspe = map_dbl(fit, "pre_rmspe"),
        post_rmspe = map_dbl(fit, "post_rmspe"),
        rho = map_dbl(fit, "rho"),
        is_france = treated == treated_unit
    )

pre_fr <- placebo_metrics %>% filter(treated == treated_unit) %>% pull(pre_rmspe)
rho_fr <- placebo_metrics %>% filter(treated == treated_unit) %>% pull(rho)

placebo_all <- placebo_metrics %>% filter(!is_france)
n_all <- nrow(placebo_all)
p_all <- (1 + sum(placebo_all$rho >= rho_fr, na.rm = TRUE)) / (1 + n_all)

fit_screen_results <- tibble(c_tol = fit_screen_c_grid) %>%
    mutate(
        cutoff = c_tol * pre_fr,
        n_placebos = map_int(cutoff, ~ sum(placebo_all$pre_rmspe <= .x, na.rm = TRUE)),
        n_ge_fr = map_int(cutoff, ~ sum(placebo_all$pre_rmspe <= .x & placebo_all$rho >= rho_fr, na.rm = TRUE)),
        p_screened = if_else(
            n_placebos > 0,
            (1 + n_ge_fr) / (1 + n_placebos),
            NA_real_
        )
    ) %>%
    mutate(p_unscreened = p_all)

write_csv(placebo_metrics, file.path(out_dir, "A_placebo_metrics_raw.csv"))
write_csv(fit_screen_results, file.path(out_dir, "A_prefit_screen_pvalues.csv"))

screen_cutoff_primary <- fit_screen_c_primary * pre_fr
screened_primary <- placebo_all %>%
    mutate(kept = pre_rmspe <= screen_cutoff_primary)

p_screen_primary <- fit_screen_results %>%
    filter(c_tol == fit_screen_c_primary) %>%
    pull(p_screened)

p_prefit <- placebo_metrics %>%
    mutate(
        kept = case_when(
            is_france ~ TRUE,
            pre_rmspe <= screen_cutoff_primary ~ TRUE,
            TRUE ~ FALSE
        )
    ) %>%
    ggplot(aes(x = pre_rmspe, y = fct_reorder(treated, pre_rmspe), color = kept, shape = is_france)) +
    geom_vline(xintercept = pre_fr, linetype = "dotted", color = pku_pal$accent) +
    geom_vline(xintercept = screen_cutoff_primary, linetype = "dashed", color = "black") +
    geom_point(size = 3) +
    scale_color_manual(values = c("TRUE" = pku_pal$accent, "FALSE" = "grey70")) +
    scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 16)) +
    labs(
        title = "A) Pre-Fit RMSPE Screen for Placebo Inference",
        subtitle = paste0(
            "Rule: keep placebo units with RMSPE_pre_j <= ", fit_screen_c_primary, " x RMSPE_pre_FR. ",
            "p (unscreened) = ", formatC(p_all, digits = 3, format = "f"),
            ", p (screened) = ", formatC(p_screen_primary, digits = 3, format = "f")
        ),
        x = "Pre-treatment RMSPE",
        y = "",
        color = paste0("Kept under c = ", fit_screen_c_primary),
        shape = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_prefit, "A_prefit_rmspe_screen_dotplot.png", width = 10.5, height = 5.8)

prefit_sentence_1 <- paste0(
    "Without a pre-fit screen, the in-space placebo p-value is ",
    formatC(p_all, digits = 3, format = "f"), "."
)
prefit_sentence_2 <- paste0(
    "Applying RMSPE_pre_j <= ", fit_screen_c_primary, " x RMSPE_pre_FR yields p = ",
    formatC(p_screen_primary, digits = 3, format = "f"),
    " with ", sum(screened_primary$kept), " of ", nrow(screened_primary), " placebo units retained."
)

writeLines(
    c(prefit_sentence_1, prefit_sentence_2),
    file.path(out_dir, "A_prefit_two_sentence_summary.txt")
)

# ------------------------------------------------------------------------------
# 3) B) SCHOCK-USSCHLUSS-SENSITIVITET (2018Q4-2019Q4 USSELANDE)
# ------------------------------------------------------------------------------
df_monthly_shock_excl <- df_monthly_focal %>%
    filter(date < shock_start | date > shock_end)

donors_focal <- df_scm %>%
    filter(category_standardized == focal_outcome, country != treated_unit) %>%
    select(year, country, count)

fr_annual_baseline <- build_france_annual_from_monthly(df_monthly_focal) %>%
    mutate(country = treated_unit)

fr_annual_shock_excl <- build_france_annual_from_monthly(df_monthly_shock_excl) %>%
    mutate(country = treated_unit)

panel_baseline <- bind_rows(fr_annual_baseline, donors_focal) %>%
    filter(year >= 2010, year <= 2022)

panel_shock_excl <- bind_rows(fr_annual_shock_excl, donors_focal) %>%
    filter(year >= 2010, year <= 2022)

fit_baseline <- fit_log_scm_panel(
    panel_baseline,
    treated = treated_unit,
    t0 = treatment_year,
    years = analysis_years,
    fill_treated = TRUE
)

fit_shock_excl <- fit_log_scm_panel(
    panel_shock_excl,
    treated = treated_unit,
    t0 = treatment_year,
    years = analysis_years,
    fill_treated = FALSE
)

if (is.null(fit_baseline) || is.null(fit_shock_excl)) {
    stop("SCM fit failed in shock-exclusion block.")
}

gap_annual_compare <- bind_rows(
    fit_baseline$series %>% mutate(spec = "Baseline"),
    fit_shock_excl$series %>% mutate(spec = "Shock-excluded (drop 2018Q4-2019Q4)")
) %>%
    select(year, gap, spec)

write_csv(gap_annual_compare, file.path(out_dir, "B_gap_annual_compare.csv"))

p_gap_annual <- ggplot(gap_annual_compare, aes(x = year, y = gap, color = spec)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black") +
    scale_color_manual(values = c(
        "Baseline" = pku_pal$accent,
        "Shock-excluded (drop 2018Q4-2019Q4)" = pku_pal$navy
    )) +
    labs(
        title = "B) Annual SCM Gap Sensitivity to Mobilisation Shock Exclusion",
        subtitle = "Re-estimated SCM after dropping 2018Q4-2019Q4 from France monthly input",
        x = "Year",
        y = "Gap (Real - Synthetic)",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_gap_annual, "B_gap_annual_sensitivity.png", width = 11, height = 6)

monthly_gap_baseline <- build_monthly_gap(df_monthly_focal, fit_baseline, "Baseline")
monthly_gap_excl <- build_monthly_gap(df_monthly_shock_excl, fit_shock_excl, "Shock-excluded (drop 2018Q4-2019Q4)")
monthly_gap_compare <- bind_rows(monthly_gap_baseline, monthly_gap_excl)

write_csv(monthly_gap_compare, file.path(out_dir, "B_gap_monthly_compare.csv"))

p_gap_monthly <- ggplot(monthly_gap_compare, aes(x = date, y = gap, color = spec)) +
    annotate("rect", xmin = shock_start, xmax = shock_end, ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.25) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = t0_month, linetype = "dashed", color = "black") +
    scale_color_manual(values = c(
        "Baseline" = pku_pal$accent,
        "Shock-excluded (drop 2018Q4-2019Q4)" = pku_pal$navy
    )) +
    labs(
        title = "B) Monthly Gap Comparison",
        subtitle = "Grey band marks excluded mobilisation window (2018Q4-2019Q4)",
        x = "Date",
        y = "Gap (Monthly Real - Monthly Synthetic)",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_gap_monthly, "B_gap_monthly_sensitivity.png", width = 11.5, height = 6.2)

# ITS-Bruchteschts uf monetligi Lück
its_results <- bind_rows(
    treatments %>%
        mutate(
            spec = "Baseline",
            tmp = map(law_date, ~ run_its_gap(monthly_gap_baseline, .x))
        ) %>%
        unnest(tmp),
    treatments %>%
        mutate(
            spec = "Shock-excluded (drop 2018Q4-2019Q4)",
            tmp = map(law_date, ~ run_its_gap(monthly_gap_excl, .x))
        ) %>%
        unnest(tmp)
) %>%
    select(spec, law_code, break_date, term, estimate, std.error, conf.low, conf.high, p.value, n_obs)

write_csv(its_results, file.path(out_dir, "B_its_break_results.csv"))

p_its_level <- its_results %>%
    filter(term == "post") %>%
    ggplot(aes(x = law_code, y = estimate, color = spec)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_point(position = position_dodge(width = 0.5), size = 2.6) +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.5),
        linewidth = 0.75
    ) +
    scale_color_manual(values = c(
        "Baseline" = pku_pal$accent,
        "Shock-excluded (drop 2018Q4-2019Q4)" = pku_pal$navy
    )) +
    labs(
        title = "B) ITS Level-Shift Estimates on Monthly SCM Gap",
        subtitle = "Coefficient on post-law indicator (95% CI)",
        x = "",
        y = "Estimated Level Shift in Gap",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_its_level, "B_its_level_shift_comparison.png", width = 11, height = 6)

# TOST-Stabilitetsteschts uf quartalswisi Lück
tost_results <- bind_rows(
    treatments %>%
        mutate(
            spec = "Baseline",
            tmp = map(law_date, ~ run_tost_gap_quarterly(monthly_gap_baseline, .x, bound_sd = tost_bound_sd, window_years = tost_window_years))
        ) %>%
        unnest(tmp),
    treatments %>%
        mutate(
            spec = "Shock-excluded (drop 2018Q4-2019Q4)",
            tmp = map(law_date, ~ run_tost_gap_quarterly(monthly_gap_excl, .x, bound_sd = tost_bound_sd, window_years = tost_window_years))
        ) %>%
        unnest(tmp)
) %>%
    select(spec, law_code, break_date, beta_post, se_post, delta, p_tost, stable, n_obs)

write_csv(tost_results, file.path(out_dir, "B_tost_results.csv"))

p_tost <- tost_results %>%
  filter(law_code != "FR2014_CT") %>%
  mutate(
    label = paste0("p=", formatC(p_tost, digits = 3, format = "f"))
  ) %>%
  ggplot(aes(x = law_code, y = spec, fill = stable)) +
  geom_tile(color = "white", linewidth = 0.9) +
  geom_text(aes(label = label), size = 3.4, family = "Times New Roman", color = "white") +
  scale_fill_manual(
    values = c("TRUE" = pku_pal$navy, "FALSE" = pku_pal$accent),
    breaks = c(TRUE, FALSE),
    labels = c("Stable (TOST p<0.05)", "Not stable (p≥0.05)")
  ) +
  labs(
    title = "B) TOST Stability Tests on Quarterly SCM Gap",
    subtitle = paste0("Equivalence bound: +/- ", tost_bound_sd, " SD (stability if p<0.05)"),
    x = "", y = "", fill = "",
    caption = std_caption
  ) +
  theme_pku() +
  theme(legend.position = "bottom") %>%
  add_watermark()


save_plot(p_tost, "B_tost_stability_heatmap.png", width = 11, height = 4.6)

# ------------------------------------------------------------------------------
# 4) C) KOVARIATE-ERWIITERETI ITS UF LUCK (NU FRANKRICH-KONTROLLE)
# ------------------------------------------------------------------------------
df_cov <- monthly_gap_baseline %>%
    select(date, gap) %>%
    left_join(controls_monthly, by = "date") %>%
    filter(!is.na(control_unemployment), !is.na(control_asylum)) %>%
    mutate(
        z_unemployment = as.numeric(scale(control_unemployment)),
        logz_asylum = as.numeric(scale(log1p(control_asylum))),
        t = month_index(date),
        rel_t = month_index(date, t0_month) - 1,
        post = as.integer(date >= t0_month),
        t_post = if_else(rel_t >= 0, rel_t, 0)
    )

model_its_plain <- lm(gap ~ t + post + t_post, data = df_cov)
model_its_cov <- lm(gap ~ t + post + t_post + z_unemployment + logz_asylum, data = df_cov)

coef_cov <- bind_rows(
    broom::tidy(model_its_plain, conf.int = TRUE) %>% mutate(spec = "ITS on gap (no controls)"),
    broom::tidy(model_its_cov, conf.int = TRUE) %>% mutate(spec = "ITS on gap + U_t + A_t")
) %>%
    filter(term %in% c("post", "t_post", "z_unemployment", "logz_asylum")) %>%
    mutate(term = recode(
        term,
        "post" = "Post-level shift",
        "t_post" = "Post-trend shift",
        "z_unemployment" = "Unemployment z-score",
        "logz_asylum" = "Asylum log-z"
    ))

write_csv(coef_cov, file.path(out_dir, "C_gap_its_covariate_coefficients.csv"))

model_fit_stats <- bind_rows(
    glance(model_its_plain) %>% mutate(spec = "ITS on gap (no controls)"),
    glance(model_its_cov) %>% mutate(spec = "ITS on gap + U_t + A_t")
) %>%
    select(spec, r.squared, adj.r.squared, sigma, AIC, BIC, nobs)

write_csv(model_fit_stats, file.path(out_dir, "C_gap_its_model_fit_stats.csv"))

p_cov_coef <- coef_cov %>%
    ggplot(aes(x = term, y = estimate, color = spec)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
    geom_point(position = position_dodge(width = 0.6), size = 2.6) +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        position = position_dodge(width = 0.6),
        width = 0.12,
        linewidth = 0.75
    ) +
    scale_color_manual(values = c("ITS on gap (no controls)" = pku_pal$mono[2], "ITS on gap + U_t + A_t" = pku_pal$accent)) +
    labs(
        title = "C) Covariate-Augmented ITS on SCM Gap",
        subtitle = "Gap_t = alpha + beta1*t + beta2*Post + beta3*t_post + gamma1*U_t + gamma2*A_t + epsilon_t",
        x = "",
        y = "Coefficient Estimate (95% CI)",
        color = "",
        caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "bottom") %>%
    add_watermark()

save_plot(p_cov_coef, "C_gap_its_covariate_coefficients.png", width = 11, height = 6.2)

# ------------------------------------------------------------------------------
# 5) CHURZI ERGEBNIS-NOTIZ
# ------------------------------------------------------------------------------
post_plain <- coef_cov %>% filter(spec == "ITS on gap (no controls)", term == "Post-level shift")
post_cov <- coef_cov %>% filter(spec == "ITS on gap + U_t + A_t", term == "Post-level shift")
tpost_plain <- coef_cov %>% filter(spec == "ITS on gap (no controls)", term == "Post-trend shift")
tpost_cov <- coef_cov %>% filter(spec == "ITS on gap + U_t + A_t", term == "Post-trend shift")

summary_lines <- c(
    "# Additional Robustness (Script 20)",
    "",
    paste0("Generated: ", Sys.Date()),
    "",
    "## A) Pre-fit placebo screen",
    paste0("Unscreened placebo p-value: ", formatC(p_all, digits = 3, format = "f")),
    paste0(
        "Screened placebo p-value (c = ", fit_screen_c_primary, "): ",
        formatC(p_screen_primary, digits = 3, format = "f")
    ),
    "",
    "## B) Shock-exclusion sensitivity",
    paste0("Shock window excluded: ", shock_start, " to ", shock_end),
    "ITS and TOST re-estimated on SCM gap under baseline and exclusion designs.",
    "",
    "## C) Covariate-augmented ITS on gap",
    paste0(
        "Post-level shift: plain=", round(post_plain$estimate, 3),
        " (p=", formatC(post_plain$p.value, digits = 3, format = "f"), "); ",
        "with controls=", round(post_cov$estimate, 3),
        " (p=", formatC(post_cov$p.value, digits = 3, format = "f"), ")"
    ),
    paste0(
        "Post-trend shift: plain=", round(tpost_plain$estimate, 3),
        " (p=", formatC(tpost_plain$p.value, digits = 3, format = "f"), "); ",
        "with controls=", round(tpost_cov$estimate, 3),
        " (p=", formatC(tpost_cov$p.value, digits = 3, format = "f"), ")"
    ),
    "",
    "Note: This is an auxiliary within-France robustness check, not a full donor-side covariate SCM."
)

writeLines(summary_lines, file.path(out_dir, "20_additional_robustness_summary.md"))

print("------------------------------------------------------------")
print("20_additional_robustness.R complete.")
print(paste("Outputs saved to:", out_dir))
print("------------------------------------------------------------")
