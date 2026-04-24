# ==============================================================================
# Title: Differential Effects Analysis
# Purpose: Test differential effects of legal instruments on extremist actors.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse, lubridate, readxl, broom, scales, zoo, MASS, strucchange
)

# 1. YRICHTIG ------------------------------------------------------------------
source("R/theme_pku.R")
source("R/controls.R")

out_dir <- "outputs/differential_effects"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

law_date <- as.Date("2021-07-30")
# Bruch dr erschti voll Monet nochem Gsetz für kei Teilmonet-Verschmutzig
post_start <- ceiling_date(law_date, "month") # 2021-08-01

controls <- load_controls()

make_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

write_html_table <- function(df, path, title = NULL) {
    sink(path)
    cat("<html><head><style>table, th, td {border: 1px solid black; border-collapse: collapse; padding: 5px;} th {background-color: #8A0000; color: white;}</style></head><body>")
    if (!is.null(title)) cat(paste0("<h1>", title, "</h1>"))
    cat("<table>")
    cat(paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>"))
    for (i in 1:nrow(df)) {
        row_vals <- sapply(df[i, ], as.character)
        cat(paste0("<tr>", paste0("<td>", row_vals, "</td>", collapse = ""), "</tr>"))
    }
    cat("</table></body></html>")
    sink()
}

save_plot <- function(p, path, w = 10, h = 6) {
    ggsave(path, p, width = w, height = h, bg = "white")
}

# 2. DATE LADE -----------------------------------------------------------------
df_raw <- read_excel("data/full_dataset_w_predictions_vX.xlsx", guess_max = 10000)

if (!"event_date" %in% names(df_raw)) {
    stop("Column 'event_date' not found in the dataset.")
}

# Excel numerischi Date oder scho parsierti Date behandle
if (inherits(df_raw$event_date, "Date") || inherits(df_raw$event_date, "POSIXt")) {
    df_raw <- df_raw %>% mutate(event_date = as.Date(event_date))
} else if (is.numeric(df_raw$event_date)) {
    df_raw <- df_raw %>% mutate(event_date = as.Date(as.numeric(event_date), origin = "1899-12-30"))
} else {
    df_raw <- df_raw %>% mutate(event_date = as.Date(event_date))
}

df <- df_raw %>%
    mutate(
        month = floor_date(event_date, "month"),
        tags_clean = tolower(tags),
        tags_clean = ifelse(tags_clean == "religious", "religious", tags_clean),
        religion_clean = str_to_title(tolower(religion)),
        religion_clean = na_if(religion_clean, ""),
        auto_label_margin = suppressWarnings(as.numeric(auto_label_margin)),
        country = as.character(country),
        admin1 = as.character(admin1),
        admin2 = as.character(admin2)
    ) %>%
    mutate(
        group_left_right = case_when(
            tags_clean == "left" ~ "Left",
            tags_clean == "right" ~ "Right",
            TRUE ~ NA_character_
        ),
        group_religious_right = case_when(
            tags_clean == "religious" ~ "Religious",
            tags_clean == "right" ~ "Right",
            TRUE ~ NA_character_
        ),
        group_islam_right = case_when(
            religion_clean == "Islam" ~ "Islam",
            tags_clean == "right" ~ "Right",
            TRUE ~ NA_character_
        )
    )

# 3. GRUNDLÄGENLICHI ZAMMEFASSIGE ----------------------------------------------
summary_dir <- file.path(out_dir, "summaries")
make_dir(summary_dir)

counts_by_year_tag <- df %>%
    filter(country == "France") %>%
    count(year, tags_clean, name = "events") %>%
    arrange(year, desc(events))

write_csv(counts_by_year_tag, file.path(summary_dir, "counts_by_year_tags_france.csv"))
write_html_table(counts_by_year_tag, file.path(summary_dir, "counts_by_year_tags_france.html"), "France: Counts by Year and Tag")

counts_by_year_religion <- df %>%
    filter(country == "France") %>%
    count(year, religion_clean, name = "events") %>%
    arrange(year, desc(events))

write_csv(counts_by_year_religion, file.path(summary_dir, "counts_by_year_religion_france.csv"))
write_html_table(counts_by_year_religion, file.path(summary_dir, "counts_by_year_religion_france.html"), "France: Counts by Year and Religion")

# 4. HILF ----------------------------------------------------------------------
prepare_monthly <- function(df_in, group_col, group_levels, post_start_date) {
    df_use <- df_in %>%
        filter(!is.na(.data[[group_col]])) %>%
        dplyr::select(month, !!sym(group_col))

    if (nrow(df_use) == 0) {
        return(NULL)
    }

    df_monthly <- df_use %>%
        group_by(month, group = .data[[group_col]]) %>%
        summarise(count = n(), .groups = "drop")

    all_months <- seq(min(df_monthly$month), max(df_monthly$month), by = "month")

    df_monthly <- df_monthly %>%
        complete(month = all_months, group = group_levels, fill = list(count = 0)) %>%
        arrange(month) %>%
        mutate(
            group = factor(group, levels = group_levels),
            post = month >= post_start_date,
            month_index = year(month) * 12 + month(month),
            month_diff = month_index - (year(post_start_date) * 12 + month(post_start_date)),
            month_num = month(month)
        )

    return(df_monthly)
}

calc_pre_post_summary <- function(df_monthly) {
    df_summary <- df_monthly %>%
        group_by(group, period = ifelse(post, "Post", "Pre")) %>%
        summarise(
            months = n(),
            total = sum(count),
            mean = round(mean(count), 3),
            median = round(median(count), 3),
            sd = round(sd(count), 3),
            .groups = "drop"
        )

    df_wide <- df_summary %>%
        pivot_wider(
            names_from = period,
            values_from = c(months, total, mean, median, sd)
        ) %>%
        mutate(
            pct_change_mean = round((mean_Post - mean_Pre) / mean_Pre * 100, 2)
        )

    return(df_wide)
}

run_pre_post_tests <- function(df_monthly) {
    groups <- levels(df_monthly$group)
    results <- map_dfr(groups, function(g) {
        df_g <- df_monthly %>% filter(group == g)
        t_res <- tryCatch(t.test(count ~ post, data = df_g), error = function(e) NULL)
        w_res <- tryCatch(wilcox.test(count ~ post, data = df_g), error = function(e) NULL)

        tibble(
            group = g,
            t_p_value = if (!is.null(t_res)) round(t_res$p.value, 5) else NA_real_,
            wilcox_p_value = if (!is.null(w_res)) round(w_res$p.value, 5) else NA_real_
        )
    })

    # Fisher-Tescht uf aggregierti vor/noch no Gruppe (guet für dünni Date)
    pre_post_table <- df_monthly %>%
        group_by(group, post) %>%
        summarise(total = sum(count), .groups = "drop") %>%
        pivot_wider(names_from = post, values_from = total, values_fill = 0)

    fisher_p <- NA_real_
    if (nrow(pre_post_table) == 2) {
        mat <- as.matrix(pre_post_table %>% dplyr::select(`FALSE`, `TRUE`))
        rownames(mat) <- pre_post_table$group
        fisher_p <- tryCatch(fisher.test(mat)$p.value, error = function(e) NA_real_)
    }

    results <- results %>%
        mutate(fisher_p_value = round(fisher_p, 5))

    return(results)
}

run_models <- function(df_monthly) {
    # Basis DiD
    model_lm <- lm(count ~ post * group + control_unemployment + control_asylum, data = df_monthly)
    model_lm_fe <- lm(count ~ post * group + control_unemployment + control_asylum + factor(month_num), data = df_monthly)

    model_pois <- glm(count ~ post * group + control_unemployment + control_asylum, family = poisson, data = df_monthly)
    model_pois_fe <- glm(count ~ post * group + control_unemployment + control_asylum + factor(month_num), family = poisson, data = df_monthly)

    model_nb <- tryCatch(glm.nb(count ~ post * group + control_unemployment + control_asylum, data = df_monthly), error = function(e) NULL)
    model_nb_fe <- tryCatch(glm.nb(count ~ post * group + control_unemployment + control_asylum + factor(month_num), data = df_monthly), error = function(e) NULL)

    tidy_with_model <- function(model, model_name) {
        if (is.null(model)) return(tibble())
        tidy(model) %>%
            mutate(
                model = model_name,
                exp_estimate = ifelse(str_detect(model_name, "pois|nb"), round(exp(estimate), 4), NA_real_)
            )
    }

    bind_rows(
        tidy_with_model(model_lm, "lm"),
        tidy_with_model(model_lm_fe, "lm_month_fe"),
        tidy_with_model(model_pois, "poisson"),
        tidy_with_model(model_pois_fe, "poisson_month_fe"),
        tidy_with_model(model_nb, "neg_binomial"),
        tidy_with_model(model_nb_fe, "neg_binomial_month_fe")
    )
}

run_pretrend_test <- function(df_monthly) {
    df_pre <- df_monthly %>% filter(!post)
    if (nrow(df_pre) < 10) return(tibble())
    model_pre <- lm(count ~ month_index * group, data = df_pre)
    tidy(model_pre) %>%
        filter(str_detect(term, "month_index:group"))
}

run_chow_tests <- function(df_monthly, post_start_date) {
    groups <- levels(df_monthly$group)
    results <- map_dfr(groups, function(g) {
        df_g <- df_monthly %>% filter(group == g)
        if (nrow(df_g) < 6) {
            return(tibble(group = g, statistic = NA_real_, p_value = NA_real_))
        }
        ts_data <- ts(df_g$count, start = c(year(min(df_g$month)), month(min(df_g$month))), frequency = 12)
        break_idx <- which(df_g$month == post_start_date)
        if (length(break_idx) != 1 || break_idx < 2 || break_idx > length(ts_data) - 1) {
            return(tibble(group = g, statistic = NA_real_, p_value = NA_real_))
        }
        test <- tryCatch(sctest(ts_data ~ 1, type = "Chow", point = break_idx), error = function(e) NULL)
        if (is.null(test)) {
            tibble(group = g, statistic = NA_real_, p_value = NA_real_)
        } else {
            tibble(group = g, statistic = round(test$statistic, 4), p_value = round(test$p.value, 5))
        }
    })
    results
}

run_window_sensitivity <- function(df_monthly, pre_options, post_options) {
    grid <- expand.grid(pre_months = pre_options, post_months = post_options)
    results <- pmap_dfr(grid, function(pre_months, post_months) {
        df_win <- df_monthly %>%
            filter(month_diff >= -pre_months & month_diff < post_months) %>%
            mutate(post_win = month_diff >= 0)

        if (nrow(df_win) < 10) {
            return(tibble(pre_months, post_months, estimate = NA_real_, p_value = NA_real_))
        }

        m <- lm(count ~ post_win * group + control_unemployment + control_asylum, data = df_win)
        t <- tidy(m) %>% filter(str_detect(term, "post_win:group"))
        if (nrow(t) == 0) {
            tibble(pre_months, post_months, estimate = NA_real_, p_value = NA_real_)
        } else {
            tibble(pre_months, post_months, estimate = round(t$estimate[1], 4), p_value = round(t$p.value[1], 5))
        }
    })
    results
}

run_placebo_tests <- function(df_monthly, placebo_dates) {
    placebo_results <- map_dfr(placebo_dates, function(d) {
        df_p <- df_monthly %>%
            mutate(post_pl = month >= d)
        m <- lm(count ~ post_pl * group + control_unemployment + control_asylum, data = df_p)
        t <- tidy(m) %>% filter(str_detect(term, "post_pl:group"))
        tibble(
            placebo_date = d,
            estimate = ifelse(nrow(t) > 0, round(t$estimate[1], 4), NA_real_),
            p_value = ifelse(nrow(t) > 0, round(t$p.value[1], 5), NA_real_)
        )
    })
    placebo_results
}

# 5. HAUPTANALYSE-SCHLEIFE ----------------------------------------------------
analysis_specs <- list(
    list(
        name = "left_right",
        label = "Left vs Right (Robust)",
        group_col = "group_left_right",
        group_levels = c("Right", "Left"),
        colors = c("Right" = pku_pal$accent, "Left" = pku_pal$navy)
    ),
    list(
        name = "religious_right",
        label = "Religious vs Right (Sparse)",
        group_col = "group_religious_right",
        group_levels = c("Right", "Religious"),
        colors = c("Right" = pku_pal$accent, "Religious" = pku_pal$amber)
    ),
    list(
        name = "islam_right",
        label = "Islam vs Right (Very Sparse)",
        group_col = "group_islam_right",
        group_levels = c("Right", "Islam"),
        colors = c("Right" = pku_pal$accent, "Islam" = pku_pal$purple)
    )
)

notes_path <- file.path(out_dir, "analysis_notes.txt")
sink(notes_path)
cat("DIFFERENTIAL EFFECTS ANALYSIS NOTES\n")
cat("Law date: ", as.character(law_date), " | Post start (first full month): ", as.character(post_start), "\n\n", sep = "")

for (spec in analysis_specs) {
    cat("=== ", spec$label, " ===\n", sep = "")

    sub_dir <- file.path(out_dir, spec$name)
    make_dir(sub_dir)

    df_fr <- df %>% filter(country == "France")
    df_monthly <- prepare_monthly(df_fr, spec$group_col, spec$group_levels, post_start)

    if (is.null(df_monthly)) {
        cat("No data for spec: ", spec$name, "\n\n", sep = "")
        next
    }

    df_monthly <- df_monthly %>%
        left_join(controls$monthly, by = c("month" = "date")) %>%
        filter(!is.na(control_unemployment), !is.na(control_asylum))

    # Stichprobenumfang-Notize
    totals <- df_monthly %>%
        group_by(group) %>%
        summarise(total_events = sum(count), .groups = "drop")
    print(totals)

    # Monetligi Azaale spichere
    write_csv(df_monthly, file.path(sub_dir, "monthly_counts.csv"))

    # Vor/Noch Zämmefassigstabelle
    summary_table <- calc_pre_post_summary(df_monthly)
    write_csv(summary_table, file.path(sub_dir, "pre_post_summary.csv"))
    write_html_table(summary_table, file.path(sub_dir, "pre_post_summary.html"), paste0(spec$label, ": Pre/Post Summary"))

    # Grundplotte ---------------------------------------------------------------
    p_raw <- ggplot(df_monthly, aes(x = month, y = count, color = group)) +
        geom_line(linewidth = 1) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = post_start, linetype = "dashed", color = "black") +
        scale_color_manual(values = spec$colors) +
        labs(
            title = paste0(spec$label, ": Monthly Counts"),
            subtitle = paste0("Vertical line: ", post_start),
            x = "", y = "Events per Month", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_raw, file.path(sub_dir, "ts_monthly_counts.png"))

    df_roll <- df_monthly %>%
        group_by(group) %>%
        mutate(roll6 = zoo::rollmean(count, k = 6, fill = NA, align = "right")) %>%
        ungroup()

    p_roll <- ggplot(df_roll, aes(x = month, y = roll6, color = group)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = post_start, linetype = "dashed", color = "black") +
        scale_color_manual(values = spec$colors) +
        labs(
            title = paste0(spec$label, ": 6-Month Rolling Mean"),
            subtitle = "Smoothing to address short pre-trend window",
            x = "", y = "Rolling Mean (6m)", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_roll, file.path(sub_dir, "ts_rolling6.png"))

    df_index <- df_monthly %>%
        group_by(group) %>%
        mutate(
            pre_mean = mean(count[!post], na.rm = TRUE),
            index = ifelse(pre_mean > 0, 100 * count / pre_mean, NA_real_)
        ) %>%
        ungroup()

    p_index <- ggplot(df_index, aes(x = month, y = index, color = group)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = post_start, linetype = "dashed", color = "black") +
        scale_color_manual(values = spec$colors) +
        labs(
            title = paste0(spec$label, ": Indexed to Pre-Period Mean"),
            subtitle = "Pre-period mean = 100",
            x = "", y = "Index", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_index, file.path(sub_dir, "ts_indexed.png"))

    df_cum <- df_monthly %>%
        group_by(group) %>%
        mutate(cumulative = cumsum(count)) %>%
        ungroup()

    p_cum <- ggplot(df_cum, aes(x = month, y = cumulative, color = group)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = post_start, linetype = "dashed", color = "black") +
        scale_color_manual(values = spec$colors) +
        labs(
            title = paste0(spec$label, ": Cumulative Events"),
            subtitle = "Running sum",
            x = "", y = "Cumulative Count", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_cum, file.path(sub_dir, "ts_cumulative.png"))

    df_share <- df_monthly %>%
        group_by(month) %>%
        mutate(share = count / sum(count)) %>%
        ungroup()

    p_share <- ggplot(df_share, aes(x = month, y = share, color = group)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = post_start, linetype = "dashed", color = "black") +
        scale_color_manual(values = spec$colors) +
        scale_y_continuous(labels = percent) +
        labs(
            title = paste0(spec$label, ": Share of Monthly Events"),
            subtitle = "Within-group shares by month",
            x = "", y = "Share", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_share, file.path(sub_dir, "ts_share.png"))

    # Vor/Noch Verdailigsplot
    p_box <- ggplot(df_monthly, aes(x = group, y = count, fill = post)) +
        geom_boxplot(alpha = 0.6) +
        scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = pku_pal$accent)) +
        labs(
            title = paste0(spec$label, ": Pre vs Post Distribution"),
            x = "", y = "Monthly Counts", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_box, file.path(sub_dir, "pre_post_boxplot.png"), w = 8, h = 5)

    # Event-Study-Stil (3-Mönet-Bins um s Gsetz)
    df_bins <- df_monthly %>%
        mutate(bin = floor(month_diff / 3)) %>%
        group_by(bin, group) %>%
        summarise(mean_count = mean(count), .groups = "drop")

    p_bins <- ggplot(df_bins, aes(x = bin, y = mean_count, color = group)) +
        geom_line(linewidth = 1) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
        scale_color_manual(values = spec$colors) +
        labs(
            title = paste0(spec$label, ": Binned Event Study (3-Month)"),
            subtitle = "Bin 0 starts at law month",
            x = "3-Month Bins (relative to law)", y = "Mean Count", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_bins, file.path(sub_dir, "event_study_binned.png"))

    # Teschts & Modell ----------------------------------------------------------
    test_table <- run_pre_post_tests(df_monthly)
    write_csv(test_table, file.path(sub_dir, "pre_post_tests.csv"))
    write_html_table(test_table, file.path(sub_dir, "pre_post_tests.html"), paste0(spec$label, ": Pre/Post Tests"))

    model_results <- run_models(df_monthly)
    write_csv(model_results, file.path(sub_dir, "models_full.csv"))

    did_results <- model_results %>%
        filter(str_detect(term, "post:group")) %>%
        dplyr::select(model, term, estimate, std.error, statistic, p.value, exp_estimate)
    write_csv(did_results, file.path(sub_dir, "models_interaction_only.csv"))
    write_html_table(did_results, file.path(sub_dir, "models_interaction_only.html"), paste0(spec$label, ": Interaction Results"))

    pretrend <- run_pretrend_test(df_monthly)
    write_csv(pretrend, file.path(sub_dir, "pretrend_test.csv"))

    chow_results <- run_chow_tests(df_monthly, post_start)
    write_csv(chow_results, file.path(sub_dir, "chow_tests.csv"))

    # Fäischter-Sensitivität ----------------------------------------------------
    window_results <- run_window_sensitivity(df_monthly, pre_options = c(6, 9, 12, 18), post_options = c(6, 12, 18, 24))
    write_csv(window_results, file.path(sub_dir, "window_sensitivity.csv"))

    p_heat <- ggplot(window_results, aes(x = factor(post_months), y = factor(pre_months), fill = p_value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = ifelse(is.na(p_value), "NA", round(p_value, 3))), size = 3) +
        scale_fill_gradientn(colors = c("#b2182b", "#ef8a62", "#f7f7f7", "#67a9cf", "#2166ac"), na.value = "grey90") +
        labs(
            title = paste0(spec$label, ": Window Sensitivity (Interaction p)"),
            x = "Post window (months)", y = "Pre window (months)", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_heat, file.path(sub_dir, "window_sensitivity_heatmap.png"), w = 8, h = 5)

    # Placebo-Teschts -----------------------------------------------------------
    placebo_dates <- as.Date(c("2020-07-01", "2020-10-01", "2021-01-01", "2021-04-01", "2022-01-01"))
    placebo_results <- run_placebo_tests(df_monthly, placebo_dates)
    write_csv(placebo_results, file.path(sub_dir, "placebo_tests.csv"))

    p_placebo <- ggplot(placebo_results, aes(x = placebo_date, y = p_value)) +
        geom_line(color = pku_pal$accent) +
        geom_point(size = 2, color = pku_pal$accent) +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
        labs(
            title = paste0(spec$label, ": Placebo Tests (Interaction p)"),
            subtitle = "Dashed line = 0.05 threshold",
            x = "Placebo Date", y = "p-value", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_placebo, file.path(sub_dir, "placebo_tests.png"), w = 8, h = 5)

    cat("\n")
}

sink()

# 6. LANDVERGLIICH (FR gege DE) -----------------------------------------------
country_dir <- file.path(out_dir, "country_comparison_left_right")
make_dir(country_dir)

df_lr_country <- df %>%
    filter(country %in% c("France", "Germany")) %>%
    filter(!is.na(group_left_right)) %>%
    group_by(country, month, group = group_left_right) %>%
    summarise(count = n(), .groups = "drop")

if (nrow(df_lr_country) > 0) {
    all_months_c <- seq(min(df_lr_country$month), max(df_lr_country$month), by = "month")
    df_lr_country <- df_lr_country %>%
        complete(month = all_months_c, country, group, fill = list(count = 0)) %>%
        mutate(
            group = factor(group, levels = c("Right", "Left")),
            post = month >= post_start,
            france = ifelse(country == "France", 1, 0),
            month_num = month(month)
        )

    # Plot no Land
    p_country <- ggplot(df_lr_country, aes(x = month, y = count, color = group)) +
        geom_line(linewidth = 0.9) +
        geom_vline(xintercept = post_start, linetype = "dashed", color = "black") +
        scale_color_manual(values = c("Right" = pku_pal$accent, "Left" = pku_pal$navy)) +
        facet_wrap(~country, scales = "free_y") +
        labs(
            title = "Left vs Right: France vs Germany",
            subtitle = "Germany as placebo control",
            x = "", y = "Monthly Events", caption = std_caption
        ) +
        theme_pku() %>%
        add_watermark()
    save_plot(p_country, file.path(country_dir, "france_germany_timeseries.png"), w = 12, h = 6)

    # Drifach-Differänz-Modell
    model_triple <- lm(count ~ post * group * france + factor(month_num), data = df_lr_country)
    model_triple_tidy <- tidy(model_triple)
    write_csv(model_triple_tidy, file.path(country_dir, "triple_diff_model.csv"))
    write_html_table(model_triple_tidy, file.path(country_dir, "triple_diff_model.html"), "Triple Difference Model (FR vs DE)")

    # Schlissel-Interaktione extrahiere
    triple_key <- model_triple_tidy %>% filter(str_detect(term, "post:group.*:france"))
    write_csv(triple_key, file.path(country_dir, "triple_diff_key_term.csv"))
}

print("--- 17_differential_effects_analysis.R COMPLETE ---")
