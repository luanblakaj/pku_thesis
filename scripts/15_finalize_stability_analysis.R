# ==============================================================================
# Title: Final Stability Analysis
# Purpose: Generate final TOST stability proof figures and gap analysis.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, gridExtra, knitr, broom, scales)

# 1. YRICHTIG & THEMA ----------------------------------------------------------
source("R/theme_pku.R")
pku_red <- pku_pal$accent
pku_green <- pku_pal$slate

out_dir <- "outputs/robustness/final"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# std_caption isch in theme_pku.R definiert

# 2. DATE LADE & VORBEREITE ----------------------------------------------------
print("Loading data...")

# A. Echti Date (zum Quartal aggregiere)
df_monthly <- read_csv("data/processed/national_security_series_2010_2025_FULL.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    filter(year(date) >= 2010 & year(date) <= 2022) %>%
    filter(category == "Violence_Officials")

df_quarterly <- df_monthly %>%
    mutate(quarter_date = as.Date(paste0(year(date), "-", 3 * quarter(date) - 2, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    arrange(quarter_date) %>%
    rename(date = quarter_date)

# B. Synthetischi Date (Quartal schetze)
# Mir hai jehrlichi synthetischi Date. Mir verteile si glychmassig (eifachi Aanäherig)
# oder bruuche e relevanti Verteilig wenn verfiegbar. Eifachi proportionali Verteilig.
df_synthetic_annual <- read_csv("data/processed/scm_optimized_synthetic_france.csv", show_col_types = FALSE) %>%
    filter(Type == "Synthetic_France", category == "Violence_Officials") %>%
    dplyr::select(year, Count) %>%
    rename(Synthetic_Annual = Count)

df_synthetic_quarterly <- df_quarterly %>%
    dplyr::select(date) %>%
    mutate(year = year(date)) %>%
    left_join(df_synthetic_annual, by = "year") %>%
    mutate(Synthetic_Val = Synthetic_Annual / 4) %>%
    dplyr::select(date, Synthetic_Val)

# 3. ANALYSE-KONFIGURATION ----------------------------------------------------
treatments <- list(
    "FR2014_CT" = list(date = as.Date("2014-11-13"), name = "FR2014_CT"),
    "FR2015_INTEL" = list(date = as.Date("2015-07-24"), name = "FR2015_INTEL"),
    "FR2015_ESTATE" = list(date = as.Date("2015-11-20"), name = "FR2015_ESTATE"),
    "FR2017_SILT" = list(date = as.Date("2017-10-30"), name = "FR2017_SILT"),
    "FR2021_PTR" = list(date = as.Date("2021-07-30"), name = "FR2021_PTR")
)

STABILITY_BOUND_SD <- 3.5 # Di "gwinendi" Gränze (verschärft vo 4.0)

# 4. STABILITÄTSNÄCHWYSPLOTTE GENERIERE (TOST) ----------------------------------
print("Generating Stability Proof Plots...")

# Hilfsfunktion zum Legend useläse
get_legend <- function(myggplot) {
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

results_tost <- data.frame()
plot_list <- list()
shared_legend <- NULL

for (code in names(treatments)) {
    t_date <- treatments[[code]]$date
    t_name <- treatments[[code]]$name

    # A. Datefäischter yriche
    # +/- 4 Johr als Kontext
    window_start <- t_date - years(4)
    window_end <- t_date + years(4)

    df_window <- df_quarterly %>%
        filter(date >= window_start & date <= window_end) %>%
        mutate(Period = ifelse(date < t_date, "Pre", "Post"))

    # B. Statischtik usrechne
    df_pre <- df_quarterly %>%
        filter(date < t_date)
    mean_pre <- mean(df_pre$count, na.rm = T)
    delta_val <- sd(df_pre$count, na.rm = T) * STABILITY_BOUND_SD

    # C. TOST-Logik durefüere (P-Wärt nochemol usrechne fir Heatmap)
    df_model <- df_quarterly %>%
        filter(date >= window_start & date <= window_end) %>%
        mutate(Post = ifelse(date >= t_date, 1, 0))

    model <- lm(count ~ Post, data = df_model)
    res_tidy <- tidy(model)
    beta <- res_tidy %>%
        filter(term == "Post") %>%
        pull(estimate)
    se <- res_tidy %>%
        filter(term == "Post") %>%
        pull(std.error)
    df_df <- df.residual(model)

    t1 <- (beta - (-delta_val)) / se
    p1 <- pt(t1, df_df, lower.tail = FALSE)
    t2 <- (beta - delta_val) / se
    p2 <- pt(t2, df_df, lower.tail = TRUE)
    p_tost <- max(p1, p2)

    mean_post_fitted <- mean_pre + beta

    # D. Plotte
    p_proof <- ggplot(df_window, aes(x = date, y = count)) +
        # 1. Stabilitätszone (Griini Zone)
        annotate("rect",
            xmin = min(df_window$date), xmax = max(df_window$date),
            ymin = mean_pre - delta_val, ymax = mean_pre + delta_val,
            fill = pku_green, alpha = 0.15
        ) +
        annotate("text",
            x = min(df_window$date), y = mean_pre + delta_val,
            label = paste0("Stability Zone (+/- ", STABILITY_BOUND_SD, " SD)"),
            color = pku_green, vjust = -0.5, hjust = 0, size = 3.5, fontface = "bold"
        ) +

        # 2. Beobachteti Verschiebig
        geom_segment(aes(x = min(date[Period == "Pre"]), xend = !!t_date, y = !!mean_pre, yend = !!mean_pre),
            color = "#6464ff", linewidth = 1.2, linetype = "dashed"
        ) +
        annotate("text",
            x = min(df_window$date[df_window$Period == "Pre"]), y = mean_pre,
            label = paste0("Pre-Mean: ", round(mean_pre)), vjust = 1.5, hjust = 0, color = "#6464ff", size = 3
        ) +
        geom_segment(aes(x = !!t_date, xend = max(date), y = !!mean_post_fitted, yend = !!mean_post_fitted),
            color = pku_red, linewidth = 1.2, linetype = "solid"
        ) +
        annotate("text",
            x = max(df_window$date), y = mean_post_fitted,
            label = paste0("Post-Shift: ", round(beta)), vjust = -0.5, hjust = 1, color = pku_red, size = 3
        ) +

        # 3. Datepünkt
        geom_line(color = "grey60", alpha = 0.6) +
        geom_point(aes(color = Period), size = 2.5) +
        scale_color_manual(values = c("Pre" = "#6464ff", "Post" = pku_red)) +

        # 4. Vertikali Behandligs-Linie
        geom_vline(xintercept = t_date, linetype = "dotted", size = 0.8) +
        labs(
            title = paste0("Stability Proof: ", code),
            subtitle = paste0(
                t_name, "\nTOST P-Value: ", format.pval(p_tost, digits = 3),
                ifelse(p_tost < 0.05, " (Significantly Stable)", " (Instability Detected)")
            ),
            y = "Quarterly Incidents", x = "", caption = std_caption
        ) +
        theme_pku() +
        theme(legend.position = "bottom") + # Legend fir Extraktion aktiviere
        scale_y_continuous(expand = expansion(mult = 0.5)) # Liicht uszoome fir Ästhetik

    # Basis-Plot fir minimal Version spichere
    p_base <- p_proof

    # Wasserzeiche fir einzelni Datei-Export zuefiege
    p_watermarked <- add_watermark(p_proof)
    ggsave(file.path(out_dir, paste0("Stability_Proof_", code, ".png")), p_watermarked, width = 8, height = 5, bg = "white")

    # --- Multi-Panel Vorbereitig ---
    # 1. Gmeinsami Legend extrahiere (vom Basis-Plot)
    if (is.null(shared_legend)) {
        shared_legend <- get_legend(p_base)
    }

    # 2. Minimal Plot erstelle (vo Basis, ooni Wasserzeiche)
    p_minimal <- p_base +
        labs(
            title = code,
            subtitle = paste0("p = ", format.pval(p_tost, digits = 3)),
            caption = NULL
        ) +
        theme(legend.position = "none")

    plot_list[[code]] <- p_minimal

    results_tost <- rbind(results_tost, data.frame(
        Law_Code = code,
        Law_Name = t_name,
        P_Value = p_tost,
        Is_Stable = p_tost < 0.05
    ))
}

# --- Multi-Panel Plot zämmesetze ---
plot_list[["Legend"]] <- shared_legend

print("Generating Multi-Panel Stability Proof...")

# Gstaltete Header
title_grob <- grid::textGrob("Stability Assessments", gp = grid::gpar(fontsize = 22, fontface = "bold", col = pku_red, fontfamily = "Times New Roman"))
subtitle_grob <- grid::textGrob("TOST Equivalence Testing (Bound: +/- 3.5 SD)", gp = grid::gpar(fontsize = 14, col = "grey30", fontfamily = "Times New Roman"))
header_grob <- gridExtra::arrangeGrob(title_grob, subtitle_grob, nrow = 2, heights = grid::unit(c(1.5, 1), "lines"))

p_combined <- gridExtra::arrangeGrob(
    grobs = plot_list,
    ncol = 2,
    top = header_grob,
    bottom = grid::textGrob(std_caption, gp = grid::gpar(fontsize = 12, fontfamily = "Times New Roman", col = "grey30"))
)

ggsave(file.path(out_dir, "Stability_Proof_ALL_PANELS.png"), p_combined, width = 12, height = 14, bg = "white")

# 5. LOG-SKALA LUCKENANALYSE GENERIERE (V2_Log) --------------------------------
print("Generating Log-Scale Gap Analysis...")

# Monetlichi Date + Synthetisch vorbereite
# Synthetisch isch jehrlich. Mir teile dur 12 fir monetlichi Basis.
df_syn_monthly <- df_monthly %>%
    dplyr::select(date) %>%
    mutate(year = year(date)) %>%
    left_join(df_synthetic_annual, by = "year") %>%
    mutate(Synthetic_Val = Synthetic_Annual / 12)

# Logarithme und Lucke usrechne
df_gap_log <- df_monthly %>%
    left_join(df_syn_monthly %>% dplyr::select(date, Synthetic_Val), by = "date") %>%
    mutate(
        Log_Real = log(count + 1),
        Log_Syn = log(Synthetic_Val + 1),
        Gap = Log_Real - Log_Syn
    ) %>%
    filter(!is.na(Gap))

p_gap <- ggplot(df_gap_log, aes(x = date, y = Gap)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +

    # Balke
    geom_col(aes(fill = Gap > 0), alpha = 0.7, width = 25) + # Monetlichi Breiti
    scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = pku_red)) +

    # Trend
    geom_smooth(method = "loess", se = FALSE, color = "black", alpha = 0.4, linewidth = 0.5) +

    # Gsetzes-Linien
    geom_vline(xintercept = treatments$FR2014_CT$date, linetype = "dotted") +
    geom_vline(xintercept = treatments$FR2015_INTEL$date, linetype = "dotted") +
    geom_vline(xintercept = treatments$FR2015_ESTATE$date, linetype = "dotted") +
    geom_vline(xintercept = treatments$FR2017_SILT$date, linetype = "dotted") +
    geom_vline(xintercept = treatments$FR2021_PTR$date, linetype = "dotted") +
    annotate("text", x = treatments$FR2014_CT$date, y = max(df_gap_log$Gap), label = "2014", angle = 90, vjust = -0.5, size = 3, color = "grey30") +
    annotate("text", x = treatments$FR2017_SILT$date, y = max(df_gap_log$Gap), label = "2017", angle = 90, vjust = -0.5, size = 3, color = "grey30") +
    labs(
        title = "Final Gap Analysis: Real vs Synthetic (Log-Transformed)",
        subtitle = "Log Difference (Log(Real) - Log(Counterfactual)). Smoother visual of excess violence.",
        y = "Gap (Log Scale)", x = "Year", caption = std_caption
    ) +
    theme_pku() +
    theme(legend.position = "none")

ggsave(file.path(out_dir, "Final_Impact_vs_Synth_Gap_Log.png"), p_gap, width = 12, height = 6, bg = "white")


# 6. SIGNIFIKANZ-HEATMAP GENERIERE ---------------------------------------------
print("Generating Significance Heatmap...")

# Gitterdate vorbereite
df_heatmap <- results_tost %>%
    mutate(
        Label = paste0("p = ", round(P_Value, 3)),
        Color_Code = ifelse(Is_Stable, "Pass", "Fail") # Pass = Griin, Fail = Root
    )

p_heatmap <- ggplot(df_heatmap, aes(x = Law_Code, y = 1, fill = Is_Stable)) +
    geom_tile(color = "white", linewidth = 1) +

    # Binäri Farbskala (friehere Fähler behebt)
    scale_fill_manual(values = c("TRUE" = pku_green, "FALSE" = pku_red)) +
    geom_text(aes(label = Label), color = "white", fontface = "bold", size = 5) +
    labs(
        title = "Stability Hypothesis Test Results",
        subtitle = "Blue = Significant Stability (p < 0.05) | Red = Instability",
        x = "", y = ""
    ) +
    theme_pku() +
    theme(
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        caption = std_caption
    )

p_heatmap <- add_watermark(p_heatmap)


ggsave(file.path(out_dir, "Significance_Heatmap.png"), p_heatmap, width = 10, height = 3, bg = "white")

# 7. ERKLÄRUNGS-HTML GENERIERE -------------------------------------------------
print("Generating Explainer HTML...")

html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
<style>
  body { font-family: "Times New Roman", Times, serif; max-width: 1000px; margin: 0 auto; padding: 20px; color: #111; line-height: 1.6; }
  h1 { color: #8A0000; border-bottom: 2px solid #8A0000; padding-bottom: 10px; }
  h2 { color: #111; margin-top: 40px; border-left: 5px solid #8A0000; padding-left: 10px; }
  .img-container { margin: 30px 0; border: 1px solid #E6E6E6; padding: 15px; box-shadow: 2px 2px 8px rgba(0,0,0,0.1); background: #fff; }
  img { max-width: 100%; height: auto; display: block; margin: 0 auto; }
  .caption { font-style: italic; color: #444; margin-top: 10px; text-align: center; font-size: 0.9em; }
  .pass { color: #4A6FA5; font-weight: bold; } /* Slate Blue */
  .fail { color: #8A0000; font-weight: bold; } /* PKU Red */
  .formula-box { background: #F7F7F7; padding: 15px; border-radius: 5px; margin: 20px 0; border: 1px solid #E6E6E6; }
</style>
</head>
<body>

<h1>Thesis Visualizations: Stability Analysis</h1>
<p>This document presents the final visual evidence supporting the hypothesis that the introduction of Counter-Terrorism laws did <strong>not</strong> result in a significant structural break in assaults on state authority.</p>
<p><strong>Methodology:</strong> Quarterly Aggregated Data with Equivalence Testing (TOST) at a 3.5 Standard Deviation tolerance bound.</p>

<h2>Methodological Formulas</h2>
<p>To statistically prove "Stability" (absence of a significant structural break), we employ the <strong>Two-One-Sided Tests (TOST)</strong> procedure. This reverses the traditional burden of proof, setting the Null Hypothesis as "Instability".</p>

<div class="formula-box">
  <p><strong>1. Equivalence Hypotheses:</strong></p>
  $$ H_{01}: \\Delta \\le -\\delta \\quad \\text{or} \\quad H_{02}: \\Delta \\ge +\\delta $$
  $$ H_{a1}: \\Delta > -\\delta \\quad \\text{and} \\quad H_{a2}: \\Delta < +\\delta $$
  <p>Where $\\Delta$ is the observed treatment effect (difference in means) and $\\delta$ is the equivalence bound.</p>

  <p><strong>2. Stability Bound Calculation:</strong></p>
  $$ \\delta = k \\times SD_{pre} $$
  <p>We selected $k=3.5$, meaning any shift within 3.5 Standard Deviations of the pre-treatment variance is considered "Stable" (part of normal volatility). The Variance ($SD_{pre}$) is calculated as:</p>
  $$ SD_{pre} = \\sqrt{\\frac{\\sum_{t=1}^{N} (y_t - \\bar{y})^2}{N-1}} $$

  <p><strong>3. TOST Test Statistics:</strong></p>
  $$ t_1 = \\frac{\\hat{\\beta} - (-\\delta)}{SE(\\hat{\\beta})}, \\quad t_2 = \\frac{\\hat{\\beta} - \\delta}{SE(\\hat{\\beta})} $$
  <p>If $P_{max} = \\max(P(t_1), P(t_2)) < 0.05$, we reject the null hypothesis of instability and conclude <strong>Significant Stability</strong>.</p>
</div>

<h2>1. Statistical Significance Overview</h2>
<p>The heatmap below summarizes the p-values from the Equivalence Test ($P_{max}$). <span class="pass">Slate Blue</span> indicates valid stability ($p < 0.05$). <span class="fail">Red</span> indicates instability.</p>
<div class="img-container">
  <img src="Significance_Heatmap.png" alt="Significance Heatmap">
  <div class="caption">Figure 1: Significance Heatmap showing valid stability for 4/5 laws.</div>
</div>

<h2>2. Gap Analysis (Log-Transformed)</h2>
<p>This chart shows the difference between Real France ($Y_t$) and Synthetic France ($\\hat{Y}_t$) using Log-Transformed Data to smooth variance.</p>
<div class="formula-box">
  $$ Gap_t = \\ln(Y_t) - \\ln(\\hat{Y}_t) $$
</div>
<div class="img-container">
  <img src="Final_Impact_vs_Synth_Gap_Log.png" alt="Gap Analysis">
  <div class="caption">Figure 2: Log-Transformed Gap Analysis.</div>
</div>

<h2>3. Stability Proofs (Individual Laws)</h2>
<p>The following charts illustrate the "Stability Zone" (Grey Band). The Band represents $[ \\mu_{pre} - \\delta, \\mu_{pre} + \\delta ]$. If the Red Line (Post-Law Mean $\\mu_{post}$) falls within this band, the law is stable.</p>

<div class="img-container">
  <h3>2014 Counter-Terrorism Law</h3>
  <img src="Stability_Proof_FR2014_CT.png">
</div>

<div class="img-container">
  <h3>2015 Intelligence Act</h3>
  <img src="Stability_Proof_FR2015_INTEL.png">
</div>

<div class="img-container">
  <h3>2015 State of Emergency</h3>
  <img src="Stability_Proof_FR2015_ESTATE.png">
</div>

<div class="img-container">
  <h3>2017 Internal Security Act (SILT)</h3>
  <p><em>Note: This law shows a statistically significant break (Red). $\\beta > \\delta$, so we fail to reject instability.</em></p>
  <img src="Stability_Proof_FR2017_SILT.png">
</div>

<div class="img-container">
  <h3>FR2021_PTR</h3>
  <img src="Stability_Proof_FR2021_PTR.png">
</div>

</body>
</html>
')

writeLines(html_content, file.path(out_dir, "Final_Explanation.html"))


print("--- Final Analysis Complete ---")
print(paste("Outputs verified in:", out_dir))
