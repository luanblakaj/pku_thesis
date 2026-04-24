# ==============================================================================
# Title: 00_run_all.R
# Purpose: Master script to run the entire analysis and visualization pipeline.
#          Executes all scripts that produce image outputs in logical order.
# Author: Luan Blakaj 齐凯
# ==============================================================================

rm(list = ls())
gc()

# 1. YRICHTIG -------------------------------------------------------------------
# Projäkt-Stammverzeichnis ystelle (Aapasse wänn nötig, aber das passt zu dyynere aktuelle Umgebig)
# Versuech z erkenne, ob's vo scripts/ oder vom Root us lauft
if (grepl("scripts$", getwd())) {
    setwd("..")
} else {
    # Hardkodierti Fallback-Lösig, aber relativ isch sicherer
    try(setwd("c:/Users/User/OneDrive/Desktop/PKU/Master Thesis/R/thesis_france_part"), silent = TRUE)
}

print(paste("Working Directory:", getwd()))

# Usgabeverzeichnis erstelle, wänn si no nidd exischtiere
dirs <- c(
    "outputs",
    "outputs/figures_final",
    "outputs/figures_additional",
    "outputs/animation",
    "outputs/tests",
    "outputs/robustness",
    "outputs/robustness/equivalence",
    "outputs/robustness/final"
)
sapply(dirs, function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE))

# Thema und Hilfsfunktion lade
if (file.exists("R/theme_pku.R")) {
    source("R/theme_pku.R")
    print("Theme Loaded: PKU Academic")
} else {
    warning("R/theme_pku.R not found! Plots may lack consistent styling.")
}

# Hilfsfunktion zum Skript usfüehre mit Ruggmäldig
run_script <- function(path) {
    cat(paste0("\n\n>>> RUNNING: ", path, " <<<\n"))
    t0 <- Sys.time()
    tryCatch(
        {
            source(path, echo = FALSE)
            cat(paste0(">>> SUCCESS: ", path, " (Time: ", round(difftime(Sys.time(), t0, units = "secs"), 1), "s)\n"))
        },
        error = function(e) {
            cat(paste0(">>> FAILURE: ", path, "\nERROR: ", e$message, "\n"))
        }
    )
}

# 3. CHÄRNANALYSE & PIPELINE ---------------------------------------------------
# 04 produziert Hybrid-Visualisierige
run_script("scripts/04_scm_optimization.R")

# 05 produziert d Master-Animation/Plott
run_script("scripts/05_full_pipeline.R")

# 4. VISUALISIERIGS-SUITES (Chärnfigure) ---------------------------------------
# 06 & 07 sin d Hauptthese-Figure
run_script("scripts/06_thesis_figures.R")
run_script("scripts/07_extended_figures.R")

# 5. ADDITIONAL ANALYSIS -------------------------------------------------------
# 08 & 09: Advanced Stats, Heatmaps, Donut Ratios
run_script("scripts/08_advanced_analysis.R")
run_script("scripts/09_additional_openai.R")
run_script("scripts/22_addition_spatial.R")

# 6. ANIMATIONS ----------------------------------------------------------------
# 10: Maps and Trajectories
run_script("scripts/10_animations.R")

# 7. HYPOTHESIS TESTING & STABILITY --------------------------------------------
# 11: Formal Hypothesis Tests
run_script("scripts/11_testing_hypothesis.R")

# 13: Structural Break Variations
run_script("scripts/13_structural_break_variations.R")

# 14 & 15: Equivalence Testing & Layouts
run_script("scripts/14_equivalence_testing.R")
run_script("scripts/15_finalize_stability_analysis.R")

# 8. SUMMARY -------------------------------------------------------------------
# 12: Generates PDF/HTML Summary of all outputs
run_script("scripts/12_summary_PDF.R")

print("\n\n====================================================================")
print("ALL REQUESTED SCRIPTS EXECUTED")
print("Check 'outputs/' directory for results.")
print("====================================================================")
