# ==============================================================================
# Title: Selective Pipeline Runner
# Purpose: Run a subset of the analysis pipeline for targeted updates.
# Author: Luan Blakaj 齐凯
# ==============================================================================

rm(list = ls())
gc()

# 1. SETUP ---------------------------------------------------------------------
# Set project root (Adjust if needed, but this matches your current environment)
# Trying to detect if running from scripts/ or root
if (grepl("scripts$", getwd())) {
    setwd("..")
} else {
    # Hardcoded fallback as requested in user example, though relative is safer
    try(setwd("c:/Users/User/OneDrive/Desktop/PKU/Master Thesis/R/thesis_france_part"), silent = TRUE)
}

print(paste("Working Directory:", getwd()))

# Create output directories if they don't exist
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

# Load Theme and Helpers
if (file.exists("R/theme_pku.R")) {
    source("R/theme_pku.R")
    print("Theme Loaded: PKU Academic")
} else {
    warning("R/theme_pku.R not found! Plots may lack consistent styling.")
}

# Helper to run script with feedback
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

# 3. CORE ANALYSIS & PIPELINE --------------------------------------------------
# 04 produces Hybrid Visualizations
run_script("scripts/01_data_cleaning.R")
run_script("scripts/03_SCM_data_preparation.R")
run_script("scripts/04_scm_optimization.R")

# 05 produces Master Animation/Plot
run_script("scripts/05_full_pipeline.R")

# 4. VISUALIZATION SUITES ----------------------------------------------------------------------------------
# 06 & 07 are main thesis figures
run_script("scripts/06_thesis_figures.R")
run_script("scripts/07_extended_figures.R")

# 5. ADDITIONAL ANALYSIS -------------------------------------------------------
# 08 & 09: Advanced Stats, Heatmaps, Donut Ratios
run_script("scripts/08_advanced_analysis.R")
run_script("scripts/09_additional_openai.R")
run_script("scripts/22_addition_spatial.R")

# 7. HYPOTHESIS TESTING & STABILITY --------------------------------------------
# 11: Formal Hypothesis Tests
run_script("scripts/11_testing_hypothesis.R")

# 14 & 15: Equivalence Testing & Layouts
run_script("scripts/14_equivalence_testing.R")
run_script("scripts/15_finalize_stability_analysis.R")
run_script("scripts/17_differential_effects_analysis.R")


print("\n\n====================================================================")
print("ALL REQUESTED SCRIPTS EXECUTED")
print("Check 'outputs/' directory for results.")
print("====================================================================")
