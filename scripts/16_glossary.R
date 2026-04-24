# ==============================================================================
# Title: Output Glossary Dashboard
# Purpose: Generate an exhaustive HTML glossary of all thesis outputs.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# 1. CATALOG -----------------------------------------------------------------
# Columns: id, path, title, explanation, formula, tags (pipe-separated)


glossary <- tribble(
  ~id, ~path, ~title, ~explanation, ~formula, ~tags,
  "DATA-01", "data/processed/standardized_counts_donor.csv", "Standardized Donor Counts", "Donor data cleaned and mapped to standardized categories by regex.", "count_{y,country,cat} = cleaned(raw_{y,cat})", "Data|Donors|Input",
  "DATA-02", "data/processed/scm_master_dataset_annual.csv", "SCM Master Dataset (Annual)", "France annual totals merged with donors for SCM.", "count_{y,country,cat} = sum_{m in y} count_{m,cat}", "Data|SCM",
  "DATA-03", "data/processed/scm_master_dataset_annual_indexed.csv", "SCM Master (Indexed)", "Annual series indexed to base year for comparability.", "index_{y} = count_{y} / count_{base} * 100", "Data|SCM",
  "DATA-04", "outputs/scm_indexed_trends_check.png", "Indexed Trends Check", "Quick diagnostic that all series start at 100 (2010).", "index_{y} = count_{y} / count_{2010} * 100", "SCM|Diagnostics",
  "DATA-05", "data/processed/scm_master_dataset_indexed.csv", "SCM Master Indexed (Alt)", "Indexed master dataset from script 03.", "index_{y} = count_{y} / count_{min(y)} * 100", "Data|SCM",
  "DATA-06", "data/processed/scm_master_dataset_zscore.csv", "SCM Master Z-Score", "Standardized z-scores for trend comparability.", "z_{y}=(count_{y}-\\mu)/\\sigma", "Data|SCM",
  "DATA-07", "outputs/scm_trends_indexed.png", "SCM Indexed Trends", "Indexed comparisons across donors and France.", "index_{y}=count_{y}/count_{2010}*100", "SCM|Trends",
  "DATA-08", "outputs/scm_trends_zscore.png", "SCM Z-Score Trends", "Z-score comparisons across donors and France.", "z_{y}=(count_{y}-\\mu)/\\sigma", "SCM|Trends",
  "SCM-01", "outputs/scm_optimized_fit.png", "Hybrid SCM Fit", "Monthly real series vs annual synthetic (monthly avg).", "Y^*_m = Y^*_{year}/12", "SCM|Core",
  "SCM-02", "data/processed/scm_optimized_synthetic_france.csv", "Optimized Synthetic Series", "OLS-weighted donor mix in log space with mean alignment.", "\\hat{Y} = \\exp(\\alpha + \\sum_j w_j \\log(Y_j+1)) - 1", "SCM|Data",
  "SCM-03", "outputs/MASTER_SCM_PLOT.png", "Master Pipeline Plot", "Full SCM pipeline output (real monthly vs synthetic annual avg).", "Gap_t=Y_t-\\hat{Y}_t", "SCM|Core",

  "OVK-01", "outputs/figures_final/Fig1_Overall_Trend_Laws.png", "Overall Trend & Laws", "Aggregated political violence with policy markers.", "Y_t=\\sum_{c\\in\\{SA,Incendiary,AFIN\\}} count_{c,t}", "Trend|Laws",
  "OVK-02", "outputs/figures_final/Fig2_Placebo_Test.png", "Placebo: Burglary", "Violence vs burglary indexed to 2010.", "Index_{t} = count_t / count_{2010} * 100", "Robustness|Placebo",
  "OVK-03", "outputs/figures_final/Fig3_Paris_vs_Rest.png", "Paris vs Rest", "Spatial split (Paris vs rest).", "Y_{region,t}=\\sum_{d\\in region} count_{d,t}", "Spatial",
  "OVK-04", "outputs/figures_final/Fig4_Seasonality_Heatmap.png", "Seasonality Heatmap", "Monthly intensity by year.", "Cell_{m,y}=count_{m,y}", "Seasonality|Trend",
  "OVK-05", "outputs/figures_final/Fig5_SCM_Gap.png", "SCM Gap (Annual)", "Annual gap between real and synthetic.", "Gap_y=Y_{Fra,y}-Y^*_{y}", "SCM|Gap",
  "OVK-06", "outputs/figures_final/Fig6_Category_Breakdown.png", "Category Breakdown", "Faceted trends by category.", "Y_{c,t}=count_{c,t}", "Trend|Categories",

  "OVK2-01", "outputs/figures_final/FigA1_Overall_Trend.png", "Overall Trend (Alt)", "Alternative overall trend rendering.", "Y_t=\\sum_{c\\in\\{SA,Incendiary,AFIN\\}} count_{c,t}", "Trend",
  "OVK2-01b", "outputs/figures_final/FigB0_Zoom_CT2014.png", "Zoom: 2014 Counter-Terrorism", "24-month window around Nov 2014.", "Y_t|_{t\\in[t^*-24,t^*+24]}", "Laws|2014",
  "OVK2-02", "outputs/figures_final/FigB1_Zoom_Emergency2015.png", "Zoom: 2015 Emergency", "24-month window around Nov 2015.", "Y_t|_{t\\in[t^*-24,t^*+24]}", "Laws|2015",
  "OVK2-02b", "outputs/figures_final/FigB1b_Zoom_Intel2015.png", "Zoom: 2015 Intelligence Act", "24-month window around Jul 2015.", "Y_t|_{t\\in[t^*-24,t^*+24]}", "Laws|2015",
  "OVK2-03", "outputs/figures_final/FigB2_Zoom_SILT2017.png", "Zoom: 2017 SILT", "24-month window around Oct 2017.", "Y_t|_{t\\in[t^*-24,t^*+24]}", "Laws|2017",
  "OVK2-04", "outputs/figures_final/FigB3_Zoom_Separatism2021.png", "Zoom: 2021 Separatism", "24-month window around Aug 2021.", "Y_t|_{t\\in[t^*-24,t^*+24]}", "Laws|2021",
  "OVK2-05", "outputs/figures_final/FigC1_Spatial_Absolute.png", "Spatial Counts", "Paris vs suburbs vs rest (absolute).", "Y_{region,t}=\\sum_{d\\in region} count_{d,t}", "Spatial",
  "OVK2-06", "outputs/figures_final/FigC2_Spatial_Indexed.png", "Spatial Indexed", "Growth indexed to 2010 avg.", "Index_{r,t}=count_{r,t}/\\overline{count}_{r,2010}*100", "Spatial",
  "OVK2-07", "outputs/figures_final/FigC3_Spatial_Share.png", "Donut Effect", "Share of Paris vs suburbs.", "Share_t=\\frac{Paris_t}{Paris_t+Suburbs_t}", "Spatial",
  "OVK2-08", "outputs/figures_final/FigD1_Placebo_Test.png", "Placebo (Alt)", "Robustness placebo indexed.", "Index_t=count_t/count_{2010}*100", "Robustness",
  "OVK2-09", "outputs/figures_final/FigD2_Category_Breakdown.png", "Category Breakdown (Alt)", "Faceted category trends.", "Y_{c,t}=count_{c,t}", "Categories",
  "OVK2-10", "outputs/figures_final/FigD3_Seasonality.png", "Seasonality Heatmap (Alt)", "Monthly intensity by year.", "Cell_{m,y}=count_{m,y}", "Seasonality",
  "OVK2-11", "outputs/figures_final/FigE1_SCM_Gap.png", "SCM Gap (Alt)", "Gap line/ribbon annual.", "Gap_y=Y_{Fra,y}-Y^*_{y}", "SCM|Gap",

  "AG-01", "outputs/figures_final/Fig_New_A1_RollingVolatility.png", "Rolling Volatility", "12-month rolling SD of violence.", "\\sigma_t=\\sqrt{\\frac{1}{12}\\sum_{i=0}^{11}(x_{t-i}-\\bar{x})^2}", "Stats",
  "AG-02", "outputs/figures_final/Fig_New_A2_STL_Decomposition.png", "STL Decomposition", "Trend/seasonal/remainder split.", "Y_t=T_t+S_t+R_t", "Stats",
  "AG-03", "outputs/figures_final/Fig_New_A3_LagPlot.png", "Lag Plot", "Persistence check (t vs t-1).", "\\rho_1=Corr(Y_t,Y_{t-1})", "Stats",
  "AG-04", "outputs/figures_final/Fig_New_A4_LongTermHistory.png", "Long-Term History", "Stitched DataGouv + SSMSI.", "Y_t=Y_{DataGouv}\\cup Y_{SSMSI}", "Trend|LongData",
  "AG-05", "outputs/figures_final/Fig_New_A5_StructuralBreaks.png", "Structural Break Test", "F-statistics over time.", "F=(RSS_R-RSS_U)/(RSS_U/(n-2k))", "Tests",
  "AG-06", "outputs/figures_final/Fig_New_B1_GTD_Map.png", "GTD Terror Map", "Point map of terror incidents.", "Coord_i=(lat_i,lon_i), size\\propto fatalities_i", "Maps",
  "AG-07", "outputs/figures_final/Fig_New_B2_ACLED_Heatmap.png", "ACLED Heatmap", "Kernel density of events.", "Density(x,y)=\\frac{1}{n}\\sum K_H(x-X_i)", "Maps",
  "AG-08", "outputs/figures_final/Fig_New_B3_DonutRatio.png", "Donut Ratio", "Suburbs/Paris ratio over time.", "Ratio_t=Suburbs_t/Paris_t", "Spatial",
  "AG-09", "outputs/figures_final/Fig_New_B4_Dept_Growth.png", "Dept Growth (Top 20)", "% change 2015 to 2022.", "\\Delta\\%_d=(Y_{d,2022}-Y_{d,2015})/Y_{d,2015}*100", "Spatial",
  "AG-10", "outputs/figures_final/Fig_New_C1_SCM_RawComparison.png", "Raw SCM Comparison", "France vs donors (raw counts).", "Y_{c,y}=count_{c,y}", "SCM|Robustness",
  "AG-11", "outputs/figures_final/Fig_New_C2_MSPE_Ratio.png", "MSPE Ratio (Mock)", "Pre vs post prediction errors.", "Gap_t=|Y_t-\\hat{Y}_t|", "SCM|Robustness",
  "AG-12", "outputs/figures_final/Fig_New_D1_ViolinDist.png", "Yearly Distribution", "Violence distribution by year.", "f_y(x)=density(Y_{t\\in y})", "Descriptive",
  "AG-13", "outputs/figures_final/Fig_New_D2_CorrelationMatrix.png", "Correlation Matrix", "Correlation of categories.", "\\rho_{ij}=\\frac{cov(i,j)}{\\sigma_i\\sigma_j}", "Descriptive",
  "AG-14", "outputs/figures_final/Fig_New_D3_ACLED_Subtypes.png", "ACLED Subtypes", "Top 10 sub_event_type counts.", "n_k=\\sum 1{sub\\_event\\_type=k}", "Descriptive",
  "AG-15", "outputs/figures_final/Fig_New_D4_TemporalHeatmap.png", "Temporal Heatmap", "Month/year intensity.", "Cell_{m,y}=count_{m,y}", "Seasonality",
  "ADD-01", "outputs/figures_additional/Table1_Pre_Post_Averages.csv", "Pre/Post Averages", "Average monthly counts by period.", "\\Delta\\%=(\\mu_{post}/\\mu_{pre}-1)*100", "Tables",
  "ADD-02", "outputs/figures_additional/Table2_Department_Totals.csv", "Department Totals", "Total violence per department.", "Total_d=\\sum_t Y_{d,t}", "Tables",
  "ADD-03", "outputs/figures_additional/Table3_ACLED_Event_Types.csv", "ACLED Event Types", "Counts by event_type.", "n_k=\\sum 1{event\\_type=k}", "Tables",
  "ADD-04", "outputs/figures_additional/FigE1_Category_Trends.png", "Category Trends", "Monthly trends by category.", "Y_{c,t}=count_{c,t}", "Trend",
  "ADD-05", "outputs/figures_additional/FigE2_Political_Rolling12.png", "Rolling Mean", "12-month rolling mean.", "\\bar{Y}_t=\\frac{1}{12}\\sum_{i=0}^{11}Y_{t-i}", "Trend",
  "ADD-06", "outputs/figures_additional/FigE3_Source_Overlap.png", "Source Overlap", "Etat 4001 vs SSMSI continuity.", "Y_{source,t}", "Data|Validation",
  "ADD-07", "outputs/figures_additional/FigE4_Pre_Post_Change.png", "Pre/Post Change", "% change by category.", "\\Delta\\%=(\\mu_{post}-\\mu_{pre})/\\mu_{pre}*100", "Stats",
  "ADD-08", "outputs/figures_additional/FigE5_YoY_Change.png", "YoY Change", "Year-over-year growth.", "YoY_t=(Y_t/Y_{t-12}-1)*100", "Stats",
  "ADD-09", "outputs/figures_additional/FigE6_Cumulative.png", "Cumulative Count", "Running sum of incidents.", "C_t=\\sum_{i=1}^t Y_i", "Trend",
  "ADD-10", "outputs/figures_additional/FigE7_Monthly_Distribution.png", "Monthly Distribution", "Histogram of monthly counts.", "f(x)=hist(Y_t)", "Descriptive",
  "ADD-11", "outputs/figures_additional/FigE8_Seasonal_Profile.png", "Seasonal Profile", "Average by month.", "\\bar{Y}_m=\\frac{1}{N_m}\\sum_{t\\in m}Y_t", "Seasonality",
  "ADD-12", "outputs/figures_additional/FigE9_Category_Seasonality.png", "Category Seasonality", "Average by month and category.", "\\bar{Y}_{c,m}=\\frac{1}{N_{c,m}}\\sum Y_{c,t}", "Seasonality",
  "ADD-13", "outputs/figures_additional/FigF1_Top_Departments.png", "Top Departments", "Highest totals 2010-2022.", "Total_d=\\sum_t Y_{d,t}", "Spatial",
  "ADD-14", "outputs/figures_additional/FigF2_Department_Share_2022.png", "Dept Share 2022", "Share of national total.", "Share_d=Y_{d,2022}/\\sum_d Y_{d,2022}", "Spatial",
  "ADD-15", "outputs/figures_additional/FigF3_Department_Slope_Change.png", "Slope Change", "Post-Pre slope difference.", "\\Delta\\beta_d=\\beta_{post}-\\beta_{pre}", "Spatial",
  "ADD-16", "outputs/figures_additional/FigF4_Paris_Suburbs_Indexed.png", "Paris vs Suburbs (Indexed)", "Indexed regional trends.", "Index_{r,t}=count_{r,t}/\\overline{count}_{r,2010}*100", "Spatial",
  "ADD-17", "outputs/figures_additional/FigG1_Event_Study_2015.png", "Event Study 2015", "Avg incidents by month from event.", "E_k=\\bar{Y}_{t^*+k}", "Laws|2015",
  "ADD-18", "outputs/figures_additional/FigG2_Event_Study_2017.png", "Event Study 2017", "Avg incidents by month from event.", "E_k=\\bar{Y}_{t^*+k}", "Laws|2017",
  "ADD-19", "outputs/figures_additional/FigG3_Event_Study_2021.png", "Event Study 2021", "Avg incidents by month from event.", "E_k=\\bar{Y}_{t^*+k}", "Laws|2021",
  "ADD-20", "outputs/figures_additional/FigH1_Donor_Trends.png", "Donor Trends", "France vs donors by category.", "Y_{country,y}=count_{country,y}", "SCM|Donors",
  "ADD-21", "outputs/figures_additional/FigH2_Indexed_SCM.png", "Indexed SCM Trends", "Indexed donor comparison.", "Index_{y}=count_{y}/count_{2010}*100", "SCM|Donors",
  "ADD-22", "outputs/figures_additional/FigH3_SCM_Gap_Bars.png", "SCM Gap Bars", "Annual gap bars.", "Gap_y=Y_{Fra,y}-Y^*_{y}", "SCM|Gap",
  "ADD-23", "outputs/figures_additional/FigI1_ACLED_Point_Map.png", "ACLED Point Map", "Event locations (2020-2025).", "Coord_i=(lat_i,lon_i)", "Maps",
  "ADD-24", "outputs/figures_additional/FigI2_ACLED_Event_Types.png", "ACLED Event Types Map", "Event locations by type.", "Coord_i=(lat_i,lon_i)", "Maps",
  "ADD-25", "outputs/figures_additional/FigI3_ACLED_Density.png", "ACLED Density", "2D bin density.", "Density(x,y)=bin_count", "Maps",
  "ADD-26", "outputs/figures_additional/FigI4_ACLED_Monthly_Trend.png", "ACLED Monthly Trend", "Monthly ACLED event counts.", "N_m=\\sum 1{event\\_month=m}", "Trend",
  "ADD-27", "outputs/figures_additional/FigI5_ACLED_Fatalities.png", "ACLED Fatalities", "Histogram of fatalities.", "f(x)=hist(fatalities)", "Descriptive",

  "ANIM-01", "outputs/animation/serieschrono_animation.gif", "Serieschrono Heat Animation", "Animated density by year.", "Density_t(x,y)=\\sum K_H(x-X_{i,t})", "Animations",
  "ANIM-02", "outputs/animation/serieschrono_center_trajectory.png", "Serieschrono Center Gravity", "Trajectory of weighted mean center.", "(\\bar{x}_t,\\bar{y}_t)=\\sum w_i(x_i,y_i)/\\sum w_i", "Animations",
  "ANIM-03", "outputs/animation/acled_animation.gif", "ACLED Heat Animation", "Animated density by year.", "Density_t(x,y)=\\sum K_H(x-X_{i,t})", "Animations",
  "ANIM-04", "outputs/animation/acled_center_trajectory.png", "ACLED Center Gravity", "Trajectory of mean event location.", "(\\bar{x}_t,\\bar{y}_t)=\\sum w_i(x_i,y_i)/\\sum w_i", "Animations",
  "ANIM-05", "outputs/animation/gtd_animation.gif", "GTD Heat Animation", "Animated terror density by year.", "Density_t(x,y)=\\sum K_H(x-X_{i,t})", "Animations",
  "ANIM-06", "outputs/animation/gtd_center_trajectory.png", "GTD Center Gravity", "Trajectory of mean terror location.", "(\\bar{x}_t,\\bar{y}_t)=\\sum w_i(x_i,y_i)/\\sum w_i", "Animations",

  "TEST-01", "outputs/tests/Hypothesis_Test_FR2014_CT.png", "Hypothesis 2014", "Real vs synthetic around 2014 CT law.", "\\Delta\\mu=(\\bar{Y}_{post}-\\bar{Y}_{pre})/\\bar{Y}_{pre}", "Tests|2014",
  "TEST-02", "outputs/tests/Hypothesis_Test_FR2015_INTEL.png", "Hypothesis 2015 Intel", "Real vs synthetic around 2015 Intel law.", "\\Delta\\mu=(\\bar{Y}_{post}-\\bar{Y}_{pre})/\\bar{Y}_{pre}", "Tests|2015",
  "TEST-03", "outputs/tests/Hypothesis_Test_FR2015_ESTATE.png", "Hypothesis 2015 SoE", "Real vs synthetic around 2015 SoE law.", "\\Delta\\mu=(\\bar{Y}_{post}-\\bar{Y}_{pre})/\\bar{Y}_{pre}", "Tests|2015",
  "TEST-04", "outputs/tests/Hypothesis_Test_FR2017_SILT.png", "Hypothesis 2017 SILT", "Real vs synthetic around 2017 SILT law.", "\\Delta\\mu=(\\bar{Y}_{post}-\\bar{Y}_{pre})/\\bar{Y}_{pre}", "Tests|2017",
  "TEST-05", "outputs/tests/Hypothesis_Test_FR2021_PACTI.png", "Hypothesis 2021 PACTI", "Real vs synthetic around 2021 PACTI law.", "\\Delta\\mu=(\\bar{Y}_{post}-\\bar{Y}_{pre})/\\bar{Y}_{pre}", "Tests|2021",
  "TEST-06", "outputs/tests/Impact_vs_Synth_Gap.png", "Gap Analysis (Monthly)", "Monthly gap with smooth.", "Gap_t=Y_t-\\hat{Y}_t", "Tests|SCM",
  "TEST-07", "outputs/tests/statistical_results.txt", "Chow Test Results", "Text output of Chow tests.", "F=(RSS_R-RSS_U)/(RSS_U/(n-2k))", "Tests|Text",

  "VAR-01", "outputs/robustness/Plot_V1_Monthly.png", "Variation V1 (Monthly)", "Baseline monthly series.", "Y_t=count_t", "Variations",
  "VAR-02", "outputs/robustness/Plot_V2_Log.png", "Variation V2 (Log)", "Log-transformed series.", "Y'_t=\\log(Y_t+1)", "Variations",
  "VAR-03", "outputs/robustness/Plot_V3_Quarterly.png", "Variation V3 (Quarterly)", "Quarterly aggregated series.", "Y_q=\\sum_{t\\in q}Y_t", "Variations",
  "VAR-04", "outputs/robustness/Plot_V4_Annual.png", "Variation V4 (Annual)", "Annual aggregated series.", "Y_y=\\sum_{t\\in y}Y_t", "Variations",
  "VAR-05", "outputs/robustness/Plot_V5_Rolling6m.png", "Variation V5 (Rolling 6m)", "6-month rolling mean.", "\\bar{Y}_t=\\frac{1}{6}\\sum_{i=0}^{5}Y_{t-i}", "Variations",
  "VAR-06", "outputs/robustness/Plot_V6_Ratio.png", "Variation V6 (Ratio)", "Violence/Burglary ratio.", "R_t=Y^{Violence}_t/Y^{Burglary}_t", "Variations",
  "VAR-07", "outputs/robustness/Summary_P_Values_Matrix.csv", "P-Value Matrix", "P-values by law and variation.", "p=Pr(F\\ge f)", "Variations|Tables",
  "VAR-08", "outputs/robustness/Ranking_Best_Fit.csv", "Variation Ranking", "Count of non-significant laws per variation.", "NS_count=\\sum 1{p>0.05}", "Variations|Tables",
  "VAR-09", "outputs/robustness/Results_Overview.html", "Results Overview (HTML)", "HTML summary table of p-values.", "p=Pr(F\\ge f)", "Variations|HTML",
  "VAR-10", "outputs/robustness/T_V1.png", "Gap T_V1 (Monthly)", "Gap on monthly level.", "Gap_t=Y_t-\\hat{Y}_t", "Variations|Gap",
  "VAR-11", "outputs/robustness/T_V2.png", "Gap T_V2 (Log)", "Gap in log space.", "Gap_t=\\log(Y_t+1)-\\log(\\hat{Y}_t+1)", "Variations|Gap",
  "VAR-12", "outputs/robustness/T_V3.png", "Gap T_V3 (Quarterly)", "Quarterly gap.", "Gap_q=Y_q-\\hat{Y}_q", "Variations|Gap",
  "VAR-13", "outputs/robustness/T_V4.png", "Gap T_V4 (Annual)", "Annual gap.", "Gap_y=Y_y-\\hat{Y}_y", "Variations|Gap",
  "VAR-14", "outputs/robustness/T_V5.png", "Gap T_V5 (Rolling)", "Gap on 6-month rolling mean.", "Gap_t=\\bar{Y}_t-\\bar{\\hat{Y}}_t", "Variations|Gap",
  "VAR-15", "outputs/robustness/T_V6.png", "Gap T_V6 (Ratio)", "Gap on ratio vs baseline.", "Gap_t=R_t-\\bar{R}_{pre}", "Variations|Gap",
  "EQ-01", "outputs/robustness/equivalence/Equivalence_Results.csv", "Equivalence Results", "TOST results table.", "p_{TOST}=\\max(p_1,p_2)", "TOST|Tables",
  "EQ-02", "outputs/robustness/equivalence/Equivalence_Summary.html", "Equivalence Summary (HTML)", "HTML summary of TOST.", "p_{TOST}=\\max(p_1,p_2)", "TOST|HTML",
  "EQ-03", "outputs/robustness/equivalence/TOST_Annual_FR2014_CT_2SD.png", "TOST Annual 2014 (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2014",
  "EQ-04", "outputs/robustness/equivalence/TOST_Annual_FR2014_CT_4SD.png", "TOST Annual 2014 (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2014",
  "EQ-05", "outputs/robustness/equivalence/TOST_Annual_FR2015_INTEL_2SD.png", "TOST Annual 2015 Intel (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2015",
  "EQ-06", "outputs/robustness/equivalence/TOST_Annual_FR2015_INTEL_4SD.png", "TOST Annual 2015 Intel (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2015",
  "EQ-07", "outputs/robustness/equivalence/TOST_Annual_FR2015_ESTATE_2SD.png", "TOST Annual 2015 SoE (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2015",
  "EQ-08", "outputs/robustness/equivalence/TOST_Annual_FR2015_ESTATE_4SD.png", "TOST Annual 2015 SoE (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2015",
  "EQ-09", "outputs/robustness/equivalence/TOST_Annual_FR2017_SILT_2SD.png", "TOST Annual 2017 SILT (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2017",
  "EQ-10", "outputs/robustness/equivalence/TOST_Annual_FR2017_SILT_4SD.png", "TOST Annual 2017 SILT (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2017",
  "EQ-11", "outputs/robustness/equivalence/TOST_Annual_FR2021_PACTI_2SD.png", "TOST Annual 2021 PACTI (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2021",
  "EQ-12", "outputs/robustness/equivalence/TOST_Annual_FR2021_PACTI_4SD.png", "TOST Annual 2021 PACTI (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2021",
  "EQ-13", "outputs/robustness/equivalence/TOST_Quarterly_FR2014_CT_2SD.png", "TOST Quarterly 2014 (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2014",
  "EQ-14", "outputs/robustness/equivalence/TOST_Quarterly_FR2014_CT_4SD.png", "TOST Quarterly 2014 (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2014",
  "EQ-15", "outputs/robustness/equivalence/TOST_Quarterly_FR2015_INTEL_2SD.png", "TOST Quarterly 2015 Intel (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2015",
  "EQ-16", "outputs/robustness/equivalence/TOST_Quarterly_FR2015_INTEL_4SD.png", "TOST Quarterly 2015 Intel (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2015",
  "EQ-17", "outputs/robustness/equivalence/TOST_Quarterly_FR2015_ESTATE_2SD.png", "TOST Quarterly 2015 SoE (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2015",
  "EQ-18", "outputs/robustness/equivalence/TOST_Quarterly_FR2015_ESTATE_4SD.png", "TOST Quarterly 2015 SoE (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2015",
  "EQ-19", "outputs/robustness/equivalence/TOST_Quarterly_FR2017_SILT_2SD.png", "TOST Quarterly 2017 SILT (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2017",
  "EQ-20", "outputs/robustness/equivalence/TOST_Quarterly_FR2017_SILT_4SD.png", "TOST Quarterly 2017 SILT (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2017",
  "EQ-21", "outputs/robustness/equivalence/TOST_Quarterly_FR2021_PACTI_2SD.png", "TOST Quarterly 2021 PACTI (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2021",
  "EQ-22", "outputs/robustness/equivalence/TOST_Quarterly_FR2021_PACTI_4SD.png", "TOST Quarterly 2021 PACTI (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2021",
  "EQ-23", "outputs/robustness/equivalence/TOST_Log_Annual_FR2014_CT_2SD.png", "TOST Log-Annual 2014 (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2014",
  "EQ-24", "outputs/robustness/equivalence/TOST_Log_Annual_FR2014_CT_4SD.png", "TOST Log-Annual 2014 (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2014",
  "EQ-25", "outputs/robustness/equivalence/TOST_Log_Annual_FR2015_INTEL_2SD.png", "TOST Log-Annual 2015 Intel (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2015",
  "EQ-26", "outputs/robustness/equivalence/TOST_Log_Annual_FR2015_INTEL_4SD.png", "TOST Log-Annual 2015 Intel (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2015",
  "EQ-27", "outputs/robustness/equivalence/TOST_Log_Annual_FR2015_ESTATE_2SD.png", "TOST Log-Annual 2015 SoE (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2015",
  "EQ-28", "outputs/robustness/equivalence/TOST_Log_Annual_FR2015_ESTATE_4SD.png", "TOST Log-Annual 2015 SoE (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2015",
  "EQ-29", "outputs/robustness/equivalence/TOST_Log_Annual_FR2017_SILT_2SD.png", "TOST Log-Annual 2017 SILT (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2017",
  "EQ-30", "outputs/robustness/equivalence/TOST_Log_Annual_FR2017_SILT_4SD.png", "TOST Log-Annual 2017 SILT (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2017",
  "EQ-31", "outputs/robustness/equivalence/TOST_Log_Annual_FR2021_PACTI_2SD.png", "TOST Log-Annual 2021 PACTI (2SD)", "Stability zone and mean shift.", "\\delta=2\\cdot SD_{pre}", "TOST|2021",
  "EQ-32", "outputs/robustness/equivalence/TOST_Log_Annual_FR2021_PACTI_4SD.png", "TOST Log-Annual 2021 PACTI (4SD)", "Stability zone and mean shift.", "\\delta=4\\cdot SD_{pre}", "TOST|2021",

  "FIN-01", "outputs/robustness/final/Significance_Heatmap.png", "Stability Heatmap", "Summary of TOST outcomes.", "p_{TOST}<0.05", "TOST|Core",
  "FIN-02", "outputs/robustness/final/Final_Impact_vs_Synth_Gap_Log.png", "Log Gap Analysis", "Log gap between real and synthetic.", "Gap_t=\\log(Y_t+1)-\\log(\\hat{Y}_t+1)", "SCM|Gap",
  "FIN-03", "outputs/robustness/final/Stability_Proof_FR2014_CT.png", "Stability Proof 2014", "TOST stability zone (2014 law).", "-\\delta < \\beta < \\delta", "TOST|2014",
  "FIN-04", "outputs/robustness/final/Stability_Proof_FR2015_INTEL.png", "Stability Proof 2015 Intel", "TOST stability zone (2015 Intel).", "-\\delta < \\beta < \\delta", "TOST|2015",
  "FIN-05", "outputs/robustness/final/Stability_Proof_FR2015_ESTATE.png", "Stability Proof 2015 SoE", "TOST stability zone (2015 SoE).", "-\\delta < \\beta < \\delta", "TOST|2015",
  "FIN-06", "outputs/robustness/final/Stability_Proof_FR2017_SILT.png", "Stability Proof 2017 SILT", "TOST stability zone (2017 SILT).", "-\\delta < \\beta < \\delta", "TOST|2017",
  "FIN-07", "outputs/robustness/final/Stability_Proof_FR2021_PACTI.png", "Stability Proof 2021 PACTI", "TOST stability zone (2021 PACTI).", "-\\delta < \\beta < \\delta", "TOST|2021",
  "FIN-08", "outputs/robustness/final/Final_Explanation.html", "Final Explanation (HTML)", "Narrative explainer with formulas.", "TOST formulas", "HTML"
)

# 1b. Enrich glossary with full formulas and notes ----------------------------
full_tost <- "\\begin{aligned}H_{01}: \\Delta \\le -\\delta,\\; H_{02}: \\Delta \\ge +\\delta\\\\H_{A}: -\\delta < \\Delta < +\\delta\\\\\\delta = k \\cdot SD_{pre}\\\\t_1 = \\frac{\\hat{\\beta}-(-\\delta)}{SE(\\hat{\\beta})},\\; t_2 = \\frac{\\hat{\\beta}-\\delta}{SE(\\hat{\\beta})}\\\\p_{TOST}=\\max(P(T\\ge t_1),\\; P(T\\le t_2))\\end{aligned}"
full_scm <- "\\begin{aligned}\\log(Y_{F,t}+1) = \\alpha + \\sum_{j} w_j \\log(Y_{j,t}+1) + \\varepsilon_t\\\\\\hat{Y}_{F,t} = \\exp\\Big(\\hat{\\alpha}+\\sum_j \\hat{w}_j\\log(Y_{j,t}+1)\\Big)-1\\\\\\hat{Y}^{adj}_{F,t}=\\hat{Y}_{F,t}+(\\overline{Y}_{F,pre}-\\overline{\\hat{Y}}_{F,pre})\\end{aligned}"
full_scm_monthly <- "\\begin{aligned}\\hat{Y}^{annual}_{F,t} &= \\exp\\Big(\\hat{\\alpha}+\\sum_j \\hat{w}_j\\log(Y_{j,t}+1)\\Big)-1\\\\\\hat{Y}^{monthly}_{F,t} &= \\hat{Y}^{annual}_{F,t}/12\\end{aligned}"
full_gap <- "Gap_t = Y_t - \\hat{Y}_t"
full_log_gap <- "Gap_t = \\log(Y_t+1) - \\log(\\hat{Y}_t+1)"
full_index <- "Index_t = \\frac{Y_t}{Y_{base}} \\times 100"
full_z <- "z_t = \\frac{Y_t-\\mu}{\\sigma}"
full_roll_sd <- "\\sigma_t=\\sqrt{\\frac{1}{n}\\sum_{i=0}^{n-1}(Y_{t-i}-\\bar{Y}_t)^2}"
full_roll_mean <- "\\bar{Y}_t=\\frac{1}{n}\\sum_{i=0}^{n-1}Y_{t-i}"
full_cum <- "C_t=\\sum_{i=1}^{t}Y_i"
full_ratio <- "R_t=\\frac{Y^{A}_t}{Y^{B}_t}"
full_event <- "E_k=\\frac{1}{N_k}\\sum_{t\\in k}Y_{t^*+k}"
full_correlation <- "\\rho_{ij}=\\frac{cov(X_i,X_j)}{\\sigma_i\\sigma_j}"
full_stl <- "Y_t = T_t + S_t + R_t"
full_chow <- "F = \\frac{(RSS_R - RSS_U)/k}{RSS_U/(n-2k)}"
full_kde <- "\\hat{f}_h(x,y)=\\frac{1}{n h^2}\\sum_{i=1}^{n} K\\Big(\\frac{x-x_i}{h},\\frac{y-y_i}{h}\\Big)"
full_center <- "(\\bar{x}_t,\\bar{y}_t)=\\Big(\\frac{\\sum_i w_i x_i}{\\sum_i w_i},\\frac{\\sum_i w_i y_i}{\\sum_i w_i}\\Big)"

glossary <- glossary %>%
  mutate(
    short_formula = formula,
    full_formula = case_when(
      id == "SCM-02" ~ full_scm,
      id == "SCM-01" ~ full_scm_monthly,
      str_detect(title, "Log Gap") ~ full_log_gap,
      str_detect(tags, "Gap") ~ full_gap,
      str_detect(title, "Indexed") ~ full_index,
      str_detect(title, "Z-Score") ~ full_z,
      str_detect(title, "Rolling Volatility") ~ full_roll_sd,
      str_detect(title, "Rolling Mean") ~ full_roll_mean,
      str_detect(title, "Cumulative") ~ full_cum,
      str_detect(title, "Event Study") ~ full_event,
      str_detect(title, "Correlation") ~ full_correlation,
      str_detect(title, "STL") ~ full_stl,
      str_detect(title, "Structural Break") ~ full_chow,
      str_detect(tags, "Maps") & str_detect(title, "Heatmap|Density") ~ full_kde,
      str_detect(tags, "Animations") & str_detect(title, "Heat") ~ full_kde,
      str_detect(tags, "Animations") & str_detect(title, "Center") ~ full_center,
      str_detect(title, "Ratio") ~ full_ratio,
      str_detect(tags, "TOST") ~ full_tost,
      TRUE ~ formula
    ),
    notes = case_when(
      str_detect(tags, "SCM") ~ "Synthetic Control is fit on the pre-treatment window using a log-linear donor mix and then back-transformed; results are interpreted as a counterfactual path.",
      str_detect(tags, "TOST") ~ "Equivalence testing reverses the burden of proof: stability is supported when the estimated shift lies within the pre-variance tolerance band.",
      str_detect(tags, "Tests") ~ "Structural break tests assess whether a policy date coincides with a statistically significant change in level or trend.",
      str_detect(tags, "Maps") ~ "Spatial layers use the France outline and plot events or densities to visualize concentration.",
      str_detect(tags, "Animations") ~ "Animations show the evolution of event density or the mean center over time.",
      TRUE ~ ""
    ),
    script_hint = case_when(
      str_detect(path, "outputs/figures_final/Fig[1-6]_") ~ "scripts/06_thesis_figures.R",
      str_detect(path, "outputs/figures_final/Fig[A-E]") ~ "scripts/07_extended_figures.R",
      str_detect(path, "outputs/figures_final/Fig_New_") ~ "scripts/08_advanced_analysis.R",
      str_detect(path, "outputs/figures_additional/") ~ "scripts/09_additional_openai.R",
      str_detect(path, "outputs/animation/") ~ "scripts/10_animations.R",
      str_detect(path, "outputs/tests/") ~ "scripts/11_testing_hypothesis.R",
      str_detect(path, "outputs/robustness/equivalence/") ~ "scripts/14_equivalence_testing.R",
      str_detect(path, "outputs/robustness/final/") ~ "scripts/15_finalize_stability_analysis.R",
      str_detect(path, "outputs/robustness/") ~ "scripts/13_structural_break_variations.R",
      path %in% c("outputs/scm_optimized_fit.png", "data/processed/scm_optimized_synthetic_france.csv") ~ "scripts/04_scm_optimization.R",
      path == "outputs/MASTER_SCM_PLOT.png" ~ "scripts/05_full_pipeline.R",
      str_detect(path, "scm_master_dataset|scm_trends") ~ "scripts/03_SCM_data_preparation.R",
      path == "data/processed/standardized_counts_donor.csv" ~ "scripts/02_donor_data.R",
      path == "outputs/scm_indexed_trends_check.png" ~ "scripts/01_data_cleaning.R",
      TRUE ~ ""
    ),
    data_source = case_when(
      str_detect(path, "ACLED") ~ "ACLED",
      str_detect(path, "GTD") ~ "GTD",
      str_detect(path, "serieschrono") ~ "DataGouv/SSMSI",
      str_detect(path, "department") ~ "SSMSI",
      str_detect(path, "scm_optimized|scm_master") ~ "SSMSI + Donors",
      str_detect(path, "fraud") ~ "SSMSI + SCM",
      TRUE ~ "SSMSI"
    )
  )

# Exclude exploratory equivalence testing outputs from glossary cards
glossary <- glossary %>%
  filter(!str_detect(path, "outputs/robustness/equivalence/"))

# Normalize tags and build tag list
parse_tags <- function(tags_str) {
  tags_str <- as.character(tags_str)
  if (length(tags_str) == 0 || is.na(tags_str)) {
    return(character(0))
  }
  str_split(tags_str, "\\|")[[1]] %>%
    str_trim() %>%
    purrr::discard(~ .x == "")
}

all_tags <- glossary %>%
  pull(tags) %>%
  purrr::map(parse_tags) %>%
  unlist() %>%
  unique() %>%
  sort()

# Root path for quick file lookup
root_path <- "c:\\Users\\User\\OneDrive\\Desktop\\PKU\\Master Thesis\\R\\thesis_france_part"

# 2. HTML OUTPUT ---------------------------------------------------------------
html_file <- file.path("outputs", "Thesis_Glossary.html")
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

sink(html_file)

cat("<!DOCTYPE html>\n<html lang='en'>\n<head>\n")
cat("<meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1.0'>\n")
cat("<title>Thesis Glossary Dashboard</title>\n")
cat("<script src=\"https://polyfill.io/v3/polyfill.min.js?features=es6\"></script>\n")
cat("<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>\n")

cat("<style>\n")
cat(":root { --pku-red: #8A0000; --ink: #1a1a1a; --muted: #5b5b5b; --panel: #f2efe9; --wash: #ebe7e0; --accent: #1F3A5F; }\n")
cat("body { font-family: 'Palatino Linotype','Book Antiqua',serif; margin: 0; padding: 0; display: flex; height: 100vh; background: #e9e4dd; color: var(--ink); }\n")
cat(".sidebar { width: 280px; background: #e7e2db; color: #2a2a2a; overflow-y: auto; padding: 24px; box-sizing: border-box; flex-shrink: 0; box-shadow: inset 6px 6px 12px #d4cfc8, inset -6px -6px 12px #f7f3ee; }\n")
cat(".sidebar h2 { color: var(--pku-red); font-size: 1.1em; letter-spacing: 0.5px; border-bottom: 1px solid #cfcac4; padding-bottom: 10px; font-family: 'Georgia',serif; }\n")
cat(".sidebar a { display: block; color: #4d4a46; text-decoration: none; padding: 6px 0; font-size: 0.82em; transition: 0.2s; }\n")
cat(".sidebar a:hover { color: #000; padding-left: 6px; }\n")
cat(".main { flex-grow: 1; overflow-y: auto; padding: 26px 44px; box-sizing: border-box; }\n")
cat("h1 { color: var(--pku-red); border-bottom: 2px solid var(--pku-red); padding-bottom: 10px; font-family: 'Georgia',serif; }\n")
cat(".tabs { display: flex; gap: 8px; margin: 12px 0 18px; }\n")
cat(".tab-btn { padding: 8px 14px; border: none; background: #e9e4dd; cursor: pointer; border-radius: 10px; font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; font-size: 0.9em; box-shadow: 6px 6px 12px #d2cdc6, -6px -6px 12px #f8f3ee; }\n")
cat(".tab-btn.active { color: var(--pku-red); box-shadow: inset 5px 5px 10px #d2cdc6, inset -5px -5px 10px #f8f3ee; }\n")
cat(".tab-content { display: none; }\n")
cat(".tab-content.active { display: block; }\n")
cat(".filter-bar { background: #e9e4dd; padding: 12px; margin-bottom: 20px; border-radius: 12px; box-shadow: inset 6px 6px 12px #d2cdc6, inset -6px -6px 12px #f8f3ee; display: flex; gap: 10px; flex-wrap: wrap; align-items: center; position: sticky; top: 0; z-index: 100; }\n")
cat(".filter-btn { padding: 6px 12px; border: none; background: #e9e4dd; cursor: pointer; border-radius: 10px; font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; font-size: 0.82em; box-shadow: 5px 5px 10px #d2cdc6, -5px -5px 10px #f8f3ee; }\n")
cat(".filter-btn:hover, .filter-btn.active { color: var(--pku-red); box-shadow: inset 4px 4px 8px #d2cdc6, inset -4px -4px 8px #f8f3ee; }\n")
cat(".fig-card { background: #e9e4dd; padding: 18px; margin-bottom: 22px; border-radius: 14px; box-shadow: 8px 8px 16px #d1ccc5, -8px -8px 16px #f8f3ee; transition: 0.25s; }\n")
cat(".fig-card:hover { box-shadow: 10px 10px 18px #cfcac3, -10px -10px 18px #fbf6f1; transform: translateY(-1px); }\n")
cat(".fig-card h3 { margin-top: 0; color: #333; display: flex; justify-content: space-between; align-items: center; font-family: 'Georgia',serif; }\n")
cat(".fig-id { font-size: 0.78em; background: #f1edea; padding: 2px 6px; border-radius: 4px; color: #6a635d; font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; }\n")
cat("img { max-width: 100%; max-height: 440px; width: auto; height: auto; border: 1px solid #eee; display: block; margin: 12px auto; box-shadow: 0 1px 4px rgba(0,0,0,0.12); }\n")
cat(".explanation { background: #e9e4dd; padding: 10px; border-left: 4px solid var(--pku-red); color: #555; font-style: italic; box-shadow: inset 3px 3px 6px #d3cec7, inset -3px -3px 6px #f7f2ed; border-radius: 8px; }\n")
cat(".meta { display: grid; grid-template-columns: 1fr 1fr; gap: 8px 14px; margin-top: 10px; font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; font-size: 0.85em; color: var(--muted); }\n")
cat(".meta strong { color: #2a2a2a; font-weight: 600; }\n")
cat(".formula { background: #e9e4dd; padding: 10px; margin-top: 10px; font-family: 'Times New Roman'; text-align: center; position: relative; border-radius: 10px; box-shadow: inset 4px 4px 8px #d2cdc6, inset -4px -4px 8px #f8f3ee; }\n")
cat(".formula-label { font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; font-size: 0.8em; text-transform: uppercase; letter-spacing: 0.06em; color: #6e6259; margin-bottom: 4px; }\n")
cat("details.formula-full { margin-top: 10px; }\n")
cat("details.formula-full summary { cursor: pointer; font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; font-size: 0.85em; color: var(--accent); }\n")
cat(".copy-btn { position: absolute; right: 6px; top: 6px; background: #e9e4dd; border: none; font-size: 0.7em; cursor: pointer; padding: 2px 6px; border-radius: 6px; box-shadow: 4px 4px 8px #d2cdc6, -4px -4px 8px #f8f3ee; }\n")
cat(".copy-btn:hover { box-shadow: inset 3px 3px 6px #d2cdc6, inset -3px -3px 6px #f8f3ee; }\n")
cat(".notes { margin-top: 8px; padding: 8px 10px; background: #f5f1ed; border-radius: 6px; font-size: 0.88em; color: #5a524c; }\n")
cat(".tags { margin-top: 10px; }\n")
cat(".tag { display: inline-block; background: #e6e0db; padding: 2px 8px; border-radius: 10px; font-size: 0.72em; color: #5d564f; margin-right: 5px; font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; }\n")
cat(".missing { color: #b00020; font-weight: bold; }\n")
cat(".filelink { font-family: 'Gill Sans MT','Trebuchet MS',sans-serif; font-size: 0.85em; }\n")
cat(".fig-card { display: none; }\n")
cat(".show { display: block; animation: fadeEffect 0.5s; }\n")
cat("@keyframes fadeEffect { from {opacity: 0;} to {opacity: 1;} }\n")
cat("</style>\n</head>\n<body>\n")

cat("<div class='sidebar'>\n")
cat("<h2>Navigation</h2>\n")
for (i in seq_len(nrow(glossary))) {
  cat(paste0("<a href='#", glossary$id[i], "'>", glossary$id[i], ": ", glossary$title[i], "</a>\n"))
}
cat("</div>\n")

cat("<div class='main'>\n")
cat("<h1>Thesis Glossary Dashboard</h1>\n")

cat("<div class='tabs'>\n")
cat("<button class='tab-btn active' onclick=\"openTab('overview', this)\">Overview</button>\n")
cat("<button class='tab-btn' onclick=\"openTab('scm', this)\">SCM Details</button>\n")
cat("<button class='tab-btn' onclick=\"openTab('methods', this)\">Methods Library</button>\n")
cat("</div>\n")

cat("<div id='tab-overview' class='tab-content active'>\n")

cat("<div class='filter-bar'>\n")
cat("<strong>Filters:</strong>")
cat("<button class='filter-btn active' onclick=\"filterSelection('all')\">Show All</button>\n")
for (tag in all_tags) {
  cat(paste0("<button class='filter-btn' onclick=\"filterSelection('", tag, "')\">", tag, "</button>\n"))
}
cat("</div>\n")

# Helper to detect file type
is_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  ext %in% c("png", "jpg", "jpeg", "gif", "bmp")
}

for (i in seq_len(nrow(glossary))) {
  row <- glossary[i, ]
  tags_vec <- parse_tags(row$tags)
  classes <- paste(c("fig-card", tags_vec), collapse = " ")

  cat(paste0("<div class='", classes, "' id='", row$id, "'>\n"))
  cat(paste0("<h3>", row$title, " <span class='fig-id'>", row$id, "</span></h3>\n"))

  if (file.exists(row$path)) {
    rel_path <- row$path
    if (grepl("^outputs/", row$path)) {
      rel_path <- sub("^outputs/", "", row$path)
    }
    if (is_image(row$path)) {
      cat(paste0("<img src='", rel_path, "' alt='", row$title, "'>\n"))
    } else {
      cat(paste0("<div class='filelink'>File: <a href='", rel_path, "'>", rel_path, "</a></div>\n"))
    }
  } else {
    cat(paste0("<p class='missing'>Missing file: ", row$path, "</p>\n"))
  }

  cat(paste0("<div class='explanation'>", row$explanation, "</div>\n"))

  cat("<div class='meta'>")
  if (!is.null(row$script_hint) && nchar(row$script_hint) > 0) {
    cat(paste0("<div><strong>Generated by:</strong> ", row$script_hint, "</div>"))
  }
  if (!is.null(row$data_source) && nchar(row$data_source) > 0) {
    cat(paste0("<div><strong>Data source:</strong> ", row$data_source, "</div>"))
  }
  cat(paste0("<div><strong>File:</strong> ", basename(row$path), "</div>"))
  cat(paste0("<div><strong>Path:</strong> ", root_path, "\\\\", row$path, "</div>"))
  cat("</div>\n")

  if (!is.null(row$notes) && nchar(row$notes) > 0) {
    cat(paste0("<div class='notes'>", row$notes, "</div>\n"))
  }

  if (!is.null(row$short_formula) && nchar(row$short_formula) > 0) {
    cat(paste0(
      "<div class='formula'><div class='formula-label'>Short Formula</div>$$ ", row$short_formula, " $$",
      "<button class='copy-btn' data-tex='", row$short_formula, "' onclick='copyFormula(this)'>Copy LaTeX</button>",
      "</div>\n"
    ))
  }

  if (!is.null(row$full_formula) && nchar(row$full_formula) > 0) {
    cat(paste0(
      "<details class='formula-full'><summary>Full formula</summary>",
      "<div class='formula'><div class='formula-label'>Full Formula</div>$$ ", row$full_formula, " $$",
      "<button class='copy-btn' data-tex='", row$full_formula, "' onclick='copyFormula(this)'>Copy LaTeX</button>",
      "</div></details>\n"
    ))
  }

  cat("<div class='tags'>")
  for (t in tags_vec) {
    cat(paste0("<span class='tag'>", t, "</span>"))
  }
  cat("</div>\n")
  cat("</div>\n")
}

cat("</div>\n") # end overview tab

cat("<div id='tab-scm' class='tab-content'>\n")
cat("<h2>Synthetic Control Method (SCM)</h2>\n")
cat("<p>This project uses a log-linear donor mix to estimate a counterfactual France series. The model is trained on the pre-treatment period (2010–2015), then back-transformed and mean-aligned to preserve levels.</p>\n")
cat("<div class='formula'><div class='formula-label'>Model</div>$$ \\log(Y_{F,t}+1) = \\alpha + \\sum_j w_j \\log(Y_{j,t}+1) + \\varepsilon_t $$<button class='copy-btn' data-tex='\\\\log(Y_{F,t}+1) = \\\\alpha + \\\\sum_j w_j \\\\log(Y_{j,t}+1) + \\\\varepsilon_t' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='formula'><div class='formula-label'>Prediction + Back-Transform</div>$$ \\hat{Y}_{F,t} = \\exp\\Big(\\hat{\\alpha}+\\sum_j \\hat{w}_j\\log(Y_{j,t}+1)\\Big)-1 $$<button class='copy-btn' data-tex='\\\\hat{Y}_{F,t} = \\\\exp(\\\\hat{\\\\alpha}+\\\\sum_j \\\\hat{w}_j\\\\log(Y_{j,t}+1))-1' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='formula'><div class='formula-label'>Mean Alignment (Pre-period)</div>$$ \\hat{Y}^{adj}_{F,t}=\\hat{Y}_{F,t}+(\\overline{Y}_{F,pre}-\\overline{\\hat{Y}}_{F,pre}) $$<button class='copy-btn' data-tex='\\\\hat{Y}^{adj}_{F,t}=\\\\hat{Y}_{F,t}+(\\\\overline{Y}_{F,pre}-\\\\overline{\\\\hat{Y}}_{F,pre})' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='formula'><div class='formula-label'>Monthly View</div>$$ \\hat{Y}^{monthly}_{F,t} = \\hat{Y}^{annual}_{F,t}/12 $$<button class='copy-btn' data-tex='\\\\hat{Y}^{monthly}_{F,t} = \\\\hat{Y}^{annual}_{F,t}/12' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='notes'><strong>Why this formula?</strong> Log-transforming counts stabilizes variance and avoids donor dominance. OLS weights provide the best linear fit in the pre-treatment window, and mean alignment corrects level shifts without changing the trend.</div>\n")
cat("<div class='notes'><strong>Interpretation:</strong> Positive gaps (Real − Synthetic) indicate excess violence relative to the counterfactual path constructed from donors.</div>\n")
cat("<div class='notes'><strong>Inputs:</strong> France annual counts (2010–2024), donor countries (Austria, UK, Spain), categories standardized across sources, pre-treatment window 2010–2015.</div>\n")
cat("<div class='notes'><strong>Outputs:</strong> Annual synthetic path (saved to `data/processed/scm_optimized_synthetic_france.csv`) and the hybrid plot (`outputs/scm_optimized_fit.png`).</div>\n")
cat("</div>\n")

cat("<div id='tab-methods' class='tab-content'>\n")
cat("<h2>Methods Library</h2>\n")
cat("<h3>Equivalence Testing (TOST)</h3>\n")
cat("<div class='formula'><div class='formula-label'>Hypotheses</div>$$ H_{01}: \\Delta \\le -\\delta,\\; H_{02}: \\Delta \\ge +\\delta \\quad\\quad H_A: -\\delta < \\Delta < +\\delta $$<button class='copy-btn' data-tex='H_{01}: \\\\Delta \\\\le -\\\\delta,\\\\; H_{02}: \\\\Delta \\\\ge +\\\\delta \\quad H_A: -\\\\delta < \\\\Delta < +\\\\delta' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='formula'><div class='formula-label'>Test Statistics</div>$$ t_1 = \\frac{\\hat{\\beta}-(-\\delta)}{SE(\\hat{\\beta})},\\; t_2 = \\frac{\\hat{\\beta}-\\delta}{SE(\\hat{\\beta})} $$<button class='copy-btn' data-tex='t_1 = \\\\frac{\\\\hat{\\\\beta}-(-\\\\delta)}{SE(\\\\hat{\\\\beta})},\\\\; t_2 = \\\\frac{\\\\hat{\\\\beta}-\\\\delta}{SE(\\\\hat{\\\\beta})}' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='formula'><div class='formula-label'>Decision Rule</div>$$ p_{TOST} = \\max(P(T\\ge t_1),\\; P(T\\le t_2)) < 0.05 $$<button class='copy-btn' data-tex='p_{TOST}=\\\\max(P(T\\\\ge t_1),P(T\\\\le t_2))<0.05' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='notes'>Exploratory TOST runs from `scripts/14_equivalence_testing.R` are excluded from the glossary cards because they were used to probe optimal bounds and stability sensitivity. They remain in `outputs/robustness/equivalence/` for reference.</div>\n")
cat("<h3>Structural Break (Chow Test)</h3>\n")
cat("<div class='formula'><div class='formula-label'>F-statistic</div>$$ F = \\frac{(RSS_R - RSS_U)/k}{RSS_U/(n-2k)} $$<button class='copy-btn' data-tex='F = \\\\frac{(RSS_R - RSS_U)/k}{RSS_U/(n-2k)}' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<h3>Indexing and Z-Score</h3>\n")
cat("<div class='formula'><div class='formula-label'>Index</div>$$ Index_t = \\frac{Y_t}{Y_{base}} \\times 100 $$<button class='copy-btn' data-tex='Index_t = \\\\frac{Y_t}{Y_{base}} \\\\times 100' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<div class='formula'><div class='formula-label'>Z-Score</div>$$ z_t = \\frac{Y_t-\\mu}{\\sigma} $$<button class='copy-btn' data-tex='z_t = \\\\frac{Y_t-\\\\mu}{\\\\sigma}' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("<h3>Kernel Density (Maps)</h3>\n")
cat("<div class='formula'><div class='formula-label'>KDE</div>$$ \\hat{f}_h(x,y)=\\frac{1}{n h^2}\\sum_{i=1}^{n} K\\Big(\\frac{x-x_i}{h},\\frac{y-y_i}{h}\\Big) $$<button class='copy-btn' data-tex='\\\\hat{f}_h(x,y)=\\\\frac{1}{n h^2}\\\\sum_{i=1}^{n} K(\\\\frac{x-x_i}{h},\\\\frac{y-y_i}{h})' onclick='copyFormula(this)'>Copy LaTeX</button></div>\n")
cat("</div>\n")

cat("</div>\n") # end main

cat("<script>\n")
cat("function openTab(name, btn) {\n")
cat("  var tabs = document.getElementsByClassName('tab-content');\n")
cat("  for (var i = 0; i < tabs.length; i++) { tabs[i].classList.remove('active'); }\n")
cat("  document.getElementById('tab-' + name).classList.add('active');\n")
cat("  var btns = document.getElementsByClassName('tab-btn');\n")
cat("  for (var j = 0; j < btns.length; j++) { btns[j].classList.remove('active'); }\n")
cat("  if (btn) { btn.classList.add('active'); }\n")
cat("}\n")
cat("function filterSelection(c) {\n")
cat("  var x, i;\n")
cat("  x = document.getElementsByClassName('fig-card');\n")
cat("  if (c == 'all') c = '';\n")
cat("  for (i = 0; i < x.length; i++) {\n")
cat("    w3RemoveClass(x[i], 'show');\n")
cat("    if (x[i].className.indexOf(c) > -1) w3AddClass(x[i], 'show');\n")
cat("  }\n")
cat("  var btns = document.getElementsByClassName('filter-btn');\n")
cat("  for (var i = 0; i < btns.length; i++) {\n")
cat("    btns[i].classList.remove('active');\n")
cat("    if (btns[i].innerText.indexOf(c) > -1 || (c=='' && btns[i].innerText=='Show All')) btns[i].classList.add('active');\n")
cat("  }\n")
cat("}\n")

cat("function w3AddClass(element, name) {\n")
cat("  var i, arr1, arr2;\n")
cat("  arr1 = element.className.split(' ');\n")
cat("  arr2 = name.split(' ');\n")
cat("  for (i = 0; i < arr2.length; i++) {\n")
cat("    if (arr1.indexOf(arr2[i]) == -1) {element.className += ' ' + arr2[i];}\n")
cat("  }\n")
cat("}\n")

cat("function w3RemoveClass(element, name) {\n")
cat("  var i, arr1, arr2;\n")
cat("  arr1 = element.className.split(' ');\n")
cat("  arr2 = name.split(' ');\n")
cat("  for (i = 0; i < arr2.length; i++) {\n")
cat("    while (arr1.indexOf(arr2[i]) > -1) {\n")
cat("      arr1.splice(arr1.indexOf(arr2[i]), 1);\n")
cat("    }\n")
cat("  }\n")
cat("  element.className = arr1.join(' ');\n")
cat("}\n")

cat("function copyFormula(btn) {\n")
cat("  var tex = btn.getAttribute('data-tex') || '';\n")
cat("  tex = tex.replace(/^\\$\\$\\s*/, '').replace(/\\s*\\$\\$$/, '');\n")
cat("  navigator.clipboard.writeText(tex).then(function() {\n")
cat("    var original = btn.innerText;\n")
cat("    btn.innerText = 'Copied!';\n")
cat("    setTimeout(function(){ btn.innerText = original; }, 1500);\n")
cat("  });\n")
cat("}\n")

cat("filterSelection('all');\n")
cat("</script>\n")

cat("</body>\n</html>")

sink()

print(paste("Glossary generated at:", html_file))
