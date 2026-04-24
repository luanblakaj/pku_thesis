# ==============================================================================
# Title: Differential Effects Dashboard
# Purpose: Generate HTML dashboard for the differential-effects analysis.
# Author: Luan Blakaj 齐凯
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate)

out_dir <- "outputs/differential_effects"
dashboard_path <- file.path(out_dir, "differential_effects_Dashboard.html")

if (!dir.exists(out_dir)) {
    stop("outputs/differential_effects does not exist. Run scripts/17_differential_effects_analysis.R first.")
}

safe_read_csv <- function(path) {
    if (!file.exists(path)) return(NULL)
    tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
}

escape_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
}

render_table <- function(df, max_rows = 20) {
    if (is.null(df) || nrow(df) == 0) {
        return("<div class='muted'>No table data available.</div>")
    }
    df <- head(df, max_rows)
    cols <- paste0("<th>", escape_html(names(df)), "</th>", collapse = "")
    rows <- apply(df, 1, function(r) {
        vals <- paste0("<td>", escape_html(as.character(r)), "</td>", collapse = "")
        paste0("<tr>", vals, "</tr>")
    })
    paste0(
        "<div class='table-wrap'><table class='data-table'>",
        "<tr>", cols, "</tr>",
        paste(rows, collapse = ""),
        "</table></div>"
    )
}

img_card <- function(title, rel_path, caption = NULL) {
    full_path <- file.path(out_dir, rel_path)
    if (!file.exists(full_path)) {
        return(paste0(
            "<div class='card'>",
            "<div class='card-title'>", escape_html(title), "</div>",
            "<div class='muted'>Missing: ", escape_html(rel_path), "</div>",
            "</div>"
        ))
    }
    paste0(
        "<div class='card'>",
        "<div class='card-title'>", escape_html(title), "</div>",
        "<img src='", rel_path, "' alt='", escape_html(title), "' />",
        if (!is.null(caption)) paste0("<div class='card-caption'>", escape_html(caption), "</div>") else "",
        "</div>"
    )
}

section_header <- function(id, title, subtitle = NULL) {
    paste0(
        "<section id='", id, "'>",
        "<div class='section-header'>",
        "<h2>", escape_html(title), "</h2>",
        if (!is.null(subtitle)) paste0("<p>", escape_html(subtitle), "</p>") else "",
        "</div>"
    )
}

section_footer <- function() {
    "</section>"
}

specs <- list(
    list(name = "left_right", label = "Left vs Right (Robust)"),
    list(name = "religious_right", label = "Religious vs Right (Sparse)"),
    list(name = "islam_right", label = "Islam vs Right (Very Sparse)")
)

# Deckigs-Statistike usem Roordate
df_raw <- read_excel("data/full_dataset_w_predictions_vX.xlsx", guess_max = 10000)
if (inherits(df_raw$event_date, "Date") || inherits(df_raw$event_date, "POSIXt")) {
    df_raw <- df_raw %>% mutate(event_date = as.Date(event_date))
} else if (is.numeric(df_raw$event_date)) {
    df_raw <- df_raw %>% mutate(event_date = as.Date(as.numeric(event_date), origin = "1899-12-30"))
} else {
    df_raw <- df_raw %>% mutate(event_date = as.Date(event_date))
}

law_date <- as.Date("2021-07-30")
post_start <- ceiling_date(law_date, "month")

df_fr <- df_raw %>% filter(country == "France")
min_date <- min(df_fr$event_date, na.rm = TRUE)
max_date <- max(df_fr$event_date, na.rm = TRUE)
pre_events <- sum(df_fr$event_date < post_start, na.rm = TRUE)
post_events <- sum(df_fr$event_date >= post_start, na.rm = TRUE)
pre_months <- length(unique(floor_date(df_fr$event_date[df_fr$event_date < post_start], "month")))
post_months <- length(unique(floor_date(df_fr$event_date[df_fr$event_date >= post_start], "month")))

counts_by_year_tags <- safe_read_csv(file.path(out_dir, "summaries", "counts_by_year_tags_france.csv"))
counts_by_year_religion <- safe_read_csv(file.path(out_dir, "summaries", "counts_by_year_religion_france.csv"))

# HTML baue
html_parts <- c()
html_parts <- c(html_parts, "<!DOCTYPE html><html lang='en'><head>")
html_parts <- c(html_parts, "<meta charset='UTF-8'>")
html_parts <- c(html_parts, "<meta name='viewport' content='width=device-width, initial-scale=1.0'>")
html_parts <- c(html_parts, "<title>Differential Effects Dashboard</title>")
html_parts <- c(html_parts, "<link rel='preconnect' href='https://fonts.googleapis.com'>")
html_parts <- c(html_parts, "<link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>")
html_parts <- c(html_parts, "<link href='https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&family=Playfair+Display:wght@600;700&family=JetBrains+Mono:wght@400&display=swap' rel='stylesheet'>")
html_parts <- c(html_parts, "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>")

html_parts <- c(html_parts, "<style>
:root {
  --bg-body: #F9FAFB;
  --bg-card: #FFFFFF;
  --bg-sidebar: #FFFFFF;
  --text-main: #111827;
  --text-muted: #6B7280;
  --accent: #8A0000;
  --accent-light: #FEE2E2;
  --border: #E5E7EB;
  --font-sans: 'Inter', sans-serif;
  --font-serif: 'Playfair Display', serif;
  --font-mono: 'JetBrains Mono', monospace;
  --shadow-sm: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
  --shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
  --radius-md: 0.5rem;
  --radius-lg: 0.75rem;
}
body {
  margin: 0;
  font-family: var(--font-sans);
  background: var(--bg-body);
  color: var(--text-main);
  display: flex;
  height: 100vh;
  overflow: hidden;
}
.app-container {
  display: flex;
  width: 100%;
  height: 100%;
}
.sidebar {
  width: 320px;
  background: var(--bg-sidebar);
  border-right: 1px solid var(--border);
  display: flex;
  flex-direction: column;
  flex-shrink: 0;
}
.branding {
  padding: 1.5rem;
  border-bottom: 1px solid var(--border);
}
.branding h1 {
  margin: 0;
  font-family: var(--font-serif);
  font-size: 1.25rem;
  color: var(--accent);
}
.branding p {
  margin: 0.25rem 0 0;
  color: var(--text-muted);
  font-size: 0.875rem;
}
.nav-scroller {
  flex: 1;
  overflow-y: auto;
  padding: 1rem;
}
.nav-item {
  display: block;
  padding: 0.5rem 0.75rem;
  color: var(--text-main);
  text-decoration: none;
  border-radius: var(--radius-md);
  font-size: 0.9rem;
  transition: background 0.15s;
  margin-bottom: 2px;
}
.nav-item:hover { background: #F3F4F6; }
.nav-item.active { background: var(--accent-light); color: var(--accent); font-weight: 500; }
.content {
  flex: 1;
  overflow-y: auto;
  padding: 2rem;
}
.section-header h2 {
  margin: 0 0 0.4rem;
  font-family: var(--font-serif);
  color: var(--accent);
}
.section-header p { margin: 0 0 1.25rem; color: var(--text-muted); }
.grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 1rem;
}
.card {
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 1rem;
  box-shadow: var(--shadow-sm);
}
.card img {
  width: 100%;
  border-radius: var(--radius-md);
  border: 1px solid var(--border);
  margin-top: 0.5rem;
}
.card-title {
  font-weight: 600;
  margin-bottom: 0.25rem;
}
.card-caption {
  margin-top: 0.5rem;
  font-size: 0.85rem;
  color: var(--text-muted);
}
.muted { color: var(--text-muted); font-size: 0.9rem; }
.table-wrap { overflow-x: auto; }
.data-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.9rem;
}
.data-table th, .data-table td {
  border: 1px solid var(--border);
  padding: 0.4rem 0.6rem;
  text-align: left;
}
.data-table th { background: var(--accent-light); }
.formula {
  background: #fff;
  border: 1px dashed var(--border);
  border-radius: var(--radius-md);
  padding: 0.75rem;
  font-family: var(--font-mono);
  font-size: 0.85rem;
}
.badge {
  display: inline-block;
  background: var(--accent-light);
  color: var(--accent);
  padding: 0.15rem 0.5rem;
  border-radius: 999px;
  font-size: 0.75rem;
  margin-right: 0.35rem;
}
</style>")

html_parts <- c(html_parts, "</head><body>")
html_parts <- c(html_parts, "<div class='app-container'>")

# Sytebalken
html_parts <- c(html_parts, "<aside class='sidebar'><div class='branding'>")
html_parts <- c(html_parts, "<h1>Differential Effects Dashboard</h1>")
html_parts <- c(html_parts, "<p>Differential effects of legal instruments on extremist actors</p>")
html_parts <- c(html_parts, "</div><div class='nav-scroller'>")

nav_items <- c(
    "overview" = "Overview",
    "data" = "Data Coverage",
    "methods" = "Methods & Formulas",
    "left_right" = "Left vs Right",
    "religious_right" = "Religious vs Right",
    "islam_right" = "Islam vs Right",
    "country" = "France vs Germany",
    "files" = "Output Index"
)

for (id in names(nav_items)) {
    html_parts <- c(html_parts, paste0("<a class='nav-item' href='#", id, "'>", nav_items[[id]], "</a>"))
}
html_parts <- c(html_parts, "</div></aside>")

# Inhaut agfange
html_parts <- c(html_parts, "<main class='content'>")

# Iberblick
html_parts <- c(html_parts, section_header("overview", "Overview", "Research question, hypothesis, and timing constraints."))
html_parts <- c(html_parts, "<div class='grid'>")
html_parts <- c(html_parts, paste0(
    "<div class='card'>",
    "<div class='card-title'>Research Question</div>",
    "<div>Do these legal instruments exert differential effects on Islamist versus Far-Right actors, given their differing organizational structures?</div>",
    "<div class='card-caption'>Primary hypothesis: Islamist actors are targeted more strongly. Robustness: Left vs Right.</div>",
    "</div>"
))
html_parts <- c(html_parts, paste0(
    "<div class='card'>",
    "<div class='card-title'>Timing</div>",
    "<div><span class='badge'>Law</span> 2021-07-30</div>",
    "<div><span class='badge'>Post Start</span> ", as.character(post_start), " (first full month)</div>",
    "<div class='card-caption'>Short pre-trend window (data start in 2020).</div>",
    "</div>"
))
html_parts <- c(html_parts, paste0(
    "<div class='card'>",
    "<div class='card-title'>France Coverage</div>",
    "<div>Range: ", as.character(min_date), " to ", as.character(max_date), "</div>",
    "<div>Pre events: ", pre_events, " | Post events: ", post_events, "</div>",
    "<div>Pre months: ", pre_months, " | Post months: ", post_months, "</div>",
    "</div>"
))
html_parts <- c(html_parts, "</div>", section_footer())

# Dateabdeckig
html_parts <- c(html_parts, section_header("data", "Data Coverage", "Breakdowns of labels used in the analysis."))
html_parts <- c(html_parts, "<div class='grid'>")
html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>France: Counts by Year and Tag</div>", render_table(counts_by_year_tags, 50), "</div>"))
html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>France: Counts by Year and Religion</div>", render_table(counts_by_year_religion, 50), "</div>"))
html_parts <- c(html_parts, "</div>", section_footer())

# Methode
html_parts <- c(html_parts, section_header("methods", "Methods & Formulas", "Design choices to handle short pre-trends and sparse labels."))
html_parts <- c(html_parts, "<div class='grid'>")
html_parts <- c(html_parts, paste0(
    "<div class='card'><div class='card-title'>Difference-in-Differences</div>",
    "<div class='formula'>$$y_{t,g}=\\alpha+\\beta\\,Post_t+\\gamma\\,Group_g+\\delta(Post_t\\times Group_g)+\\varepsilon_{t,g}$$</div>",
    "<div class='card-caption'>Interaction term (delta) captures differential post shift.</div></div>"
))
html_parts <- c(html_parts, paste0(
    "<div class='card'><div class='card-title'>Count Models</div>",
    "<div class='formula'>$$E[y_{t,g}] = \\exp(\\alpha+\\beta\\,Post_t+\\gamma\\,Group_g+\\delta(Post_t\\times Group_g))$$</div>",
    "<div class='card-caption'>Poisson and Negative Binomial for sparse counts.</div></div>"
))
html_parts <- c(html_parts, paste0(
    "<div class='card'><div class='card-title'>Pre-trend Check</div>",
    "<div class='formula'>$$y_{t,g}=\\alpha+\\theta\\,Time_t+\\phi\\,Group_g+\\psi(Time_t\\times Group_g)+\\varepsilon_{t,g}$$</div>",
    "<div class='card-caption'>Runs only on pre-period months.</div></div>"
))
html_parts <- c(html_parts, paste0(
    "<div class='card'><div class='card-title'>Chow Break Test</div>",
    "<div class='formula'>$$F=\\frac{(RSS_R-RSS_U)/k}{RSS_U/(n-2k)}$$</div>",
    "<div class='card-caption'>Tests for a mean shift at the post-start month.</div></div>"
))
html_parts <- c(html_parts, "</div>", section_footer())

# Spezifikationsabschnitt
for (spec in specs) {
    spec_dir <- file.path(out_dir, spec$name)
    pre_post_summary <- safe_read_csv(file.path(spec_dir, "pre_post_summary.csv"))
    tests <- safe_read_csv(file.path(spec_dir, "pre_post_tests.csv"))
    did <- safe_read_csv(file.path(spec_dir, "models_interaction_only.csv"))
    pretrend <- safe_read_csv(file.path(spec_dir, "pretrend_test.csv"))
    chow <- safe_read_csv(file.path(spec_dir, "chow_tests.csv"))

    html_parts <- c(html_parts, section_header(spec$name, spec$label, "Core outputs and statistical tests."))
    html_parts <- c(html_parts, "<div class='grid'>")
    html_parts <- c(html_parts, img_card("Monthly Counts", file.path(spec$name, "ts_monthly_counts.png")))
    html_parts <- c(html_parts, img_card("Rolling Mean (6m)", file.path(spec$name, "ts_rolling6.png")))
    html_parts <- c(html_parts, img_card("Indexed to Pre Mean", file.path(spec$name, "ts_indexed.png")))
    html_parts <- c(html_parts, img_card("Cumulative Counts", file.path(spec$name, "ts_cumulative.png")))
    html_parts <- c(html_parts, img_card("Share of Monthly Events", file.path(spec$name, "ts_share.png")))
    html_parts <- c(html_parts, img_card("Pre/Post Distribution", file.path(spec$name, "pre_post_boxplot.png")))
    html_parts <- c(html_parts, img_card("Binned Event Study (3-month)", file.path(spec$name, "event_study_binned.png")))
    html_parts <- c(html_parts, img_card("Window Sensitivity Heatmap", file.path(spec$name, "window_sensitivity_heatmap.png")))
    html_parts <- c(html_parts, img_card("Placebo Tests", file.path(spec$name, "placebo_tests.png")))
    html_parts <- c(html_parts, "</div>")

    html_parts <- c(html_parts, "<div class='grid'>")
    html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>Pre/Post Summary</div>", render_table(pre_post_summary), "</div>"))
    html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>Pre/Post Tests</div>", render_table(tests), "</div>"))
    html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>Interaction (DiD) Terms</div>", render_table(did), "</div>"))
    html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>Pre-trend Test</div>", render_table(pretrend), "</div>"))
    html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>Chow Break Test</div>", render_table(chow), "</div>"))
    html_parts <- c(html_parts, "</div>")

    html_parts <- c(html_parts, section_footer())
}

# Landvergliichsabschnitt
html_parts <- c(html_parts, section_header("country", "France vs Germany (Placebo)", "Triple-difference placebo for Left vs Right."))
html_parts <- c(html_parts, "<div class='grid'>")
html_parts <- c(html_parts, img_card("France vs Germany Time Series", "country_comparison_left_right/france_germany_timeseries.png"))
triple_key <- safe_read_csv(file.path(out_dir, "country_comparison_left_right", "triple_diff_key_term.csv"))
html_parts <- c(html_parts, paste0("<div class='card'><div class='card-title'>Triple Diff Key Term</div>", render_table(triple_key), "</div>"))
html_parts <- c(html_parts, "</div>", section_footer())

# Usgabeverzeichnis
html_parts <- c(html_parts, section_header("files", "Output Index", "All files created under outputs/differential_effects."))
file_index <- tibble(
    file = list.files(out_dir, recursive = TRUE, full.names = FALSE)
)
html_parts <- c(html_parts, "<div class='card'>", render_table(file_index, 200), "</div>")
html_parts <- c(html_parts, section_footer())

html_parts <- c(html_parts, "</main></div>")
html_parts <- c(html_parts, "</body></html>")

writeLines(html_parts, con = dashboard_path)

print(paste("Dashboard saved to", dashboard_path))
