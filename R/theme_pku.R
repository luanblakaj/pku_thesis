library(ggplot2)
library(grid)

# 0. Font Management
# We use 'showtext' to ensure Times New Roman works on PNG/PDF across Windows/Mac/Linux.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(showtext, sysfonts)

# Add Times New Roman (searching standard system paths)
# "time.ttf" (lower case) or "Times New Roman.ttf" depends on OS, but "times.ttf" is standard Windows.
# We try to add it. If it fails, showtext usually warns but proceeds.
tryCatch(
    {
        # Windows standard path usually has times.ttf
        font_add(family = "Times New Roman", regular = "times.ttf", bold = "timesbd.ttf", italic = "timesi.ttf", bolditalic = "timesbi.ttf")
    },
    error = function(e) {
        # Fallback: try just the family name if sysfonts can find it registered
        tryCatch(
            {
                font_add(family = "Times New Roman", regular = "Times New Roman")
            },
            error = function(e2) {
                message("Could not load specific Times New Roman file. Using default serif.")
            }
        )
    }
)

showtext_auto(enable = TRUE)
# Fix for some RStudio viewers / resolutions (optional but good for consistency)
# Use higher DPI so text renders crisper and larger in PNG outputs.
showtext_opts(dpi = 150)

# 1. Color Palette
pku_pal <- list(
    accent   = "#8A0000", # PKU red
    text     = "#111111",
    text_sec = "#444444",
    grid     = "#E6E6E6",
    panel    = "#F7F7F7",
    plot     = "#FFFFFF",
    # Monochrome line set
    mono     = c("#1A1A1A", "#4D4D4D", "#808080", "#A6A6A6", "#CCCCCC"),
    # Extras (only if needed)
    navy     = "#1F3A5F",
    slate    = "#4A6FA5",
    purple   = "#6A1B9A",
    amber    = "#B07D12"
)

# 3. Global Caption
std_caption <- "Luan Blakaj, 2026, Created for Master’s Thesis at Peking University, SIS"

# 2. Category Display Labels (Translations)
category_display <- function(x) {
    dplyr::recode(
        x,
        "Violence_Officials" = "Assaults on State Authority",
        "Incendies" = "Incendiary Attacks",
        "Arson" = "Incendiary Attacks",
        "Terrorism_AIFN" = "Attacks on the Fundamental Interests of the Nation",
        "Terrorism" = "Attacks on the Fundamental Interests of the Nation",
        "Terrorism_Explosives" = "Detonation of Explosive Devices",
        .default = x
    )
}

# 4. Theme Function
theme_pku <- function(base_size = 14) {
    # Target Font Family (Now strictly "Times New Roman" thanks to showtext)
    target_font <- "Times New Roman"

    # Update global defaults so strict annotated text uses it too
    update_geom_defaults("text", list(family = target_font, color = pku_pal$text_sec, size = 4.2))
    update_geom_defaults("label", list(family = target_font, color = pku_pal$text_sec, size = 4.2))

    theme_minimal(base_size = base_size, base_family = target_font) +
        theme(
            # Backgrounds
            plot.background  = element_rect(fill = pku_pal$plot, color = NA),
            panel.background = element_rect(fill = pku_pal$panel, color = NA),

            # Grids
            panel.grid.major = element_line(color = pku_pal$grid),
            panel.grid.minor = element_blank(),

            # Text (All inheriting strictly)
            text             = element_text(color = pku_pal$text, family = target_font),
            axis.text        = element_text(color = pku_pal$text_sec, size = rel(1.0), family = target_font),
            axis.title       = element_text(color = pku_pal$text_sec, family = target_font),
            legend.text      = element_text(color = pku_pal$text_sec, size = rel(1.0), family = target_font),
            legend.title     = element_text(color = pku_pal$text_sec, face = "bold", size = rel(1.0), family = target_font),

            # Titles
            plot.title       = element_text(color = pku_pal$accent, face = "bold", size = rel(1.35), family = target_font),
            plot.subtitle    = element_text(color = pku_pal$text_sec, size = rel(1.1), family = target_font),
            plot.caption     = element_text(color = pku_pal$text_sec, size = rel(0.95), hjust = 0, family = target_font),

            # Margins (give room for watermark on the right)
            plot.margin      = margin(t = 10, r = 40, b = 10, l = 10)
        )
}

# 5. Watermark Function
add_watermark <- function(p) {
    # Uses plot.tag to position text consistently outside the plot area
    # regardless of coordinate system
    p +
        labs(tag = "Luan Blakaj, 2026, Created for Master’s Thesis at Peking University, SIS") +
        theme(
            plot.tag.position = c(1.02, 0.5), # Right edge, centered vertically
            plot.tag = element_text(
                angle = -90,
                color = "#444444",
                size = 11, # larger watermark text for readability
                family = "Times New Roman",
                hjust = 0.5,
                vjust = 0.5
            ),
            # Ensure no clipping of the tag
            plot.margin = margin(t = 10, r = 35, b = 10, l = 10)
        )
}
