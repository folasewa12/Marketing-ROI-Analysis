# =============================================================================
# 00_setup.R  -  packages, paths and shared chart settings
# Project: Marketing ROI & Effectiveness Analysis
# Author:  Afolasewa Duduyemi
# =============================================================================
# Run from the project root (open the folder in RStudio, or setwd() to it).

required_packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "broom", "skimr")
missing <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)
invisible(lapply(required_packages, library, character.only = TRUE))

data_file  <- file.path("data", "ev_20.xlsx")
figure_dir <- file.path("outputs", "figures")
table_dir  <- file.path("outputs", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir,  recursive = TRUE, showWarnings = FALSE)

# Fixed colours: one per campaign, so a channel keeps its colour on every chart
campaign_colours <- c(fbook = "#2a78d6", instagram = "#eb6834",
                      tiktok = "#1baf7a", twitter = "#6250d6")

theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.title       = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom")
}

save_plot <- function(plot, name, width = 8, height = 5) {
  ggsave(file.path(figure_dir, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
}
