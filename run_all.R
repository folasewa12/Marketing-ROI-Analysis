# Run the full analysis. Tables and charts are written to outputs/.
source("R/00_setup.R")
source("R/01_clean_data.R")
source("R/02_kpis_and_segments.R")
source("R/03_return_on_spend.R")
source("R/04_charts.R")
message("Done - see the outputs/ folder.")
