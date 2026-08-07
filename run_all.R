# Reproduce the complete econometric analysis from the prepared source panel.
# Run from the repository root: Rscript run_all.R

source(file.path("R", "00_setup.R"))
source(file.path("R", "01_prepare_data.R"))
source(file.path("R", "02_descriptive_analysis.R"))
source(file.path("R", "03_main_models.R"))
source(file.path("R", "04_diagnostics_and_cook.R"))
source(file.path("R", "05_robustness.R"))

message("Analysis completed. See results/tables and results/figures.")
