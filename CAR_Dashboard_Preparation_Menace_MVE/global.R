options(shiny.sanitize.errors = FALSE, encoding = "UTF-8")

required_packages <- c(
  "shiny", "bslib", "htmltools", "readxl", "dplyr", "tidyr", "stringr",
  "lubridate", "ggplot2", "scales", "forcats", "reactable", "sf",
  "plotly", "leaflet"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Packages R manquants : ", paste(missing_packages, collapse = ", "),
       ". Installer ces packages ou adapter les modules concernés.")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(reactable)
  library(sf)
  library(plotly)
  library(leaflet)
})

source(file.path("R", "config.R"), encoding = "UTF-8")
source(file.path("R", "functions_cleaning.R"), encoding = "UTF-8")
source(file.path("R", "functions_indicators.R"), encoding = "UTF-8")
source(file.path("R", "functions_maps.R"), encoding = "UTF-8")
source(file.path("R", "functions_maps_leaflet.R"), encoding = "UTF-8")
source(file.path("R", "functions_import.R"), encoding = "UTF-8")
source(file.path("R", "functions_tables.R"), encoding = "UTF-8")
source(file.path("R", "functions_export_html.R"), encoding = "UTF-8")
source(file.path("R", "functions_poe_analysis.R"), encoding = "UTF-8")

module_files <- list.files("modules", pattern = "[.]R$", full.names = TRUE)
for (module_file in module_files) {
  source(module_file, encoding = "UTF-8")
}

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
