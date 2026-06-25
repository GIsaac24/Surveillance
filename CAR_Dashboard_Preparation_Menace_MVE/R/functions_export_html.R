export_dashboard_html <- function(output_path = file.path(OUTPUT_DIR, paste0(DASHBOARD_NAME, ".html"))) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Le package rmarkdown est requis pour produire l’export HTML.")
  }
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  rmd <- file.path(APP_DIR, "dashboard_static.Rmd")
  if (!file.exists(rmd)) stop("Fichier RMarkdown introuvable : ", rmd)
  rmarkdown::render(
    input = rmd,
    output_file = basename(output_path),
    output_dir = dirname(output_path),
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )
  invisible(output_path)
}
