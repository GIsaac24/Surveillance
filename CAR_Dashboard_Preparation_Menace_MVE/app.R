app_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
if (!is.na(app_file) && nzchar(app_file)) {
  setwd(dirname(app_file))
}
source("global.R", encoding = "UTF-8")
source("ui.R", encoding = "UTF-8")
source("server.R", encoding = "UTF-8")

shinyApp(ui = ui, server = server)
