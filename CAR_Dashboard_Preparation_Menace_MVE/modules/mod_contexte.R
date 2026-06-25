mod_contexte_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Contexte de la menace MVE", "Contexte régional, menace d’importation en RCA et justification stratégique."),
    div(class = "panel-card", uiOutput(ns("context_text"))),
    div(class = "panel-card", uiOutput(ns("context_source")))
  )
}

mod_contexte_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$context_text <- renderUI({
      dat <- req(dashboard_data())
      div(class = "context-box", dat$contexte$text)
    })
    output$context_source <- renderUI({
      dat <- req(dashboard_data())
      tagList(
        h4("Source du contexte"),
        p(dat$contexte$source),
        if (isTRUE(dat$contexte$is_example)) div(class = "assumption", "Texte provisoire : ajouter un fichier Contexte dans le dossier data pour remplacer automatiquement ce contenu.")
      )
    })
  })
}
