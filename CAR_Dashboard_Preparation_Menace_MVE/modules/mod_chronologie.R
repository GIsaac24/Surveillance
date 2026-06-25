mod_chronologie_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Chronologie des activités préparatoires", "Timeline des actions de préparation, réunions, formations et jalons opérationnels."),
    fluidRow(
      column(5, div(class = "panel-card", uiOutput(ns("timeline")))),
      column(7, div(class = "panel-card", reactable::reactableOutput(ns("table"))))
    )
  )
}

mod_chronologie_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$timeline <- renderUI({
      dat <- req(dashboard_data())
      chr <- dat$chronologie |> arrange(date_activite)
      tagList(lapply(seq_len(nrow(chr)), function(i) {
        div(
          class = "timeline-item",
          div(class = "timeline-date", format(chr$date_activite[i], "%d/%m/%Y")),
          tags$b(chr$titre_activite[i]),
          div(chr$description[i]),
          status_badge(chr$statut[i])
        )
      }))
    })
    output$table <- reactable::renderReactable({
      dat <- req(dashboard_data())
      make_reactable(dat$chronologie, page_size = 10)
    })
  })
}
