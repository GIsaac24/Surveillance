mod_piliers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Piliers de préparation et réponse", "Matrice consolidée de suivi des piliers, statuts, retards et goulots d’étranglement."),
    fluidRow(
      column(7, div(class = "panel-card", plotOutput(ns("progress"), height = 420))),
      column(5, div(class = "panel-card", plotOutput(ns("late"), height = 420)))
    ),
    div(class = "panel-card", reactable::reactableOutput(ns("table")))
  )
}

mod_piliers_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$progress <- renderPlot({
      s <- pillar_progress_summary(req(dashboard_data())$pillar_activities)
      ggplot(s, aes(forcats::fct_reorder(pilier, taux_avancement), taux_avancement)) +
        geom_col(fill = "#B91C1C", width = .72) +
        geom_text(aes(label = scales::percent(taux_avancement, accuracy = 1)), hjust = -0.05, size = 3.2) +
        coord_flip() +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.08)) +
        labs(title = "Taux d’avancement par pilier", x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$late <- renderPlot({
      s <- pillar_progress_summary(req(dashboard_data())$pillar_activities)
      ggplot(s, aes(forcats::fct_reorder(pilier, en_retard), en_retard)) +
        geom_col(fill = "#DC2626", width = .72) +
        coord_flip() +
        labs(title = "Activités en retard par pilier", x = NULL, y = "Nombre") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$table <- reactable::renderReactable({
      s <- pillar_progress_summary(req(dashboard_data())$pillar_activities)
      make_reactable(
        s,
        page_size = 12,
        columns = list(
          taux_avancement = reactable::colDef(cell = function(value) progress_bar_html(value), html = TRUE),
          cout_total = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0))
        )
      )
    })
  })
}
