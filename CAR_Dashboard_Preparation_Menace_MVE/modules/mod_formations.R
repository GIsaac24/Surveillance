mod_formations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Matrice des formations et chronogramme", "Formations planifiées, réalisées, en retard, par district, pilier et période."),
    uiOutput(ns("kpis")),
    fluidRow(
      column(7, div(class = "panel-card", plotOutput(ns("gantt"), height = 360))),
      column(5, div(class = "panel-card", plotOutput(ns("status"), height = 360)))
    ),
    div(class = "panel-card", reactable::reactableOutput(ns("table")))
  )
}

mod_formations_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$kpis <- renderUI({
      dat <- req(dashboard_data())
      ind <- dat$indicators$formations
      div(
        class = "kpi-grid",
        kpi_card("Formations prévues", fmt_num(ind$total_formations)),
        kpi_card("Formations réalisées", fmt_num(ind$formations_realisees)),
        kpi_card("Formations planifiées/en cours", fmt_num(ind$formations_planifiees)),
        kpi_card("Taux participants formés", fmt_pct(ind$taux_participation))
      )
    })

    output$gantt <- renderPlot({
      f <- req(dashboard_data())$formations |>
        mutate(
          start = dplyr::coalesce(date_debut_reelle, date_debut_prevue),
          end = dplyr::coalesce(date_fin_reelle, date_fin_prevue, start),
          label = stringr::str_trunc(intitule_formation, 42)
        ) |>
        arrange(start)
      ggplot(f, aes(y = forcats::fct_reorder(label, start), x = start, xend = end, yend = label, color = statut)) +
        geom_segment(linewidth = 6, lineend = "round") +
        geom_point(size = 2) +
        scale_color_manual(values = c("non démarrée" = "#9CA3AF", "planifiée" = "#F97316", "en cours" = "#2563EB", "réalisée" = "#16A34A", "en retard" = "#DC2626", "reportée" = "#6B7280", "annulée" = "#111827")) +
        labs(title = "Chronogramme des formations", x = NULL, y = NULL, color = "Statut") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"), legend.position = "bottom")
    })

    output$status <- renderPlot({
      s <- status_summary(req(dashboard_data())$formations)
      names(s)[1] <- "statut"
      ggplot(s, aes(forcats::fct_reorder(statut, n), n, fill = statut)) +
        geom_col(show.legend = FALSE, width = .72) +
        coord_flip() +
        scale_fill_manual(values = c("non démarrée" = "#9CA3AF", "planifiée" = "#F97316", "en cours" = "#2563EB", "réalisée" = "#16A34A", "en retard" = "#DC2626", "reportée" = "#6B7280", "annulée" = "#111827")) +
        labs(title = "Formations par statut", x = NULL, y = "Nombre") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$table <- reactable::renderReactable({
      make_reactable(req(dashboard_data())$formations, page_size = 10)
    })
  })
}
