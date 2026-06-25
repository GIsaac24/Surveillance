mod_poe_aeroport_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Surveillance à l’Aéroport international Bangui M’Poko", "Flux voyageurs, dépistage thermique et alertes au point d’entrée aérien."),
    uiOutput(ns("kpis")),
    fluidRow(
      column(7, div(class = "panel-card", plotOutput(ns("trend"), height = 330))),
      column(5, div(class = "panel-card", plotOutput(ns("alerts"), height = 330)))
    ),
    div(class = "panel-card", reactable::reactableOutput(ns("table")))
  )
}

mod_poe_aeroport_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    aero <- reactive({
      dat <- req(dashboard_data())
      if (!is.null(dat$poe_aeroport)) dat$poe_aeroport else dat$poe |> filter(type_poe == "aeroport")
    })

    output$kpis <- renderUI({
      d <- aero()
      ind <- poe_indicators(d)
      div(
        class = "kpi-grid",
        kpi_card("Voyageurs aéroport", fmt_num(ind$voyageurs)),
        kpi_card("Température ≥ 38°C", fmt_num(ind$temp_sup38), fmt_pct(ind$taux_temp_sup38)),
        kpi_card("Alertes détectées", fmt_num(ind$alertes)),
        kpi_card("Alertes validées", fmt_num(ind$alertes_validees))
      )
    })

    output$trend <- renderPlot({
      d <- daily_poe_summary(aero())
      if (!nrow(d)) return(empty_plot("Aucune donnée aéroport disponible dans le fichier actuel."))
      ggplot(d, aes(date_collecte, voyageurs)) +
        geom_line(color = "#B91C1C", linewidth = 1.2) +
        geom_point(color = "#B91C1C", fill = "#FDE68A", shape = 21, size = 3) +
        labs(title = "Tendance journalière des voyageurs à l’aéroport", x = NULL, y = "Voyageurs") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$alerts <- renderPlot({
      base <- daily_poe_summary(aero())
      if (!nrow(base)) return(empty_plot("Aucune alerte aéroport disponible dans le fichier actuel."))
      d <- base |>
        select(date_collecte, alertes, alertes_verifiees, alertes_validees) |>
        tidyr::pivot_longer(-date_collecte, names_to = "indicateur", values_to = "valeur")
      ggplot(d, aes(date_collecte, valeur, color = indicateur)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +
        scale_color_manual(values = c(alertes = "#F97316", alertes_verifiees = "#2563EB", alertes_validees = "#16A34A")) +
        labs(title = "Cascade des alertes — aéroport", x = NULL, y = "Nombre", color = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"), legend.position = "bottom")
    })

    output$table <- reactable::renderReactable({
      make_reactable(aero(), page_size = 10)
    })
  })
}
