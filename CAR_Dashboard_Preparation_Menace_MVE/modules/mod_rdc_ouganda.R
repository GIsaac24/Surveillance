mod_rdc_ouganda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Situation épidémiologique RDC et Ouganda", "Indicateurs de référence et histogramme épidémique issus des fichiers disponibles et de la référence OMS."),
    uiOutput(ns("kpis")),
    fluidRow(
      column(7, div(class = "panel-card", plotOutput(ns("epi_curve"), height = 360))),
      column(5, div(class = "panel-card", plotOutput(ns("country_bars"), height = 360)))
    ),
    fluidRow(
      column(6, div(class = "panel-card", h4("Synthèse pays"), reactable::reactableOutput(ns("summary_table")))),
      column(6, div(class = "panel-card", h4("Zones de santé RDC"), reactable::reactableOutput(ns("zones_table"))))
    )
  )
}

mod_rdc_ouganda_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$kpis <- renderUI({
      dat <- req(dashboard_data())
      ind <- dat$indicators$epidemio
      div(
        class = "kpi-grid",
        kpi_card("Cas confirmés cumulés", fmt_num(ind$cas_confirmes_total), "RDC + Ouganda"),
        kpi_card("CFR confirmé", fmt_pct(ind$cfr)),
        kpi_card("Cas confirmés dernières 24h", fmt_num(ind$cas_confirmes_24h), "RDC — dernière date du fichier"),
        kpi_card("Décès confirmés", fmt_num(ind$deces_confirmes_total)),
        kpi_card("Guérisons", fmt_num(ind$guerisons_total)),
        kpi_card("Cas confirmés Ouganda", fmt_num(ind$cas_confirmes_ouganda))
      )
    })

    output$epi_curve <- renderPlot({
      d <- req(dashboard_data())$rdc_daily
      ggplot(d, aes(date, cas_confirmes)) +
        geom_col(fill = "#B91C1C", width = .85) +
        labs(title = "Histogramme épidémique RDC — cas confirmés journaliers", x = NULL, y = "Cas confirmés") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$country_bars <- renderPlot({
      s <- req(dashboard_data())$rdc_ouganda_summary |>
        select(pays, cas_confirmes, deces_confirmes, guerisons) |>
        tidyr::pivot_longer(-pays, names_to = "indicateur", values_to = "valeur")
      ggplot(s, aes(pays, valeur, fill = indicateur)) +
        geom_col(position = "dodge", width = .72) +
        scale_fill_manual(values = c(cas_confirmes = "#B91C1C", deces_confirmes = "#111827", guerisons = "#16A34A")) +
        scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
        labs(title = "Indicateurs par pays", x = NULL, y = "Nombre", fill = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"), legend.position = "bottom")
    })

    output$summary_table <- reactable::renderReactable({
      make_reactable(req(dashboard_data())$rdc_ouganda_summary, page_size = 5)
    })

    output$zones_table <- reactable::renderReactable({
      zones <- req(dashboard_data())$rdc_zones
      if (!nrow(zones)) zones <- tibble::tibble(message = "Aucune table de zones de santé disponible.")
      make_reactable(zones, page_size = 8)
    })
  })
}
