mod_laboratoires_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Laboratoires, équipements et sites d’installation", "Suivi des besoins matériels, des gaps et des localités prévues pour l’appui laboratoire MVE."),
    uiOutput(ns("kpis")),
    fluidRow(
      column(6, div(class = "panel-card", plotOutput(ns("materials_gap"), height = 330))),
      column(6, div(class = "panel-card", plotOutput(ns("lab_map"), height = 330)))
    ),
    fluidRow(
      column(6, div(class = "panel-card", h4("Matériels de laboratoire"), reactable::reactableOutput(ns("materials_table")))),
      column(6, div(class = "panel-card", h4("Sites d’installation"), reactable::reactableOutput(ns("sites_table"))))
    )
  )
}

mod_laboratoires_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$kpis <- renderUI({
      dat <- req(dashboard_data())
      ind <- dat$indicators$lab
      div(
        class = "kpi-grid",
        kpi_card("Laboratoires prévus", fmt_num(ind$laboratoires_prevus)),
        kpi_card("Laboratoires installés", fmt_num(ind$laboratoires_installes)),
        kpi_card("Gap matériel total", fmt_num(ind$gap_materiel)),
        kpi_card("Matériels priorité haute", fmt_num(ind$materiels_priorite_haute))
      )
    })

    output$materials_gap <- renderPlot({
      m <- req(dashboard_data())$materiels_laboratoire |>
        mutate(nom_materiel = stringr::str_trunc(nom_materiel, 34)) |>
        slice_max(gap, n = 12)
      ggplot(m, aes(forcats::fct_reorder(nom_materiel, gap), gap, fill = priorite)) +
        geom_col(width = .72) +
        coord_flip() +
        scale_fill_manual(values = c(haute = "#DC2626", moyenne = "#F97316", faible = "#16A34A")) +
        labs(title = "Gaps matériels prioritaires", x = NULL, y = "Gap", fill = "Priorité") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"), legend.position = "bottom")
    })

    output$lab_map <- renderPlot({
      dat <- req(dashboard_data())
      shp <- load_priority_district_shapes(SHAPEFILE_DIR)
      plot_lab_sites(shp, dat$sites_laboratoires)
    })

    output$materials_table <- reactable::renderReactable({
      make_reactable(req(dashboard_data())$materiels_laboratoire, page_size = 8)
    })

    output$sites_table <- reactable::renderReactable({
      make_reactable(req(dashboard_data())$sites_laboratoires, page_size = 8)
    })
  })
}
