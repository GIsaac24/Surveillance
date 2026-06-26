server <- function(input, output, session) {
  dashboard_data <- reactiveVal(NULL)

  observe({
    dashboard_data(load_dashboard_data(DATA_DIR))
  })

  mod_overview_server("overview", dashboard_data)
  mod_contexte_server("contexte", dashboard_data)
  mod_chronologie_server("chronologie", dashboard_data)
  mod_poe_fluvial_server("poe_fluvial", dashboard_data)
  mod_poe_aeroport_server("poe_aeroport", dashboard_data)
  mod_formations_server("formations", dashboard_data)
  mod_piliers_server("piliers", dashboard_data)
  mod_laboratoires_server("laboratoires", dashboard_data)
  mod_cartes_server("cartes", dashboard_data)
}
