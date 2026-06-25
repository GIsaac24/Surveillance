mod_cartes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title(
      "Cartes stratégiques interactives",
      "Districts prioritaires, sous-préfectures et sites d’installation des laboratoires."
    ),
    div(class = "panel-card map-toolbar", p(class = "small-note", textOutput(ns("map_note")))),
    bslib::layout_columns(
      col_widths = c(7, 5),
      div(
        class = "panel-card map-card",
        h4("Districts prioritaires et sous-préfectures"),
        leaflet::leafletOutput(ns("priority_map"), height = 560)
      ),
      div(
        class = "panel-card side-insight",
        h4("District / sous-préfecture survolé(e)"),
        uiOutput(ns("priority_info")),
        tags$hr(),
        h4("Districts prioritaires"),
        reactable::reactableOutput(ns("district_table"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      div(
        class = "panel-card map-card",
        h4("Sites d’installation des laboratoires"),
        leaflet::leafletOutput(ns("lab_map"), height = 560)
      ),
      div(
        class = "panel-card side-insight",
        h4("Site laboratoire survolé"),
        uiOutput(ns("lab_info")),
        tags$hr(),
        h4("Sites prévus"),
        reactable::reactableOutput(ns("lab_table"))
      )
    )
  )
}

mod_cartes_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    shapes <- reactive(load_priority_district_shapes(SHAPEFILE_DIR))
    subpref <- reactive(load_subpref_shapes(SHAPEFILE_DIR, shapes()))

    output$map_note <- renderText(map_note())

    output$priority_map <- leaflet::renderLeaflet({
      leaflet_priority_map(shapes(), subpref())
    })

    priority_feature_id <- reactive({
      (input$priority_map_shape_mouseover$id %||% input$priority_map_shape_click$id) %||% NA_character_
    })

    output$priority_info <- renderUI({
      id <- priority_feature_id()
      shp <- shapes()
      subs <- subpref()
      if (is.null(id) || is.na(id) || !nzchar(id)) {
        return(div(class = "note-box compact-box", "Survolez ou cliquez un district ou une sous-préfecture pour zoomer et afficher le nom du district et de la sous-préfecture."))
      }

      if (startsWith(id, "subpref:") && !is.null(subs) && nrow(subs)) {
        idx <- suppressWarnings(as.integer(sub("^subpref:", "", id)))
        if (!is.na(idx) && idx >= 1 && idx <= nrow(subs)) {
          sp <- sf::st_drop_geometry(subs[idx, ])
          return(div(
            class = "insight-box compact-box",
            h3(sp$sous_prefecture[1]),
            p(strong("Sous-préfecture : "), sp$sous_prefecture[1]),
            p(strong("District sanitaire : "), sp$district_sanitaire[1] %||% "ND")
          ))
        }
      }

      focus_norm <- sub("^district:", "", id)
      district <- shp |> dplyr::filter(district_norm == .env$focus_norm)
      if (!nrow(district)) {
        return(div(class = "note-box compact-box", "Information cartographique non disponible pour cette entité."))
      }
      district_subs <- subprefs_in_district(subs, district)
      subs_names <- if (!is.null(district_subs) && nrow(district_subs)) sort(unique(district_subs$sous_prefecture)) else character()
      div(
        class = "insight-box compact-box",
        h3(district$district_sanitaire[1]),
        p(strong("Statut : "), ifelse(isTRUE(district$prioritaire[1]), "District prioritaire", "District non prioritaire")),
        p(strong("Sous-préfectures : "), ifelse(length(subs_names), paste(length(subs_names), "sous-préfecture(s)"), "Non disponible")),
        if (length(subs_names)) tags$ul(lapply(head(subs_names, 12), tags$li)),
        if (length(subs_names) > 12) p(class = "small-note", paste("+", length(subs_names) - 12, "autre(s) sous-préfecture(s)."))
      )
    })

    lab_sites <- reactive({
      dat <- req(dashboard_data())
      enrich_lab_sites_with_subpref(dat$sites_laboratoires, subpref())
    })

    output$lab_map <- leaflet::renderLeaflet({
      dat <- req(dashboard_data())
      leaflet_lab_sites_map(shapes(), dat$sites_laboratoires, subpref())
    })

    output$lab_info <- renderUI({
      id <- (input$lab_map_marker_mouseover$id %||% input$lab_map_marker_click$id) %||% NA_character_
      sites <- lab_sites()
      if (is.null(id) || is.na(id) || !nzchar(id)) {
        return(div(class = "note-box compact-box", "Survolez ou cliquez un point laboratoire pour afficher la localité, le district et la sous-préfecture."))
      }
      site_id <- suppressWarnings(as.integer(sub("^lab:", "", id)))
      site <- sites |> dplyr::filter(site_id == site_id)
      if (!nrow(site)) return(div(class = "note-box compact-box", "Site laboratoire non retrouvé."))
      div(
        class = "insight-box compact-box",
        h3(site$localite[1]),
        p(strong("District sanitaire : "), site$district_sanitaire[1] %||% "ND"),
        p(strong("Sous-préfecture : "), site$sous_prefecture[1] %||% "ND"),
        p(strong("Type : "), site$type_laboratoire[1] %||% "ND"),
        p(strong("Statut installation : "), site$statut_installation[1] %||% "ND"),
        p(class = "small-note", paste0("Coordonnées : ", round(site$latitude[1], 3), ", ", round(site$longitude[1], 3)))
      )
    })

    output$district_table <- reactable::renderReactable({
      make_reactable(req(dashboard_data())$districts_prioritaires, page_size = 8)
    })

    output$lab_table <- reactable::renderReactable({
      table <- lab_sites() |>
        dplyr::select(localite, district_sanitaire, sous_prefecture, type_laboratoire, statut_installation, latitude, longitude)
      make_reactable(table, page_size = 8)
    })
  })
}
