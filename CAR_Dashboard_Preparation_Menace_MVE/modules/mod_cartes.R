mod_cartes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title(
      "Cartes stratégiques interactives",
      "Districts prioritaires, sous-préfectures et sites d’installation des laboratoires."
    ),
    div(
      class = "panel-card map-toolbar",
      p(class = "small-note", textOutput(ns("map_note"))),
      checkboxInput(ns("auto_zoom"), "Zoom automatique au survol d’un district prioritaire", value = TRUE)
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      div(
        class = "panel-card map-card",
        h4("Districts prioritaires et sous-préfectures"),
        plotOutput(
          ns("priority_map"),
          height = 560,
          hover = hoverOpts(ns("priority_hover"), delay = 120, delayType = "throttle", clip = TRUE),
          click = clickOpts(ns("priority_click"), clip = TRUE),
          dblclick = dblclickOpts(ns("priority_dblclick"), clip = TRUE)
        )
      ),
      div(
        class = "panel-card side-insight",
        h4("District survolé / sélectionné"),
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
        plotOutput(
          ns("lab_map"),
          height = 560,
          hover = hoverOpts(ns("lab_hover"), delay = 100, delayType = "throttle", clip = TRUE),
          click = clickOpts(ns("lab_click"), clip = TRUE)
        )
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
    selected_district <- reactiveVal(NA_character_)
    selected_lab <- reactiveVal(NA_integer_)

    output$map_note <- renderText(map_note())

    observeEvent(input$priority_click, {
      hit <- sf_feature_at_xy(shapes(), input$priority_click$x, input$priority_click$y)
      if (!is.na(hit)) selected_district(hit)
    })

    observeEvent(input$priority_dblclick, {
      selected_district(NA_character_)
    })

    hovered_district <- reactive({
      sf_feature_at_xy(shapes(), input$priority_hover$x, input$priority_hover$y)
    })

    focus_district <- reactive({
      hovered_district() %||% selected_district()
    })

    output$priority_map <- renderPlot({
      plot_priority_districts(
        shapes(),
        subpref = subpref(),
        focus_norm = focus_district(),
        zoom_focus = isTRUE(input$auto_zoom)
      )
    })

    output$priority_info <- renderUI({
      shp <- shapes()
      focus <- focus_district()
      if (is.null(shp) || is.na(focus)) {
        return(div(class = "note-box compact-box", "Survolez un district pour zoomer et afficher les sous-préfectures. Cliquez pour conserver la sélection ; double-cliquez pour réinitialiser."))
      }
      district <- shp |> dplyr::filter(district_norm == focus)
      subs <- subprefs_in_district(subpref(), district)
      subs_names <- if (!is.null(subs) && nrow(subs)) sort(unique(subs$sous_prefecture)) else character()

      div(
        class = "insight-box compact-box",
        h3(district$district_sanitaire[1]),
        p(strong("Statut : "), ifelse(isTRUE(district$prioritaire[1]), "District prioritaire", "District non prioritaire")),
        p(strong("Sous-préfectures intersectées : "), ifelse(length(subs_names), paste(length(subs_names), "sous-préfecture(s)"), "Non disponible")),
        if (length(subs_names)) tags$ul(lapply(head(subs_names, 12), tags$li)),
        if (length(subs_names) > 12) p(class = "small-note", paste("+", length(subs_names) - 12, "autre(s) sous-préfecture(s)."))
      )
    })

    lab_sites <- reactive({
      dat <- req(dashboard_data())
      enrich_lab_sites_with_subpref(dat$sites_laboratoires, subpref())
    })

    hovered_lab_id <- reactive({
      nearest_lab_site(lab_sites(), input$lab_hover$x, input$lab_hover$y)
    })

    observeEvent(input$lab_click, {
      hit <- nearest_lab_site(lab_sites(), input$lab_click$x, input$lab_click$y)
      if (!is.na(hit)) selected_lab(hit)
    })

    focus_lab <- reactive({
      hovered_lab_id() %||% selected_lab()
    })

    output$lab_map <- renderPlot({
      dat <- req(dashboard_data())
      plot_lab_sites(
        shapes(),
        dat$sites_laboratoires,
        subpref = subpref(),
        focus_site_id = focus_lab(),
        zoom_focus = FALSE
      )
    })

    output$lab_info <- renderUI({
      sites <- lab_sites()
      focus <- focus_lab()
      if (!nrow(sites) || is.na(focus)) {
        return(div(class = "note-box compact-box", "Survolez un point laboratoire pour afficher la localité, le district et la sous-préfecture. Cliquez pour conserver un site en focus."))
      }
      site <- sites |> dplyr::filter(site_id == focus)
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
