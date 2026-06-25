mod_poe_fluvial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title(
      "Surveillance aux points d’entrée fluviaux",
      "Flux voyageurs, dépistage thermique, signaux d’alerte et priorités opérationnelles sur les PoE fluviaux."
    ),
    div(
      class = "panel-card control-card",
      tags$p(
        class = "small-note",
        "Cette page est centrée sur les PoE fluviaux. La surveillance à l’Aéroport international Bangui M’Poko est conservée dans une page dédiée."
      )
    ),
    bslib::navset_card_tab(
      id = ns("poe_tabs"),
      title = "Analyse interactive des PoE fluviaux",
      bslib::nav_panel(
        "Synthèse",
        uiOutput(ns("kpis")),
        uiOutput(ns("executive"))
      ),
      bslib::nav_panel(
        "Activité et tendances",
        div(class = "hover-help", uiOutput(ns("activity_hover_info"))),
        fluidRow(
          column(
            6,
            div(
              class = "panel-card",
              tags$h4("Évolution quotidienne"),
              plotOutput(ns("daily_volume"), height = 340, hover = hoverOpts(ns("daily_hover"), delay = 120, delayType = "throttle"))
            )
          ),
          column(
            6,
            div(
              class = "panel-card",
              tags$h4("Signaux fébriles"),
              plotOutput(ns("fever_trend"), height = 340, hover = hoverOpts(ns("fever_hover"), delay = 120, delayType = "throttle"))
            )
          )
        ),
        div(
          class = "panel-card",
          tags$h4("Volume par PoE fluvial"),
          plotOutput(ns("site_volume"), height = 430, hover = hoverOpts(ns("site_hover"), delay = 120, delayType = "throttle"))
        )
      ),
      bslib::nav_panel(
        "Alertes",
        fluidRow(
          column(5, div(class = "panel-card", tags$h4("Cascade des alertes"), plotOutput(ns("alert_cascade"), height = 340))),
          column(7, div(class = "panel-card", tags$h4("PoE avec alertes"), reactable::reactableOutput(ns("alert_sites"))))
        )
      ),
      bslib::nav_panel(
        "Tableau détaillé",
        div(class = "panel-card", reactable::reactableOutput(ns("detail_table")))
      ),
      bslib::nav_panel(
        "Priorités",
        uiOutput(ns("actions"))
      )
    )
  )
}

mod_poe_fluvial_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    pack <- reactive({
      poe_analysis_pack(req(dashboard_data())$poe, "fluvial")
    })

    output$kpis <- renderUI({
      k <- pack()$kpi
      div(
        class = "kpi-grid",
        kpi_card("Voyageurs contrôlés", fmt_num(k$voyageurs), paste(fmt_num(k$rapports), "rapports reçus"), "#2B6CB0"),
        kpi_card("Signaux T° ≥ 38 °C", fmt_num(k$temp_sup38), paste("Taux :", fmt_pct(k$taux_fievre)), "#F97316"),
        kpi_card("Alertes vérifiées", fmt_num(k$alertes_verifiees), paste("Vérification :", fmt_pct(k$taux_verification)), "#16A34A"),
        kpi_card("Alertes validées", fmt_num(k$alertes_validees), paste("Validation :", fmt_pct(k$taux_validation)), "#B91C1C"),
        kpi_card("PoE fluviaux observés", fmt_num(k$n_poe), paste(fmt_num(k$n_districts), "districts"), "#7C3AED")
      )
    })

    output$executive <- renderUI({
      poe_executive_ui(pack(), include_operational_notes = FALSE)
    })

    output$activity_hover_info <- renderUI({
      daily <- pack()$daily
      site <- pack()$site
      date_focus <- input$daily_hover$x %||% input$fever_hover$x
      site_y <- input$site_hover$y

      if (!is.null(site_y) && nrow(site)) {
        ord <- site |> dplyr::arrange(voyageurs)
        idx <- round(site_y)
        if (!is.na(idx) && idx >= 1 && idx <= nrow(ord)) {
          s <- ord[idx, ]
          return(div(
            class = "note-box compact-box",
            strong(s$point_entree), " — ", fmt_num(s$voyageurs), " voyageurs ; ",
            fmt_num(s$temp_sup38), " signal(aux) ≥ 38 °C ; ",
            fmt_num(s$alertes_detectees), " alerte(s)."
          ))
        }
      }

      if (!is.null(date_focus) && nrow(daily)) {
        day <- as.Date(round(date_focus), origin = "1970-01-01")
        row <- daily |> dplyr::filter(date_collecte == day)
        if (nrow(row)) {
          return(div(
            class = "note-box compact-box",
            strong(poe_date_fr(day)), " — ", fmt_num(row$voyageurs), " voyageurs ; ",
            fmt_num(row$temp_sup38), " signal(aux) ≥ 38 °C ; ",
            fmt_num(row$alertes_detectees), " alerte(s)."
          ))
        }
      }

      div(class = "note-box compact-box", "Survolez les graphiques pour afficher les valeurs du jour ou du PoE.")
    })

    output$daily_volume <- renderPlot({
      plot_poe_daily_volume(pack())
    })

    output$fever_trend <- renderPlot({
      plot_poe_fever_trend(pack())
    })

    output$site_volume <- renderPlot({
      plot_poe_volume_site(pack())
    })

    output$alert_cascade <- renderPlot({
      plot_poe_alert_cascade(pack())
    })

    output$alert_sites <- reactable::renderReactable({
      alerts <- pack()$alert_sites |>
        dplyr::select(
          district_sanitaire,
          point_entree,
          voyageurs,
          temp_sup38,
          alertes_detectees,
          alertes_verifiees,
          alertes_validees,
          taux_fievre
        )

      if (!nrow(alerts)) {
        return(make_reactable(dplyr::tibble(message = "Aucun PoE fluvial avec alerte ou signal fébrile."), page_size = 5))
      }

      make_reactable(
        alerts,
        page_size = 10,
        columns = list(
          voyageurs = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          temp_sup38 = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          alertes_detectees = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          alertes_verifiees = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          alertes_validees = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          taux_fievre = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1))
        )
      )
    })

    output$actions <- renderUI({
      poe_priority_actions_ui(pack(), include_quality_action = FALSE)
    })

    output$detail_table <- reactable::renderReactable({
      detail <- pack()$site |>
        dplyr::select(
          district_sanitaire,
          point_entree,
          rapports,
          jours_notifies,
          voyageurs,
          temp_inf38,
          temp_sup38,
          alertes_detectees,
          alertes_verifiees,
          alertes_validees,
          taux_fievre,
          part_volume
        )

      make_reactable(
        detail,
        page_size = 12,
        columns = list(
          voyageurs = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          temp_inf38 = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          temp_sup38 = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          alertes_detectees = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          alertes_verifiees = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          alertes_validees = reactable::colDef(format = reactable::colFormat(separators = TRUE, digits = 0)),
          taux_fievre = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1)),
          part_volume = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1))
        )
      )
    })
  })
}
