mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Vue d’ensemble", "Synthèse stratégique de la préparation, de la surveillance PoE et de la situation régionale."),
    uiOutput(ns("kpis")),
    fluidRow(
      column(6, div(class = "panel-card", plotOutput(ns("pillar_progress"), height = 330))),
      column(6, div(class = "panel-card", plotOutput(ns("poe_trend"), height = 330)))
    ),
    fluidRow(
      column(6, div(class = "panel-card", plotOutput(ns("activity_status"), height = 290))),
      column(6, div(class = "panel-card", uiOutput(ns("attention_points"))))
    )
  )
}

mod_overview_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$kpis <- renderUI({
      dat <- req(dashboard_data())
      ind <- dat$indicators
      div(
        class = "kpi-grid",
        kpi_card("Voyageurs dépistés", fmt_num(ind$poe$voyageurs), paste(ind$poe$n_poe, "PoE suivis")),
        kpi_card("Alertes PoE", fmt_num(ind$poe$alertes), paste("Validées :", fmt_num(ind$poe$alertes_validees))),
        kpi_card("Districts prioritaires", fmt_num(ind$strategic$districts_prioritaires), "Préparation ciblée"),
        kpi_card("Activités préparatoires", fmt_num(ind$activities$total_activites), paste("Avancement moyen :", fmt_pct(ind$activities$taux_avancement_global))),
        kpi_card("Formations", fmt_num(ind$formations$total_formations), paste("Réalisées :", fmt_num(ind$formations$formations_realisees))),
        kpi_card("Laboratoires prévus", fmt_num(ind$lab$laboratoires_prevus), paste("Gap matériel :", fmt_num(ind$lab$gap_materiel))),
        kpi_card("Cas confirmés RDC/Ouganda", fmt_num(ind$epidemio$cas_confirmes_total), paste("CFR :", fmt_pct(ind$epidemio$cfr))),
        kpi_card("Score indicatif préparation", fmt_pct(ind$strategic$score_preparation), "Calculé sur activités, formations et labos")
      )
    })

    output$pillar_progress <- renderPlot({
      dat <- req(dashboard_data())
      s <- pillar_progress_summary(dat$pillar_activities)
      ggplot(s, aes(x = forcats::fct_reorder(pilier, taux_avancement), y = taux_avancement)) +
        geom_col(fill = "#B91C1C", width = .72) +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(title = "Avancement moyen par pilier", x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$poe_trend <- renderPlot({
      dat <- req(dashboard_data())
      d <- daily_poe_summary(dat$poe)
      ggplot(d, aes(date_collecte, voyageurs, color = type_poe)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.2) +
        scale_color_manual(values = c(aeroport = "#B91C1C", fluvial = "#F97316")) +
        scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
        labs(title = "Tendance des voyageurs dépistés", x = NULL, y = "Voyageurs", color = "Type PoE") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$activity_status <- renderPlot({
      dat <- req(dashboard_data())
      s <- status_summary(dat$pillar_activities)
      names(s)[1] <- "statut"
      ggplot(s, aes(x = forcats::fct_reorder(statut, n), y = n, fill = statut)) +
        geom_col(width = .72, show.legend = FALSE) +
        coord_flip() +
        scale_fill_manual(values = c("non démarrée" = "#9CA3AF", "planifiée" = "#F97316", "en cours" = "#2563EB", "réalisée" = "#16A34A", "en retard" = "#DC2626", "reportée" = "#6B7280", "annulée" = "#111827")) +
        labs(title = "Statut des activités", x = NULL, y = "Nombre") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$attention_points <- renderUI({
      dat <- req(dashboard_data())
      alerts <- quality_alerts(dat)
      tagList(
        h4("Points d’attention opérationnels"),
        lapply(alerts$message, function(x) div(class = "assumption", x)),
        p(class = "small-note", paste("Données chargées le", format(dat$loaded_at, "%d/%m/%Y à %H:%M")))
      )
    })
  })
}
