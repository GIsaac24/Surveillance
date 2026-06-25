mod_activites_piliers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Activités par pilier", "Exploitation détaillée de la liste d’activités : filtres, avancement, statuts, priorités et goulots."),
    fluidRow(
      column(3, selectInput(ns("pilier"), "Pilier", choices = "Tous")),
      column(3, selectInput(ns("district"), "District", choices = "Tous")),
      column(2, selectInput(ns("statut"), "Statut", choices = "Tous")),
      column(2, selectInput(ns("priorite"), "Priorité", choices = "Tous")),
      column(2, selectInput(ns("partenaire"), "Partenaire", choices = "Tous"))
    ),
    uiOutput(ns("kpis")),
    fluidRow(
      column(6, div(class = "panel-card", plotOutput(ns("by_pillar"), height = 340))),
      column(6, div(class = "panel-card", plotOutput(ns("progress"), height = 340)))
    ),
    fluidRow(
      column(6, div(class = "panel-card", reactable::reactableOutput(ns("late_table")))),
      column(6, div(class = "panel-card", reactable::reactableOutput(ns("priority_table"))))
    ),
    div(class = "panel-card", reactable::reactableOutput(ns("table")))
  )
}

mod_activites_piliers_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(dashboard_data(), {
      a <- dashboard_data()$pillar_activities
      updateSelectInput(session, "pilier", choices = c("Tous", sort(unique(a$pilier))))
      updateSelectInput(session, "district", choices = c("Tous", sort(unique(a$district))))
      updateSelectInput(session, "statut", choices = c("Tous", sort(unique(a$statut))))
      updateSelectInput(session, "priorite", choices = c("Tous", sort(unique(a$niveau_priorite))))
      updateSelectInput(session, "partenaire", choices = c("Tous", sort(unique(na.omit(a$partenaire)))))
    }, ignoreInit = FALSE)

    filtered <- reactive({
      a <- req(dashboard_data())$pillar_activities
      if (!is.null(input$pilier) && input$pilier != "Tous") a <- filter(a, pilier == input$pilier)
      if (!is.null(input$district) && input$district != "Tous") a <- filter(a, district == input$district)
      if (!is.null(input$statut) && input$statut != "Tous") a <- filter(a, statut == input$statut)
      if (!is.null(input$priorite) && input$priorite != "Tous") a <- filter(a, niveau_priorite == input$priorite)
      if (!is.null(input$partenaire) && input$partenaire != "Tous") a <- filter(a, partenaire == input$partenaire)
      a
    })

    output$kpis <- renderUI({
      a <- filtered()
      ind <- activities_indicators(a)
      div(
        class = "kpi-grid",
        kpi_card("Activités filtrées", fmt_num(ind$total_activites)),
        kpi_card("Avancement moyen", fmt_pct(ind$taux_avancement_global)),
        kpi_card("En retard", fmt_num(ind$activites_retard)),
        kpi_card("Priorité haute", fmt_num(ind$activites_priorite_haute))
      )
    })

    output$by_pillar <- renderPlot({
      s <- filtered() |> count(pilier, name = "n") |> arrange(n)
      ggplot(s, aes(forcats::fct_reorder(pilier, n), n)) +
        geom_col(fill = "#B91C1C", width = .72) +
        coord_flip() +
        labs(title = "Nombre d’activités par pilier", x = NULL, y = "Nombre") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$progress <- renderPlot({
      s <- pillar_progress_summary(filtered())
      ggplot(s, aes(forcats::fct_reorder(pilier, taux_avancement), taux_avancement)) +
        geom_col(fill = "#F97316", width = .72) +
        coord_flip() +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(title = "Avancement par pilier — sélection", x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", color = "#7F1D1D"))
    })

    output$late_table <- reactable::renderReactable({
      make_reactable(filtered() |> filter(statut == "en retard") |> select(pilier, activite, district, responsable, goulot_etranglement, action_correctrice), page_size = 6)
    })

    output$priority_table <- reactable::renderReactable({
      make_reactable(filtered() |> filter(niveau_priorite == "haute") |> select(pilier, activite, district, statut, responsable, partenaire), page_size = 6)
    })

    output$table <- reactable::renderReactable({
      make_reactable(
        filtered(),
        page_size = 12,
        columns = list(
          taux_avancement = reactable::colDef(cell = function(value) progress_bar_html(value), html = TRUE),
          statut = reactable::colDef(cell = function(value) status_badge(value), html = TRUE)
        )
      )
    })
  })
}
