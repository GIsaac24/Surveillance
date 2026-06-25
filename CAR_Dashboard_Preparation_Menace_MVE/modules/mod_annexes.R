mod_annexes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    section_title("Données, hypothèses et annexes", "Traçabilité des fichiers importés, limites actuelles et qualité des données."),
    fluidRow(
      column(6, div(class = "panel-card", h4("Fichiers détectés"), reactable::reactableOutput(ns("files_table")))),
      column(6, div(class = "panel-card", h4("Alertes qualité / hypothèses"), uiOutput(ns("assumptions"))))
    ),
    div(class = "panel-card", h4("Tables sources principales"), reactable::reactableOutput(ns("source_table")))
  )
}

mod_annexes_server <- function(id, dashboard_data) {
  moduleServer(id, function(input, output, session) {
    output$files_table <- reactable::renderReactable({
      dat <- req(dashboard_data())
      f <- dat$files
      df <- tibble::tibble(
        objet = names(f)[!names(f) %in% c("pillar_workbooks")],
        chemin = vapply(f[!names(f) %in% c("pillar_workbooks")], function(x) paste(x, collapse = "; "), character(1))
      )
      make_reactable(df, page_size = 12)
    })

    output$assumptions <- renderUI({
      dat <- req(dashboard_data())
      alerts <- quality_alerts(dat)
      tagList(
        lapply(alerts$message, function(x) div(class = "assumption", x)),
        div(class = "assumption", "Les valeurs RDC/Ouganda de référence proviennent de l’OMS, image publiée le 23/06/2026 avec données au 21/06/2026 ; elles doivent être remplacées par les fichiers nationaux si disponibles."),
        div(class = "assumption", "Les données PoE de température ≥ 38°C sont traitées comme signaux fébriles/alertes à vérifier, et non comme cas suspects MVE sans investigation clinique.")
      )
    })

    output$source_table <- reactable::renderReactable({
      dat <- req(dashboard_data())
      df <- dplyr::bind_rows(
        tibble::tibble(table = "surveillance_poe", lignes = nrow(dat$poe), source = paste(unique(dat$poe$source_donnees), collapse = "; "), type = paste(unique(dat$poe$source_type), collapse = "; ")),
        tibble::tibble(table = "activites_par_piliers", lignes = nrow(dat$pillar_activities), source = paste(unique(dat$pillar_activities$source_donnees), collapse = "; "), type = paste(unique(dat$pillar_activities$source_type), collapse = "; ")),
        tibble::tibble(table = "formations", lignes = nrow(dat$formations), source = paste(unique(dat$formations$source_donnees), collapse = "; "), type = paste(unique(dat$formations$source_type), collapse = "; ")),
        tibble::tibble(table = "laboratoires", lignes = nrow(dat$sites_laboratoires), source = paste(unique(dat$sites_laboratoires$source_donnees), collapse = "; "), type = paste(unique(dat$sites_laboratoires$source_type), collapse = "; ")),
        tibble::tibble(table = "rdc_daily", lignes = nrow(dat$rdc_daily), source = paste(unique(dat$rdc_daily$source_donnees), collapse = "; "), type = paste(unique(dat$rdc_daily$source_type), collapse = "; "))
      )
      make_reactable(df, page_size = 10)
    })
  })
}
