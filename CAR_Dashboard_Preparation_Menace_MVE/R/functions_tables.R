kpi_card <- function(label, value, subtitle = NULL, accent = "#B91C1C") {
  htmltools::div(
    class = "kpi-card",
    style = paste0("border-top-color:", accent, ";"),
    htmltools::div(class = "kpi-label", label),
    htmltools::div(class = "kpi-value", value),
    if (!is.null(subtitle)) htmltools::div(class = "kpi-subtitle", subtitle)
  )
}

section_title <- function(title, subtitle = NULL) {
  htmltools::tagList(
    htmltools::h2(class = "section-title", title),
    if (!is.null(subtitle)) htmltools::p(class = "section-subtitle", subtitle)
  )
}

status_badge <- function(x) {
  cls <- dplyr::case_when(
    x == "réalisée" ~ "badge-green",
    x == "en cours" ~ "badge-blue",
    x == "en retard" ~ "badge-red",
    x == "planifiée" ~ "badge-orange",
    x == "non démarrée" ~ "badge-gray",
    TRUE ~ "badge-gray"
  )
  htmltools::span(class = paste("status-badge", cls), x)
}

progress_bar_html <- function(x) {
  pct <- ifelse(is.na(x), 0, pmax(pmin(x, 1), 0))
  htmltools::div(
    class = "progress-shell",
    htmltools::div(class = "progress-fill", style = paste0("width:", round(100 * pct), "%;")),
    htmltools::span(class = "progress-label", fmt_pct(pct, 0))
  )
}

pretty_column_label <- function(x) {
  key <- clean_names_fr(x)
  labels <- c(
    id_activite = "ID activité",
    date_activite = "Date activité",
    titre_activite = "Titre activité",
    description = "Description",
    pilier = "Pilier",
    district = "District",
    localite = "Localité",
    responsable = "Responsable",
    partenaire = "Partenaire",
    statut = "Statut",
    resultat_cle = "Résultat clé",
    commentaire = "Commentaire",
    district_sanitaire = "District sanitaire",
    region_sanitaire = "Région sanitaire",
    point_entree = "Point d’entrée",
    type_poe = "Type PoE",
    date_collecte = "Date collecte",
    rapports = "Rapports",
    jours_notifies = "Jours notifiés",
    voyageurs = "Voyageurs",
    nombre_voyageur = "Nombre de voyageurs",
    temp_inf38 = "T° < 38 °C",
    temp_sup38 = "T° ≥ 38 °C",
    alertes_detectees = "Alertes détectées",
    alertes_verifiees = "Alertes vérifiées",
    alertes_validees = "Alertes validées",
    taux_fievre = "Taux de fièvre",
    part_volume = "Part du volume",
    id_formation = "ID formation",
    intitule_formation = "Intitulé formation",
    cible = "Cible",
    date_debut_prevue = "Début prévu",
    date_fin_prevue = "Fin prévue",
    date_debut_reelle = "Début réel",
    date_fin_reelle = "Fin réelle",
    participants_attendus = "Participants attendus",
    participants_formes = "Participants formés",
    observations = "Observations",
    taux_avancement = "Taux d’avancement",
    cout_total = "Coût total",
    cout_investissement = "Coût investissement",
    cout_operationnel = "Coût opérationnel",
    niveau_priorite = "Niveau de priorité",
    goulot_etranglement = "Goulot d’étranglement",
    action_correctrice = "Action correctrice",
    id_materiel = "ID matériel",
    nom_materiel = "Matériel",
    categorie = "Catégorie",
    quantite_disponible = "Quantité disponible",
    quantite_requise = "Quantité requise",
    gap = "Gap",
    priorite = "Priorité",
    localisation_prevue = "Localisation prévue",
    statut_acquisition = "Statut acquisition",
    image_materiel = "Image matériel",
    id_site = "ID site",
    type_laboratoire = "Type laboratoire",
    niveau_laboratoire = "Niveau laboratoire",
    statut_installation = "Statut installation",
    date_prevue_installation = "Date prévue installation",
    partenaire_appui = "Partenaire d’appui",
    sous_prefecture = "Sous-préfecture",
    latitude = "Latitude",
    longitude = "Longitude",
    pays = "Pays",
    date_situation = "Date situation",
    cas_suspects = "Cas suspects",
    cas_confirmes = "Cas confirmés",
    deces = "Décès",
    deces_confirmes = "Décès confirmés",
    deces_probables = "Décès probables",
    guerisons = "Guérisons",
    cfr_confirme = "CFR confirmé",
    source_donnees = "Source des données",
    source_type = "Type de source",
    url_source = "URL source",
    province_region = "Province / région",
    zone_sante = "Zone de santé",
    semaine_epidemiologique = "Semaine épidémiologique",
    contacts_listes = "Contacts listés",
    contacts_suivis = "Contacts suivis",
    message = "Message"
  )
  if (key %in% names(labels)) return(unname(labels[[key]]))
  label <- gsub("_+", " ", as.character(x))
  label <- trimws(label)
  if (!nzchar(label)) return(as.character(x))
  label <- paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  label <- gsub("\\bpoe\\b", "PoE", label, ignore.case = TRUE)
  label <- gsub("\\bid\\b", "ID", label, ignore.case = TRUE)
  label
}

reactable_columns_with_labels <- function(data, columns = NULL) {
  nms <- names(data)
  out <- stats::setNames(vector("list", length(nms)), nms)
  for (nm in nms) {
    if (!is.null(columns) && nm %in% names(columns)) {
      def <- columns[[nm]]
      if (is.null(def$name)) def$name <- pretty_column_label(nm)
      out[[nm]] <- def
    } else {
      out[[nm]] <- reactable::colDef(name = pretty_column_label(nm))
    }
  }
  if (!is.null(columns)) {
    extra <- setdiff(names(columns), nms)
    for (nm in extra) out[[nm]] <- columns[[nm]]
  }
  out
}

make_reactable <- function(data, searchable = TRUE, page_size = 8, columns = NULL) {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    return(htmltools::tags$pre(utils::capture.output(print(utils::head(data, page_size)))))
  }
  columns <- reactable_columns_with_labels(data, columns)
  reactable::reactable(
    data,
    searchable = searchable,
    filterable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    defaultPageSize = page_size,
    columns = columns,
    theme = reactable::reactableTheme(
      borderColor = "#E5E7EB",
      stripedColor = "#F9FAFB",
      highlightColor = "#FEF2F2",
      headerStyle = list(background = "#7F1D1D", color = "white", fontWeight = "600")
    )
  )
}

empty_plot <- function(message = "Aucune donnée disponible") {
  ggplot2::ggplot() +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::annotate("text", x = 0, y = 0, label = message, color = "#6B7280", fontface = "bold")
}

contact_footer_ui <- function() {
  contacts <- list(
    list(
      nom = "Dr Jean Méthode MOYEN",
      fonction = "Coordonnateur du COUSP",
      tel = "+23672248722",
      mail = "jmethodemoyen@gmail.com"
    ),
    list(
      nom = "Dr Daniel WEA YOUNGAÏ",
      fonction = "Incident Manager",
      tel = "+23672569182",
      mail = "youngaiwea@gmail.com"
    ),
    list(
      nom = "M. Isaac Simplice KENGUELA",
      fonction = "Suivi-Évaluation",
      tel = "+23672601806",
      mail = "sikendba2016@gmail.com"
    )
  )

  htmltools::tags$footer(
    class = "dashboard-footer",
    htmltools::div(class = "footer-title", "Pour toute information"),
    htmltools::div(class = "footer-subtitle", "Contactez"),
    htmltools::div(
      class = "footer-contact-grid",
      lapply(contacts, function(x) {
        htmltools::div(
          class = "footer-contact-card",
          htmltools::div(class = "footer-contact-name", x$nom),
          htmltools::div(class = "footer-contact-role", x$fonction),
          htmltools::div(class = "footer-contact-line", paste("Tél. :", x$tel)),
          htmltools::div(
            class = "footer-contact-line",
            "Mail : ",
            htmltools::tags$a(href = paste0("mailto:", x$mail), x$mail)
          )
        )
      })
    )
  )
}
