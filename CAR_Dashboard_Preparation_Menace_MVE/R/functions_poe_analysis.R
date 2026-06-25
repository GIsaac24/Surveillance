poe_prepare_analysis_data <- function(poe) {
  if (is.null(poe) || !nrow(poe)) return(dplyr::tibble())

  if (!"sexe_coherent" %in% names(poe)) poe$sexe_coherent <- NA
  if (!"temperature_coherente" %in% names(poe)) poe$temperature_coherente <- NA
  if (!"cascade_coherente" %in% names(poe)) poe$cascade_coherente <- NA
  if (!"rapport_complet" %in% names(poe)) poe$rapport_complet <- NA

  poe |>
    dplyr::mutate(
      point_entree = dplyr::if_else(is.na(point_entree) | point_entree == "", "Non renseigné", point_entree),
      district_sanitaire = dplyr::if_else(is.na(district_sanitaire) | district_sanitaire == "", district_from_poe(point_entree), district_sanitaire),
      type_poe = dplyr::coalesce(type_poe, classify_type_poe(point_entree)),
      nombre_voyageur = dplyr::coalesce(nombre_voyageur, 0),
      temp_inf38 = dplyr::coalesce(temp_inf38, 0),
      temp_sup38 = dplyr::coalesce(temp_sup38, 0),
      nombre_alerte = dplyr::coalesce(nombre_alerte, 0),
      alertes_verifiees = dplyr::coalesce(alertes_verifiees, 0),
      alertes_validees = dplyr::coalesce(alertes_validees, 0),
      alertes_detectees_analyse = pmax(nombre_alerte, temp_sup38, na.rm = TRUE),
      alertes_verifiees_analyse = dplyr::if_else(
        alertes_verifiees == 0 & nombre_alerte == 0 & temp_sup38 > 0,
        temp_sup38,
        alertes_verifiees
      ),
      alertes_validees_analyse = alertes_validees,
      temperature_coherente = dplyr::coalesce(temperature_coherente, abs((temp_inf38 + temp_sup38) - nombre_voyageur) < 1e-6),
      cascade_coherente = dplyr::coalesce(cascade_coherente, alertes_detectees_analyse >= alertes_verifiees_analyse & alertes_verifiees_analyse >= alertes_validees_analyse),
      rapport_complet = dplyr::coalesce(rapport_complet, !is.na(date_collecte) & point_entree != "Non renseigné"),
      taux_fievre = safe_rate(temp_sup38, nombre_voyageur)
    ) |>
    dplyr::filter(!grepl("^total", normalize_text(point_entree)))
}

poe_mean_logical <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

poe_date_fr <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("date non renseignée")
  x <- as.Date(x)[1]
  mois <- c(
    "janvier", "février", "mars", "avril", "mai", "juin",
    "juillet", "août", "septembre", "octobre", "novembre", "décembre"
  )
  paste(format(x, "%d"), mois[as.integer(format(x, "%m"))], format(x, "%Y"))
}

poe_date_range_label <- function(poe) {
  dates <- sort(unique(as.Date(poe$date_collecte[!is.na(poe$date_collecte)])))
  if (!length(dates)) return("période non renseignée")
  if (length(dates) == 1) return(poe_date_fr(dates[1]))
  paste("du", poe_date_fr(min(dates)), "au", poe_date_fr(max(dates)))
}

poe_reporting_matrix_advanced <- function(poe) {
  dates <- sort(unique(as.Date(poe$date_collecte[!is.na(poe$date_collecte)])))
  sites <- sort(unique(poe$point_entree[!is.na(poe$point_entree) & poe$point_entree != "Non renseigné"]))

  if (!length(dates) || !length(sites)) {
    return(dplyr::tibble(date_collecte = as.Date(character()), point_entree = character(), nombre_rapports = integer(), statut = character()))
  }

  all_dates <- seq(min(dates), max(dates), by = "day")
  observed <- poe |>
    dplyr::filter(!is.na(date_collecte), point_entree %in% sites) |>
    dplyr::count(date_collecte, point_entree, name = "nombre_rapports")

  tidyr::expand_grid(date_collecte = all_dates, point_entree = sites) |>
    dplyr::left_join(observed, by = c("date_collecte", "point_entree")) |>
    dplyr::mutate(
      nombre_rapports = dplyr::coalesce(nombre_rapports, 0L),
      statut = dplyr::if_else(nombre_rapports > 0, "Rapport reçu", "Rapport absent")
    )
}

poe_kpis_advanced <- function(poe, reporting_matrix = NULL) {
  total_voyageurs <- sum(poe$nombre_voyageur, na.rm = TRUE)
  total_temp_sup <- sum(poe$temp_sup38, na.rm = TRUE)
  total_alertes <- sum(poe$alertes_detectees_analyse, na.rm = TRUE)
  total_verifiees <- sum(poe$alertes_verifiees_analyse, na.rm = TRUE)
  total_validees <- sum(poe$alertes_validees_analyse, na.rm = TRUE)

  couverture <- NA_real_
  if (!is.null(reporting_matrix) && nrow(reporting_matrix)) {
    couverture <- mean(reporting_matrix$nombre_rapports > 0, na.rm = TRUE)
  }

  list(
    rapports = nrow(poe),
    jours = dplyr::n_distinct(poe$date_collecte[!is.na(poe$date_collecte)]),
    voyageurs = total_voyageurs,
    temp_sup38 = total_temp_sup,
    alertes_detectees = total_alertes,
    alertes_verifiees = total_verifiees,
    alertes_validees = total_validees,
    taux_fievre = safe_rate(total_temp_sup, total_voyageurs),
    taux_verification = safe_rate(total_verifiees, total_alertes),
    taux_validation = safe_rate(total_validees, total_alertes),
    n_poe = dplyr::n_distinct(poe$point_entree[poe$point_entree != "Non renseigné"]),
    n_districts = dplyr::n_distinct(poe$district_sanitaire[poe$district_sanitaire != "Non renseigné"]),
    couverture_notification = couverture,
    coherence_temperature = poe_mean_logical(poe$temperature_coherente),
    coherence_sexe = poe_mean_logical(poe$sexe_coherent),
    coherence_cascade = poe_mean_logical(poe$cascade_coherente),
    rapports_incoherence_sexe = sum(!poe$sexe_coherent, na.rm = TRUE),
    rapports_incoherence_temperature = sum(!poe$temperature_coherente, na.rm = TRUE),
    rapports_incoherence_cascade = sum(!poe$cascade_coherente, na.rm = TRUE)
  )
}

poe_summary_daily_advanced <- function(poe) {
  if (!nrow(poe) || all(is.na(poe$date_collecte))) {
    return(dplyr::tibble(date_collecte = as.Date(character()), voyageurs = numeric(), temp_sup38 = numeric(), alertes_detectees = numeric(), alertes_verifiees = numeric(), alertes_validees = numeric(), poe_rapporteurs = integer(), rapports = integer(), taux_fievre = numeric()))
  }

  poe |>
    dplyr::filter(!is.na(date_collecte)) |>
    dplyr::group_by(date_collecte) |>
    dplyr::summarise(
      voyageurs = sum(nombre_voyageur, na.rm = TRUE),
      temp_sup38 = sum(temp_sup38, na.rm = TRUE),
      alertes_detectees = sum(alertes_detectees_analyse, na.rm = TRUE),
      alertes_verifiees = sum(alertes_verifiees_analyse, na.rm = TRUE),
      alertes_validees = sum(alertes_validees_analyse, na.rm = TRUE),
      poe_rapporteurs = dplyr::n_distinct(point_entree),
      rapports = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(taux_fievre = safe_rate(temp_sup38, voyageurs)) |>
    dplyr::arrange(date_collecte)
}

poe_summary_site_advanced <- function(poe) {
  if (!nrow(poe)) {
    return(dplyr::tibble())
  }

  poe |>
    dplyr::group_by(district_sanitaire, point_entree, type_poe) |>
    dplyr::summarise(
      rapports = dplyr::n(),
      jours_notifies = dplyr::n_distinct(date_collecte[!is.na(date_collecte)]),
      voyageurs = sum(nombre_voyageur, na.rm = TRUE),
      temp_inf38 = sum(temp_inf38, na.rm = TRUE),
      temp_sup38 = sum(temp_sup38, na.rm = TRUE),
      alertes_detectees = sum(alertes_detectees_analyse, na.rm = TRUE),
      alertes_verifiees = sum(alertes_verifiees_analyse, na.rm = TRUE),
      alertes_validees = sum(alertes_validees_analyse, na.rm = TRUE),
      coherence_temperature = poe_mean_logical(temperature_coherente),
      coherence_sexe = poe_mean_logical(sexe_coherent),
      coherence_cascade = poe_mean_logical(cascade_coherente),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      taux_fievre = safe_rate(temp_sup38, voyageurs),
      part_volume = safe_rate(voyageurs, sum(voyageurs, na.rm = TRUE))
    ) |>
    dplyr::arrange(dplyr::desc(voyageurs))
}

poe_quality_summary_advanced <- function(poe, reporting_matrix = NULL) {
  k <- poe_kpis_advanced(poe, reporting_matrix)
  dplyr::tibble(
    controle = c("Cohérence températures", "Cohérence sexe", "Cohérence cascade alertes", "Complétude minimale", "Couverture notification"),
    valeur = c(
      k$coherence_temperature,
      k$coherence_sexe,
      k$coherence_cascade,
      poe_mean_logical(poe$rapport_complet),
      k$couverture_notification
    ),
    commentaire = c(
      "Voyageurs = T° < 38 + T° ≥ 38",
      "Masculin + féminin = voyageurs",
      "Détectées ≥ vérifiées ≥ validées",
      "Date et PoE renseignés",
      "Proxy : PoE observés × jours de la période"
    )
  )
}

poe_analysis_pack <- function(poe, scope = c("all", "fluvial")) {
  scope <- match.arg(scope)
  dat <- poe_prepare_analysis_data(poe)
  if (scope == "fluvial") dat <- dat |> dplyr::filter(type_poe == "fluvial")

  matrix <- poe_reporting_matrix_advanced(dat)
  kpi <- poe_kpis_advanced(dat, matrix)
  daily <- poe_summary_daily_advanced(dat)
  site <- poe_summary_site_advanced(dat)
  quality <- poe_quality_summary_advanced(dat, matrix)

  list(
    scope = scope,
    scope_label = ifelse(scope == "fluvial", "PoE fluviaux", "Tous les PoE"),
    data = dat,
    matrix = matrix,
    kpi = kpi,
    daily = daily,
    site = site,
    quality = quality,
    alert_sites = site |> dplyr::filter(alertes_detectees > 0) |> dplyr::arrange(dplyr::desc(alertes_detectees)),
    date_range_label = poe_date_range_label(dat),
    jour_pic = if (nrow(daily)) daily |> dplyr::slice_max(voyageurs, n = 1, with_ties = FALSE) else NULL,
    poe_principal = if (nrow(site)) site |> dplyr::slice_max(voyageurs, n = 1, with_ties = FALSE) else NULL
  )
}

poe_executive_ui <- function(pack, include_operational_notes = TRUE) {
  k <- pack$kpi
  if (is.null(k) || isTRUE(k$voyageurs == 0)) {
    return(htmltools::div(class = "warning-box", "Aucun volume de voyageurs exploitable n’est disponible pour cette sélection. Vérifier le classeur source ou alimenter les lignes PoE de la période."))
  }

  principal <- pack$poe_principal
  jour_pic <- pack$jour_pic
  part_poe <- if (!is.null(principal) && nrow(principal)) safe_rate(principal$voyageurs, k$voyageurs) else NA_real_
  alerte_phrase <- if (k$alertes_validees > 0) {
    paste0(fmt_num(k$alertes_validees), " alerte(s) validée(s) nécessitent une revue opérationnelle immédiate.")
  } else {
    "Aucune alerte n’est enregistrée comme validée dans le fichier."
  }

  executive <- htmltools::div(
      class = "insight-box",
      htmltools::h3("Lecture épidémiologique et opérationnelle"),
      htmltools::tags$ul(
        htmltools::tags$li(
          htmltools::tags$strong(fmt_num(k$voyageurs), " voyageurs"),
          " ont été contrôlés sur ", fmt_num(k$jours), " jour(s), à partir de ",
          fmt_num(k$rapports), " rapport(s) reçu(s), ", pack$date_range_label, "."
        ),
        if (!is.null(jour_pic) && nrow(jour_pic)) {
          htmltools::tags$li(
            "Le pic journalier est de ",
            htmltools::tags$strong(fmt_num(jour_pic$voyageurs)),
            " voyageurs le ", poe_date_fr(jour_pic$date_collecte), "."
          )
        },
        if (!is.null(principal) && nrow(principal)) {
          htmltools::tags$li(
            "Le PoE ",
            htmltools::tags$strong(htmltools::htmlEscape(principal$point_entree)),
            " concentre ", fmt_pct(part_poe), " du volume contrôlé (",
            fmt_num(principal$voyageurs), " voyageurs)."
          )
        },
        htmltools::tags$li(
          htmltools::tags$strong(fmt_num(k$temp_sup38), " température(s) ≥ 38 °C"),
          " ont été enregistrées, soit ", fmt_pct(k$taux_fievre), " des voyageurs contrôlés."
        ),
        htmltools::tags$li(
          fmt_num(k$alertes_detectees), " alerte(s)/signal(aux) de dépistage ont été détecté(s), ",
          fmt_num(k$alertes_verifiees), " vérifié(s), et ",
          fmt_num(k$alertes_validees), " validé(s). ", alerte_phrase
        )
      )
    )

  if (!isTRUE(include_operational_notes)) {
    return(executive)
  }

  htmltools::tagList(
    executive,
    htmltools::div(
      class = "alert-box",
      htmltools::tags$strong("Interprétation MVE : "),
      "une température ≥ 38 °C est un signal de dépistage et non un cas suspect MVE à elle seule. La classification exige les symptômes, les expositions, l’itinéraire et l’investigation clinique."
    ),
    htmltools::div(
      class = ifelse(k$rapports_incoherence_sexe > 0 || is.na(k$coherence_sexe), "warning-box", "note-box"),
      htmltools::tags$strong("Qualité critique : "),
      if (k$rapports_incoherence_sexe > 0) {
        paste0("la somme masculin + féminin ne correspond pas au nombre de voyageurs dans ", fmt_num(k$rapports_incoherence_sexe), " rapport(s) sur ", fmt_num(k$rapports), ". La ventilation par sexe doit être corrigée avant interprétation.")
      } else if (is.na(k$coherence_sexe)) {
        "la ventilation par sexe n’est pas suffisamment renseignée pour produire une analyse fiable."
      } else {
        "la ventilation par sexe est cohérente avec le total des voyageurs dans les rapports exploitables."
      }
    ),
    htmltools::div(
      class = "note-box",
      htmltools::tags$strong("Notification : "),
      "la couverture affichée est un proxy basé sur les PoE observés et les jours de la période. Elle doit être recalculée avec un référentiel officiel des PoE attendus et de leur fréquence de notification."
    )
  )
}

poe_priority_actions_ui <- function(pack, include_quality_action = TRUE) {
  k <- pack$kpi
  actions <- c(
    "Investiguer et documenter toutes les alertes/signaux : définition de cas, symptômes, exposition, itinéraire, vérification et issue finale.",
    "Afficher et briefer les procédures opérationnelles MVE dans chaque PoE : détection, vérification, validation, isolement temporaire et référence.",
    "Formaliser le référentiel des PoE attendus : district, type de PoE, jours d’activité, fréquence de rapportage et responsable de notification.",
    "Renforcer les PoE à forte charge ou avec signaux fébriles : agents, thermomètres fonctionnels, EPI, espace d’isolement temporaire et circuit de référence.",
    "Ajouter les variables MVE critiques : provenance, destination, voyage en zone affectée dans les 21 jours, symptômes, exposition, délai de vérification, isolement et référence."
  )

  if (isTRUE(include_quality_action)) {
    actions <- c(actions, "Corriger les contrôles qualité récurrents, notamment la cohérence sexe/voyageurs et la cascade détectées–vérifiées–validées.")
  }

  if (!is.null(k) && k$alertes_validees > 0) {
    actions <- c("Activer une revue immédiate des alertes validées avec l’équipe de surveillance, le laboratoire et la prise en charge.", actions)
  }

  htmltools::div(
    class = "insight-box",
    htmltools::h3("Conclusions et actions prioritaires proposées"),
    htmltools::tags$ol(lapply(actions, htmltools::tags$li))
  )
}

plot_poe_daily_volume <- function(pack) {
  d <- pack$daily
  if (!nrow(d)) return(empty_plot("Aucune tendance quotidienne exploitable."))
  ggplot2::ggplot(d, ggplot2::aes(date_collecte, voyageurs)) +
    ggplot2::geom_col(fill = "#2B6CB0", width = 0.72) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "#163A63", linewidth = 0.8) +
    ggplot2::geom_point(color = "#163A63", size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = scales::label_number(big.mark = " ", decimal.mark = ",")(voyageurs)), vjust = -0.5, size = 3.3, color = "#172033") +
    ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Évolution quotidienne des voyageurs contrôlés", x = NULL, y = "Voyageurs") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#163A63"))
}

plot_poe_fever_trend <- function(pack) {
  d <- pack$daily
  if (!nrow(d)) return(empty_plot("Aucun signal fébrile exploitable."))
  ggplot2::ggplot(d, ggplot2::aes(date_collecte, temp_sup38)) +
    ggplot2::geom_col(fill = "#FED7AA", color = "#C05621", width = 0.72) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "#C05621", linewidth = 0.9) +
    ggplot2::geom_point(color = "#9C4221", size = 2.4) +
    ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Signaux fébriles — T° ≥ 38 °C", x = NULL, y = "Voyageurs") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#9C4221"))
}

plot_poe_volume_site <- function(pack, n = 14) {
  s <- pack$site |> dplyr::slice_max(voyageurs, n = n)
  if (!nrow(s)) return(empty_plot("Aucun volume par PoE exploitable."))
  ggplot2::ggplot(s, ggplot2::aes(forcats::fct_reorder(point_entree, voyageurs), voyageurs, fill = district_sanitaire)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
    ggplot2::labs(title = "Volume par PoE", x = NULL, y = "Voyageurs", fill = "District") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#163A63"), legend.position = "bottom")
}

plot_poe_alert_cascade <- function(pack) {
  k <- pack$kpi
  cascade <- dplyr::tibble(
    etape = factor(c("Détectées", "Vérifiées", "Validées"), levels = c("Détectées", "Vérifiées", "Validées")),
    n = c(k$alertes_detectees, k$alertes_verifiees, k$alertes_validees)
  )
  ggplot2::ggplot(cascade, ggplot2::aes(etape, n, fill = etape)) +
    ggplot2::geom_col(width = 0.64) +
    ggplot2::geom_text(ggplot2::aes(label = scales::label_number(big.mark = " ", decimal.mark = ",")(n)), vjust = -0.45, fontface = "bold", size = 4) +
    ggplot2::scale_fill_manual(values = c("Détectées" = "#F97316", "Vérifiées" = "#2B6CB0", "Validées" = "#B91C1C")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "), expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::labs(title = "Cascade des alertes et signaux", x = NULL, y = "Nombre") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#7F1D1D"), legend.position = "none")
}

plot_poe_reporting_matrix <- function(pack) {
  m <- pack$matrix
  if (!nrow(m)) return(empty_plot("Matrice de notification non disponible : dates ou PoE manquants."))
  ggplot2::ggplot(m, ggplot2::aes(date_collecte, forcats::fct_rev(point_entree), fill = statut)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c("Rapport reçu" = "#2B6CB0", "Rapport absent" = "#FEE2E2")) +
    ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
    ggplot2::labs(title = "Matrice de notification", x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#163A63"), legend.position = "bottom")
}

plot_poe_quality <- function(pack) {
  q <- pack$quality |>
    dplyr::mutate(controle = forcats::fct_reorder(controle, valeur, .desc = FALSE))
  ggplot2::ggplot(q, ggplot2::aes(controle, valeur, fill = controle)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(
      ggplot2::aes(label = dplyr::if_else(is.na(valeur), "ND", scales::percent(valeur, accuracy = 0.1, decimal.mark = ","))),
      hjust = -0.08,
      fontface = "bold",
      size = 3.5
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = "Contrôles qualité", x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", color = "#7F1D1D"), legend.position = "none")
}
