fmt_num <- function(x, digits = 0) {
  if (length(x) == 0 || all(is.na(x))) return("ND")
  format(round(sum(x, na.rm = TRUE), digits), big.mark = " ", decimal.mark = ",", trim = TRUE, scientific = FALSE)
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x)) return("ND")
  paste0(format(round(100 * x, digits), decimal.mark = ",", trim = TRUE, scientific = FALSE), " %")
}

safe_rate <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

poe_indicators <- function(poe) {
  total_voyageurs <- sum(poe$nombre_voyageur, na.rm = TRUE)
  total_temp_sup <- sum(poe$temp_sup38, na.rm = TRUE)
  total_alertes <- sum(poe$nombre_alerte, na.rm = TRUE)
  total_verifiees <- sum(poe$alertes_verifiees, na.rm = TRUE)
  total_validees <- sum(poe$alertes_validees, na.rm = TRUE)

  list(
    voyageurs = total_voyageurs,
    temp_sup38 = total_temp_sup,
    alertes = total_alertes,
    alertes_verifiees = total_verifiees,
    alertes_validees = total_validees,
    taux_temp_sup38 = safe_rate(total_temp_sup, total_voyageurs),
    taux_alertes_verifiees = safe_rate(total_verifiees, total_alertes),
    taux_alertes_validees = safe_rate(total_validees, total_alertes),
    n_poe = dplyr::n_distinct(poe$point_entree),
    n_districts = dplyr::n_distinct(poe$district_sanitaire),
    date_min = suppressWarnings(min(poe$date_collecte, na.rm = TRUE)),
    date_max = suppressWarnings(max(poe$date_collecte, na.rm = TRUE))
  )
}

activities_indicators <- function(activities) {
  n <- nrow(activities)
  done <- sum(activities$statut == "réalisée", na.rm = TRUE)
  late <- sum(activities$statut == "en retard", na.rm = TRUE)
  high <- sum(activities$niveau_priorite == "haute", na.rm = TRUE)
  list(
    total_activites = n,
    taux_realisation = safe_rate(done, n),
    taux_avancement_global = mean(activities$taux_avancement, na.rm = TRUE),
    activites_retard = late,
    activites_priorite_haute = high,
    n_piliers = dplyr::n_distinct(activities$pilier)
  )
}

formations_indicators <- function(formations) {
  total <- nrow(formations)
  done <- sum(formations$statut == "réalisée", na.rm = TRUE)
  planned <- sum(formations$statut %in% c("planifiée", "en cours"), na.rm = TRUE)
  list(
    total_formations = total,
    formations_realisees = done,
    formations_planifiees = planned,
    taux_participation = safe_rate(sum(formations$participants_formes, na.rm = TRUE), sum(formations$participants_attendus, na.rm = TRUE))
  )
}

lab_indicators <- function(lab_sites, materials) {
  list(
    laboratoires_prevus = nrow(lab_sites),
    laboratoires_installes = sum(lab_sites$statut_installation == "réalisée", na.rm = TRUE),
    gap_materiel = sum(materials$gap, na.rm = TRUE),
    materiels_priorite_haute = sum(materials$priorite == "haute", na.rm = TRUE)
  )
}

epidemio_indicators <- function(summary_df, rdc_daily = NULL) {
  total_confirmed <- sum(summary_df$cas_confirmes, na.rm = TRUE)
  total_deaths <- sum(summary_df$deces_confirmes, na.rm = TRUE)
  total_recovered <- sum(summary_df$guerisons, na.rm = TRUE)
  cfr <- safe_rate(total_deaths, total_confirmed)
  last24 <- NA_real_
  if (!is.null(rdc_daily) && nrow(rdc_daily)) {
    last_date <- max(rdc_daily$date, na.rm = TRUE)
    last24 <- sum(rdc_daily$cas_confirmes[rdc_daily$date == last_date], na.rm = TRUE)
  }
  list(
    cas_confirmes_total = total_confirmed,
    deces_confirmes_total = total_deaths,
    guerisons_total = total_recovered,
    cfr = cfr,
    cas_confirmes_24h = last24,
    cas_confirmes_ouganda = sum(summary_df$cas_confirmes[normalize_text(summary_df$pays) %in% c("ouganda", "uganda")], na.rm = TRUE)
  )
}

calculate_dashboard_indicators <- function(data) {
  poe <- poe_indicators(data$poe)
  act <- activities_indicators(data$activities)
  frm <- formations_indicators(data$formations)
  lab <- lab_indicators(data$lab_sites, data$materials)
  epi <- epidemio_indicators(data$rdc_ouganda_summary, data$rdc_daily)
  list(
    poe = poe,
    activities = act,
    formations = frm,
    lab = lab,
    epidemio = epi,
    strategic = list(
      districts_prioritaires = nrow(data$districts),
      score_preparation = mean(c(act$taux_avancement_global, frm$taux_participation %||% NA_real_, safe_rate(lab$laboratoires_installes, lab$laboratoires_prevus)), na.rm = TRUE)
    )
  )
}

daily_poe_summary <- function(poe) {
  poe |>
    dplyr::group_by(date_collecte, type_poe) |>
    dplyr::summarise(
      voyageurs = sum(nombre_voyageur, na.rm = TRUE),
      temp_sup38 = sum(temp_sup38, na.rm = TRUE),
      alertes = sum(nombre_alerte, na.rm = TRUE),
      alertes_verifiees = sum(alertes_verifiees, na.rm = TRUE),
      alertes_validees = sum(alertes_validees, na.rm = TRUE),
      .groups = "drop"
    )
}

site_poe_summary <- function(poe) {
  poe |>
    dplyr::group_by(district_sanitaire, point_entree, type_poe) |>
    dplyr::summarise(
      voyageurs = sum(nombre_voyageur, na.rm = TRUE),
      temp_sup38 = sum(temp_sup38, na.rm = TRUE),
      alertes = sum(nombre_alerte, na.rm = TRUE),
      alertes_verifiees = sum(alertes_verifiees, na.rm = TRUE),
      alertes_validees = sum(alertes_validees, na.rm = TRUE),
      jours_notifies = dplyr::n_distinct(date_collecte),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(voyageurs))
}

pillar_progress_summary <- function(activities) {
  activities |>
    dplyr::group_by(pilier) |>
    dplyr::summarise(
      n_activites = dplyr::n(),
      taux_avancement = mean(taux_avancement, na.rm = TRUE),
      realisees = sum(statut == "réalisée", na.rm = TRUE),
      en_retard = sum(statut == "en retard", na.rm = TRUE),
      priorite_haute = sum(niveau_priorite == "haute", na.rm = TRUE),
      cout_total = sum(cout_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(taux_avancement)
}

status_summary <- function(data, status_col = "statut") {
  data |>
    dplyr::count(.data[[status_col]], name = "n") |>
    dplyr::mutate(pct = n / sum(n))
}

quality_alerts <- function(data) {
  poe <- data$poe
  temp_incoherent <- sum(abs((poe$temp_inf38 + poe$temp_sup38) - poe$nombre_voyageur) > 0, na.rm = TRUE)
  alerts <- c()
  if (temp_incoherent > 0) alerts <- c(alerts, paste(temp_incoherent, "lignes PoE ont une incohérence entre voyageurs et températures."))
  if (any(data$pillar_activities$source_type == "Données fictives / modèle")) alerts <- c(alerts, "Certaines activités par pilier proviennent du modèle fictif.")
  if (any(data$formations$source_type == "Données fictives / modèle")) alerts <- c(alerts, "Le chronogramme des formations utilise des exemples fictifs faute de fichier réel.")
  if (any(data$materiels_laboratoire$source_type == "Données fictives / modèle")) alerts <- c(alerts, "Les besoins matériels de laboratoire utilisent des exemples fictifs faute de fichier réel.")
  if (!length(alerts)) alerts <- "Aucune alerte qualité majeure détectée."
  dplyr::tibble(message = alerts)
}
