import_data_file <- function(file_path, sheet = NULL, n_max = Inf) {
  if (is.null(file_path) || length(file_path) == 0 || !file.exists(file_path)) {
    stop("Fichier introuvable : ", file_path %||% "<NULL>")
  }
  ext <- tolower(tools::file_ext(file_path))
  switch(
    ext,
    csv = utils::read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", check.names = FALSE),
    txt = paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    rds = readRDS(file_path),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Le package readxl est requis pour lire les fichiers Excel.")
      sheets <- readxl::excel_sheets(file_path)
      sheet <- sheet %||% sheets[[1]]
      suppressMessages(suppressWarnings(readxl::read_excel(file_path, sheet = sheet, n_max = n_max)))
    },
    shp = read_spatial_data(file_path),
    geojson = {
      if (!requireNamespace("sf", quietly = TRUE)) stop("Le package sf est requis pour lire les fichiers spatiaux.")
      sf::st_read(file_path, quiet = TRUE)
    },
    docx = read_docx_text(file_path),
    stop("Extension non prise en charge : .", ext)
  )
}

detect_dashboard_files <- function(data_dir = DATA_DIR) {
  files <- list.files(data_dir, full.names = TRUE, recursive = FALSE)
  bn <- basename(files)
  norm <- normalize_text(tools::file_path_sans_ext(bn))
  ext <- tolower(tools::file_ext(files))

  pick <- function(pattern, exts = c("xlsx", "csv", "rds", "txt", "docx")) {
    i <- which(grepl(pattern, norm) & ext %in% exts)
    if (length(i)) files[i[1]] else NA_character_
  }

  pillar_candidates <- files[
    ext %in% c("xlsx", "csv", "rds") &
      !grepl("data_poe|poe|rdc|ouganda|contexte|template|chronologie|formation|site|materiel|district", norm)
  ]

  template_candidates <- c(
    file.path(APP_DIR, "outputs", "templates_donnees_dashboard_MVE.xlsx"),
    file.path(OUTPUT_DIR, "templates_donnees_dashboard_MVE.xlsx"),
    file.path(data_dir, "templates_donnees_dashboard_MVE.xlsx")
  )
  if (tolower(Sys.getenv("MVE_USE_OUTPUT_TEMPLATE", unset = "true")) %in% c("false", "0", "no", "non")) {
    template_candidates <- file.path(data_dir, "templates_donnees_dashboard_MVE.xlsx")
  }
  template_file <- template_candidates[file.exists(template_candidates)][1] %||% NA_character_

  list(
    template = template_file,
    contexte = pick("^contexte", c("docx", "txt", "xlsx", "csv")),
    poe = pick("data_poe|surveillance_poe|donnees_poe|poe", c("xlsx", "csv", "rds")),
    aeroport = pick("data_aeroport|donnees_aeroport|aeroport|airport", c("xlsx", "csv", "rds")),
    poe_ancien = {
      old_poe <- file.path(data_dir, "data_poeancien.xlsx")
      if (file.exists(old_poe)) old_poe else pick("data_poeancien|poe_ancien", c("xlsx", "csv", "rds"))
    },
    chronologie = pick("chronologie|timeline", c("xlsx", "csv", "rds")),
    formations = pick("formation|chronogramme", c("xlsx", "csv", "rds")),
    matrice_piliers = pick("matrice_pilier", c("xlsx", "csv", "rds")),
    activites_par_piliers = pick("activites_par_pilier|activites_pilier", c("xlsx", "csv", "rds")),
    materiels_laboratoire = pick("materiels_laboratoire|materiel_laboratoire|equipements_laboratoire|equipement_laboratoire", c("xlsx", "csv", "rds")),
    sites_laboratoires = pick("sites_laboratoire|site_laboratoire|sites_installation|installation_laboratoire", c("xlsx", "csv", "rds")),
    districts_prioritaires = pick("districts_prioritaires|district_prioritaire", c("xlsx", "csv", "rds")),
    rdc = pick("data_rdc|donnees_rdc|rdc", c("xlsx", "csv", "rds")),
    ouganda = pick("donnees_ouganda|uganda|ouganda", c("xlsx", "csv", "rds")),
    pillar_workbooks = pillar_candidates,
    images_dir = file.path(data_dir, "images")
  )
}

read_template_sheet <- function(files, sheet) {
  if (is.null(files$template) || is.na(files$template) || !file.exists(files$template)) return(NULL)
  tryCatch(suppressMessages(suppressWarnings(readxl::read_excel(files$template, sheet = sheet))), error = function(e) NULL)
}

read_docx_text <- function(file_path) {
  tmp <- tempfile("docx_")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(file_path, files = "word/document.xml", exdir = tmp)
  xml_path <- file.path(tmp, "word", "document.xml")
  if (!file.exists(xml_path)) return("Document Word illisible ou vide.")
  xml <- paste(readLines(xml_path, warn = FALSE, encoding = "UTF-8"), collapse = "")
  pars <- unlist(strsplit(xml, "</w:p>"), use.names = FALSE)
  text_pars <- vapply(pars, function(p) {
    tokens <- regmatches(p, gregexpr("<w:t[^>]*>.*?</w:t>", p, perl = TRUE))[[1]]
    if (!length(tokens) || identical(tokens, character(0))) return("")
    txt <- gsub("<[^>]+>", "", tokens)
    txt <- gsub("&amp;", "&", txt, fixed = TRUE)
    txt <- gsub("&lt;", "<", txt, fixed = TRUE)
    txt <- gsub("&gt;", ">", txt, fixed = TRUE)
    txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
    txt <- gsub("&apos;", "'", txt, fixed = TRUE)
    paste(txt, collapse = "")
  }, character(1))
  text_pars <- trimws(text_pars[nzchar(trimws(text_pars))])
  if (!length(text_pars)) "Document Word vide." else paste(text_pars, collapse = "\n\n")
}

read_context_data <- function(files) {
  if (!is.na(files$contexte) && file.exists(files$contexte)) {
    ext <- tolower(tools::file_ext(files$contexte))
    txt <- tryCatch({
      if (ext == "xlsx") {
        dat <- import_data_file(files$contexte)
        paste(utils::capture.output(print(dat)), collapse = "\n")
      } else if (ext == "csv") {
        dat <- import_data_file(files$contexte)
        paste(utils::capture.output(print(dat)), collapse = "\n")
      } else {
        import_data_file(files$contexte)
      }
    }, error = function(e) paste("Contexte non lisible :", conditionMessage(e)))
    return(list(text = txt, source = basename(files$contexte), is_example = FALSE))
  }

  list(
    text = paste(
      "Texte provisoire — à remplacer par le fichier Contexte dans le dossier data.",
      "La République Centrafricaine renforce sa préparation face à la menace d’importation de la maladie à virus Ebola, souche Bundibugyo, dans un contexte régional marqué par une flambée en RDC et en Ouganda.",
      "Les priorités immédiates portent sur la surveillance aux points d’entrée, l’alerte précoce, la préparation des laboratoires, la coordination multisectorielle, la communication de risque et la disponibilité des intrants critiques.",
      sep = "\n\n"
    ),
    source = "Texte provisoire",
    is_example = TRUE
  )
}

score_poe_sheet <- function(file_path, sheet) {
  preview <- tryCatch(suppressMessages(suppressWarnings(readxl::read_excel(file_path, sheet = sheet, n_max = 3))), error = function(e) NULL)
  if (is.null(preview)) return(0)

  sheet_name <- normalize_text(sheet)
  cols <- normalize_text(names(preview))
  score <- 0
  score <- score + ifelse(grepl("surveillance|saisie|poe|point entree", sheet_name), 5, 0)
  score <- score + ifelse(any(grepl("point entree|poe|site controle", cols)), 4, 0)
  score <- score + ifelse(any(grepl("voyageur|voyageurs", cols)), 4, 0)
  score <- score + ifelse(any(grepl("alerte|alertes", cols)), 3, 0)
  score <- score + ifelse(any(grepl("38", cols)), 3, 0)
  score <- score + ifelse(any(grepl("date|today|collecte", cols)), 2, 0)
  score
}

read_poe_data <- function(file_path) {
  if (is.na(file_path) || !file.exists(file_path)) {
    return(example_surveillance_poe())
  }

  raw <- tryCatch({
    ext <- tolower(tools::file_ext(file_path))
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(file_path)
      scores <- vapply(sheets, function(s) score_poe_sheet(file_path, s), numeric(1))
      chosen_sheet <- sheets[which.max(scores)]
      suppressMessages(suppressWarnings(readxl::read_excel(file_path, sheet = chosen_sheet)))
    } else {
      import_data_file(file_path)
    }
  }, error = function(e) {
    import_data_file(file_path)
  })

  clean_surveillance_poe(raw, source = basename(file_path), is_example = FALSE)
}

clean_surveillance_poe <- function(raw, source = "source PoE", is_example = FALSE) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (!nrow(raw)) return(example_surveillance_poe())

  date_collecte <- col_or_default(raw, c("Date de collecte...4", "Date de collecte rapportée", "Date collecte", "today", "Date de collecte...3", "Date"), NA)
  district <- col_or_default(raw, c("District sanitaire", "district", "district_sanitaire"), NA_character_)
  point <- col_or_default(raw, c("Point d’entrée / site de contrôle", "Point d'entree / site de controle", "point_entree", "Point_Entree", "poe", "PoE"), NA_character_)
  type_poe_raw <- col_or_default(raw, c("type_poe", "type poe", "type PoE", "type_point_entree", "type point entree"), NA_character_)
  voyageurs <- safe_num(col_or_default(raw, c("Nombre de voyageurs contrôlés", "Nombre de voyageurs controles", "Nombre de voyageur", "Nombre voyageurs", "nombrevoyageur", "nombre_voyageur", "voyageurs"), 0))

  read_metric <- function(primary, fallback = character(), default = 0) {
    existing_cols <- function(candidates) {
      if (!length(candidates)) return(character())
      nm <- names(raw)
      nmc <- clean_names_fr(nm)
      out <- character()
      for (candidate in candidates) {
        exact_idx <- match(tolower(candidate), tolower(nm))
        if (!is.na(exact_idx)) {
          out <- c(out, nm[exact_idx])
        } else {
          clean_idx <- match(clean_names_fr(candidate), nmc)
          if (!is.na(clean_idx)) out <- c(out, nm[clean_idx])
        }
      }
      unique(out)
    }
    pick_metric <- function(cols) {
      if (!length(cols)) return(NULL)
      vals <- lapply(cols, function(col) safe_num(raw[[col]]))
      info <- vapply(vals, function(v) sum(abs(v), na.rm = TRUE), numeric(1))
      chosen <- which(info > 0)[1]
      if (is.na(chosen)) chosen <- 1
      vals[[chosen]]
    }
    primary_val <- pick_metric(existing_cols(primary))
    fallback_val <- pick_metric(existing_cols(fallback))
    if (is.null(primary_val)) primary_val <- rep(default, nrow(raw))
    if (is.null(fallback_val)) fallback_val <- rep(default, nrow(raw))
    primary_has_info <- sum(abs(primary_val), na.rm = TRUE) > 0
    fallback_has_info <- sum(abs(fallback_val), na.rm = TRUE) > 0
    if (primary_has_info || !fallback_has_info) primary_val else fallback_val
  }

  temp_inf38 <- read_metric(
    c("temp_inf_38", "temp_inf38", "T°≤ 38°C", "T°≤38°C", "T <= 38", "T < 38", "Température < 38°C", "Nombre de voyageurs avec température < 38°C"),
    c("Nombre de voyageurs avec température ≤ 38°C", "Nombre de voyageurs avec température <= 38°C")
  )
  temp_sup38 <- read_metric(
    c("temp_sup_38", "temp_sup38", "T°≥38°C", "T°≥ 38°C", "T >= 38", "T > 38", "Température ≥ 38°C", "Nombre de voyageurs avec température ≥ 38°C"),
    c("Nombre de voyageurs avec température > 38°C", "Nombre de voyageurs avec température >= 38°C")
  )
  alerte <- read_metric(
    c("nombalerte", "nombre_alerte", "Alerte", "Alertes", "Nombre Alerte", "Nombre d’alertes détectées", "Nombre d'alertes détectées"),
    c("Nombre d’alertes", "Nombre d'alertes")
  )
  verifiees <- safe_num(col_or_default(raw, c("Nombre d’alertes vérifiées", "Nombre d'alertes vérifiées", "alertes_verifiees", "alertes vérifiées"), alerte))
  validees <- safe_num(col_or_default(raw, c("Nombre d’alertes validées", "Nombre d'alertes validées", "alertes_validees", "alertes validées"), 0))
  masculin <- safe_num(col_or_default(raw, c("Masculin", "Hommes", "Homme", "sexe_masculin"), NA_real_))
  feminin <- safe_num(col_or_default(raw, c("Feminin", "Féminin", "Femmes", "Femme", "sexe_feminin"), NA_real_))
  agent <- col_or_default(raw, c("Nom de l’agent collecteur", "Nom de l'agent collecteur", "agent_name", "agent_collecteur"), NA_character_)
  commentaire <- col_or_default(raw, c("Commentaire de l’agent", "Commentaire de l'agent", "commentaire"), NA_character_)
  latitude <- safe_num(col_or_default(raw, c("_gps_point_latitude", "latitude"), NA))
  longitude <- safe_num(col_or_default(raw, c("_gps_point_longitude", "longitude"), NA))

  out <- dplyr::tibble(
    date_collecte = safe_date(date_collecte),
    district_sanitaire = as.character(district),
    point_entree = as.character(point),
    type_poe_source = as.character(type_poe_raw),
    nombre_voyageur = dplyr::coalesce(voyageurs, 0),
    temp_inf38 = dplyr::coalesce(temp_inf38, 0),
    temp_sup38 = dplyr::coalesce(temp_sup38, 0),
    nombre_alerte = dplyr::coalesce(alerte, 0),
    alertes_verifiees = dplyr::coalesce(verifiees, 0),
    alertes_validees = dplyr::coalesce(validees, 0),
    masculin = masculin,
    feminin = feminin,
    agent_collecteur = as.character(agent),
    latitude = latitude,
    longitude = longitude,
    commentaire = as.character(commentaire),
    source_donnees = source,
    source_type = flag_source(is_example)
  ) |>
    dplyr::mutate(
      point_entree = dplyr::if_else(is.na(point_entree) | normalize_text(point_entree) %in% c("", "na", "nd", "non renseigne"), "Non renseigné", point_entree),
      district_sanitaire = dplyr::if_else(is.na(district_sanitaire) | normalize_text(district_sanitaire) %in% c("", "na", "nd", "non renseigne"), district_from_poe(point_entree), district_sanitaire),
      type_poe = clean_type_poe(type_poe_source, point_entree),
      temp_inf38 = dplyr::if_else(temp_inf38 == 0 & nombre_voyageur > 0 & temp_sup38 <= nombre_voyageur, nombre_voyageur - temp_sup38, temp_inf38),
      temperature_coherente = abs((temp_inf38 + temp_sup38) - nombre_voyageur) < 1e-6,
      sexe_renseigne = !is.na(masculin) | !is.na(feminin),
      sexe_coherent = dplyr::if_else(sexe_renseigne, abs((dplyr::coalesce(masculin, 0) + dplyr::coalesce(feminin, 0)) - nombre_voyageur) < 1e-6, NA),
      cascade_coherente = nombre_alerte >= alertes_verifiees & alertes_verifiees >= alertes_validees,
      rapport_complet = !is.na(date_collecte) & point_entree != "Non renseigné",
      taux_fievre = safe_rate(temp_sup38, nombre_voyageur)
    ) |>
    dplyr::filter(
      !grepl("^total", normalize_text(point_entree)),
      !(point_entree == "Non renseigné" & nombre_voyageur == 0 & temp_inf38 == 0 & temp_sup38 == 0 & nombre_alerte == 0)
    )

  if (!nrow(out)) return(example_surveillance_poe()[0, ])
  out
}

fix_possible_airport_date_swap <- function(dates) {
  raw_dates <- dates
  dates <- tryCatch(
    safe_date(raw_dates),
    error = function(e) {
      x <- as.character(raw_dates)
      out <- rep(as.Date(NA), length(x))

      numeric_x <- suppressWarnings(as.numeric(x))
      numeric_like <- !is.na(numeric_x) & grepl("^\\s*[0-9]+([.][0-9]+)?\\s*$", x)
      out[numeric_like] <- suppressWarnings(as.Date(numeric_x[numeric_like], origin = "1899-12-30"))

      remaining <- is.na(out)
      dmy_like <- remaining & grepl("^\\s*[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{2,4}\\s*$", x)
      if (any(dmy_like) && requireNamespace("lubridate", quietly = TRUE)) {
        out[dmy_like] <- suppressWarnings(lubridate::dmy(x[dmy_like]))
      }

      remaining <- is.na(out)
      if (any(remaining)) {
        out[remaining] <- tryCatch(
          suppressWarnings(as.Date(x[remaining])),
          error = function(e2) rep(as.Date(NA), sum(remaining))
        )
      }

      remaining <- is.na(out)
      if (any(remaining) && requireNamespace("lubridate", quietly = TRUE)) {
        out[remaining] <- suppressWarnings(as.Date(lubridate::parse_date_time(
          x[remaining],
          orders = c("dmy HMS", "dmy HM", "dmy", "ymd HMS", "mdy HMS", "ymd HM", "mdy HM", "ymd", "mdy")
        )))
      }
      out
    }
  )
  if (!length(dates) || all(is.na(dates))) return(dates)

  today <- Sys.Date()
  swapped <- suppressWarnings(as.Date(sprintf(
    "%s-%02d-%02d",
    format(dates, "%Y"),
    as.integer(format(dates, "%d")),
    as.integer(format(dates, "%m"))
  )))

  valid_swap <- !is.na(dates) & dates > today + 30 & !is.na(swapped) & swapped <= today + 30 & swapped >= today - 365
  if (any(valid_swap, na.rm = TRUE)) {
    dates[valid_swap] <- swapped[valid_swap]
  }
  dates
}

read_airport_data <- function(file_path) {
  if (is.na(file_path) || !file.exists(file_path)) {
    return(example_surveillance_poe()[0, ])
  }

  raw <- tryCatch({
    ext <- tolower(tools::file_ext(file_path))
    if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(file_path)
      sheet <- if ("data" %in% sheets) "data" else sheets[[1]]
      suppressMessages(suppressWarnings(readxl::read_excel(file_path, sheet = sheet)))
    } else {
      import_data_file(file_path)
    }
  }, error = function(e) NULL)

  if (is.null(raw) || !nrow(raw)) return(example_surveillance_poe()[0, ])
  clean_airport_data(raw, source = basename(file_path))
}

clean_airport_data <- function(raw, source = "data_aeroport") {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (!nrow(raw)) return(example_surveillance_poe()[0, ])

  screening_date <- col_or_default(raw, c("screening_date", "date_screening", "date_depistage", "date", "Date"), NA)
  temperature <- safe_num(col_or_default(raw, c("temperature", "température", "temp", "Temperature"), NA_real_))
  risk_level <- as.character(col_or_default(raw, c("risk_level", "niveau_risque", "risque", "Risk"), NA_character_))
  poe <- as.character(col_or_default(raw, c("PoE", "poe", "point_entree", "point d’entrée", "site"), "Aeroport Bangui Mpoko"))
  nationality <- as.character(col_or_default(raw, c("nationality", "nationalite", "nationalité"), NA_character_))

  dates <- fix_possible_airport_date_swap(screening_date)
  risk_norm <- normalize_text(risk_level)
  risk_alert <- !is.na(risk_norm) &
    nzchar(risk_norm) &
    !(risk_norm %in% c("libere", "liberee", "normal", "ok", "aucun risque", "faible", "ras"))
  temp_alert <- !is.na(temperature) & temperature >= 38

  line_level <- dplyr::tibble(
    date_collecte = dates,
    point_entree = dplyr::if_else(is.na(poe) | !nzchar(trimws(poe)), "Aeroport Bangui Mpoko", poe),
    temperature = temperature,
    risk_level = risk_level,
    nationality = nationality,
    alerte_ligne = dplyr::coalesce(temp_alert, FALSE) | dplyr::coalesce(risk_alert, FALSE)
  )

  out <- line_level |>
    dplyr::filter(!is.na(date_collecte)) |>
    dplyr::group_by(date_collecte, point_entree) |>
    dplyr::summarise(
      nombre_voyageur = dplyr::n(),
      temp_sup38 = sum(!is.na(temperature) & temperature >= 38, na.rm = TRUE),
      nombre_alerte = sum(alerte_ligne, na.rm = TRUE),
      alertes_verifiees = nombre_alerte,
      alertes_validees = 0,
      nationalites = dplyr::n_distinct(nationality[!is.na(nationality) & nzchar(nationality)]),
      niveaux_risque = paste(sort(unique(risk_level[!is.na(risk_level) & nzchar(risk_level)])), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      district_sanitaire = district_from_poe(point_entree),
      type_poe = "aeroport",
      temp_inf38 = pmax(nombre_voyageur - temp_sup38, 0),
      masculin = NA_real_,
      feminin = NA_real_,
      agent_collecteur = NA_character_,
      latitude = NA_real_,
      longitude = NA_real_,
      commentaire = paste0("Nationalités distinctes : ", nationalites, "; niveaux de risque : ", dplyr::coalesce(niveaux_risque, "ND")),
      source_donnees = source,
      source_type = "Données réelles importées",
      temperature_coherente = TRUE,
      sexe_renseigne = FALSE,
      sexe_coherent = NA,
      cascade_coherente = nombre_alerte >= alertes_verifiees & alertes_verifiees >= alertes_validees,
      rapport_complet = TRUE,
      taux_fievre = safe_rate(temp_sup38, nombre_voyageur)
    ) |>
    dplyr::select(
      date_collecte,
      district_sanitaire,
      point_entree,
      type_poe,
      nombre_voyageur,
      temp_inf38,
      temp_sup38,
      nombre_alerte,
      alertes_verifiees,
      alertes_validees,
      masculin,
      feminin,
      agent_collecteur,
      latitude,
      longitude,
      commentaire,
      source_donnees,
      source_type,
      temperature_coherente,
      sexe_renseigne,
      sexe_coherent,
      cascade_coherente,
      rapport_complet,
      taux_fievre
    )

  if (!nrow(out)) return(example_surveillance_poe()[0, ])
  out
}

example_surveillance_poe <- function() {
  dplyr::tibble(
    date_collecte = Sys.Date() - 6:0,
    district_sanitaire = rep(c("Bangui I, II et III", "Bimbo", "Ouango-Gambo"), length.out = 7),
    point_entree = rep(c("AEROPORT", "MPOKO 1", "OUANGO"), length.out = 7),
    type_poe = classify_type_poe(point_entree),
    nombre_voyageur = c(220, 180, 75, 260, 95, 145, 210),
    temp_inf38 = nombre_voyageur - c(2, 0, 1, 3, 0, 2, 1),
    temp_sup38 = c(2, 0, 1, 3, 0, 2, 1),
    nombre_alerte = temp_sup38,
    alertes_verifiees = temp_sup38,
    alertes_validees = 0,
    masculin = NA_real_,
    feminin = NA_real_,
    agent_collecteur = "Exemple",
    latitude = NA_real_,
    longitude = NA_real_,
    commentaire = "Données fictives de démonstration",
    source_donnees = "Exemple intégré",
    source_type = "Données fictives / modèle",
    temperature_coherente = TRUE,
    sexe_renseigne = FALSE,
    sexe_coherent = NA,
    cascade_coherente = TRUE,
    rapport_complet = TRUE,
    taux_fievre = safe_rate(temp_sup38, nombre_voyageur)
  )
}

derive_pillar_name <- function(file_path) {
  nm <- tools::file_path_sans_ext(basename(file_path))
  n <- normalize_text(nm)
  dplyr::case_when(
    grepl("coordination", n) ~ "Coordination, leadership et gouvernance multisectorielle",
    grepl("communication", n) ~ "Communication de Risque et Engagement Communautaire",
    grepl("surveillance", n) ~ "Surveillance épidémiologique",
    grepl("gestion", n) ~ "Gestion des cas et prise en charge holistique",
    grepl("pci|wash|eds", n) ~ "PCI/WASH et EDS",
    grepl("support|logistique|main", n) ~ "Support aux opérations / logistique",
    grepl("laboratoire|sequenc", n) ~ "Système de laboratoires et séquençage génomique",
    grepl("recherche|innovation", n) ~ "Recherche et innovation",
    grepl("continuite", n) ~ "Continuité des services essentiels",
    grepl("pseah|exploitation|abus|harcelement|vulnerable", n) ~ "PSEAH et protection des personnes vulnérables",
    TRUE ~ nm
  )
}

read_pillar_activity_files <- function(files) {
  if (!is.null(files$activites_par_piliers) && !is.na(files$activites_par_piliers) && file.exists(files$activites_par_piliers)) {
    dat <- tryCatch(import_data_file(files$activites_par_piliers), error = function(e) NULL)
    if (!is.null(dat)) return(process_pillar_activities(dat, source = basename(files$activites_par_piliers), is_example = FALSE))
  }

  if (length(files$pillar_workbooks)) {
    out <- lapply(files$pillar_workbooks, function(f) {
      dat <- tryCatch(import_data_file(f), error = function(e) NULL)
      if (is.null(dat) || !nrow(dat)) return(NULL)
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      lib <- col_or_default(dat, c("Libellés", "Libelles", "activite", "activité", "sous_activite"), NA_character_)
      cout_total <- safe_num(col_or_default(dat, c("Coût total", "Cout total", "Coût Total", "FraisTotal"), NA))
      cout_inv <- safe_num(col_or_default(dat, c("Cout investissement", "Coût investissement"), NA))
      cout_op <- safe_num(col_or_default(dat, c("Cout opérationnel", "Cout operationnel", "Coût opérationnel"), NA))
      pilier <- derive_pillar_name(f)
      dplyr::tibble(
        id_activite = paste0(substr(clean_names_fr(pilier), 1, 18), "_", seq_along(lib)),
        pilier = pilier,
        domaine_intervention = NA_character_,
        activite = as.character(lib),
        sous_activite = NA_character_,
        indicateur = NA_character_,
        cible = NA_real_,
        valeur_realisee = NA_real_,
        unite = NA_character_,
        taux_avancement = 0,
        district = "National",
        localite = NA_character_,
        responsable = NA_character_,
        partenaire = NA_character_,
        date_debut = as.Date(NA),
        date_fin = as.Date(NA),
        statut = "planifiée",
        niveau_priorite = "moyenne",
        goulot_etranglement = NA_character_,
        action_correctrice = NA_character_,
        cout_total = cout_total,
        cout_investissement = cout_inv,
        cout_operationnel = cout_op,
        commentaire = "Importé depuis un fichier de pilier existant",
        source_donnees = basename(f),
        source_type = "Données réelles importées"
      )
    })
    out <- dplyr::bind_rows(out)
    if (nrow(out)) return(process_pillar_activities(out, source = "Fichiers de piliers existants", is_example = FALSE))
  }

  templ <- read_template_sheet(files, "activites_par_piliers")
  if (!is.null(templ) && nrow(templ)) {
    return(process_pillar_activities(templ, source = basename(files$template), is_example = FALSE))
  }

  process_pillar_activities(example_pillar_activities(), source = "Exemple intégré", is_example = TRUE)
}

process_pillar_activities <- function(data, source = "activités par pilier", is_example = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!nrow(data)) data <- example_pillar_activities()

  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  taux <- safe_num(get(c("taux_avancement", "taux d’avancement", "progression"), NA))
  cible <- safe_num(get(c("cible"), NA))
  realise <- safe_num(get(c("valeur_realisee", "valeur réalisée", "realise", "réalisé"), NA))
  taux <- dplyr::case_when(
    !is.na(taux) & taux > 1 ~ taux / 100,
    !is.na(taux) ~ taux,
    !is.na(cible) & cible > 0 & !is.na(realise) ~ pmin(realise / cible, 1),
    TRUE ~ 0
  )

  dplyr::tibble(
    id_activite = as.character(get(c("id_activite", "id_action"), paste0("ACT_", seq_len(nrow(data))))),
    pilier = as.character(get(c("pilier"), "Pilier non renseigné")),
    domaine_intervention = as.character(get(c("domaine_intervention"), NA_character_)),
    activite = as.character(get(c("activite", "activité", "Libellés", "Libelles"), NA_character_)),
    sous_activite = as.character(get(c("sous_activite", "sous activité"), NA_character_)),
    indicateur = as.character(get(c("indicateur"), NA_character_)),
    cible = cible,
    valeur_realisee = realise,
    unite = as.character(get(c("unite", "unité"), NA_character_)),
    taux_avancement = pmax(pmin(taux, 1), 0),
    district = as.character(get(c("district", "district_sanitaire"), "National")),
    localite = as.character(get(c("localite", "localité"), NA_character_)),
    responsable = as.character(get(c("responsable"), NA_character_)),
    partenaire = as.character(get(c("partenaire"), NA_character_)),
    date_debut = safe_date(get(c("date_debut", "date début"), NA)),
    date_fin = safe_date(get(c("date_fin", "date fin"), NA)),
    statut = as.character(clean_status(get(c("statut"), NA_character_))),
    niveau_priorite = as.character(clean_priority(get(c("niveau_priorite", "priorite", "priorité"), NA_character_))),
    goulot_etranglement = as.character(get(c("goulot_etranglement", "goulot d’étranglement"), NA_character_)),
    action_correctrice = as.character(get(c("action_correctrice"), NA_character_)),
    cout_total = safe_num(get(c("cout_total", "Coût total", "Cout total", "FraisTotal"), NA)),
    cout_investissement = safe_num(get(c("cout_investissement", "Cout investissement"), NA)),
    cout_operationnel = safe_num(get(c("cout_operationnel", "Cout opérationnel"), NA)),
    commentaire = as.character(get(c("commentaire"), NA_character_)),
    source_donnees = as.character(get(c("source_donnees"), source)),
    source_type = as.character(get(c("source_type"), flag_source(is_example)))
  ) |>
    dplyr::filter(!is.na(activite), nzchar(trimws(activite)))
}

example_pillar_activities <- function() {
  dplyr::tibble(
    id_activite = paste0("EX_", seq_along(PILLARS)),
    pilier = PILLARS,
    domaine_intervention = "Préparation MVE",
    activite = paste("Activité prioritaire —", PILLARS),
    sous_activite = "Sous-activité à renseigner",
    indicateur = "Taux d’avancement",
    cible = 100,
    valeur_realisee = c(40, 25, 55, 20, 30, 35, 30, 45, 10, 20, 15),
    unite = "%",
    taux_avancement = c(0.40, 0.25, 0.55, 0.20, 0.30, 0.35, 0.30, 0.45, 0.10, 0.20, 0.15),
    district = "National",
    localite = NA_character_,
    responsable = "À renseigner",
    partenaire = "À renseigner",
    date_debut = Sys.Date() - 10,
    date_fin = Sys.Date() + 30,
    statut = "planifiée",
    niveau_priorite = "haute",
    goulot_etranglement = "À renseigner",
    action_correctrice = "À renseigner",
    cout_total = NA_real_,
    cout_investissement = NA_real_,
    cout_operationnel = NA_real_,
    commentaire = "Donnée fictive de structuration",
    source_donnees = "Exemple intégré",
    source_type = "Données fictives / modèle"
  )
}

load_chronologie <- function(files) {
  if (!is.na(files$chronologie) && file.exists(files$chronologie)) {
    dat <- tryCatch(import_data_file(files$chronologie), error = function(e) NULL)
    if (!is.null(dat)) return(clean_chronologie(dat, basename(files$chronologie), FALSE))
  }
  templ <- read_template_sheet(files, "chronologie_activites")
  if (!is.null(templ) && nrow(templ)) return(clean_chronologie(templ, basename(files$template), FALSE))
  clean_chronologie(example_chronologie(), "Exemple intégré", TRUE)
}

clean_chronologie <- function(data, source, is_example) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  dplyr::tibble(
    id_activite = as.character(get(c("id_activite"), paste0("CHR_", seq_len(nrow(data))))),
    date_activite = safe_date(get(c("date_activite", "date"), Sys.Date())),
    titre_activite = as.character(get(c("titre_activite", "titre", "activite"), NA_character_)),
    description = as.character(get(c("description"), NA_character_)),
    pilier = as.character(get(c("pilier"), NA_character_)),
    district = as.character(get(c("district"), "National")),
    localite = as.character(get(c("localite"), NA_character_)),
    responsable = as.character(get(c("responsable"), NA_character_)),
    partenaire = as.character(get(c("partenaire"), NA_character_)),
    statut = as.character(clean_status(get(c("statut"), "planifiée"))),
    resultat_cle = as.character(get(c("resultat_cle", "résultat clé"), NA_character_)),
    commentaire = as.character(get(c("commentaire"), NA_character_)),
    source_donnees = source,
    source_type = flag_source(is_example)
  )
}

example_chronologie <- function() {
  dplyr::tibble(
    id_activite = paste0("CHR_", 1:5),
    date_activite = Sys.Date() + c(-14, -7, 0, 7, 14),
    titre_activite = c("Activation coordination", "Mise à jour surveillance PoE", "Pré-positionnement intrants", "Formation équipes rapides", "Exercice de simulation"),
    description = c("Réunion stratégique", "Compilation des données", "Plan logistique", "Briefing opérationnel", "Test des circuits d’alerte"),
    pilier = c("Coordination", "Surveillance épidémiologique", "Logistique", "Gestion des cas", "Coordination"),
    district = "National",
    localite = NA_character_,
    responsable = "À renseigner",
    partenaire = "À renseigner",
    statut = c("réalisée", "en cours", "planifiée", "planifiée", "planifiée"),
    resultat_cle = "À renseigner",
    commentaire = "Donnée fictive de démonstration"
  )
}

load_formations <- function(files) {
  if (!is.na(files$formations) && file.exists(files$formations)) {
    dat <- tryCatch(import_data_file(files$formations), error = function(e) NULL)
    if (!is.null(dat)) return(clean_formations(dat, basename(files$formations), FALSE))
  }
  templ <- read_template_sheet(files, "formations")
  if (!is.null(templ) && nrow(templ)) return(clean_formations(templ, basename(files$template), FALSE))
  clean_formations(example_formations(), "Exemple intégré", TRUE)
}

clean_formations <- function(data, source, is_example) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  dplyr::tibble(
    id_formation = as.character(get(c("id_formation"), paste0("FOR_", seq_len(nrow(data))))),
    intitule_formation = as.character(get(c("intitule_formation", "formation", "intitulé formation"), NA_character_)),
    pilier = as.character(get(c("pilier"), NA_character_)),
    cible = as.character(get(c("cible"), NA_character_)),
    district = as.character(get(c("district"), "National")),
    localite = as.character(get(c("localite"), NA_character_)),
    date_debut_prevue = safe_date(get(c("date_debut_prevue"), Sys.Date())),
    date_fin_prevue = safe_date(get(c("date_fin_prevue"), Sys.Date() + 2)),
    date_debut_reelle = safe_date(get(c("date_debut_reelle"), NA)),
    date_fin_reelle = safe_date(get(c("date_fin_reelle"), NA)),
    participants_attendus = safe_num(get(c("participants_attendus"), NA)),
    participants_formes = safe_num(get(c("participants_formes"), NA)),
    responsable = as.character(get(c("responsable"), NA_character_)),
    partenaire = as.character(get(c("partenaire"), NA_character_)),
    statut = as.character(clean_status(get(c("statut"), "planifiée"))),
    observations = as.character(get(c("observations", "commentaire"), NA_character_)),
    source_donnees = source,
    source_type = flag_source(is_example)
  )
}

example_formations <- function() {
  dplyr::tibble(
    id_formation = paste0("FOR_", 1:5),
    intitule_formation = c("Surveillance et alerte PoE", "PCI/WASH MVE", "Prélèvement et transport échantillons", "Gestion des cas", "Communication de risque"),
    pilier = c("Surveillance épidémiologique", "PCI/WASH et EDS", "Système de laboratoires et séquençage génomique", "Gestion des cas", "CREC"),
    cible = c("Agents PoE", "FOSA prioritaires", "Laboratoires", "Équipes cliniques", "Relais communautaires"),
    district = c("Bimbo", "Bangui I", "Bangassou", "Haut-Mbomou", "Mbaïki"),
    localite = NA_character_,
    date_debut_prevue = Sys.Date() + c(1, 4, 7, 10, 13),
    date_fin_prevue = Sys.Date() + c(2, 5, 8, 11, 14),
    date_debut_reelle = as.Date(NA),
    date_fin_reelle = as.Date(NA),
    participants_attendus = c(30, 25, 15, 25, 40),
    participants_formes = c(0, 0, 0, 0, 0),
    responsable = "À renseigner",
    partenaire = "À renseigner",
    statut = "planifiée",
    observations = "Donnée fictive de démonstration"
  )
}

load_laboratory_materials <- function(files) {
  if (!is.na(files$materiels_laboratoire) && file.exists(files$materiels_laboratoire)) {
    dat <- tryCatch(import_data_file(files$materiels_laboratoire), error = function(e) NULL)
    if (!is.null(dat)) return(clean_materials(dat, basename(files$materiels_laboratoire), FALSE))
  }
  templ <- read_template_sheet(files, "materiels_laboratoire")
  if (!is.null(templ) && nrow(templ)) return(clean_materials(templ, basename(files$template), FALSE))
  clean_materials(example_materials(), "Exemple intégré", TRUE)
}

clean_materials <- function(data, source, is_example) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  req <- safe_num(get(c("quantite_requise", "quantité requise"), NA))
  disp <- safe_num(get(c("quantite_disponible", "quantité disponible"), NA))
  gap <- safe_num(get(c("gap"), req - disp))
  dplyr::tibble(
    id_materiel = as.character(get(c("id_materiel"), paste0("MAT_", seq_len(nrow(data))))),
    nom_materiel = as.character(get(c("nom_materiel", "materiel", "matériel"), NA_character_)),
    categorie = as.character(get(c("categorie", "catégorie"), NA_character_)),
    description = as.character(get(c("description"), NA_character_)),
    quantite_disponible = dplyr::coalesce(disp, 0),
    quantite_requise = dplyr::coalesce(req, 0),
    gap = dplyr::coalesce(gap, pmax(req - disp, 0)),
    priorite = as.character(clean_priority(get(c("priorite", "priorité"), "moyenne"))),
    localisation_prevue = as.character(get(c("localisation_prevue"), NA_character_)),
    statut_acquisition = as.character(clean_status(get(c("statut_acquisition", "statut"), "planifiée"))),
    image_materiel = as.character(get(c("image_materiel"), NA_character_)),
    commentaire = as.character(get(c("commentaire"), NA_character_)),
    source_donnees = source,
    source_type = flag_source(is_example)
  )
}

example_materials <- function() {
  dplyr::tibble(
    id_materiel = paste0("MAT_", 1:5),
    nom_materiel = c("EPI complet", "Thermoflash", "Triple emballage échantillons", "Glacière biomédicale", "Kits PCR"),
    categorie = c("PCI", "Surveillance", "Laboratoire", "Laboratoire", "Laboratoire"),
    description = "À préciser",
    quantite_disponible = c(120, 20, 30, 5, 0),
    quantite_requise = c(300, 45, 80, 15, 20),
    gap = quantite_requise - quantite_disponible,
    priorite = c("haute", "moyenne", "haute", "haute", "haute"),
    localisation_prevue = c("Districts prioritaires", "PoE", "Sites laboratoires", "Sites laboratoires", "Laboratoire national"),
    statut_acquisition = c("en cours", "planifiée", "planifiée", "planifiée", "non démarrée"),
    image_materiel = NA_character_,
    commentaire = "Donnée fictive de démonstration"
  )
}

load_lab_sites <- function(files) {
  if (!is.na(files$sites_laboratoires) && file.exists(files$sites_laboratoires)) {
    dat <- tryCatch(import_data_file(files$sites_laboratoires), error = function(e) NULL)
    if (!is.null(dat)) return(clean_lab_sites(dat, basename(files$sites_laboratoires), FALSE))
  }
  templ <- read_template_sheet(files, "sites_laboratoires")
  if (!is.null(templ) && nrow(templ)) return(clean_lab_sites(templ, basename(files$template), FALSE))
  clean_lab_sites(LAB_SITES_DEFAULT, "Liste fournie dans le cahier des charges", TRUE)
}

clean_lab_sites <- function(data, source, is_example) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  dplyr::tibble(
    id_site = as.character(get(c("id_site"), paste0("LAB_", seq_len(nrow(data))))),
    localite = as.character(get(c("localite", "localité"), NA_character_)),
    district_sanitaire = as.character(get(c("district_sanitaire", "district"), NA_character_)),
    region_sanitaire = as.character(get(c("region_sanitaire", "region"), NA_character_)),
    latitude = safe_num(get(c("latitude"), NA)),
    longitude = safe_num(get(c("longitude"), NA)),
    type_laboratoire = as.character(get(c("type_laboratoire"), "Laboratoire MVE")),
    niveau_laboratoire = as.character(get(c("niveau_laboratoire"), NA_character_)),
    statut_installation = as.character(clean_status(get(c("statut_installation", "statut"), "planifiée"))),
    date_prevue_installation = safe_date(get(c("date_prevue_installation"), NA)),
    partenaire_appui = as.character(get(c("partenaire_appui", "partenaire"), NA_character_)),
    commentaire = as.character(get(c("commentaire"), NA_character_)),
    source_donnees = source,
    source_type = flag_source(is_example)
  )
}

load_rdc_ouganda_data <- function(files) {
  daily <- NULL
  zones <- NULL
  if (!is.na(files$rdc) && file.exists(files$rdc)) {
    daily <- tryCatch({
      d <- if (tolower(tools::file_ext(files$rdc)) == "xlsx") suppressMessages(suppressWarnings(readxl::read_excel(files$rdc, sheet = "data_rdc"))) else import_data_file(files$rdc)
      clean_rdc_daily(d, basename(files$rdc), FALSE)
    }, error = function(e) NULL)
    zones <- tryCatch({
      if (tolower(tools::file_ext(files$rdc)) == "xlsx") {
        z <- suppressMessages(suppressWarnings(readxl::read_excel(files$rdc, sheet = "Sheet1")))
        clean_rdc_zones(z, basename(files$rdc), FALSE)
      } else NULL
    }, error = function(e) NULL)
  }
  if (is.null(daily)) daily <- example_rdc_daily()
  templ_rdc <- read_template_sheet(files, "donnees_rdc")
  if (!is.null(templ_rdc) && nrow(templ_rdc) && (is.null(daily) || any(daily$source_type == "Données fictives / modèle"))) {
    daily <- clean_rdc_daily(templ_rdc, basename(files$template), FALSE)
  }
  if (is.null(zones)) zones <- dplyr::tibble()
  list(
    rdc_daily = daily,
    rdc_zones = zones,
    rdc_ouganda_summary = WHO_REFERENCE_VALUES
  )
}

clean_rdc_daily <- function(data, source, is_example) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  dplyr::tibble(
    pays = "RDC",
    date = safe_date(get(c("Date confirmation", "date"), NA)),
    cas_confirmes = safe_num(get(c("Nombre cas confirmés", "cas_confirmes", "Conf"), 0)),
    cas_suspects = safe_num(get(c("cas_suspects", "Suspects"), NA)),
    deces = safe_num(get(c("décès", "deces", "Décès"), NA)),
    guerisons = safe_num(get(c("guerisons", "guérisons"), NA)),
    source_donnees = source,
    source_type = flag_source(is_example)
  ) |>
    dplyr::filter(!is.na(date))
}

clean_rdc_zones <- function(data, source, is_example) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  get <- function(candidates, default = NA) col_or_default(data, candidates, default)
  zone <- get(c("zone_sante", "zone de santé", "...1"), NA_character_)
  out <- dplyr::tibble(
    pays = "RDC",
    province_region = NA_character_,
    zone_sante = as.character(zone),
    cas_confirmes = safe_num(get(c("Conf", "cas_confirmes"), 0)),
    cas_suspects = safe_num(get(c("Suspects", "cas_suspects"), NA)),
    deces = safe_num(get(c("Décès", "deces"), NA)),
    source_donnees = source,
    source_type = flag_source(is_example)
  )
  out |>
    dplyr::filter(!is.na(zone_sante), nzchar(trimws(zone_sante)))
}

example_rdc_daily <- function() {
  dplyr::tibble(
    pays = "RDC",
    date = seq(Sys.Date() - 14, Sys.Date(), by = "day"),
    cas_confirmes = c(8, 12, 9, 18, 20, 25, 18, 30, 28, 35, 31, 40, 37, 42, 45),
    cas_suspects = NA_real_,
    deces = NA_real_,
    guerisons = NA_real_,
    source_donnees = "Exemple intégré",
    source_type = "Données fictives / modèle"
  )
}

load_districts_prioritaires <- function(files) {
  if (!is.na(files$districts_prioritaires) && file.exists(files$districts_prioritaires)) {
    dat <- tryCatch(import_data_file(files$districts_prioritaires), error = function(e) NULL)
    if (!is.null(dat) && nrow(dat)) {
      col <- first_existing_col(dat, c("district_sanitaire", "district", "nom"))
      return(dplyr::tibble(district_sanitaire = as.character(dat[[col]]), source_type = "Données réelles importées"))
    }
  }
  templ <- read_template_sheet(files, "districts_prioritaires")
  if (!is.null(templ) && nrow(templ)) {
    col <- first_existing_col(templ, c("district_sanitaire", "district", "nom"))
    if (!is.na(col)) return(dplyr::tibble(district_sanitaire = as.character(templ[[col]]), source_type = paste("Modèle renseigné :", basename(files$template))))
  }
  dplyr::tibble(district_sanitaire = PRIORITY_DISTRICTS, source_type = "Liste fournie dans le cahier des charges")
}

load_dashboard_data <- function(data_dir = DATA_DIR) {
  files <- detect_dashboard_files(data_dir)
  epi <- load_rdc_ouganda_data(files)
  poe_template <- read_template_sheet(files, "surveillance_poe")
  poe_fluvial <- if (!is.null(poe_template) && nrow(poe_template)) {
    clean_surveillance_poe(
      poe_template,
      source = paste0(basename(files$template), " / surveillance_poe"),
      is_example = FALSE
    ) |>
      dplyr::filter(type_poe == "fluvial")
  } else {
    read_poe_data(files$poe) |>
      dplyr::filter(type_poe == "fluvial")
  }
  if (nrow(poe_fluvial) == 0 || sum(poe_fluvial$nombre_voyageur, na.rm = TRUE) == 0) {
    if (!is.na(files$poe_ancien) && file.exists(files$poe_ancien)) {
      poe_fluvial <- read_poe_data(files$poe_ancien) |>
        dplyr::filter(type_poe == "fluvial") |>
        dplyr::mutate(
          source_donnees = paste0(source_donnees, " — utilisé comme secours car le template/data_poe.xlsx ne contient pas de volume exploitable")
        )
    }
  }
  poe_aeroport <- read_airport_data(files$aeroport)
  if (nrow(poe_aeroport) == 0 && !is.null(poe_template) && nrow(poe_template)) {
    poe_aeroport <- clean_surveillance_poe(
      poe_template,
      source = paste0(basename(files$template), " / surveillance_poe"),
      is_example = FALSE
    ) |>
      dplyr::filter(type_poe == "aeroport")
  }
  poe <- dplyr::bind_rows(poe_fluvial, poe_aeroport)
  activities <- read_pillar_activity_files(files)
  formations <- load_formations(files)
  lab_sites <- load_lab_sites(files)
  materials <- load_laboratory_materials(files)
  districts <- load_districts_prioritaires(files)

  list(
    files = files,
    contexte = read_context_data(files),
    poe = poe,
    poe_fluvial = poe_fluvial,
    poe_aeroport = poe_aeroport,
    chronologie = load_chronologie(files),
    formations = formations,
    pillar_activities = activities,
    matrice_piliers = activities,
    materiels_laboratoire = materials,
    sites_laboratoires = lab_sites,
    districts_prioritaires = districts,
    rdc_daily = epi$rdc_daily,
    rdc_zones = epi$rdc_zones,
    rdc_ouganda_summary = epi$rdc_ouganda_summary,
    indicators = calculate_dashboard_indicators(list(
      poe = poe,
      activities = activities,
      formations = formations,
      lab_sites = lab_sites,
      materials = materials,
      districts = districts,
      rdc_daily = epi$rdc_daily,
      rdc_ouganda_summary = epi$rdc_ouganda_summary
    )),
    loaded_at = Sys.time()
  )
}
