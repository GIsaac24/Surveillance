normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[’'`´]", " ", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

clean_names_fr <- function(x) {
  x <- normalize_text(x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("^_|_$", "", x)
  x[x == ""] <- "colonne"
  make.unique(x, sep = "_")
}

normalize_district_name <- function(x) {
  y <- normalize_text(x)
  y <- gsub("\\bbangui 1\\b|\\bbangui i\\b", "bangui i", y)
  y <- gsub("\\bbangui 2\\b|\\bbangui ii\\b", "bangui ii", y)
  y <- gsub("\\bbangui 3\\b|\\bbangui iii\\b", "bangui iii", y)
  y <- gsub("\\bmbaiki\\b", "mbaiki", y)
  trimws(y)
}

safe_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("[[:space:]\u00A0]", "", x)
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    out <- suppressWarnings(as.Date(x, origin = "1899-12-30"))
    return(out)
  }
  x <- as.character(x)
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out)) && requireNamespace("lubridate", quietly = TRUE)) {
    out <- suppressWarnings(lubridate::ymd(x))
    miss <- is.na(out)
    out[miss] <- suppressWarnings(lubridate::dmy(x[miss]))
    miss <- is.na(out)
    out[miss] <- suppressWarnings(as.Date(lubridate::parse_date_time(x[miss], orders = c("ymd HMS", "dmy HMS", "ymd HM", "dmy HM"))))
  }
  out
}

first_existing_col <- function(data, candidates) {
  if (is.null(data) || !ncol(data)) return(NA_character_)
  nm <- names(data)
  nmc <- clean_names_fr(nm)
  candidates_clean <- clean_names_fr(candidates)
  idx <- match(candidates_clean, nmc)
  idx <- idx[!is.na(idx)]
  if (length(idx)) nm[idx[1]] else NA_character_
}

col_or_default <- function(data, candidates, default = NA) {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) {
    if (length(default) == nrow(data)) {
      default
    } else {
      rep(default, length.out = nrow(data))
    }
  } else {
    data[[col]]
  }
}

clean_status <- function(x) {
  y <- normalize_text(x)
  y[y %in% c("", "na", "nd", "non renseigne")] <- NA_character_
  y <- dplyr::case_when(
    grepl("real", y) ~ "réalisée",
    grepl("cours", y) ~ "en cours",
    grepl("retard", y) ~ "en retard",
    grepl("plan|prev", y) ~ "planifiée",
    grepl("report", y) ~ "reportée",
    grepl("annul", y) ~ "annulée",
    grepl("non", y) | grepl("demar", y) ~ "non démarrée",
    TRUE ~ y
  )
  y[is.na(y)] <- "planifiée"
  factor(y, levels = STATUS_LEVELS)
}

clean_priority <- function(x) {
  y <- normalize_text(x)
  y <- dplyr::case_when(
    grepl("haut|urgent|crit", y) ~ "haute",
    grepl("moy", y) ~ "moyenne",
    grepl("faib|bas", y) ~ "faible",
    TRUE ~ "moyenne"
  )
  factor(y, levels = PRIORITY_LEVELS)
}

classify_type_poe <- function(point_entree) {
  p <- normalize_text(point_entree)
  dplyr::case_when(
    grepl("aero|aeroport|airport", p) ~ "aeroport",
    TRUE ~ "fluvial"
  )
}

clean_type_poe <- function(type_poe, point_entree = NULL) {
  t <- normalize_text(type_poe)
  inferred <- if (!is.null(point_entree)) classify_type_poe(point_entree) else rep(NA_character_, length(t))
  dplyr::case_when(
    grepl("aero|aeroport|airport", t) ~ "aeroport",
    grepl("fluv|port|river|riviere", t) ~ "fluvial",
    is.na(t) | t == "" | t %in% c("na", "nd", "non renseigne") ~ inferred,
    TRUE ~ inferred
  )
}

district_from_poe <- function(point_entree) {
  p <- normalize_text(point_entree)
  dplyr::case_when(
    grepl("ouango|bema", p) ~ "Ouango-Gambo",
    grepl("mpoko|argue|modale", p) ~ "Bimbo",
    grepl("aero|airport|beach|sao|sucaf|dameca|ngou", p) ~ "Bangui I, II et III",
    TRUE ~ "Non renseigné"
  )
}

flag_source <- function(is_example) {
  ifelse(isTRUE(is_example), "Données fictives / modèle", "Données réelles importées")
}
