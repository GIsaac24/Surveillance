options(encoding = "UTF-8")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

dashboard_app_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) NA_character_)
    if (!is.na(path) && nzchar(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

APP_DIR <- Sys.getenv("MVE_APP_DIR", unset = dashboard_app_dir())
PROJECT_BASE <- Sys.getenv("MVE_PROJECT_BASE", unset = normalizePath(file.path(APP_DIR, ".."), winslash = "/", mustWork = FALSE))
DATA_DIR <- Sys.getenv("MVE_DATA_DIR", unset = file.path(PROJECT_BASE, "data"))
SHAPEFILE_DIR <- Sys.getenv("MVE_SHAPEFILE_DIR", unset = file.path(PROJECT_BASE, "shapefile"))
OUTPUT_DIR <- Sys.getenv("MVE_OUTPUT_DIR", unset = file.path(PROJECT_BASE, "outputs"))
WWW_DIR <- file.path(APP_DIR, "www")

publication_dir_candidates <- c(
  Sys.getenv("MVE_PUBLICATION_DIR", unset = NA_character_),
  file.path(PROJECT_BASE, "publication"),
  file.path(APP_DIR, "publication"),
  "C:/Users/user/Documents/Préparation EBOLA/SitReps/Data surveillance/analyse_poe/analyse_poe/publication"
)
publication_dir_candidates <- publication_dir_candidates[!is.na(publication_dir_candidates) & nzchar(publication_dir_candidates)]
publication_dir_candidates <- normalizePath(publication_dir_candidates, winslash = "/", mustWork = FALSE)
PUBLICATION_DIR <- publication_dir_candidates[dir.exists(publication_dir_candidates)][1] %||% file.path(APP_DIR, "publication")

DASHBOARD_NAME <- "CAR_Dashboard_Preparation_Menace_MVE"
DASHBOARD_TITLE <- "Préparation de la RCA face à la menace d’importation de la MVE"
DASHBOARD_SUBTITLE <- "Maladie à Virus Ebola — souche Bundibugyo"

PRIORITY_DISTRICTS <- c(
  "Haut-Mbomou", "Bangassou", "Ouango-Gambo", "Kembe-Satema", "Mobaye-Zangba",
  "Kouango-Grimari", "Kemo", "Bimbo", "Begoua", "Bangui I", "Bangui II",
  "Bangui III", "Mbaïki"
)

LAB_SITES_DEFAULT <- data.frame(
  localite = c("Zémio", "Bangassou", "Berberati", "Bangui I", "Bambari", "Mobaye", "Bossangoa"),
  district_sanitaire = c("Haut-Mbomou", "Bangassou", "Berbérati", "Bangui I", "Bambari", "Mobaye-Zangba", "Bossangoa"),
  region_sanitaire = c("RS6", "RS6", "RS2", "RS7", "RS4", "RS6", "RS3"),
  latitude = c(5.032, 4.741, 4.261, 4.394, 5.766, 4.319, 6.493),
  longitude = c(25.136, 22.819, 15.792, 18.558, 20.676, 21.180, 17.455),
  type_laboratoire = "Laboratoire d’appui MVE",
  niveau_laboratoire = c("Site périphérique", "Site périphérique", "Site régional", "Référence nationale", "Site régional", "Site périphérique", "Site régional"),
  statut_installation = c("prévu", "prévu", "prévu", "prévu", "prévu", "prévu", "prévu"),
  stringsAsFactors = FALSE
)

WHO_REFERENCE_VALUES <- data.frame(
  pays = c("RDC", "Ouganda"),
  date_situation = as.Date(c("2026-06-21", "2026-06-21")),
  cas_suspects = c(202, NA),
  cas_confirmes = c(1048, 20),
  deces_confirmes = c(267, 2),
  deces_probables = c(NA, 1),
  guerisons = c(112, 14),
  cfr_confirme = c(0.25, 0.10),
  source_donnees = "OMS — Daily epidemiological update, image publiée le 23 juin 2026, données au 21 juin 2026",
  url_source = "https://www.who.int/emergencies/alert-and-response",
  commentaire = "Valeurs de référence provisoires ; remplacer par les fichiers nationaux actualisés si disponibles.",
  stringsAsFactors = FALSE
)

PILLARS <- c(
  "Coordination, leadership et gouvernance multisectorielle",
  "Communication de Risque et Engagement Communautaire",
  "Surveillance épidémiologique",
  "Gestion des cas et prise en charge holistique",
  "PCI/WASH et EDS",
  "Support aux opérations",
  "Logistique et déploiement des mains d’œuvre",
  "Système de laboratoires et séquençage génomique",
  "Recherche et innovation",
  "Continuité des services de santé, des services sociaux essentiels et des systèmes de santé communautaires",
  "Prévention et réponse à l’exploitation, aux abus et au harcèlement sexuels, PSEAH, et protection des personnes vulnérables"
)

STATUS_LEVELS <- c("non démarrée", "planifiée", "en cours", "réalisée", "en retard", "reportée", "annulée")
PRIORITY_LEVELS <- c("haute", "moyenne", "faible")
