logo_path <- file.path("www", "armoiries_rca.png")

dashboard_header <- htmltools::div(
  class = "main-header",
  if (file.exists(logo_path)) htmltools::img(src = "armoiries_rca.png", class = "header-logo"),
  htmltools::div(
    htmltools::div(class = "eyebrow", "République Centrafricaine"),
    htmltools::h1(DASHBOARD_TITLE),
    htmltools::p(DASHBOARD_SUBTITLE)
  )
)

ui <- bslib::page_navbar(
  title = "Dashboard MVE RCA",
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#B91C1C",
    secondary = "#374151",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Montserrat")
  ),
  header = htmltools::tagList(
    htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    publication_banner_ui(PUBLICATION_DIR),
    dashboard_header
  ),
  footer = contact_footer_ui(),
  bslib::nav_panel("Vue d’ensemble", mod_overview_ui("overview")),
  bslib::nav_panel("Contexte", mod_contexte_ui("contexte")),
  bslib::nav_panel("Chronologie", mod_chronologie_ui("chronologie")),
  bslib::nav_panel("PoE fluviaux", mod_poe_fluvial_ui("poe_fluvial")),
  bslib::nav_panel("Aéroport M’Poko", mod_poe_aeroport_ui("poe_aeroport")),
  bslib::nav_panel(
    "Préparation",
    section_title(
      "Préparation opérationnelle",
      "Lecture consolidée des piliers de préparation, de la réponse et du chronogramme des formations."
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Piliers de préparation et réponse", mod_piliers_ui("piliers")),
      bslib::nav_panel("Matrice des formations et chronogramme", mod_formations_ui("formations"))
    )
  ),
  bslib::nav_panel("Laboratoires", mod_laboratoires_ui("laboratoires")),
  bslib::nav_panel("Cartographie", mod_cartes_ui("cartes"))
)
