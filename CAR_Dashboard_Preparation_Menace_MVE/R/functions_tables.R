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

make_reactable <- function(data, searchable = TRUE, page_size = 8, columns = NULL) {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    return(htmltools::tags$pre(utils::capture.output(print(utils::head(data, page_size)))))
  }
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
