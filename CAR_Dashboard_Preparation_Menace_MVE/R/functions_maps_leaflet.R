load_subpref_shapes <- function(shapefile_dir = SHAPEFILE_DIR, district_shapes = NULL) {
  path <- find_subpref_shapefile(shapefile_dir)
  if (is.na(path)) return(NULL)
  shp <- tryCatch(make_valid_sf(read_spatial_data(path)), error = function(e) NULL)
  if (is.null(shp)) return(NULL)
  if (!is.null(district_shapes) && is.na(sf::st_crs(shp)) && !is.na(sf::st_crs(district_shapes))) {
    sf::st_crs(shp) <- sf::st_crs(district_shapes)
  }
  if (!is.null(district_shapes) && !is.na(sf::st_crs(shp)) && !is.na(sf::st_crs(district_shapes)) && sf::st_crs(shp) != sf::st_crs(district_shapes)) {
    shp <- sf::st_transform(shp, sf::st_crs(district_shapes))
  }

  name_col <- first_existing_col(sf::st_drop_geometry(shp), c("NAME", "NAMEMAJ", "admin4Name", "sous_prefecture", "sous_pref"))
  shp$sous_prefecture <- if (!is.na(name_col)) {
    as.character(sf::st_drop_geometry(shp)[[name_col]])
  } else {
    paste("Sous-préfecture", seq_len(nrow(shp)))
  }

  district_col <- first_existing_col(sf::st_drop_geometry(shp), c("admin2Name", "district_sanitaire", "district", "ADM2_FR", "NOM_DS"))
  shp$district_sanitaire <- if (!is.na(district_col)) as.character(sf::st_drop_geometry(shp)[[district_col]]) else NA_character_
  if (!is.null(district_shapes) && all(is.na(shp$district_sanitaire))) {
    centers <- suppressWarnings(sf::st_point_on_surface(shp))
    idx <- tryCatch(sf::st_intersects(centers, district_shapes), error = function(e) NULL)
    if (!is.null(idx)) {
      shp$district_sanitaire <- vapply(
        idx,
        function(i) if (length(i)) district_shapes$district_sanitaire[i[1]] else NA_character_,
        character(1)
      )
    }
  }
  shp$district_norm <- normalize_district_name(shp$district_sanitaire)
  shp
}

sf_to_leaflet <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- 4326
    return(x)
  }
  sf::st_transform(x, 4326)
}

leaflet_html_labels <- function(x) {
  lapply(x, htmltools::HTML)
}

leaflet_zoom_on_hover <- function(widget, marker_zoom = TRUE, polygon_zoom = TRUE) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) return(widget)
  htmlwidgets::onRender(
    widget,
    sprintf(
      "
function(el, x) {
  var map = this;
  var markerZoom = %s;
  var polygonZoom = %s;
  function zoomBounds(layer, maxZoom) {
    if (!layer.getBounds) return;
    var b = layer.getBounds();
    if (b && b.isValid && b.isValid()) {
      map.fitBounds(b.pad(0.12), {animate: true, duration: 0.25, maxZoom: maxZoom});
    }
  }
  map.eachLayer(function(layer) {
    if (polygonZoom && layer.feature && layer.getBounds) {
      layer.on('mouseover', function(e) {
        zoomBounds(layer, layer.feature.properties && layer.feature.properties.sous_prefecture ? 10 : 8);
        if (layer.openTooltip) layer.openTooltip();
      });
      layer.on('click', function(e) {
        zoomBounds(layer, layer.feature.properties && layer.feature.properties.sous_prefecture ? 10 : 8);
        if (layer.openPopup) layer.openPopup();
      });
    }
    if (markerZoom && layer.getLatLng && !layer.getBounds) {
      layer.on('mouseover', function(e) {
        map.flyTo(layer.getLatLng(), Math.max(map.getZoom(), 8), {animate: true, duration: 0.25});
        if (layer.openTooltip) layer.openTooltip();
      });
      layer.on('click', function(e) {
        map.flyTo(layer.getLatLng(), Math.max(map.getZoom(), 9), {animate: true, duration: 0.25});
        if (layer.openPopup) layer.openPopup();
      });
    }
  });
}
",
      tolower(as.character(marker_zoom)),
      tolower(as.character(polygon_zoom))
    )
  )
}

leaflet_priority_map <- function(shp, subpref = NULL) {
  if (is.null(shp) || !requireNamespace("leaflet", quietly = TRUE)) {
    return(htmltools::div(class = "note-box", "Carte Leaflet indisponible."))
  }
  shp <- sf_to_leaflet(shp)
  subpref <- sf_to_leaflet(subpref)

  shp$map_label <- paste0(
    "<strong>District : </strong>", shp$district_sanitaire,
    "<br><strong>Statut : </strong>", ifelse(shp$prioritaire, "Prioritaire MVE", "Autre district")
  )
  shp$map_popup <- paste0(
    "<strong>District sanitaire</strong><br>", shp$district_sanitaire,
    "<br><br><strong>Prioritaire : </strong>", ifelse(shp$prioritaire, "Oui", "Non")
  )
  shp$layer_id <- paste0("district:", shp$district_norm)

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolygons(
      data = shp,
      layerId = ~layer_id,
      group = "Districts sanitaires",
      fillColor = ~ifelse(prioritaire, "#DC2626", "#E5E7EB"),
      fillOpacity = ~ifelse(prioritaire, 0.58, 0.34),
      color = "#FFFFFF",
      weight = 0.8,
      label = leaflet_html_labels(shp$map_label),
      popup = leaflet_html_labels(shp$map_popup),
      highlightOptions = leaflet::highlightOptions(weight = 3, color = "#111827", bringToFront = TRUE)
    )

  if (!is.null(subpref) && nrow(subpref)) {
    subpref$district_sanitaire <- dplyr::coalesce(subpref$district_sanitaire, "District non renseigné")
    subpref$map_label <- paste0(
      "<strong>Sous-préfecture : </strong>", subpref$sous_prefecture,
      "<br><strong>District : </strong>", subpref$district_sanitaire
    )
    subpref$map_popup <- paste0(
      "<strong>Sous-préfecture</strong><br>", subpref$sous_prefecture,
      "<br><br><strong>District sanitaire : </strong>", subpref$district_sanitaire
    )
    subpref$layer_id <- paste0("subpref:", seq_len(nrow(subpref)))
    map <- map |>
      leaflet::addPolygons(
        data = subpref,
        layerId = ~layer_id,
        group = "Sous-préfectures",
        fillColor = "#FEF3C7",
        fillOpacity = 0.08,
        color = "#111827",
        weight = 0.65,
        dashArray = "4",
        label = leaflet_html_labels(subpref$map_label),
        popup = leaflet_html_labels(subpref$map_popup),
        highlightOptions = leaflet::highlightOptions(weight = 2.5, color = "#F97316", fillOpacity = 0.22, bringToFront = TRUE)
      )
  }

  map |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#DC2626", "#E5E7EB", "#FEF3C7"),
      labels = c("District prioritaire", "Autre district", "Sous-préfecture"),
      opacity = 0.85
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet_zoom_on_hover(marker_zoom = FALSE, polygon_zoom = TRUE)
}

leaflet_lab_sites_map <- function(shp, lab_sites, subpref = NULL) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    return(htmltools::div(class = "note-box", "Carte Leaflet indisponible."))
  }
  shp <- sf_to_leaflet(shp)
  subpref <- sf_to_leaflet(subpref)
  sites <- enrich_lab_sites_with_subpref(lab_sites, subpref) |>
    dplyr::filter(!is.na(longitude), !is.na(latitude))

  if (nrow(sites)) {
    sites$site_id <- if ("site_id" %in% names(sites)) sites$site_id else seq_len(nrow(sites))
    sites$map_label <- paste0(
      "<strong>", sites$localite, "</strong>",
      "<br>Sous-préfecture : ", dplyr::coalesce(sites$sous_prefecture, "ND"),
      "<br>District : ", dplyr::coalesce(sites$district_sanitaire, "ND")
    )
    sites$map_popup <- paste0(
      "<strong>Site laboratoire : </strong>", sites$localite,
      "<br><strong>Sous-préfecture : </strong>", dplyr::coalesce(sites$sous_prefecture, "ND"),
      "<br><strong>District : </strong>", dplyr::coalesce(sites$district_sanitaire, "ND"),
      "<br><strong>Type : </strong>", dplyr::coalesce(sites$type_laboratoire, "ND"),
      "<br><strong>Statut : </strong>", dplyr::coalesce(sites$statut_installation, "ND")
    )
  }

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  if (!is.null(shp) && nrow(shp)) {
    map <- map |>
      leaflet::addPolygons(
        data = shp,
        group = "Districts sanitaires",
        fillColor = ~ifelse(prioritaire, "#FECACA", "#F3F4F6"),
        fillOpacity = ~ifelse(prioritaire, 0.35, 0.18),
        color = "#FFFFFF",
        weight = 0.6,
        label = leaflet_html_labels(paste0("<strong>District : </strong>", shp$district_sanitaire)),
        highlightOptions = leaflet::highlightOptions(weight = 2.5, color = "#7F1D1D", bringToFront = TRUE)
      )
  }

  if (!is.null(subpref) && nrow(subpref)) {
    subpref$district_sanitaire <- dplyr::coalesce(subpref$district_sanitaire, "District non renseigné")
    subpref$map_label <- paste0(
      "<strong>Sous-préfecture : </strong>", subpref$sous_prefecture,
      "<br><strong>District : </strong>", subpref$district_sanitaire
    )
    map <- map |>
      leaflet::addPolygons(
        data = subpref,
        group = "Sous-préfectures",
        fillColor = "#FFFFFF",
        fillOpacity = 0,
        color = "#9CA3AF",
        weight = 0.45,
        dashArray = "3",
        label = leaflet_html_labels(subpref$map_label),
        highlightOptions = leaflet::highlightOptions(weight = 2.2, color = "#F97316", fillOpacity = 0.16, bringToFront = TRUE)
      )
  }

  if (nrow(sites)) {
    map <- map |>
      leaflet::addCircleMarkers(
        data = sites,
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~paste0("lab:", site_id),
        group = "Sites laboratoire",
        radius = 7,
        stroke = TRUE,
        color = "#111827",
        weight = 1.2,
        fillColor = "#FBBF24",
        fillOpacity = 0.95,
        label = leaflet_html_labels(sites$map_label),
        popup = leaflet_html_labels(sites$map_popup)
      )
  } else {
    map <- leaflet::addControl(map, html = "Aucun site laboratoire avec coordonnées disponibles.", position = "topright")
  }

  map |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#FBBF24", "#FECACA", "#F3F4F6"),
      labels = c("Site laboratoire", "District prioritaire", "Autre district"),
      opacity = 0.9
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet_zoom_on_hover(marker_zoom = TRUE, polygon_zoom = TRUE)
}

map_note <- function() {
  "Cartes Leaflet interactives : survolez ou cliquez un district, une sous-préfecture ou un site laboratoire pour zoomer et afficher les informations."
}
