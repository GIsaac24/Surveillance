read_spatial_data <- function(path) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Le package sf est requis pour les cartes.")
  shx <- sub("[.]shp$", ".shx", path, ignore.case = TRUE)
  if (!file.exists(shx)) Sys.setenv(SHAPE_RESTORE_SHX = "YES")
  sf::st_read(path, quiet = TRUE)
}

find_district_shapefile <- function(shapefile_dir = SHAPEFILE_DIR) {
  candidates <- c(
    file.path(shapefile_dir, "DS et RS", "CAR_ADM2_DS.shp"),
    file.path(shapefile_dir, "CAR_ADM2_DS.shp"),
    file.path(shapefile_dir, "Couche DS", "District Sanitaire Nouveau Decoupage_VF3.shp")
  )
  candidates[file.exists(candidates)][1] %||% NA_character_
}

find_subpref_shapefile <- function(shapefile_dir = SHAPEFILE_DIR) {
  candidates <- c(
    file.path(shapefile_dir, "admin4 sous pref.shp"),
    file.path(shapefile_dir, "New shapefile for 35 districts", "CAR Admin 4 sous_pref", "admin4 sous pref.shp"),
    file.path(shapefile_dir, "New shapefile for 35 districts", "CAR Admin 4 sous_pref cartes", "admin4 sous pref cartes.shp"),
    file.path(shapefile_dir, "New shapefile for 35 districts", "CAR Admin 4 sous_pref_data", "CAR Admin 4 sous_pref_data.shp")
  )
  candidates[file.exists(candidates)][1] %||% NA_character_
}

make_valid_sf <- function(x) {
  if (is.null(x)) return(NULL)
  tryCatch(sf::st_make_valid(x), error = function(e) x)
}

load_priority_district_shapes <- function(shapefile_dir = SHAPEFILE_DIR, priority = PRIORITY_DISTRICTS) {
  path <- find_district_shapefile(shapefile_dir)
  if (is.na(path)) return(NULL)
  shp <- tryCatch(make_valid_sf(read_spatial_data(path)), error = function(e) NULL)
  if (is.null(shp)) return(NULL)
  name_col <- first_existing_col(sf::st_drop_geometry(shp), c("admin2Name", "district", "district_sanitaire", "NOM"))
  if (is.na(name_col)) return(NULL)
  shp$district_sanitaire <- as.character(sf::st_drop_geometry(shp)[[name_col]])
  shp$district_norm <- normalize_district_name(shp$district_sanitaire)
  shp$prioritaire <- shp$district_norm %in% normalize_district_name(priority)
  shp
}

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
  shp$sous_prefecture <- if (!is.na(name_col)) as.character(sf::st_drop_geometry(shp)[[name_col]]) else paste("Sous-préfecture", seq_len(nrow(shp)))
  shp
}

sf_feature_at_xy <- function(shp, x, y, id_col = "district_norm") {
  if (is.null(shp) || is.null(x) || is.null(y) || is.na(x) || is.na(y)) return(NA_character_)
  pt <- sf::st_sfc(sf::st_point(c(x, y)), crs = sf::st_crs(shp))
  hit <- tryCatch(sf::st_intersects(shp, pt, sparse = FALSE)[, 1], error = function(e) rep(FALSE, nrow(shp)))
  if (!any(hit)) return(NA_character_)
  as.character(sf::st_drop_geometry(shp)[[id_col]][which(hit)[1]])
}

subprefs_in_district <- function(subpref, district) {
  if (is.null(subpref) || is.null(district) || !nrow(district)) return(NULL)
  if (is.na(sf::st_crs(subpref)) && !is.na(sf::st_crs(district))) sf::st_crs(subpref) <- sf::st_crs(district)
  if (!is.na(sf::st_crs(subpref)) && !is.na(sf::st_crs(district)) && sf::st_crs(subpref) != sf::st_crs(district)) {
    subpref <- sf::st_transform(subpref, sf::st_crs(district))
  }
  idx <- tryCatch(sf::st_intersects(subpref, sf::st_union(sf::st_geometry(district)), sparse = FALSE)[, 1], error = function(e) rep(FALSE, nrow(subpref)))
  subpref[idx, ]
}

expand_bbox <- function(x, factor = 0.12) {
  bb <- sf::st_bbox(x)
  dx <- as.numeric(bb["xmax"] - bb["xmin"])
  dy <- as.numeric(bb["ymax"] - bb["ymin"])
  list(
    xlim = c(as.numeric(bb["xmin"] - dx * factor), as.numeric(bb["xmax"] + dx * factor)),
    ylim = c(as.numeric(bb["ymin"] - dy * factor), as.numeric(bb["ymax"] + dy * factor))
  )
}

plot_priority_districts <- function(shp, subpref = NULL, focus_norm = NA_character_, zoom_focus = FALSE) {
  if (is.null(shp) || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::annotate("text", x = 0, y = 0, label = "Carte indisponible"))
  }

  shp <- shp |>
    dplyr::mutate(
      focus = !is.na(focus_norm) & district_norm == focus_norm,
      statut = dplyr::case_when(
        focus ~ "District survolé",
        prioritaire ~ "Prioritaire",
        TRUE ~ "Autre district"
      )
    )

  focus_shape <- shp |> dplyr::filter(focus)
  sub_focus <- if (nrow(focus_shape)) subprefs_in_district(subpref, focus_shape) else NULL

  p <- ggplot2::ggplot(shp) +
    ggplot2::geom_sf(ggplot2::aes(fill = statut), color = "#FFFFFF", linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = c("District survolé" = "#7F1D1D", "Prioritaire" = "#DC2626", "Autre district" = "#E5E7EB"), name = NULL) +
    ggplot2::labs(title = "Districts sanitaires prioritaires", subtitle = "Préparation à la menace d’importation MVE — RCA") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#7F1D1D"),
      plot.subtitle = ggplot2::element_text(color = "#4B5563"),
      legend.position = "bottom"
    )

  if (!is.null(sub_focus) && nrow(sub_focus)) {
    sub_centers <- suppressWarnings(sf::st_point_on_surface(sub_focus))
    p <- p +
      ggplot2::geom_sf(data = sub_focus, fill = NA, color = "#111827", linewidth = 0.38, linetype = "22") +
      ggplot2::geom_sf_text(data = sub_centers, ggplot2::aes(label = sous_prefecture), size = 2.6, color = "#111827")
  }

  if (isTRUE(zoom_focus) && nrow(focus_shape)) {
    lim <- expand_bbox(focus_shape)
    p <- p + ggplot2::coord_sf(xlim = lim$xlim, ylim = lim$ylim, expand = FALSE)
  }

  p
}

enrich_lab_sites_with_subpref <- function(lab_sites, subpref = NULL) {
  sites <- lab_sites |>
    dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
    dplyr::mutate(site_id = dplyr::row_number(), sous_prefecture = NA_character_)

  if (!nrow(sites) || is.null(subpref)) return(sites)

  pts <- sf::st_as_sf(sites, coords = c("longitude", "latitude"), crs = sf::st_crs(subpref), remove = FALSE)
  idx <- tryCatch(sf::st_intersects(pts, subpref), error = function(e) NULL)
  if (is.null(idx)) return(sites)
  sites$sous_prefecture <- vapply(idx, function(i) if (length(i)) subpref$sous_prefecture[i[1]] else NA_character_, character(1))
  sites
}

nearest_lab_site <- function(sites, x, y, threshold = 0.35) {
  if (is.null(sites) || !nrow(sites) || is.null(x) || is.null(y) || is.na(x) || is.na(y)) return(NA_integer_)
  dist <- sqrt((sites$longitude - x)^2 + (sites$latitude - y)^2)
  if (!length(dist) || min(dist, na.rm = TRUE) > threshold) return(NA_integer_)
  sites$site_id[which.min(dist)]
}

plot_lab_sites <- function(shp, lab_sites, subpref = NULL, focus_site_id = NA_integer_, zoom_focus = FALSE) {
  if (is.null(shp) || !requireNamespace("ggplot2", quietly = TRUE)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::annotate("text", x = 0, y = 0, label = "Carte indisponible"))
  }
  sites <- enrich_lab_sites_with_subpref(lab_sites, subpref)
  focus_site <- sites |> dplyr::filter(site_id == focus_site_id)
  p <- ggplot2::ggplot(shp) +
    ggplot2::geom_sf(fill = "#F3F4F6", color = "#FFFFFF", linewidth = 0.25) +
    ggplot2::labs(title = "Sites d’installation des laboratoires", subtitle = "Localités prévues pour l’appui laboratoire MVE") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#7F1D1D"),
      plot.subtitle = ggplot2::element_text(color = "#4B5563")
    )
  if (nrow(sites)) {
    p <- p +
      ggplot2::geom_point(data = sites, ggplot2::aes(x = longitude, y = latitude), inherit.aes = FALSE, color = "#B91C1C", fill = "#FDE68A", size = 3, shape = 21, stroke = 1.1) +
      ggplot2::geom_text(data = sites, ggplot2::aes(x = longitude, y = latitude, label = localite), inherit.aes = FALSE, color = "#111827", size = 3, nudge_y = 0.16)
  }
  if (nrow(focus_site)) {
    p <- p +
      ggplot2::geom_point(data = focus_site, ggplot2::aes(x = longitude, y = latitude), inherit.aes = FALSE, color = "#111827", fill = "#22C55E", size = 5.2, shape = 21, stroke = 1.35) +
      ggplot2::geom_label(
        data = focus_site,
        ggplot2::aes(x = longitude, y = latitude, label = paste0(localite, "\nSous-préfecture : ", dplyr::coalesce(sous_prefecture, "ND"))),
        inherit.aes = FALSE,
        nudge_y = 0.35,
        size = 3.2,
        color = "#111827",
        fill = "#FFFFFF",
        label.size = 0.25
      )
  }
  if (isTRUE(zoom_focus) && nrow(focus_site)) {
    lim <- list(
      xlim = c(focus_site$longitude - 1.2, focus_site$longitude + 1.2),
      ylim = c(focus_site$latitude - 1.0, focus_site$latitude + 1.0)
    )
    p <- p + ggplot2::coord_sf(xlim = lim$xlim, ylim = lim$ylim, expand = FALSE)
  }
  p
}

map_note <- function() {
  "Cartes Shiny interactives : survoler un district prioritaire pour zoomer et afficher les sous-préfectures ; survoler un site laboratoire pour afficher sa sous-préfecture."
}
