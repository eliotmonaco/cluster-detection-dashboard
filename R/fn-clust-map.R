# Functions for cluster map

# Filter location geometries by cluster
get_cluster_boundaries <- function(ls, locations, var) {
  clust <- ls$shapeclust # contains clusters
  loc <- ls$gis # contains locations within each cluster

  if (!is.data.frame(clust) || nrow(clust) == 0) {
    return(NULL)
  }

  # For each cluster, get the geometries in `locations` and take the union
  sf <- lapply(clust$cluster, \(x) {
    sfc <- locations |>
      filter(.data[[var]] %in% loc$loc_id[loc$cluster == x]) |>
      st_union()

    st_set_geometry(data.frame(cluster = x), sfc)
  })

  sf <- do.call(rbind, sf)

  # Add cluster label for map
  sf |>
    mutate(lbl = paste("Cluster", cluster)) |>
    relocate(geometry, .after = everything())
}

# Create a custom leaflet legend to add to the `html` arg in `addControl()`
custom_legend_row <- function(ls) {
  if ("class" %in% names(ls)) {
    icon <- paste0(
      "    <div class = '", ls$class, "'>",
      "</div>\n"
    )
  } else {
    icon <- paste0(
      "    <div style = '",
      "background: ", setmeup::color_to_css_rgba(ls$fill, ls$opac2), "; ",
      "width:20px; height:20px; ",
      "border: ", ls$wt, "px solid ",
      setmeup::color_to_css_rgba(ls$clr, ls$opac1), "; ",
      "border-radius: ", switch(ls$shp, square = "0%;", circle = "50%;"), "'>",
      "</div>\n"
    )
  }

  label <- paste0(
    "    <div style = 'padding-left: 5px;'>",
    ls$name,
    "</div>\n"
  )

  paste0(
    "  <div style = 'display: flex; align-items: center; margin: 1px 0;'>\n",
    icon,
    label,
    "  </div>\n"
  )
}

custom_legend_combine <- function(ls) {
  paste0(
    "<div style = 'line-height: 0px;'>\n",
    paste(ls, collapse = "  <br>\n"),
    "</div>"
  )
}

# Leaflet map showing study area and syndrome clusters using Satscan output
cluster_map <- function(
    cluster_boundaries,
    location_boundaries,
    kc_boundary,
    hospital_locations = NULL,
    gp,
    zoom_level
) {
  # Map center point
  center <- st_coordinates(st_centroid(st_union(location_boundaries))) |>
    as.data.frame()

  legend_rows <- lapply(gp, custom_legend_row)

  map <- leaflet(
    options = leafletOptions(scrollWheelZoom = FALSE)
  ) |>
    setView(lng = center$X, lat = center$Y, zoom = zoom_level) |>
    addProviderTiles("CartoDB.Positron") |>
    addMapPane("hospital_markers", zIndex = 420) |>
    addMapPane("cluster_outline", zIndex = 430) |>
    addMapPane("cluster_boundaries", zIndex = 440) |>
    addPolygons(
      data = location_boundaries,
      weight = gp$study$wt,
      color = gp$study$clr,
      opacity = gp$study$opac1,
      fillColor = gp$study$fill,
      fillOpacity = gp$study$opac2
    ) |>
    addPolygons(
      data = kc_boundary,
      weight = gp$kc$wt,
      color = gp$kc$clr,
      opacity = gp$kc$opac1,
      fillColor = gp$kc$fill,
      fillOpacity = gp$kc$opac2
    )

  # Add cluster regions
  if (!is.null(cluster_boundaries)) {
    map <- map |>
      addPolygons(
        data = cluster_boundaries,
        layerId = ~cluster,
        weight = gp$clust$wt,
        color = gp$clust$clr,
        opacity = gp$clust$opac1,
        fillColor = gp$clust$fill,
        fillOpacity = gp$clust$opac2,
        label = ~lbl,
        options = pathOptions(pane = "cluster_boundaries"),
        highlightOptions = highlightOptions(
          weight = 4,
          opacity = 1
        )
      )
  } else {
    legend_rows <- legend_rows[-which(names(legend_rows) == "clust")]
  }

  # Add hospital locations
  if (!is.null(hospital_locations)) {
    hospicon <- makeIcon(
      iconUrl = "www/img/transparent-square.svg",
      iconWidth = 12,
      iconHeight = 12,
      className = "plus"
    )

    map <- map |>
      addMarkers(
        data = hospital_locations,
        icon = hospicon,
        label = ~hospital_name,
        options = pathOptions(pane = "hospital_markers")
      )
  }

  # Add legend
  legend_html <- custom_legend_combine(legend_rows)

  map |>
    addControl(
      html = legend_html,
      position = "bottomright"
    )
}

add_cluster_outline <- function(map_id, data, shape_id) {
  # Remove all cluster outlines
  map <- leafletProxy(map_id) |>
    removeShape(
      layerId = data$lbl
    )

  # Add outline only if the shape ID is not NULL
  if (!is.null(shape_id)) {
    map |>
      addPolygons(
        data = data |>
          filter(cluster == shape_id),
        layerId = ~lbl,
        weight = 4,
        color = "red",
        opacity = 1,
        fill = FALSE,
        options = pathOptions(pane = "cluster_outline")
      )
  } else {
    map
  }
}

