#' Move and enlarge San Andres and Providencia next to the mainland
#'
#' At their real position and size, San Andres and Providencia are a couple
#' of dots far out in the Caribbean. This moves each island group next to the
#' Caribbean coast and enlarges it about 13 times, as most printed maps of
#' Colombia do. San Andres and Providencia (with Santa Catalina) are moved
#' separately: enlarged together, the sea between them would be as wide as
#' the country.
#'
#' The placement follows the original atlasr (2018): in MAGNA-SIRGAS /
#' Colombia Bogota (EPSG:3116), each group is scaled by `1 / 0.077` from the
#' lower-left corner of its bounding box, which is then placed at
#' (700000, 1700000) for San Andres and (777000, 1770000) for Providencia.
#'
#' Only for static maps: the coordinates of the islands are no longer real,
#' so on an interactive map with a basemap they would float over the sea.
#'
#' @param geom an `sf` object from [atlas_geom()] (with a `dept_cod` column).
#'
#' @return `geom` with the geometry of department `"88"` replaced, in the
#'   CRS of `geom`.
#' @export
inset_sanandres <- function(geom) {
  is_88 <- geom$dept_cod %in% "88"
  if (!any(is_88)) return(geom)

  # only the islands go through EPSG:3116; the rest stays untouched
  all_geom <- sf::st_geometry(geom)
  local <- sf::st_transform(all_geom[is_88], 3116)

  # one element per island polygon, remembering which feature it came from
  pieces <- lapply(seq_along(local), function(i) {
    sf::st_cast(sf::st_cast(local[i], "MULTIPOLYGON"), "POLYGON")
  })
  feature <- rep(seq_along(local), lengths(pieces))
  island <- do.call(c, pieces)

  # San Andres lies west of Providencia; split at the original cut-off
  x_centre <- sf::st_coordinates(sf::st_centroid(island))[, "X"]
  group <- ifelse(x_centre < 180000, "sanandres", "providencia")

  for (g in unique(group)) {
    sel <- group == g
    island[sel] <- move_scale(island[sel], inset_params[[g]])
  }
  sf::st_crs(island) <- 3116

  moved <- lapply(seq_along(local), function(i) {
    sf::st_cast(sf::st_combine(island[feature == i]), "MULTIPOLYGON")
  })
  moved <- sf::st_transform(do.call(c, moved), sf::st_crs(all_geom))
  all_geom[is_88] <- moved
  sf::st_geometry(geom) <- all_geom
  geom
}

inset_params <- list(
  sanandres = list(scale = 1 / 0.077, origin = c(700000, 1700000)),
  providencia = list(scale = 1 / 0.077, origin = c(777000, 1770000))
)

# scale a set of geometries from the lower-left corner of their joint bounding
# box, then place that corner at `origin`
move_scale <- function(x, params) {
  bb <- sf::st_bbox(x)
  corner <- c(bb[["xmin"]], bb[["ymin"]])
  x <- sf::st_set_crs(x, NA)
  (x - corner) * params$scale + params$origin
}
