#' Geometries of Colombian departments
#'
#' Returns the department polygons as an `sf` object with standard columns,
#' so that you never have to handle the geometries yourself. They are not
#' shipped with the package: the first call downloads them and caches them
#' locally (see [atlas_cache_dir()]), so later calls work offline.
#'
#' Sources, tried in this order when `source = "auto"`:
#' - `"colopendata"`: DANE's Marco Geoestadistico Nacional 2018 (simplified),
#'   via [ColOpenData::download_geospatial()]. Comes with DIVIPOLA codes.
#' - `"geoboundaries"`: geoBoundaries gbOpen COL ADM1 (IGAC 2024, simplified,
#'   CC BY 4.0). It has names only, so atlasr adds the DIVIPOLA codes from an
#'   internal lookup table.
#'
#' With `source = "auto"`, a cached copy of any source is used before
#' downloading anything.
#'
#' @param level spatial level. Only `"dept"` (departments) for now.
#' @param source where to get the geometries from: `"auto"`, `"colopendata"`
#'   or `"geoboundaries"`.
#' @param inset if `TRUE`, San Andres and Providencia are moved and enlarged
#'   next to the Caribbean coast, as in most printed maps of Colombia (see
#'   [inset_sanandres()]). Use it for static maps only: the coordinates of the
#'   islands are no longer real.
#' @param refresh if `TRUE`, ignore the cache and download again.
#'
#' @return an `sf` object in WGS 84 (EPSG:4326) with columns `dept_cod`
#'   (2-character DIVIPOLA code), `dept_nom` and `geometry`. The attribute
#'   `"source"` says where it came from.
#' @export
#'
#' @examples
#' \dontrun{
#' dept <- atlas_geom("dept")
#' plot(sf::st_geometry(atlas_geom("dept", inset = TRUE)))
#' }
atlas_geom <- function(level = "dept",
                       source = c("auto", "colopendata", "geoboundaries"),
                       inset = FALSE,
                       refresh = FALSE) {
  level <- match.arg(level)
  source <- match.arg(source)

  geom <- if (source == "auto") {
    geom_auto(level, refresh)
  } else {
    geom_cached(level, source, refresh)
  }

  if (isTRUE(inset)) {
    geom <- inset_sanandres(geom)
  }
  geom
}

geom_sources <- c("colopendata", "geoboundaries")

geom_auto <- function(level, refresh) {
  if (!isTRUE(refresh)) {
    for (src in geom_sources) {
      path <- cache_path(level, src)
      if (file.exists(path)) return(readRDS(path))
    }
  }
  errors <- character()
  for (src in geom_sources) {
    # hold warnings back: a failed source also warns (e.g. GDAL's HTTP error),
    # which would only repeat the error; a working source's warnings are kept
    warnings <- list()
    geom <- tryCatch(
      withCallingHandlers(
        geom_cached(level, src, refresh = TRUE),
        warning = function(w) {
          warnings[[length(warnings) + 1]] <<- w
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        errors[[src]] <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(geom)) {
      for (w in warnings) warning(w)
      if (length(errors) > 0) {
        message(
          "atlasr: ", names(errors), " unavailable (", errors, "); used ",
          src, " instead."
        )
      }
      return(geom)
    }
  }
  stop(
    "Could not download the ", level, " geometries from any source, ",
    "and there is no cached copy.\n",
    paste0("- ", names(errors), ": ", errors, collapse = "\n"),
    call. = FALSE
  )
}

geom_cached <- function(level, source, refresh) {
  path <- cache_path(level, source)
  if (!isTRUE(refresh) && file.exists(path)) {
    return(readRDS(path))
  }
  fetch <- switch(source,
    colopendata = fetch_colopendata,
    geoboundaries = fetch_geoboundaries
  )
  geom <- fetch(level)
  geom <- standardise_geom(geom, source)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(geom, path)
  geom
}

fetch_colopendata <- function(level) {
  spatial_level <- c(dept = "department")[[level]]
  raw <- suppressMessages(ColOpenData::download_geospatial(
    spatial_level,
    simplified = TRUE, include_geom = TRUE, include_cnpv = FALSE
  ))
  out <- data.frame(
    dept_cod = pad_code(raw$codigo_departamento, 2),
    dept_nom = raw$departamento
  )
  sf::st_sf(out, geometry = sf::st_geometry(raw))
}

fetch_geoboundaries <- function(level) {
  adm <- c(dept = "ADM1")[[level]]
  api <- paste0("https://www.geoboundaries.org/api/current/gbOpen/COL/", adm, "/")
  meta <- paste(readLines(api, warn = FALSE), collapse = "")
  url <- regmatches(
    meta, regexpr('"simplifiedGeometryGeoJSON": *"[^"]+"', meta)
  )
  if (length(url) == 0) {
    stop("geoBoundaries API did not return a download URL", call. = FALSE)
  }
  url <- sub('^.*: *"([^"]+)"$', "\\1", url)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  raw <- sf::st_read(tmp, quiet = TRUE)

  codes <- gb_dept_codes$dept_cod[match(raw$shapeName, gb_dept_codes$shapeName)]
  if (anyNA(codes)) {
    stop(
      "geoBoundaries names without a DIVIPOLA code: ",
      paste(raw$shapeName[is.na(codes)], collapse = ", "),
      ". The internal lookup table (data-raw/gb_codes.R) needs updating.",
      call. = FALSE
    )
  }
  out <- data.frame(dept_cod = codes, dept_nom = raw$shapeName)
  sf::st_sf(out, geometry = sf::st_geometry(raw))
}

standardise_geom <- function(geom, source) {
  geom <- sf::st_transform(geom, 4326)
  geom <- sf::st_make_valid(geom)
  geom <- geom[order(geom$dept_cod), ]
  row.names(geom) <- NULL
  attr(geom, "source") <- source
  geom
}

#' Where atlasr caches the geometries
#'
#' `atlas_cache_dir()` returns the cache folder; `atlas_cache_clear()` deletes
#' the cached geometries, so the next call to [atlas_geom()] downloads them
#' again.
#'
#' @return the path to the cache folder (invisibly for `atlas_cache_clear()`).
#' @export
atlas_cache_dir <- function() {
  getOption("atlasr.cache_dir", tools::R_user_dir("atlasr", "cache"))
}

#' @rdname atlas_cache_dir
#' @export
atlas_cache_clear <- function() {
  files <- list.files(atlas_cache_dir(), pattern = "\\.rds$", full.names = TRUE)
  unlink(files)
  invisible(atlas_cache_dir())
}

cache_path <- function(level, source) {
  file.path(atlas_cache_dir(), paste0(level, "_", source, ".rds"))
}
