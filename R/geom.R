#' Geometries of Colombian departments and municipalities
#'
#' Returns the polygons as an `sf` object with standard columns, so that you
#' never have to handle the geometries yourself. They are not shipped with
#' the package: the first call downloads them and caches them locally (see
#' [atlas_cache_dir()]), so later calls work offline.
#'
#' Sources, tried in this order when `source = "auto"`:
#' - `"colopendata"`: DANE's Marco Geoestadistico Nacional 2018 (simplified),
#'   via [ColOpenData::download_geospatial()]. Comes with DIVIPOLA codes.
#' - `"geoboundaries"`: geoBoundaries gbOpen COL, simplified, CC BY 4.0:
#'   departments from IGAC (2024), municipalities from DANE (2020). It has
#'   names only, so atlasr adds the DIVIPOLA codes: departments by name,
#'   municipalities by which polygon contains each municipal seat
#'   (DIVIPOLA's cabecera municipal), since municipal names repeat.
#'
#' With `source = "auto"`, a cached copy of any source is used before
#' downloading anything. Names come from DIVIPOLA whatever the source.
#'
#' Municipalities created after the source's boundaries have no polygon
#' (e.g. Belen de Bajira, 27086, in geoBoundaries' 2020 boundaries).
#'
#' @param level spatial level: `"dept"` (departments) or `"muni"`
#'   (municipalities, including the departmental corregimientos of the
#'   Amazon).
#' @param source where to get the geometries from: `"auto"`, `"colopendata"`
#'   or `"geoboundaries"`.
#' @param inset if `TRUE`, San Andres and Providencia are moved and enlarged
#'   next to the Caribbean coast, as in most printed maps of Colombia (see
#'   [inset_sanandres()]). The coordinates of the islands are then no longer
#'   real.
#' @param refresh if `TRUE`, ignore the cache and download again.
#'
#' @return an `sf` object in WGS 84 (EPSG:4326). For departments: `dept_cod`
#'   (2-character DIVIPOLA code) and `dept_nom`. For municipalities:
#'   `muni_cod` (5 characters), `muni_nom`, `dept_cod` and `dept_nom`. The
#'   attribute `"source"` says where the geometries came from.
#' @export
#'
#' @examples
#' \dontrun{
#' dept <- atlas_geom("dept")
#' muni <- atlas_geom("muni")
#' plot(sf::st_geometry(atlas_geom("dept", inset = TRUE)))
#' }
atlas_geom <- function(level = c("dept", "muni"),
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
  geom <- standardise_geom(geom, source, level)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(geom, path)
  geom
}

fetch_colopendata <- function(level) {
  spatial_level <- c(dept = "department", muni = "municipality")[[level]]
  raw <- suppressMessages(ColOpenData::download_geospatial(
    spatial_level,
    simplified = TRUE, include_geom = TRUE, include_cnpv = FALSE
  ))
  candidates <- list(
    dept = "codigo_departamento",
    muni = c("codigo_municipio", "codigo_mpio", "mpio_cdpmp")
  )[[level]]
  code_col <- intersect(candidates, names(raw))[1]
  if (is.na(code_col)) {
    stop("ColOpenData returned no code column (looked for ",
         paste(candidates, collapse = ", "), "; got ",
         paste(names(raw), collapse = ", "), ")", call. = FALSE)
  }
  codes <- pad_code(raw[[code_col]], level_spec(level)$width)
  with_names(codes, sf::st_geometry(raw), level)
}

fetch_geoboundaries <- function(level) {
  adm <- c(dept = "ADM1", muni = "ADM2")[[level]]
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
  raw <- sf::st_make_valid(sf::st_read(tmp, quiet = TRUE))

  codes <- switch(level,
    dept = gb_dept_codes$dept_cod[match(raw$shapeName, gb_dept_codes$shapeName)],
    muni = seat_codes(raw)
  )
  if (level == "dept" && anyNA(codes)) {
    stop(
      "geoBoundaries names without a DIVIPOLA code: ",
      paste(raw$shapeName[is.na(codes)], collapse = ", "),
      ". The internal lookup table (data-raw/sysdata.R) needs updating.",
      call. = FALSE
    )
  }
  keep <- !is.na(codes)
  with_names(codes[keep], sf::st_geometry(raw)[keep], level)
}

# Code of each municipality polygon: the municipal seat that falls inside it.
# When a polygon holds more than one seat (a municipality created after the
# boundaries, whose seat falls in its parent), the seat whose name matches
# wins. Polygons without a seat get NA, with a warning.
seat_codes <- function(polys) {
  seats <- sf::st_as_sf(divipola_muni, coords = c("lon", "lat"), crs = 4326)
  # planar overlay in metres: avoids s2 trouble with simplified polygons
  inside <- sf::st_within(
    sf::st_transform(seats, 3116),
    sf::st_transform(sf::st_geometry(polys), 3116)
  )
  poly_of_seat <- vapply(inside, function(i) if (length(i)) i[[1]] else NA_integer_,
                         integer(1))

  norm <- function(s) {
    tolower(gsub("[^a-z]", "", iconv(s, to = "ASCII//TRANSLIT")))
  }
  codes <- rep(NA_character_, nrow(polys))
  for (p in seq_len(nrow(polys))) {
    candidates <- which(poly_of_seat == p)
    if (length(candidates) > 1) {
      same_name <- norm(divipola_muni$muni_nom[candidates]) ==
        norm(sub(" *\\(.*$", "", polys$shapeName[p]))
      if (any(same_name)) candidates <- candidates[same_name]
    }
    if (length(candidates) > 0) {
      codes[p] <- divipola_muni$muni_cod[candidates[[1]]]
    }
  }
  if (anyNA(codes)) {
    warning(sum(is.na(codes)), " geoBoundaries municipalities contain no ",
            "municipal seat and were dropped: ",
            paste(polys$shapeName[is.na(codes)], collapse = ", "),
            call. = FALSE)
  }
  codes
}

# Standard columns, with names from DIVIPOLA so they don't depend on the
# source
with_names <- function(codes, geometry, level) {
  out <- if (level == "dept") {
    data.frame(
      dept_cod = codes,
      dept_nom = divipola_dept$dept_nom[match(codes, divipola_dept$dept_cod)]
    )
  } else {
    i <- match(codes, divipola_muni$muni_cod)
    dept_cod <- divipola_muni$dept_cod[i]
    data.frame(
      muni_cod = codes,
      muni_nom = divipola_muni$muni_nom[i],
      dept_cod = dept_cod,
      dept_nom = divipola_dept$dept_nom[match(dept_cod, divipola_dept$dept_cod)]
    )
  }
  sf::st_sf(out, geometry = geometry)
}

standardise_geom <- function(geom, source, level = "dept") {
  geom <- sf::st_transform(geom, 4326)
  geom <- sf::st_make_valid(geom)
  geom <- sf::st_cast(geom, "MULTIPOLYGON")
  geom <- geom[order(geom[[level_spec(level)$code]]), ]
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
