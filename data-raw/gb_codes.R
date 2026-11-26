# Lookup table from geoBoundaries department names to DIVIPOLA codes.
#
# geoBoundaries (gbOpen COL ADM1) has names but no DIVIPOLA codes, so atlasr
# ships this small table (names and codes only, no geometry) to add them.
# Re-run when geoBoundaries publishes a new version; atlas_geom() stops with
# an error naming any department it can't find here.

gb_api <- "https://www.geoboundaries.org/api/current/gbOpen/COL/ADM1/"
meta <- paste(readLines(gb_api, warn = FALSE), collapse = "")
url <- sub('^.*"simplifiedGeometryGeoJSON": *"([^"]+)".*$', "\\1", meta)
gb <- sf::st_read(url, quiet = TRUE)

norm <- function(s) {
  tolower(gsub("[^a-z ]", "", iconv(s, to = "ASCII//TRANSLIT")))
}

divipola <- daner::divipola_dept
gb_dept_codes <- data.frame(shapeName = gb$shapeName)
key <- norm(gb_dept_codes$shapeName)
gb_dept_codes$dept_cod <- divipola$dept_cod[match(key, norm(divipola$dept_nom))]
i <- is.na(gb_dept_codes$dept_cod)
gb_dept_codes$dept_cod[i] <-
  divipola$dept_cod[match(key[i], norm(divipola$dept_nom_corto))]

# names that don't match DIVIPOLA's spelling
manual <- c(
  "Distrito Capital de Bogotá" = "11",
  "San Andrés, Providencia y Santa Catalina" = "88"
)
i <- gb_dept_codes$shapeName %in% names(manual)
gb_dept_codes$dept_cod[i] <- manual[gb_dept_codes$shapeName[i]]

stopifnot(
  !anyNA(gb_dept_codes$dept_cod),
  !anyDuplicated(gb_dept_codes$dept_cod),
  setequal(gb_dept_codes$dept_cod, divipola$dept_cod)
)
gb_dept_codes <- gb_dept_codes[order(gb_dept_codes$dept_cod), ]
row.names(gb_dept_codes) <- NULL

usethis::use_data(gb_dept_codes, internal = TRUE, overwrite = TRUE)
