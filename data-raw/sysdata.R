# Internal lookup tables (R/sysdata.rda). Names, codes and points only: no
# geometry is shipped with atlasr.
#
# - divipola_dept: department codes and short names.
# - divipola_muni: municipality codes, names, department, and the location of
#   the municipal seat (cabecera municipal). geoBoundaries' municipalities
#   have neither codes nor departments, and many names repeat (several "San
#   Rafael"), so atlasr gives each polygon the code of the seat that falls
#   inside it. Used for ColOpenData's names too, so names are the same
#   whatever the source.
# - gb_dept_codes: geoBoundaries department names -> DIVIPOLA codes.
#
# Source: DANE's DIVIPOLA via daner (github.com/edalfon/daner). Re-run when
# DIVIPOLA or geoBoundaries change.

library(sf)

# Title case for DIVIPOLA's upper-case names: "SAN JOSE DEL GUAVIARE" ->
# "San José del Guaviare"
title_case <- function(s) {
  s <- tolower(s)
  s <- gsub("(^|[ (\"/-])([[:alpha:]])", "\\1\\U\\2", s, perl = TRUE)
  gsub(" (De|Del|Y|E|La|Las|Los|El)(?= )", " \\L\\1", s, perl = TRUE)
}

norm <- function(s) {
  tolower(gsub("[^a-z ]", "", iconv(s, to = "ASCII//TRANSLIT")))
}

# Departments ---------------------------------------------------------------

divipola_dept <- data.frame(
  dept_cod = daner::divipola_dept$dept_cod,
  dept_nom = daner::divipola_dept$dept_nom_corto
)
divipola_dept <- divipola_dept[order(divipola_dept$dept_cod), ]
row.names(divipola_dept) <- NULL

# Municipalities --------------------------------------------------------------

seats <- daner::divipola_muni
seats <- seats[grepl("^CABECERA", seats$tipo_centro_poblado), ]
divipola_muni <- data.frame(
  muni_cod = seats$muni_cod,
  muni_nom = title_case(seats$muni_nom),
  dept_cod = seats$dept_cod,
  lon = seats$long,
  lat = seats$lat
)
divipola_muni$muni_nom[divipola_muni$muni_cod == "11001"] <- "Bogotá, D.C."
divipola_muni <- divipola_muni[order(divipola_muni$muni_cod), ]
row.names(divipola_muni) <- NULL
stopifnot(
  !anyDuplicated(divipola_muni$muni_cod),
  !anyNA(divipola_muni[c("lon", "lat")]),
  all(divipola_muni$dept_cod %in% divipola_dept$dept_cod)
)

# geoBoundaries department names -> codes -----------------------------------

gb_url <- function(adm) {
  api <- paste0("https://www.geoboundaries.org/api/current/gbOpen/COL/", adm, "/")
  meta <- paste(readLines(api, warn = FALSE), collapse = "")
  sub('^.*"simplifiedGeometryGeoJSON": *"([^"]+)".*$', "\\1", meta)
}
gb <- sf::st_read(gb_url("ADM1"), quiet = TRUE)

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
  setequal(gb_dept_codes$dept_cod, divipola_dept$dept_cod)
)
gb_dept_codes <- gb_dept_codes[order(gb_dept_codes$dept_cod), ]
row.names(gb_dept_codes) <- NULL

usethis::use_data(
  divipola_dept, divipola_muni, gb_dept_codes,
  internal = TRUE, overwrite = TRUE
)
