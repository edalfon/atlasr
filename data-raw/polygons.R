## code to prepare `polygons` dataset goes here

library(dplyr)

#' Get departments spatial polygons data
#'
#' This function reads the departments limits shape file (.shp) that was
#' originally downloaded from the IGAC web site
#' http://sigotn.igac.gov.co/sigotn/
#' Modifies the polygons for San andres y providencia to make the plot look nice
#' and saves the SpatialPolygonsDataFrame in the data of the package to make
#' available to any user
#' @return SpatialPolygonsDataFrame containing departments limits
#' @seealso rgdal::readOGR
#' @keywords shape file, rgdal
#' #@import rgdal
#' @examples
#' shapes <- get_dept_shape()
#' plot(shapes)
get_dept_shape <- function(){

  # Locate the shape files in the package
  igac_shp_dir <- here::here("data-raw/ColombiaPolygons")

  # Read the spatial polygons
  dept_polyg <- rgdal::readOGR(
    dsn = igac_shp_dir, layer = "Limite Departamental",
    encoding = "UTF-8", stringsAsFactors = FALSE
  )

  # https://stackoverflow.com/questions/18837855/making-non-ascii-data-suitable-for-cran
  Encoding(dept_polyg@data$NOMBRE_DPT) <- "latin1" # fix encoding warning

  # Bring DIVIPOLA data

  original_rownames <- row.names(dept_polyg@data) # tidyverse mess with this
  dept_polyg@data <- 	dept_polyg@data %>%
    mutate(dept_cod = daner::fix_cod_dpt(ID_ESPACIA)) %>%
    mutate(dept_cod = case_when(
      NOMBRE_DPT == "NN" ~ NA_character_,
      TRUE ~ dept_cod
    )) %>%
    left_join(daner::divipola_dept, by = c("dept_cod" = "dept_cod"))
  row.names(dept_polyg@data) <- original_rownames

  # Fix san andres y providencia
  dept_polyg <- fix_sanandres(dept_polyg)

  # Simplify it
  dept_polyg_simplified <- rgeos::gSimplify(
    dept_polyg, tol = 100, topologyPreserve=TRUE
  )

  # And restore the data, because it is lost in the simplifying step
  dept_polyg <- sp::SpatialPolygonsDataFrame(
    Sr = dept_polyg_simplified,
    data = dept_polyg@data
  )

  # Save as data for the package
  usethis::use_data(dept_polyg, overwrite = TRUE)

  # Return object
  dept_polyg
}


#' Get municipalities spatial polygons data
#'
#' This function reads the departments limits shape file (.shp) that was
#' originally downloaded from the IGAC web site
#' http://sigotn.igac.gov.co/sigotn/
#' Modifies the polygons for San andres y providencia to make the plot look nice
#' and saves the SpatialPolygonsDataFrame in the data of the package to make
#' available to any user
#' @return SpatialPolygonsDataFrame containing municipality limits
#' @seealso rgdal::readOGR
#' @keywords shape file, rgdal
#' #@import rgdal
#' @examples
#' shapes <- get_muni_shape()
#' plot(shapes)
get_muni_shape <- function(){

  # Locate the shape files in the package
  igac_shp_dir <- here::here("data-raw/ColombiaPolygons")

  # Read the spatial polygons
  muni_polyg <- rgdal::readOGR(
    dsn = igac_shp_dir, layer = "Municipios",
    encoding = "UTF-8", stringsAsFactors = FALSE, use_iconv = TRUE
  )

  # Bring DIVIPOLA data
  muni_polyg@data <- daner::join_divipola_muni_cod(muni_polyg@data, "ID_ESPACIA")

  # Fix san andres y providencia
  muni_polyg <- fix_sanandres(muni_polyg)

  # Simplify it
  muni_polyg_simplified <- rgeos::gSimplify(
    muni_polyg, tol = 100, topologyPreserve = TRUE
  )

  # And restore the data, because it is lost in the simplifying step
  muni_polyg <- sp::SpatialPolygonsDataFrame(
    Sr = muni_polyg_simplified,
    data = muni_polyg@data
  )

  # Save as data for the package
  usethis::use_data(muni_polyg, overwrite = TRUE)

  # Return object
  muni_polyg
}


fix_text <- function(x) {

  #x <- provins_polyg@data$NOM_SUBREG[14]
  x %>%
    toupper() %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all(",", "") %>%
    stringr::str_replace_all("Á", "A") %>%
    stringr::str_replace_all("É", "E") %>%
    stringr::str_replace_all("Í", "I") %>%
    stringr::str_replace_all("Ó", "O") %>%
    stringr::str_replace_all("Ú", "U") %>%
    stringr::str_replace_all("À", "A") %>%
    stringr::str_replace_all("È", "E") %>%
    stringr::str_replace_all("Ì", "I") %>%
    stringr::str_replace_all("Ò", "O") %>%
    stringr::str_replace_all("Ù", "U") %>%
    stringr::str_replace_all("Á", "A") %>%
    stringr::str_replace_all("  ", " ")
}

#' Get provinces spatial polygons data
#'
#' Subregiones__Provincias_de_Colombia
#'
#' Modifies the polygons for San andres y providencia to make the plot look nice
#' and saves the SpatialPolygonsDataFrame in the data of the package to make
#' available to any user
#'
#' @return SpatialPolygonsDataFrame containing municipality limits
#' @seealso rgdal::readOGR
#' @keywords shape file, rgdal
#' #@import rgdal
#' @examples
#' shapes <- get_muni_shape()
#' plot(shapes)
get_provins_shape_OLD <- function(){

  # Locate the shape files in the package
  igac_shp_dir <- here::here("data-raw/ColombiaPolygons")

  # Read the spatial polygons
  # yeah, again and not use the one inclued as package data, because that one
  # has been simplified and therefore, not all municipalities within a
  # province will intersect and the unify will leave traces/noise of the
  # municipalities limits within the province
  provins_polyg <- rgdal::readOGR(
    dsn = igac_shp_dir, layer = "Subregiones__Provincias_de_Colombia",
    encoding = "UTF-8", stringsAsFactors = FALSE, use_iconv = TRUE
  )

  # Fix san andres y providencia
  provins_polyg <- fix_sanandres_provins(provins_polyg)

  # Simplify it
  provins_polyg_simplified <- rgeos::gSimplify(
    spgeom = provins_polyg, tol = 0.002, topologyPreserve = TRUE
  )

  provins_polyg <- sp::SpatialPolygonsDataFrame(
    Sr = provins_polyg_simplified, data = provins_polyg@data
  )
  #mapview::mapview(provins_polyg)

  # Save as data for the package
  usethis::use_data(provins_polyg, overwrite = TRUE)

  # Return object
  provins_polyg
}


#' Get provinces spatial polygons data
#'
#' Subregiones__Provincias_de_Colombia
#'
#' This function originally attempted to read the limits shape file (.shp) that
#' was originally downloaded from
#' http://datosabiertos.esri.co/datasets/subregiones-provincias-de-colombia
#' Unfortunately, those do not have municipality data or a code I can actually
#' match with the other data which is at the municipality level. Trying to match
#' it by departament and province name did not work because it has actually less
#' provinces and a lot of names are totally different.
#'
#' So the new approach is to get the provinces data we have linked to divipola
#' and just merge the polygons of the munincipalities within each region and
#' thereby build the provinces polygons from scratch
#'
#' Modifies the polygons for San andres y providencia to make the plot look nice
#' and saves the SpatialPolygonsDataFrame in the data of the package to make
#' available to any user
#'
#' @return SpatialPolygonsDataFrame containing municipality limits
#' @seealso rgdal::readOGR
#' @keywords shape file, rgdal
#' #@import rgdal
#' @examples
#' shapes <- get_muni_shape()
#' plot(shapes)
get_provins_shape <- function(){

  # Locate the shape files in the package
  igac_shp_dir <- here::here("data-raw/ColombiaPolygons")

  # Read the spatial polygons
  # yeah, again and not use the one inclued as package data, because that one
  # has been simplified and therefore, not all municipalities within a
  # province will intersect and the unify will leave traces/noise of the
  # municipalities limits within the province
  muni_polyg <- rgdal::readOGR(
    dsn = igac_shp_dir, layer = "Municipios",
    encoding = "UTF-8", stringsAsFactors = FALSE, use_iconv = TRUE
  )

  # Fix san andres y providencia
  muni_polyg <- fix_sanandres(muni_polyg)

  # Now bring the DIVIPOLA data, to identify provinces
  muni_polyg@data <- daner::join_divipola_muni_cod(muni_polyg@data, "ID_ESPACIA")

  unify_provincia <- function(dept_cod_i, provincia_i) {

    sub_polys <- subset(muni_polyg, dept_cod == dept_cod_i & provincia == provincia_i)

    sub_polys_merged <- rgeos::gUnaryUnion(sub_polys)#, id = paste0(dept_cod_i, provincia_i))

    sub_polys_data <- muni_polyg@data %>%
      filter(dept_cod == dept_cod_i, provincia == provincia_i) %>%
      group_by(dept_cod, provincia) %>%
      summarise(
        # vars from the original polygons data
        AREA_OFICI = sum(AREA_OFICI, na.rm = TRUE),
        NOM_DEPART = efunc::most_frequent(NOM_DEPART),
        ID_ESPACIA = efunc::most_frequent(COD_DEPTO),
        COD_DEPTO = efunc::most_frequent(COD_DEPTO),
        region_nom = efunc::most_frequent(region_nom),
        dept_nom = efunc::most_frequent(dept_nom),
        long = mean(long, na.rm = TRUE),
        lat = mean(lat, na.rm = TRUE),
        region_nom = efunc::most_frequent(region_nom),
        region_cod = efunc::most_frequent(region_cod),
        dept_nom_corto = efunc::most_frequent(dept_nom_corto),
        muni_nom_corto = efunc::most_frequent(muni_nom_corto),
        # And a bunch of auxiliary test variables
        nunique_NOM_DEPART = dplyr::n_distinct(NOM_DEPART),
        nunique_COD_DEPTO = dplyr::n_distinct(COD_DEPTO),
        nunique_dept_cod = dplyr::n_distinct(dept_cod),
        nunique_dept_nom = dplyr::n_distinct(dept_nom),
        nunique_long = dplyr::n_distinct(long, na.rm = TRUE),
        nunique_lat = dplyr::n_distinct(lat, na.rm = TRUE),
        nunique_region_nom = dplyr::n_distinct(region_nom),
        nunique_region_cod = dplyr::n_distinct(region_cod),
        nunique_dept_nom_corto = dplyr::n_distinct(dept_nom_corto),
        nunique_muni_nom_corto = dplyr::n_distinct(muni_nom_corto),
      )


    assertthat::assert_that(
      sub_polys_data %>% ungroup() %>% select(starts_with("nunique_")) %>% max() == 1,
      msg = "There are distinct values within provincia groups"
    )

    sub_polys_data <- sub_polys_data %>%
      select(-starts_with("nunique_")) %>%
      as.data.frame()

    #rownames(sub_polys_data) <- paste0(dept_cod_i, provincia_i)

    sub_polys_merged <- sp::SpatialPolygonsDataFrame(
      sub_polys_merged, sub_polys_data, match.ID = FALSE
    )

    sub_polys_merged
  }
  unify_provincia <- purrr::safely(unify_provincia)


  provins <- muni_polyg@data %>%
    filter(ID_ESPACIA != "99999") %>%
    count(dept_cod, provincia)

  provins_polyg_list <- purrr::map2(provins$dept_cod, provins$provincia,
                                    unify_provincia)

  provins_polyg <- do.call(
    what = sp::rbind.SpatialPolygonsDataFrame,
    args = purrr::transpose(provins_polyg_list)$result
  )

  provins_polyg <- sp::SpatialPolygonsDataFrame(
    Sr = provins_polyg, data = provins_polyg@data, match.ID = FALSE
  )

  # Simplify it
  mapview::mapview(provins_polyg)
  provins_polyg_simplified <- rgeos::gSimplify(
    spgeom = provins_polyg, tol = 1000, topologyPreserve = TRUE
  )
  provins_polyg <- sp::SpatialPolygonsDataFrame(
    Sr = provins_polyg_simplified, data = provins_polyg@data, match.ID = FALSE
  )
  mapview::mapview(provins_polyg)

  # Save as data for the package
  usethis::use_data(provins_polyg, overwrite = TRUE)

  # Return object
  provins_polyg
}


#' Fix the size and position of san andres y providencia
#'
#' The maps downloaded from IGAC are real-life maps
#' (exact lat and long coordinates).
#' Thus, any plot will show san andres y providencia as a tiny point very far
#' from Colombia. Why?, because that's the truth!, san andres y provedencia are
#' indeed quite far from continental Colombia and its size relative to the size
#' of the country is very small.
#'
#' This function moves and scales san andres y providencia to show them closer
#' to continental Colombia and enlarge them to make those territories visible
#'
#' This function follows the tutorials in two blogs:
#' 1. https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/
#' to extract, and anti-shrink & move san andres (and reset projection)
#' Unfortunately, we cannot simply extract as easy as in the blog:
#' e.g. sanandres <- shape[shape$ID_ESPACIA=="88", ]
#' because san andres y providencia has more than one polygon,
#' so if we extract only those, in moving and zooming (expanding) to a size
#' that looks similar to the traditional maps of colombia, they get
#' proportionally separated and the distance between them in the new scale
#' would be as large as all Colombia. So, we have to
#' clip (subset) the polygons separately, based on the polygons and not on the
#' ID. To do that, use this function from this
#' blog post: http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
#'
#' @param shape an object sp::SpatialPolygonsDataFrame with the map of Colombia
#'
#' @return sp::SpatialPolygonsDataFrame with the map of Colombia, showing
#' san andres y providencia closer to continental Colombia
#'
#' #@import sp
#'
#' @examples
#' ...
fix_sanandres <- function(shape){
  # shape <- provins_polyg
  # shape <- dept_polyg
  # We need to extract (subset) sanandres y providencia apart, because we
  # need to move them in the map and enlarge them

  # sanandresyprovidencia <- shape[
  # 	shape$ID_ESPACIA == "88" |
  # 	shape$ID_ESPACIA == "88001" |
  # 	shape$ID_ESPACIA == "88564" |
  # 	shape$COD_DEPTO == "88",
  # ]

  sanandresyprovidencia <- subset(
    shape, ID_ESPACIA == "88" | ID_ESPACIA == "88001" | ID_ESPACIA == "88564"
  )
  #plot(sanandresyprovidencia)

  # But we cannot move and enlarge them together
  # (scaling them up, the space would be larger than Colombia itself)
  # So we need to clip san andres and providencia apart

  # Clip san andres, by setting one of the limits
  b <- sanandresyprovidencia@bbox #or use the function bbox
  b[1, 2] <- 180000
  sanandres <- gClip(sanandresyprovidencia, b, keep.attribs = TRUE)
  # Test to see how it loooks
  #plot(sanandres)

  # clip providencia, by setting one of the limits
  # Need the ifelse because some of the shapes are exoressed in different
  # projections/units, so the cutoff point is different
  b <- sanandresyprovidencia@bbox #or use the function bbox
  b[1, 1] <- 180000
  providencia <- gClip(sanandresyprovidencia, b, keep.attribs = TRUE)
  # Test to see how it loooks
  #plot(providencia)

  # Now we need to enlarge (anti-shrink) each of them and position them closer
  # to continental Colombia. Scale them similarly, but move them as usually
  # presented in Colombian maps that include san andres

  sanandres <- maptools::elide(sanandres, scale = max(apply(sp::bbox(sanandres), 1, diff)) / 0.077)
  sanandres <- maptools::elide(sanandres, shift=c(+700000, +1700000))
  sp::proj4string(sanandres) <- sp::proj4string(shape)

  providencia <- maptools::elide(providencia, scale=max(apply(sp::bbox(providencia), 1, diff)) / 0.077)
  providencia <- maptools::elide(providencia, shift=c(+777000, +1770000))
  sp::proj4string(providencia) <- sp::proj4string(shape)

  # Remove the original polygons for san andres & providencia
  # shape <- shape[!(shape$ID_ESPACIA == "88" |
  # 				 	shape$ID_ESPACIA == "88001" |
  # 				 	shape$ID_ESPACIA == "88564"), ]
  shape <- subset(
    shape, !(ID_ESPACIA == "88" | ID_ESPACIA == "88001" | ID_ESPACIA == "88564")
  )


  # And bind the modified polygons together
  shape <- rbind(shape, providencia, sanandres, makeUniqueIDs = TRUE)
  #plot(shape)

  shape
}



fix_sanandres_provins <- function(shape){
  # shape <- provinces_polyg
  # shape <- dept_polyg
  # We need to extract (subset) sanandres y providencia apart, because we
  # need to move them in the map and enlarge them

  sanandresyprovidencia <- subset(shape, COD_DEPTO == "88")

  #plot(sanandresyprovidencia)

  # But we cannot move and enlarge them together
  # (scaling them up, the space would be larger than Colombia itself)
  # So we need to clip san andres and providencia apart

  # Clip san andres, by setting one of the limits
  b <- sanandresyprovidencia@bbox #or use the function bbox
  b[1, 2] <- -81.60661
  sanandres <- gClip(sanandresyprovidencia, b, keep.attribs = TRUE)
  #plot(sanandres) # Test to see how it loooks

  # clip providencia, by setting one of the limits
  # Need the ifelse because some of the shapes are exoressed in different
  # projections/units, so the cutoff point is different
  b <- sanandresyprovidencia@bbox #or use the function bbox
  b[1, 1] <- -81.60661
  providencia <- gClip(sanandresyprovidencia, b, keep.attribs = TRUE)
  #plot(providencia) # Test to see how it loooks

  # Now we need to enlarge (anti-shrink) each of them and position them closer
  # to continental Colombia. Scale them similarly, but move them as usually
  # presented in Colombian maps that include san andres
  k <- (180000/-81.60661)
  700000/k
  sanandres <- maptools::elide(sanandres, scale = max(apply(sp::bbox(sanandres), 1, diff)) / 0.077)
  sanandres <- maptools::elide(sanandres, shift=c(-77, +11))
  sp::proj4string(sanandres) <- sp::proj4string(shape)

  providencia <- maptools::elide(providencia, scale=max(apply(sp::bbox(providencia), 1, diff)) / 0.077)
  providencia <- maptools::elide(providencia, shift=c(-76.3, +11.7))
  sp::proj4string(providencia) <- sp::proj4string(shape)

  shape <- subset(shape, COD_DEPTO != "88")

  # And bind the modified polygons together
  shape <- rbind(shape, providencia, sanandres, makeUniqueIDs = TRUE)
  #plot(shape)

  shape
}


#' clip some points of a polygon apart
#'
#' See: http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
#' @param shp shp file
#' @param bb box to clip
#' @param keep.attribs keep attributes
#'
#' @return
#'
#' #@import sp raster rgeos
#'
#' @examples
#' ...
gClip <- function(shp, bb, keep.attribs=TRUE) {
  # shp <- sanandresyprovidencia
  # bb <- b
  if (class(bb) == "matrix") {
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  } else {
    b_poly <- as(raster::extent(bb), "SpatialPolygons")
  }

  new.shape <- rgeos::gIntersection(shp, b_poly, byid = T)
  if (keep.attribs){
    new.attribs <- data.frame(
      do.call(rbind, strsplit(row.names(new.shape), " ")),
      stringsAsFactors = FALSE
    )
    new.attrib.data <- shp[new.attribs$X1, ]@data
    row.names(new.shape) <- row.names(new.attrib.data)
    return(sp::SpatialPolygonsDataFrame(new.shape, new.attrib.data))
  } else {
    return(new.shape)
  }
}




#' clip some points of a polygon apart
#'
#' See: http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
#' @param shp shp file
#' @param bb box to clip
#' @param keep.attribs keep attributes
#'
#' @return
#'
#' #@import sp raster rgeos
#'
#' @examples
#' ...
test_plot <- function(){

  ggplot(data = dept_polyg,
         aes(x=long, y=lat, group = group)) +
    geom_polygon()  +
    coord_equal() +
    geom_path(color = "white") +
    theme(axis.title = element_blank(), axis.text = element_blank())

  ggplot(data = muni_polyg,
         aes(x=long, y=lat, group = group)) +
    geom_polygon()  +
    coord_equal() +
    geom_path(color = "white") +
    theme(axis.title = element_blank(), axis.text = element_blank())

  library(sp)
  plot(dept_polyg)
}






# how much of a sacrilege is this approach to documenting data in a package? How
# about deviating from the "canonical" advice (https://r-pkgs.org/data.html)
# -also hinted by `usethis::use_data`- and put the code to generate the data in
# the R folder and document it using `roxygen2` syntax, but using `@aliases
# mycooldata` to trick it into documenting
#
#
# I have a couple of internal packages, whose main purpose is to share data and
# some utility functions. We have been following the "canonical" advice
# (https://r-pkgs.org/data.html) and we keep in the `data-raw` folder the raw
# data and the scripts to process it and produce the final datasets to include
# in the package. Usually, we have a function that in the end calls
# `usethis::use_data` to save the package data. Then we write the package
# documentation as hinted by `usethis::use_data` and explained in more detail in
# the R Packages book linked above.
#
# I've always found that the last part (documenting the data) far from ideal and
# felt it could and should be automated further. I am not alone, of course. This
# has been discussed here and in some Github issues
# ([link](https://community.rstudio.com/t/template-for-documenting-a-dataset/5365),
# [link](https://github.com/r-lib/usethis/issues/274),
# [link](https://github.com/r-lib/usethis/issues/801),
# [link](https://github.com/r-lib/usethis/pull/1086)). Discussions concluded
# that something could probably be implemented in `roxygen2`, but AFAIK, there
# are no new features related to this.





# https://stackoverflow.com/questions/18923405/possible-to-create-rd-help-files-for-objects-not-in-a-package
# https://r-pkgs.org/data.html
#
#
# sourcefile <- rstudioapi::getActiveDocumentContext()$path
# roc_env <- roxygen2::env_file(sourcefile)
# #roc_env <- roxygen2::env_package(".")
# rd_blocks = roxygen2::parse_file(sourcefile, roc_env)
# help_topics = roxygen2::roclet_process(roxygen2::rd_roclet(), rd_blocks, roc_env, dirname(sourcefile))
#
# roxygen2::roclet_output(roxygen2::rd_roclet(), help_topics, ".", options=list(wrap=FALSE), check = FALSE)
#
#

# toysdf <- function() {
#
#   roxygen2::roxygenise(load_code = function(path = ".") {
#     print("jhdsf")
#     roc_env <- roxygen2::load_pkgload(".")
#     sourcefile <- rstudioapi::getActiveDocumentContext()$path
#     file_env <- roxygen2::env_file(sourcefile)
#     #rlang::env_bind(roc_env, file_env$fix_sanandres)
#     rm(.packageName, envir = file_env)
#     for(n in ls(file_env, all.names=TRUE)) assign(n, get(n, file_env), roc_env)
#     #roc_env <- source("data-raw/polygons.R", roc_env)
#     rlang::env_print(roc_env)
#     roc_env
#   })
#
# }

