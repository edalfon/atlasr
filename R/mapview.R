#' Plot an interactive choropleth map of Colombia, colored by department
#'
#' You pass the data you want to plot, the variable to use for merging the
#'
#'
#' An interactive map powered by `mapview`.
#'
#'
#' @param plot_data a data.frame-like containing join_var and zcol
#' @param join_var character vector length 1, with the name of the variable
#' to be used to join departments
#' @param zcol `zcol` as in mapview::mapview. But in this case, zcol can be a
#' named vector, in which case it will be used as layer.name. If zcol has no
#' names, it will check if the zcol has a label attribute to use it as layer
#' name. If none of that works, it simply uses zcol as layer name.
#' @param discretize character of length 1, specifying how to discretize zcol
#' continuous variable. It must be:
#' - "auto" to use `mapview`'s default way to discretize and assign colors
#' - any of efun::break_methods() to specify which method to use.
#' See ?efun::break_methods
#' @param factor_discretize boolean indicating if a hard discretization
#' should be used, by converting zcol to a factor.
#' - FALSE, the default, calculate the breaks using efun::get_breaks and pass
#'   them to `mapview` using the `at` argument.
#' - TRUE uses efun::discretize to convert zcol to a factor, which could be
#'   useful if you want to customize the labels of the breaks
#' @param n.breaks number of breaks (passed to efun::get_breaks discretize)
#' @param ... further arguments passed to efun::discretize
#' @param tooltip either "auto" (the default) or a character vector:
#' - "auto" put as tooltip the name of the department, followed by the name and
#'   value of the zcol variable(s)
#' - other character vector to be processed by glue::glue using the data as
#'   environment, so you can put the values of the variables you want using the
#'   syntax {varname}
#' @param map.types see ?mapview::mapview
#' @param col.regions see ?mapview::mapview
#' @param popup see ?mapview::mapview
#' @param homebutton see ?mapview::mapview
#'
#' @import sp rgeos
#'
#' @return a `mapview` object
#' @export
#' @md
#'
#' @examples
#' mapview_dept()
#' mapview_dept(discretize = "jenks")
#' mapview_dept(discretize = "jenks", n.breaks = 3)
#'
#' # tooltips
#' mapview_dept(tooltip = "auto")
#' mapview_dept(tooltip = "This is pob: {pob_total}")



mapview_atlasr <- function(polyg,
                           plot_data = daner::pob_2018_dept,
                           join_var = "dept_cod",
                         zcol = c(Population = "pob_total"),
                         discretize = c("auto", efun::break_methods()),
                         factor_discretize = FALSE,
                         n.breaks = 5,
                         ...,
                         map.types = mapview::mapviewGetOption("basemaps"),
                         col.regions = mapview::mapviewGetOption("vector.palette"),
                         tooltip = "auto",
                         popup = TRUE,
                         homebutton = TRUE) {

  . <- NULL

  discretize <- match.arg(discretize)
  vctrs::vec_assert(tooltip, ptype = character())

  # Obtain polyons ####


  # set layer.name, that appears on the legend and layer selector ####
  # priority to zcol names, then var labels and finally varname
  zcol_names <- rlang::names2(zcol) %>% replace(. == "", NA)
  zcol_labels <- purrr::map_chr(zcol, ~ ifelse(
    test = !is.null(attr(plot_data[[.x]], "label")),
    yes = attr(plot_data[[.x]], "label"),
    no = .x
  ))
  layer.name <- dplyr::coalesce(zcol_names, zcol_labels)

  # discretize ####
  at <- NULL
  if (discretize != "auto") {
    if (isTRUE(factor_discretize)) {
      polyg@data[[zcol]] <- efun::discretize(
        polyg@data[[zcol]], n.breaks, discretize, ...
      )
    } else {
      at <- efun::get_breaks(polyg@data[[zcol]], n.breaks, discretize)
    }
  }

  # tooltip ####
  # here some notes on how to make it multiline: the key is to use HTML function
  # https://github.com/rstudio/leaflet/issues/412
  # https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
  if (identical(tooltip, "auto")) {
    tooltip <- paste0(
      "<b>", polyg@data$area_name, "</b><br/>",
      layer.name, ": ", scales::number(polyg@data[[zcol]], big.mark = ",")
    )
    tooltip <- lapply(tooltip, htmltools::HTML)
  } else {
    tooltip <- glue::glue(tooltip, .envir = polyg@data)
    tooltip <- lapply(tooltip, htmltools::HTML)
  }

  # mapview it, finally ####
  m <- mapview::mapview(
    x = polyg,
    map.types = map.types,
    zcol = zcol,
    at = at,
    col.regions = col.regions,
    #na.color = "NA", # mapview::mapviewGetOption("na.color"),
    hide = TRUE, # hide all layers but the first
    burst = FALSE,
    layer.name = layer.name,
    label = tooltip,
    popup = popup, # TODO: make a cool table as in positiva
    homebutton = homebutton
  ) %>% leafem::removeMouseCoordinates()

  m
}

#' @rdname mapview_atlasr
#' @export
mapview_dept <- function(plot_data = daner::pob_2018_dept,
                         join_var = "dept_cod",
                         zcol = c(Population = "pob_total"),
                         discretize = c("auto", efun::break_methods()),
                         factor_discretize = FALSE,
                         n.breaks = 5,
                         ...,
                         map.types = mapview::mapviewGetOption("basemaps"),
                         col.regions = mapview::mapviewGetOption("vector.palette"),
                         tooltip = "auto",
                         popup = TRUE,
                         homebutton = TRUE) {

  polyg <- join_dept_polyg_data(plot_data, join_var)

  mapview_atlasr(
    polyg, plot_data, join_var, zcol, discretize, factor_discretize, n.breaks, ...,
    map.types, col.regions = col.regions, tooltip, popup, homebutton
  )

}

#' @rdname mapview_atlasr
#' @export
mapview_muni <- function(plot_data = daner::pob_2018_muni,
                         join_var = "muni_cod",
                         zcol = c(Population = "pob_total"),
                         discretize = c("auto", efun::break_methods()),
                         factor_discretize = FALSE,
                         n.breaks = 5,
                         ...,
                         map.types = mapview::mapviewGetOption("basemaps"),
                         col.regions = mapview::mapviewGetOption("vector.palette"),
                         tooltip = "auto",
                         popup = TRUE,
                         homebutton = TRUE) {

  polyg <- join_muni_polyg_data(plot_data, join_var)

  mapview_atlasr(
    polyg, plot_data, join_var, zcol, discretize, factor_discretize, n.breaks, ...,
    map.types = map.types, col.regions = col.regions, tooltip, popup, homebutton
  )

}




join_dept_polyg_data <- function(plot_data = daner::pob_2018_dept,
                                 join_var = "dept_cod") {

  polyg <- atlasr::dept_polyg
  polyg <- polyg[polyg$ID_ESPACIA != "999", ]

  polyg@data <- polyg@data %>% # TODO: check overlapping names
    dplyr::mutate(area_name = dept_nom_corto) %>%
    # TODO: let also join by name
    dplyr::left_join(plot_data, by = c("dept_cod" = join_var))

  polyg
}

join_muni_polyg_data <- function(plot_data = daner::pob_2018_muni,
                                 join_var = "muni_cod") {

  polyg <- atlasr::muni_polyg
  polyg <- polyg[polyg$ID_ESPACIA != "99999", ]

  polyg@data <- polyg@data %>% # TODO: check overlapping names
    dplyr::mutate(area_name = muni_nom_corto) %>%
    # TODO: let also join by name
    dplyr::left_join(plot_data, by = c("muni_cod" = join_var))

  polyg
}
