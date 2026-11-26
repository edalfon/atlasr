#' Map department data
#'
#' Pass a data frame with one row per department and a DIVIPOLA code, say
#' which variable(s) to map, and get a choropleth map. atlasr finds the
#' geometries (see [atlas_geom()]) and joins them for you.
#'
#' - `map_dept()` draws an interactive map with `mapview` (leaflet), on the
#'   real geometry. Several `zcol` give several layers, one shown at a time.
#' - `map_dept_static()` returns a `ggplot` (so you can add to it), with San
#'   Andres and Providencia moved next to the mainland by default.
#'
#' @param plot_data a data frame with `join_var` and the `zcol` variable(s).
#' @param zcol the variable(s) to map. Names, if given, are used as the
#'   legend/layer title (e.g. `c(Population = "pob_total")`); otherwise the
#'   variable's `label` attribute, if any, or its name. `map_dept_static()`
#'   takes one variable only.
#' @param join_var name of the column in `plot_data` with the department
#'   code. Codes can be character or numeric: `5`, `"5"` and `"05"` all match
#'   Antioquia. A national-total row (code `"00"`) is ignored.
#' @param discretize how to bin the values: `"none"` (continuous colours; for
#'   `map_dept()` that's mapview's own binning), `"jenks"` (natural breaks),
#'   `"quantile"` or `"equal"` (equal width).
#' @param n_breaks number of classes when binning.
#' @param tooltip `"auto"` (department name and value) or a [glue::glue()]
#'   template evaluated on the joined data, e.g.
#'   `"{dept_nom}: {scales::percent(rate)}"`.
#' @param col.regions colour palette function, as in [mapview::mapview()].
#' @param ... further arguments passed to [mapview::mapview()].
#' @param source,refresh passed to [atlas_geom()].
#'
#' @return `map_dept()`: a `mapview` object. `map_dept_static()`: a `ggplot`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(dept_cod = c("05", "11", "76"), rate = c(0.4, 0.5, 0.6))
#' map_dept(df, zcol = c(`C-section rate` = "rate"))
#' map_dept_static(df, zcol = "rate", discretize = "quantile", n_breaks = 3)
#' }
map_dept <- function(plot_data,
                     zcol,
                     join_var = "dept_cod",
                     discretize = c("none", "jenks", "quantile", "equal"),
                     n_breaks = 5,
                     tooltip = "auto",
                     col.regions = mapview::mapviewGetOption("vector.palette"),
                     ...,
                     source = "auto",
                     refresh = FALSE) {
  discretize <- match.arg(discretize)
  stopifnot(is.character(tooltip), length(tooltip) == 1)

  geom <- atlas_geom("dept", source = source, inset = FALSE, refresh = refresh)
  geom <- join_data(geom, plot_data, join_var, zcol)
  layer_names <- layer_names(plot_data, zcol)

  maps <- lapply(seq_along(zcol), function(i) {
    var <- unname(zcol[[i]])
    at <- if (discretize == "none") NULL else {
      get_breaks(geom[[var]], n_breaks, discretize)
    }
    mapview::mapview(
      geom,
      zcol = var,
      at = at,
      col.regions = col.regions,
      layer.name = layer_names[[i]],
      label = make_tooltip(geom, var, layer_names[[i]], tooltip),
      hide = i > 1,
      ...
    )
  })
  m <- Reduce(`+`, maps)
  # keep the mapview object (so more layers can be added with `+`)
  m@map <- leafem::removeMouseCoordinates(m@map)
  m
}

#' @rdname map_dept
#' @param inset move and enlarge San Andres and Providencia next to the
#'   mainland (see [inset_sanandres()]).
#' @param palette for `map_dept_static()`, a viridis option (`"viridis"`,
#'   `"magma"`, `"cividis"`, ...).
#' @export
map_dept_static <- function(plot_data,
                            zcol,
                            join_var = "dept_cod",
                            discretize = c("none", "jenks", "quantile", "equal"),
                            n_breaks = 5,
                            inset = TRUE,
                            palette = "viridis",
                            source = "auto",
                            refresh = FALSE) {
  discretize <- match.arg(discretize)
  if (length(zcol) != 1) {
    stop("`map_dept_static()` maps one variable; `zcol` has ", length(zcol),
         ".", call. = FALSE)
  }

  geom <- atlas_geom("dept", source = source, inset = inset, refresh = refresh)
  geom <- join_data(geom, plot_data, join_var, zcol)
  var <- unname(zcol[[1]])
  title <- layer_names(plot_data, zcol)[[1]]

  if (discretize == "none") {
    fill <- ggplot2::aes(fill = .data[[var]])
    scale <- ggplot2::scale_fill_viridis_c(
      option = palette, name = title, na.value = "grey85"
    )
  } else {
    breaks <- get_breaks(geom[[var]], n_breaks, discretize)
    geom$.class <- cut(geom[[var]], breaks, include.lowest = TRUE,
                       labels = break_labels(breaks))
    fill <- ggplot2::aes(fill = .data[[".class"]])
    scale <- ggplot2::scale_fill_viridis_d(
      option = palette, name = title, na.value = "grey85", drop = FALSE
    )
  }

  ggplot2::ggplot(geom) +
    ggplot2::geom_sf(fill, colour = "white", linewidth = 0.15) +
    scale +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_void()
}
