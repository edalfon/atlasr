#' Map department data
#'
#' Pass a data frame with one row per department and a DIVIPOLA code, say
#' which variable(s) to map, and get a choropleth map. atlasr finds the
#' geometries (see [atlas_geom()]) and joins them for you.
#'
#' - `map_dept()` draws an interactive map with `mapview` (leaflet) over an
#'   Esri basemap. Several `zcol` give several layers, one shown at a time.
#' - `map_dept_static()` returns a `ggplot`, so you can keep adding to it.
#'
#' Both take the same arguments and bin the values the same way, so the same
#' call gives the same classes and colours on either map.
#'
#' Both move and enlarge San Andres and Providencia next to the mainland by
#' default (`inset = TRUE`), so that their colour is visible at country
#' scale. On the interactive map they then sit off the Caribbean coast, over
#' the sea of the basemap; their tooltip still says which department they
#' are. Use `inset = FALSE` for the real geometry.
#'
#' @param data a data frame with `join_var` and the `zcol` variable(s).
#' @param zcol the variable(s) to map. Names, if given, are used as the
#'   legend/layer title (e.g. `c(Population = "pob_total")`); otherwise the
#'   variable's `label` attribute, if any, or its name. `map_dept_static()`
#'   takes one variable only.
#' @param join_var name of the column in `data` with the department code.
#'   Codes can be character or numeric: `5`, `"5"` and `"05"` all match
#'   Antioquia. A national-total row (code `"00"`) is ignored.
#' @param discretize how to bin the values: `"pretty"` (round numbers),
#'   `"jenks"` (natural breaks), `"quantile"`, `"equal"` (equal width), or
#'   `"none"` (continuous colours; for `map_dept()` that means mapview's own
#'   legend, without thousands separators).
#' @param n_breaks number of classes when binning (approximate for
#'   `"pretty"`).
#' @param palette colours: a viridis option (`"viridis"`, `"magma"`,
#'   `"cividis"`, `"mako"`, ...), a vector of colours to interpolate (e.g.
#'   `c("white", "darkred")`), or a function taking `n` and returning `n`
#'   colours.
#' @param inset move and enlarge San Andres and Providencia next to the
#'   mainland (see [inset_sanandres()]).
#' @param tooltip `"auto"` (department name and value) or a [glue::glue()]
#'   template evaluated on the joined data, e.g.
#'   `"{dept_nom}: {scales::percent(rate)}"`.
#' @param basemaps basemap(s), as `map.types` in [mapview::mapview()]; the
#'   first is shown and the rest are offered in the layer switcher. The
#'   defaults, Esri's light grey canvas and topographic map, need no API key.
#'   mapview's own default, CARTO, now needs a key; OpenStreetMap's
#'   volunteer-run servers block maps opened from a local file or the RStudio
#'   viewer (no referrer).
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
#' map_dept_static(df, zcol = "rate", palette = c("white", "darkred"))
#' }
map_dept <- function(data,
                     zcol,
                     join_var = "dept_cod",
                     discretize = c("pretty", "jenks", "quantile", "equal",
                                    "none"),
                     n_breaks = 5,
                     palette = "viridis",
                     inset = TRUE,
                     tooltip = "auto",
                     basemaps = c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap"),
                     ...,
                     source = "auto",
                     refresh = FALSE) {
  map_interactive(
    "dept", data, zcol, join_var, match.arg(discretize), n_breaks, palette,
    inset, tooltip, basemaps, ..., source = source, refresh = refresh
  )
}

#' @rdname map_dept
#' @export
map_dept_static <- function(data,
                            zcol,
                            join_var = "dept_cod",
                            discretize = c("pretty", "jenks", "quantile",
                                           "equal", "none"),
                            n_breaks = 5,
                            palette = "viridis",
                            inset = TRUE,
                            source = "auto",
                            refresh = FALSE) {
  map_static(
    "dept", data, zcol, join_var, match.arg(discretize), n_breaks, palette,
    inset, source = source, refresh = refresh
  )
}

# Internals shared by all levels ------------------------------------------

map_interactive <- function(level, data, zcol, join_var, discretize, n_breaks,
                            palette, inset, tooltip, basemaps, ...,
                            source, refresh) {
  stopifnot(is.character(tooltip), length(tooltip) == 1)
  pal <- palette_fun(palette)

  geom <- atlas_geom(level, source = source, inset = inset, refresh = refresh)
  geom <- join_data(geom, data, join_var, zcol, level)
  titles <- layer_names(data, zcol)

  maps <- lapply(seq_along(zcol), function(i) {
    var <- unname(zcol[[i]])
    label <- make_tooltip(geom, var, titles[[i]], tooltip, level)
    if (discretize == "none") {
      col <- var
      colours <- pal
    } else {
      # bin here rather than with mapview's `at`, whose legend can't format
      # numbers: a factor shows our labels ("1,000 - 2,000") in order
      col <- paste0(var, "_class")
      binned <- bin_values(geom[[var]], n_breaks, discretize, pal)
      geom[[col]] <- binned$class
      colours <- binned$colours
    }
    mapview::mapview(
      geom,
      zcol = col,
      col.regions = colours,
      map.types = basemaps,
      layer.name = titles[[i]],
      label = label,
      hide = i > 1,
      ...
    )
  })
  m <- Reduce(`+`, maps)
  # keep the mapview object (so more layers can be added with `+`)
  m@map <- leafem::removeMouseCoordinates(m@map)
  m
}

map_static <- function(level, data, zcol, join_var, discretize, n_breaks,
                       palette, inset, source, refresh) {
  if (length(zcol) != 1) {
    stop("Static maps show one variable; `zcol` has ", length(zcol), ".",
         call. = FALSE)
  }
  pal <- palette_fun(palette)

  geom <- atlas_geom(level, source = source, inset = inset, refresh = refresh)
  geom <- join_data(geom, data, join_var, zcol, level)
  var <- unname(zcol[[1]])
  title <- layer_names(data, zcol)[[1]]

  if (discretize == "none") {
    fill <- ggplot2::aes(fill = .data[[var]])
    scale <- ggplot2::scale_fill_gradientn(
      colours = pal(256), name = title, na.value = "grey85"
    )
  } else {
    col <- paste0(var, "_class")
    binned <- bin_values(geom[[var]], n_breaks, discretize, pal)
    geom[[col]] <- binned$class
    fill <- ggplot2::aes(fill = .data[[col]])
    scale <- ggplot2::scale_fill_manual(
      values = stats::setNames(binned$colours, levels(binned$class)),
      name = title, na.value = "grey85"
    )
  }

  ggplot2::ggplot(geom) +
    ggplot2::geom_sf(fill, colour = "white", linewidth = 0.15) +
    scale +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_void()
}
