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

#' Map municipality data
#'
#' The municipal version of [map_dept()] and [map_dept_static()]: same
#' arguments and behaviour, with `muni_cod` (5-character DIVIPOLA code; `5001`
#' and `"05001"` both match Medellin) as the join variable. Department borders
#' are drawn on top, and tooltips name the department too ("Medellin
#' (Antioquia)"), since municipal names repeat.
#'
#' @inheritParams map_dept
#' @param join_var name of the column in `data` with the municipality code.
#' @param dept_borders draw department borders over the municipalities.
#'
#' @return `map_muni()`: a `mapview` object. `map_muni_static()`: a `ggplot`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(muni_cod = c("05001", "11001", "76001"), x = c(1, 2, 3))
#' map_muni(df, zcol = "x")
#' map_muni_static(df, zcol = "x", palette = "mako")
#' }
map_muni <- function(data,
                     zcol,
                     join_var = "muni_cod",
                     discretize = c("pretty", "jenks", "quantile", "equal",
                                    "none"),
                     n_breaks = 5,
                     palette = "viridis",
                     inset = TRUE,
                     tooltip = "auto",
                     basemaps = c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap"),
                     dept_borders = TRUE,
                     ...,
                     source = "auto",
                     refresh = FALSE) {
  map_interactive(
    "muni", data, zcol, join_var, match.arg(discretize), n_breaks, palette,
    inset, tooltip, basemaps, ..., dept_borders = dept_borders,
    source = source, refresh = refresh
  )
}

#' @rdname map_muni
#' @export
map_muni_static <- function(data,
                            zcol,
                            join_var = "muni_cod",
                            discretize = c("pretty", "jenks", "quantile",
                                           "equal", "none"),
                            n_breaks = 5,
                            palette = "viridis",
                            inset = TRUE,
                            dept_borders = TRUE,
                            source = "auto",
                            refresh = FALSE) {
  map_static(
    "muni", data, zcol, join_var, match.arg(discretize), n_breaks, palette,
    inset, dept_borders = dept_borders, source = source, refresh = refresh
  )
}

# Internals shared by all levels ------------------------------------------

# Line styles: municipalities need thinner lines than departments
line_style <- function(level) {
  switch(level,
    dept = list(static = 0.15, interactive = 1),
    muni = list(static = 0.05, interactive = 0.3)
  )
}

map_interactive <- function(level, data, zcol, join_var, discretize, n_breaks,
                            palette, inset, tooltip, basemaps, ...,
                            dept_borders = FALSE, source, refresh) {
  stopifnot(is.character(tooltip), length(tooltip) == 1)
  pal <- palette_fun(palette)

  geom <- atlas_geom(level, source = source, inset = inset, refresh = refresh)
  geom <- join_data(geom, data, join_var, zcol, level)
  titles <- layer_names(data, zcol)
  # defaults the user can override through `...`
  style <- utils::modifyList(
    list(color = "white", lwd = line_style(level)$interactive),
    list(...)
  )

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
    do.call(mapview::mapview, c(list(
      geom,
      zcol = col,
      col.regions = colours,
      map.types = basemaps,
      layer.name = titles[[i]],
      label = label,
      hide = i > 1
    ), style))
  })
  if (isTRUE(dept_borders)) {
    maps <- c(maps, list(mapview::mapview(
      sf::st_cast(dept_outline(geom), "MULTILINESTRING"),
      color = "white", lwd = 1.5, legend = FALSE, label = FALSE,
      popup = FALSE, homebutton = FALSE, layer.name = "Departments"
    )))
  }
  m <- Reduce(`+`, maps)
  # keep the mapview object (so more layers can be added with `+`)
  m@map <- leafem::removeMouseCoordinates(m@map)
  m
}

map_static <- function(level, data, zcol, join_var, discretize, n_breaks,
                       palette, inset, dept_borders = FALSE, source, refresh) {
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

  p <- ggplot2::ggplot(geom) +
    ggplot2::geom_sf(fill, colour = "white",
                     linewidth = line_style(level)$static)
  if (isTRUE(dept_borders)) {
    p <- p + ggplot2::geom_sf(data = dept_outline(geom), fill = NA,
                              colour = "white", linewidth = 0.35,
                              inherit.aes = FALSE)
  }
  p +
    scale +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_void()
}

# Department outlines, by merging the municipalities: they always match the
# municipal polygons (same source, same inset). Kept in memory for the
# session, since merging takes about a second.
outline_cache <- new.env(parent = emptyenv())

dept_outline <- function(geom) {
  key <- paste(attr(geom, "source"), nrow(geom),
               paste(round(sf::st_bbox(geom), 4), collapse = ","))
  if (!is.null(outline_cache[[key]])) {
    return(outline_cache[[key]])
  }
  crs <- sf::st_crs(geom)
  local <- sf::st_transform(sf::st_geometry(geom), 3116)
  parts <- split(local, geom$dept_cod)
  merged <- do.call(c, lapply(parts, sf::st_union))
  out <- sf::st_sf(
    dept_cod = names(parts),
    geometry = sf::st_transform(sf::st_sfc(merged, crs = 3116), crs)
  )
  outline_cache[[key]] <- out
  out
}
