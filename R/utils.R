# Left-pad DIVIPOLA codes with zeros: 5, "5" and "05" all become "05"
pad_code <- function(x, width) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x) # 5.0 read from a spreadsheet
  ok <- !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) < width
  x[ok] <- formatC(as.integer(x[ok]), width = width, flag = "0")
  x
}

# Join data to the geometries, by department code, keeping every
# department (those without data get NA)
join_data <- function(geom, data, join_var, zcol) {
  data <- as.data.frame(data)
  if (!join_var %in% names(data)) {
    stop("`data` has no column `", join_var, "` (set `join_var`).",
         call. = FALSE)
  }
  missing_vars <- setdiff(unname(zcol), names(data))
  if (length(missing_vars) > 0) {
    stop("`data` has no column(s): ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  codes <- pad_code(data[[join_var]], 2)
  # "00" is the national total in DANE tables: not a department, so ignore it
  data <- data[!codes %in% "00", , drop = FALSE]
  codes <- codes[!codes %in% "00"]
  if (anyDuplicated(codes[!is.na(codes)])) {
    stop("`data` must have one row per department; duplicated codes: ",
         paste(unique(codes[duplicated(codes) & !is.na(codes)]),
               collapse = ", "),
         call. = FALSE)
  }
  unmatched <- setdiff(codes, geom$dept_cod)
  if (length(unmatched) > 0) {
    warning("Codes in `data` that match no department (dropped): ",
            paste(unmatched, collapse = ", "), call. = FALSE)
  }

  data[[join_var]] <- NULL
  data <- data[setdiff(names(data), names(geom))]
  vars <- names(data)
  idx <- match(geom$dept_cod, codes)
  for (v in vars) {
    geom[[v]] <- data[[v]][idx]
  }
  geom
}

# Layer/legend titles: zcol names first, then the variable label, then the
# variable name
layer_names <- function(data, zcol) {
  nms <- rlang::names2(zcol)
  vapply(seq_along(zcol), function(i) {
    if (nzchar(nms[[i]])) return(nms[[i]])
    label <- attr(data[[zcol[[i]]]], "label", exact = TRUE)
    if (!is.null(label)) return(as.character(label))
    unname(zcol[[i]])
  }, character(1))
}

# Class breaks; "jenks" uses Fisher's algorithm (same optimal breaks, faster)
get_breaks <- function(x, n,
                       method = c("pretty", "jenks", "quantile", "equal")) {
  method <- match.arg(method)
  x <- x[is.finite(x)]
  if (method == "pretty") {
    return(pretty(x, n))
  }
  style <- c(jenks = "fisher", quantile = "quantile", equal = "equal")[[method]]
  breaks <- suppressWarnings(classInt::classIntervals(x, n, style = style)$brks)
  unique(breaks)
}

# "40,800 - 710,000" style labels (en dash) for the classes between breaks
break_labels <- function(breaks) {
  b <- format(signif(breaks, 3), big.mark = ",", scientific = FALSE,
              trim = TRUE, drop0trailing = TRUE)
  paste(utils::head(b, -1), utils::tail(b, -1), sep = " \u2013 ")
}

viridis_options <- c("magma", "inferno", "plasma", "viridis", "cividis",
                     "rocket", "mako", "turbo", LETTERS[1:8])

# Turn `palette` into a function n -> n colours: a viridis option name, a
# vector of colours to interpolate, or a function already
palette_fun <- function(palette) {
  if (is.function(palette)) {
    return(palette)
  }
  if (!is.character(palette) || length(palette) == 0) {
    stop("`palette` must be a viridis option, a vector of colours or a ",
         "function.", call. = FALSE)
  }
  if (length(palette) == 1) {
    if (!palette %in% viridis_options) {
      stop("Unknown viridis option \"", palette, "\"; use one of ",
           paste(viridis_options[1:8], collapse = ", "),
           ", or give two or more colours.", call. = FALSE)
    }
    return(function(n) viridisLite::viridis(n, option = palette))
  }
  grDevices::colorRampPalette(palette)
}

# Bin values into a factor with formatted labels, and the colour of each
# class that occurs. Colours come from the full palette, so a class keeps its
# colour even when an empty class is dropped (mapview needs exactly one colour
# per level present, or it trims the palette and shifts the colours).
bin_values <- function(x, n, method, col.regions) {
  breaks <- get_breaks(x, n, method)
  class <- cut(x, breaks, include.lowest = TRUE, labels = break_labels(breaks))
  all_colours <- if (is.function(col.regions)) {
    col.regions(nlevels(class))
  } else {
    rep_len(col.regions, nlevels(class))
  }
  present <- levels(class) %in% class
  list(class = droplevels(class), colours = all_colours[present])
}

make_tooltip <- function(geom, var, layer_name, tooltip) {
  data <- sf::st_drop_geometry(geom)
  text <- if (identical(tooltip, "auto")) {
    value <- data[[var]]
    value <- if (is.numeric(value)) {
      scales::number(value, big.mark = ",", accuracy = NULL)
    } else {
      as.character(value)
    }
    paste0("<b>", data$dept_nom, "</b><br/>", layer_name, ": ", value)
  } else {
    glue::glue_data(data, tooltip)
  }
  lapply(as.character(text), htmltools::HTML)
}
