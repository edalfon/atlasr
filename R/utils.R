# Left-pad DIVIPOLA codes with zeros: 5, "5" and "05" all become "05"
pad_code <- function(x, width) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x) # 5.0 read from a spreadsheet
  ok <- !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) < width
  x[ok] <- formatC(as.integer(x[ok]), width = width, flag = "0")
  x
}

# Join plot_data to the geometries, by department code, keeping every
# department (those without data get NA)
join_data <- function(geom, plot_data, join_var, zcol) {
  plot_data <- as.data.frame(plot_data)
  if (!join_var %in% names(plot_data)) {
    stop("`plot_data` has no column `", join_var, "` (set `join_var`).",
         call. = FALSE)
  }
  missing_vars <- setdiff(unname(zcol), names(plot_data))
  if (length(missing_vars) > 0) {
    stop("`plot_data` has no column(s): ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  codes <- pad_code(plot_data[[join_var]], 2)
  # "00" is the national total in DANE tables: not a department, so ignore it
  plot_data <- plot_data[!codes %in% "00", , drop = FALSE]
  codes <- codes[!codes %in% "00"]
  if (anyDuplicated(codes[!is.na(codes)])) {
    stop("`plot_data` must have one row per department; duplicated codes: ",
         paste(unique(codes[duplicated(codes) & !is.na(codes)]),
               collapse = ", "),
         call. = FALSE)
  }
  unmatched <- setdiff(codes, geom$dept_cod)
  if (length(unmatched) > 0) {
    warning("Codes in `plot_data` that match no department (dropped): ",
            paste(unmatched, collapse = ", "), call. = FALSE)
  }

  plot_data[[join_var]] <- NULL
  plot_data <- plot_data[setdiff(names(plot_data), names(geom))]
  vars <- names(plot_data)
  idx <- match(geom$dept_cod, codes)
  for (v in vars) {
    geom[[v]] <- plot_data[[v]][idx]
  }
  geom
}

# Layer/legend titles: zcol names first, then the variable label, then the
# variable name
layer_names <- function(plot_data, zcol) {
  nms <- rlang::names2(zcol)
  vapply(seq_along(zcol), function(i) {
    if (nzchar(nms[[i]])) return(nms[[i]])
    label <- attr(plot_data[[zcol[[i]]]], "label", exact = TRUE)
    if (!is.null(label)) return(as.character(label))
    unname(zcol[[i]])
  }, character(1))
}

# Class breaks; "jenks" uses Fisher's algorithm (same optimal breaks, faster)
get_breaks <- function(x, n, method = c("jenks", "quantile", "equal")) {
  method <- match.arg(method)
  x <- x[is.finite(x)]
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
