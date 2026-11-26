# Small fake departments (squares), in WGS 84, so most tests run offline.
# "88" has two islands at the real positions of San Andres and Providencia.
fake_geom <- function() {
  square <- function(x, y, d) {
    sf::st_polygon(list(rbind(
      c(x, y), c(x + d, y), c(x + d, y + d), c(x, y + d), c(x, y)
    )))
  }
  geoms <- sf::st_sfc(
    sf::st_multipolygon(list(square(-76, 6, 1))),
    sf::st_multipolygon(list(square(-75, 10.5, 0.5))),
    sf::st_multipolygon(list(
      square(-81.73, 12.48, 0.05), # San Andres
      square(-81.39, 13.32, 0.04)  # Providencia
    )),
    crs = 4326
  )
  sf::st_sf(
    dept_cod = c("05", "08", "88"),
    dept_nom = c("Antioquia", "Atlantico", "San Andres"),
    geometry = geoms
  )
}

test_that("pad_code pads numeric and short codes", {
  expect_equal(pad_code(c(5, 8, 11), 2), c("05", "08", "11"))
  expect_equal(pad_code(c("5", "05", " 8 ", "5.0"), 2), c("05", "05", "08", "05"))
  expect_equal(pad_code(c(NA, "abc"), 2), c(NA, "abc"))
})

test_that("join_data keeps every department and matches codes", {
  geom <- fake_geom()
  df <- data.frame(code = c(5, 88), x = c(1, 2))
  out <- join_data(geom, df, "code", "x")
  expect_equal(nrow(out), 3)
  expect_equal(out$x, c(1, NA, 2))
})

test_that("join_data ignores a national total and warns on unknown codes", {
  geom <- fake_geom()
  expect_silent(join_data(geom, data.frame(dept_cod = c("00", "05"), x = 1:2),
                          "dept_cod", "x"))
  expect_warning(
    join_data(geom, data.frame(dept_cod = c("05", "77"), x = 1:2),
              "dept_cod", "x"),
    "77"
  )
})

test_that("join_data stops on bad input", {
  geom <- fake_geom()
  expect_error(join_data(geom, data.frame(a = 1), "dept_cod", "x"), "join_var")
  expect_error(join_data(geom, data.frame(dept_cod = "05"), "dept_cod", "x"),
               "no column")
  expect_error(join_data(geom, data.frame(dept_cod = c(5, "05"), x = 1:2),
                         "dept_cod", "x"),
               "one row per department")
})

test_that("layer names come from zcol names, then labels, then names", {
  df <- data.frame(a = 1, b = 2, c = 3)
  attr(df$b, "label") <- "Label B"
  expect_equal(layer_names(df, c(`Name A` = "a", "b", "c")),
               c("Name A", "Label B", "c"))
})

test_that("binning drops empty classes but keeps each class's colour", {
  pal <- function(n) paste0("col", seq_len(n))
  # pretty breaks 0-20-40-60-80-100: classes 2 and 3 empty
  b <- bin_values(c(5, 10, 70, 90, 95), 5, "pretty", pal)
  expect_true(is.factor(b$class))
  expect_equal(levels(b$class), c("0 \u2013 20", "60 \u2013 80", "80 \u2013 100"))
  expect_equal(b$colours, c("col1", "col4", "col5"))
  expect_equal(nlevels(b$class), length(b$colours))
})

test_that("breaks and labels", {
  x <- c(1, 2, 3, 10, 11, 12, 100, 101)
  for (m in c("jenks", "quantile", "equal")) {
    b <- get_breaks(x, 3, m)
    expect_equal(range(b), range(x))
  }
  expect_equal(break_labels(c(40800, 710000, 1930000)),
               c("40,800 \u2013 710,000", "710,000 \u2013 1,930,000"))
})

test_that("inset moves and enlarges the islands, leaving the rest alone", {
  geom <- fake_geom()
  moved <- inset_sanandres(geom)

  expect_equal(sf::st_crs(moved), sf::st_crs(geom))
  expect_equal(sf::st_geometry(moved)[1:2], sf::st_geometry(geom)[1:2])

  islands <- sf::st_transform(sf::st_geometry(moved)[3], 3116)
  parts <- sf::st_cast(sf::st_cast(islands, "MULTIPOLYGON"), "POLYGON")
  expect_length(parts, 2)
  bb <- lapply(parts, sf::st_bbox)
  # San Andres' lower-left corner is placed at (700000, 1700000)
  expect_equal(unname(bb[[1]][c("xmin", "ymin")]), c(700000, 1700000),
               tolerance = 1e-6)
  expect_equal(unname(bb[[2]][c("xmin", "ymin")]), c(777000, 1770000),
               tolerance = 1e-6)

  # ~13 times larger than the original island
  orig <- sf::st_bbox(sf::st_transform(
    sf::st_cast(sf::st_geometry(geom)[3], "POLYGON")[1], 3116
  ))
  ratio <- (bb[[1]][["xmax"]] - bb[[1]][["xmin"]]) /
    (orig[["xmax"]] - orig[["xmin"]])
  expect_equal(ratio, 1 / 0.077, tolerance = 1e-6)
})

test_that("geometries are read from the cache", {
  withr::local_options(atlasr.cache_dir = withr::local_tempdir())
  fake <- standardise_geom(fake_geom(), "geoboundaries")
  dir.create(atlas_cache_dir(), showWarnings = FALSE)
  saveRDS(fake, cache_path("dept", "geoboundaries"))

  out <- atlas_geom("dept")
  expect_equal(attr(out, "source"), "geoboundaries")
  expect_equal(out$dept_cod, c("05", "08", "88"))

  atlas_cache_clear()
  expect_length(list.files(atlas_cache_dir()), 0)
})

test_that("static map is a ggplot with the data joined", {
  withr::local_options(atlasr.cache_dir = withr::local_tempdir())
  dir.create(atlas_cache_dir(), showWarnings = FALSE)
  saveRDS(standardise_geom(fake_geom(), "geoboundaries"),
          cache_path("dept", "geoboundaries"))

  df <- data.frame(dept_cod = c("05", "08", "88"), v = c(1, 5, 9))
  p <- map_dept_static(df, c(Value = "v"))
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$v, c(1, 5, 9))
  p2 <- map_dept_static(df, "v", discretize = "equal", n_breaks = 2)
  expect_true(is.factor(p2$data$v_class))
  expect_s3_class(map_dept_static(df, "v", discretize = "none"), "ggplot")
  expect_error(map_dept_static(df, c("v", "v")), "one variable")
})

test_that("palette accepts a viridis option, colours or a function", {
  expect_equal(palette_fun("magma")(3), viridisLite::magma(3))
  expect_equal(palette_fun(c("white", "black"))(3),
               c("#FFFFFF", "#7F7F7F", "#000000"))
  f <- function(n) rep("red", n)
  expect_identical(palette_fun(f), f)
  expect_error(palette_fun("reds"), "Unknown viridis option")
  expect_error(palette_fun(1), "palette")
})

test_that("both maps bin the same way and use the same colours", {
  withr::local_options(atlasr.cache_dir = withr::local_tempdir())
  dir.create(atlas_cache_dir(), showWarnings = FALSE)
  saveRDS(standardise_geom(fake_geom(), "geoboundaries"),
          cache_path("dept", "geoboundaries"))

  df <- data.frame(dept_cod = c("05", "08", "88"), v = c(5, 50, 95))
  p <- map_dept_static(df, "v", palette = "mako")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expected <- bin_values(df$v, 5, "pretty", palette_fun("mako"))
  expect_setequal(unique(built$fill), expected$colours)
})

test_that("interactive map is a mapview object with one layer per zcol", {
  withr::local_options(atlasr.cache_dir = withr::local_tempdir())
  dir.create(atlas_cache_dir(), showWarnings = FALSE)
  saveRDS(standardise_geom(fake_geom(), "geoboundaries"),
          cache_path("dept", "geoboundaries"))

  df <- data.frame(dept_cod = c("05", "08", "88"), a = 1:3, b = 4:6)
  m <- map_dept(df, c(A = "a", B = "b"))
  expect_s4_class(m, "mapview")
  expect_length(m@object, 2)

  # islands moved by default, real geometry on request
  bbox_of <- function(m) sf::st_bbox(m@object[[1]])
  expect_gt(bbox_of(m)[["xmin"]], -80)
  expect_lt(bbox_of(map_dept(df, "a", inset = FALSE))[["xmin"]], -81.5)

  # continuous colours also work
  expect_s4_class(map_dept(df, "a", discretize = "none"), "mapview")
})

test_that("municipality polygons get the code of the seat inside them", {
  seat <- function(code) {
    unlist(divipola_muni[divipola_muni$muni_cod == code, c("lon", "lat")])
  }
  around <- function(code, d = 0.05) {
    p <- seat(code)
    sf::st_polygon(list(rbind(
      p + c(-d, -d), p + c(d, -d), p + c(d, d), p + c(-d, d), p + c(-d, -d)
    )))
  }
  # Bajo Baudo and Belen de Bajira's seats (as DIVIPOLA locates them) in one
  # polygon: the name decides
  both <- sf::st_convex_hull(sf::st_union(around("27077"), around("27086")))
  ocean <- sf::st_polygon(list(rbind(
    c(-85, 0), c(-84, 0), c(-84, 1), c(-85, 1), c(-85, 0)
  )))
  polys <- sf::st_sf(
    shapeName = c("Medellín", "Bajo Baudó (Pizarro)", "Nowhere"),
    geometry = sf::st_sfc(around("05001"), both, ocean, crs = 4326)
  )
  expect_warning(codes <- seat_codes(polys), "Nowhere")
  expect_equal(codes, c("05001", "27077", NA))
})

test_that("standard columns and names come from DIVIPOLA", {
  g <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  muni <- with_names(c("05001", "11001"), g, "muni")
  expect_named(muni, c("muni_cod", "muni_nom", "dept_cod", "dept_nom",
                       "geometry"))
  expect_equal(muni$muni_nom, c("Medellín", "Bogotá, D.C."))
  expect_equal(muni$dept_nom, c("Antioquia", "Bogotá"))
  dept <- with_names(c("05", "88"), g, "dept")
  expect_equal(dept$dept_nom, c("Antioquia", "San Andrés"))
})

test_that("municipality codes are padded to 5; tooltips name the department", {
  geom <- with_names(
    c("05001", "08001"),
    sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326),
    "muni"
  )
  out <- join_data(geom, data.frame(muni_cod = 5001, x = 1), "muni_cod", "x",
                   "muni")
  expect_equal(out$x, c(1, NA))
  expect_equal(unit_label(sf::st_drop_geometry(out), "muni"),
               c("Medellín (Antioquia)", "Barranquilla (Atlántico)"))
})

test_that("the real sources download and standardise", {
  skip_on_cran()
  skip_if_offline()
  withr::local_options(atlasr.cache_dir = withr::local_tempdir())

  gb <- atlas_geom("dept", source = "geoboundaries")
  expect_equal(nrow(gb), 33)
  expect_setequal(gb$dept_cod, gb_dept_codes$dept_cod)
  expect_equal(sf::st_crs(gb)$epsg, 4326L)

  gb_muni <- atlas_geom("muni", source = "geoboundaries")
  expect_gt(nrow(gb_muni), 1100)
  expect_false(anyDuplicated(gb_muni$muni_cod) > 0)
  expect_true(all(gb_muni$dept_cod %in% gb$dept_cod))

  col <- tryCatch(suppressWarnings(atlas_geom("dept", source = "colopendata")),
                  error = function(e) NULL)
  skip_if(is.null(col), "ColOpenData server unavailable")
  expect_equal(nrow(col), 33)
  expect_setequal(col$dept_cod, gb$dept_cod)
  col_muni <- suppressWarnings(atlas_geom("muni", source = "colopendata"))
  expect_gt(nrow(col_muni), 1100)
})
