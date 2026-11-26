# Knit the vignette locally: atlasr.Rmd.orig -> atlasr.Rmd and figures/.
# The maps need geometries downloaded from the internet, which CRAN and
# R CMD check can't rely on, so the package ships the knitted result.
#
# Run from the package root, with atlasr installed (the vignette uses
# library(atlasr)):
#   devtools::install(); source("vignettes/precompile.R")
# Re-run after changing the .orig file or the maps.

local({
old <- setwd("vignettes")
on.exit(setwd(old), add = TRUE)

knitr::knit("atlasr.Rmd.orig", "atlasr.Rmd")

# Screenshot of the interactive map (needs Chrome or Edge)
pob_dept <- daner::pob_2018_dept
pob_dept$rural <- pob_dept$pob_rural / pob_dept$pob_total
m <- atlasr::map_dept(pob_dept, zcol = c(Population = "pob_total",
                                         `Rural share` = "rural"))
html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(m@map, html, selfcontained = FALSE)
browser <- Filter(file.exists, c(
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
  Sys.which(c("google-chrome", "chromium"))
))[1]
system2(browser, c(
  "--headless=new", "--disable-gpu", "--hide-scrollbars",
  "--window-size=700,650", "--virtual-time-budget=10000",
  paste0("--screenshot=", normalizePath("figures/atlasr-interactive.png",
                                        mustWork = FALSE)),
  paste0("file:///", normalizePath(html, winslash = "/"))
))
})
