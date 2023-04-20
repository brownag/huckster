#' @keywords internal
#' @import terra
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @return `huckster_usgs_hydro_wbd_url()`: returns the value of system environment variable `R_huckster_usgs_hydro_wbd_url`, R option `huckster.usgs_base_url` or the default value `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`.
#' @usage NULL
#' @rdname huckster-package
#' @export
huckster_usgs_hydro_wbd_url <- function() {
  s <- Sys.getenv("R_huckster_usgs_hydro_wbd_url", unset = "")

  if (nchar(s) == 0) {
    s2 <- getOption("huckster.usgs_base_url", default = "")
    if (nchar(s2) == 0) {
      return("https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/")
    } else return(s2)
  } else return(s)
}
