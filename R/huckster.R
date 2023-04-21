#' Get boundaries for hydrologic unit IDs at the specified level.
#'
#' Hydrologic unit boundaries are retrieved from the USGS NationalMap ArcGIS MapServer web services: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`.
#'
#' @param x character. Hydrologic Unit IDs
#' @param layerid numeric. `1:8` where `2*layerid` is equal to the number of digits in the Hydrologic Unit Code (HUC). Default: `5` for "HUC-10" watershed. See details.
#' @param base_url Default: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`
#' @details
#' The levels of `layerid` correspond to the numeric codes for the following hydrologic units (HU):
#' - WBDLine (0)
#' - 2-digit HU (Region) (1)
#' - 4-digit HU (Subregion) (2)
#' - 6-digit HU (Basin) (3)
#' - 8-digit HU (Subbasin) (4)
#' - 10-digit HU (Watershed) (5)
#' - 12-digit HU (Subwatershed) (6)
#' - 14-digit HU (7)
#' - 16-digit HU (8)
#' @return A `SpatVector` object derived from GeoJSON.
#' @export
#' @examplesIf !inherits(requireNamespace("terra", quietly = TRUE), 'try-error')
#' @examples
#' \dontrun{
#' x <- id_to_huc(c("071000050101", "071000050102", "071000050103", "071000050104"), layerid = 6)
#' terra::plot(x)
#' }
id_to_huc <- function(x,
                      layerid = 5,
                      base_url = huckster_usgs_hydro_wbd_url()) {

  stopifnot(requireNamespace("terra"))

  stopifnot(all(layerid %in% 0:8))

  if (length(layerid) < length(x)) {
    layerid <- rep(layerid, length(x))
  }

  if (length(layerid) != length(x)) {
    stop("`layerid` should have length 1 or length equal to `x`", call. = FALSE)
  }

  res <- lapply(seq_along(x), function(i) {
    in_id <-
      utils::URLencode(paste0("huc", layerid[i] * 2, "=", shQuote(x[i], type = 'sh')))
    queryurl <- paste0(base_url, layerid[i], "/query")
    urx <- paste0(queryurl, "?WHERE=", in_id, "&f=geojson")
    terra::vect(urx)
  })

  do.call('rbind', res)
}

#' Intersect hydrologic unit boundaries at the specified level with a SpatVector containing points or X,Y coordinates
#'
#' Hydrologic unit boundaries are retrieved from the USGS NationalMap ArcGIS MapServer web services: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`.
#'
#' @param x A `SpatVector` object or `numeric` vector.
#' @param y `NULL` (when x is a spatial object); otherwise a `numeric` vector equal in length to `x`
#' @param layerid numeric. `1:8` where `2*layerid` is equal to the number of digits in the Hydrologic Unit Code (HUC). Default: `5` for "HUC-10" watershed. See details.
#' @param sr_in integer. Spatial Reference System of input (`x`, `y`) as specified with numeric code. Default: `4326` for `"EPSG:4326"`.
#' @param sr_out integer. Spatial Reference System of result as specified with numeric code. Default: `sr_in`; equivalent to input.
#' @param base_url Default: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`
#' @details
#' The levels of `layerid` correspond to the numeric codes for the following hydrologic units (HU):
#' - WBDLine (0)
#' - 2-digit HU (Region) (1)
#' - 4-digit HU (Subregion) (2)
#' - 6-digit HU (Basin) (3)
#' - 8-digit HU (Subbasin) (4)
#' - 10-digit HU (Watershed) (5)
#' - 12-digit HU (Subwatershed) (6)
#' - 14-digit HU (7)
#' - 16-digit HU (8)
#' @return A `SpatVector` object derived from GeoJSON.
#' @export
#' @examplesIf !inherits(requireNamespace("terra", quietly = TRUE), 'try-error')
#' @examples
#' \dontrun{
#' point_to_huc(-120, 36:39)
#' }
point_to_huc <-
  function(x,
           y = NULL,
           layerid = 5,
           sr_in = 4326,
           sr_out = sr_in,
           base_url = huckster_usgs_hydro_wbd_url()) {

  stopifnot(requireNamespace("terra"))

  # convert to SpatVector points; polygons -> centroids
  if (!is.numeric(x)) {
    if (!inherits(x, 'SpatVector')) {
      x <- terra::vect(x)
    }
    if (!terra::is.points(x)) {
      x <- terra::centroids(x)
    }
    xx <- terra::crds(x)
    x <- xx[,1]
    y <- xx[,2]
  }

  stopifnot(all(layerid %in% 0:8))

  if (length(y) < length(x)) {
    y <- rep(y, length(x))
  } else if (length(x) < length(y)) {
    x <- rep(x, length(y))
  }

  if (length(x) != length(y)) {
    stop("`x` should have length 1 or length equal to `y`", call. = FALSE)
  }

  if (length(layerid) < length(x)) {
    layerid <- rep(layerid, length(x))
  }

  if (length(layerid) != length(x)) {
    stop("`layerid` should have length 1 or length equal to `x`", call. = FALSE)
  }

  res <- lapply(seq_along(x), function(i) {
    in_geom <- utils::URLencode(sprintf("%%28%s,%s%%29", x[i], y[i]))
    queryurl <- paste0(base_url, layerid[i], "/query")
    urx <- paste0(
      queryurl,
      "?geometry=",
      in_geom,
      "&geometryType=esriGeometryPoint&inSR=",
      sr_in,
      "&spatialRel=esriSpatialRelIntersects&outSR=",
      sr_out,
      "&f=geojson"
    )
    terra::vect(urx)
  })

  do.call('rbind', res)
}

#' Intersect hydrologic unit boundaries at the specified level with a SpatExtent or bounding box/envelope.
#'
#' Hydrologic unit boundaries are retrieved from the USGS NationalMap ArcGIS MapServer web services: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`.
#'
#' @param x A `SpatExtent` object (or object coercible to one) or `numeric` vector.
#' @param ... Additional arguments are (in order) `ymin`, `xmax`, `ymax` when `x` is numeric (`xmin`).
#' @param layerid numeric. `1:8` where `2*layerid` is equal to the number of digits in the Hydrologic Unit Code (HUC). Default: `5` for "HUC-10" watershed. See details.
#' @param sr_in integer. Spatial Reference System of input (`x`, `y`) as specified with numeric code. Default: `4326` for `"EPSG:4326"`.
#' @param sr_out integer. Spatial Reference System of result as specified with numeric code. Default: `sr_in`; equivalent to input.
#' @param base_url Default: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`
#' @details
#' The levels of `layerid` correspond to the numeric codes for the following hydrologic units (HU):
#' - WBDLine (0)
#' - 2-digit HU (Region) (1)
#' - 4-digit HU (Subregion) (2)
#' - 6-digit HU (Basin) (3)
#' - 8-digit HU (Subbasin) (4)
#' - 10-digit HU (Watershed) (5)
#' - 12-digit HU (Subwatershed) (6)
#' - 14-digit HU (7)
#' - 16-digit HU (8)
#' @return A `SpatVector` object derived from GeoJSON.
#' @export
#' @examplesIf !inherits(requireNamespace("terra", quietly = TRUE), 'try-error')
#' @examples
#' \dontrun{
#' envelope_to_huc(-121, 36, -120, 37, layerid = 4)
#' }
envelope_to_huc <-
  function(x,
           ...,
           layerid = 5,
           sr_in = 4326,
           sr_out = sr_in,
           base_url = huckster_usgs_hydro_wbd_url()) {

  stopifnot(requireNamespace("terra"))

  # convert to SpatVector points; polygons -> centroids
  if (!is.numeric(x)) {
    if (!inherits(x, 'SpatExtent')) {
       e <- terra::ext(x)
    } else {
       e <- terra::ext(x[1], x[3], x[2], x[4], xy = TRUE)
    }
  } else {
    bbx <- list(...)
    e <- terra::ext(x, bbx[1], bbx[2], bbx[3], xy = TRUE)
  }

  stopifnot(all(layerid %in% 0:8))

  if (length(layerid) != 1) {
    stop("`layerid` should have length 1", call. = FALSE)
  }

  in_geom <- utils::URLencode(sprintf("%%28%s,%s,%s,%s%%29", e[1], e[3], e[2], e[4]))
  queryurl <- paste0(base_url, layerid, "/query")
  urx <- paste0(
    queryurl,
    "?geometry=",
    in_geom,
    "&geometryType=esriGeometryEnvelope&inSR=",
    sr_in,
    "&spatialRel=esriSpatialRelIntersects&outSR=",
    sr_out,
    "&f=geojson"
  )
  terra::vect(urx)
}

#' Intersect hydrologic unit boundaries at the specified level with a SpatVector containing polygons.
#'
#' Hydrologic unit boundaries are retrieved from the USGS NationalMap ArcGIS MapServer web services: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`.
#'
#' @param x character. WKT string.
#' @param layerid numeric. `1:8` where `2*layerid` is equal to the number of digits in the Hydrologic Unit Code (HUC). Default: `5` for "HUC-10" watershed. See details.
#' @param sr_in integer. Spatial Reference System of input (`x`, `y`) as specified with numeric code. Default: `4326` for `"EPSG:4326"`.
#' @param sr_out integer. Spatial Reference System of result as specified with numeric code. Default: `sr_in`; equivalent to input.
#' @param base_url Default: `"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"`
#' @details
#' The levels of `layerid` correspond to the numeric codes for the following hydrologic units (HU):
#' - WBDLine (0)
#' - 2-digit HU (Region) (1)
#' - 4-digit HU (Subregion) (2)
#' - 6-digit HU (Basin) (3)
#' - 8-digit HU (Subbasin) (4)
#' - 10-digit HU (Watershed) (5)
#' - 12-digit HU (Subwatershed) (6)
#' - 14-digit HU (7)
#' - 16-digit HU (8)
#' @return A `SpatVector` object derived from GeoJSON.
#' @export
#' @examplesIf !inherits(requireNamespace("terra", quietly = TRUE), 'try-error')
#' @examples
#' \dontrun{
#' x <- 'POLYGON((-121.355 37.56,-121.355 37.555,
#'           -121.35 37.555,-121.35 37.56,
#'           -121.355 37.56))' |>
#'   terra::vect(crs = "OGC:CRS84")
#' polygon_to_huc(x, layerid = 6)
#' }
polygon_to_huc <-
  function(x,
           layerid = 5,
           sr_in = 4326,
           sr_out = sr_in,
           base_url = huckster_usgs_hydro_wbd_url()) {
    stopifnot(requireNamespace("terra"))

    # convert to SpatVector points; polygons -> centroids
    if (!terra::is.polygons(x)) {
      stop("`x` should be a `SpatVector` containing polygon geometries")
    }

    # if the CRS has a defined code, use it as sr_in
    sr_in <- .poly_crs(x, .default = sr_in)

    stopifnot(all(layerid %in% 0:8))

    if (length(layerid) != 1) {
      stop("`layerid` should have length 1", call. = FALSE)
    }

    gme <- as.data.frame(terra::geom(x))
    gms <- paste0("[", paste0(sapply(
      split(gme, gme$geom), \(x) paste0(sprintf("[%s, %s]", x$x, x$y), collapse = ",")
    ), collapse = "],["), "]")
    bse <- "{\"rings\":[%s],\"spatialReference\":{\"wkid\":%s}}"
    in_geom <-
      utils::URLencode(sprintf(bse, gms, sr_in), reserved = TRUE)
    queryurl <- paste0(base_url, layerid, "/query")
    urx <- paste0(
      queryurl,
      "?geometry=",
      in_geom,
      "&geometryType=esriGeometryPolygon&inSR=",
      sr_in,
      "&spatialRel=esriSpatialRelIntersects&outSR=",
      sr_out,
      "&f=geojson"
    )
    terra::vect(urx)
}


.poly_crs <- function(x, .default = 4326) {
  sr_x <- terra::crs(x, describe = TRUE)$code
  if (!is.null(sr_x) && !is.na(sr_x)) {
    return(sr_x)
  }
  return(.default)
}
