% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/huckster.R
\name{id_to_huc}
\alias{id_to_huc}
\title{Get boundaries for hydrologic unit IDs at the specified level.}
\usage{
id_to_huc(
  x,
  layer = x,
  layerid = huc_code(layer),
  base_url = huckster_usgs_hydro_wbd_url()
)
}
\arguments{
\item{x}{character. Hydrologic Unit IDs}

\item{layer}{One or more: labels, codes, or code lengths to convert to \code{layerid}. Default \code{"watershed"}, which is equivalent to "10-digit", "10" or any 10-digit code.}

\item{layerid}{numeric. \code{1:8} where \code{2*layerid} is equal to the number of digits in the Hydrologic Unit Code (HUC). Default: \code{5} derived from \code{"watershed"} for "10-digit" hydrologic unit boundary. See details.}

\item{base_url}{Default: \code{"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"}}
}
\value{
A \code{SpatVector} object derived from GeoJSON.
}
\description{
Hydrologic unit boundaries are retrieved from the USGS NationalMap ArcGIS MapServer web services: \code{"https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/"}.
}
\details{
The levels of \code{layerid} correspond to the numeric codes for the following hydrologic units (HU):
\itemize{
\item WBDLine (0)
\item 2-digit HU (Region) (1)
\item 4-digit HU (Subregion) (2)
\item 6-digit HU (Basin) (3)
\item 8-digit HU (Subbasin) (4)
\item 10-digit HU (Watershed) (5)
\item 12-digit HU (Subwatershed) (6)
\item 14-digit HU (7)
\item 16-digit HU (8)
}
}
\examples{
\dontshow{if (!inherits(requireNamespace("terra", quietly = TRUE), 'try-error')) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontshow{\}) # examplesIf}
\dontrun{
x <- id_to_huc(c("071000050101", "071000050102", "071000050103", "071000050104"))
terra::plot(x)
}
}
