% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GridRequestor.R
\name{getGrids}
\alias{getGrids}
\title{Get climate data grids for specified parameter(s) and NPS unit(s)}
\usage{
getGrids(
  unitCode = NULL,
  sdate = NULL,
  edate = NULL,
  distance = NULL,
  climateParameters = NULL,
  duration = "dly",
  gridDescription = "PRISM",
  filePath = NULL,
  customBBox = NULL
)
}
\arguments{
\item{unitCode}{(optional) One unit code as a string. If not specified, defaults to CONUS geographic extent.}

\item{sdate}{sdate (required) Format as a string (yyyy-mm, yyyymm, yyyy). The beginning of the desired date range.}

\item{edate}{edate (required) Format as a string (yyyy-mm, yyyymm, yyyy). The end of the desired date range.}

\item{distance}{(optional) Distance (in kilometers) to buffer park bounding box}

\item{climateParameters}{(optional) A list of one or more daily, monthly, or yearly climate parameters (e.g. pcpn, mint, maxt, avgt or mly_pcpn, mly_mint, mly_maxt, mly_avgt or yly_pcpn, yly_mint, yly_maxt, yly_avgt).  If not specified, defaults to all parameters for the specified duration. See the ACIS Web Services page: \url{http://www.rcc-acis.org/docs_webservices.html}}

\item{duration}{(optional)  "dly" | "mly" | "yly" (i.e, daily, monthly, or yearly). If not specified, defaults to dly.}

\item{gridDescription}{(optional) "PRISM" is the default. For all options, see Table 6 on the ACIS Web Services page: \url{http://www.rcc-acis.org/docs_webservices.html}}

\item{filePath}{filePath (optional) Folder path for output ASCII grid(s). If specified, grid(s) are saved to the folder. Otherwise, grid(s) are saved to an in-memory raster stack (by date). If using Windows, include double back-slashes.}

\item{customBBox}{(optional) String containing bounding box geographic coordinates (longitude,latitude) using the WGS84 datum in the following format: Lower Left Longitude, Lower Left Latitude, Upper Right Longitude, Upper Right Latitude. Note: longitude is negative in the western hemisphere. Example: "-114.291153779, 35.5612153111, -111.252315168, 37.0351548001"}
}
\value{
ASCII-formatted grid file for each parameter for each date or an in-memory raster stack with a layer for each parameter for each date
}
\description{
Takes one park code and a list of one or more climate parameters and requests gridded climate data from ACIS Web Services (\url{http://www.rcc-acis.org/docs_webservices.html}) for the specified duration (daily, monthly, or yearly). Returns a grid or grids (by parameter) in ASCII format or an in-memory raster stack.
}
\examples{
\dontrun{
Two daily PRISM grids (default grid source) for GRSM for one date: print output to console
getGrids(unitCode = list("GRSM"), sdate = "20160615", edate = "20160616", climateParameters = list("mint", "maxt"), duration = "dly")

Two daily NRCC Interpolated grids for GRSM for one date: print output to console
getGrids(unitCode = list("GRSM"), sdate = "20160615", edate = "20160616", climateParameters = list("mint", "maxt"), duration = "dly", gridDescription = "NRCC Interpolated")

Two monthly PRISM grids for PRWI for one date: returns one grid for each parameter for each date - 4 grids total
getGrids(unitCode = list("PRWI"), sdate = "201606", edate = "201607", duration = "mly", climateParameters = list("mly_mint", "mly_maxt"), filePath="d:\\\\temp\\\\trash")

Two monthly PRISM grids for GRSM for one date: print output to console
getGrids(unitCode = list("GRSM"), sdate = "20160615", edate = "20160616", climateParameters = list("mly_mint", "mly_maxt"), duration = "mly")

The same request for two monthly PRISM grids for GRSM for one date: output to in-memory raster stack, t
t <- getGrids(unitCode = list("GRSM"), sdate = "201606", edate = "201607", duration = "mly", climateParameters = list("mly_mint", "mly_maxt"))

Get all monthly PRISM grids for one year for one parameter
getGrids(unitCode = list("GRKO"), sdate = "190001", edate = "190012", duration = "mly", climateParameters = list("mly_mint"))

Get yearly PRISM grids for three years for all parameters
getGrids(unitCode = list("SHEN"), sdate = "2013", edate = "2016", duration = "yly")
}
}
