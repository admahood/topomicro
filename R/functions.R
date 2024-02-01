#' Read in and wrangle data from Blue Maestro sensors
#'
#' @param filename path to the csv file obtained from the blue maestro app.
#' @export
bm_prep <- function(filename){
  requireNamespace("readr")
  requireNamespace("tibble")
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  top <- readr::read_csv(filename,n_max = 1)

  bottom <- read.csv(filename) %>%
    dplyr::slice(3:nrow(.)) |>
    dplyr::select(index =1, date=2, temperature =3, humidity=4, dewpoint=5 ) |>
    tibble::as_tibble() |>
    dplyr::mutate_at(3:5, as.numeric) |>
    tidyr::separate(date, sep = " ",
                    into = c("weekday", "month", "mday", "year", "hms", "tz", "tzv"),
                    extra = "merge") |>
    dplyr::mutate(dt = lubridate::ymd_hms(paste(year, month, mday, hms, sep="-")),
           ymd = lubridate::as_date(dt)) |>
    dplyr::rename(temperature_c = temperature,
                  humidity_pct = humidity,
                  dewpoint_c = dewpoint) |>
    dplyr::mutate(id = top$Name,
           dl_date = top$Download_Date)
  lubridate::tz(bottom$dt) <- "US/Mountain"
  return(bottom)
}

#' Calculate upslope areas
#'
#' Thanks to https://stackoverflow.com/questions/58553966/calculating-twi-in-r
#'
#' @param dem A digital elevation model loaded in using the Raster package
#' @param log calculate twi as the log of the topographic index
#' @param atb
#' @param deg some kind of degree thing
#' @param fill.sinks if true, small depressions are smoothed out in the dem
#' @export
upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.12, resol=NA,
                     fill.sinks = TRUE)
{ requireNamespace("topmodel")
  requireNamespace("raster")
  if (!all.equal(xres(dem), yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if(is.na(res)) resol <- raster::xres(dem) else resol <- resol
  if (fill.sinks) {
    capture.output(dem <- invisible(
      raster::setValues(dem,
                        topmodel::sinkfill(raster::as.matrix(dem),
                                           res = resol,
                                           degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = resol)
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- raster::addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}

#' Create Topographic Wetness Index and assocated layers
#'
#' Thanks to https://stackoverflow.com/questions/58553966/calculating-twi-in-r
#'
#' @param dem A digital elevation model loaded in using the Raster package
#' @export
create_layers <- function (dem, fill.sinks = TRUE, deg = 0.1, res = NA)
{
  requireNamespace("raster")
  requireNamespace("terra")
  if(as.character(class(dem)) == "SpatRaster") dem <- as(dem, "Raster")
  layers <- raster::stack(dem)
  message("Building upslope areas...")
  a.atb <- upslope(dem, atb = TRUE, fill.sinks = fill.sinks, deg = deg)
  layers <- raster::addLayer(layers, a.atb)
  names(layers) <- c("filled.elevations", "upslope.area", "twi")
  return(layers)
}

terra_twi <- function(dem, deg = 0.1, fill.sinks=T, res=NA){
  requireNamespace("terra")
  requireNamespace("topmodel")
  if (fill.sinks) {
    capture.output(dem1 <- invisible(terra::setValues(dem,
                                                      topmodel::sinkfill(terra::as.matrix(dem),
                                                                         res = terra::xres(dem),
                                                                         degree = deg))))
  }else{dem1 <- dem}
  ti <- topmodel::topidx(terra::as.matrix(dem1), res = terra::xres(dem1))
  a <- terra::setValues(dem1, ti$area)
  a <- log(a + 1)

  if (atb) {
    atb <- terra::setValues(dem1, ti$atb)
    a <- terra::addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
}

#' Calculate folded aspect
#'
#' @param aspect Aspect in degrees. Can be a raster or a vector of numbers
#' @export
#'
#' @return folded aspect, ranges from 0-180, with 180 being the hottest aspect (SW)
folded_aspect <- function(aspect){abs(180 - abs(aspect - 225))}

#' Saturation pressure
#'
#' Thanks https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
#'
#' @param t temperature
#' @export
get_es <- function(temp_c){
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273.15 - 1 / (273.15 + temp_c)))
  return(es)
}

#' Vapor pressure deficit
#'
#' Thanks https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
#'
#' @param t temperature
#' @export
get_vpd <- function(rh, temp_c){
  ## calculate saturation vapor pressure
  es <- get_es(temp_c)
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}
