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

#' Calculate Topographic Wetness Index and upslope area
#'
#' Thanks to https://stackoverflow.com/questions/58553966/calculating-twi-in-r
#'
#' @param dem A digital elevation model loaded in using the Raster or terra package. Units must be in meters
#' @param deg some kind of degree thing
#' @param fill.sinks if true, small depressions are smoothed out in the dem
#' @param resolution NA is the default, assuming the native XY resolution of the
#' raster is in meters. Set to the resolution in meters if the native xy resolution
#' of the raster is in degrees
#'
#' @export
get_twi <- function(dem, deg = 0.12, fill.sinks=T, resolution=NA){
  requireNamespace("terra")
  requireNamespace("topmodel")
  if(class(dem)|>as.character() == "RasterLayer") dem <- terra::rast(dem)
  if(is.na(resolution)) resolution <- terra::xres(dem)
  if (fill.sinks) {
    utils::capture.output(dem1 <- invisible(terra::setValues(dem,
                                                      topmodel::sinkfill(terra::as.matrix(dem, wide = T),
                                                                         res = resolution,
                                                                         degree = deg))))
  }else{dem1 <- dem}
  ti <- topmodel::topidx(terra::as.matrix(dem1, wide=T), res = resolution)
  a <- terra::setValues(dem1, ti$area)
  a <- log(a + 1)
  atb <- terra::setValues(dem1, ti$atb)
  a <- c(a, atb)
  names(a) <- c("upslope_area", "twi")
  return(a)
}

#' Calculate folded aspect
#'
#' @param aspect Aspect in degrees. Can be a raster or a vector of numbers
#' @export
#'
#' @return folded aspect, ranges from 0-180, with 180 being the hottest aspect (SW)
get_folded_aspect <- function(aspect){abs(180 - abs(aspect - 225))}

#' Saturation pressure
#'
#' Thanks https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
#'
#' @param temp_c temperature
#' @export
get_es <- function(temp_c){
  es <- 6.11 * exp((2.5e6 / 461) * (1 / 273.15 - 1 / (273.15 + temp_c)))
  return(es)
}

#' Vapor pressure deficit
#'
#' Thanks https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
#'
#' @param temp_c temperature
#' @param rh relative humidity
#' @export
get_vpd <- function(rh, temp_c){
  ## calculate saturation vapor pressure
  es <- get_es(temp_c)
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}
