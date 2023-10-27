#' Read in and wrangle data from Blue Maestro sensors
#'
#' @param filename path to the csv file obtained from the blue maestro app.
#' @export
bm_prep <- function(filename){
  top <- readr::read_csv(filename,n_max = 1)

  bottom <- read.csv(filename) %>%
    dplyr::slice(3:nrow(.)) %>%
    dplyr::select(index =1, date=2, temperature =3, humidity=4, dewpoint=5 ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(3:5, as.numeric) %>%
    tidyr::separate(date, sep = " ",
                    into = c("weekday", "month", "mday", "year", "hms", "tz", "tzv"),
                    extra = "merge") %>%
    dplyr::mutate(dt = lubridate::ymd_hms(paste(year, month, mday, hms, sep="-")),
           ymd = lubridate::as_date(dt)) %>%
    dplyr::rename(temperature_c = temperature,
                  humidity_pct = humidity,
                  dewpoint_c = dewpoint) %>%
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
#' @export
upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.12, fill.sinks = TRUE)

{ requireNamespace("topmodel")
  requireNamespace("raster")
  if (!all.equal(xres(dem), yres(dem))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if (fill.sinks) {
    capture.output(dem <- invisible(raster::setValues(dem,
                                                      topmodel::sinkfill(raster::as.matrix(dem),
                                                                         res = xres(dem),
                                                                         degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = xres(dem))
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- addLayer(a, atb)
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
create_layers <- function (dem, fill.sinks = TRUE, deg = 0.1)
{
  requireNamespace("raster")
  layers <- stack(dem)
  message("Building upslope areas...")
  a.atb <- upslope(dem, atb = TRUE, fill.sinks = fill.sinks, deg = deg)
  layers <- addLayer(layers, a.atb)
  names(layers) <- c("filled.elevations", "upslope.area", "twi")
  return(layers)
}

#' Calculate folded aspect
#'
#' @param aspect Aspect in degrees. Can be a raster or a vector of numbers
#' @export
#'
#' @return folded aspect, ranges from 0-180, with 180 being the hottest aspect (SW)
folded_aspect <- function(aspect){abs(180 - abs(aspect - 225))}
