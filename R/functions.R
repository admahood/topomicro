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
           dl_date = top$Download_Date,
           vpd_kPa = topomicro::get_vpd(rh = humidity_pct,
                                        temp_c = temperature_c))
  lubridate::tz(bottom$dt) <- "US/Mountain"

  return(bottom)
}

#' summarise blue maestro data to the plot scale
#'
#' @param mdf a data frame created by the bm_prep function
#' @examples
#' data('mef_micro_data')
#' plotwise_summary(mef_micro_data)
#'
#'
#'
#' @export
plotwise_summary <- function(mdf){
  requireNamespace("dplyr")
  requireNamespace("tibble")

  mdf |>
    dplyr::group_by(ymd, id) |>
    dplyr::summarise(tmean = mean(temperature_c),
              tmax = max(temperature_c),
              tmin= min(temperature_c),
              tdelta = tmax - tmin,
              vmean = mean(vpd_kPa),
              vmax = max(vpd_kPa),
              vmin= min(vpd_kPa),
              vdelta = vmax - vmin,
              rmean = mean(humidity_pct),
              rmax = max(humidity_pct),
              rmin= min(humidity_pct),
              rdelta = rmax - rmin) |>
    dplyr::ungroup() |>
    dplyr::select(-ymd) |>
    dplyr::group_by(id) |>
    dplyr::summarise_all(mean) |>
    dplyr::ungroup() |>
    dplyr::arrange(id) |>
    tibble::column_to_rownames("id")
}

#' summarise blue maestro data to the plot scale, standardized against a single time series
#'
#' columns of standardized values start with 's' (e.g tmax standardized is stmax)
#'
#' @param mdf a data frame created by the bm_prep function
#' @param stdf data frame consisting of a single time series of vpd, rh and temperature against which to standardize
#'
#' data('mef_micro_data'); data('mef_station_data')
#' plotwise_summary_std(mef_micro_data, mef_station_data)
#'
#' @export
plotwise_summary_std <- function(mdf, stdf){
  requireNamespace("dplyr")
  requireNamespace("tibble")

  std <- stdf |>
    dplyr::group_by(ymd) |>
    dplyr::reframe(stmean = mean(temperature_c),
                     stmax = max(temperature_c),
                     stmin= min(temperature_c),
                     stdelta = stmax - stmin,
                     svmean = mean(vpd_kPa),
                     svmax = max(vpd_kPa),
                     svmin= min(vpd_kPa),
                     svdelta = svmax - svmin,
                     srmean = mean(humidity_pct),
                     srmax = max(humidity_pct),
                     srmin= min(humidity_pct),
                     srdelta = srmax - srmin)


  return(mdf |>
    dplyr::group_by(ymd, id) |>
    dplyr::summarise(tmean = mean(temperature_c),
                     tmax = max(temperature_c),
                     tmin= min(temperature_c),
                     tdelta = tmax - tmin,
                     vmean = mean(vpd_kPa),
                     vmax = max(vpd_kPa),
                     vmin= min(vpd_kPa),
                     vdelta = vmax - vmin,
                     rmean = mean(humidity_pct),
                     rmax = max(humidity_pct),
                     rmin= min(humidity_pct),
                     rdelta = rmax - rmin) |>
    dplyr::ungroup() |>
    dplyr::left_join(std) |>
    dplyr::filter(!is.na(stmean)) |>
    dplyr::mutate(stmean = tmean - stmean,
                  stmax  = tmax - stmax,
                  stmin = tmin - stmin,
                  stdelta = tdelta - stdelta,
                  svmean = vmean - svmean,
                  svmax = vmax - svmax,
                  svmin = vmin - svmin,
                  svdelta = vdelta - svdelta,
                  srmean = rmean - srmean,
                  srmax = rmax - srmax,
                  srmin = rmin - srmin,
                  srdelta = rdelta - srdelta) |>
    dplyr::select(-ymd) |>
    dplyr::group_by(id) |>
    dplyr::summarise_all(mean) |>
    dplyr::ungroup() |>
    dplyr::arrange(id) |>
    tibble::column_to_rownames("id"))
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
#' Returns VPD in KPa from RH (percent) and temperature (deg C)
#'
#' Thanks https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
#'
#' @param temp_c temperature
#' @param rh relative humidity
#' @export
get_vpd <- function(rh, temp_c){
  ## calculate saturation vapor pressure
  es <- topomicro::get_es(temp_c)
  ## calculate vapor pressure deficit
  vpd_kPa <- (((100 - rh) / 100) * es)/10
  return(vpd_kPa)
}

# wish list
# delineate watersheds
# do a hypsogram
# summary of landfire/nlcd data

# hypsogram <- function(dem, watershed=NA){
#   # clip to watershed
#   requireNamespace('terra')
#   requireNamespace('ggplot2')
#
#   terra::get_values(dem) |>
#     as.data.frame() |>
#     ggplot2::ggplot() +
#     ggplot2::geom_histogram()
# }

#' convert vegan::envfit output to a tidy data frame
#'
#' @param x the output of the vegan::envfit function
#' @export
tidy_envfit <- function(x){
  out <- as.data.frame(x$vectors$arrows) |>
    tibble::rownames_to_column("var")
  out$r2 <- x$vectors$r
  out$p <- x$vectors$pvals
  return(out)
}

#' summarise blue maestro data
#'
#' @param l a list of csv filenames point to raw blue maestro data
#' @param exclude which sensor numbers to exclude
#' @param start_date exclude measurements before this
#' @param end_date exclude measurements after this
#' @param min_t exclude rows with temperatures below this (typically for error exclusion)
#' @export
bm_summary <- function(l, exclude = NA, start_date = NA, end_date = NA, min_t = NA){
  requireNamespace("stringr")
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  stuff <- list()
  for (i in 1:length(l)){
    print(l[i])
    stuff[[i]] <- topomicro::bm_prep(filename = l[i])
  }

  d <- dplyr::bind_rows(stuff) |>
    dplyr::mutate(number = stringr::str_extract(id, "\\d+") %>% as.numeric(),
           vpd_kPa = topomicro::get_vpd(rh=humidity_pct, temp_c = temperature_c)/10)
  if(!is.na(end_date)) d <- d |> dplyr::filter(dt < as.Date(end_date))
  if(!is.na(start_date)) d <- d |> dplyr::filter(dt > as.Date(start_date))
  if(!is.na(min_t)) d <- d |> dplyr::filter(temperature_c > min_t)
  if(!is.na(exclude[1])){
    d <- d |>
      filter(!number %in% exclude)
  }

  pt <- d |>
    ggplot2::ggplot(aes(x=dt, y=temperature_c)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("Temperature (c)")

  pr <- d |>
    ggplot2::ggplot(aes(x=dt, y=humidity_pct)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("Relative Humidity (pct)")

  dp <- d |>
    ggplot2::ggplot(aes(x=dt, y=dewpoint_c)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("Dewpoint (c)")

  dv <- d |>
    ggplot2::ggplot(aes(x=dt, y=vpd_kPa)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("VPD (kPA)")

  d_all <- d |>
    tidyr::pivot_longer(cols = c(temperature_c, humidity_pct, vpd_kPa, dewpoint_c)) |>
    ggplot2::ggplot(aes(x=dt, y=value, color = as.factor(id))) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::facet_wrap(~name, scales = "free", ncol=1) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::ggtitle("higher number = higher elevation")

  return(list(temp_c = pt,
              rh_pct = pr,
              dewp_c = dp,
              vpd_kPa = dv,
              all = d_all,
              data = d))
}

#' get a raster of latitude values from a dem without reprojecting
#' @param dem a digital elevation model in SpatRaster format
#' @export
get_latitude_raster <- function(dem){
  requireNamespace("terra")
  requireNamespace("sf")
  requireNamespace("dplyr")
  requireNamespace("terra")
  requireNamespace("magrittr")

  dem |>
    as.data.frame(xy=TRUE, cell=T) |>
    sf::st_as_sf(coords = c("x", "y"), crs =sf::st_crs(dem)) |>
    sf::st_transform(crs=4326) %>%
    dplyr::mutate(latitude = sf::st_coordinates(.)[,2]) |>
    sf::st_set_geometry(NULL) |>
    dplyr::left_join(as.data.frame(dem, cell=T, xy = TRUE)) |>
    dplyr::select(x,y, latitude) |>
    terra::rast(type = 'xyz', crs = terra::crs(dem, proj=T))
}

#' calculates Heat Load Index (McCune and Keon 2002)
#' Code adapted from K.C. Rodman
#' @param dem a digital elevation model in SpatRaster format
#' @export
get_hli <- function(dem){

  requireNamespace("terra")
  # getting slope, converting to aspect, setting extreme highs and lows to fall
  # within the boundaries of the HLI calculation
  slope <- terra::terrain(dem, v="slope")
  slope[slope > 60] <- 60
  slope[slope < 0] <- 0
  slope <- slope * 0.017453293
  aspect <- terra::terrain(dem, v="aspect", unit = "radians")
  latitude_radians <-  get_latitude_raster(dem) * 0.017453293
  cosine_latitude <- cos(latitude_radians)
  sine_latitude <- sin(latitude_radians)
  folded_aspect <- abs(pi - abs(aspect - (5*pi/4)))
  sine_slope <- sin(slope)
  cosine_slope <- cos(slope)
  cosine_fa <- cos(folded_aspect)
  return(
    0.339 + (0.808 * (cosine_latitude * cosine_slope)) -
      (0.196 *(sine_latitude * sine_slope)) - (0.482 * (cosine_fa * sine_slope))
  )
}

#' Tidily create a spatial process model
#'
#' @param sf_df sf-style point layer with predictor and response variable
#' @param vars character vector of predictor variables
#' @param y name of response variable
#'
#'
#' @export
tidy_sp <- function(sf_df, vars, y, ...){
  requireNamespace('sf')
  requireNamespace('dplyr')
  requireNamespace('fields')
  x = sf::st_coordinates(sf_df)[,c(1,2)]
  z = dplyr::select(sf_df, vars) |>
    sf::st_set_geometry(NULL)
  y = dplyr::pull(sf_df, y)

  spmod <- fields::spatialProcess(x=x, y=y, Z=z,
                                  profileLambda = TRUE,
                                  profileARange = TRUE, ...)

  return(spmod)
}

#' apply predictions of a spatial process model to a raster
#'
#' @param rast_stack a raster stack with all the predictor variables
#' @param spmod a spatial process model object
predict_sp <- function(rast_stack, spmod){
  requireNamespace('terra')
  requireNamespace('sf')
  elv_df <- rast_stack |>
    as.data.frame(xy=TRUE) |>
    na.omit()
  lat <- elv_df$y
  lon <- elv_df$x
  elev <- elv_df[,3:4]
  xps <-cbind(lon, lat)
  yp = predict(spmod,
               x=xps,Z=elev)
  spmod_rast <- terra::rast(data.frame(x=lon, y=lat, z = yp),
                                      crs = sf::st_crs(rast_stack))
  return(spmod_rast)
}

