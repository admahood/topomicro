#' Read in and wrangle data from Blue Maestro sensors
#'
#' @param filename path to the csv file obtained from the blue maestro app.
#' @param time_zone time zone
#' @export
bm_prep <- function(filename, time_zone = "US/Mountain"){
  requireNamespace("readr")
  requireNamespace("tibble")
  requireNamespace("dplyr")
  requireNamespace('janitor')
  requireNamespace("lubridate")

  if(stringr::str_detect(filename, "log")) bm_gen <- "old" else bm_gen <- 'new'

  if(bm_gen == 'old'){
    top <- readr::read_csv(filename,n_max = 1) |>
      janitor::clean_names()
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
      dplyr::mutate(id = top$name,
                    device_id = top$device_id,
             dl_date = top$download_date,
             vpd_kPa = topomicro::vpd(rh = humidity_pct,
                                          temp_c = temperature_c))
    lubridate::tz(bottom$dt) <- time_zone

    bottom <- bottom |> dplyr::select(id,
                                      device_id,
                                     index,
                                     dt,
                                     tz,
                                     ymd,
                                     temperature_c,
                                     humidity_pct,
                                     dewpoint_c,
                                     vpd_kPa)

  }else{
    path_split <- stringr::str_split(filename, pattern = "/", simplify = T)
    filename_only <- path_split[1, length(path_split)]
    filename_split <- stringr::str_split(stringr::str_remove_all(filename_only, '.csv'), pattern = '_', simplify = T)

    bm_name <- filename_split[,1]
    bm_id <- filename_split[,2]
    bm_dl_date <- filename_split[,3]

    bottom0 <- readr::read_csv(filename) |>
      janitor::clean_names() |>
      dplyr::rename(dt = date_time)

    if(class(bottom0$dt)[1] == "character") bottom0$dt <- lubridate::parse_date_time(bottom0$dt, "%m/%d/%Y HM")

    bottom <- bottom0 |>
      dplyr::transmute(id = bm_name,
                       device_id = bm_id,
                index = 1:nrow(bottom0),
                dt = dt,
                ymd = lubridate::as_date(dt),
                tz = time_zone,
                temperature_c = (temperature - 32)*(5/9),
                humidity_pct = humidity,
                dewpoint_c = (dew_point - 32)*(5/9),
                vpd_kPa = topomicro::vpd(rh = humidity_pct,
                                         temp_c = temperature_c))
  }
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
#' @param temp_c temperature
#' @export
e_sat <- function(temp_c){
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
vpd <- function(rh, temp_c){
  ## calculate saturation vapor pressure
  es <- topomicro::e_sat(temp_c)
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
           vpd_kPa = topomicro::vpd(rh=humidity_pct, temp_c = temperature_c)/10)
  if(!is.na(end_date)) d <- d |> dplyr::filter(dt < as.Date(end_date))
  if(!is.na(start_date)) d <- d |> dplyr::filter(dt > as.Date(start_date))
  if(!is.na(min_t)) d <- d |> dplyr::filter(temperature_c > min_t)
  if(!is.na(exclude[1])){
    d <- d |>
      dplyr::filter(!number %in% exclude)
  }

  pt <- d |>
    ggplot2::ggplot(ggplot2::aes(x=dt, y=temperature_c)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("Temperature (c)")

  pr <- d |>
    ggplot2::ggplot(ggplot2::aes(x=dt, y=humidity_pct)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("Relative Humidity (pct)")

  dp <- d |>
    ggplot2::ggplot(ggplot2::aes(x=dt, y=dewpoint_c)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("Dewpoint (c)")

  dv <- d |>
    ggplot2::ggplot(ggplot2::aes(x=dt, y=vpd_kPa)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~id, ncol=1) +
    ggplot2::ggtitle("VPD (kPA)")

  d_all <- d |>
    tidyr::pivot_longer(cols = c(temperature_c, humidity_pct, vpd_kPa, dewpoint_c)) |>
    ggplot2::ggplot(ggplot2::aes(x=dt, y=value, color = as.factor(id))) +
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


#' Tidily create a spatial process model
#'
#' @param sf_df sf-style point layer with predictor and response variable
#' @param vars character vector of predictor variables
#' @param y name of response variable
#' @param ... arguments passed to fields::spatialProcess()
#'
#' @examples
#' x<-9
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

