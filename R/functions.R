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

#' summarise blue maestro data to the plot scale
#'
#' @param df a data frame created by the bm_prep function
#' @export
plotwise_summary <- function(df){
  requireNamespace("dplyr")
  requireNamespace("tibble")
  df |>
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
  es <- topomicro::get_es(temp_c)
  ## calculate vapor pressure deficit
  vpd <- ((100 - rh) / 100) * es
  return(vpd)
}

# wish list
# delineate watersheds
# do a hypsogram
# summary of landfire/nlcd data

hypsogram <- function(dem, watershed=NA){
  # clip to watershed

  terra::get_values(dem) |>
    as.data.frame() |>
    ggplot() +
    geom_histogram()
}

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
