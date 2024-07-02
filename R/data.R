#' Weather Station Data for Manitou Experimental Forest
#'
#' @format ## 'mef_station_data'
#' A simple feature collection with 59 features and 8 fields
#' \describe{
#'  \item{temperature_c}{2 meter air temperature in degrees Celsius}
#'  \item{humidity_pct}{Relative Humidity at 2 meters}
#'  \item{timestamp_ts}{datetime, at one hour increments}
#'  \item{vpd_kPa}{Vapor pressure deficit in kilopascals at 2 meters}
#'  \item{ymd}{date: year month day}
#' }
"mef_station_data"

#' Microclimate Sensor Data for 26 locations at Manitou Experimental Forest
#'
#' @format ## 'mef_micro_data'
#' A simple feature collection with 59 features and 8 fields
#' \describe{
#'  \item{temperature_c}{2 meter air temperature in degrees Celsius}
#'  \item{humidity_pct}{Relative Humidity at 2 meters}
#'  \item{timestamp_ts}{datetime, at one hour increments}
#'  \item{vpd_kPa}{Vapor pressure deficit in kilopascals at 2 meters}
#'  \item{ymd}{date: year month day}
#' }
"mef_micro_data"
