# package-wide setup


# solving the note on unused imports (we do in fact use these packages for)
ignore_unused_imports <- function(){
  utils::globalVariables
  magrittr::add
  raster::raster
  elevatr::get_elev_raster
  methods::as
}

# solving the note on global variables (a symptom of using dplyr a lot)
utils::globalVariables(c("read.csv", "year", "month",'%>%', ".", 'mday', 'hms',
                         "dt", 'temperature', 'humidity', 'dewpoint', "demr",
                         'rmin', 'rmax', 'humidity_pct', 'temperature_c'))
