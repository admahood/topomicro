# package-wide setup


# solving the note on unused imports (we do in fact use these packages for)
ignore_unused_imports <- function(){
  utils::globalVariables
  ggplot2::ggplot
  ggpubr::ggarrange
  sf::st_read
  magrittr::add
}

# solving the note on global variables (a symptom of using dplyr a lot)
utils::globalVariables(c("read.csv", "year", "month",'%>%'))
