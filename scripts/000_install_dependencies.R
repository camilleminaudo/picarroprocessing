



if (!require("pacman", quietly = TRUE))
  install.packages("pacman")

# Then load packages. Pacman will check whether each package has been installed, and if not, will install it automatically.

pacman::p_load("data.table", "dplyr",  "egg", "ggnewscale", "ggplot2", "graphics", "grDevices", "grid", "gridExtra",
               "lubridate", "minpack.lm", "msm", "pbapply", "plyr", "purrr", "readxl", "remotes","rjson", "rlist", "SimDesign",
               "stats", "tibble", "tidyr", "tools","utils")

remotes::install_github("Qepanna/GoFluxYourself")


