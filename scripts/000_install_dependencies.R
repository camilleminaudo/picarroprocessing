


# ---- packages ----
# install.packages(c('tidyverse','readxl','lubridate','zoo','ggplot2','grid','egg','dplyr','purrr','msm'))

if (!require("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("Qepanna/GoFluxYourself")



data.table, dplyr, ggnewscale, ggplot2, graphics, grDevices, grid, gridExtra, egg,
  lubridate, minpack.lm, msm, pbapply, plyr, purrr, readxl, rjson, rlist, SimDesign,
  stats, tibble, tidyr, utils)


install.packages("pacman")
# Then load packages. Pacman will check whether each package has been installed, and if not, will install it automatically.

pacman::p_load("stockPortfolio","quadprog")
