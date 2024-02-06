
# ---
# Authors: Camille Minaudo
# Project: "RESTORE4Cs"
# date: "Janv 2024"
# https://github.com/camilleminaudo/picarroprocessing
# ---

# --- Description of this script
# This script transforms raw data of the Picarro gas scouter analyser into fluxes


rm(list = ls()) # clear workspace
cat("/014") # clear console


# ---- packages ----
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(grid)
library(egg)
library(GoFluxYourself)
require(dplyr)
require(purrr)
require(msm)
require(pbapply)


repo_root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))


source(paste0(repo_root,"/scripts/get_unix_times.R"))
source(paste0(repo_root,"/scripts/read_GHG_fieldsheets.R"))
source(paste0(repo_root,"/scripts/flux.term.R"))
source(paste0(repo_root,"/scripts/LM.flux.R"))
source(paste0(repo_root,"/scripts/import2RData.R"))



# ---- Directories ----
datapath <- paste0(repo_root,"/data")
rawdatapath <- paste0(datapath,"/raw_data/RAW Data Picarro")
fieldsheetpath <- paste0(datapath,"/fieldsheets")
# corrfieldsheetpath <- paste0(datapath,"/GHG/Processed data/corrected_fieldsheets")
loggerspath <- paste0(datapath,"/raw_data/RAW Data Logger")
plots_path <- paste0(repo_root,"/results/plots")
results_path <- paste0(repo_root,"/results/processed")


doPlot <- F

# ---- List GHG chamber fieldsheets in data folder and read them ---
# list filenames
myfieldsheets_list <- list.files(fieldsheetpath, pattern = "-Fieldsheet-GHG.xlsx", all.files = T, full.names = T, recursive = T)
# Read all fieldsheets and put them in a single dataframe
fieldsheet <- read_GHG_fieldsheets(myfieldsheets_list)




# ---- Import and store measurements to RData ----
data_folders <- list.dirs(rawdatapath, full.names = T, recursive = F)
r <- grep(pattern = "RData",x=data_folders)
if(length(r)>0){data_folders <- data_folders[-r]}


message("Here is the list of data folders in here:")
print(data_folders)


for (data_folder in data_folders){
  setwd(data_folder)
  subsite = basename(data_folder)
  message(paste0("processing folder ",basename(data_folder)))
  import2RData(path = data_folder, instrument = "G2508",
               date.format = "ymd", timezone = 'UTC')

  # load all these R.Data into a single dataframe
  file_list <- list.files(path = paste(data_folder,"/RData",sep=""), full.names = T)
  isF <- T
  for(i in seq_along(file_list)){
    load(file_list[i])
    if(isF){
      isF <- F
      mydata_imp <- data.raw
    } else {
      mydata_imp <- rbind(mydata_imp, data.raw)
    }
    rm(data.raw)
  }

  # get read of possible duplicated data
  is_duplicate <- duplicated(mydata_imp$POSIX.time)
  mydata <- mydata_imp[!is_duplicate,]

  # creating a folder where to put this data
  dir.create(file.path(results_path, subsite))
  setwd(file.path(results_path, subsite))

  # save this dataframe as a new RData file
  save(mydata, file = paste0("data_",subsite,".RData"))
}



# ---- Correct fieldsheet  times based on "incubation map" ----

read_map_incubations <- function(path2folder){

  my_maps_filenames <- list.files(data_folder, pattern = ".csv", all.files = T, full.names = T, recursive = F)
  isF <- T
  for(my_maps_filename in my_maps_filenames){

    map_incubations_temp <- read.csv(file = my_maps_filename,
                                     header = T)
    map_incubations_temp <- map_incubations_temp[map_incubations_temp$Species ==  "CH4",]

    if(isF){
      isF <- F
      map_incubations <- map_incubations_temp
    } else {
      map_incubations <- rbind(map_incubations, map_incubations_temp)
    }
  }

  map_incubations <- data.frame(subsite = basename(path2folder),
                                plot = map_incubations$Comment,
                                time_code = map_incubations$Time.Code,
                                start = map_incubations$start_fit,
                                stop = map_incubations$end_fit)

  return(map_incubations)
}


# read all the csv files in data_folder, and group into a single one
map_incubations <- read_map_incubations(path2folder = rawdatapath)


# check the closest incubation in map_incubations for each row in fieldsheet.
# if more than 3 minutes apart, we consider the row in map_incubations out of sampling
corresponding_row <- NA*fieldsheet$plot_id

for (i in seq_along(fieldsheet$plot_id)){
  ind <- which.min(abs(fieldsheet$unix_start[i] - map_incubations$start))
  if(abs(fieldsheet$unix_start[i] - map_incubations$start[ind])<3*60){corresponding_row[i] <- ind}
}
corresponding_row <- corresponding_row[!is.na(corresponding_row)]
map_incubations <- map_incubations[corresponding_row, ]

if(dim(map_incubations)[1] != dim(fieldsheet)[1]){
  fieldsheet <- fieldsheet[seq_along(corresponding_row),]
}

fieldsheet$start_time <- format(as.POSIXct(fieldsheet$start_time, tz = 'utc', format = "%T"), "%H:%M:%S")
fieldsheet$end_time <- format(as.POSIXct(fieldsheet$end_time, tz = 'utc', format = "%T"), "%H:%M:%S")

fieldsheet$unix_start_corrected <- map_incubations$start
fieldsheet$unix_stop_corrected <- map_incubations$stop

fieldsheet$timestamp_start <- as.POSIXct(fieldsheet$unix_start_corrected, tz = "UTC", origin = "1970-01-01")
fieldsheet$timestamp_stop <- as.POSIXct(fieldsheet$unix_stop_corrected, tz = "UTC", origin = "1970-01-01")

fieldsheet$start_time_corrected <- strftime(fieldsheet$timestamp_start, format="%H:%M:%S", tz = 'utc')
fieldsheet$end_time_corrected <- strftime(fieldsheet$timestamp_stop, format="%H:%M:%S", tz = 'utc')

setwd(paste0(results_path,"/corrected_fieldsheets"))
f_name <- paste0(subsite,"-Fieldsheet-GHG_corrected.csv")
write.csv(file = f_name, x = fieldsheet, row.names = F)


# ---- Process incubations timeseries and compute fluxes ----

# Function to save a list of plots into pdf file
gg_save_pdf = function(list, filename) {
  pdf(filename)
  for (p in list) {
    print(p)
  }
  dev.off()
  invisible(NULL)
}

# ---- Go through each incubation in fieldsheet and compute linear model for co2 and ch4 ----
subsites <- unique(fieldsheet$subsite)
isF_incub <- T
for (subsite in subsites){
  message("Now processing ",subsite)

  corresp_fs <- fieldsheet[fieldsheet$subsite == subsite,]
  corresp_fs$unix_start <- corresp_fs$unix_start_corrected
  corresp_fs$unix_stop <- corresp_fs$unix_stop_corrected
  corresp_fs <- corresp_fs[!is.na(corresp_fs$unix_start),]

  # read corresponding temperature logger file and keep initial temperature
  # --- Read corresponding Loggers data ----
  SN_logger_float <- first(corresp_fs$logger_floating_chamber)
  SN_logger_tube <- first(corresp_fs$logger_transparent_chamber)
  site_ID <- str_sub(subsite, start = 1, end = 5)

  # finding out if corresponding file exists, and its extension
  require(tools)
  dir_exists_loggdata <- dir.exists(paste0(loggerspath,"/",site_ID,"/"))
  if(dir_exists_loggdata){
    f <- list.files(paste0(loggerspath,"/",site_ID,"/"), full.names = T)
    r <- grep(pattern = ".hobo", x = f)
    if(length(r)>0){f <- f[-r]}
    r <- grep(pattern = ".txt", x = f)
    if(length(r)>0){f <- f[-r]}
    f_ext <- file_ext(f)
    i_f_float <- grep(pattern = SN_logger_float, x = f)[1]
    i_f_tube <- grep(pattern = SN_logger_tube, x = f)[1]
  }

  if(!is.na(SN_logger_float) & !is.na(i_f_float)){
    is_data_logger_float = T
    message("...reading corresponding temperature logger file for floating chamber")
    if(f_ext[i_f_float]=="xlsx"){
      data_logger_float <- readxl::read_xlsx(f[i_f_float],col_names = T)
    } else if(f_ext[i_f_float]=="csv"){
      data_logger_float <- read.csv(f[i_f_float], header = T)
    }
    data_logger_float <- data_logger_float[,seq(1,3)]
    names(data_logger_float) <- c("sn","datetime","temperature")
    data_logger_float$sn <- SN_logger_float
    if(is.character(data_logger_float$datetime)){
      data_logger_float$datetime <- as.POSIXct(data_logger_float$datetime, tz = 'utc', tryFormats = c("%m/%d/%y %r", "%d/%m/%Y %H:%M"))
    }
    data_logger_float$unixtime <- as.numeric(data_logger_float$datetime)
  } else {
    message("===> no data logger linked to the floating chamber!")
    is_data_logger_float = F}

  if(!is.na(SN_logger_tube) & !is.na(i_f_tube)){
    is_data_logger_tube = T
    message("...reading corresponding temperature logger file for tube chamber")
    if(f_ext[i_f_tube]=="xlsx"){
      data_logger_tube <- readxl::read_xlsx(f[i_f_tube],col_names = T)
    } else if(f_ext[i_f_tube]=="csv"){
      data_logger_tube <- read.csv(f[i_f_tube], header = T, fill = T)
    }
    data_logger_tube <- data_logger_tube[,seq(1,4)]
    names(data_logger_tube) <- c("sn","datetime","temperature","light")
    data_logger_tube$sn <- SN_logger_tube
    if(is.character(data_logger_tube$datetime)){
      if(length(grep(pattern = "AM", x = first(data_logger_tube$datetime)))>0 | length(grep(pattern = "PM", x = first(data_logger_tube$datetime)))>0){
        data_logger_tube$datetime <- as.POSIXct(data_logger_tube$datetime, tz = 'UTC', format = c("%m/%d/%y %r"))
      } else {
        data_logger_tube$datetime <- as.POSIXct(data_logger_tube$datetime, tz = 'UTC', format = "%m/%d/%Y %H:%M")
      }
    }
    data_logger_tube$unixtime <- as.numeric(data_logger_tube$datetime)
  } else {
    is_data_logger_tube = F
    message("===> no data logger linked to the tube chamber!")
  }


  path2data <- paste0(results_path,"/",subsite)
  if(dir.exists(path2data)){
    setwd(path2data)
    load(file = paste0("data_",subsite,".RData"))

    if(doPlot){plt_list <- vector('list', length(corresp_fs$plot_id))}

    for (incub in seq_along(corresp_fs$plot_id)){
      my_incub <- mydata[as.numeric(mydata$POSIX.time)> corresp_fs$unix_start[incub] &
                           as.numeric(mydata$POSIX.time)< corresp_fs$unix_stop[incub],]
      my_incub <- my_incub[!is.na(my_incub$CO2dry_ppm),]
      if (dim(my_incub)[1]>0){

        my_incub$elapsed_time <- as.numeric(my_incub$POSIX.time - corresp_fs$unix_start[incub])

        if(doPlot){
          plt_CO2 <- ggplot(my_incub, aes(POSIX.time, CO2dry_ppm))+geom_line()+
            theme_article()+
            xlab("time UTC")+
            ylab("CO2dry [ppm]")+
            ggtitle(paste0(subsite," plot ",
                           corresp_fs$plot_id[incub]," ",corresp_fs$strata[incub]," ",
                           corresp_fs$transparent_dark[incub], ", depth = ",corresp_fs$water_depth[incub], " cm"))
          plt_CH4 <- ggplot(my_incub, aes(POSIX.time, CH4dry_ppb))+geom_line()+
            theme_article()+
            xlab("time UTC")+
            ylab("CH4dry [ppm]")
          plt_H2O <- ggplot(my_incub, aes(POSIX.time, H2O_ppm))+geom_line()+
            theme_article()+
            xlab("time UTC")+
            ylab("H2O [ppm]")

          # plt <- ggarrange(plt_CO2, plt_CH4, plt_H2O, ncol = 1)
          plt_list[[incub]] <- ggarrange(plt_CO2, plt_CH4, plt_H2O, ncol = 1)

        }

        # Compute Temperature, Area and Volume from fieldsheet info
        myTemp <- 15 # a default temperature to run the flux calculation...
        if (corresp_fs$chamber_type[incub] == "floating"){
          if(is_data_logger_float){
            myTemp <- median(approx(data_logger_float$unixtime, data_logger_float$temperature, xout = as.numeric(my_incub$POSIX.time))$y )
          }
          myArea = 14365.4439 # cm2
          myVtot = 115 # L
        } else if (corresp_fs$chamber_type[incub] == "tube"){
          if(is_data_logger_tube){
            myTemp <- median(approx(data_logger_tube$unixtime, data_logger_tube$temperature, xout = as.numeric(my_incub$POSIX.time))$y )
          }
          myArea = pi*12.1**2 # cm2
          myVtot = myArea*as.numeric(corresp_fs$chamber_height_cm[incub])*1e-3 # L
        } else {
          warning("chamber type not correct!")
        }


        # Fit a simple linear model using functions from GoFluxYourself package
        myfluxterm <- flux.term(V_L = myArea,
                                P_kPa = 100,
                                A_cm2 = myArea,
                                T_C = myTemp,
                                H2O_mol = first(my_incub$H2O_ppm)/(1000*1000))
        lm_co2 <- LM.flux(gas.meas = my_incub$CO2dry_ppm,
                          time.meas = my_incub$elapsed_time,
                          flux.term = myfluxterm)
        lm_ch4 <- LM.flux(gas.meas = my_incub$CH4dry_ppb*1e-3, #in ppm
                          time.meas = my_incub$elapsed_time,
                          flux.term = myfluxterm)

        names(lm_co2) <- paste0(names(lm_co2),"_co2")
        names(lm_ch4) <- paste0(names(lm_ch4),"_ch4")

        flux_table_incub <- cbind(corresp_fs[incub,], lm_co2, lm_ch4)

        if(isF_incub) {
          isF_incub <- F
          flux_table_incub_all <- flux_table_incub
        } else {
          flux_table_incub_all <- rbind(flux_table_incub_all, flux_table_incub)
        }
      }
    }
    if(doPlot){
      # Print pdf
      setwd(plots_path)
      gg_save_pdf(list = plt_list, filename = paste0(subsite,".pdf"))
    }
  }

  setwd(path2data)
  flux_table_incub_this_subsite <- flux_table_incub_all[flux_table_incub_all$subsite == subsite,]
  myfilename <- paste("fluxes",subsite,min(flux_table_incub_this_subsite$date), sep = "_")
  write.csv(x = flux_table_incub_this_subsite, file = paste0(myfilename,".csv"), row.names = F)

}

setwd(results_path)
myfilename <- paste("fluxes_all",subsite,min(flux_table_incub_all$date),max(flux_table_incub_all$date), sep = "_")
write.csv(x = flux_table_incub_all, file = paste0(myfilename,".csv"), row.names = F)



# ---- Some plots ----

ggplot(flux_table_incub_all, aes(LM.r2_co2, fill = strata))+geom_density(alpha=0.5)+
  theme_article()

ggplot(flux_table_incub_all, aes(LM.r2_ch4, fill = strata))+geom_density(alpha=0.5)+
  theme_article()


ggplot(flux_table_incub_all[flux_table_incub_all$LM.r2_co2>0.5,], aes(subsite, LM.flux_co2, colour = transparent_dark))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_boxplot()+
  geom_jitter(width = 0.1)+
  facet_grid(strata~.)+
  theme_article()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylab("CO2 flux [µmol/m2/s]")



ggplot(flux_table_incub_all[flux_table_incub_all$LM.r2_ch4>0.5,], aes(subsite, LM.flux_ch4, colour = transparent_dark))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_boxplot()+facet_grid(strata~.)+
  theme_article()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+scale_y_log10()+ylab("CH4 flux [µmol/m2/s]")





ggplot(flux_table_incub_all, aes(pilot_site, LM.flux_co2, colour = transparent_dark))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_boxplot()+facet_grid(strata~.)+
  theme_article()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ylab("CO2 flux [µmol/m2/s]")



ggplot(flux_table_incub_all, aes(pilot_site, LM.flux_ch4, colour = transparent_dark))+
  geom_hline(yintercept = 0, alpha = 0.2)+
  geom_boxplot()+facet_grid(strata~.)+
  theme_article()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_log10()+
  ylab("CH4 flux [µmol/m2/s]")

