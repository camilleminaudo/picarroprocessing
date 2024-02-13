# ---
# Authors: Camille Minaudo
# Project: "RESTORE4Cs"
# date: "Feb 2024"
# https://github.com/camilleminaudo/restore4cs-scripts
# ---

# --- Description of this script
# This script loads and processes fluxes estimates from randomly chosen incubations available in the dataset. 
# The user is invited to select what looks like "good" data, getting rid of obvious outliers, ...etc.
# The scripts then computes fluxes blindly without any fancy QAQC, and flux estimates of the Blind versus Expert approaches are compared.


rm(list = ls()) # clear workspace
cat("/014") # clear console



############################3
# SPECIFY HERE YOUR NAME
username <- "Camille"

nb_draw <- 10

load_data <- F

############################

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
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}



# ---- Directories ----
datapath <- paste0(repo_root,"/data")
rawdatapath <- paste0(datapath,"/raw_data/RAW Data Picarro")
fieldsheetpath <- paste0(datapath,"/fieldsheets")
# corrfieldsheetpath <- paste0(datapath,"/GHG/Processed data/corrected_fieldsheets")
loggerspath <- paste0(datapath,"/raw_data/RAW Data Logger")
plots_path <- paste0(repo_root,"/results/plots")
results_path <- paste0(repo_root,"/results/processed")



# ---- List GHG chamber fieldsheets in data folder and read them ---
# list filenames
myfieldsheets_list <- list.files(fieldsheetpath, pattern = "-Fieldsheet-GHG.xlsx", all.files = T, full.names = T, recursive = T)
# Read all fieldsheets and put them in a single dataframe
fieldsheet <- read_GHG_fieldsheets(myfieldsheets_list)




# ---- Import and store measurements to RData ----
if (load_data){
  data_folders <- list.dirs(rawdatapath, full.names = T, recursive = T)[-1]
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
  
}



# ---- Correct fieldsheet  times based on "incubation map" ----

read_map_incubations <- function(path2folder){
  
  my_maps_filenames <- list.files(path2folder, pattern = ".csv", all.files = T, full.names = T, recursive = T)
  isF <- T
  for(my_maps_filename in my_maps_filenames){
    
    map_incubations_temp <- read.csv(file = my_maps_filename,
                                     header = T)
    map_incubations_temp <- map_incubations_temp[map_incubations_temp$Species ==  "CH4",]
    map_incubations_temp$subsite <- basename(dirname(my_maps_filename))
    
    if(isF){
      isF <- F
      map_incubations <- map_incubations_temp
    } else {
      map_incubations <- rbind(map_incubations, map_incubations_temp)
    }
  }
  
  map_incubations <- data.frame(subsite =map_incubations$subsite,
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

# saving this is likely unnecessary
# setwd(paste0(results_path,"/corrected_fieldsheets"))
# f_name <- paste0("Fieldsheet-GHG_corrected.csv")
# write.csv(file = f_name, x = fieldsheet, row.names = F)


###########################################################################

# draw a few incubations randomly
draw <- sample(seq_along(fieldsheet$subsite), nb_draw)

table_draw <- data.frame(draw = draw,
                         subsite = fieldsheet$subsite[draw],
                         ID = paste(fieldsheet$subsite[draw], fieldsheet$plot_id[draw],fieldsheet$strata[draw],fieldsheet$transparent_dark[draw],sep = "-"))

###########################################################################


# ---- Go through each incubation selected and build auxfile table ----
subsites <- unique(table_draw$subsite)

auxfile <- NULL

for (subsite in subsites){
  
  corresp_fs <- fieldsheet[fieldsheet$subsite == subsite,]
  gs <- first(corresp_fs$gas_analyzer)
  corresp_fs <- fieldsheet[fieldsheet$subsite == subsite,]
  corresp_fs$unix_start <- corresp_fs$unix_start_corrected
  corresp_fs$unix_stop <- corresp_fs$unix_stop_corrected
  corresp_fs <- corresp_fs[!is.na(corresp_fs$unix_start),]
  
  UniqueID <- paste(subsite, seq_along(corresp_fs$pilot_site),corresp_fs$strata,corresp_fs$transparent_dark,sep = "-")
  corresp_fs$UniqueID = gsub(" ", "", UniqueID, fixed = TRUE)
  
  corresp_fs$ID <-paste(subsite, corresp_fs$plot_id,corresp_fs$strata,corresp_fs$transparent_dark,sep = "-")
  
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
    
    incubs <- match(table_draw$ID[which(table_draw$subsite == subsite)], corresp_fs$ID)
    
    # looking at which incubations corresponds to the one randomly chosen
    for (incub in incubs){
      my_incub <- mydata[as.numeric(mydata$POSIX.time)> corresp_fs$unix_start[incub] &
                           as.numeric(mydata$POSIX.time)< corresp_fs$unix_stop[incub],]
      my_incub <- my_incub[!is.na(my_incub$CO2dry_ppm),]
      
      if (dim(my_incub)[1]>0){
        
        # Compute Temperature, Area and Volume from fieldsheet info
        myTemp <- 15 # a default temperature to run the flux calculation...
        if (corresp_fs$chamber_type[incub] == "floating"){
          if(is_data_logger_float){
            myTemp <- median(approx(data_logger_float$unixtime, data_logger_float$temperature, xout = as.numeric(my_incub$POSIX.time))$y )
          }
          myArea = 14365.4439 # cm2
          myVtot = 115/2 # L
        } else if (corresp_fs$chamber_type[incub] == "tube"){
          if(is_data_logger_tube){
            myTemp <- median(approx(data_logger_tube$unixtime, data_logger_tube$temperature, xout = as.numeric(my_incub$POSIX.time))$y )
          }
          myArea = pi*12.1**2 # cm2
          myVtot = myArea*as.numeric(corresp_fs$chamber_height_cm[incub])*1e-3 # L
        } else {
          warning("chamber type not correct!")
        }
        myPcham <- 100.1 #kPa
        
        # --- Create auxfile table ----
        # An auxfile table, made of fieldsheet, adding important variables. The auxfile
        # requires start.time and UniqueID.
        # start.time must be in the format "%Y-%m-%d %H:%M:%S"
        
        auxfile_tmp <- data.frame(username = username,
                                  subsite = subsite,
                                  UniqueID = corresp_fs$UniqueID[incub],
                                  gas_analiser = gs,
                                  start.time = as.POSIXct((corresp_fs$unix_start[incub]), tz = "UTC"),
                                  duration = (corresp_fs$unix_stop[incub]) - (corresp_fs$unix_start[incub]),
                                  water_depth = corresp_fs$water_depth[incub],
                                  Area = myArea,
                                  Vtot = myVtot,
                                  Tcham = myTemp,
                                  Pcham = myPcham,
                                  strata = corresp_fs$strata[incub],
                                  chamberType = corresp_fs$chamber_type[incub],
                                  lightCondition = corresp_fs$transparent_dark[incub])
        if(is.null(auxfile)){
          auxfile <- auxfile_tmp
          mydata_all <- mydata
        } else {
          auxfile <- rbind(auxfile, auxfile_tmp)
          mydata_all <- rbind(mydata_all, mydata)
        }
        
      }
    }
  }
}





# ---- Process incubations timeseries and compute fluxes ----


# Define the measurements' window of observation
myauxfile <- auxfile
mydata_ow <- obs.win(inputfile = mydata_all, auxfile = myauxfile,
                     obs.length = myauxfile$duration, shoulder = 2)


# ----------- Compute fluxes after manual selection of CO2 data

# Manually identify measurements by clicking on the start and end points
mydata_manID <- lapply(seq_along(mydata_ow), click.peak.loop,
                       flux.unique = mydata_ow,
                       gastype = "CO2dry_ppm",
                       plot.lim = c(200,1000)) %>%
  map_df(., ~as.data.frame(.x))


# Additional auxiliary data required for flux calculation.
mydata_manID <- mydata_manID %>%
  left_join(auxfile %>% select(username, UniqueID, Area, Vtot, Tcham, Pcham))

# Add instrument precision for each gas
prec = c(3.5, 0.6, 0.4, 45, 45)
mydata_manID <- mydata_manID %>%
  mutate(CO2_prec = prec[1], CH4_prec = prec[2], N2O_prec = prec[3],
         H2O_prec = prec[4])


# Calculate fluxes
CO2_results_manID <- goFlux(mydata_manID, "CO2dry_ppm")
H2O_results_manID <- goFlux(mydata_manID, "H2O_ppm")
CH4_results_manID <- goFlux(mydata_manID, "CH4dry_ppb")

# Use best.flux to select the best flux estimates (LM or HM)
# based on a list of criteria
criteria <- c("g.factor", "kappa", "MDF", "R2", "SE.rel")

CO2_flux_res_manID <- best.flux(CO2_results_manID, criteria)
H2O_flux_res_manID <- best.flux(H2O_results_manID, criteria)
CH4_flux_res_manID <- best.flux(CH4_results_manID, criteria)

# CO2_flux_res_manID <- CO2_flux_res_manID %>%
#   left_join(myauxfile %>% select(UniqueID, strata, chamberType, lightCondition))


# Plots results
# Make a list of plots of all measurements, for each gastype
# CO2_flux_plots <- flux.plot(CO2_flux_res_manID, mydata_manID, "CO2dry_ppm")
# H2O_flux_plots <- flux.plot(H2O_flux_res_manID, mydata_manID, "H2O_ppm")
# CH4_flux_plots <- flux.plot(CH4_flux_res_manID, mydata_manID, "CH4dry_ppb")

# ----------- Compute fluxes blindly without any manual selection

mydata_ow <- obs.win(inputfile = mydata_all, auxfile = myauxfile,
                     obs.length = myauxfile$duration, shoulder = 2)

# Join mydata_ow with info on start end incubation
mydata_auto <- lapply(seq_along(mydata_ow), join_auxfile_with_data.loop, flux.unique = mydata_ow) %>%
  map_df(., ~as.data.frame(.x))

# Additional auxiliary data required for flux calculation.
mydata_auto <- mydata_auto %>%
  left_join(myauxfile %>% select(username, UniqueID, Area, Vtot, Tcham, Pcham))

# Add instrument precision for each gas
prec = c(3.5, 0.6, 0.4, 45, 45)
mydata_auto <- mydata_auto %>%
  mutate(CO2_prec = prec[1], CH4_prec = prec[2], N2O_prec = prec[3],
         H2O_prec = prec[4])

# Calculate fluxes
CO2_results_auto <- goFlux(dataframe = mydata_auto, gastype = "CO2dry_ppm")
H2O_results_auto <- goFlux(mydata_auto, "H2O_ppm")
CH4_results_auto <- goFlux(mydata_auto, "CH4dry_ppb")

# Use best.flux to select the best flux estimates (LM or HM)
# based on a list of criteria
criteria <- c("g.factor", "kappa", "MDF", "R2", "SE.rel")

CO2_flux_res_auto <- best.flux(CO2_results_auto, criteria)
H2O_flux_res_auto <- best.flux(H2O_results_auto, criteria)
CH4_flux_res_auto <- best.flux(CH4_results_auto, criteria)


# Use best.flux to select the best flux estimates (LM or HM)
# based on a list of criteria
criteria <- c("g.factor", "kappa", "MDF", "R2", "SE.rel")

CO2_flux_res_auto <- best.flux(CO2_results_auto, criteria)
H2O_flux_res_auto <- best.flux(H2O_results_auto, criteria)
CH4_flux_res_auto <- best.flux(CH4_results_auto, criteria)


CO2_flux_res_auto$flux_method <- "Blind"
CO2_flux_res_manID$flux_method <- "Expert"

CH4_flux_res_auto$flux_method <- "Blind"
CH4_flux_res_manID$flux_method <- "Expert"




# ----------- Estimate CH4 diffusion/ebullition contributions
# method 1: density of prob. of first derivative
# method 2: manual selection of linear pattern in CH4

# estimate ch4 diffusion and ebullition components---------| METHOD 1 |-----------
CH4_res_meth1 <- CH4_flux_res_auto
CH4_res_meth1$total_estimated <- NA
CH4_res_meth1$ebullition <- NA
CH4_res_meth1$diffusion <- NA


for (i in seq_along(auxfile$subsite)){
  if(auxfile$water_depth[i]>0){
    my_incub <- mydata_all[as.numeric(mydata_all$POSIX.time)> auxfile$start.time[i] &
                             as.numeric(mydata_all$POSIX.time)< auxfile$start.time[i]+auxfile$duration[i],]
    my_incub <- my_incub[!is.na(my_incub$CO2dry_ppm),]
    # calling dedicated function
    df_ebull <- separate_ebullition_from_diffusion(my_incub = my_incub, UniqueID = auxfile$UniqueID[i])
    # computing fluxes
    H2O_mol = my_incub$H2O_ppm / (1000*1000)
    myfluxterm <- flux.term(auxfile$Vtot[i], auxfile$Pcham[i], auxfile$Area[i],
                            auxfile$Tcham[i], first(H2O_mol))
    CH4_flux_total <- df_ebull$delta_ch4/df_ebull$duration*myfluxterm # nmol/m2/s
    CH4_flux_diff <- df_ebull$avg_diff_slope*myfluxterm # nmol/m2/s
    CH4_flux_ebull <- CH4_flux_total - CH4_flux_diff
  } else {
    CH4_flux_total <- CH4_flux_diff <- CH4_res_meth1$best.flux[which(CH4_res_meth1$UniqueID==auxfile$UniqueID[i])]
    CH4_flux_ebull <- 0
  }
  CH4_res_meth1$total_estimated[which(CH4_res_meth1$UniqueID==auxfile$UniqueID[i])] <- CH4_flux_total
  CH4_res_meth1$ebullition[which(CH4_res_meth1$UniqueID==auxfile$UniqueID[i])] <- CH4_flux_ebull
  CH4_res_meth1$diffusion[which(CH4_res_meth1$UniqueID==auxfile$UniqueID[i])] <- CH4_flux_diff
}






# Estimate ch4 diffusion and ebullition components ---------| METHOD 2 |-----------


# Manually identify diffusive (more or less linear) CH4 behaviors by clicking on the start and end points
myCH4_diffusion <- lapply(seq_along(mydata_ow), click.peak.loop,
                          flux.unique = mydata_ow,
                          gastype = "CH4dry_ppb",
                          plot.lim = c(1900,max(fieldsheet$final_ch4)*1000)) %>%
  map_df(., ~as.data.frame(.x))


myCH4_diffusion <- myCH4_diffusion %>%
  mutate(CO2_prec = prec[1], CH4_prec = prec[2], N2O_prec = prec[3],
         H2O_prec = prec[4])


# Calculate fluxes for CH4
CH4_results_diffusion <- goFlux(myCH4_diffusion, "CH4dry_ppb")

# Use best.flux to select the best flux estimates (LM or HM)
# based on a list of criteria
criteria <- c("g.factor", "kappa", "MDF", "R2", "SE.rel")

CH4_res_diff <- best.flux(CH4_results_diffusion, criteria)


CH4_res_meth2 <- CH4_flux_res_manID
CH4_res_meth2$total_estimated <- NA
CH4_res_meth2$ebullition <- NA
CH4_res_meth2$diffusion <- CH4_res_diff$best.flux


# Estimating ebullition component
for (id in unique(CH4_res_meth2$UniqueID)){
  i <- which(CH4_res_meth2$UniqueID == id)
  
  CH4_final <- CH4_flux_res_manID$Ct[i]
  CH4_initial <-  CH4_flux_res_manID$C0[i]
  incubation_time <- myauxfile$duration[which(myauxfile$UniqueID == id)]
  CH4_res_meth2$total_estimated[i] <- (CH4_final-CH4_initial)/incubation_time*CH4_flux_res_manID$flux.term[i] # nmol/m2/s
  CH4_res_meth2$ebullition[i] <- CH4_res_meth2$total_estimated[i]  - CH4_res_meth2$diffusion[i] # total flux - diffusive term
}


# compare method 1 and 2 to estimate ebullitive contribution
CH4_res_meth1$flux_method <- "dydt"
CH4_res_meth2$flux_method <- "Expert"


CO2_flux_res_auto$variable <- "CO2"
CO2_flux_res_manID$variable <- "CO2"
CH4_flux_res_auto$variable <- "CH4"
CH4_flux_res_manID$variable <- "CH4"


table_results <- rbind(CO2_flux_res_auto, CO2_flux_res_manID, CH4_flux_res_auto, CH4_flux_res_manID)

table_results_ebull <- rbind(CH4_res_meth1, CH4_res_meth2)


# saving fluxes estimates
setwd(results_path)

append_if_exists <- function(filename, data){
  if(file.exists(filename)){
    data <- rbind(read.csv(filename), data)
  } else {
    write.csv(x = data, file = filename, 
              row.names = F)
  }
  return(data)
}


filename <- paste0("BLIND_vs_EXPERT_co2_ch4_fluxes_",Sys.Date(),".csv")
table_results <- append_if_exists(filename, data = table_results)


filename <- paste0("BLIND_vs_EXPERT_ch4_ebullition_",Sys.Date(),".csv")
table_results_ebull <- append_if_exists(filename, data = table_results_ebull)



#----- some plots -----



p_auto_vs_manual <- ggplot(data = table_results)+
  geom_abline(slope = 0,intercept = 0, color = 'lightgrey')+
  # geom_segment(data = data.frame(UniqueID = CO2_flux_res_auto$UniqueID,
  #                                meth1 = CO2_flux_res_auto$best.flux,
  #                                meth2 = CO2_flux_res_manID$best.flux), aes(x=UniqueID, xend=UniqueID, y = meth1, yend = meth2), linewidth=1, alpha = 0.5)+
  geom_point(aes(UniqueID, best.flux, colour = flux_method), size=4, alpha = 0.5)+
  ylab("flux [(mmolCO2 or nmolCH4)/m2/s]")+
  theme_article()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_colour_viridis_d(begin = 0.1, end = 0.9, option = "F")+facet_wrap(variable~., scales = 'free')

p_auto_vs_manual


p_auto_vs_manual_ch4_ebullition <- ggplot(data = table_results_ebull)+
  geom_abline(slope = 0,intercept = 0, color = 'lightgrey')+
  geom_segment(data = data.frame(UniqueID = CH4_res_meth1$UniqueID,
                                 meth1 = CH4_res_meth1$ebullition,
                                 meth2 = CH4_res_meth2$ebullition), aes(x=UniqueID, xend=UniqueID, y = meth1, yend = meth2), size=1, alpha = 0.5)+
  geom_point(aes(UniqueID, ebullition, colour = flux_method), size=4, alpha=0.5)+
  ylab("ebullition component [nmol/m2/s]")+
  theme_article()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+scale_colour_viridis_d(begin = 0.1, end = 0.9, option = "F")

p_auto_vs_manual_ch4_ebullition




