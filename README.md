# picarroprocessing

This repository contains the scripts to harmonize and process field data collected with the Picarro Gas Scouter.
Users need to specify a path to a file or folder needed to be processed, and the scripts harmonize and structure the data in a systematic manner.
Then, fluxes are being processed with different approaches inspired from the R package GoFluxYourself (https://github.com/Qepanna/GoFluxYourself/tree/master).
Data and fluxes estimates are saved locally, not on the remote repository.

## Install
Clone this repository on your computer, and then you'll need to install the different packages.
To do so, run the "000_install_dependencies.R" script in your R Studio.
If prompted, it is recommended to update any pre-installed packages.


## Process your data
Make sure you properly add raw data and fieldsheet to your data folder.
The main script to process Picarro data from raw field data to automatically processed fluxes is "raw2flux.R"

The main script to compare automatic processing versus expert manual selection of the data is "expert_vs_automatic_processing.R"
