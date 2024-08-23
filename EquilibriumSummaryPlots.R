### make scatterplots and maps of equilibrium sum data ###
### iterate thru all summary aij files, extract hemis data for each variable, ###
### and make maps for each variable ###

### maybe I already did this... ###

library(mapproj) # global map projection library
library(tidyverse)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(reshape)
library(viridis)


setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Topo Ensemble/Data/SumData')

dirName <- paste0('AllSimsSummary_', format(Sys.time(), "%y%m%d"))
dir.create(dirName)
dir.create(paste0(dirName, '/Plots'))

runList <- list.files(getwd(), pattern = glob2rx('TopoEns_*.rds'))
diags <- c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')

labs <- c('Temperature (ºC)', 'Water Vapor Column (kg/m^2)', 'Net Planetary Radiation (W/m^2)', 'Earth Water (kg/m^2)', 'Earth Ice (kg/m^2)', 'Precipitation (mm/day)', 'Potential Evaporation (mm/day)', 'Ocean/Lake Ice Thickness (m)', 'Snow Depth (mm H2O)')
names(labs) <- c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')
