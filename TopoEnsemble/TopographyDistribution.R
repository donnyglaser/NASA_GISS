## goal: create distribution plots of 'continents' vs latitude ##
## %area on y axis, lat on x axis ##

## Donny Glaser

## inputs: summary equilibrium aij files (50 year ANN aij)
## local directory w/ summary aij files:
setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Topo Ensemble/Data/SumData')

## outputs: 
# 1) table with rotation angle, water content, lat, %area
# 2) histogram plots of %area on y axis, lat on x axis

# libraries
library(tidyverse)
library(ncdf4)

# find all aij files
aij <- list.files(getwd(), pattern = '.aij')
aijk <- subset(aij, grepl('aijk', aij) == TRUE)
aijl <- subset(aij, grepl('aijl', aij) == TRUE)
aij <- subset(aij, grepl('aijk', aij) == FALSE)
aij <- subset(aij, grepl('aijl', aij) == FALSE)

## only considering the 50/50 land/ocean scenario (highland/lowland dichotomy)
aij <- subset(aij, grepl('_0x50_', aij) == TRUE | grepl('_90x50_', aij) == TRUE)

# lat lon for 4x5
latList <- seq(-90,90,4)
lonList <- seq(-177.5,177.5,5)

for(ifile in 1:length(aij)) {
    nc <- nc_open(aij[ifile])

    # topography
    land <- ncvar_get(nc, varid = 'landfr') # m above 'sea level'
    row.names(land) <- lonList
    colnames(land) <- latList
    land.m <- melt(land)
    colnames(land.m) <- c('lon', 'lat', 'landfr')

    # grid area
    area <- ncvar_get(nc, varid = 'axyp') # area in m^2
    row.names(area) <- lonList
    colnames(area) <- latList
    area.m <- melt(area)
    colnames(area.m) <- c('lon', 'lat', 'area')
    area.t <- sum(area.m$area)

    lxa <- merge(land.m, area.m, by = c('lon', 'lat'))
    lxa$landfr <- round(lxa$landfr, digits = 0)
    lxa <- subset(lxa, landfr == 100)
    lxa$lat <- abs(lxa$lat)
    lxa.agg <- aggregate(lxa$area, by = list(lat = lxa$lat), FUN=sum)
    colnames(lxa.agg)[2] <- 'area'
    land.t <- sum(lxa.agg$area)
    lxa.agg <- lxa.agg %>% mutate(areaPCT = area / land.t)

    ## left off here ##
    ## punchline: ~50% more 'land' above 60º in 90 compared to 0
    # now only consider 'land' above 60º(??)
    # export table
    # make plots
}