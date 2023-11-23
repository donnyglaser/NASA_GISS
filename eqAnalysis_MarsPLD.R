### R script to analyze concatenate all monthly mean aij data into one file for timeseries analysis ###
### run this script in a directory with only AIJ files ###

### target diagnostics that measure snow (water and CO2) deposition and evaporation (and sublimation) ###


### updated (230822) ###
### added poles ###

library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(reshape)

aijList <- list.files(getwd(), pattern = '.aij')
run <- aijList[1]
run <- substring(run, 12, (nchar(run)-3))

dateList <- substring(aijList, 4, 7)
dateList <- unique(as.numeric(dateList))
YearI <- min(dateList)
YearE <- max(dateList)
MonList <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
LatList <- c(90, 78, 62, 46, 30, 14, 2, -14, -30, -46, -62, -78, -90)
VarList <- c("axyp", "bs_snowdp", "gice", "gwtr", "incsw_toa", "landicefr", "mass_CO2cond", "prsurf", "qatm", "snowdp", "srf_wind_dir", "srtrnf_grnd", "tgrnd", "tsurf", "wsurf", "zsnow")

outFrame <- data.frame()

for(i in YearI:YearE) {
    if(i > 99){
        yearL <- aijList[grepl(paste0('0', i, '.'), aijList)]
    } else if(i > 9) {
        yearL <- aijList[grepl(paste0('00', i, '.'), aijList)]
    } else {
        yearL <- aijList[grepl(paste0('000', i, '.'), aijList)]
    }

    if((length(yearL) / 2) < 12) {
        if(length(yearL) == 1) {
            break
        } else {
            monEnd <- (length(yearL) / 2) - 1
        }
        monEnd <- (length(yearL) / 2) - 1
    } else {
        monEnd <- 12
    }


    for(j in 1:monEnd) {
        tFile <- yearL[grepl(MonList[j], yearL)]

        fil <- paste0(getwd(), '/', tFile)
        print(paste0(i, ", ", j)) ### to troubleshoot (231016)
        nc_data <- nc_open(fil)

        for(iVar in 1:length(VarList)) {
            t_data <- ncvar_get(nc_data, varid=VarList[iVar])
            
            colnames(t_data) <- seq(-90,90,4)
            row.names(t_data) <- seq(-177.5,177.5,5)
            t_data <- melt(t_data)
            colnames(t_data) <- c('lon', 'lat', 'value')

            for(k in 1:length(LatList)) {

                t <- subset(t_data, lat == LatList[k])
                meanV <- mean(t$value)
                sdV <- sd(t$value)

                tOut <- cbind(run, i, sprintf('%02s', j), MonList[j], LatList[k], VarList[iVar], meanV, sdV)
                outFrame <- rbind(outFrame, tOut)
            }
        }
        nc_close(nc_data)
    }
}

colnames(outFrame) <- c('Run_ID', 'Year', 'Month_Num', 'Month_Name', 'Latitude', 'Variable', 'Mean_Value', 'SD_Value')
outFrame[,c(2,5,7:8)] <- apply(outFrame[,c(2,5,7:8)], 2, as.numeric)
outFrame <- outFrame %>% mutate(Time = as.POSIXct(paste0('00', Year, '-', Month_Num, '-', 01, ' ', 00, ':', 00, ':', 00), format='%Y-%m-%d %H:%M:%OS'))

saveRDS(outFrame, file = paste0(run, '_Equilibrium_', format(Sys.time(), "%y%m%d"), '.rds'))