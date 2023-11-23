### R script to analyze concatenate all monthly mean data into one file for timeseries analysis ###
### run this script in a directory with only diagnostic files ###

## Libraries ##
library(ncdf4) 
library(tidyverse)
library(reshape)

diagList <- c()
####################################
### Comment unwanted diagnostics ###
####################################
# I think these are the best diagnostics for equilibrium analysis #
## ATMOS ##
diagList <- c(diagList, 'aij')
diagList <- c(diagList, 'aijk')
diagList <- c(diagList, 'aijl')
## OCEAN ##
diagList <- c(diagList, 'oijl')

## Extract name of run ##
runName <- list.files(getwd(), pattern = diagList[1])[1]
runName <- substr(runName,9,nchar(runName)-3)
runName <- gsub(diagList[1], '', runName)

dateList <- list.files(getwd(), pattern = diagList[1])
dateList <- substr(dateList, 4, 7)
dateList <- unique(as.numeric(dateList))
dateList <- dateList[!is.na(dateList)]

YearI <- min(dateList)
YearE <- max(dateList)
monList <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

## Generic subset of specific latitudes ##
latList <- c(90, 78, 62, 46, 30, 14, 2, -14, -30, -46, -62, -78, -90)

## This array needs to be customized for specific diagnostics, these are a select set for aij files ##
allVar <- data.frame()
allVar <- rbind(allVar, cbind('aij', c("bs_snowdp", "gice", "gwtr", "incsw_toa", "landicefr", "prsurf", "qatm", "snowdp", "srtrnf_grnd", "tgrnd", "tsurf", "wsurf", "zsnow")))
allVar <- rbind(allVar, cbind('aijl', c('q', 'rh', 'temp')))
allVar <- rbind(allVar, cbind('aijk',c('tb')))
allVar <- rbind(allVar, cbind('oijl', c('heat', 'pot_temp', 'salt')))
colnames(allVar) <- c('Diagnostic', 'Variable')

outFrame <- data.frame()
for(iDiag in 1:length(diagList)) {
    diagFiles <- list.files(getwd(), pattern = diagList[iDiag])

    for(iYear in YearI:YearE) {
        if(iYear > 99){
            yearL <- diagFiles[grepl(paste0('0', iYear, '.'), diagFiles)]
        } else if(iYear > 9) {
            yearL <- diagFiles[grepl(paste0('00', iYear, '.'), diagFiles)]
        } else {
            yearL <- diagFiles[grepl(paste0('000', iYear, '.'), diagFiles)]
        }

        if((length(yearL)) < 12) {
            if(length(yearL) == 1) {
                break
            } else {
                monEnd <- (length(yearL)) - 1
            }
            monEnd <- (length(yearL)) - 1
        } else {
            monEnd <- 12
        }

        for(iMon in 1:monEnd) {
            tFile <- yearL[grepl(monList[iMon], yearL)]

            fil <- paste0(getwd(), '/', tFile)
            nc_data <- nc_open(fil)

            varList <- subset(allVar, Diagnostic %in% diagList[iDiag])
            for(iVar in 1:length(varList)) {
                t_data <- ncvar_get(nc_data, varid=varList[iVar])

                ## need to add a bit here to add z levels ##
                ## if else ##
                
                colnames(t_data) <- seq(-90,90,4)
                row.names(t_data) <- seq(-177.5,177.5,5)
                t_data <- melt(t_data)
                colnames(t_data) <- c('lon', 'lat', 'value')

                for(iLat in 1:length(latList)) {
                    t <- subset(t_data, lat == latList[iLat])
                    tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], latList[iLat], varList[iVar], mean(t$value), sd(t$value))
                    outFrame <- rbind(outFrame, tOut)
                }

                ## Global and Hemisphere Averages ##
                hemiData <- ncvar_get(nc_data, varid=paste0(varList[iVar], '_hemis'))
                tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], 'SouthernHemisphere', varList[iVar], hemiData[1], 0)
                outFrame <- rbind(outFrame, tOut)
                tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], 'NorthernHemisphere', varList[iVar], hemiData[2], 0)
                outFrame <- rbind(outFrame, tOut)
                tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], 'Global', varList[iVar], hemiData[3], 0)
                outFrame <- rbind(outFrame, tOut)
            }
            nc_close(nc_data)
        }
    }
}

colnames(outFrame) <- c('Run_ID', 'Year', 'Month_Num', 'Month_Name', 'Latitude', 'Variable', 'Mean_Value', 'SD_Value') ## add level column (surface/level#/pressure)
outFrame[,c(2,5,7:8)] <- apply(outFrame[,c(2,5,7:8)], 2, as.numeric)
outFrame <- outFrame %>% mutate(Time = as.POSIXct(paste0('00', Year, '-', Month_Num, '-', 01, ' ', 00, ':', 00, ':', 00), format='%Y-%m-%d %H:%M:%OS'))

saveRDS(outFrame, file = paste0(runName, '_Equilibrium_', format(Sys.time(), "%y%m%d"), '.rds'))

