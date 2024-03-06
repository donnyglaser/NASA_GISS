### R script to analyze concatenate all monthly mean data into one file for timeseries analysis ###
### run this script in a directory with only diagnostic files ###

## Libraries ##
library(ncdf4) 
library(tidyverse)
library(reshape)

startTime <- Sys.time()
goFlag <- TRUE
janOnly <- FALSE 
janOnly <- TRUE # comment this line out if you want to calculate the average of all months

diagList <- c()
############################################
### Check directory for diagnostic files ###
############################################
aij <- list.files(getwd(), pattern = 'aij')
aij <- Filter(function(x) !any(grepl('aijk', x)), aij)
aij <- Filter(function(x) !any(grepl('aijl', x)), aij)
aij <- Filter(function(x) !any(grepl('taij', x)), aij)
if(length(aij) > 0) {
    diagList <- c(diagList, 'aij')
}
if(length(list.files(getwd(), pattern = 'aijk')) > 0) {
    diagList <- c(diagList, 'aijk')
}
if(length(list.files(getwd(), pattern = 'aijl')) > 0) {
    diagList <- c(diagList, 'aijl')
}

oij <- list.files(getwd(), pattern = 'oij')
oij <- Filter(function(x) !any(grepl('oijl', x)), oij)
if(length(oij) > 0) {
    diagList <- c(diagList, 'oij')
}
if(length(list.files(getwd(), pattern = 'oijl')) > 0) {
    diagList <- c(diagList, 'oijl')
}

## Extract name of run ##
runName <- list.files(getwd(), pattern = diagList[1])
runName <- runName[!grepl('aijk|aijl|taij', runName)]
runName <- runName[1]
runName <- substr(runName,9,nchar(runName)-3)
runName <- gsub(diagList[1], '', runName)

dateList <- list.files(getwd(), pattern = diagList[1])
dateList <- substr(dateList, 4, 7)
dateList <- unique(as.numeric(dateList))
dateList <- dateList[!is.na(dateList)]



## check to see if this is the first run of analysis ##
if(length(list.files(getwd(), pattern='STOP_FLAG.rds')) > 0) {
    goFlag <- FALSE
    print('STOP = TRUE')
} else if(length(list.files(getwd(), pattern='Temporary_')) > 0) {
    ## Bring in settings from first eqAnalysis run ##
    iTab <- readRDS('Temporary_IterationVariables.rds')
    outFileName <- readRDS('Temporary_TableFileName.rds')
    outFrame <- readRDS(outFileName)
    diagI <- iTab[1]
    YearI <- iTab[2] + 1
    YearE <- max(dateList)
    file.remove(outFileName)
    file.remove('Temporary_IterationVariables.rds')
    file.remove('Temporary_TableFileName.rds')
    print('TEMPORARY = TRUE')
} else {
    outFrame <- data.frame()
    diagI <- 1
    YearI <- min(dateList)
    YearE <- max(dateList)
    MonI <- 1
    VarI <- 1
    print('TEMPORARY = FALSE')
}

monList <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

## Generic subset of specific latitudes ##
latList <- c(90, 78, 62, 46, 30, 14, 2, -14, -30, -46, -62, -78, -90)

## select variables to analyze equilibrium in aij, aijk, aijl, oil, and oijl diagnostics ## edit if you like
allVar <- data.frame()
allVar <- rbind(allVar, cbind('aij', c("bs_snowdp", "gice", "gwtr", "incsw_toa", "landicefr", "prsurf", "qatm", "snowdp", "srf_wind_dir", "srtrnf_grnd", "tgrnd", "tsurf", "wsurf", "zsnow")))
allVar <- rbind(allVar, cbind('aijk',c('tb')))
allVar <- rbind(allVar, cbind('aijl', c('q', 'rh', 'temp')))
allVar <- rbind(allVar, cbind('oij', c('oij_hbl', 'oij_mld')))
allVar <- rbind(allVar, cbind('oijl', c('heat', 'pot_temp', 'salt')))
colnames(allVar) <- c('Diagnostic', 'Variable')
outNames <- c('Run_ID', 'Year', 'Month_Num', 'Month_Name', 'Latitude', 'Diagnostic', 'Variable', 'Layer', 'Mean_Value', 'SD_Value')

## select atmos and ocean layers ##  edit if you like
aLayers <- c(1, 2, 4, 9, 14, 26, 35, 39)
oLayers <- c(1, 2, 3, 8, 13)

if(goFlag == TRUE) {
    print('goFlag = TRUE; main loop')
    for(iDiag in diagI:length(diagList)) {
        diagFiles <- list.files(getwd(), pattern = paste0(diagList[iDiag], runName))

        for(iYear in YearI:YearE) { # handles up to 9999 years
            if(iYear > 999) {
                yearL <- diagFiles[grepl(paste0(iYear, '.'), diagFiles)]
            } else if(iYear > 99){
                yearL <- diagFiles[grepl(paste0('0', iYear, '.'), diagFiles)]
            } else if(iYear > 9) {
                yearL <- diagFiles[grepl(paste0('00', iYear, '.'), diagFiles)]
            } else {
                yearL <- diagFiles[grepl(paste0('000', iYear, '.'), diagFiles)]
            }

            if((length(yearL)) < 12) { # handles partial years (if < 12 months)
                if(length(yearL) == 1) {
                    break
                } else {
                    monEnd <- (length(yearL)) - 1
                }
                monEnd <- (length(yearL)) - 1
            } else {
                monEnd <- 12
            }

            if(janOnly) {
                monEnd <- 1
            }

            for(iMon in 1:monEnd) {
                tFile <- yearL[grepl(monList[iMon], yearL)]

                fil <- paste0(getwd(), '/', tFile)
                nc_data <- nc_open(fil)

                varList <- subset(allVar, Diagnostic %in% diagList[iDiag])
                for(iVar in 1:nrow(varList)) {
                    t_data <- ncvar_get(nc_data, varid=varList[iVar,2])

                    print(paste0(iDiag, ', ', iYear, ', ', iMon, ', ', iVar))

                    if(diagList[iDiag] == 'aij') {
                        ## surface layer only ##
                        colnames(t_data) <- seq(-90,90,4)
                        row.names(t_data) <- seq(-177.5,177.5,5)
                        t_data <- melt(t_data)
                        colnames(t_data) <- c('lon', 'lat', 'value')

                        for(iLat in 1:length(latList)) {
                            t <- subset(t_data, lat == latList[iLat])
                            tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], latList[iLat], diagList[iDiag], varList[iVar,2], 'AtmosSurface', mean(t$value, na.rm=TRUE), sd(t$value, na.rm=TRUE))
                            colnames(tOut) <- outNames
                            outFrame <- rbind(outFrame, tOut)
                        }

                        ## Global and Hemisphere Averages ##
                        hemiData <- ncvar_get(nc_data, varid=paste0(varList[iVar,2], '_hemis'))
                        tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], 'SouthernHemisphere', diagList[iDiag], varList[iVar,2], 'AtmosSurface', hemiData[1], 0)
                        colnames(tOut) <- outNames
                        outFrame <- rbind(outFrame, tOut)
                        tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], 'NorthernHemisphere', diagList[iDiag], varList[iVar,2], 'AtmosSurface', hemiData[2], 0)
                        colnames(tOut) <- outNames
                        outFrame <- rbind(outFrame, tOut)
                        tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], 'Global', diagList[iDiag], varList[iVar,2], 'AtmosSurface', hemiData[3], 0)
                        colnames(tOut) <- outNames
                        outFrame <- rbind(outFrame, tOut)

                    } else if(diagList[iDiag] == 'oij') {
                        ## surface layer only ##
                        colnames(t_data) <- seq(-90,90,4)
                        row.names(t_data) <- seq(-177.5,177.5,5)
                        t_data <- melt(t_data)
                        colnames(t_data) <- c('lon', 'lat', 'value')

                        for(iLat in 1:length(latList)) {
                            t <- subset(t_data, lat == latList[iLat])
                            tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], latList[iLat], diagList[iDiag], varList[iVar,2], 'OceanSurface', mean(t$value, na.rm=TRUE), sd(t$value, na.rm=TRUE))
                            colnames(tOut) <- outNames
                            outFrame <- rbind(outFrame, tOut)
                        }

                    } else if(diagList[iDiag] == 'oijl') {
                        ## 13 ocean layers ##
                        for(iLayer in 1:length(oLayers)) {
                            print(iLayer)
                            tDat <- t_data[,,oLayers[iLayer]]
                            colnames(tDat) <- seq(-90,90,4)
                            row.names(tDat) <- seq(-177.5,177.5,5)
                            tDat <- melt(tDat)
                            colnames(tDat) <- c('lon', 'lat', 'value')

                            for(iLat in 1:length(latList)) {
                                t <- subset(tDat, lat == latList[iLat])
                                tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], latList[iLat], diagList[iDiag], varList[iVar,2], paste0('OceanLayer_', oLayers[iLayer]), mean(t$value, na.rm=TRUE), sd(t$value, na.rm=TRUE))
                                colnames(tOut) <- outNames
                                outFrame <- rbind(outFrame, tOut)
                            }
                        }

                    } else { ## aijl and aijk
                        ## 40 atmos layers ##
                        for(iLayer in 1:length(aLayers)) {
                            print(iLayer)
                            tDat <- t_data[,,aLayers[iLayer]]
                            colnames(tDat) <- seq(-90,90,4)
                            row.names(tDat) <- seq(-177.5,177.5,5)
                            tDat <- melt(tDat)
                            colnames(tDat) <- c('lon', 'lat', 'value')

                            for(iLat in 1:length(latList)) {
                                t <- subset(tDat, lat == latList[iLat])
                                tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], latList[iLat], diagList[iDiag], varList[iVar,2], paste0('OceanLayer_', aLayers[iLayer]), mean(t$value, na.rm=TRUE), sd(t$value, na.rm=TRUE))
                                colnames(tOut) <- outNames
                                outFrame <- rbind(outFrame, tOut)
                            }
                        }
                    }
                }
                nc_close(nc_data)
            }
            if(as.numeric(difftime(Sys.time(),startTime,units='hours')) > 11.25) {
                    print('Time > 11.5')

                    saveRDS(c(iDiag, iYear), file = 'Temporary_IterationVariables.rds')

                    colnames(outFrame) <- outNames
                    outFrame[,c(2:3,9:10)] <- apply(outFrame[,c(2:3,9:10)], 2, as.numeric)

                    saveRDS(outFrame, file = paste0(runName, '_EqPARTIAL_', format(Sys.time(), "%y%m%d"), '.rds'))
                    saveRDS(paste0(runName, '_EqPARTIAL_', format(Sys.time(), "%y%m%d"), '.rds'), file = 'Temporary_TableFileName.rds')

                    print('saved partial')

                    goFlag <- FALSE
                    print('go = FALSE')
                    break
            }
        }
        YearI <- 1
        if(goFlag == FALSE) {
            print('break')
            break
        }
    }
}

if(goFlag == TRUE) {
    print('Final Save')
    colnames(tOut) <- outNames 
    outFrame[,c(2:3,9:10)] <- apply(outFrame[,c(2:3,9:10)], 2, as.numeric)
    outFrame <- outFrame %>% mutate(Time = as.POSIXct(paste0('00', Year, '-', Month_Num, '-', 01, ' ', 00, ':', 00, ':', 00), format='%Y-%m-%d %H:%M:%OS'))

    saveRDS(outFrame, file = paste0(runName, '_Equilibrium_', format(Sys.time(), "%y%m%d"), '.rds'))
    saveRDS('STOP', 'STOP_FLAG.rds')
    goFlag <- FALSE
    ## maybe add delete partial file here??
} else {
    print('Skip Final Save')
}