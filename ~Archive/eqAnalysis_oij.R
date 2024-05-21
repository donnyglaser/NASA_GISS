### R script to analyze concatenate all monthly mean data into one file for timeseries analysis ###
### run this script in a directory with only diagnostic files ###

## Libraries ##
library(ncdf4) 
library(tidyverse)
library(reshape)

startTime <- Sys.time()
goFlag <- TRUE

## Extract name of run ##
runName <- list.files(getwd(), pattern = 'aij')
runName <- runName[!grepl('aijk|aijl|taij', runName)]
runName <- runName[1]
runName <- substr(runName,9,nchar(runName)-3)
runName <- gsub('aij', '', runName)

dateList <- list.files(getwd(), pattern = 'aij')
dateList <- substr(dateList, 4, 7)
dateList <- unique(as.numeric(dateList))
dateList <- dateList[!is.na(dateList)]



## check to see if this is the first run of analysis ##
if(length(list.files(getwd(), pattern='STOPFLAG_oij.rds')) > 0) {
    goFlag <- FALSE
    print('STOP = TRUE')
} else if(length(list.files(getwd(), pattern='Temporary_oij_')) > 0) {
    ## Bring in settings from first eqAnalysis run ##
    iTab <- readRDS('Temporary_oij_IterationVariables.rds')
    outFileName <- readRDS('Temporary_oij_TableFileName.rds')
    outFrame <- readRDS(outFileName)
    diagI <- iTab[1]
    YearI <- iTab[2] + 1
    YearE <- max(dateList)
    file.remove(outFileName)
    file.remove('Temporary_oij_IterationVariables.rds')
    file.remove('Temporary_oij_TableFileName.rds')
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
allVar <- rbind(allVar, cbind('oij', c('oij_hbl', 'oij_mld')))
colnames(allVar) <- c('Diagnostic', 'Variable')
outNames <- c('Run_ID', 'Year', 'Month_Num', 'Month_Name', 'Latitude', 'Diagnostic', 'Variable', 'Layer', 'Mean_Value', 'SD_Value')

## select atmos and ocean layers ##  edit if you like
aLayers <- c(1, 2, 4, 9, 14, 26, 35, 39)
oLayers <- c(1, 2, 3, 8, 13)

if(goFlag == TRUE) {
    print('goFlag = TRUE; main loop')

    diagFiles <- list.files(getwd(), pattern = paste0('oij', runName))

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

        for(iMon in 1:monEnd) {
            tFile <- yearL[grepl(monList[iMon], yearL)]

            fil <- paste0(getwd(), '/', tFile)
            nc_data <- nc_open(fil)

            varList <- subset(allVar, Diagnostic %in% 'oij')
            for(iVar in 1:nrow(varList)) {
                t_data <- ncvar_get(nc_data, varid=varList[iVar,2])

                print(paste0('oij', ', ', iYear, ', ', iMon, ', ', iVar))

                ## surface layer only ##
                colnames(t_data) <- seq(-90,90,4)
                row.names(t_data) <- seq(-177.5,177.5,5)
                t_data <- melt(t_data)
                colnames(t_data) <- c('lon', 'lat', 'value')

                for(iLat in 1:length(latList)) {
                    t <- subset(t_data, lat == latList[iLat])
                    tOut <- cbind(runName, iYear, sprintf('%02s', iMon), monList[iMon], latList[iLat], 'oij', varList[iVar,2], 'OceanSurface', mean(t$value, na.rm=TRUE), sd(t$value, na.rm=TRUE))
                    colnames(tOut) <- outNames
                    outFrame <- rbind(outFrame, tOut)
                }
            }
            nc_close(nc_data)
        }
        if(as.numeric(difftime(Sys.time(),startTime,units='hours')) > 11.25) {
                print('Time > 11.25')

                saveRDS(c('oij', iYear), file = 'Temporary_oij_IterationVariables.rds')

                colnames(outFrame) <- outNames
                outFrame[,c(2:3,9:10)] <- apply(outFrame[,c(2:3,9:10)], 2, as.numeric)

                saveRDS(outFrame, file = paste0(runName, '_EqPARTIAL_oij_', format(Sys.time(), "%y%m%d"), '.rds'))
                saveRDS(paste0(runName, '_EqPARTIAL_oij_', format(Sys.time(), "%y%m%d"), '.rds'), file = 'Temporary_oij_TableFileName.rds')

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

if(goFlag == TRUE) {
    print('Final Save')
    colnames(tOut) <- outNames 
    outFrame[,c(2:3,9:10)] <- apply(outFrame[,c(2:3,9:10)], 2, as.numeric)
    outFrame <- outFrame %>% mutate(Time = as.POSIXct(paste0('00', Year, '-', Month_Num, '-', 01, ' ', 00, ':', 00, ':', 00), format='%Y-%m-%d %H:%M:%OS'))

    saveRDS(outFrame, file = paste0(runName, '_Equilibrium_oij_', format(Sys.time(), "%y%m%d"), '.rds'))
    saveRDS('STOP', 'STOPFLAG_oij.rds')
    goFlag <- FALSE
    ## maybe add delete partial file here??
} else {
    print('Skip Final Save')
}