### script to take sum files (annual averages over 5 - 10 years), and extract data for prelim analysis ###

## shell script: take list of runs -> sumfiles -> scaleacc (aij, oijl) -> move to single dir
## then...
## R script: take aij, oij files -> extract diags (e.g., tsurf, gwtr, qatm, snowicefr, plan_alb, prec, pcldt, etc.) ->
## write geo & hemis data to sheet -> make plots
## Q: precipitation over land? land T?, ground water?

library(ncdf4)
library(reshape2)
library(tidyverse)


aijList <- list.files(getwd(), pattern = c('ANN', 'aij'))
oijlList <- list.files(getwd(), pattern = c('ANN', 'oij'))

aijDiag <- c('tsurf', 'gwtr', 'grnd_alb', 'qatm', 'snowicefr', 'plan_alb', 'prec', 'pcldt')
oijlDiag <- 'pot_temp'

dzoc1 <- 12 # depth of first ocean layer

sumTab <- data.frame()
plotTab <- data.frame()
for(iaij in 1:length(aijList)) {
    nc_data <- nc_open(aijList[iaij])

    axypTab <- ncvar_get(nc_data, varid='axyp')
    colnames(axypTab) <- seq(-90,90,4)
    row.names(axypTab) <- seq(-177.5,177.5,5)
    axypTab <- melt(axypTab)
    colnames(axypTab) <- c('lon', 'lat', 'value')
    axypPole <- subset(axypTab, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
    axypTab <- subset(axypTab, lat > -90 & lat < 90)
    axypTab <- rbind(axypTab, axypPole)
    globalArea <- sum(axypTab$value)

    ocnTab <- ncvar_get(nc_data, varid='ocnfr')
    colnames(ocnTab) <- seq(-90,90,4)
    row.names(ocnTab) <- seq(-177.5,177.5,5)
    ocnTab <- melt(ocnTab)
    colnames(ocnTab) <- c('lon', 'lat', 'value')
    ocnPole <- subset(ocnTab, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
    ocnTab <- subset(ocnTab, lat > -90 & lat < 90)
    ocnTab <- rbind(ocnTab, ocnPole)
    ocnTab <- subset(ocnTab, value == 0)

    landTab <- merge(ocnTab, axypTab, by = c('lon', 'lat'), all.x = TRUE)
    landTab <- landTab[,-3]
    colnames(landTab)[3] <- 'axyp'
    landArea <- sum(landTab$axyp)
    landTab <- landTab %>% mutate(axyp_pct = axyp / landArea)

    runName <- str_split(aijList[iaij], 'aij')[[1]][[2]]
    runName <- str_split(runName, '.nc')[[1]][1]

    if(grepl('aqua', runName)) {
        runAng <- 'NA'
        runWC <- '100'
    } else {
        
        runAng <- str_split(runName, '_')[[1]][2]
        runWC <- str_split(runAng, 'x')[[1]][2]
        runAng <- str_split(runAng, 'x')[[1]][1]
    }

    plotOut <- data.frame()
    for(ivar in 1:length(aijDiag)) {
        allData <- ncvar_get(nc_data, varid=aijDiag[ivar])
        hemiData <- ncvar_get(nc_data, varid=paste0(aijDiag[ivar], '_hemis'))

        colnames(allData) <- seq(-90,90,4)
        row.names(allData) <- seq(-177.5,177.5,5)
        allData <- melt(allData)
        colnames(allData) <- c('lon', 'lat', 'value')
        plotData <- cbind(runName, runAng, runWC, aijDiag[ivar], allData)
        plotTab <- rbind(plotTab, plotData)
        colnames(plotData)[1:4] <- c('Run', 'Angle', 'WaterContent', 'Diagnostic')
        allPole <- subset(allData, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
        allData <- subset(allData, lat > -90 & lat < 90)
        allData <- rbind(allData, allPole)

        landData <- merge(allData, landTab, by = c('lon', 'lat'), all.y = FALSE)
        landData <- landData %>% mutate(meanValue = value * axyp_pct)
        lowData <- subset(landData, lat >= -30 & lat <= 30)
        lowArea <- sum(lowData$axyp)
        lowData <- lowData %>% mutate(axyp_pct = (axyp / lowArea))
        lowData <- lowData %>% mutate(meanValue = value * axyp_pct)
        midData <- subset(landData, lat < -30 | lat > 30)
        midData <- subset(midData, lat >= -62 & lat <= 62)
        midArea <- sum(midData$axyp)
        midData <- midData %>% mutate(axyp_pct = axyp / midArea)
        midData <- midData %>% mutate(meanValue = value * axyp_pct)
        hiData <- subset(landData, lat < -62 | lat > 62)
        hiArea <- sum(hiData$axyp)
        hiData <- hiData %>% mutate(axyp_pct = axyp / hiArea)
        hiData <- hiData %>% mutate(meanValue = value * axyp_pct)

        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'Global', hemiData[3], globalArea)
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'SouthHemi', hemiData[1], (globalArea / 2))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'NorthHemi', hemiData[2], (globalArea / 2))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'AllLand', sum(landData$meanValue), landArea)
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'LowLand', sum(lowData$meanValue), sum(lowData$axyp))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'MidLand', sum(midData$meanValue), sum(midData$axyp))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, aijDiag[ivar], 'HiLand', sum(hiData$meanValue), sum(hiData$axyp))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)

        ## extract and save plotting data here?? ##

    }
}
saveRDS(sumTab, file = paste0('SummaryData_aij_', format(Sys.time(), "%y%m%d"), '.rds'))
saveRDS(plotTab, file = paste0('PlotData_aij_', format(Sys.time(), "%y%m%d"), '.rds'))