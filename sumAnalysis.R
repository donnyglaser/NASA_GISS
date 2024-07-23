### script to take sum files (annual averages over 5 - 10 years), and extract data for prelim analysis ###

## shell script: take list of runs -> sumfiles -> scaleacc (aij, oijl) -> move to single dir
## then...
## R script: take aij, oij files -> extract diags (e.g., tsurf, gwtr, qatm, snowicefr, plan_alb, prec, pcldt, etc.) ->
## write geo & hemis data to sheet -> make plots
## Q: precipitation over land? land T?, ground water?

## DEPRECIATED (240723) ##

library(ncdf4)
library(reshape2)
library(tidyverse)


aijList <- list.files(getwd(), pattern = '.aij')
aijList <- aijList[grep('ANN', aijList)]
oijlList <- list.files(getwd(), pattern = '.oijl')
oijlList <- oijlList[grep('ANN', oijlList)]

aijDiag <- c('tsurf', 'gwtr', 'grnd_alb', 'qatm', 'snowicefr', 'plan_alb', 'prec', 'pcldt')
oijlDiag <- c('pot_temp')

#dzoc1 <- 12 # depth of first ocean layer

sumTab <- data.frame()
plotTab <- data.frame()
for(iaij in 1:length(aijList)) {
    nc_data <- nc_open(aijList[iaij])

    runName <- str_split(aijList[iaij], 'aij')[[1]][[2]]
    runName <- str_split(runName, '.nc')[[1]][1]

    axypTab <- ncvar_get(nc_data, varid='axyp')
    colnames(axypTab) <- seq(-90,90,4)
    row.names(axypTab) <- seq(-177.5,177.5,5)
    axypTab <- melt(axypTab)
    colnames(axypTab) <- c('lon', 'lat', 'value')
    axypPole <- subset(axypTab, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
    axypTab <- subset(axypTab, lat > -90 & lat < 90)
    axypTab <- rbind(axypTab, axypPole)
    globalArea <- sum(axypTab$value)
    #saveRDS(axypTab, file = paste0('GridArea_', runName, '_', format(Sys.time(), "%y%m%d"), '.rds'))

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

    

    if(grepl('aqua', runName)) {
        runAng <- 'NA'
        runWC <- '100'
    } else {
        
        runAng <- str_split(runName, '_')[[1]][2]
        runWC <- str_split(runAng, 'x')[[1]][2]
        runAng <- str_split(runAng, 'x')[[1]][1]
    }

    for(ivar in 1:length(aijDiag)) {
        allData <- ncvar_get(nc_data, varid=aijDiag[ivar])
        hemiData <- ncvar_get(nc_data, varid=paste0(aijDiag[ivar], '_hemis'))

        colnames(allData) <- seq(-90,90,4)
        row.names(allData) <- seq(-177.5,177.5,5)
        allData <- melt(allData)
        colnames(allData) <- c('lon', 'lat', 'value')
        plotData <- cbind(runName, runAng, runWC, aijDiag[ivar], allData)
        colnames(plotData)[1:4] <- c('Run', 'Angle', 'WaterContent', 'Diagnostic')
        plotTab <- rbind(plotTab, plotData)
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
    }
    ### CALCULATE ARIDITY INDEX ### (prec / pot_evap)

    precHemi <- ncvar_get(nc_data, varid='prec_hemis')
    evapHemi <- ncvar_get(nc_data, varid='pot_evap_hemis')
    hemiData <- precHemi / evapHemi
    precData <- ncvar_get(nc_data, varid='prec')
    evapData <- ncvar_get(nc_data, varid='pot_evap')
    aridData <- precData / evapData

    colnames(aridData) <- seq(-90,90,4)
    row.names(aridData) <- seq(-177.5,177.5,5)
    aridData <- melt(aridData)
    colnames(aridData) <- c('lon', 'lat', 'value')
    plotData <- cbind(runName, runAng, runWC, 'AridityIndex', aridData)
    colnames(plotData)[1:4] <- c('Run', 'Angle', 'WaterContent', 'Diagnostic')
    plotTab <- rbind(plotTab, plotData)
    allPole <- subset(aridData, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
    aridData <- subset(aridData, lat > -90 & lat < 90)
    aridData <- rbind(aridData, allPole)

    landData <- merge(aridData, landTab, by = c('lon', 'lat'), all.y = FALSE)
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
    saData <- subset(landData, value <= 0.5)
    saArea <- sum(saData$axyp)
    saData <- saData %>% mutate(axyp_pct = axyp / saArea)
    saData <- saData %>% mutate(meanValue = value * axyp_pct)
    ahData <- subset(landData, value <= 0.2)
    ahArea <- sum(ahData$axyp)
    ahData <- ahData %>% mutate(axyp_pct = axyp / ahArea)
    ahData <- ahData %>% mutate(meanValue = value * axyp_pct)
    hyperData <- subset(landData, value <= 0.05)
    hyperArea <- sum(hyperData$axyp)
    hyperData <- hyperData %>% mutate(axyp_pct = axyp / hyperArea)
    hyperData <- hyperData %>% mutate(meanValue = value * axyp_pct)
    humData <- subset(landData, value > 0.65)
    humArea <- sum(humData$axyp)
    humData <- humData %>% mutate(axyp_pct = axyp / humArea)
    humData <- humData %>% mutate(meanValue = value * axyp_pct)

    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'AllLand', sum(landData$meanValue), landArea)
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'LowLand', sum(lowData$meanValue), sum(lowData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'MidLand', sum(midData$meanValue), sum(midData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'HiLand', sum(hiData$meanValue), sum(hiData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'PctSemiaridAridHyper', sum(saData$meanValue), sum(saData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'PctAridHyper', sum(ahData$meanValue), sum(ahData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'PctHyperarid', sum(hyperData$meanValue), sum(hyperData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    tempOut <- cbind(runName, runAng, runWC, 'AridityIndex', 'PctHumid', sum(humData$meanValue), sum(humData$axyp))
    colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
    sumTab <- rbind(sumTab, tempOut)
    
    nc_close(nc_data)
}


sumTab[,c(3,6:7)] <- apply(sumTab[,c(3,6:7)], 2, as.numeric)
saveRDS(sumTab, file = paste0('SummaryData_TopoEns_aij_', format(Sys.time(), "%y%m%d"), '.rds'))
plotTab[,c(3,5:7)] <- apply(plotTab[,c(3,5:7)], 2, as.numeric)
saveRDS(plotTab, file = paste0('PlotData_TopoEns_aij_', format(Sys.time(), "%y%m%d"), '.rds'))


########### THEN DO OIJL DATA ###########


sumTab <- data.frame()
plotTab <- data.frame()
for(ioij in 1:length(oijlList)) {
    nc_data <- nc_open(oijlList[ioij])
    
    runName <- str_split(oijlList[ioij], 'oijl')[[1]][[2]]
    runName <- str_split(runName, '.nc')[[1]][1]

    oxypTab <- ncvar_get(nc_data, varid='oxyp')
    colnames(oxypTab) <- seq(-90,90,4)
    row.names(oxypTab) <- seq(-177.5,177.5,5)
    oxypTab <- melt(oxypTab)
    colnames(oxypTab) <- c('lon', 'lat', 'value')
    oxypPole <- subset(oxypTab, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
    oxypTab <- subset(oxypTab, lat > -90 & lat < 90)
    oxypTab <- rbind(oxypTab, oxypPole)
    ocArea <- sum(oxypTab$value)

    if(grepl('aqua', runName)) {
        runAng <- 'NA'
        runWC <- '100'
    } else {
        
        runAng <- str_split(runName, '_')[[1]][2]
        runWC <- str_split(runAng, 'x')[[1]][2]
        runAng <- str_split(runAng, 'x')[[1]][1]
    }

    for(ivar in 1:length(oijlDiag)) {
        allData <- ncvar_get(nc_data, varid=oijlDiag[ivar])
        allData <- allData[,,1] ## only does surface layer (12 m)
        colnames(allData) <- seq(-90,90,4)
        row.names(allData) <- seq(-177.5,177.5,5)
        allData <- melt(allData)
        colnames(allData) <- c('lon', 'lat', 'value')
        plotData <- cbind(runName, runAng, runWC, oijlDiag[ivar], allData)
        colnames(plotData)[1:4] <- c('Run', 'Angle', 'WaterContent', 'Diagnostic')
        plotTab <- rbind(plotTab, plotData)
        allPole <- subset(allData, (lat == -90 & lon == -177.5) | (lat == 90 & lon == -177.5))
        allData <- subset(allData, lat > -90 & lat < 90)
        allData <- rbind(allData, allPole)
        allData <- na.omit(allData)
        allData <- merge(allData, oxypTab, by = c('lon', 'lat'), all.x = TRUE)
        colnames(allData)[3:4] <- c('value', 'oxyp')
        ocArea <- sum(allData$oxyp)
        allData <- allData %>% mutate(oxyp_pct = oxyp / ocArea)
        allData <- allData %>% mutate(meanValue = value * oxyp_pct)
        lowData <- subset(allData, lat >= -30 & lat <= 30)
        lowArea <- sum(lowData$oxyp)
        lowData <- lowData %>% mutate(oxyp_pct = (oxyp / lowArea))
        lowData <- lowData %>% mutate(meanValue = value * oxyp_pct)
        midData <- subset(allData, lat < -30 | lat > 30)
        midData <- subset(midData, lat >= -62 & lat <= 62)
        midArea <- sum(midData$oxyp)
        midData <- midData %>% mutate(oxyp_pct = oxyp / midArea)
        midData <- midData %>% mutate(meanValue = value * oxyp_pct)
        hiData <- subset(allData, lat < -62 | lat > 62)
        hiArea <- sum(hiData$oxyp)
        hiData <- hiData %>% mutate(oxyp_pct = oxyp / hiArea)
        hiData <- hiData %>% mutate(meanValue = value * oxyp_pct)

        tempOut <- cbind(runName, runAng, runWC, oijlDiag[ivar], 'AllOcean', sum(allData$meanValue), ocArea)
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, oijlDiag[ivar], 'LowOcean', sum(lowData$meanValue), sum(lowData$oxyp))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, oijlDiag[ivar], 'MidOcean', sum(midData$meanValue), sum(midData$oxyp))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)
        tempOut <- cbind(runName, runAng, runWC, oijlDiag[ivar], 'HiOcean', sum(hiData$meanValue), sum(hiData$oxyp))
        colnames(tempOut) <- c('Run', 'Angle', 'WaterContent', 'Diagnostic', 'Region', 'Value', 'Area')
        sumTab <- rbind(sumTab, tempOut)

    }
    nc_close(nc_data)
}

sumTab[,c(3,6:7)] <- apply(sumTab[,c(3,6:7)], 2, as.numeric)
saveRDS(sumTab, file = paste0('SummaryData_TopoEns_oijl_', format(Sys.time(), "%y%m%d"), '.rds'))
plotTab[,c(3,5:7)] <- apply(plotTab[,c(3,5:7)], 2, as.numeric)
saveRDS(plotTab, file = paste0('PlotData_TopoEns_oijl_', format(Sys.time(), "%y%m%d"), '.rds'))