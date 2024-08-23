## map plots for all topo ens eq global means ##


library(mapproj) # global map projection library
library(tidyverse)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(reshape)
library(viridis)
library(ncdf4)
library(ggnewscale)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Topo Ensemble/Data/SumData')

#aijFiles <- list.files(getwd(), pattern = '.aij')
aij <- list.files(getwd(), pattern = '.aij')
aijk <- subset(aij, grepl('aijk', aij) == TRUE)
aijl <- subset(aij, grepl('aijl', aij) == TRUE)
aij <- subset(aij, grepl('aijk', aij) == FALSE)
aijFiles <- subset(aij, grepl('aijl', aij) == FALSE)

diagList <- c('tsurf', 'lakefr', 'grnd_alb', 'pcldt', 'prec', 'pot_evap', 'pev_pen', 'snowdp', 'snowicefr', 'ZSI')
diagNames <- c('Surface Temperature (ºC)', 'Lake Fraction (%)', 'Ground Albedo (%)', 'Total Cloud Cover (%)', 'Precipitation (mm/day)', 'Potential Evaporation (mm/day)', 'PENMAN POTENTIAL EVAPORATION (mm/day)', 'Snow Depth (mm H2O)', 'Snow Ice Fraction (%)', 'Ocean & Lake Ice Thickness (m)')
latList <- seq(-90,90,4)
lonList <- seq(-177.5,177.5,5)
panoplyPAL <- c('#050fd9', '#2050ff', '#4297ff', '#6dc1ff', '#86daff', '#9defff', '#aff6ff', '#cfffff', '#ffffff', '#ffff66', '#ffec00', '#ffc400', '#ff9000', '#ff4900', '#ff0000', '#d50000', '#9f0000')

dirName <- paste0('Plots_', format(Sys.time(), "%y%m%d"))
dir.create(dirName)

## LIMITS ##
limits <- data.frame(matrix(NA, nrow = (length(aijFiles)*length(diagList)), ncol = 3))
for(ifile in 1:length(aijFiles)) {
    nc <- nc_open(aijFiles[ifile])
    for(idiag in 1:length(diagList)) {
        diag <- ncvar_get(nc, varid = diagList[idiag])
        limits[(((ifile-1)*length(diagList))+idiag), 1] <- aijFiles[ifile]
        limits[(((ifile-1)*length(diagList))+idiag), 2] <- diagList[idiag]
        limits[(((ifile-1)*length(diagList))+idiag), 3] <- min(diag, na.rm = TRUE)
        limits[(((ifile-1)*length(diagList))+idiag), 4] <- quantile(diag, 0.8, na.rm = TRUE)
        
    }
}
colnames(limits) <- c('run', 'diag', 'min', 'max')

## round limits ##
limits <- subset(limits, diag == 'tsurf' | diag == 'prec' | diag == 'pot_evap' | diag == 'ZSI' | diag == 'snowdp')

limits <- limits %>% mutate(roundMin = round_any(min, 10^(round_any(log10(min),1,floor)), floor))
limits <- limits %>% mutate(roundMax = round_any(max, 10^(round_any(log10(max),1,floor)), ceiling))
for(ilen in 1:nrow(limits)) {
    if(is.na(limits[ilen,5])) {
        if(limits[ilen,2] == 'tsurf') {
            tMin <- round_any(limits[ilen,3], 10^(round_any(log10(abs(limits[ilen,3])),1,floor)), floor)
            tMax <- limits[ilen,6]
            if(abs(tMin) > tMax) {
                limits[ilen,5] <- tMin
                limits[ilen,6] <- abs(tMin)
            }else {
                limits[ilen,5] <- -1*tMax
                limits[ilen,6] <- tMax
            }
        } else {
            limits[ilen,5] <- 0
        }
    }
    if(limits[ilen,2] %in% c('lakefr', 'grnd_alb', 'pcldt', 'prec', 'pot_evap', 'snowdp', 'snowicefr', 'ZSI')) {
        limits[ilen,5] <- 0
    }
    if((limits[ilen,2] == 'tsurf') & ((-1 * limits[ilen,5]) != limits[ilen,6])) {
        limits[ilen,5] <- -1*limits[ilen,6]
    }
}
limits2 <- data.frame(matrix(NA, nrow = length(unique(limits$diag)), ncol = 3))
for(idiag in 1:length(unique(limits$diag))) {
    temp <- subset(limits, diag == unique(limits$diag)[idiag])

    if(unique(limits$diag)[idiag] == 'tsurf') {
        limits2[idiag, 1] <- unique(limits$diag)[idiag]
        limits2[idiag, 2] <- max(temp$roundMax, (temp$roundMin * -1)) * -1
        limits2[idiag, 3] <- max(temp$roundMax, (temp$roundMin * -1))
    } else {
        limits2[idiag, 1] <- unique(limits$diag)[idiag]
        limits2[idiag, 2] <- 0
        limits2[idiag, 3] <- max(temp$roundMax, na.rm = TRUE)
    }
}
colnames(limits2) <- c('diag', 'min', 'max')

pctList <- c('lakefr', 'grnd_alb', 'pcldt', 'snowicefr', 'ZSI')

## Color palattes ##
## tsurf: panoply; lakefr: blue/white; grndalb: b/w; pcldt: b/w; prec: blue/white; evap: red/white; snowdp: b/w; snowicefr: b/w; ZSI: b/w ##
panList <- c('tsurf')
UWList <- c('lakefr', 'prec')
BWList <- c('grnd_alb', 'pcldt', 'snowdp', 'snowicefr', 'ZSI')
RWList <- c('pot_evap', 'pev_pen')

## PLOTS and MAP DATA ##
allOut <- matrix(nrow = 0, ncol = 6)
saveRDS(allOut, paste0('MapData_TopoEns_All_', format(Sys.time(), "%y%m%d"), ".rds"))

for(ifile in 1:length(aijFiles)) {
    nc <- nc_open(aijFiles[ifile])
    tFile <- strsplit(aijFiles[ifile], split = '[.]')
    tFile <- str_replace(tFile[[1]][2], 'aij', '')

    ## Initial water body outlines ##
    ocn <- ncvar_get(nc, varid = 'topog')
    colnames(ocn) <- latList
    row.names(ocn) <- lonList
    ocnm <- melt(ocn)
    colnames(ocnm) <- c('lon', 'lat', 'landfr')
    ocnm$landfr <- ifelse(ocnm$landfr>0,1,0)


    for(idiag in 1:length(diagList)) {
        diag <- ncvar_get(nc, varid = diagList[idiag])
        row.names(diag) <- lonList
        colnames(diag) <- latList
        diagm <- melt(diag)
        colnames(diagm) <- c('lon', 'lat', 'value')

        if(diagList[idiag] == 'ZSI') {
            diagm$value <- ifelse(is.na(diagm$value), 0, diagm$value)
        }

        ## do limits and colors here ##
        tmin <- 0
        if(diagList[idiag] == 'tsurf') {
            tmin <- subset(limits2, diag == 'tsurf')[,2]
            tmax <- subset(limits2, diag == 'tsurf')[,3]
        } else if(diagList[idiag] == 'prec' | diagList[idiag] == 'pot_evap' | diagList[idiag] == 'ZSI' | diagList[idiag] == 'snowdp'){
            tmax <- subset(limits2, diag == diagList[idiag])[,3]
        } else {
            tmax <- 100
        }
        ###############################

        p <- ggplot(diagm, aes(x = lon, y = lat))
        p <- p + geom_tile(aes(fill = value), width = 6, height = 5)
        p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25)
        p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', size = 0.75)
        p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25)
        p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
        p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30))
        p <- p + xlab("Longitude")
        p <- p + ylab("Latitude")
        #p <- p + labs(title = diagNames[idiag])

        if(diagList[idiag] %in% BWList) {
            if(diagList[idiag] == 'snowdp') {
                p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(0, tmax), guide = 'colorbar', breaks = seq(0,tmax, tmax/5), oob = scales::squish)
            } else if(diagList[idiag] == 'ZSI') {
                p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(0, tmax), guide = 'colorbar', breaks = seq(0,tmax, tmax/5), oob = scales::squish)
            } else {
                p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(0,100, 25), oob = scales::squish)
            }
        } else if(diagList[idiag] %in% UWList) {                                                        
            p <- p + scale_fill_gradient(low = 'white', high = 'blue', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/5), oob = scales::squish)
        } else if(diagList[idiag] %in% RWList) {
            p <- p + scale_fill_gradient(low = 'white', high = 'red', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/7), oob = scales::squish)
        } else {
            # p <- p + scale_fill_gradientn(colors = panoplyPAL, values = seq(tmin, tmax, ((tmax*2)/16)), limits = c(tmin, tmax), guide = 'colorbar')
            p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/7), oob = scales::squish)
        }

        #scale_fill_manual(limits = names(palTemp), values = palTemp) +
        #guides(fill = guide_colorsteps(show.limits = TRUE, title = NULL)) +
        p <- p + geom_contour(data = ocnm, aes(x = lon, y = lat, z = landfr), breaks = 1, color = 'black')

        p <- p + ggtitle(NULL)
        p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = diagNames[idiag]))
        p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
        p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
            face = "bold"),
            text = element_text(size = 18),
            axis.text.x = element_text(size = 14),
            aspect.ratio = 0.625,
            axis.line = element_line(color = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", fill=NA, size=2),
            legend.key=element_blank(),
            legend.key.height = unit(0.25, "inch"),
            legend.key.width = unit(1.37, "inch"),
            legend.title.align=0.5,
            plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
            plot.tag.position = c(0.15, 0.02),
            axis.title.y.right = element_text(margin = margin(l = 83)),
            legend.position = 'bottom'
        )
        ggsave(plot = p, file = paste0(dirName, '/', tFile, '_', diagList[idiag], 'Map_', format(Sys.time(), "%y%m%d"), ".png"), height = 6, width = 8, unit = 'in', dpi = 300)

        ## save copy of tsurf plot with different scale ##
        if(diagList[idiag] == 'tsurf') {
            p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(-40, 40, (80)/5), oob = scales::squish)
            ggsave(plot = p, file = paste0(dirName, '/', tFile, '_', diagList[idiag], 'Scale_Map_', format(Sys.time(), "%y%m%d"), ".png"), height = 6, width = 8, unit = 'in', dpi = 300)
        }


        out <- merge(diagm, ocnm, by = c('lon', 'lat'), all.x = TRUE, all.y = TRUE)
        out <- cbind(tFile, diagList[idiag], out)
        colnames(out)[1:2] <- c('run', 'diag')
        saveRDS(out, file = paste0(dirName, '/', 'MapData_', tFile, '_', diagList[idiag], '_', format(Sys.time(), "%y%m%d"), ".rds"))

        tAll <- readRDS(paste0('MapData_TopoEns_All_', format(Sys.time(), "%y%m%d"), ".rds"))
        tAll <- rbind(tAll, out)
        saveRDS(tAll, file = paste0(dirName, '/', 'MapData_TopoEns_All_', format(Sys.time(), "%y%m%d"), ".rds"))
    }
    nc_close(nc)
}




## calculate aridity ##

tFile <- strsplit(aijFiles[], split = '[.]')

fileName <- data.frame()
for(i in 1:length(tFile)) {
    x <- str_replace(unlist(tFile[[i]][2]), 'aij', '')
    fileName <- rbind(fileName, cbind(aijFiles[i], x))
}

aridPAL <- c('red4', 'orange3', 'khaki3', 'greenyellow', 'seagreen4')
names(aridPAL) <- c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid')
tPAL <- c('#050fd9', '#6dc1ff', '#C0FF9F', '#54D244', '#9f0000')
names(tPAL) <- c('Polar', 'SubTemperate', 'Temperate', 'Tropical', 'SuperTropical')

aridityOut <- data.frame()
for(irun in 1:length(tFile)) {

    tRunList <- list.files(paste0('./', dirName, '/'), pattern = paste0('MapData_', fileName[irun,2]))
    prec <- tRunList[grep('_prec_', tRunList)]
    #evap <- tRunList[grep('_pot_evap_', tRunList)]
    evap <- tRunList[grep('_pev_pen_', tRunList)]
    prec <- readRDS(paste0(dirName, '/', prec))
    evap <- readRDS(paste0(dirName, '/', evap))
    prec <- prec[,c(1,3:4,6,5)]
    evap <- evap[,c(3:5)]



    ## need to extract gridcell area as well ##
    nc <- nc_open(fileName[irun,1])
    grid <- ncvar_get(nc, varid = 'axyp')
    colnames(grid) <- latList
    row.names(grid) <- lonList
    gridm <- melt(grid)
    colnames(gridm) <- c('lon', 'lat', 'GridArea')
    globTot <- sum(gridm$GridArea) # m^2
    # landfr <- ncvar_get(nc, varid = 'landfr')
    # colnames(landfr) <- latList
    # row.names(landfr) <- lonList
    # landfrm <- melt(landfr)
    nc_close(nc)
    lakefr <- readRDS(paste0(dirName, '/', tRunList[grep('_lakefr_', tRunList)]))
    colnames(lakefr)[5] <- 'lakefr'
    lakefr <- lakefr[,3:5]
    tsurf <- readRDS(paste0(dirName, '/', tRunList[grep('_tsurf_', tRunList)]))
    colnames(tsurf)[5] <- 'tsurf'
    tsurf <- tsurf[,3:5]

    arid <- merge(prec, evap, by = c('lon', 'lat'), all.x = TRUE, all.y = TRUE)
    colnames(arid)[5:6] <- c('prec', 'pot_evap')
    arid <- merge(arid, gridm, by = c('lon', 'lat'))
    arid <- merge(arid, lakefr, by = c('lon', 'lat'))
    arid <- merge(arid, tsurf, by = c('lon', 'lat'))
    


    arid <- arid %>% mutate(arid, aridityIndex = prec / (pot_evap))
    ## discrete scale ##
    #arid <- arid %>% cut(arid$aridityIndex, breaks = c(0, 0.05, 0.2, 0.5, 0.65, Inf, NA), labels = c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid'))

    aridLabels <- cut(arid$aridityIndex, breaks = c(0, 0.05, 0.2, 0.5, 0.65, Inf), labels = c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid'))
    #aridLabels <- cut(arid$aridityIndex, breaks = c(0, 0.05, 0.2, 0.5, 0.65, Inf), labels = c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid'))
    arid <- cbind(arid, aridLabels)
    tempLabels <- cut(arid$tsurf, breaks = c(-Inf, -10, 5, 20, 35, Inf), labels = c('Polar', 'SubTemperate', 'Temperate', 'Tropical', 'SuperTropical'))
    arid <- cbind(arid, tempLabels)

    ## MAYBE ADJUST THIS ##
    arid$venAridTemp <- ifelse(arid$aridityIndex >= 0.2 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, 0)
    #arid$venAridTemp <- ifelse(arid$aridityIndex >= 0.2 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, 0)
    arid$venAridTemp <- ifelse(is.na(arid$venAridTemp) == TRUE, 0, arid$venAridTemp)

    arid$venLandLakeOcean <- ifelse(arid$aridityIndex >= 0.2 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, ifelse(arid$lakefr >= 50 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, ifelse(arid$landfr == 0 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, 0)))
    #arid$venLandLakeOcean <- ifelse(arid$aridityIndex >= 0.2 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, ifelse(arid$lakefr >= 50 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, ifelse(arid$landfr == 0 & arid$tsurf >= -10 & arid$tsurf <= 35, 1, 0)))    
    arid$venLandLakeOcean <- ifelse(is.na(arid$venLandLakeOcean) == TRUE, 1, arid$venLandLakeOcean)

    landTot <- sum(subset(arid, landfr == 1)$GridArea) # m^2

    
    # ## ARIDITY LABELS ##
    # p <- ggplot(arid, aes(x = lon, y = lat, fill = aridLabels))
    # p <- p + geom_tile(aes(fill = aridLabels), width = 6, height = 5)
    # p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    # p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', linewidth = 0.75)
    # p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    # p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    # p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    # p <- p + xlab(NULL)
    # p <- p + ylab(NULL)
    # p <- p + scale_fill_manual(name = 'Aridity Index', limits = names(aridPAL), values = aridPAL, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid'))
    # p <- p + ggtitle(NULL)
    # p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Aridity Index'))
    # p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    # p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
    #     face = "bold"),
    #     text = element_text(size = 18),
    #     axis.text.x = element_text(size = 14),
    #     aspect.ratio = 0.625,
    #     axis.line = element_line(color = "black"),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.background = element_blank(),
    #     panel.border = element_rect(color = "black", fill=NA, size=2),
    #     legend.key=element_blank(),
    #     legend.key.height = unit(0.25, "inch"),
    #     legend.key.width = unit(3.25, "inch"),
    #     legend.title.align=0.5,
    #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    #     plot.tag.position = c(0.15, 0.02),
    #     axis.title.y.right = element_text(margin = margin(l = 83)),
    #     strip.background =element_rect(fill="white"),
    #     legend.position = 'bottom'
    # )

    # ## HABITABLE LAND (aridity background) ##
    # p <- ggplot()
    # p <- p + geom_tile(data = arid, aes(x = lon, y = lat, fill = aridLabels), width = 6, height = 5)
    # p <- p + scale_fill_manual(name = 'Aridity Index', limits = names(aridPAL), values = aridPAL, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid'))
    # p <- p + geom_tile(data = subset(arid, venAridTemp == 0), aes(x = lon, y = lat), fill = 'black', width = 1, height = 0.75)
    # p <- p + geom_contour(data = arid, aes(x = lon, y = lat, z = venAridTemp), breaks = 0.01, color = 'black', size = 1)
    # p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25)
    # p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', size = 0.75)
    # p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25)
    # p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    # p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    # p <- p + xlab(NULL)
    # p <- p + ylab(NULL)
    # p <- p + ggtitle(NULL)
    # p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Habitable Regions'))
    # p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    # p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
    #     face = "bold"),
    #     text = element_text(size = 18),
    #     axis.text.x = element_text(size = 14),
    #     aspect.ratio = 0.625,
    #     axis.line = element_line(color = "black"),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.background = element_blank(),
    #     panel.border = element_rect(color = "black", fill=NA, size=2),
    #     legend.key=element_blank(),
    #     legend.key.height = unit(0.25, "inch"),
    #     legend.key.width = unit(3.25, "inch"),
    #     legend.title.align=0.5,
    #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    #     plot.tag.position = c(0.15, 0.02),
    #     axis.title.y.right = element_text(margin = margin(l = 83)),
    #     strip.background =element_rect(fill="white"),
    #     legend.position = 'bottom'
    # )


    ## HABITABLE ZONES (aridity background) ## (land + lake + ocean)
    p <- ggplot()
    p <- p + geom_tile(data = arid, aes(x = lon, y = lat, fill = aridLabels), width = 6, height = 5)
    p <- p + scale_fill_manual(name = 'Aridity Index', limits = names(aridPAL), values = aridPAL, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Dry Subhumid', 'Humid'))
    p <- p + geom_tile(data = subset(arid, venLandLakeOcean == 0), aes(x = lon, y = lat), fill = 'black', width = 1.2, height = 0.75)
    p <- p + geom_contour(data = arid, aes(x = lon, y = lat, z = venLandLakeOcean), breaks = 0.01, color = 'black', size = 1)
    p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25)
    p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', size = 0.75)
    p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25)
    p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    p <- p + xlab(NULL)
    p <- p + ylab(NULL)
    p <- p + ggtitle(NULL)
    p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Habitable Regions'))
    p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
        face = "bold"),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        aspect.ratio = 0.625,
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, size=2),
        legend.key=element_blank(),
        legend.key.height = unit(0.25, "inch"),
        legend.key.width = unit(3.25, "inch"),
        legend.title.align=0.5,
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
        plot.tag.position = c(0.15, 0.02),
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        legend.position = 'bottom'
    )
    ggsave(plot = p, file = paste0(dirName, '/', fileName[irun,2], '_', 'AridityMap_HabZones_', format(Sys.time(), "%y%m%d"), ".png"), height = 6, width = 8, unit = 'in', dpi = 300)



    # ## then do plot with T ##
    # p <- ggplot()
    # p <- p + geom_tile(data = arid, aes(x = lon, y = lat, fill = tempLabels), width = 6, height = 5)
    # p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    # p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', linewidth = 0.75)
    # p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    # p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    # p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    # p <- p + xlab(NULL)
    # p <- p + ylab(NULL)
    # p <- p + scale_fill_manual(name = 'Temperature Climate', limits = names(tPAL), values = tPAL, labels = c('Polar', 'SubTemperate', 'Temperate', 'Tropical', 'SuperTropical'))
    # p <- p + ggtitle(NULL)
    # p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Temperature Climate'))
    # p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    # p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
    #     face = "bold"),
    #     text = element_text(size = 18),
    #     axis.text.x = element_text(size = 14),
    #     aspect.ratio = 0.625,
    #     axis.line = element_line(color = "black"),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.background = element_blank(),
    #     panel.border = element_rect(color = "black", fill=NA, size=2),
    #     legend.key=element_blank(),
    #     legend.key.height = unit(0.25, "inch"),
    #     legend.key.width = unit(3.25, "inch"),
    #     legend.title.align=0.5,
    #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    #     plot.tag.position = c(0.15, 0.02),
    #     axis.title.y.right = element_text(margin = margin(l = 83)),
    #     strip.background =element_rect(fill="white"),
    #     legend.position = 'bottom'
    # )


    # ## HABITABLE LAND (T background) ##
    # p <- ggplot()
    # p <- p + geom_tile(data = arid, aes(x = lon, y = lat, fill = tempLabels), width = 6, height = 5)
    # p <- p + scale_fill_manual(name = 'Temperature Climate', limits = names(tPAL), values = tPAL, labels = c('Polar', 'SubTemperate', 'Temperate', 'Tropical', 'SuperTropical'))
    # p <- p + geom_tile(data = subset(arid, venAridTemp == 0), aes(x = lon, y = lat), fill = 'black', width = 1, height = 0.75)
    # p <- p + geom_contour(data = arid, aes(x = lon, y = lat, z = venAridTemp), breaks = 0.01, color = 'black', size = 1)
    # p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    # p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', linewidth = 0.75)
    # p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    # p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    # p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    # p <- p + xlab(NULL)
    # p <- p + ylab(NULL)
    # p <- p + ggtitle(NULL)
    # p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Temperature Climate'))
    # p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    # p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
    #     face = "bold"),
    #     text = element_text(size = 18),
    #     axis.text.x = element_text(size = 14),
    #     aspect.ratio = 0.625,
    #     axis.line = element_line(color = "black"),
    #     panel.grid.major = element_blank(),
    #     panel.grid.minor = element_blank(),
    #     panel.background = element_blank(),
    #     panel.border = element_rect(color = "black", fill=NA, size=2),
    #     legend.key=element_blank(),
    #     legend.key.height = unit(0.25, "inch"),
    #     legend.key.width = unit(3.25, "inch"),
    #     legend.title.align=0.5,
    #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    #     plot.tag.position = c(0.15, 0.02),
    #     axis.title.y.right = element_text(margin = margin(l = 83)),
    #     strip.background =element_rect(fill="white"),
    #     legend.position = 'bottom'
    # )

    ## HABITABLE ZONES (T background) ## (land + lake + ocean)
    p <- ggplot()
    p <- p + geom_tile(data = arid, aes(x = lon, y = lat, fill = tempLabels), width = 6, height = 5)
    p <- p + scale_fill_manual(name = 'Temperature Climate', limits = names(tPAL), values = tPAL, labels = c('Polar', 'SubTemperate', 'Temperate', 'Tropical', 'SuperTropical'))
    p <- p + geom_tile(data = subset(arid, venLandLakeOcean == 0), aes(x = lon, y = lat), fill = 'black', width = 1, height = 0.75)
    p <- p + geom_contour(data = arid, aes(x = lon, y = lat, z = venLandLakeOcean), breaks = 0.01, color = 'black', size = 1)
    p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', linewidth = 0.75)
    p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, linewidth = 0.25)
    p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    p <- p + xlab(NULL)
    p <- p + ylab(NULL)
    p <- p + ggtitle(NULL)
    p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Temperature Climate'))
    p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
        face = "bold"),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        aspect.ratio = 0.625,
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, size=2),
        legend.key=element_blank(),
        legend.key.height = unit(0.25, "inch"),
        legend.key.width = unit(3.25, "inch"),
        legend.title.align=0.5,
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
        plot.tag.position = c(0.15, 0.02),
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        legend.position = 'bottom'
    )
    ggsave(plot = p, file = paste0(dirName, '/', fileName[irun,2], '_', 'TempZonesMap_HabZones_', format(Sys.time(), "%y%m%d"), ".png"), height = 6, width = 8, unit = 'in', dpi = 300)


    landHab <- subset(arid, venAridTemp == 1)
    landHab <- (sum(landHab$GridArea) / landTot) * 100
    globHab <- subset(arid, venLandLakeOcean == 1)
    globHab <- (sum(globHab$GridArea) / globTot) * 100

    landHyper <- subset(arid, aridLabels == 'Hyperarid')
    globHyper <- (sum(landHyper$GridArea) / landTot) * 100
    landHyper <- (sum(landHyper$GridArea) / landTot) * 100

    landArid <- subset(arid, aridLabels == 'Arid')
    globArid <- (sum(landArid$GridArea) / globTot) * 100
    landArid <- (sum(landArid$GridArea) / landTot) * 100

    landSemi <- subset(arid, aridLabels == 'Semiarid')
    globSemi <- (sum(landSemi$GridArea) / globTot) * 100
    landSemi <- (sum(landSemi$GridArea) / landTot) * 100

    landSubHum <- subset(arid, aridLabels == 'Dry Subhumid')
    globSubHum <- (sum(landSubHum$GridArea) / globTot) * 100
    landSubHum <- (sum(landSubHum$GridArea) / landTot) * 100

    landHumid <- subset(arid, aridLabels == 'Humid')
    globHumid <- (sum(landHumid$GridArea) / globTot) * 100
    landHumid <- (sum(landHumid$GridArea) / landTot) * 100

    landPol <- subset(arid, tempLabels == 'Polar')
    globPol <- (sum(landPol$GridArea) / globTot) * 100
    landPol <- (sum(landPol$GridArea) / landTot) * 100

    landSubTemp <- subset(arid, tempLabels == 'SubTemperate')
    globSubTemp <- (sum(landSubTemp$GridArea) / globTot) * 100
    landSubTemp <- (sum(landSubTemp$GridArea) / landTot) * 100

    landTemp <- subset(arid, tempLabels == 'Temperate')
    globTemp <- (sum(landTemp$GridArea) / globTot) * 100
    landTemp <- (sum(landTemp$GridArea) / landTot) * 100

    landTrop <- subset(arid, tempLabels == 'Tropical')
    globTrop <- (sum(landTrop$GridArea) / globTot) * 100
    landTrop <- (sum(landTrop$GridArea) / landTot) * 100

    landSupTrop <- subset(arid, tempLabels == 'SuperTropical')
    globSupTrop <- (sum(landSupTrop$GridArea) / globTot) * 100
    landSupTrop <- (sum(landSupTrop$GridArea) / landTot) * 100

    tOut <- cbind(arid$run[1], landHab, globHab, landHyper, globHyper, landArid, globArid, landSemi, globSemi, landSubHum, globSubHum, landHumid, globHumid)
    tOut <- cbind(tOut, landPol, globPol, landSubTemp, globSubTemp, landTemp, globTemp, landTrop, globTrop, landSupTrop, globSupTrop)

    colnames(tOut)[1:13] <- c('runName', 'LandHabitability', 'GlobalHabitability', 'LandHyperarid', 'GlobalHyperarid', 'LandArid', 'GlobalArid', 'LandSemiarid', 'GlobalSemiarid', 'LandDrySubhumid', 'GlobalDrySubhumid', 'LandHumid', 'GlobalHumid')
    colnames(tOut)[14:23] <- c('LandPolar', 'GlobalPolar', 'LandSubTemperate', 'GlobalSubTemperate', 'LandTemperate', 'GlobalTemperate', 'LandTropical', 'GlobalTropical', 'LandSuperTropical', 'GlobalSuperTropical')
    aridityOut <- rbind(aridityOut, tOut)


    ## Idea: plot polar/alpine areas (tsurf < 0) as blue, plot extreme heat areas (> 40 or > 50) as dark grey ##
    ## then plot temperate areas (0 - 20?; lighter colors) and hot areas (20-40? 20-50?; darker colors) by aridity index ##


    ## then summarize for line plots: run, %hyper, %arid, etc... ##
}

saveRDS(aridityOut, file = paste0(dirName, '/', 'AllRuns_AridityIndex_', format(Sys.time(), "%y%m%d"), ".rds"))




### Now, do facets here ###
### get all diags, facet by water and rotation ###

rotLab <- c(' ', ' ')
names(rotLab) <- c(0, 90)

for(idiag in 1:length(diagList)) {
    tdiagList <- aijFiles <- list.files(paste0(getwd(), '/', dirName), pattern = paste0('_', diagList[idiag], '_'))
    tdiagList <- tdiagList[!grepl('aqua', tdiagList)]
    tdiagList <- tdiagList[!grepl('E1oM20', tdiagList)]
    tdiagList <- tdiagList[!grepl('.png', tdiagList)]

    allDiag <- data.frame()
    for(ifile in 1:length(tdiagList)) {
        tfile <- readRDS(file = paste0(dirName, '/', tdiagList[ifile]))
        allDiag <- rbind(allDiag, tfile)
    }

    allDiag <- allDiag %>% mutate(allDiag, rotate = as.numeric(str_split(str_split(run, '_', simplify = TRUE)[,2], 'x', simplify = TRUE)[,1]))
    allDiag <- allDiag %>% mutate(allDiag, wc = as.numeric(str_split(str_split(run, '_', simplify = TRUE)[,2], 'x', simplify = TRUE)[,2]))
    allDiag$rotate <- factor(allDiag$rotate, levels = c(0,90))

    if(diagList[idiag] == 'ZSI') {
        diagm$value <- ifelse(is.na(diagm$value), 0, diagm$value)
    }

    ## do limits and colors here ##
    tmin <- 0
    if(diagList[idiag] == 'tsurf') {
        tmin <- subset(limits2, diag == 'tsurf')[,2]
        tmax <- subset(limits2, diag == 'tsurf')[,3]
    } else if(diagList[idiag] == 'prec' | diagList[idiag] == 'pot_evap' | diagList[idiag] == 'ZSI' | diagList[idiag] == 'snowdp'){
        tmax <- subset(limits2, diag == diagList[idiag])[,3]
    } else {
        tmax <- 100
    }

    p <- ggplot(allDiag, aes(x = lon, y = lat, fill = value))
    p <- p + geom_tile(aes(fill = value), width = 6, height = 5)
    p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25)
    p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', size = 0.75)
    p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25)
    p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
    p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
    p <- p + xlab(NULL)
    p <- p + ylab(NULL)

    p <- p + facet_grid(rows = vars(wc), cols = vars(rotate), labeller = labeller(rotate = rotLab), switch = 'y') # , labeller = labeller(Diagnostic = climLab)

    if(diagList[idiag] %in% BWList) {
        if(diagList[idiag] == 'snowdp') {
            p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(0, tmax), guide = 'colorbar', breaks = seq(0,tmax, tmax/5), oob = scales::squish)
        } else if(diagList[idiag] == 'ZSI') {
            p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(0, tmax), guide = 'colorbar', breaks = seq(0,tmax, tmax/5), oob = scales::squish)
        } else {
            p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(0,100, 25), oob = scales::squish)
        }
    } else if(diagList[idiag] %in% UWList) {                                                        
        p <- p + scale_fill_gradient(low = 'white', high = 'blue', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/5), oob = scales::squish)
    } else if(diagList[idiag] %in% RWList) {
        p <- p + scale_fill_gradient(low = 'white', high = 'red', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/7), oob = scales::squish)
    } else {
        # p <- p + scale_fill_gradientn(colors = panoplyPAL, values = seq(tmin, tmax, ((tmax*2)/16)), limits = c(tmin, tmax), guide = 'colorbar')
        p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/7), oob = scales::squish)
    }

    #scale_fill_manual(limits = names(palTemp), values = palTemp) +
    #guides(fill = guide_colorsteps(show.limits = TRUE, title = NULL)) +
    p <- p + geom_contour(aes(x = lon, y = lat, z = landfr), breaks = 1, color = 'black')
    p <- p + ggtitle(NULL)
    p <- p + guides(fill = guide_colorbar(title.position = 'right', title = diagNames[idiag]))
    p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
        face = "bold"),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        aspect.ratio = 0.625,
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill=NA, size=2), #
        legend.key=element_blank(),
        legend.title = element_text(angle = -90),
        legend.key.height = unit(2.15, "inch"),
        legend.key.width = unit(0.15, "inch"),
        legend.title.align=0.5,
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
        plot.tag.position = c(0.15, 0.02),
        axis.title.y.right = element_text(margin = margin(l = 83)),
        axis.line.y = element_blank(),
        strip.background =element_rect(fill="white", color = 'white'),
        legend.position = 'right'
    )
    ggsave(plot = p, file = paste0(dirName, '/', 'FacetMaps_', diagList[idiag], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 12, width = 6, unit = 'in', dpi = 300)

    if(diagList[idiag] == 'tsurf') {
        p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(-40,40), guide = 'colorbar', breaks = seq(-40, 40, (80)/5), labels = c('<-40', seq(-24, 24, 16), '>40'), oob = scales::squish)
        ggsave(plot = p, file = paste0(dirName, '/', 'FacetMaps_', diagList[idiag], 'Scale_', format(Sys.time(), "%y%m%d"), ".png"), height = 12, width = 6, unit = 'in', dpi = 300)
    }
}




## 50% snow ice fraction facets ##
idiag=9
tdiagList <- aijFiles <- list.files(paste0(getwd(), '/', dirName), pattern = paste0('_snowicefr_'))
tdiagList <- tdiagList[!grepl('aqua', tdiagList)]
tdiagList <- tdiagList[!grepl('E1oM20', tdiagList)]
tdiagList <- tdiagList[!grepl('.png', tdiagList)]
tdiagList <- tdiagList[grepl('x50_', tdiagList)]

allDiag <- data.frame()
for(ifile in 1:length(tdiagList)) {
    tfile <- readRDS(file = paste0(dirName, '/', tdiagList[ifile]))
    allDiag <- rbind(allDiag, tfile)
}

allDiag <- allDiag %>% mutate(allDiag, rotate = as.numeric(str_split(str_split(run, '_', simplify = TRUE)[,2], 'x', simplify = TRUE)[,1]))
allDiag <- allDiag %>% mutate(allDiag, wc = as.numeric(str_split(str_split(run, '_', simplify = TRUE)[,2], 'x', simplify = TRUE)[,2]))
allDiag$rotate <- factor(allDiag$rotate, levels = c(0,90))

## do limits and colors here ##
tmin <- 0
if(diagList[idiag] == 'tsurf') {
    tmin <- subset(limits2, diag == 'tsurf')[,2]
    tmax <- subset(limits2, diag == 'tsurf')[,3]
} else if(diagList[idiag] == 'prec' | diagList[idiag] == 'pot_evap' | diagList[idiag] == 'ZSI' | diagList[idiag] == 'snowdp'){
    tmax <- subset(limits2, diag == diagList[idiag])[,3]
} else {
    tmax <- 100
}

p <- ggplot(allDiag, aes(x = lon, y = lat, fill = value))
p <- p + geom_tile(aes(fill = value), width = 6, height = 5)
p <- p + geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25)
p <- p + geom_vline(xintercept = c(-175, 175), color = 'black', size = 0.75)
p <- p + geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25)
p <- p + scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL)
p <- p + scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = NULL)
p <- p + xlab(NULL)
p <- p + ylab(NULL)

p <- p + facet_grid(rows = vars(wc), cols = vars(rotate), labeller = labeller(NULL), switch = 'y') # , labeller = labeller(Diagnostic = climLab)

if(diagList[idiag] %in% BWList) {
    if(diagList[idiag] == 'snowdp') {
        p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(0, tmax), guide = 'colorbar', breaks = seq(0,tmax, tmax/5), oob = scales::squish)
    } else if(diagList[idiag] == 'ZSI') {
        p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(0, tmax), guide = 'colorbar', breaks = seq(0,tmax, tmax/5), oob = scales::squish)
    } else {
        p <- p + scale_fill_gradient(low = 'grey20', high = 'white', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(0,100, 25), oob = scales::squish)
    }
} else if(diagList[idiag] %in% UWList) {                                                        
    p <- p + scale_fill_gradient(low = 'white', high = 'blue', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/5), oob = scales::squish)
} else if(diagList[idiag] %in% RWList) {
    p <- p + scale_fill_gradient(low = 'white', high = 'red', limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/7), oob = scales::squish)
} else {
    # p <- p + scale_fill_gradientn(colors = panoplyPAL, values = seq(tmin, tmax, ((tmax*2)/16)), limits = c(tmin, tmax), guide = 'colorbar')
    p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(tmin, tmax), guide = 'colorbar', breaks = seq(tmin, tmax, (tmax-tmin)/7), oob = scales::squish)
}

#scale_fill_manual(limits = names(palTemp), values = palTemp) +
#guides(fill = guide_colorsteps(show.limits = TRUE, title = NULL)) +
p <- p + geom_contour(aes(x = lon, y = lat, z = landfr), breaks = 1, color = 'black')
p <- p + ggtitle(NULL)
p <- p + guides(fill = guide_colorbar(title.position = 'bottom', title = 'Snow Ice Fraction (%)'))
p <- p + coord_map('moll') ## <- converts map to mollweide projection (equal area, pseudocylindrical)
p <- p + theme(plot.title = element_text(hjust = 0.5, size = 21,
    face = "bold"),
    text = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = 'black', fill=NA, size=2), #
    legend.key=element_blank(),
    legend.title = element_text(angle = 0),
    legend.key.height = unit(0.15, "inch"),
    legend.key.width = unit(2.25, "inch"),
    legend.title.align=0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    axis.line.y = element_blank(),
    strip.background =element_rect(fill="white", color = 'white'),
    strip.text = element_blank(),
    legend.position = 'bottom'
)
ggsave(plot = p, file = paste0(dirName, '/', 'FacetMaps_snowice_50_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 11.75, unit = 'in', dpi = 300)

















aij <- readRDS(aijFiles[length(aijFiles)])
oijl <- readRDS(list.files(getwd(), pattern = 'PlotData_TopoEns_oijl'))

aijVar <- unique(aij$Diagnostic)


##

for(ivar in 1:length(aijVar)) {
    varTab <- subset(aij, Diagnostic == aijVar[ivar])
    err <- try(cut(varTab$value, breaks = quantile(varTab$value, c(0,0.2,0.4,0.6,0.8,1), na.rm=TRUE)), silent = TRUE)
    if(inherits(err, 'try-error')) {
        brk <- seq(0, max(varTab$value), max(varTab$value) / 5)
        varTab <- cbind(varTab, cut(varTab$value, breaks = brk))
    } else {
        varTab <- cbind(varTab, cut(varTab$value, breaks = quantile(varTab$value, c(0,0.2,0.4,0.6,0.8,1), na.rm=TRUE)))

    }
    colnames(varTab)[8] <- 'valueDis'
    palTemp <- magma(5)
    names(palTemp) <- levels(varTab$valueDis)


    ggplot(varTab, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = valueDis), width = 6, height = 5) +
    #geom_contour_filled() + ## <- basic contour plot 
    #geom_contour(data = cont, aes(x = lon, y = lat, z = cont), breaks = -500, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
    geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
    geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
    #annotate(geom = 'text', label = 'Temperature', x = 265, y = 0, angle = -90, size = 7) +
    scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
    scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
    #coord_cartesian(xlim = c(-175, 175), ylim = c(-88, 88), clip = 'off') +
    xlab("Longitude") +
    ylab("Latitude") +
    facet_grid(cols=vars(WaterContent), rows = vars(Angle)) +
    labs(title = aijVar[ivar]) +
    #scale_fill_discrete(limits = names(palTemp), values = palTemp) +
    scale_fill_manual(limits = names(palTemp), values = palTemp) +
    #scale_fill_gradient(limits = c(valLim[1], valLim[2]), guide = 'legend') + # limits = c(-200,0), low = palTemp[1], mid = palTemp[6], high = palTemp[11], midpoint = breaks[6], 
    #scale_fill_viridis(option = 'A', discrete = TRUE) +
    #coord_cartesian(expand = FALSE, xlim=c(-180,250), clip="off") +
    guides(fill = guide_colorsteps(show.limits = TRUE, title = NULL)) +
    coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
    #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
    #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
    #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
    theme(plot.title = element_text(hjust = 0.5, size = 21,
        face = "bold"),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        aspect.ratio = 0.625,
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, size=2),
        legend.key=element_blank(),
        legend.key.height = unit(1.2, "inch"),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
        plot.tag.position = c(0.15, 0.02),
        axis.title.y.right = element_text(margin = margin(l = 83)),
        #legend.position = c(1.07, 0.52)
    )

    ggsave(paste0('TopoEns_Maps_', aijVar[ivar], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 8, width = 14, unit = 'in', dpi = 300)
    print(ivar)
}