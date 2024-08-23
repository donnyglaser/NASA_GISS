## take equilibrium sum files -> calculate aridity for each cell -> plot
## maybe: biomes??

library(ncdf4)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tidyverse)
library(mapproj) # global map projection library
library(reshape)
library(plyr)
library(viridis)
library(shadowtext)

subDir <- paste0('Plots_', format(Sys.time(), "%y%m%d"))
dir.create(subDir)
fileList <- list.files(getwd(), pattern = c('ANN0091-0100.aij', '*', '.nc'))
rotList <- data.frame(Angle = c(0, 22, 45, 60, 90), Name = c('_0x', '_22x', '_45x', '_60x', '_90x')) 
WCList <- data.frame(WC = c(0, 1, 5, 10, 20, 30, 40, 50), Name = c('x0_', 'x1_', 'x5_', 'x10_', 'x20_', 'x30_', 'x40_', 'x50_'))
latList <- seq(-90,90,4)
lonList <- seq(-177.5,177.5,5)
aridList <- c(0, 0.05, 0.2, 0.5, 0.65, 998, 1000)
aridConv <- c("(0,0.05]"='Hyperarid', "(0.05,0.2]"='Arid', "(0.2,0.5]"='Semiarid', "(0.5,0.65]"='DrySubhumid', "(0.65,998]"='Humid', '(998,1e+03]'='Ocean')
planetR <- 6357 * 1000 # Earth radius in m
areaList <- expand.grid(latList, lonList)
colnames(areaList) <- c('Lat', 'Lon')
areaList <- areaList %>% mutate(Area = ((planetR^2) * (sin(abs(Lat * 0.0175)) - sin(abs(Lat * 0.0175) - 0.07)) * 0.0875))
areaG <- sum(areaList$Area)

aridPAL <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6", '#000251')
names(aridPAL) <- c('Hyperarid', 'Arid', 'Semiarid', 'DrySubhumid', 'Humid', 'Ocean')

tempSHP <- c('|', '+', '*') #✳︎
names(tempSHP) <- c('Cold', 'Temperate', 'Hot')

tempPAL <- c(1, 0.7, 0.4)
names(tempPAL) <- c('Cold', 'Temperate', 'Hot')

biomePAL <- c('#a30306', '#fc9735', '#fcfc8d', '#83cce6', '#0769b3', '#D7191C', '#FDAE61', '#FFFFBF', '#ABD9E9', '#2C7BB6', '#fc4245', '#fac591', '#fcfcd4', '#bad8e3', '#4a87b5', '#000251')
names(biomePAL) <- c('Cold_Hyperarid', 'Cold_Arid', 'Cold_Semiarid', 'Cold_DrySubhumid', 'Cold_Humid', 'Temperate_Hyperarid', 'Temperate_Arid', 'Temperate_Semiarid', 'Temperate_DrySubhumid', 'Temperate_Humid', 'Hot_Hyperarid', 'Hot_Arid', 'Hot_Semiarid', 'Hot_DrySubhumid', 'Hot_Humid', 'Ocean')

varList <- c('GlobalPreip', 'GlobalEvap', 'GlobalSurfTemp', 'GlobalWaterColumn', 'GlobalCloudCover', 'LandAridity', 'GlobalHyper', 'LandHyper', 'GlobalArid', 'LandArid', 'GlobalSemiarid', 'LandSemiarid', 'GlobalDrySubhumid', 'LandDrySubhumid', 'GlobalHumid', 'LandHumid', 'OceanFraction', 'LandFraction')


### INDIVIDUAL PLOTS ###
dataOut <- data.frame()
mapDataOut <- data.frame()
for(irot in 1:nrow(rotList)) {
    subRot <- fileList[grep(rotList[irot,2],fileList)]
    for(iwc in 1:nrow(WCList)) { 
        if((irot == 4) & (iwc == 6)) {

        } else {
            subwc <- subRot[grep(WCList[iwc,2],subRot)]

            nc <- nc_open(subwc)

            precip <- ncvar_get(nc, varid = 'prec')
            colnames(precip) <- latList
            row.names(precip) <- lonList
            evap <- ncvar_get(nc, varid = 'pev_pen')
            colnames(evap) <- latList
            row.names(evap) <- lonList
            topo <- ncvar_get(nc, varid = 'topog')
            colnames(topo) <- latList
            row.names(topo) <- lonList
            tsurf <- ncvar_get(nc, varid = 'tsurf')
            colnames(tsurf) <- latList
            row.names(tsurf) <- lonList
            arid <- precip / evap
            qatm <- ncvar_get(nc, varid = 'qatm')
            colnames(qatm) <- latList
            row.names(qatm) <- lonList
            pcldt <- ncvar_get(nc, varid = 'pcldt')
            colnames(pcldt) <- latList
            row.names(pcldt) <- lonList

            precipM <- melt(precip)
            colnames(precipM) <- c('Lon', 'Lat', 'Prec_mmday')
            evapM <- melt(evap)
            colnames(evapM) <- c('Lon', 'Lat', 'evap_mmday')
            topoM <- melt(topo)
            colnames(topoM) <- c('Lon', 'Lat', 'topo_m')
            foc <- subset(topoM, topo_m <= 0)
            foc <- merge(foc, areaList, by = c('Lat', 'Lon'), all.x = T)
            tsurfM <- melt(tsurf)
            colnames(tsurfM) <- c('Lon', 'Lat', 'tsurf_C')
            aridM <- melt(arid)
            colnames(aridM) <- c('Lon', 'Lat', 'Aridity')
            aridM <- merge(aridM, topoM, by = c('Lat', 'Lon'), all.x = T)
            aridM[aridM$topo_m == 0, 3] <- 999
            aridM <- cbind(aridM, cut(aridM$Aridity, breaks = aridList))
            colnames(aridM)[5] <- 'AridCategory'
            aridM$AridCategory <- revalue(aridM$AridCategory, aridConv)
            qatmM <- melt(qatm)
            colnames(qatmM) <- c('Lon', 'Lat', 'WaterColumn')
            pcldtM <- melt(pcldt)
            colnames(pcldtM) <- c('Lon', 'Lat', 'Clouds')


            ### WRITE GLOBAL DATA ###
            precG <- ncvar_get(nc, varid = 'prec_hemis')
            precG <- precG[3]
            evapG <- ncvar_get(nc, varid = 'pev_pen_hemis')
            evapG <- evapG[3]
            tsurfG <- ncvar_get(nc, varid = 'tsurf_hemis')
            tsurfG <- tsurfG[3]
            qatmG <- ncvar_get(nc, varid = 'qatm_hemis')
            qatmG <- qatmG[3]
            pcldtG <- ncvar_get(nc, varid = 'pcldt_hemis')
            pcldtG <- pcldtG[3]
            aridityL <- subset(aridM, topo_m > 0)
            aridityL <- merge(aridityL, areaList, by = c('Lat', 'Lon'), all.x = T)
            aridityL <- aridityL %>% mutate(MeanAridity = Aridity * Area)

            ### ARID REGION DATA ###
            areaL <- sum(aridityL$Area)
            areaO <- sum(foc$Area)
            hyper <- sum(subset(aridityL, AridCategory == 'Hyperarid')$Area)
            hyper <- c((hyper / areaG), (hyper / areaL))
            arid <- sum(subset(aridityL, AridCategory == 'Arid')$Area)
            arid <- c((arid / areaG), (arid / areaL))
            semi <- sum(subset(aridityL, AridCategory == 'Semiarid')$Area)
            semi <- c((semi / areaG), (semi / areaL))
            drySub <- sum(subset(aridityL, AridCategory == 'DrySubhumid')$Area)
            drySub <- c((drySub / areaG), (drySub / areaL))
            humid <- sum(subset(aridityL, AridCategory == 'Humid')$Area)
            humid <- c((humid / areaG), (humid / areaL))
            aridityL <- sum(aridityL$MeanAridity) / areaL

            ### CALCULATE BIOME DATA HERE??? ###
            biomeM <- aridM
            biomeM <- merge(aridM, tsurfM, by = c('Lat', 'Lon'), all.x = T)
            biomeM <- cbind(biomeM, cut(biomeM$tsurf_C, breaks = c(-1000, 10, 18, 1000)))
            colnames(biomeM)[7] <- 'TempCategory'
            biomeM$TempCategory <- revalue(biomeM$TempCategory, c('(-1e+03,10]' = 'Cold', '(10,18]' = 'Temperate', '(18,1e+03]' = 'Hot'))
            biomeM$Biome <- paste0(biomeM$TempCategory, '_', biomeM$AridCategory)
            biomeM$Biome <- ifelse(biomeM$AridCategory == 'Ocean', 'Ocean', biomeM$Biome)

            tOut <- c(precG, evapG, tsurfG, qatmG, pcldtG, aridityL, hyper, arid, semi, drySub, humid, (areaO / areaG), (areaL / areaG))
            tOut <- cbind(rotList[irot,1], WCList[iwc,1], varList, tOut)
            dataOut <- rbind(dataOut, tOut)

            tsurfOut <- cbind(tsurfM[,1:2], rotList[irot,1], WCList[iwc,1], 'tsurf', tsurfM[,3])
            colnames(tsurfOut)[3:6] <- c('RotationAngle', 'WaterContent', 'Variable', 'Value')
            qatmOut <- cbind(qatmM[,1:2], rotList[irot,1], WCList[iwc,1], 'qatm', qatmM[,3])
            colnames(qatmOut)[3:6] <- c('RotationAngle', 'WaterContent', 'Variable', 'Value')
            pcldtOut <- cbind(pcldtM[,1:2], rotList[irot,1], WCList[iwc,1], 'pcldt', pcldtM[,3])
            colnames(pcldtOut)[3:6] <- c('RotationAngle', 'WaterContent', 'Variable', 'Value')
            #aridOut <- cbind(aridM[,1:2], rotList[irot,1], WCList[iwc,1], 'aridity', aridM[,3])
            #colnames(aridOut)[3:6] <- c('RotationAngle', 'WaterContent', 'Variable', 'Value')

            ### ARIDITY INDEX ###
            if(iwc > 1) {
                tempArid <- subset(aridM, topo_m == 0)
                tempArid$Aridity <- NA
                tempArid <- rbind(tempArid, subset(aridM, topo_m > 0))
            } else {
                tempArid <- aridM
            }
            aridOut <- cbind(tempArid[,1:2], rotList[irot,1], WCList[iwc,1], 'aridity', tempArid[,3])
            colnames(aridOut)[3:6] <- c('RotationAngle', 'WaterContent', 'Variable', 'Value')

            mapDataOut <- rbind(mapDataOut, tsurfOut, qatmOut, pcldtOut, aridOut)

            nc_close(nc)

            # ## ARIDITY CATEGORIES ##
            # ggplot(aridM, aes(x = Lon, y = Lat)) +
            # geom_tile(aes(fill = AridCategory), width = 6, height = 5) +
            # #geom_contour(data = topoM, aes(x = Lon, y = Lat, z = topo_m), breaks = 50, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
            # #geom_raster(data = foc, aes(x = Lon, y = Lat, color = 'black'), alpha = 1) + #, width = 6, height = 5
            # scale_fill_manual(values = aridPAL, na.value = 'black') + #"#fde725", "#aadc32", "#5ac864", "#25ac82", "#228d8d"
            # #scale_color_manual(values = 'black', name = 'the fill', guide = 'legend', labels = c('m1')) +
            # geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            # geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            # scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            # scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) + #, sec.axis = sec_axis(~., name = 'Temperature (ºC)')
            # xlab("Longitude") +
            # ylab("Latitude") +
            # guides(fill = 'legend') +
            # coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            # #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
            # theme(
            #     plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            #     text = element_text(size = 18), 
            #     axis.text.x = element_text(size = 16), 
            #     aspect.ratio = 0.625, 
            #     axis.line = element_line(color = "black"),
            #     panel.grid.major = element_blank(), 
            #     panel.grid.minor = element_blank(), 
            #     panel.background = element_blank(), 
            #     panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            #     legend.key=element_blank(), 
            #     legend.key.height = unit(1.3, 'cm'), 
            #     legend.title=element_blank(),
            #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
            #     plot.tag.position = c(0.15, 0.02), 
            #     axis.title.y.right = element_text(margin = margin(l = 83)), 
            #     #legend.position = c(1.07, 0.52), 
            #     panel.spacing = unit(5, "mm")
            # )
            # ggsave(paste0(subDir, '/', "TopoEns_Map_AridityCat_", rotList[irot,1], 'x', WCList[iwc,1], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4, width = 8, unit = 'in', dpi = 300)


            ### ARIDITY INDEX ###

            ggplot(tempArid, aes(x = Lon, y = Lat)) +
            geom_tile(aes(fill = Aridity), width = 6, height = 5) +
            #geom_contour(data = topoM, aes(x = Lon, y = Lat, z = topo_m), breaks = 50, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
            #geom_raster(data = foc, aes(x = Lon, y = Lat, color = 'black'), alpha = 1) + #, width = 6, height = 5
            scale_fill_gradient(limits = c(0,0.65), high = 'lightskyblue1', low = 'darkorange3', oob = scales::squish, na.value = '#000251') +
            #scale_color_manual(values = 'black', name = 'the fill', guide = 'legend', labels = c('m1')) +
            geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) + #, sec.axis = sec_axis(~., name = 'Temperature (ºC)')
            xlab("Longitude") +
            ylab("Latitude") +
            guides(fill = guide_colorbar(title.position = "right", title = 'Aridity Index')) + 
            coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
            #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
            #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
            theme(
                plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
                text = element_text(size = 18), 
                axis.text.x = element_text(size = 16), 
                aspect.ratio = 0.625, 
                axis.line = element_line(color = "black"), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
                legend.key=element_blank(), 
                legend.key.height = unit(1.3, 'cm'), 
                legend.title=element_text(angle = -90),
                legend.title.align = 0.5,
                plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
                plot.tag.position = c(0.15, 0.02), 
                axis.title.y.right = element_text(margin = margin(l = 83)), 
                #legend.position = c(1.07, 0.52), 
                panel.spacing = unit(5, "mm")
            )
            ggsave(paste0(subDir, '/', "TopoEns_Map_Aridity_", rotList[irot,1], 'x', WCList[iwc,1], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4, width = 8, unit = 'in', dpi = 300)


            # ## BIOME CATEGORIES ##
            # ggplot(biomeM, aes(x = Lon, y = Lat)) +
            # geom_tile(aes(x = Lon, y = Lat, fill = AridCategory, alpha = TempCategory), width = 5.1, height = 4.1) + #
            # #geom_point(aes(x = Lon, y = Lat, fill = AridCategory, shape = TempCategory)) +
            # #geom_point(aes(x = Lon, y = Lat, fill = AridCategory, alpha = TempCategory), color = 'white') +
            # #geom_point(data = subset(biomeM, TempCategory == 'Cold'), aes(x = Lon, y = Lat), shape = 1) +
            # #geom_tile(aes(color = TempCategory), width = 6, height = 5, alpha = 0.3) +
            # #geom_contour(data = topoM, aes(x = Lon, y = Lat, z = topo_m), breaks = 50, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
            # #geom_raster(data = foc, aes(x = Lon, y = Lat, color = 'black'), alpha = 1) + #, width = 6, height = 5
            # scale_fill_manual(values = aridPAL, na.value = 'black') + #"#fde725", "#aadc32", "#5ac864", "#25ac82", "#228d8d"
            # scale_alpha_manual(values = tempPAL) +
            # #scale_shape_manual(values = tempSHP) +
            # #scale_alpha_discrete(start = 0.7, end = 0.3) +
            # geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            # geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            # scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            # scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) + #, sec.axis = sec_axis(~., name = 'Temperature (ºC)')
            # xlab("Longitude") +
            # ylab("Latitude") +
            # guides(fill = 'legend', alpha = 'legend') +
            # coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            # #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
            # theme(
            #     plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            #     text = element_text(size = 18), 
            #     axis.text.x = element_text(size = 16), 
            #     aspect.ratio = 0.625, 
            #     axis.line = element_line(color = "black"),
            #     panel.grid.major = element_blank(), 
            #     panel.grid.minor = element_blank(), 
            #     panel.background = element_blank(), 
            #     panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            #     legend.key=element_blank(), 
            #     legend.key.height = unit(0.8, 'cm'), 
            #     legend.title=element_blank(),
            #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
            #     plot.tag.position = c(0.15, 0.02), 
            #     axis.title.y.right = element_text(margin = margin(l = 83)), 
            #     #legend.position = c(1.07, 0.52), 
            #     panel.spacing = unit(5, "mm")
            # )
            # ggsave(paste0(subDir, '/', "TopoEns_Map_BiomeCat_", rotList[irot,1], 'x', WCList[iwc,1], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4, width = 8, unit = 'in', dpi = 300)



            # ## TEMPERATURE ##
            # ggplot(tsurfM, aes(x = Lon, y = Lat)) +
            # geom_tile(aes(fill = tsurf_C), width = 6, height = 5) +
            # #geom_contour(data = topoM, aes(x = Lon, y = Lat, z = topo_m), breaks = 50, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
            # #geom_raster(data = foc, aes(x = Lon, y = Lat, color = 'black'), alpha = 1) + #, width = 6, height = 5
            # scale_fill_viridis_c(limits = c(-60, 40), option = 'magma', oob = scales::squish) +
            # #scale_color_manual(values = 'black', name = 'the fill', guide = 'legend', labels = c('m1')) +
            # geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            # geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            # scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            # scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) + #, sec.axis = sec_axis(~., name = 'Temperature (ºC)')
            # xlab("Longitude") +
            # ylab("Latitude") +
            # guides(fill = guide_colorbar(title.position = "right", title = 'Temperature (ºC)')) +
            # coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            # #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
            # theme(
            #     plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            #     text = element_text(size = 18), 
            #     axis.text.x = element_text(size = 16), 
            #     aspect.ratio = 0.625, 
            #     axis.line = element_line(color = "black"), 
            #     panel.grid.major = element_blank(), 
            #     panel.grid.minor = element_blank(), 
            #     panel.background = element_blank(), 
            #     panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            #     legend.key=element_blank(), 
            #     legend.key.height = unit(1.3, 'cm'),
            #     legend.title=element_text(angle = -90),
            #     legend.title.align = 0.5, 
            #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
            #     plot.tag.position = c(0.15, 0.02), 
            #     axis.title.y.right = element_text(margin = margin(l = 83)), 
            #     #legend.position = c(1.07, 0.52), 
            #     panel.spacing = unit(5, "mm")
            # )
            # ggsave(paste0(subDir, '/', "TopoEns_Map_Temp_", rotList[irot,1], 'x', WCList[iwc,1], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4, width = 8, unit = 'in', dpi = 300)


            # ### WATER COLUMN ###
            # ggplot(qatmM, aes(x = Lon, y = Lat)) +
            # geom_tile(aes(fill = WaterColumn), width = 6, height = 5) +
            # #geom_contour(data = topoM, aes(x = Lon, y = Lat, z = topo_m), breaks = 50, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
            # #geom_raster(data = foc, aes(x = Lon, y = Lat, color = 'black'), alpha = 1) + #, width = 6, height = 5
            # scale_fill_gradient(limits = c(0,50), low = '#FFFFFF', high = 'dodgerblue4', oob = scales::squish, na.value = '#000251') +
            # #scale_color_manual(values = 'black', name = 'the fill', guide = 'legend', labels = c('m1')) +
            # geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            # geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            # scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            # scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) + #, sec.axis = sec_axis(~., name = 'Temperature (ºC)')
            # xlab("Longitude") +
            # ylab("Latitude") +
            # guides(fill = guide_colorbar(title.position = "right", title = expression("Water Vapor Column (" * kg ~ m^-2 * ")"))) +
            # coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            # #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
            # theme(
            #     plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            #     text = element_text(size = 18), 
            #     axis.text.x = element_text(size = 16), 
            #     aspect.ratio = 0.625, 
            #     axis.line = element_line(color = "black"), 
            #     panel.grid.major = element_blank(), 
            #     panel.grid.minor = element_blank(), 
            #     panel.background = element_blank(), 
            #     panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            #     legend.key=element_blank(), 
            #     legend.key.height = unit(1.7, 'cm'),
            #     legend.title=element_text(angle = -90),
            #     legend.title.align = 0.5, 
            #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
            #     plot.tag.position = c(0.15, 0.02), 
            #     axis.title.y.right = element_text(margin = margin(l = 83)), 
            #     #legend.position = c(1.07, 0.52), 
            #     panel.spacing = unit(5, "mm")
            # )
            # ggsave(paste0(subDir, '/', "TopoEns_Map_qatm_", rotList[irot,1], 'x', WCList[iwc,1], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4, width = 8, unit = 'in', dpi = 300)



            # ### CLOUD COVER ###
            # ggplot(pcldtM, aes(x = Lon, y = Lat)) +
            # geom_tile(aes(fill = Clouds), width = 6, height = 5) +
            # #geom_contour(data = topoM, aes(x = Lon, y = Lat, z = topo_m), breaks = 50, color = 'black') + # breaks = seq(-200, 0, 20), bins = 10
            # #geom_raster(data = foc, aes(x = Lon, y = Lat, color = 'black'), alpha = 1) + #, width = 6, height = 5
            # scale_fill_gradient(limits = c(0,100), low = 'black', high = 'white', oob = scales::squish, na.value = '#000251') +        #scale_color_manual(values = 'black', name = 'the fill', guide = 'legend', labels = c('m1')) +
            # geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            # geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            # scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            # scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) + #, sec.axis = sec_axis(~., name = 'Temperature (ºC)')
            # xlab("Longitude") +
            # ylab("Latitude") +
            # guides(fill = guide_colorbar(title.position = "right", title = 'Total Cloud Cover (%)')) +
            # coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            # #annotate(geom = 'text', x = 97, y = -78, label = '90', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = 5, y = -78, label = '0', size = 5, color = 'grey10') +
            # #annotate(geom = 'text', x = -82, y = -78, label = '-90', size = 5, color = 'grey10') +
            # theme(
            #     plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            #     text = element_text(size = 18), 
            #     axis.text.x = element_text(size = 16), 
            #     aspect.ratio = 0.625, 
            #     axis.line = element_line(color = "black"), 
            #     panel.grid.major = element_blank(), 
            #     panel.grid.minor = element_blank(), 
            #     panel.background = element_blank(), 
            #     panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            #     legend.key=element_blank(), 
            #     legend.key.height = unit(1.7, 'cm'),
            #     legend.title=element_text(angle = -90),
            #     legend.title.align = 0.5, 
            #     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
            #     plot.tag.position = c(0.15, 0.02), 
            #     axis.title.y.right = element_text(margin = margin(l = 83)), 
            #     #legend.position = c(1.07, 0.52), 
            #     panel.spacing = unit(5, "mm")
            # )
            # ggsave(paste0(subDir, '/', "TopoEns_Map_clouds_", rotList[irot,1], 'x', WCList[iwc,1], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4, width = 8, unit = 'in', dpi = 300)
        }
    }
}

colnames(dataOut) <- c('RotationAngle', 'WaterContent', 'Variable', 'Value')

saveRDS(dataOut, file = paste0('EquilibriumSummaryData_', format(Sys.time(), "%y%m%d"), '.rds'))
saveRDS(tsurfOut, file = paste0('MapData_tsurf_', format(Sys.time(), "%y%m%d"), '.rds'))
saveRDS(qatmOut, file = paste0('MapData_qatm_', format(Sys.time(), "%y%m%d"), '.rds'))
saveRDS(pcldtOut, file = paste0('MapData_pcldt_', format(Sys.time(), "%y%m%d"), '.rds'))
saveRDS(aridOut, file = paste0('MapData_arid_', format(Sys.time(), "%y%m%d"), '.rds'))

saveRDS(mapDataOut, file = paste0('MapData_all_', format(Sys.time(), "%y%m%d"), '.rds'))


### FACET MAP PLOTS ###
varNames <- c('tsurf', 'qatm', 'pcldt', 'aridity')
varLabs <- c('Temperature (ºC)', expression("Water Vapor Column (" * kg ~ m^-2 * ")"), 'Total Cloud Cover (%)', 'Aridity Index')
##########
scaleList <- data.frame(Variable = c(), min = c(), max = c())


varTab <- subset(mapDataOut, Variable == varNames[1])
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(0,1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'GlobalSurfTemp')
sumData$Value <- round_any(as.numeric(sumData$Value), 0.1)

### TEMPERATURE ###
ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_viridis_c(limits = c(-60, 40), option = 'magma', oob = scales::squish) +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(0,1,5,10,20,30,40,50))), rows = vars(RotationAngle)) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[1])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_tsurf_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 16, unit = 'in', dpi = 300)



varTab <- subset(mapDataOut, Variable == varNames[2])
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(0,1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'GlobalWaterColumn')
sumData$Value <- round_any(as.numeric(sumData$Value), 0.1)

### Water Column ###
ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_gradient(limits = c(0,50), low = '#FFFFFF', high = 'dodgerblue4', oob = scales::squish, na.value = '#000251') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(0,1,5,10,20,30,40,50))), rows = vars(RotationAngle)) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[2])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey80"),
    strip.background = element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_qatm_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 16, unit = 'in', dpi = 300)



varTab <- subset(mapDataOut, Variable == varNames[3])
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(0,1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'GlobalCloudCover')
sumData$Value <- round_any(as.numeric(sumData$Value), 0.1)

### Cloud Cover ###
ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_gradient(limits = c(0,100), low = 'black', high = 'white', oob = scales::squish, na.value = '#000251') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(0,1,5,10,20,30,40,50))), rows = vars(RotationAngle)) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[3])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey80"),
    strip.background = element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_pcldt_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 16, unit = 'in', dpi = 300)



varTab <- subset(mapDataOut, Variable == varNames[4])
varTab$Value <- replace(varTab$Value, 999, NA)
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(0,1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'LandAridity')
sumData$Value <- round_any(as.numeric(sumData$Value), 0.01)

### Aridity ###
### need to fix this one to omit ocean cells ###
ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_gradient(limits = c(0,0.65), high = 'lightskyblue1', low = 'darkorange3', oob = scales::squish, na.value = '#000251') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(0,1,5,10,20,30,40,50))), rows = vars(RotationAngle)) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[4])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey80"),
    strip.background = element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_aridity_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 16, unit = 'in', dpi = 300)



######################################
## Just 0 and 90 ##
######################################

### TEMPERATURE ###
varTab <- subset(mapDataOut, Variable == varNames[1] & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
varTab <- subset(varTab, WaterContent != 30 & WaterContent != 40)
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'GlobalSurfTemp' & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
sumData <- subset(sumData, WaterContent != 30 & WaterContent != 40)
sumData$Value <- round_any(as.numeric(sumData$Value), 0.1)

ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_viridis_c(limits = c(-60, 40), option = 'magma', oob = scales::squish) +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(rows=vars(factor(WaterContent, levels = c(1,5,10,20,30,40,50))), cols = vars(factor(RotationAngle, levels = c(0,90)))) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[1])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1.5, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_0-90_tsurf_", format(Sys.time(), "%y%m%d"), ".png"), height = 9, width = 7, unit = 'in', dpi = 300)


### TEMPERATURE (rotate) ###
varTab <- subset(mapDataOut, Variable == varNames[1] & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
varTab <- subset(varTab, WaterContent != 30 & WaterContent != 40)
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'GlobalSurfTemp' & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
sumData <- subset(sumData, WaterContent != 30 & WaterContent != 40)
sumData$Value <- round_any(as.numeric(sumData$Value), 0.1)

ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_viridis_c(limits = c(-60, 40), option = 'magma', oob = scales::squish) +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(1,5,10,20,30,40,50))), rows = vars(factor(RotationAngle, levels = c(0,90)))) +
guides(fill = guide_colorbar(title.position = "top", title = varLabs[1], label.position = 'top')) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.8,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    #legend.key.height = unit(1.5, "inch"),
    legend.key.width = unit(1.5, "inch"),
    #legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    legend.position = 'top',
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_0-90_tsurf_rot_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)




### WATER COLUMN ###
varTab <- subset(mapDataOut, Variable == varNames[2] & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
varTab <- subset(varTab, WaterContent != 30 & WaterContent != 40)
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'GlobalWaterColumn' & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
sumData <- subset(sumData, WaterContent != 30 & WaterContent != 40)
sumData$Value <- round_any(as.numeric(sumData$Value), 0.1)

ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_gradient(limits = c(0,50), low = '#FFFFFF', high = 'dodgerblue4', oob = scales::squish, na.value = '#000251') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(rows=vars(factor(WaterContent, levels = c(1,5,10,20,30,40,50))), cols = vars(factor(RotationAngle, levels = c(0,90)))) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[2])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1.5, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_0-90_qatm_", format(Sys.time(), "%y%m%d"), ".png"), height = 9, width = 7, unit = 'in', dpi = 300)

### WATER COLUMN (rotate) ###
ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_gradient(limits = c(0,50), low = '#FFFFFF', high = 'dodgerblue4', oob = scales::squish, na.value = '#000251') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(1,5,10,20,30,40,50))), rows = vars(factor(RotationAngle, levels = c(0,90)))) +
guides(fill = guide_colorbar(title.position = "top", title = varLabs[2], label.position = 'top')) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.8,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    #legend.key.height = unit(1.5, "inch"),
    legend.key.width = unit(1.5, "inch"),
    #legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    legend.position = 'top',
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_0-90_qatm_rot_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)



### ARIDITY ###
varTab <- subset(mapDataOut, Variable == varNames[4] & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
varTab <- subset(varTab, WaterContent != 30 & WaterContent != 40)
varTab$WaterContent <- factor(varTab$WaterContent, levels = c(1,5,10,20,30,40,50))
sumData <- subset(dataOut, Variable == 'LandAridity' & (RotationAngle == 0 | RotationAngle == 90) & WaterContent > 0)
sumData <- subset(sumData, WaterContent != 30 & WaterContent != 40)
sumData$Value <- round_any(as.numeric(sumData$Value), 0.01)

varTab <- cbind(varTab, cut(varTab$Value, breaks = aridList))
colnames(varTab)[7] <- 'AridCategory'
varTab$AridCategory <- revalue(varTab$AridCategory, aridConv)
varTab$AridCategory <- replace_na(varTab$AridCategory, 'Ocean')



ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = Value), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_manual(values = aridPAL, na.value = 'black') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(rows=vars(factor(WaterContent, levels = c(1,5,10,20,30,40,50))), cols = vars(factor(RotationAngle, levels = c(0,90)))) +
guides(fill = guide_colorbar(title.position = "right", title = varLabs[4])) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.625,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    legend.key.height = unit(1.5, "inch"),
    legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_0-90_qatm_", format(Sys.time(), "%y%m%d"), ".png"), height = 9, width = 7, unit = 'in', dpi = 300)

### ARIDITY (rotate) ###
ggplot(varTab, aes(x = Lon, y = Lat)) +
geom_tile(aes(fill = AridCategory), width = 6, height = 5) +
geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
geom_vline(xintercept = c(-176.8, -90, 0, 90, 176.8), color = 'grey10', alpha = 0.5, size = 0.25) +
geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
scale_fill_manual(values = aridPAL, na.value = 'black') +
scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
xlab("Longitude") +
ylab("Latitude") +
facet_grid(cols=vars(factor(WaterContent, levels = c(1,5,10,20,30,40,50))), rows = vars(factor(RotationAngle, levels = c(0,90)))) +
guides(fill = guide_legend(title.position = "top", title = 'Aridity Category', label.position = 'top', nrow = 1)) +
coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    aspect.ratio = 0.8,
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background =element_rect(fill="white"),
    panel.border = element_rect(color = "black", fill=NA, size=2),
    legend.key=element_blank(),
    #legend.key.height = unit(1.5, "inch"),
    legend.key.width = unit(1.0, "inch"),
    #legend.title=element_text(angle = -90),
    legend.title.align = 0.5,
    legend.position = 'top',
    plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
    plot.tag.position = c(0.15, 0.02),
    axis.title.y.right = element_text(margin = margin(l = 83)),
    #legend.position = c(1.07, 0.52)
)
ggsave(paste0(subDir, '/', "TopoEns_FacetMap_0-90_aridity_rot_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)

