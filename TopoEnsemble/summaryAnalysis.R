library(tidyverse)
library(ncdf4)
library(mapproj) # global map projection library
library(reshape)
library(plyr)
library(viridis)
library(shadowtext)

aij <- list.files(getwd(), pattern = 'aij')
subDir <- paste0('Plots_', format(Sys.time(), "%y%m%d"))
dir.create(subDir)

diags <- c('tsurf_hemis', 'qatm_hemis', 'pcldt_hemis', 'prsurf_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')
latList <- seq(-90,90,4)
lonList <- seq(-177.5,177.5,5)

tempCut <- seq(-40,40, 80/15)
tempCut <- c(-1000, tempCut)
tempCut <- c(tempCut, 1000)
panoplyPAL <- c('#050fd9', '#2050ff', '#4297ff', '#6dc1ff', '#86daff', '#9defff', '#aff6ff', '#cfffff', '#ffffff', '#ffff66', '#ffec00', '#ffc400', '#ff9000', '#ff4900', '#ff0000', '#d50000', '#9f0000')
cutNames <-  c("(-1e+03,-40]","(-40,-34.7]","(-34.7,-29.3]","(-29.3,-24]","(-24,-18.7]","(-18.7,-13.3]","(-13.3,-8]","(-8,-2.67]","(-2.67,2.67]","(2.67,8]","(8,13.3]","(13.3,18.7]","(18.7,24]","(24,29.3]","(29.3,34.7]","(34.7,40]","(40,1e+03]")
names(panoplyPAL) <- cutNames

dummy <- data.frame(Lon = c(180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180,180), Lat = c(91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91), Value = c(-500,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,500), ValueCut = cutNames)

dataOut <- data.frame()
for(ifile in 1:length(aij)) {
    nc <- nc_open(aij[ifile])
    runName <- substr(aij[ifile],17,nchar(aij[ifile])-3)
    endYear <- substr(aij[ifile],9,12)
    startYear <- substr(aij[ifile],4,7)
    title <- strsplit(runName, '_')[[1]][2]
    if(title == 'aqua') {
        rot <- 'NA'
        wat <- 100
    } else {
        rot <- strsplit(title, 'x')[[1]][1]
        wat <- strsplit(title, 'x')[[1]][2]
    }
    

    for(idiag in 1:length(diags)) {
        tDat <- ncvar_get(nc, varid = diags[idiag])
        tOut <- cbind(runName, rot, wat, endYear, startYear, diags[idiag], tDat[3])
        colnames(tOut) <- c('RunName', 'RotationAngle', 'WaterContent', 'EndYear', 'StartYear', 'Diagnostic', 'Value')
        dataOut <- rbind(dataOut, tOut)

        if(idiag == 1) {
            ######## PLOT ########
            tPlot <- ncvar_get(nc, varid = strsplit(diags[idiag], '_')[[1]][1])
            row.names(tPlot) <- lonList
            colnames(tPlot) <- latList
            tPlot <- melt(tPlot)
            colnames(tPlot) <- c('Lon', 'Lat', 'Value')
            tPlot <- tPlot %>% mutate(ValueCut = cut(Value, breaks = tempCut))
            tPlot <- rbind(tPlot, dummy)


            ggplot(tPlot, aes(x = Lon, y = Lat, fill = ValueCut)) +
            #geom_tile(data = dummy, aes(x = Lon, y = Lat, color = ValueCut)) +
            geom_tile(width = 6, height = 5) +
            ggtitle(title) +
            #geom_shadowtext(data = sumData, aes(x = 165, y = 50, label = Value), color = 'black', bg.color = 'white', nudge_x = 10) + #, nudge_x = 20
            geom_vline(xintercept = c(-90, 0, 90), color = 'grey10', alpha = 0.5, size = 0.25) +
            geom_hline(yintercept = c(-60, -30, 0, 30, 60), color = 'grey10', alpha = 0.5, size = 0.25) +
            scale_fill_manual(values = panoplyPAL, , labels = c('-40','','','','','','','','','','','','','','','40')) + 
            scale_x_continuous(expand = expansion(), limits = c(-177, 177), breaks = NULL) +
            scale_y_continuous(expand = expansion(), limits = c(-88, 88), breaks = seq(-60, 60, 30)) +
            xlab("Longitude") +
            ylab("Latitude") +
            guides(fill = guide_colorsteps(title.position = "top", title = strsplit(diags[idiag], '_')[[1]][1], label.position = 'top', show.limits = TRUE)) + # 
            coord_map('moll') + ## <- converts map to mollweide projection (equal area, pseudocylindrical)
            #geom_shadowtext(aes(x = 165, y = 60, label = round(tDat[3], digits = 1)), size = 8, color = 'black', bg.color = 'white', nudge_x = 20) + #, nudge_x = 20
            theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),
                text = element_text(size = 16),
                axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 12),
                aspect.ratio = 0.6,
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
            ggsave(paste0(subDir, '/TopoEns_', title, "_tsurf_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 16, unit = 'in', dpi = 300)

        } else {
        }
    }
}

dataOut[,2] <- as.numeric(dataOut[,2])
dataOut[,3] <- as.numeric(dataOut[,3])
dataOut[,4] <- as.numeric(dataOut[,4])
dataOut[,5] <- as.numeric(dataOut[,5])
dataOut[,7] <- as.numeric(dataOut[,7])
saveRDS(dataOut, file = paste0('AllData_Sum_TopoEns_', format(Sys.time(), "%y%m%d"), ".png"))

linePlot <- subset(dataOut, RotationAngle == 0 | RotationAngle == 90 | WaterContent == 100)
tsurf <- subset(linePlot, Diagnostic == 'tsurf_hemis')
tsurf$RotationAngle <- factor(tsurf$RotationAngle)

ggplot(subset(tsurf, WaterContent < 100), aes(x = WaterContent, y = Value, Group = RotationAngle, color = RotationAngle)) +
geom_point() +
geom_line() +
geom_hline(data = subset(tsurf, WaterContent == 100), aes(yintercept = Value))

## compare tsurf and cloud cover ##