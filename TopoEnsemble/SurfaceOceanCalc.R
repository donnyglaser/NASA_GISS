## calculate surface ocean coverage ##

library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(reshape)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars')

ncdf <- nc_open('mars_topo_trans0_paleo_C.nc')

ncdfTopo <- ncvar_get(ncdf, varid= 'topography')

colnames(ncdfTopo) <- ncdf[["dim"]][["longitude"]][["vals"]]
row.names(ncdfTopo) <- seq(-90, 90, 4)
topo_m <- melt.matrix(ncdfTopo)
colnames(topo_m) <- c('lat', 'lon', 'topo')

radMars <- 3390 * 1000 # Mars radius in m

topo_m <- topo_m %>% mutate(area = ((radMars^2) * (sin(abs(lat * 0.0175)) - sin(abs(lat * 0.0175) - 0.07)) * 0.0875))

surfMars <- sum(topo_m$area)

topo_m <- topo_m %>% mutate(area_pct = (area / surfMars) * 100)

topoList <- seq(0, 10000, 100)

oceanList <- data.frame()
for(i in 1:length(topoList)) {
    temp <- subset(topo_m, topo < topoList[i])

    tempO <- cbind(topoList[i], sum(temp$area_pct))
    oceanList <- rbind(oceanList, tempO)
}
colnames(oceanList) <- c('TopoHeight_m', 'SurfaceArea_pct')
###### make plot of surface ocean coverage #######

ggplot(oceanList, aes(x = TopoHeight_m, y = SurfaceArea_pct)) +
geom_point(size = 2) +
xlab('Sea Level Height (m)') +
ylab('Ocean Surface Area (%)') +
scale_x_continuous(expand = expansion(), limits = c(-9000, 5000), breaks = seq(-9000, 5000, 2000)) +
scale_y_continuous(expand = expansion(), limits = c(0, 100), breaks = seq(0,100, 20)) +
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), text = element_text(size = 18), axis.text.x = element_text(size = 16), aspect.ratio = 0.625, axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(color = "black", fill=NA, size=2), legend.key=element_blank(), legend.key.height = unit(0.83, "inch"), plot.margin = margin(0.25, 0.75, 0.25, 0.25, "cm"), plot.tag.position = c(0.15, 0.02), axis.title.y.right = element_text(margin = margin(l = 83)), panel.spacing = unit(5, "mm"), strip.background = element_rect(fill="white"))

ggsave(paste0('TopovsOceanSurface_', format(Sys.time(), "%y%m%d"), ".png"), dpi = 300)

##### plot w/ annotations at specific ocean surface areas #####

oceanList <- oceanList %>% mutate(SurfaceArea_rnd = round_any(SurfaceArea_pct, 0.5, f = round))
pctSub <- subset(oceanList, SurfaceArea_rnd == 10 | SurfaceArea_rnd == 20 | SurfaceArea_rnd == 30 | SurfaceArea_rnd == 40.5 | SurfaceArea_rnd == 50)

ggplot(oceanList, aes(x = TopoHeight_m, y = SurfaceArea_pct)) +
geom_point(size = 2) +
geom_point(data = pctSub, aes(x = TopoHeight_m, y = SurfaceArea_pct), color = 'red', size = 2) +
xlab('Sea Level Height (m)') +
ylab('Ocean Surface Area (%)') +
geom_segment(data = pctSub, aes(x = pctSub$TopoHeight_m, y = pctSub$SurfaceArea_pct, xend = pctSub$TopoHeight_m, yend = 0), color = 'red') +
scale_x_continuous(expand = expansion(), limits = c(0,10000), breaks = seq(0, 10000, 2000)) +
scale_y_continuous(expand = expansion(), limits = c(0, 100), breaks = seq(0,100, 20)) +
theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), text = element_text(size = 18), axis.text.x = element_text(size = 16), aspect.ratio = 0.625, axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(color = "black", fill=NA, size=2), legend.key=element_blank(), legend.key.height = unit(0.83, "inch"), plot.margin = margin(0.25, 0.75, 0.25, 0.25, "cm"), plot.tag.position = c(0.15, 0.02), axis.title.y.right = element_text(margin = margin(l = 83)), panel.spacing = unit(5, "mm"), strip.background = element_rect(fill="white"))

ggsave(paste0('TopovsOceanSurface_wLabels_', format(Sys.time(), "%y%m%d"), ".png"), dpi = 300)


pctSub <- cbind(c(10, 20, 30, 40, 50), pctSub)
colnames(pctSub)[1] <- 'SurfaceArea'
pctSub <- pctSub[,-4]


topoOcean_m <- data.frame()
for(i in 1:5) {
    temp <- topo_m
    x <- pctSub[i,2]
    temp <- temp %>% mutate(OceanHeight = x - topo)
    temp <- cbind(pctSub[i,1], temp)
    colnames(temp)[1] <- 'OceanSurfaceArea'
    topoOcean_m <- rbind(topoOcean_m, temp)
}

gelTab <- data.frame()
for(i in 1:5) {
    temp <- subset(topoOcean_m, OceanSurfaceArea == pctSub[i,1])
    temp <- subset(temp, OceanHeight > 0)
    temp <- temp %>% mutate(OceanVol = area * OceanHeight)
    vol <- sum(temp$OceanVol) / sum(topoOcean_m$area)
    x <- cbind(pctSub[i,1], vol)
    gelTab <- rbind(gelTab, x)
}

colnames(gelTab)[2] <- 'GEL_m'

pctSub <- cbind(pctSub, gelTab[,2])
colnames(pctSub)[4] <- 'GEL_m'

save(pctSub, file = paste0('OceanSurfaceAreaTable_wGEL_', format(Sys.time(), "%y%m%d"), '.Rda'))
