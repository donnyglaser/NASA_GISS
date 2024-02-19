

library(mapproj) # global map projection library
library(tidyverse)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(reshape)
library(viridis)

aijFiles <- list.files(getwd(), pattern = 'PlotData_TopoEns_aij')
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