


library(tidyverse)
library(reshape)
library(dplyr)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Topo Ensemble/Data/AllEquilibriumData/EQ Timeseries')

dirName <- paste0('Plots_', format(Sys.time(), "%y%m%d"))
dir.create(dirName)

runList <- list.files(getwd(), pattern = glob2rx('TopoEns_*.rds'))
climLab <- c('Temperature (ºC)', 'Vapor Column (kg/m^2)', 'Net Planetary Radiation (W/m^2)')
names(climLab) <- c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis')

hydLab <- c('Earth Water (kg/m^2)', 'Earth Ice (kg/m^2)', 'Precipitation (mm/day)', 'Potential Evaporation (mm/day)', 'Ocean/Lake Ice Thickness (m)', 'Snow Depth (mm H2O)')
names(hydLab) <- c('gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')

comLab <- c('Temperature (ºC)', 'Net Planetary Radiation (W/m^2)', 'Precipitation (mm/day)', 'Potential Evaporation (mm/day)')
names(comLab) <- c('tsurf_hemis', 'net_rad_planet_hemis', 'prec_hemis', 'pot_evap_hemis')

for(irun in 1:length(runList)) {
    runName <- runList[irun]
    tempDat <- readRDS(runName)
    tempDat$Diagnostic <- factor(tempDat$Diagnostic, levels = c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis'))
    tempEQ <- subset(tempDat, Year == 'eq50')
    tempEQ$Value <- as.numeric(tempEQ$Value)
    tempDat$Year <- as.numeric(tempDat$Year)
    tempDat$Value <- as.numeric(tempDat$Value)
    
    runName <- substr(runName,9,nchar(runName)-3)
    runName <- strsplit(runName, split='_')
    runName <- runName[[1]][1]


    ######## CLIMATE DIAGNOSTICS ########
    clim <- subset(tempDat, Diagnostic %in% c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis'))
    clim$Diagnostic <- factor(clim$Diagnostic, levels = c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis'))
    climeq <- subset(tempEQ, Diagnostic %in% c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis'))

    ggplot(data = clim, aes(x = Year, y = Value)) +
    geom_hline(data = climeq, aes(yintercept = Value), size = 1, color = 'red') +
    geom_point(color = 'black', size = 1) +
    geom_line(color = 'black') +
    facet_wrap(~Diagnostic, ncol = 1, strip.position = 'top', labeller = labeller(Diagnostic = climLab), scales = 'free') + # 
    scale_x_continuous(expand = expansion(mult = 0.05)) + # , limits = , breaks = 
    scale_y_continuous(n.breaks = 5, expand = expansion(mult=c(0.2, 0.2))) + # expand = expansion(0, mult = 0.3), , breaks = waiver(),  expand = expansion(), limits = c(0, maxY), 
    xlab("Model Years") + 
    ylab(NULL) +
    ggtitle(paste0("Climate Diagnostics: ", runName)) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
        text = element_text(size = 18), 
        axis.text.x = element_text(size = 16),
        aspect.ratio = 0.5, 
        axis.line = element_line(color = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
        legend.key=element_blank(), 
        legend.key.height = unit(1.3, 'cm'), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.tag.position = c(0.15, 0.02), 
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        #legend.position = c(1.07, 0.52), 
        panel.spacing = unit(5, "mm")
    )
    ggsave(paste0(dirName, '/', tempDat[1,1], '_ClimateEq_Global_', format(Sys.time(), "%y%m%d"), ".png"), height = 10, width = 5, unit = 'in', dpi = 300)


    
    ######## HYDROLOGY DIAGNOSTICS ########
    hyd <- subset(tempDat, Diagnostic %in% c('gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis'))
    hyd$Diagnostic <- factor(hyd$Diagnostic, levels = c('gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis'))
    hydEQ <- subset(tempEQ, Diagnostic %in% c('gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis'))

    ggplot(data = hyd, aes(x = Year, y = Value)) +
    geom_hline(data = hydEQ, aes(yintercept = Value), size = 1, color = 'red') +
    geom_point(color = 'black', size = 1) +
    geom_line(color = 'black') +
    facet_wrap(~Diagnostic, nrow = 2, strip.position = 'top', labeller = labeller(Diagnostic = hydLab), scales = 'free', dir='v') + # 
    scale_x_continuous(expand = expansion(mult = 0.05)) + # , limits = , breaks = 
    scale_y_continuous(limits = c(0, NA), n.breaks = 5, expand = expansion(mult=c(0, 0.2))) + # expand = expansion(0, mult = 0.3), , breaks = waiver(),  expand = expansion(), limits = c(0, maxY), 
    #ylim(0,NA) +
    #expand_limits(y = 0) +
    xlab("Model Years") + ## ~687 Earth days
    ylab(NULL) +
    ggtitle(paste0("Hydrology Diagnostics: ", runName)) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
        text = element_text(size = 18), 
        axis.text.x = element_text(size = 16),
        aspect.ratio = 0.5, 
        axis.line = element_line(color = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
        legend.key=element_blank(), 
        legend.key.height = unit(1.3, 'cm'), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.tag.position = c(0.15, 0.02), 
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        #legend.position = c(1.07, 0.52), 
        panel.spacing = unit(5, "mm")
    )
    ggsave(paste0(dirName, '/', tempDat[1,1], '_HydrologyEq_Global_', format(Sys.time(), "%y%m%d"), ".png"), height = 6, width = 12, unit = 'in', dpi = 300)

    ######## COMBINED DIAGNOSTICS ########
    com <- subset(tempDat, Diagnostic %in% c('tsurf_hemis', 'net_rad_planet_hemis', 'prec_hemis', 'pot_evap_hemis'))
    com$Diagnostic <- factor(com$Diagnostic, levels = c('tsurf_hemis', 'net_rad_planet_hemis', 'prec_hemis', 'pot_evap_hemis'))
    comeq <- subset(tempEQ, Diagnostic %in% c('tsurf_hemis', 'net_rad_planet_hemis', 'prec_hemis', 'pot_evap_hemis'))

    ggplot(data = com, aes(x = Year, y = Value)) +
    geom_hline(data = comeq, aes(yintercept = Value), size = 1, color = 'red') +
    geom_point(color = 'black', size = 1) +
    geom_line(color = 'black') +
    facet_wrap(~Diagnostic, ncol = 2, strip.position = 'top', labeller = labeller(Diagnostic = comLab), scales = 'free') + # 
    scale_x_continuous(expand = expansion(mult = 0.05)) + # , limits = , breaks = 
    scale_y_continuous(n.breaks = 5, expand = expansion(mult=c(0.2, 0.2))) + # expand = expansion(0, mult = 0.3), , breaks = waiver(),  expand = expansion(), limits = c(0, maxY), 
    xlab("Model Years") + 
    ylab(NULL) +
    ggtitle(paste0("Equilibrium Assessment: ", runName)) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
        text = element_text(size = 18), 
        axis.text.x = element_text(size = 16),
        aspect.ratio = 0.5, 
        axis.line = element_line(color = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
        legend.key=element_blank(), 
        legend.key.height = unit(1.3, 'cm'), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.tag.position = c(0.15, 0.02), 
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        #legend.position = c(1.07, 0.52), 
        panel.spacing = unit(5, "mm")
    )
    ggsave(paste0(dirName, '/', tempDat[1,1], '_AllEq_Global_', format(Sys.time(), "%y%m%d"), ".png"), height = 6.5, width = 9, unit = 'in', dpi = 300)


    ## INDIVIDUAL ##

    for(idiag in 1:length(names(comLab))) {
        ggplot(data = subset(com, Diagnostic == names(comLab)[idiag]), aes(x = Year, y = Value)) +
        geom_hline(data = subset(comeq, Diagnostic == names(comLab)[idiag]), aes(yintercept = Value), size = 1, color = 'red') +
        geom_point(color = 'black', size = 1) +
        geom_line(color = 'black') +
        # facet_wrap(~Diagnostic, ncol = 2, strip.position = 'top', labeller = labeller(Diagnostic = comLab), scales = 'free') + # 
        scale_x_continuous(expand = expansion(mult = 0.05)) + # , limits = , breaks = 
        scale_y_continuous(n.breaks = 5, expand = expansion(mult=c(0.2, 0.2))) + # expand = expansion(0, mult = 0.3), , breaks = waiver(),  expand = expansion(), limits = c(0, maxY), 
        xlab("Model Years") + 
        ylab(comLab[idiag]) +
        ggtitle(NULL) +
        theme(
            plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            text = element_text(size = 18), 
            axis.text.x = element_text(size = 16),
            aspect.ratio = 0.5, 
            axis.line = element_line(color = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            legend.key=element_blank(), 
            legend.key.height = unit(1.3, 'cm'), 
            plot.margin = margin(0.25, 0.7, 0.25, 0.25, "cm"), 
            plot.tag.position = c(0.15, 0.02), 
            axis.title.y.right = element_text(margin = margin(l = 83)),
            strip.background =element_rect(fill="white"),
            #legend.position = c(1.07, 0.52), 
            panel.spacing = unit(5, "mm")
        )
        ggsave(paste0(dirName, '/', tempDat[1,1], '_EqGlobal_', names(comLab)[idiag], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 9, unit = 'in', dpi = 300)
    }
}