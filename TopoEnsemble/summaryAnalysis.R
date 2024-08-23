## script to summarize the final equilibrium state (end of run sumfiles) of all the runs ##
## run in the SumData folder ##
## outputs a dataframe with all the global means for each run, and plots(??) ##

library(tidyverse)
library(ncdf4)
library(mapproj) # global map projection library
library(reshape)
library(plyr)
library(viridis)
library(shadowtext)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Topo Ensemble/Data/SumData')

#eqRuns <- c('_0x1_', '_0x5_', '_0x10_', '_90x5_', '_90x10_', '_90x20_', '_90x30_') ## update this to identify the runs that are in equilibrium

aij <- list.files(getwd(), pattern = '.aij')
aijk <- subset(aij, grepl('aijk', aij) == TRUE)
aijl <- subset(aij, grepl('aijl', aij) == TRUE)
aij <- subset(aij, grepl('aijk', aij) == FALSE)
aij <- subset(aij, grepl('aijl', aij) == FALSE)
#aij <- subset(aij, grepl('E1oM20_Test2', aij) == FALSE)

subDir <- paste0('Plots_', format(Sys.time(), "%y%m%d"))
dir.create(subDir)

diags <- c('tsurf_hemis', 'net_rad_planet_hemis', 'qatm_hemis', 'pcldt_hemis', 'pcldh_hemis', 'pcldm_hemis', 'pcldl_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis', 'plan_alb_hemis', 'grnd_alb_hemis', 'snowicefr_hemis')
latList <- seq(-90,90,4)
lonList <- seq(-177.5,177.5,5)
diagLabs <- c('Temperature (ºC)', 'Net Planetary Radiation (W/m^2)', 'Vapor Column (kg/m^2)', 'Total Cloud Cover (%)', 'High Cloud Cover (%)', 'Mid Cloud Cover (%)', 'Low Cloud Cover (%)', 'Earth Water (kg/m^2)', 'Earth Ice (kg/m^2)', 'Precipitation (mm/day)', 'Potential Evaporation (mm/day)', 'Ocean/Lake Ice Thickness (m)', 'Snow Depth (mm H2O)', 'Planetary Albedo (%)', 'Ground Albedo (%)', 'Snow & Ice Coverage (%)')
names(diagLabs) <- diags

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
    } else if(title == 'Test2') {
        rot <- 'NA'
        wat <- 71
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
            ggsave(paste0(subDir, '/TopoEns_', title, "_map_tsurf_", format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 16, unit = 'in', dpi = 300)

        } else {
        }
    }

    # ## extract aridity ##
    # ## i dont think this works (240726) ##
    # ## constants ##
    # r_surf <- 0
    # zm <- 10 # height of wind measurements, m 
    # zh <- 10 # height of wind measurements, humidity m
    # d <- 0 # Zero plane displacement
    # veg_height <- 0 # Bulk aerodynamic resistance
    # z_om <- 0.005 # Roughness length governing momentum transfer
    # z_oh <- 0.005 # Roughness length governing transfer of heat and vapour
    # kvonKarm <- 0.41 # von Karmans constant, 0.41 [-]
    # Rgas <- 0.287058
    # MJday_to_watts = 11.5740741 # megajoule per day = 11.5740741 watt

    # ## from diagnostics ##
    # Ta <- ncvar_get(nc, varid = 'tsurf')
    # Tmax <- ncvar_get(nc, varid = 'TMAXC')
    # Tmin <- ncvar_get(nc, varid = 'TMINC')
    # Rnet <- ncvar_get(nc, varid = 'srtrnf_grnd')
    # gflux <- ncvar_get(nc, varid = 'netht_land')
    # Pea <- ncvar_get(nc, varid = 'PVS')
    # Psurf <- ncvar_get(nc, varid = 'prsurf')
    # Wind <- ncvar_get(nc, varid = 'wsurf')

    # # Calculate the latent heat of vaporization (MJ kg-1) (FAO value 2.45)
    # lambda_lv <- 2.361E-3 * Ta 
    # lambda_lv <- 2.501 - lambda_lv

    # # Convert Pressure from Pa to kPa
    # Psurf <- Psurf / 1000

    # # Convert actual vapor pressure from Pa to kPa
    # Pea <- Pea / 1000

    # # Calculate saturation vapor pressure (kPa)
    # es_min <- 0.6108 * exp((17.27 * Tmin) / (Tmin + 237.3))
    # es_max <- 0.6108 * exp((17.27 * Tmax) / (Tmax + 237.3))
    # Pes <- (es_min + es_max) / 2

    # # Slope of the vapor pressure curve (kPa C-1)
    # delta_vpc <- (4098 * (0.6108 * exp((17.27 * Ta) / (Ta+237.3)))) / ((Ta+237.3)^2)

    # # Specific heat of the air (MJ kg-1 ºC-1)  => cp
    # specific_heat_air = 1.013E-3

    # # Mean air density at constant pressure (kg m-3) => rho_a
    # # using ideal gas law
    # density_air <- Psurf / (1.01 * (Ta + 273) * Rgas)

    # # Ratio molecular weight of water vapour/dry air = 0.622
    # epsilon = 0.622

    # # Psychometric` parameter (kPa C-1)
    # psych_parameter <- (specific_heat_air * Psurf) / (epsilon * lambda_lv)

    # # Net radiation: convert W/m2 to MJ/m2/d
    # Rnet <- Rnet / MJday_to_watts

    # # Ground heat flux: convert W/m2 to MJ/m2/d
    # gflux <- gflux / MJday_to_watts
    # gflux <- ifelse(is.na(gflux[,]), 0, gflux[,])

    # # Calculate VPD
    # VPD <- Pes - Pea

    # # Calculate Relative Humidity
    # RH <- Pea / Pes

    # r_air <- (log10((zm - d) / (z_om)) * log10((zh - d) / (z_oh))) / (kvonKarm^(2 * Wind))
    # PETenergy <- (delta_vpc * (Rnet - gflux) / lambda_lv) / ((delta_vpc + psych_parameter * (1 + r_surf / r_air)))
    # PETvpd <- (86400 * density_air * specific_heat_air * ((Pes - Pea) / (lambda_lv * r_air))) / ((delta_vpc + psych_parameter * (1 + r_surf / r_air)))

    # PET <- PETenergy + PETvpd

    # ## i dont think this works (240726) ##

    nc_close(nc)
}

dataOut[,2] <- as.numeric(dataOut[,2])
dataOut[,3] <- as.numeric(dataOut[,3])
dataOut[,4] <- as.numeric(dataOut[,4])
dataOut[,5] <- as.numeric(dataOut[,5])
dataOut[,7] <- as.numeric(dataOut[,7])
saveRDS(dataOut, file = paste0('AllData_Sum_TopoEns_', format(Sys.time(), "%y%m%d"), ".rds"))

### SUMMARY PLOTS ###

#earth <- nc_open('ANN2900-3399.aijE1oM20_Test2.nc')
## line plots ##
for(idiag in 1:length(diags)) {
    tempPlot <- subset(dataOut, Diagnostic == diags[idiag])
    tempPlot$RotationAngle <- factor(tempPlot$RotationAngle)

    #tempEarth <- ncvar_get(earth, varid = diags[idiag])

    p <- ggplot(subset(tempPlot, WaterContent <= 50), aes(x = WaterContent, y = Value, group = RotationAngle, color = RotationAngle), guide = NULL)
    p <- p + geom_point(size = 4, guide = NULL) # , shape = 21, stroke = 3
    p <- p + geom_line(size = 1, guide = NULL) # , linetype = 'dashed'
    if(idiag < 3){
        p <- p + geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'grey30')
    } else {
    }
    if(idiag == 2){
        p <- p + scale_y_continuous(limits = c(-0.5, 0.5), n.breaks = 5)
        p <- p + geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'grey30')
        p <- p + geom_hline(aes(yintercept = -0.3), linetype = 'solid', color = 'darkred')
        p <- p + geom_hline(aes(yintercept = 0.3), linetype = 'solid', color = 'darkred')
    }
    p <- p + scale_color_manual(values = c('black', 'grey65'))
    p <- p + xlab("Water Surface Area (%)")
    p <- p + ylab(diagLabs[idiag])
    p <- p + guides(line = 'none', color = 'none') # color = element_blank()
    #p <- p + guides(color = guide_legend('Rotation\nAngle'))
    #p <- p + geom_point(data = subset(tempPlot, WaterContent == 0), aes(x = WaterContent, y = Value, color = RotationAngle), size = 3)
    p <- p + geom_point(data = subset(tempPlot, WaterContent == 100), aes(x = WaterContent, y = Value), color = 'black', size = 3, shape = 0, stroke = 2)
    p <- p + geom_point(data = subset(tempPlot, WaterContent == 71), aes(x = WaterContent, y = Value), fill = 'white', color = 'black', size = 3, shape = 24, stroke = 2)
    p <- p + theme(
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
        plot.margin = margin(0.75, 0.25, 0.25, 0.25, "cm"), ## margin(top, right, bottom, left, unit)
        plot.tag.position = c(0.15, 0.02), 
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        #legend.position = c(1.07, 0.52), 
        panel.spacing = unit(5, "mm")
    )

    ggsave(paste0(subDir, '/TopoEns_line_', diags[idiag], '_', format(Sys.time(), "%y%m%d"), ".png"), plot = p, height = 7, width = 11, unit = 'in', dpi = 300)
}
#nc_close(earth)


tempPlot <- subset(dataOut, Diagnostic == 'pcldh_hemis' | Diagnostic == 'pcldm_hemis' | Diagnostic == 'pcldl_hemis')
tempPlot$RotationAngle <- factor(tempPlot$RotationAngle)
tempPlot$Diagnostic <- factor(tempPlot$Diagnostic, levels = c('pcldl_hemis', 'pcldm_hemis', 'pcldh_hemis'))

p <- ggplot(subset(tempPlot, WaterContent <= 50), aes(x = WaterContent, y = Value, group = RotationAngle, color = RotationAngle), guide = NULL)
p <- p + geom_point(size = 4, guide = NULL) # , shape = 21, stroke = 3
p <- p + geom_line(size = 1, guide = NULL) # , linetype = 'dashed'
p <- p + facet_grid(. ~ Diagnostic, labeller = as_labeller(c('pcldl_hemis'='Low Clouds', 'pcldm_hemis'='Mid Clouds', 'pcldh_hemis'='High Clouds')))
p <- p + scale_color_manual(values = c('black', 'grey65'))
p <- p + xlab("Water Surface Area (%)")
p <- p + ylab('Cloud Coverage (%)')
p <- p + guides(line = 'none', color = 'none') # color = element_blank()
p <- p + scale_y_continuous(expand = expansion(), limits = c(-2,42), breaks = seq(0,40,10))

#p <- p + guides(color = guide_legend('Rotation\nAngle'))
#p <- p + geom_point(data = subset(tempPlot, WaterContent == 0), aes(x = WaterContent, y = Value, color = RotationAngle), size = 3)
p <- p + geom_point(data = subset(tempPlot, WaterContent == 100), aes(x = WaterContent, y = Value), color = 'black', size = 3, shape = 0, stroke = 2)
p <- p + geom_point(data = subset(tempPlot, WaterContent == 71), aes(x = WaterContent, y = Value), fill = 'white', color = 'black', size = 3, shape = 24, stroke = 2)
p <- p + theme(
    plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
    text = element_text(size = 18), 
    axis.text.x = element_text(size = 16),
    aspect.ratio = 1.2, 
    axis.line = element_line(color = "black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
    legend.key=element_blank(), 
    legend.key.height = unit(1.3, 'cm'), 
    plot.margin = margin(0.75, 0.25, 0.25, 0.25, "cm"), ## margin(top, right, bottom, left, unit)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm")
)

ggsave(paste0(subDir, '/TopoEns_CloudsLine_Facet_', format(Sys.time(), "%y%m%d"), ".png"), plot = p, height = 6, width = 13, unit = 'in', dpi = 300)








## SCATTER PLOTS ##
scatterPlot <- dataOut[,c(2:3,6:7)]
scatterPlot <- cast(scatterPlot, RotationAngle * WaterContent ~ Diagnostic)


## grnd alb vs temp (WC color) ##
p <- ggplot(scatterPlot, aes(x = grnd_alb_hemis, y = tsurf_hemis, fill = WaterContent), guide = NULL)
p <- p + geom_point(shape = 21, size = 4, color = 'black', stroke = 1.5) # , shape = 21, stroke = 3
p <- p + scale_fill_gradient(low = 'white', high = 'steelblue3', limits = c(0,50), guide = 'colorbar', breaks = seq(0,50, 10), labels = c(seq(0, 40, 10), '>50'), oob = scales::squish)
p <- p + xlab("Ground Albedo (%)")
p <- p + ylab('Surface Temperature (ºC)')
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Water Surface Area (%)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
p <- p + scale_x_continuous(expand = expansion(), limits = c(0,30), breaks = seq(0,30,5))
p <- p + scale_y_continuous(expand = expansion(), limits = c(-5,25), breaks = seq(-5,25,10))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_gndalbvtsurfvWC_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)


## grnd alb vs temp (WC color SQUISH) ##
p <- ggplot(scatterPlot, aes(x = grnd_alb_hemis, y = tsurf_hemis, fill = WaterContent), guide = NULL)
p <- p + geom_point(shape = 21, size = 4, color = 'black', stroke = 1.5) # , shape = 21, stroke = 3
p <- p + scale_fill_gradient(low = 'white', high = 'steelblue3', limits = c(0,50), guide = 'colorbar', breaks = seq(0,50, 10), labels = c(seq(0, 40, 10), '>50'), oob = scales::squish)
p <- p + xlab("Ground Albedo (%)")
p <- p + ylab('Surface Temperature (ºC)')
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Water Surface Area (%)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
p <- p + scale_x_continuous(expand = expansion(), limits = c(0,32), breaks = seq(0,30,5))
p <- p + scale_y_continuous(expand = expansion(), limits = c(-5,25), breaks = seq(-5,25,10))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_gndalbvtsurfvWCSquish_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)


# ## grnd alb vs temp (WC color SQUISH; rotation symbols) ##
# ## not sure if this is anything, maybe sorta helps ##
# rotFactor <- scatterPlot
# rotFactor$RotationAngle[is.na(rotFactor$RotationAngle)] <- '*'
# rotFactor$RotationAngle <- factor(rotFactor$RotationAngle, levels = c(0, 90, '*'))

# p <- ggplot(subset(rotFactor, WaterContent > 1), aes(x = grnd_alb_hemis, y = tsurf_hemis, fill = WaterContent, shape  = RotationAngle), guide = NULL)
# p <- p + geom_line(aes(group = WaterContent))
# p <- p + geom_point(size = 4, color = 'black', stroke = 1.5) # , shape = 21, stroke = 3
# #p <- p + geom_text(aes(label = WaterContent), hjust = -0.5, vjust = -1)
# p <- p + geom_point(data = subset(rotFactor, WaterContent < 5), shape = 10, size = 4, color = 'black', stroke = 1.5)
# p <- p + scale_shape_manual(values = c(21:24))
# p <- p + scale_fill_gradient(low = 'white', high = 'steelblue3', limits = c(0,50), guide = 'colorbar', breaks = seq(0,50, 10), labels = c(seq(0, 40, 10), '>50'), oob = scales::squish)
# p <- p + xlab("Ground Albedo (%)")
# p <- p + ylab('Surface Temperature (ºC)')
# p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Water Surface Area (%)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
# p <- p + scale_x_continuous(expand = expansion(), limits = c(0,32), breaks = seq(0,30,5))
# p <- p + scale_y_continuous(expand = expansion(), limits = c(-5,25), breaks = seq(-5,25,10))
# p <- p + theme(
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
#     legend.key.height = unit(0.75, 'cm'),
#     legend.key.width = unit(4, 'cm'),
#     legend.title.align=0.5,
#     plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
#     plot.tag.position = c(0.15, 0.02), 
#     axis.title.y.right = element_text(margin = margin(l = 83)),
#     strip.background =element_rect(fill="white"),
#     #legend.position = c(1.07, 0.52), 
#     panel.spacing = unit(5, "mm"),
#     legend.position = 'bottom'
# )

# ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_gndalbvtsurfvWCSquish_connect_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)



## grnd alb vs WC (tsurf color) ##
p <- ggplot(scatterPlot, aes(x = WaterContent, y = grnd_alb_hemis, fill = tsurf_hemis), guide = NULL)
p <- p + geom_point(shape = 21, size = 4, color = 'black', stroke = 1.5) # , shape = 21, stroke = 3
p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(-25,25), guide = 'colorbar', breaks = seq(-20,20, 10), oob = scales::squish)
p <- p + xlab("Water Surface Area (%)")
p <- p + ylab('Ground Albedo (%)')
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Temperature (ºC)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
p <- p + scale_x_continuous(expand = expansion(), limits = c(-5,105), breaks = seq(0,100,25))
p <- p + scale_y_continuous(expand = expansion(), limits = c(0,32), breaks = seq(0,30,5))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_gndalbvWCvtsurf_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)



## planet alb vs temp (WC color SQUISH) ##
p <- ggplot(scatterPlot, aes(x = plan_alb_hemis, y = tsurf_hemis, fill = WaterContent), guide = NULL)
p <- p + geom_point(shape = 21, size = 4, color = 'black', stroke = 1.5) # , shape = 21, stroke = 3
p <- p + scale_fill_gradient(low = 'white', high = 'steelblue3', limits = c(0,50), guide = 'colorbar', breaks = seq(0,50, 10), labels = c(seq(0, 40, 10), '>50'), oob = scales::squish)
p <- p + xlab("Planetary Albedo (%)")
p <- p + ylab('Surface Temperature (ºC)')
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Water Surface Area (%)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
p <- p + scale_x_continuous(expand = expansion(), limits = c(20,40), breaks = seq(20,40,5))
p <- p + scale_y_continuous(expand = expansion(), limits = c(-5,25), breaks = seq(-5,25,10))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_planalbvtsurfvWCSquish_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)




## grnd alb vs plan alb ##
p <- ggplot(scatterPlot, aes(x = plan_alb_hemis, y = grnd_alb_hemis, fill = tsurf_hemis))
p <- p + geom_point(aes(fill = tsurf_hemis), size = 4, shape = 21)
p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(-25,25), guide = 'colorbar', breaks = seq(-25,25, 10), oob = scales::squish)
p <- p + xlab('Planetary Albedo (%)')
p <- p + ylab("Ground Albedo (%)")
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Surface Temperature (ºC)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
p <- p + scale_x_continuous(expand = expansion(), limits = c(20,40), breaks = seq(20,40,5))
p <- p + scale_y_continuous(expand = expansion(), limits = c(0,30), breaks = seq(0,30,10))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_gndalbvplanalb_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)


## plan alb vs grnd alb (swap axes) ##
p <- ggplot(scatterPlot, aes(y = plan_alb_hemis, x = grnd_alb_hemis, fill = tsurf_hemis))
p <- p + geom_point(aes(fill = tsurf_hemis), size = 4, shape = 21)
p <- p + scale_fill_gradientn(colors = panoplyPAL, limits = c(-25,25), guide = 'colorbar', breaks = seq(-25,25, 10), oob = scales::squish)
p <- p + ylab('Planetary Albedo (%)')
p <- p + xlab("Ground Albedo (%)")
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Surface Temperature (ºC)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
p <- p + scale_y_continuous(expand = expansion(), limits = c(20,40), breaks = seq(20,40,5))
p <- p + scale_x_continuous(expand = expansion(), limits = c(0,30), breaks = seq(0,30,10))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_planalbvgrndalb_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)


## tsurf v cloud cover ##
p <- ggplot(scatterPlot, aes(y = pcldt_hemis, x = tsurf_hemis, fill = WaterContent))
p <- p + geom_point(size = 4, shape = 21)
p <- p + scale_fill_gradient(limits = c(0,50), guide = 'colorbar', breaks = seq(0,50, 10), oob = scales::squish)
p <- p + ylab('Cloud Cover (%)')
p <- p + xlab("Surface Temperature (ºC)")
p <- p + guides(line = 'none', color = 'none', fill = guide_colorbar(title.position = 'bottom', title = 'Surface Temperature (ºC)', frame.colour = "black", frame.linewidth = 0.75, ticks.colour = "black", ticks.linewidth = 0.75,)) # color = element_blank()
#p <- p + scale_y_continuous(expand = expansion(), limits = c(20,40), breaks = seq(20,40,5))
#p <- p + scale_x_continuous(expand = expansion(), limits = c(0,30), breaks = seq(0,30,10))
p <- p + theme(
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
    legend.key.height = unit(0.75, 'cm'),
    legend.key.width = unit(4, 'cm'),
    legend.title.align=0.5,
    plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"), #c(top,right,bottom,left)
    plot.tag.position = c(0.15, 0.02), 
    axis.title.y.right = element_text(margin = margin(l = 83)),
    strip.background =element_rect(fill="white"),
    #legend.position = c(1.07, 0.52), 
    panel.spacing = unit(5, "mm"),
    legend.position = 'bottom'
)

ggsave(plot = p, file = paste0(subDir, '/TopoEns_scatter_tsurfvcloud_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 9, unit = 'in', dpi = 300)
