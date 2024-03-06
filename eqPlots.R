###  ###

library(tidyverse)
library(reshape)
library(dplyr)

datList <- list.files(getwd(), pattern = glob2rx('*_Equilibrium_*.rds'))

latLabs <- c('90ºN', '78ºN', '62ºN', '46ºN', '30ºN', '14ºN', '2ºN', '14ºS', '30ºS', '46ºS', '62ºS', '78ºS', '90ºS')
names(latLabs) <- c(90, 78, 62, 46, 30, 14, 2, -14, -30, -46, -62, -78, -90)
latLo <- latLabs[5:9]
latMid <- latLabs[c(3:4, 10:11)]
latHi <- latLabs[c(1:2, 12:13)]

## comment certain of these out if you are omitting certain diagnostics ##
allVar <- data.frame()
allVar <- rbind(allVar, cbind('aij', c("bs_snowdp", "gice", "gwtr", "incsw_toa", "landicefr", "prsurf", "qatm", "snowdp", "srf_wind_dir", "srtrnf_grnd", "tgrnd", "tsurf", "wsurf", "zsnow")))
allVar <- rbind(allVar, cbind('aijl', c('q', 'rh', 'temp')))
allVar <- rbind(allVar, cbind('aijk',c('tb')))
allVar <- rbind(allVar, cbind('oij', c('oij_hbl', 'oij_mld')))
allVar <- rbind(allVar, cbind('oijl', c('heat', 'pot_temp', 'salt')))

## comment certain of these out if you are omitting certain diagnostics ##
nameVar <- c()
nameVar <- c(nameVar, c(expression('Snow Depth (mm H'[2] *'O)'), expression("Total Ice (" * kg ~ m^-2 * ")"), expression('Total Water (' * kg ~ m^-2 * ')'), expression('Incident Solar Radiation (' * W ~ m^-2 * ')'), 'Land Ice Fraction (%)', 'Surface Pressure (mb)', expression('Atmospheric Water Vapor Column (' * kg ~ m^-2 * ')'), 'Snow Depth (mm H2O)', 'Surface Wind Direction (ºCW North)', expression('Net Radiation at Ground (' * W ~ m^-2 * ')'), 'Ground Temperature (ºC)', 'Surface Air Temperature (ºC)', expression('Surface Wind Speed (' * m ~ s^-1 * ')'), 'Snow Thickness (m)')) # comment out if not plotting aij
nameVar <- c(nameVar, c(expression('Specific Humidity (' * kg ~ kg^-1 * ')'), 'Relative Humidity (%)', 'Temperature (ºC)')) # comment out if not plotting aijl
nameVar <- c(nameVar, c('Temperature (ºC)')) # comment out if not plotting aijk
nameVar <- c(nameVar, c('KPP Boundary Layer Depth (m)', 'Mixed Layer Depth (m)')) # comment out if not plotting oij
nameVar <- c(nameVar, c(expression("Ocean Heat Content (" * J ~ kg^-1 * ")"), 'Ocean Potential Temperature (ºC)', 'Salinity (psu)')) # comment out if not plotting oijl


## need to account for extra column in sheet (layer) ##

for(ifile in 1:length(datList)) {

    data <- readRDS(datList[ifile])
    run <- data[1,1]
    minTime <- min(data$Year)
    data <- data %>% mutate(Time2 = (Year - minTime) + (as.numeric(Month_Num) / 12))
    maxTime <- max(data$Time2, na.rm = TRUE)
    
    for(ivar in 1:nrow(allVar)) {
        sub <- subset(data, Variable %in% allVar[ivar,2])
        subLo <- subset(sub, Latitude %in% names(latLo))
        subMid <- subset(sub, Latitude %in% names(latMid))
        subHi <- subset(sub, Latitude %in% names(latHi))
        
        # this is to add mean lines over the last 20% of the timeseries ##
        ### LOW LATITUDE MEANS ###
        latAv <- data.frame(Latitude = names(latLo), Mean = NA)
        latAv$Latitude <- factor(latAv$Latitude, levels = c(30, 14, 2, -14, -30))
        for(lat in 1:length(latLo)) {
            tempAv <- mean(subset(sub, Latitude == names(latLo)[lat] & Time2 > (maxTime - 3))$Mean_Value)
            latAv$Mean[lat] <- tempAv
        }

        ### LOW LATITUDE PLOT ###
        subLo$Latitude <- factor(subLo$Latitude, levels = c(30, 14, 2, -14, -30))
        ggplot() +
        geom_point(data = subLo, aes(x = Time2, y = Mean_Value), color = 'black', size = 1) +
        geom_line(data = subLo, aes(x = Time2, y = Mean_Value), color = 'black') +
        geom_hline(data = latAv, aes(yintercept = Mean)) +
        facet_wrap(~Latitude, ncol = 1, strip.position = 'right', labeller = labeller(Latitude = latLo)) + # 
        scale_x_continuous(expand = expansion(), limits = c(-0.2, (maxTime+0.2)), breaks = seq(0,maxTime,round_any(maxTime/5, 0.1, f = round))) +
        scale_y_continuous(expand = expansion(mult = 0.15), breaks = waiver(), n.breaks = 3) + #expand = expansion(), limits = c(0, maxY), 
        xlab("Model Mars Years") + ## ~687 Earth days
        ylab(nameVar[ivar]) +
        theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), text = element_text(size = 18), axis.text.x = element_text(size = 16), aspect.ratio = 0.25, axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(color = "black", fill=NA, linewidth=2), legend.key=element_blank(), legend.key.height = unit(0.83, "inch"), plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), plot.tag.position = c(0.15, 0.02), axis.title.y.right = element_text(margin = margin(l = 83)), legend.position = c(1.07, 0.52), panel.spacing = unit(5, "mm"))

        ggsave(paste0(run, "_EqPlot_", allVar[ivar,2], '_LoFacet_', format(Sys.time(), "%y%m%d"), ".png"), height = 6.25, width = 5, unit = 'in', dpi = 300)


        ### MID LATITUDE MEANS ###
        latAv <- data.frame(Latitude = names(latMid), Mean = NA)
        latAv$Latitude <- factor(latAv$Latitude, levels = c(62, 46, -46, -62))
        for(lat in 1:length(latMid)) {
            tempAv <- mean(subset(sub, Latitude == names(latMid)[lat] & Time2 > (maxTime - 3))$Mean_Value)
            latAv$Mean[lat] <- tempAv
        }

        ### MID LATITUDE PLOT ###
        subMid$Latitude <- factor(subMid$Latitude, levels = c(62, 46, -46, -62))
        ggplot() +
        geom_point(data = subMid, aes(x = Time2, y = Mean_Value), color = 'black', size = 1) +
        geom_line(data = subMid, aes(x = Time2, y = Mean_Value), color = 'black') +
        geom_hline(data = latAv, aes(yintercept = Mean)) +
        facet_wrap(~Latitude, ncol = 1, strip.position = 'right', labeller = labeller(Latitude = latMid)) + # 
        scale_x_continuous(expand = expansion(), limits = c(-0.2, (maxTime+0.2)), breaks = seq(0,maxTime,round_any(maxTime/5, 0.1, f = round))) +
        scale_y_continuous(expand = expansion(mult = 0.15), breaks = waiver(), n.breaks = 3) + #expand = expansion(), limits = c(0, maxY), 
        xlab("Model Mars Years") + ## ~687 Earth days
        ylab(nameVar[ivar]) +
        theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), text = element_text(size = 18), axis.text.x = element_text(size = 16), aspect.ratio = 0.25, axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(color = "black", fill=NA, linewidth=2), legend.key=element_blank(), legend.key.height = unit(0.83, "inch"), plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), plot.tag.position = c(0.15, 0.02), axis.title.y.right = element_text(margin = margin(l = 83)), legend.position = c(1.07, 0.52), panel.spacing = unit(5, "mm"))

        ggsave(paste0(run, "_EqPlot_", allVar[ivar,2], '_MidFacet_', format(Sys.time(), "%y%m%d"), ".png"), height = 6.25, width = 5, unit = 'in', dpi = 300)



        ### HIGH LATITUDE MEANS ###
        latAv <- data.frame(Latitude = names(latHi), Mean = NA)
        latAv$Latitude <- factor(latAv$Latitude, levels = c(90, 78, -78, -90))
        for(lat in 1:length(latHi)) {
            tempAv <- mean(subset(sub, Latitude == names(latHi)[lat] & Time2 > (maxTime - 3))$Mean_Value)
            latAv$Mean[lat] <- tempAv
        }

        ### HIGH LATITUDE PLOT ###
        subHi$Latitude <- factor(subHi$Latitude, levels = c(90, 78, -78, -90))
        ggplot() +
        geom_point(data = subHi, aes(x = Time2, y = Mean_Value), color = 'black', size = 1) +
        geom_line(data = subHi, aes(x = Time2, y = Mean_Value), color = 'black') +
        geom_hline(data = latAv, aes(yintercept = Mean)) +
        facet_wrap(~Latitude, ncol = 1, strip.position = 'right', labeller = labeller(Latitude = latHi)) + # 
        scale_x_continuous(expand = expansion(), limits = c(-0.2, (maxTime+0.2)), breaks = seq(0,maxTime,round_any(maxTime/5, 0.1, f = round))) +
        scale_y_continuous(expand = expansion(mult = 0.15), breaks = waiver(), n.breaks = 3) + #expand = expansion(), limits = c(0, maxY), 
        xlab("Model Mars Years") + ## ~687 Earth days
        ylab(nameVar[ivar]) +
        theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), text = element_text(size = 18), axis.text.x = element_text(size = 16), aspect.ratio = 0.25, axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(color = "black", fill=NA, size=2), legend.key=element_blank(), legend.key.height = unit(0.83, "inch"), plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), plot.tag.position = c(0.15, 0.02), axis.title.y.right = element_text(margin = margin(l = 83)), legend.position = c(1.07, 0.52), panel.spacing = unit(5, "mm"))

        ggsave(paste0(run, "_EqPlot_", allVar[ivar,2], '_HiFacet_', format(Sys.time(), "%y%m%d"), ".png"), height = 7, width = 5, unit = 'in', dpi = 300)

    }
}