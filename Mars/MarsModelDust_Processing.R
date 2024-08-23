## Goal: convert the model output to comparable units as the observations ##
## objective 1: sum all dust ##
## objective 2: calculate dust in kg/m^2 ##
## objective 3: calculate layer h ##
## objective 4: calculate tau in km^-1 ##
## objective 5: calculate layer pressure (Jan or Kostas methods) ##
## objective 6: interpolate dust to observational pressure values ##
## objective 7: interpolate dust to observational latitudes ##
## objective 8: calculate zonal means ##

## got an error about missing packages in slurm ##
install.packages('tidyverse')
install.packages('ncdf4')
install.packages('plyr')
install.packages('dplyr')
install.packages('scales')
install.packages('viridis')
install.packages('reshape')

### libraries ###
library(tidyverse)
library(ncdf4)
library(plyr)
library(dplyr)
library(scales)
library(viridis)
library(reshape)

## for local testing ##
# setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/PerlwitzDust/Analysis/MCSWorkingDir')
setwd(getwd())

subDir <- paste0('Analysis_', format(Sys.time(), "%y%m%d"))
dir.create(subDir)
plotDir <- paste0(subDir, '/Plots')
dir.create(plotDir)

runYrs <- list.files(getwd(), pattern = 'DEC')
runYrs <- runYrs[grep('.taijl', runYrs)]
runYrs <- substr(runYrs, 4, 7)

mons <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
lonList <- seq(-177.5, 177.5, 5)
latList <- seq(-90, 90, 4)
monConv <- data.frame(MarsLs = c('Ls0-30', 'Ls30-60', 'Ls60-90', 'Ls90-120', 'Ls120-150', 'Ls150-180', 'Ls180-210', 'Ls210-240', 'Ls240-270', 'Ls270-300', 'Ls300-330', 'Ls330-360'), ModelMonth = mons)

topo <- nc_open('marstopo_cap13K_s.nc') # topography
topo <- ncvar_get(topo, varid = 'zatmo')

exMCS <- nc_open('MCS Zonal Mean Dust_4R3D_MY29.nc')
MCSPLevels <- ncvar_get(exMCS, varid = 'P') ## extract P levels from example MCS data
MCSlatLevels <- ncvar_get(exMCS, varid = 'Latitude')
nc_close(exMCS)

dustPAL <- c('#800f27', '#be1b26', '#e2201d', '#fc4d2b', '#fd8d3d', '#feb24c', '#fed976', '#ffeda0', '#fffecc', '#ffffff')
dustPAL <- dustPAL[10:1]

for(iyr in 1:length(runYrs)) { #length(runYrs)
    yrList <- list.files(getwd(), pattern = runYrs[iyr])

    for(imon in 1:12) { #length(mons)
        mList <- yrList[grep(mons[imon], yrList)]

        ###############################
        ## objective 1: sum all dust ##
        ###############################

        taijl <- grep(paste0(runYrs[iyr], '.taijl'), mList)
        taijl <- mList[taijl]
        taijl <- nc_open(taijl)

        dust <- ncvar_get(taijl, varid = 'Clay')
        dust <- dust + ncvar_get(taijl, varid = 'Silt1')
        dust <- dust + ncvar_get(taijl, varid = 'Silt2')
        dust <- dust + ncvar_get(taijl, varid = 'Silt3')
        dust <- dust + ncvar_get(taijl, varid = 'Silt4')
        dust <- dust * 1e-8
        nc_close(taijl)
        
        ## testing ##
        print('obj1: done')

        ###########################################
        ## objective 2: calculate dust in kg/m^2 ##
        ###########################################

        aijl <- grep(paste0(runYrs[iyr], '.aijl'), mList)
        aijl <- mList[aijl]
        aijl <- nc_open(aijl)
        airmass <- ncvar_get(aijl, varid = 'airmass')

        dust <- dust * airmass
        dust <- array(data = dust, dim = c(72,46,40), dimnames = list(lonList, latList, 1:40))

        ## testing ##
        print('obj2: done')

        ####################################
        ## objective 3: calculate layer h ##
        ####################################

        z <- ncvar_get(aijl, varid = 'z')
        z <- array(data = z, dim = c(72,46,40), dimnames = list(lonList, latList, 1:40))
        zdelta <- array(c(NA, NA, NA), dim = c(72, 46, 40), dimnames = list(lonList, latList, 1:40))
        for(ilayer in 1:39){
            tz <- z[,,ilayer]
            if(ilayer == 1) {
                for(ilon in 1:72) {
                    for(ilat in 1:46) {
                        zdelta[ilon, ilat, ilayer] <- (tz[ilon, ilat] - topo[ilon, ilat]) * 2
                    }
                }
            } else{
                for(ilon in 1:72) {
                    for(ilat in 1:46) {
                        tzdelta <- (tz[ilon, ilat] - topo[ilon, ilat])
                        tzdelta <- tzdelta - sum(zdelta[ilon, ilat, (1:(ilayer-1))])
                        zdelta[ilon, ilat, ilayer] <- tzdelta * 2
                    }
                }
            }
        }
        zM <- melt(z)
        colnames(zM) <- c('lon', 'lat', 'layer', 'height')
        nc_close(aijl)
        zdelta <- zdelta / 1000 # convert from m to km

        ## testing ##
        print('obj3: done')

        #########################################
        ## objective 4: calculate tau in km^-1 ##
        #########################################

        Qext = 0.3506 # dimensionless
        rEff = 1.06e-6 # m
        rhoDust = 2.5e3 # kg/m^3

        tau <- 3 * Qext * dust
        tau <- tau / (4 * rEff * rhoDust)
        tau <- tau / zdelta
        tauM <- melt(tau)
        colnames(tauM) <- c('lon', 'lat', 'layer', 'dustTau')

        ## testing ##
        print('obj4: done')

        ############################################################
        ## objective 5a: calculate layer pressure (Kostas method) ##
        ##        start at top, sum mass per square area          ##
        ############################################################

        g <- 3.71 # m/s^2
        pres <- array(c(NA, NA, NA), dim = c(72, 46, 40), dimnames = list(lonList, latList, 1:40))
        for(ilayer in 40:1) {
            tAM <- airmass[,,ilayer]
            
            if(ilayer < 40) {
                tAM <- pres[,,(ilayer+1)] + tAM
            }
            pres[,,ilayer] <- tAM
        }
        pres <- pres * g # in Pa
        pres <- pres / 100 # to mbar
        presM <- melt(pres)
        colnames(presM) <- c('lon', 'lat', 'layer', 'pressure')

        dataOut <- join(zM, presM, by = c('lon', 'lat', 'layer'))
        dataOut <- join(dataOut, tauM, by = c('lon', 'lat', 'layer'))
        dataOut <- cbind('ModelData', runYrs[iyr], mons[imon], dataOut)
        colnames(dataOut)[1:3] <- c('DataType', 'Year', 'Month')

        ## testing ##
        print('obj5: done')

        ####################################################################
        ## objective 6: interpolate dust to observational pressure values ##
        ####################################################################

        pComp <- MCSPLevels / 100
        pComp <- pComp[1:43]
        nilat <- length(latList)
        nilon <- length(lonList)
        nipres <- length(pComp)

        interpOut1 <- data.frame(matrix(NA, nrow = (nilat*nilon*nipres), ncol = 6))
        for(ilat in 1:46) {
            latTime <- Sys.time()
            subLat <- subset(dataOut, lat == latList[ilat])
            
            for(ilon in 1:72) {
                lonTime <- Sys.time()
                sub <- subset(subLat, lon == lonList[ilon])

                for(ipres in 1:length(pComp)) {
                    if(pComp[ipres] > max(sub$pressure)) { # higher pressure (lower elevation) than data
                        interpOut1[(((ilat-1)*(nilon*nipres))+((ilon-1)*nipres)+(ipres)), ] <- c(sub$Year[1], sub$Month[1], lonList[ilon], latList[ilat], pComp[ipres], NA) ## use this method to write to a large table
                    } else if(pComp[ipres] < min(sub$pressure)) { # lower pressure (higher elevation) than data
                        interpOut1[(((ilat-1)*(nilon*nipres))+((ilon-1)*nipres)+(ipres)), ] <- c(sub$ModelYear[1], sub$Month[1], latList[ilat], pComp[ipres], NA) ## use this method to write to a large table
                    } else { # data is within pressure levels
                        ## find nearest model data ##
                        tComp <- cbind(NA, pComp[ipres], NA)
                        colnames(tComp) <- c('layer', 'pressure', 'dustTau')
                        tReg <- sub[,c(6,8,9)]
                        tReg <- rbind(tReg, tComp)
                        tReg <- tReg[order(-tReg$pressure),]
                        index <- match(pComp[ipres], tReg$pressure)
                        tReg <- tReg[(index-2):(index+2),]
                        reg <- lm(tReg$dustTau ~ log(tReg$pressure))
                        reg <- summary(reg)
                        reg <- reg$coefficients
                        dInterp <- reg[1,1] + (reg[2,1] * log(pComp[ipres]))

                        interpOut1[(((ilat-1)*(nilon*nipres))+((ilon-1)*nipres)+(ipres)), ] <- c(sub$Year[1], sub$Month[1], lonList[ilon], latList[ilat], pComp[ipres], dInterp)
                    }
                }
            }

            ## testing ##
            print(paste0('ilat: ', ilat))
        }
        interpOut1[,3:6] <- lapply(interpOut1[,3:6],as.numeric)
        colnames(interpOut1) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')

        ## testing ##
        print('obj6: done')

        ##############################################################
        ## objective 7: interpolate dust to observational latitudes ##
        ##############################################################

        nilon <- length(lonList)
        nipres <- length(pComp)
        nilat <- length(MCSlatLevels)

        interpOut <- data.frame(matrix(NA, nrow = (length(lonList)*length(pComp)*length(MCSlatLevels)), ncol = 6))
        for(ilon in 1:length(lonList)){ ## subset at specific long
            lonSub <- subset(interpOut1, lon == lonList[ilon])

            for(ipres in 1:length(pComp)) { ## subset at specific pressure
                pSub <- subset(lonSub, round(pressure, digits = 3) == round(pComp[ipres], digits = 3))
                naPTab <- is.na(pSub$dustTau)
                naPTab <- naPTab[naPTab==FALSE]

                if(length(naPTab) > 0) { ## this checks to see if there are any data at the P level

                    for(ilat in 1:length(MCSlatLevels)) { ## iterate thru each MCS lat level for interpolation
                        latComp <- MCSlatLevels[ilat]
                        latSel <- c(latList, latComp)
                        latSel <- latSel[order(latSel)]

                        index <- match(latComp, latSel)
                        latSub <- subset(pSub, lat == latSel[index-1] | lat == latSel[index+1])
                        nalatTab <- is.na(latSub$dustTau)
                        nalatTab <- nalatTab[nalatTab==FALSE]

                        ## check to see if there are any model neighbors to MCS lats ##
                        if(length(nalatTab) == 2) { ## there are two neighbors: interpolation between two points
                            reg <- lm(latSub$dustTau ~ latSub$lat)
                            reg <- summary(reg)
                            reg <- reg$coefficients
                            dInterp <- reg[1,1] + (reg[2,1] * latComp)

                            interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], dInterp)

                        } else if(length(nalatTab) == 1) { ## there is one neighbor: check which one is closest
                            latSubOrd <- latSub[order(latSub$dustTau),]
                            delLat <- abs(latComp - latSubOrd$lat[1])
                            delLatna <- abs(latComp - latSubOrd$lat[2])

                            if(delLat < delLatna){ ## the 'data' latitude is closest to the MCS lat: insert data
                                interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], latSubOrd$dustTau[1])
          
                            } else{ ## the 'na' latitude is closest to MCS lat: insert na
                                interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], NA)
                            
                            }
                        } else { ## there are no neighbors: insert na
                            interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], NA)

                        }
                    }
                } else{ # there are no data at all lats
                    interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(pSub[1,1:3], MCSlatLevels, pSub[1,5], NA)
                
                }
            }

            ## testing ##
            print(paste0('ilon: ', ilon))

        }

        colnames(interpOut) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')
        saveRDS(interpOut, file = paste0(subDir, '/ModelData_Interp_', mons[imon], runYrs[iyr], '_', format(Sys.time(), "%y%m%d"), ".rds"))

        ## testing ##
        print('obj7: done')

        ########################################
        ## objective 8: calculate zonal means ##
        ########################################

        zonalOut <- data.frame()
        for(ilat in 1:length(MCSlatLevels)) {
            tLat <- subset(interpOut, lat == MCSlatLevels[ilat])
            tLat <- tLat[is.na(tLat$dustTau)==FALSE,]

            for(ipres in 1:length(pComp)) {
                tPres <- subset(tLat, round(pressure, digit = 3) == round(pComp[ipres], digit = 3))

                if(nrow(tPres) == 0) {
                    tOut <- cbind(tLat[1,1:2], MCSlatLevels[ilat], pComp[ipres], NA, NA, 0)
                    colnames(tOut) <- c('Year', 'Month', 'lat', 'pressure', 'MeanDustOpacity', 'SDDustOpacity', 'n')
                    zonalOut <- rbind(zonalOut, tOut)
                } else {
                    tOut <- cbind(tLat[1,1:2], MCSlatLevels[ilat], pComp[ipres], mean(tPres$dustTau, na.rm = TRUE), sd(tPres$dustTau, na.rm = TRUE), nrow(tPres))
                    colnames(tOut) <- c('Year', 'Month', 'lat', 'pressure', 'MeanDustOpacity', 'SDDustOpacity', 'n')
                    zonalOut <- rbind(zonalOut, tOut)
                }
            }
        }
        saveRDS(zonalOut, file = paste0(subDir, '/ModelData_ZonalMeans_', mons[imon], runYrs[iyr], '_', format(Sys.time(), "%y%m%d"), ".rds"))

        ggplot(zonalOut, aes(x = lat, y = pressure, z = MeanDustOpacity)) +
        geom_contour_filled(bins = 10) + # 
        ggtitle(paste0('Model Zonal Dust ', runYrs[iyr], mons[imon])) +
        scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), breaks = c(10, 1, 0.1), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
        scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
        scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
        #scale_fill_brewer(palette = "YlOrRd") +
        guides(fill = guide_colorsteps(title.position = "right", title = 'Dust Opacity (km-1)')) + # , title = VAR
        theme(plot.title = element_text(hjust = 0.5, size = 21,
            face = "bold"),
            text = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            aspect.ratio = 0.625,
            axis.line = element_line(color = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect('grey85'), #element_blank()
            panel.border = element_rect(color = "black", fill=NA, size=2),
            legend.title=element_text(angle = -90),
            legend.key=element_blank(),
            legend.key.height = unit(0.5, "inch"),
            legend.title.align = 0.5,
            plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
            plot.tag.position = c(0.15, 0.02),
            axis.title.y.right = element_text(margin = margin(l = 83)),
            #legend.position = c(1.2, 0.5)
        )
        ggsave(paste0(plotDir, '/', 'MCSDust_Lower_', runYrs[iyr], '_', imon, monConv[imon,2], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)
    
        ## testing ##
        print('obj8: done')

    }
}

## now we have a table for each month of each year for all relevant dust data ##