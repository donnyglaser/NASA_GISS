## Goal: convert the model output to comparable units as the observations ##
## objective 1: sum all dust ##
## objective 2: calculate dust in kg/m^2 ##
## objective 3: calculate layer h ##
## objective 4: calculate tau in km^-1 ##
## objective 5: calculate layer pressure (Jan or Kostas methods) ##
## objective 6: interpolate dust to observational pressure values ##
## objective 7: interpolate dust to observational latitudes ##
## objective 8: calculate zonal means ##

## from Jan's email: ##
# I see following steps to get there:

# You need psf = 6.86 hPa, the nominal surface pressure (=plbot(1)) for Mars; from taijl: plm, ple, tau_3d_CS; from aijl: z; from aij: prsurf

# 1. Calculate the grid box and time dependent mid pressure values of the sigma layers:

# pmid(i,j,l) = sig(l) * (prsurf(i,j) - ple(ls1-1)) + ple(ls1-1)

# with sig(l)=(plm(l) - ple(ls1-1)) / (psf - ple(ls1-1))

# In the planet version of the model, ls1 is calculated. It should be 31 for Mars (ple(30)=plbot(31)=0.216117886178862 hPa from what I see from the PRT-file). Igor, is that correct?

# With the location and time dependent pressure values at the sigma levels, we have now the AODs as a function of location and pressure tau_3d(i,j,pmid(i,j,l)).

# 2. tau_3d is dimensionless in the diagnostics. So it also needs to be divided by the layer thickness, which can be derived from z(i,j,l) in the aijl-file. I suppose, since z is the geometric height of the sigma layer, i.e, it's just a value at the midpoint, but we need a height difference, using the logarithmic mean from z(l+1)-z(l) and z(l)-z(l-1) would be the best approximation to get the layer thickness. Then we have dtau/dz as a function of location and pressure.

# 3. Interpolation from dtau(i,j,pmid(i,j,l))/dz to dtau(i,j,p(mcs(k)))/dz. I would use a log-linear interpolation.

# 4. The MCS opacities need to be multiplied with 7.3 (Kass et al. 2017), since they are at the wavelength of 22 um, and we want to convert them to the extinction AOD in the visible range to compare them with the model output.

# 5. Calculating the zonal averages of the derived dAOD/dz

###############################################################################

## second Jan email: ##

# >> the relevant paper for the MCS retrievals by Kleinboehl et al. is
# >> attached. For the dust retrieval, channel A5 is used (463 cm^-1, which
# >> is 21.6 um frequency). I don't think that we even need to do our own
# >> separate Mie calculations. The paper already provides the extinction
# >> efficiency for dust for that wavenumber from Mie calculations. It is
# >> Q_ext=0.5473 (Table 2), which is based on the assumptions on the size
# >> distribution, effective radius, and effective variance for the MCS
# >> retrievals. And we want to be as close as possible to the assumptions of
# >> the retrievals for our comparison.
# >>
# >> We just take this Q_ext and combine it with our simulated dust mass to
# >> calculate the layer optical depth from the layer dust mass in our
# >> simulations:
# >>
# >> tau_layer=3*Q_ext*m_dust / (4*r_eff*rho_dust)
# >>
# >> with m_dust dust is the mass in [kg*m^2] in the layer. m_dust can be
# >> derived from Clay, Silt1, Silt2, Silt3, Silt4 in the taijl file by
# >> multiplying those mixing ratios in the layers with the layer variable
# >> "airmass".
# >>
# >> r_eff = 1.5e-6 m is the effective radius of dust, assumed the same as in
# >> the retrievals.
# >>
# >> rho_dust is the particle density of dust, with rho_clay=2.5e3 kg m^-3
# >> and rho_silt[1-4]=2.65e3 kg m^-3.
# >>
# >> Do this for all the size bins for the individual contributions and
# >> summed up then for the total dust.
# >>
# >> tau_layer then just needs to be divided by the layer thickness again to
# >> get the simulated opacity in km^-1 in the layer, and then brought onto
# >> the pressure coordinate system for the comparison with the MCS data.

### libraries ###
library(tidyverse)
library(ncdf4)
library(plyr)
library(dplyr)
library(scales)
library(viridis)
library(reshape)


## for local testing ##
setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/PerlwitzDust/Analysis/MCSWorkingDir')


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

runTime0 <- Sys.time()

for(iyr in 1:1) { #length(runYrs)
    yrList <- list.files(getwd(), pattern = runYrs[iyr])

    for(imon in 1:1) { #length(mons)
        ###### TESTING ######
        runTime <- Sys.time()
        print(runTime)
        ###### TESTING ######

        mList <- yrList[grep(mons[imon], yrList)]

        ## objective 1: sum all dust ##
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
        ########################################################

        ## objective 2: calculate dust in kg/m^2 ##
        aijl <- grep(paste0(runYrs[iyr], '.aijl'), mList)
        aijl <- mList[aijl]
        aijl <- nc_open(aijl)
        airmass <- ncvar_get(aijl, varid = 'airmass')

        dust <- dust * airmass
        dust <- array(data = dust, dim = c(72,46,40), dimnames = list(lonList, latList, 1:40))
        ########################################################

        ## objective 3: calculate layer h ##
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
        ########################################################

        ## objective 4: calculate tau in km^-1 ##
        Qext = 0.3506 # dimensionless
        rEff = 1.06e-6 # m
        rhoDust = 2.5e3 # kg/m^3

        tau <- 3 * Qext * dust
        tau <- tau / (4 * rEff * rhoDust)
        #####################
        tau <- tau / zdelta    #work on this
        #####################
        tauM <- melt(tau)
        colnames(tauM) <- c('lon', 'lat', 'layer', 'dustTau')
        ########################################################

        ## objective 5a: calculate layer pressure (Kostas method) ##
        ## start at top, sum mass per square area ##
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
        
        # ########################################################
        # ## TEST PLOT ##
        # ## map ##
        # xmap <- subset(dataOut, layer == 1)
        # ggplot(xmap, aes(x = lon, y = lat, fill = dustTau)) +
        # geom_tile()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## profile: layer ##
        # xprof <- subset(dataOut, lon == 2.5)
        # ggplot(xprof, aes(x = lat, y = layer, fill = dustTau)) +
        # geom_tile()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## profile: pressure ##
        # ggplot(xprof, aes(x = lat, y = pressure, color = dustTau)) +
        # geom_point()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## profile: height ##
        # ggplot(xprof, aes(x = lat, y = height, color = dustTau)) +
        # geom_point()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## profile: layer ##
        # xprof <- subset(xprof, height <= 20000)
        # ggplot(xprof, aes(x = lat, y = layer, color = dustTau)) +
        # geom_point()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## single column: h vs p ##
        # xprof <- subset(dataOut, lon == 2.5)
        # xcol <- subset(xprof, lat == 2)
        # ggplot(xcol, aes(x = height, y = pressure)) +
        # geom_point()
        # ## THIS WORKS ##
        # ###########################


        ## objective 6: interpolate dust to observational pressure values ##
        ## THIS NEEDS A LOT OF FIXING TO MAKE MORE EFFICIENT ##
        pComp <- MCSPLevels / 100
        pComp <- pComp[1:43]
        nilat <- length(latList)
        nilon <- length(lonList)
        nipres <- length(pComp)

        interpOut1 <- data.frame(matrix(NA, nrow = (nilat*nilon*nipres), ncol = 6))
        ## TESTING ##
        runTime <- Sys.time()
        ## TESTING ##
        for(ilat in 1:46) {
            latTime <- Sys.time()
            subLat <- subset(dataOut, lat == latList[ilat])
            
            for(ilon in 1:72) {
                lonTime <- Sys.time()
                sub <- subset(subLat, lon == lonList[ilon])

                for(ipres in 1:length(pComp)) {

                    if(pComp[ipres] > max(sub$pressure)) { # higher pressure (lower elevation) than data
                        interpOut1[(((ilat-1)*(nilon*nipres))+((ilon-1)*nipres)+(ipres)), ] <- c(sub$Year[1], sub$Month[1], lonList[ilon], latList[ilat], pComp[ipres], NA) ## use this method to write to a large table
                        # colnames(tOut) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')
                        # interpOut1 <- rbind(interpOut1, tOut)
                    } else if(pComp[ipres] < min(sub$pressure)) { # lower pressure (higher elevation) than data
                        interpOut1[(((ilat-1)*(nilon*nipres))+((ilon-1)*nipres)+(ipres)), ] <- c(sub$ModelYear[1], sub$Month[1], latList[ilat], pComp[ipres], NA) ## use this method to write to a large table
                        # colnames(tOut) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')
                        # interpOut1 <- rbind(interpOut1, tOut)
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
                        # colnames(tOut) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')
                        # interpOut1 <- rbind(interpOut1, tOut)
                    }
                }

                # ###### TESTING ######
                # print(paste0('ilon = ', ilon, ': ', Sys.time() - lonTime))
                # ###### TESTING ######

            }

            ###### TESTING ######
            print(paste0('ilat = ', ilat, ': ', Sys.time() - latTime))
            ###### TESTING ######

        }
        ###### TESTING ######
        print(paste0('Total Runtime: ', Sys.time() - runTime))
        ###### TESTING ######
        interpOut1[,3:6] <- lapply(interpOut1[,3:6],as.numeric)
        colnames(interpOut1) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')

        

        # ## TEST PLOT ##
        # ## map ##
        # xmap <- subset(interpOut1, pressure == pComp[15])
        # ggplot(xmap, aes(x = lon, y = lat, fill = dustTau)) +
        # geom_tile()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## profile: pressure ##
        # xprof <- subset(interpOut1, lon == 2.5)
        # ggplot(xprof, aes(x = lat, y = pressure, z = dustTau)) +
        # geom_contour_filled() +
        # scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x))
        # #geom_point(size=2)
        # ## THIS WORKS ##

        ########################################################

        ## objective 7: interpolate dust to observational latitudes ##

        ## i think this might be prohibitively inefficient (240701) ##
        nilon <- length(lonList)
        nipres <- length(pComp)
        nilat <- length(MCSlatLevels)

        ## TESTING ##
        runTime1 <- Sys.time()
        ## TESTING ##

        interpOut <- data.frame(matrix(NA, nrow = (length(lonList)*length(pComp)*length(MCSlatLevels)), ncol = 6))
        for(ilon in 1:length(lonList)){ ## subset at specific long
            lonSub <- subset(interpOut1, lon == lonList[ilon])

            ## TESTING ##
            runTime <- Sys.time()
            ## TESTING ##

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

                        ## check to see if there are any model neighbors to MCS lats
                        if(length(nalatTab) == 2) { ## there are two neighbors: interpolation between two points
                            reg <- lm(latSub$dustTau ~ latSub$lat)
                            reg <- summary(reg)
                            reg <- reg$coefficients
                            dInterp <- reg[1,1] + (reg[2,1] * latComp)


                            ## work on this ##
                            interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], dInterp)
                            ## work on this ##


                        } else if(length(nalatTab) == 1) { ## there is one neighbor: check which one is closest
                            latSubOrd <- latSub[order(latSub$dustTau),]
                            delLat <- abs(latComp - latSubOrd$lat[1])
                            delLatna <- abs(latComp - latSubOrd$lat[2])


                            # if(is.na(delLatna)){

                            #     interpOut[(((ilon-1)*idx1)+((ipres-1)*72)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], latSubOrd$dustTau[1])

                            # } 
                            if(delLat < delLatna){ ## the 'data' latitude is closest to the MCS lat: insert data
                                
                                
                                ## work on this ##
                                interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], latSubOrd$dustTau[1])
                                ## work on this ##


                            } else{ ## the 'na' latitude is closest to MCS lat: insert na
                                
                                
                                ## work on this ##
                                interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], NA)
                                ## work on this ##


                            }
                        } else { ## there are no neighbors: insert na


                            ## work on this ##
                            interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(latSub[1,1:3], latComp, latSub[1,5], NA)
                            ## work on this ##


                        }
                    }
                } else{ # there are no data at all lats


                    ## work on this ##
                    interpOut[(((ilon-1)*(nipres*nilat))+((ipres-1)*nilat)+(ilat)), ] <- cbind(pSub[1,1:3], MCSlatLevels, pSub[1,5], NA)
                    ## work on this ##


                }
            }

            ## TESTING ##
            print(paste0('ilon = ', ilon, ', time: ', Sys.time() - runTime))
            ## TESTING ##
        }

        ## TESTING ##
        print(paste0('total time: ', Sys.time() - runTime1))
        ## TESTING ##

        colnames(interpOut) <- c('Year', 'Month', 'lon', 'lat', 'pressure', 'dustTau')
        saveRDS(interpOut, file = paste0(subDir, '/ModelData_Interp_', mons[imon], runYrs[iyr], '_', format(Sys.time(), "%y%m%d"), ".rds"))

        # ## TEST PLOTS ##
        # ## map ##
        # xmap <- subset(interpOut, pressure == pComp[15])
        # ggplot(xmap, aes(x = lon, y = lat, fill = dustTau)) +
        # geom_tile()
        # ## THIS WORKS ##
        # ###########################
        # ## TEST PLOT ##
        # ## profile: pressure ##
        # xprof <- subset(interpOut1, lon == 2.5)
        # ggplot(xprof, aes(x = lat, y = pressure, z = dustTau)) +
        # geom_contour_filled() +
        # scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x))
        # #geom_point(size=2)
        # ## THIS WORKS ##


        ## objective 8: calculate zonal means ##

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

        # ## TEST PLOTS ##
        # ggplot(zonalOut, aes(x = lat, y = pressure, z = MeanDustOpacity)) +
        # geom_contour_filled() +
        # scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x))
        # ## This works ##

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

        ###### TESTING ######
        runTime <- Sys.time() - runTime
        print(runTime)
        ###### TESTING ######

        #saveRDS(zonalOut, file = paste0(subDir, '/ModelData_Zonal_', mons[imon], runYrs[iyr], '_', format(Sys.time(), "%y%m%d"), ".rds"))
    }
}



## now we have a table for each month of each year for all relevant dust data ##

print(paste0('all loops run time: ', Sys.time() - runTime0))







###############
## Orphan ##
########################################################
        
        aij <- mList
        aij[c(taijl, aijl)] <- NA
        aij <- aij[!is.na(aij)]
        
        
        
        
        aij <- nc_open(aij)

        plm <- ncvar_get(taijl, varid = 'plm')
        ple <- ncvar_get(taijl, varid = 'ple')
        tau3d <- ncvar_get(taijl, varid = 'tau_3D_CS')

        z <- ncvar_get(aijl, varid = 'z')
        prsurf <- ncvar_get(aij, varid = 'prsurf')

        nc_close(taijl)
        nc_close(aijl)
        nc_close(aij)

        




        ## Calculate pressure ##
        sig <- c()
        for(ilayer in 1:40) {
            sig[ilayer] <- (plm[ilayer] - ple[30]) / (6.86 - ple[30]) # sig(l)=(plm(l) - ple(ls1-1)) / (psf - ple(ls1-1))
        }

        pmid <- array(c(NA, NA, NA), dim = c(72, 46, 40), dimnames = list(lonList, latList, 1:40))
        for(ilayer in 1:40) {
            tsig <- sig[ilayer]
            for(ilon in 1:72) {
                for(ilat in 1:46) {
                    pmid[ilon, ilat, ilayer] <- ( tsig * (prsurf[ilon,ilat] - ple[30]) ) + ple[30] # pmid(i,j,l) = sig(l) * (prsurf(i,j) - ple(ls1-1)) + ple(ls1-1)
                }
            }
        }
        ## /Calculate pressure ##

        ## calculate the height of the model layer (Jan's method) ##
        zdelta <- array(c(NA, NA, NA), dim = c(72, 46, 40), dimnames = list(lonList, latList, 1:40))
        for(ilayer in 1:39) { # using the logarithmic mean from z(l+1)-z(l) and z(l)-z(l-1)
            tz <- z[,,ilayer]
            if(ilayer == 1) {
                zup <- z[,,(ilayer + 1)]
                for(ilon in 1:72) {
                    for(ilat in 1:46) {
                        tzdelta <- (zup[ilon, ilat] - tz[ilon, ilat]) - (tz[ilon, ilat] - topo[ilon, ilat])
                        tzdelta <- abs(tzdelta) / abs(log(zup[ilon, ilat] - tz[ilon, ilat]) - log(tz[ilon, ilat] - topo[ilon, ilat]))
                        zdelta[ilon, ilat, ilayer] <- tzdelta
                    }
                }
            } else{
                zup <- z[,,(ilayer + 1)]
                zlo <- z[,,(ilayer - 1)]
                zup <- z[,,(ilayer + 1)]
                for(ilon in 1:72) {
                    for(ilat in 1:46) {
                        tzdelta <- (zup[ilon, ilat] - tz[ilon, ilat]) - (tz[ilon, ilat] - zlo[ilon, ilat])
                        tzdelta <- abs(tzdelta) / abs(log(zup[ilon, ilat] - tz[ilon, ilat]) - log(tz[ilon, ilat] - zlo[ilon, ilat]))
                        zdelta[ilon, ilat, ilayer] <- tzdelta
                    }
                }
            }
        }
        ## /calculate the height of the model layer (Jan's method) ##

        ## calculate the height of the model layer (Kostas' method) ##
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
        ## /calculate the height of the model layer (Kostas' method) ##

        ## combine all values into table ##
        dimnames(tau3d) <- list(lonList, latList, 1:40)
        tauMelt <- melt(tau3d)
        colnames(tauMelt) <- c('Lon', 'Lat', 'Layer', 'Tau3D_CS')
        pMelt <- melt(pmid)
        colnames(pMelt) <- c('Lon', 'Lat', 'Layer', 'Pressure')
        allData <- join(tauMelt, pMelt, by = c('Lon', 'Lat', 'Layer'), type = 'left')
        dimnames(z) <- list(lonList, latList, 1:40)
        zMelt <- melt(z)
        colnames(zMelt) <- c('Lon', 'Lat', 'Layer', 'Height')
        allData <- join(allData, zMelt, by = c('Lon', 'Lat', 'Layer'), type = 'left')
        zdelMelt <- melt(zdelta)
        colnames(zdelMelt) <- c('Lon', 'Lat', 'Layer', 'Delta_Z')
        allData <- join(allData, zdelMelt, c('Lon', 'Lat', 'Layer'), type = 'left')

        allData <- allData %>% mutate(dTau_dz = Tau3D_CS / Delta_Z)

        #saveRDS(allData, filename = paste0('ReducedDust_3D_', mons[imon], runYrs[iyr], '_', format(Sys.time(), "%y%m%d"), ".rds"))

        ## Zonal Means ##
        zoneOut <- data.frame()
        for(ilayer in 1:40) {
            tlayer <- subset(allData, Layer == ilayer)
            for(ilat in 1:46) {
                tlat <- subset(tlayer, Lat == latList[ilat])

                tOut <- cbind(runYrs[iyr], mons[imon], tlat[1,2:3], mean(tlat$Tau3D_CS), sd(tlat$Tau3D_CS), mean(tlat$Pressure), sd(tlat$Pressure), mean(tlat$Height), sd(tlat$Height), mean(tlat$Delta_Z), sd(tlat$Delta_Z), mean(tlat$dTau_dz), sd(tlat$dTau_dz))
                colnames(tOut)[1:2] <- c('ModelYear', 'Month')
                colnames(tOut)[5:14] <- c('mean_Tau3D_CS', 'sd_Tau3D_CS', 'mean_Pressure', 'sd_Pressure', 'mean_Height', 'sd_Height', 'mean_Delta_Z', 'sd_Delta_Z', 'mean_dTau_dz', 'sd_dTau_dz')
                zoneOut <- rbind(zoneOut, tOut)
            }
        }
        
        ## do interpolation here ##
        ## first, interpolate between pressure levels ##
        

        interpOut1$dTau_dz_interp[interpOut1$dTau_dz_interp < 0 & is.na(interpOut1$dTau_dz_interp) == FALSE] <- 0

        ## then interpolate between latitudes ##
        interpOut <- data.frame()
        for(ipres in 1:length(MCSPLevels)) {
            subPres <- interpOut1
            subPres$Pressure <- round(subPres$Pressure, digits = 3)
            subPres <- subset(subPres, Pressure == round((MCSPLevels[ipres] / 100), digits = 3))

            for(ilat in 1:length(MCSlatLevels)) {
                latComp <- MCSlatLevels[ilat]
                tComp <- cbind(latComp, NA)
                colnames(tComp) <- c('Lat', 'dTau_dz_interp')
                if(latComp < -86) {
                    tReg <- subPres[1:2,]

                } else if(latComp > 86) {
                    tReg <- subPres[45:46,]

                } else {
                    tReg <- subPres[,c(3,5)]
                    tReg <- rbind(tReg, tComp)
                    tReg <- tReg[order(tReg$Lat),]
                    index <- match(latComp, tReg$Lat)
                    tReg <- tReg[(index-1):(index+1),]
                    tReg <- tReg[-2,]
                }

                if(sum(is.na(tReg$dTau_dz_interp) > 0, na.rm=FALSE) > 0) {
                    tOut <- cbind(subPres$ModelYear[1], subLat$Month[1], latComp, round((MCSPLevels[ipres] / 100), digits = 3), NA)
                    colnames(tOut) <- c('ModelYear', 'Month', 'Lat', 'Pressure', 'dTau_dz_interp')
                    interpOut <- rbind(interpOut, tOut)
                } else {
                    reg <- lm(tReg$dTau_dz_interp ~ tReg$Lat)
                    reg <- summary(reg)
                    reg <- reg$coefficients
                    dInterp <- reg[1,1] + (reg[2,1] * latComp)

                    tOut <- cbind(subPres$ModelYear[1], subLat$Month[1], latComp, round((MCSPLevels[ipres] / 100), digits = 3), dInterp)
                    colnames(tOut) <- c('ModelYear', 'Month', 'Lat', 'Pressure', 'dTau_dz_interp')
                    interpOut <- rbind(interpOut, tOut)
                }
            }
        }
        interpOut[,3:5] <- lapply(interpOut[,3:5],as.numeric)
        interpOut$dTau_dz_interp[interpOut$dTau_dz_interp < 0 & is.na(interpOut$dTau_dz_interp) == FALSE] <- 0

        saveRDS(interpOut, file = paste0(subDir, '/ModelReducedDust_', runYrs[iyr], '_', imon, mons[imon], '.rds'))

        dustCuts <- c(seq(0, 0.01, 0.001), 10)
        dustPAL <- magma(11)
        #names(dustPAL) <- c("(0,0.001]", "(0.001,0.002]", "(0.002,0.003]", "(0.003,0.004]", "(0.004,0.005]", "(0.005,0.006]", "(0.006,0.007]", "(0.007,0.008]", "(0.008,0.009]", "(0.009,0.01]", "(0.01,10]")

        plotInterp <- interpOut
        plotInterp <- cbind(plotInterp, cut(plotInterp$dTau_dz_interp, breaks = dustCuts))
        colnames(plotInterp)[6] <- 'dustBin'
        
        ## Whole atmosphere ##
        ggplot(plotInterp, aes(x = Lat, y = Pressure, z = dTau_dz_interp)) +
        geom_contour_filled(breaks = dustCuts) + # , breaks = BREAK
        #geom_tile(aes(fill = dTau_dz_interp), height = 0.05) +
        ggtitle(paste0('Model Dust ', runYrs[iyr], mons[imon])) +
        scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.0001), breaks = c(1, 0.1, 0.01, 0.001, 0.0001), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
        scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
        scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
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
        ggsave(paste0(plotDir, '/', 'ModelDust_', runYrs[iyr], '_', imon, mons[imon], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)

        # lower atmosphere #
        ggplot(plotInterp, aes(x = Lat, y = Pressure, z = dTau_dz_interp)) +
        geom_contour_filled(breaks = dustCuts) + # , breaks = BREAK
        #geom_tile(aes(fill = dTau_dz_interp), height = 0.05) +
        ggtitle(paste0('Model Dust ', runYrs[iyr], mons[imon])) +
        scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
        scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
        scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
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
        ggsave(paste0(plotDir, '/', 'ModelDust_Lower_', runYrs[iyr], '_', imon, mons[imon], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)





## open MCS data for comparison ##

MCSList <- list.files(getwd(), pattern = 'MCS Zonal Mean Dust')
MCSYear <- str_sub(MCSList, 26, 29)

for(iyear in 1:length(MCSList)) {
    nc <- nc_open(MCSList[iyear])
    pLev <- ncvar_get(nc, varid = 'P')
    pLev <- pLev / 100
    pLev <- round(pLev, digits = 3)
    MCSlatLevels <- ncvar_get(exMCS, varid = 'Latitude')
    MCSlatLevels <- MCSlatLevels
    for(imon in 1:12) {
        tDust <- ncvar_get(nc, varid = paste0('Zonal_Mean_Dust_', monConv[imon,1]))
        colnames(tDust) <- pLev
        row.names(tDust) <- MCSlatLevels
        dustM <- melt(tDust)
        colnames(dustM) <- c('Latitude', 'Pressure', 'DustOpacity')
        dustM <- dustM %>% mutate(DustOpacity_Corr = DustOpacity * 7.3) # (Kass et al. 2017)
        dustM <- cbind(MCSYear[iyear], monConv[imon,2], dustM)
        colnames(dustM)[1:2] <- c('Year', 'Month')

        saveRDS(dustM, file = paste0(subDir, '/MCSReducedDust_', MCSYear[iyear], '_', imon, monConv[imon,2], '.rds'))

        MCSdustCuts <- c(seq(0, 0.1, 0.01), 10)
        MCSdustPAL <- magma(11)

        ggplot(dustM, aes(x = Latitude, y = Pressure, z = DustOpacity_Corr)) +
        geom_contour_filled(breaks = MCSdustCuts) + # , breaks = BREAK
        #geom_tile(aes(fill = dTau_dz_interp), height = 0.05) +
        ggtitle(paste0('MCS Dust ', MCSYear[iyear], mons[imon])) +
        scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.0001), breaks = c(1, 0.1, 0.01, 0.001, 0.0001), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
        scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
        scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
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
        ggsave(paste0(plotDir, '/', 'MCSDust_', MCSYear[iyear], '_', imon, monConv[imon,2], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)

        ggplot(dustM, aes(x = Latitude, y = Pressure, z = DustOpacity_Corr)) +
        geom_contour_filled(breaks = MCSdustCuts) + # , breaks = BREAK
        #geom_tile(aes(fill = dTau_dz_interp), height = 0.05) +
        ggtitle(paste0('MCS Dust ', MCSYear[iyear], mons[imon])) +
        scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), breaks = c(1, 0.1, 0.01, 0.001, 0.0001), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
        scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
        scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
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
        ggsave(paste0(plotDir, '/', 'MCSDust_Lower_', MCSYear[iyear], '_', imon, monConv[imon,2], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)

    }
}

## compare Model and MCS data ##



## testing ##
## for imod=1
mod <- interpOut
mod <- mod[,3:5]
colnames(mod) <- c('Latitude', 'Pressure', 'Dust')
mcs <- dustM
mcs <- mcs[, c(3,4,6)]
colnames(mcs) <- c('Latitude', 'Pressure', 'Dust')

## comp: model vs observation table w/ corrected obs ##
comp <- join(mod, mcs, by = c('Latitude', 'Pressure'))
colnames(comp)[3:4] <- c('ModelDust', 'MCSDust')
comp <- comp %>% mutate(Residual = ModelDust - MCSDust)
comp <- comp %>% mutate(ResidualPCT = (Residual / ModelDust) * 100)
lim <- round(max(abs(comp$Residual), na.rm = TRUE), digits = ceiling(abs(log10(max(abs(comp$Residual), na.rm = TRUE)))))
comp <- subset(comp, is.na(Residual) == FALSE)

## comp2: model vs observation table w/o corrected obs ##
mcs2 <- dustM
mcs2 <- mcs2[,3:5]
colnames(mcs2) <- c('Latitude', 'Pressure', 'Dust')
comp2 <- join(mod, mcs2, by = c('Latitude', 'Pressure'))
colnames(comp2)[3:4] <- c('ModelDust', 'MCSDust')
comp2 <- comp2 %>% mutate(Residual = ModelDust - MCSDust)
comp2 <- subset(comp2, is.na(Residual) == FALSE)
lim2 <- round(max(abs(comp2$Residual), na.rm = TRUE), digits = ceiling(abs(log10(max(abs(comp2$Residual), na.rm = TRUE)))))

panoplyPAL <- c('#050fd9', '#2050ff', '#4297ff', '#6dc1ff', '#86daff', '#9defff', '#aff6ff', '#cfffff', '#ffffff', '#ffff66', '#ffec00', '#ffc400', '#ff9000', '#ff4900', '#ff0000', '#d50000', '#9f0000')
names(panoplyPAL) <- seq((-1*lim), lim, (2*lim)/16)

## corrected Residuals ##
ggplot(comp, aes(x = Latitude, y = Pressure, z = Residual)) + 
#geom_contour_filled() + # , breaks = BREAK
geom_tile(aes(fill = Residual), height = 0.1) +
ggtitle(paste0('Residual (Model - MCS) 0030 JAN')) +
scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
scale_fill_gradient2(low = '#050fd9', mid = 'white', high = '#9f0000', midpoint = 0, limits = c((-1*lim), lim), n.breaks = 9) +
#scale_fill_manual(values = panoplyPAL) + # , drop = FALSE values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
guides(fill = guide_colorsteps(title.position = "right", title = 'Residual (km-1)')) + # , title = VAR
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
ggsave(paste0(plotDir, '/', 'ModelResiduals_Lower_0030_1Jan_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)

# comp$ResidualPCT[is.numeric(comp$ResidualPCT) == FALSE] <- NA
# comp$ResidualPCT[comp$ResidualPCT == -Inf] <- NA
# ggplot(comp, aes(x = Latitude, y = Pressure, z = ResidualPCT)) + 
# geom_contour_filled() + # , breaks = BREAK
# #geom_tile(aes(fill = dTau_dz_interp), height = 0.05) +
# ggtitle(paste0('Residual (Model - MCS) 0030 JAN')) +
# scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
# scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
# #scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
# guides(fill = guide_colorsteps(title.position = "right", title = 'Residual (km-1)')) + # , title = VAR
# theme(plot.title = element_text(hjust = 0.5, size = 21,
#     face = "bold"),
#     text = element_text(size = 18),
#     axis.text.x = element_text(size = 16),
#     aspect.ratio = 0.625,
#     axis.line = element_line(color = "black"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect('grey85'), #element_blank()
#     panel.border = element_rect(color = "black", fill=NA, size=2),
#     legend.title=element_text(angle = -90),
#     legend.key=element_blank(),
#     legend.key.height = unit(0.5, "inch"),
#     legend.title.align = 0.5,
#     plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
#     plot.tag.position = c(0.15, 0.02),
#     axis.title.y.right = element_text(margin = margin(l = 83)),
#     #legend.position = c(1.2, 0.5)
# )


## uncorrected residuals ##
ggplot(comp2, aes(x = Latitude, y = Pressure, z = Residual)) + 
#geom_contour_filled() + # , breaks = BREAK
geom_tile(aes(fill = Residual), height = 0.1) +
ggtitle(paste0('Residual (Model - MCS) 0030 JAN')) +
scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
scale_fill_gradient2(low = '#050fd9', mid = 'white', high = '#9f0000', midpoint = 0, limits = c((-1*lim2), lim2),, n.breaks = 9) + # ,  n.breaks = 9
#scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
guides(fill = guide_colorsteps(title.position = "right", title = 'Residual (km-1)')) + # , title = VAR
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
ggsave(paste0(plotDir, '/', 'ModelResiduals_Uncorr_Lower_0030_1Jan_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)

comp2 <- subset(comp2, ModelDust > 0)
comp2 <- comp2 %>% mutate(ResidualPCT = (Residual / ModelDust) * 100)
lim2 <- round(max(abs(comp2$ResidualPCT), na.rm = TRUE), digits = ceiling(abs(log10(max(abs(comp2$ResidualPCT), na.rm = TRUE))))) 

## uncorrected residuals PCT ##
ggplot(comp2, aes(x = Latitude, y = Pressure, z = ResidualPCT)) + 
#geom_contour_filled() + # , breaks = BREAK
geom_tile(aes(fill = ResidualPCT), height = 0.1) +
ggtitle(paste0('Residual (Model - MCS) 0030 JAN')) +
scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.1), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
scale_fill_gradient2(low = '#050fd9', mid = 'white', high = '#9f0000', midpoint = 0, limits = c((-1*lim2), lim2),, n.breaks = 9) + # ,  n.breaks = 9
#scale_fill_manual(values = dustPAL, drop = FALSE) + # values = dustPAL, limits = names(dustPAL), drop = FALSE limits = c(0, 0.01)
guides(fill = guide_colorsteps(title.position = "right", title = 'Residual (km-1)')) + # , title = VAR
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
ggsave(paste0(plotDir, '/', 'ModelResidualsPCT_Uncorr_Lower_0030_1Jan_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8.25, unit = 'in', dpi = 300)
