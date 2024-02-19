## this has been tested and created topo files that run correctly ##
## have yet to test the 0% surface area topo files (no ocean cells) ##

library(mgc)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars')
pctSurf <- readRDS('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars/OceanSurfaceAreaTable_wGEL_231214.rds')
load("/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars/OIC_AverageFile_231018.Rda") # loads as OIC_all


xvals <- seq(-177.5, 177.5, 5)
yvals <- seq(-90, 90, 4)
nx <- length(xvals)
ny <- length(yvals)
lon <- ncdim_def('lon', 'deg', xvals)
lat <- ncdim_def('lat', 'deg', yvals)

### TOPO_OC needs lato and lono dim labels ###
### g = 80000, uniform w/depth
### s = 35, uniform w/depth

lono <- ncdim_def('lono', 'deg', xvals, create_dimvar=TRUE)
lato <- ncdim_def('lato', 'deg', yvals, create_dimvar=TRUE)
olayers <- data.frame(Layer = 1:13, Bottom = c(12, 30, 57, 98, 158, 249, 386, 591, 899, 1360, 2052, 3090, 4647), Mid = c(6, 21, 44, 77, 128, 204, 318, 488, 745, 1129, 1706, 2571, 3868))
zoc <- ncdim_def('zoc', 'm', c(6, 21, 44, 77, 128, 204, 318, 488, 745, 1129, 1706, 2571, 3868))

fileList <- c('mars_topo_trans0_paleo_C.nc', 'mars_topo_trans22_paleo_C.nc', 'mars_topo_trans45_paleo_C.nc', 'mars_topo_trans60_paleo_C.nc', 'mars_topo_trans90_paleo_C.nc')

for(ifile in 1:length(fileList)) {
    orig <- nc_open(fileList[ifile]) ## open unshifted file 
    origTopo <- ncvar_get(orig, varid= 'topography')

    ### TOPO variables ###
    tTopo <- t(origTopo)
    zatmo <- t(origTopo)
    fgrnd <- t(origTopo)
    focean <- t(origTopo)
    fgice <- t(origTopo)
    flake <- t(origTopo)
    zocean <- t(origTopo)
    hlake <- t(origTopo)

    ### OIC variables ###
    ### 46x72x13 matrix for OIC file (lat x lon x ocean layer) ###
    g <- array(data = 0, dim = c(72, 46, 13)) # potential enthalpy
    gz <- array(data = 0, dim = c(72, 46, 13)) # potential enthalpy vertical gradient
    mo <- array(data = 0, dim = c(72, 46, 13)) # mass per layer
    s <- array(data = 0, dim = c(72, 46, 13)) # salinity
    sz <- array(data = 0, dim = c(72, 46, 13)) # salinity vertical gradient
    for(isurf in 1:nrow(pctSurf)) {
        tempz <- pctSurf[isurf, 2]

        for(irow in 1:72) {
            for(icol in 1:46) {
                if(tTopo[irow, icol] > tempz) { ## if: land
                    zatmo[irow, icol] <- tTopo[irow, icol] - tempz
                    fgrnd[irow, icol] <- 1
                    focean[irow, icol] <- 0
                    fgice[irow, icol] <- 0
                    flake[irow, icol] <- 0
                    zocean[irow, icol] <- 0
                    hlake[irow, icol] <- 0
                } else { # if: ocean
                    zatmo[irow, icol] <- 0
                    fgrnd[irow, icol] <- 0
                    focean[irow, icol] <- 1
                    fgice[irow, icol] <- 0
                    flake[irow, icol] <- 0
                    hlake[irow, icol] <- 0
                    zoc_t <- tempz - tTopo[irow, icol]
                    if(zoc_t < 591) {
                        zocean[irow, icol] <- 591
                    } else {
                        x <- which(abs(olayers[,2] - zoc_t) == min(abs(olayers[,2] - zoc_t)))
                        zocean[irow, icol] <- olayers[x,2]
                    }

                    for(ilayer in 1:13) {
                        if(focean[irow, icol] == 0) { ## if: land
                            g[irow, icol, ilayer] <- 0
                            gz[irow, icol, ilayer] <- 0
                            mo[irow, icol, ilayer] <- 0
                            s[irow, icol, ilayer] <- 0
                            sz[irow, icol, ilayer] <- 0
                        } else { ## if: ocean
                            if(zocean[irow, icol] >= olayers[ilayer,2]) { ## check if cell is as deep as the example Earth cell (only use values up to depth)
                                g[irow, icol, ilayer] <- 83140 # 20ºC
                                gz[irow, icol, ilayer] <- 0
                                mo[irow, icol, ilayer] <- OIC_all[ilayer, 3]
                                s[irow, icol, ilayer] <- 0.035 ## confused by units, OIC4X5LD.Z12.gas1.CLEV94.DEC01.nc labels units as psu, but the magnitude of values (0.035) indicate kg/kg
                                sz[irow, icol, ilayer] <- 0
                            } else {
                                g[irow, icol, ilayer] <- 0
                                gz[irow, icol, ilayer] <- 0
                                mo[irow, icol, ilayer] <- 0
                                s[irow, icol, ilayer] <- 0
                                sz[irow, icol, ilayer] <- 0
                            }
                        }
                    }
                }
            }
        }

        ## GLmelt loop: identify large oceans for GLmelt mask ##
        #library(mgc)

        melt <- ConnCompLabel(as.matrix(focean))
        #colnames(melt) <- seq(-90, 90, 4)
        #row.names(melt) <- seq(-177.5, 177.5, 5)
        melt <- melt(melt)
        colnames(melt) <- c('Lon', 'Lat', 'nOcean')
        melt <- subset(melt, nOcean > 0)
        melt <- melt[order(melt[,3]), ]
        GLmelt <- data.frame()
        for(iocn in 1:length(unique(melt$nOcean))) {
            tmelt <- subset(melt, nOcean == iocn)
            if(nrow(tmelt) > 15) {
                GLmelt <- rbind(GLmelt, tmelt)
            }
        }

        GL <- array(data = 0, dim = c(72, 46))
        for(igl in 1:nrow(GLmelt)) {
            GL[GLmelt[igl,1], GLmelt[igl,2]] <- 1
        }

        # fix pole issue: make all the polar cells the same and south pole = land
        if(zatmo[36,1] > 0) {
            zatmo[,1] <- zatmo[36,1]
        } else {
            zatmo[,1] <- 50
        }
        zatmo[,46] <- zatmo[36,46]

        fgrnd[,1] <- 1
        fgrnd[,46] <- fgrnd[36,46]

        focean[,1] <- 0
        focean[,46] <- focean[36,46]

        zocean[,1] <- 0
        zocean[,46] <- zocean[36,46]

        for(ilayer in 1:13) {
            g[,1,ilayer] <- 0
            g[,46,ilayer] <- g[36,46,ilayer]

            gz[,1,ilayer] <- 0
            gz[,46,ilayer] <- gz[36,46,ilayer]
            
            mo[,1,ilayer] <- 0
            mo[,46,ilayer] <- mo[36,46,ilayer]

            s[,1,ilayer] <- 0
            s[,46,ilayer] <- s[36,46,ilayer]

            sz[,1,ilayer] <- 0
            sz[,46,ilayer] <- sz[36,46,ilayer]
        }

        # if(isurf > 1) { # for runs with ocean, there needs to be an ocean cell at the N pole
        #     if(focean[1,46] == 0) {
        #         zatmo[1:72, 46] <- 0
        #         fgrnd[1:72, 46] <- 0
        #         focean[1:72, 46] <- 1
        #         fgice[1:72, 46] <- 0
        #         flake[1:72, 46] <- 0
        #         hlake[1:72, 46] <- 0
        #         zocean[1:72, 46] <- 591

        #         g[1:72, 46, 1:8] <- 83140 # 20ºC
        #         g[1:72, 46, 9:13] <- 0 # 20ºC
        #         gz[1:72, 46, 1:8] <- 0
        #         for(ilayer in 1:8) {
        #             mo[1:72, 46, ilayer] <- OIC_all[ilayer, 3]
        #         }
        #         mo[1:72, 46, 9:13] <- 0
        #         s[1:72, 46, 1:8] <- 0.035
        #         s[1:72, 46, 9:13] <- 0
        #         sz[1:72, 46, 1:8] <- 0
        #     }
        # }
        

        file.copy('TOPOZ72X46N.TEMPLATE.nc', paste0('mars_TOPO_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), overwrite = FALSE)
        template <- nc_open(paste0('mars_TOPO_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), write = TRUE)

        ## vars ##
        varfgice <- template[["var"]][["fgice"]]
        varfgrnd <- template[["var"]][["fgrnd"]]
        varflake <- template[["var"]][["flake"]]
        varfocean <- template[["var"]][["focean"]]
        varhlake <- template[["var"]][["hlake"]]
        varzatmo <- template[["var"]][["zatmo"]]
        ncvar_put(template, varzatmo, zatmo)
        ncvar_put(template, varfgrnd, fgrnd)
        ncvar_put(template, varfocean, focean)
        ncvar_put(template, varfgice, fgice)
        ncvar_put(template, varflake, flake)
        ncvar_put(template, varhlake, hlake)
        nc_close(template)

        if(isurf > 1) {
            file.copy('TOPO_OCZ72X46N.TEMPLATE.nc', paste0('mars_TOPO_OC_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), overwrite = FALSE)
            template <- nc_open(paste0('mars_TOPO_OC_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), write = TRUE)

            ## vars ##
            varo_zatmo <- template[["var"]][["zatmo"]]
            varo_focean <- template[["var"]][["focean"]]
            var_zocean <- template[["var"]][["zocean"]]
            ## add data ##
            ncvar_put(template, varo_zatmo, zatmo)
            ncvar_put(template, varo_focean, focean)
            ncvar_put(template, var_zocean, zocean)
            nc_close(template)

            file.copy('OIC4X5LD.TEMPLATE.nc', paste0('mars_OIC_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), overwrite = FALSE)
            template <- nc_open(paste0('mars_OIC_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), write = TRUE)

            ## vars ##
            varg <- template[["var"]][["g"]]
            vargz <- template[["var"]][["gz"]]
            varmo <- template[["var"]][["mo"]]
            vars <- template[["var"]][["s"]]
            varsz <- template[["var"]][["sz"]]
            ## add data ##
            ncvar_put(template, varg, g)
            ncvar_put(template, vargz, gz)
            ncvar_put(template, varmo, mo)
            ncvar_put(template, vars, s)
            ncvar_put(template, varsz, sz)
            nc_close(template)

            file.copy('GLMELT_4X5.nomelt.nc', paste0('mars_GLmelt_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), overwrite = FALSE)
            template <- nc_open(paste0('mars_GLmelt_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_paleo.nc'), write = TRUE)

            ## vars ##
            varmask <- template[["var"]][["mask"]]
            ## add data ##
            ncvar_put(template, varmask, GL)
            nc_close(template)
        }
    }
}