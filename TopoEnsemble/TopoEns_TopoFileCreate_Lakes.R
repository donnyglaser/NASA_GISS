## this has been tested and created topo files that run correctly ##
## use this just for the low water inventory runs (1 & 5%) ##
## instead of making ocean cells w/zocean depth, this makes lake cells w/hlake depth ##

library(ncdf4)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars')
#pctSurf <- readRDS('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars/OceanSurfaceAreaTable_wGEL_231214.rds')
pctSurf <- readRDS('OceanSurfaceAreaTable_wGEL_240125.rds')
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
topoName <- data.frame(i = 1:2, RotationAngle = c(0, 90))

#fileList <- c('mars_topo_trans0_paleo_C.nc', 'mars_topo_trans22_paleo_C.nc', 'mars_topo_trans45_paleo_C.nc', 'mars_topo_trans60_paleo_C.nc', 'mars_topo_trans90_paleo_C.nc')
fileList <- c('mars_topo_trans0_paleo_C.nc',  'mars_topo_trans90_paleo_C.nc') # only 0 and 90


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

    for(isurf in 2:3) {
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
                    focean[irow, icol] <- 0
                    fgice[irow, icol] <- 0
                    flake[irow, icol] <- 1
                    zocean[irow, icol] <- 0
                    zoc_t <- tempz - tTopo[irow, icol]
                    if(zoc_t < 591) {
                        hlake[irow, icol] <- 591
                    } else {
                        x <- which(abs(olayers[,2] - zoc_t) == min(abs(olayers[,2] - zoc_t)))
                        hlake[irow, icol] <- olayers[x,2]
                    }
                }
            }
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

        flake[,1] <- 0
        flake[,46] <- flake[36,46]

        hlake[,1] <- 0
        hlake[,46] <- hlake[36,46]

        
        file.copy('TOPOZ72X46N.TEMPLATE.nc', paste0('mars_TOPO_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_NPoleOc.nc'), overwrite = FALSE)
        template <- nc_open(paste0('mars_TOPO_trans_', topoName[ifile, 2], 'x', pctSurf[isurf, 1], '_NPoleOc.nc'), write = TRUE)

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
    }
}