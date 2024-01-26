

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Mars/MarsTopography/Paleo Mars')


xvals <- seq(-177.5, 177.5, 5)
yvals <- seq(-90, 90, 4)
nx <- length(xvals)
ny <- length(yvals)
lon <- ncdim_def('lon', 'deg', xvals)
lat <- ncdim_def('lat', 'deg', yvals)


tempz <- -4900 # 'sealevel' for 5% water coverage


orig <- nc_open('mars_topo_trans0_paleo_C.nc') ## open unshifted file 
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


for(irow in 1:72) {
    for(icol in 1:46) {
        if(tTopo[irow, icol] > tempz) { ## if: land
            zatmo[irow, icol] <- tTopo[irow, icol] - tempz
            fgrnd[irow, icol] <- 1
            focean[irow, icol] <- 0
            fgice[irow, icol] <- 0
            flake[irow, icol] <- 0
            hlake[irow, icol] <- 0
        } else { # if: lake
            zatmo[irow, icol] <- 0
            fgrnd[irow, icol] <- 0
            focean[irow, icol] <- 0
            fgice[irow, icol] <- 0
            flake[irow, icol] <- 1
            hlake[irow, icol] <- tempz - tTopo[irow, icol]
        }
    }
}

file.copy('TOPOZ72X46N.TEMPLATE.nc', paste0('mars_TOPO_trans_', 0, 'x', 5, '_Lakes.nc'), overwrite = FALSE)
template <- nc_open(paste0('mars_TOPO_trans_', 0, 'x', 5, '_Lakes.nc'), write = TRUE)

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

##### this works, but maybe still best to do ocean cells?
##### since at 5% there is only 3 basins that are pretty deep (> 1000m)
##### maybe make a code that identifies local depressions for lakes??