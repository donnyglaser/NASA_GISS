## run in dir with the topo_OC files ##

library(ncdf4)

allFiles <- list.files(getwd(), pattern = '_TOPO_OC_')

out <- data.frame()
for(ifile in 1:length(allFiles)) {
    nc <- nc_open(allFiles[ifile])

    zocean <- ncvar_get(nc, varid = 'zocean')
    colnames(zocean) <- seq(-90, 90, 4)
    row.names(zocean) <- seq(-177.5,177.5,5)
    zocean <- melt(zocean)
    colnames(zocean) <- c('Lon', 'Lat', 'zocean')

    planetR <- 6357 * 1000 # Earth radius in m

    zocean <- zocean %>% mutate(area = ((planetR^2) * (sin(abs(Lat * 0.0175)) - sin(abs(Lat * 0.0175) - 0.07)) * 0.0875)) # formula for grid area considering 4x5 res
    areaTot <- sum(zocean$area)
    zocean <- subset(zocean, zocean > 0)
    zocean <- zocean %>% mutate(volume = area * zocean)

    GEL <- sum(zocean$volume) / areaTot
    tempOut <- cbind(allFiles[ifile], GEL)
    out <- rbind(out, tempOut)
}