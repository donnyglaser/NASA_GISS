rmOcTab <- data.frame(lon = c(4,4,4,4,4,3,5,20,20,20,20,21,21,21), lat = c(32,33,34,35,36,35,34,31,32,33,34,31,32,33))


file.copy('mars_TOPO_trans_0x10_paleo_C.nc', 'mars_TOPO_trans_0x10_rmSmallOcean2.nc', overwrite = FALSE)
template <- nc_open('mars_TOPO_trans_0x10_rmSmallOcean2.nc', write = TRUE)

#TOPO#
varList <- data.frame(var = c('fgrnd', 'focean', 'zatmo'), val = c(1, 0, 50))
for(i in 1:nrow(varList)) {
  tempTab <- ncvar_get(template, varid= varList[i,1])
  for(j in 1:nrow(rmOcTab)) {
    tempTab[rmOcTab[j,1], rmOcTab[j,2]] <- varList[i,2]
    
    tempID <- template[["var"]][[varList[i,1]]]
    ncvar_put(template, tempID, tempTab)
  }
}

file.copy('mars_TOPO_OC_trans_0x10_paleo_C.nc', 'mars_TOPO_OC_trans_0x10_rmSmallOcean2.nc', overwrite = FALSE)
template <- nc_open('mars_TOPO_OC_trans_0x10_rmSmallOcean2.nc', write = TRUE)




#TOPO_OC#
varList <- data.frame(var = c('focean', 'zocean', 'zatmo'), val = c(0, 0, 50))
for(i in 1:nrow(varList)) {
  tempTab <- ncvar_get(template, varid= varList[i,1])
  for(j in 1:nrow(rmOcTab)) {
    tempTab[rmOcTab[j,1], rmOcTab[j,2]] <- varList[i,2]
    
    tempID <- template[["var"]][[varList[i,1]]]
    ncvar_put(template, tempID, tempTab)
  }
}



file.copy('mars_OIC_trans_0x10_paleo_C.nc', 'mars_OIC_trans_0x10_rmSmallOcean2.nc', overwrite = FALSE)
template <- nc_open('mars_OIC_trans_0x10_rmSmallOcean2.nc', write = TRUE)

#OIC#
varList <- c('g', 'gz', 'mo', 's', 'sz')
for(i in 1:length(varList)) {
  tempTab <- ncvar_get(template, varid= varList[i])
  for(j in 1:nrow(rmOcTab)) {
    tempTab[rmOcTab[j,1], rmOcTab[j,2], 1:13] <- 0
    
    tempID <- template[["var"]][[varList[i]]]
    ncvar_put(template, tempID, tempTab)
  }
}