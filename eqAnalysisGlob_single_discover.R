## takes annual files, and extracts target diagnostics for analysis ##
## run this after runSumScale.sh ##
## run this in the ANN folder ##


## Libraries ##
library(ncdf4) 
library(tidyverse)
library(reshape)

aij <- list.files(getwd(), pattern = '.aij')
aij <- aij[!grepl('-', aij)]

runName <- aij
runName <- runName[!grepl('aijk|aijl|taij', runName)]
runName <- runName[1]
runName <- substr(runName,9,nchar(runName)-3)
runName <- gsub('aij', '', runName)

diags <- c('tsurf_hemis', 'net_rad_planet_hemis', 'qatm_hemis', 'pcldt_hemis', 'prsurf_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')

nifile <- length(aij)
nidiag <- length(diags)

dataOut <- data.frame(matrix(NA, nrow = ((nifile*nidiag)), ncol = 4))
for(ifile in 1:nifile) {
    nc <- nc_open(aij[ifile])

    for(idiag in 1:nidiag) {
        tDiag <- ncvar_get(nc, varid = diags[idiag])
        tDiag <- tDiag[3]

        dataOut[(((ifile - 1)*nidiag) + idiag),] <- cbind(runName, ifile, diags[idiag], tDiag)
    }
    nc_close(nc)
    print(paste0(ifile, '/', nifile)) ## to see progress
}
colnames(dataOut) <- c('RunName', 'Year', 'Diagnostic', 'Value')

eqNC <- list.files(getwd(), pattern = 'aij')
eqNC <- eqNC[grepl('-', eqNC)]
eqNC <- eqNC[length(eqNC)]

nc <- nc_open(eqNC)
dataOut2 <- data.frame(matrix(NA, nrow = (nidiag), ncol = 4))
for(idiag in 1:nidiag) {
    tDiag <- ncvar_get(nc, varid = diags[idiag])
    tDiag <- tDiag[3]

    tOut <- cbind(runName, 'eq50', diags[idiag], tDiag)
    colnames(tOut) <- c('RunName', 'Year', 'Diagnostic', 'Value')
    dataOut2 <- rbind(dataOut, tOut)
}
nc_close(nc)

dataOut <- rbind(dataOut, dataOut2)

saveName <- paste0(runName, '_EqGlobal_', format(Sys.time(), "%y%m%d"))
saveRDS(dataOut, file = paste0(saveName, '.rds'))

dir.create('/gpfsm/dnb05/projects/p54/users/dmglaser/model_out/EQTopoEns')
file.copy(paste0(saveName, '.rds'), '/discover/nobackup/projects/giss_ana/users/dmglaser/model_out/EQTopoEns', overwrite=TRUE)

