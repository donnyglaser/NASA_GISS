## take the output from the single run eqAnalysis, ##
## extract statistics and analysis for the entire ensemble ##

## Libraries ##
library(ncdf4) 
library(tidyverse)
library(reshape)

runList <- list.files(getwd(), pattern = glob2rx('TopoEns_*.rds'))
diags <- c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')


for(irun in 1:length(runList)) {
    runName <- runList[irun]
    tempDat <- readRDS(runName)
    tempDat$Diagnostic <- factor(tempDat$Diagnostic, levels = diags)
    tempDat <- subset(tempDat, Year != "eq50")
    tempDat$Year <- as.numeric(tempDat$Year)
    tempDat$Value <- as.numeric(tempDat$Value)
    yearMax <- max(tempDat$Year)
    yearMin <- yearMax - 50

    eqSub <- subset(tempDat, Year >= yearMin)
    for(idiag in 1:length(diags)) {
        diagDat <- subset(eqSub, Diagnostic == diags[idiag])

        modelStat <- lm(Value ~ Year, data = diagDat)
        modelOut <- summary(modelStat)
        reg <- c(modelOut$coefficients[2,1], modelOut$coefficients[1,1])
        mean <- mean(diagDat$Value)
        diagDat <- diagDat %>% mutate(ModelValue = (Year * reg[1]) + reg[2])
        diagDat <- diagDat %>% mutate(ModelResidual = ModelValue - Value)
        diagDat <- diagDat %>% mutate(Mean = mean)
        diagDat <- diagDat %>% mutate(MeanResidual = Value - Mean)
        meanStat <- lm(Mean ~ Year, data = diagDat)
        meanOut <- summary(meanStat)
        modComp <- c(modelOut$coefficients[2,1], modelOut$coefficients[2,2], meanOut$coefficients[2,1], meanOut$coefficients[2,2]) ### https://www.jstor.org/stable/2782277?seq=1#page_scan_tab_contents
        modCompOut <- (modComp[1] - modComp[3]) / sqrt((modComp[2]^2) + (modComp[4]^2))        
        modCompOut <- 2 * pnorm(-abs(modCompOut))  ## this is the p value to compare the line of the mean compared to the linear regression line: p_value = 2*pnorm(-abs(compare.coeff(b1,se1,b2,se2)))

        ## here, make an if statement to add an asterisk to plot if modCompOut is >0.05 #

        ggplot(diagDat) +
        geom_point(aes(x = Year, y = Value)) +
        geom_hline(aes(yintercept = mean)) + ## mean of the proposed eq data ##
        geom_abline(aes(intercept = reg[2], slope = reg[1]), linetype = 'dashed') + ## linear regression of the data ##
        ## now we want to test if the two lines are statistically similar (pvalue of F statistic??) ##
    }
}