## take the output from the single run eqAnalysis, ##
## extract statistics and analysis for the entire ensemble ##

## Libraries ##
library(ncdf4) 
library(tidyverse)
library(reshape)
library(dplyr)

setwd('/Users/dmglaser/Documents/Research/NASA/ROCKE3DModel/Projects/Topo Ensemble/Data/AllEquilibriumData/EQ Timeseries')

runList <- list.files(getwd(), pattern = glob2rx('TopoEns_*.rds'))
diags <- c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')

labs <- c('Temperature (ºC)', 'Water Vapor Column (kg/m^2)', 'Net Planetary Radiation (W/m^2)', 'Earth Water (kg/m^2)', 'Earth Ice (kg/m^2)', 'Precipitation (mm/day)', 'Potential Evaporation (mm/day)', 'Ocean/Lake Ice Thickness (m)', 'Snow Depth (mm H2O)')
names(labs) <- c('tsurf_hemis', 'qatm_hemis', 'net_rad_planet_hemis', 'gwtr_hemis', 'gice_hemis', 'prec_hemis', 'pot_evap_hemis', 'ZSI_hemis', 'snowdp_hemis')

dirName <- paste0('eqStats_', format(Sys.time(), "%y%m%d"))
dir.create(dirName)
dir.create(paste0(dirName, '/Plots'))

dataOut <- data.frame()
for(irun in 1:length(runList)) {
    runName <- runList[irun]
    tempDat <- readRDS(runName)
    tempDat$Diagnostic <- factor(tempDat$Diagnostic, levels = diags)
    tempDat <- subset(tempDat, Year != "eq50")
    tempDat$Year <- as.numeric(tempDat$Year)
    tempDat$Value <- as.numeric(tempDat$Value)
    yearMax <- max(tempDat$Year)
    yearMin <- yearMax - 50
    runName <- substr(runName,9,nchar(runName)-3)
    runName <- strsplit(runName, split='_')
    runName <- runName[[1]][1]

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
        modComp <- c(modelOut$coefficients[2,1], modelOut$coefficients[2,2], meanOut$coefficients[2,1], meanOut$coefficients[2,2]) ### https://stats.stackexchange.com/questions/435644/is-there-a-method-to-look-for-significant-difference-between-two-linear-regressi  &  https://www.jstor.org/stable/2782277?seq=1#page_scan_tab_contents
        modCompOut <- (modComp[1] - modComp[3]) / sqrt((modComp[2]^2) + (modComp[4]^2))   #      
        modCompOut <- 2 * pnorm(-abs(modCompOut))  ## this is the p value to compare the line of the mean compared to the linear regression line: p_value = 2*pnorm(-abs(compare.coeff(b1,se1,b2,se2)))
        diagName <- unlist(strsplit(diags[idiag], split='_'))
        diagName <- diagName[-length(diagName)]
        diagName <- paste(diagName, collapse='')

        sig <- ""
        if(modCompOut < 0.05 & modelOut$coefficients[2,4] < 0.05) { #  
            sig <- "*"
        }

        modSum <- c(modelOut$coefficients[2,1], modelOut$coefficients[1,1], min(diagDat$Year))
        modSum <- c(modSum, ((modSum[1]*modSum[3]) + modSum[2]))

        tempOut <- cbind(runName, diags[idiag], mean, sd(diagDat$Value), sd(diagDat$Value)/sqrt(length(diagDat$Value)), length(diagDat$Value),
            modSum[1], modSum[2], modelOut$coefficients[2,4], modSum[3], modSum[4], modCompOut, sig)
        dataOut <- rbind(dataOut, tempOut)

        axOrder <- 10^round_any(log10(max(diagDat$Value)), 1, f = floor)
        plotLim <- c((min(diagDat$Year) - 2), (max(diagDat$Year) + 2), round_any(min(diagDat$Value), axOrder/20, f = floor), round_any(max(diagDat$Value), axOrder/20, f = ceiling))

        ggplot(diagDat) +
        geom_point(aes(x = Year, y = Value), size = 2) +
        geom_hline(aes(yintercept = mean), size = 1) + ## mean of the proposed eq data ##
        geom_abline(aes(intercept = reg[2], slope = reg[1]), linetype = 'dashed', size = 1) + ## linear regression of the data ##
        annotate(geom = 'text', label = sig,
            x = plotLim[1] + (1 * (plotLim[2] - plotLim[1])),
            y = plotLim[3] + (0.94 * (plotLim[4] - plotLim[3])), fontface = 'bold', size = 14) +
        scale_x_continuous(limits = c(plotLim[1], plotLim[2]), breaks = seq(min(diagDat$Year), max(diagDat$Year), 10)) + # expand = expansion(mult = 0.05), 
        scale_y_continuous(n.breaks = 4, limits = c(plotLim[3], plotLim[4])) + # expand = expansion(0, mult = 0.3), , breaks = waiver(),  expand = expansion(), limits = c(0, maxY), 
        xlab("Model Year") + 
        ylab(labs[idiag]) +
        ggtitle(runName) +
        theme(
            plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
            text = element_text(size = 18), 
            axis.text.x = element_text(size = 16),
            aspect.ratio = 0.625, 
            axis.line = element_line(color = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
            legend.key=element_blank(), 
            legend.key.height = unit(1.3, 'cm'), 
            plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
            plot.tag.position = c(0.15, 0.02), 
            axis.title.y.right = element_text(margin = margin(l = 83)),
            strip.background =element_rect(fill="white"),
            #legend.position = c(1.07, 0.52), 
            panel.spacing = unit(5, "mm")
        )
        ggsave(paste0(dirName, '/Plots/', runName, '_', diagName, '_eqStats_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 7.5, unit = 'in', dpi = 300)
    }
}

colnames(dataOut) <- c('RunName', 'Diagnostic', 'MeanValue', 'StdDeviation', 'StdError',
    'n', 'RegSlope', 'RegIntercept', 'RegSlopePValue', 'StYear', 'Reg@StYear', 'Mean-RegCompPValue', 'Significant')



## test significance using tsurf, net_rad_planet, prec, and pot_evap ##
compSub <- subset(dataOut, Diagnostic == 'tsurf_hemis' | Diagnostic == 'net_rad_planet_hemis' | Diagnostic == 'prec_hemis' | Diagnostic == 'pot_evap_hemis')

runName2 <- unique(dataOut$RunName)

finishOut <- data.frame()
for(irun in 1:length(runName2)) {
    temp <- subset(compSub, RunName == runName2[irun])
    temp <- temp[,c(1,13)]
    sig2 <- sum(temp$Significant == '*')

    fin <- 'N'
    if(sig2 == 0) {
        fin <- 'Y'
    }
    tFin <- cbind(runName2[irun], fin)
    finishOut <- rbind(finishOut, tFin)
}

colnames(finishOut) <- c('RunName', 'RunFinished')

saveRDS(finishOut, file = paste0(dirName, '/EquilibrimSummary_', format(Sys.time(), "%y%m%d"), ".rds"))

dataOut <- merge(dataOut, finishOut, by = 'RunName', all.x = TRUE)

runNameMatch <- data.frame(RunName = c("0x0","0x1","0x10","0x20","0x30","0x40","0x5","0x50","90x0","90x1","90x10","90x20","90x30","90x40","90x5","90x50","aqua"), 
    Angle = c(0,0,0,0,0,0,0,0,90,90,90,90,90,90,90,90,'NA'), WC = c(0,1,10,20,30,40,5,50,0,1,10,20,30,40,5,50,100))

dataOut <- merge(dataOut, runNameMatch, by = 'RunName', all.x = TRUE)
dataOut$Angle <- as.numeric(dataOut$Angle)
dataOut$WC <- as.numeric(dataOut$WC)
dataOut[3:12] <- sapply(dataOut[3:12], as.numeric)



saveRDS(dataOut, file = paste0(dirName, '/EquilibrimStats_', format(Sys.time(), "%y%m%d"), ".rds"))


### scatterplots of variables vs water content ###


for(idiag in 1:length(diags)) {
    tDiag <- subset(dataOut, Diagnostic == diags[idiag])
    tDiag$Angle <- factor(tDiag$Angle, levels = c(0, 90))
    tComp <- subset(tDiag, WC < 1 | WC >= 100)

    ### TEMPORARY ###
    tDiag <- subset(tDiag, WC > 1 & WC < 100)

    ggplot(data = tDiag, aes(x = WC, y = MeanValue, group = Angle, color = Angle)) +
    geom_hline(aes(yintercept = 0), size = 1, color = 'grey40', linetype = 'dotted') +
    geom_point(size = 2) +
    geom_line(size = 1) +
    #geom_point(data = tComp, aes(x = WC, y = MeanValue), color = 'black') +
    #facet_wrap(~Diagnostic, ncol = 1, strip.position = 'top', labeller = labeller(Diagnostic = climLab), scales = 'free') + # 
    scale_x_continuous(expand = expansion(mult = 0.05)) + # , limits = , breaks = 
    scale_y_continuous(n.breaks = 5, expand = expansion(mult=c(0.4, 0.4))) + # expand = expansion(0, mult = 0.3), , breaks = waiver(),  expand = expansion(), limits = c(0, maxY), 
    xlab("Water Surface Area (%)") + 
    ylab(labs[idiag]) +
    ggtitle(NULL) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold"), 
        text = element_text(size = 18), 
        axis.text.x = element_text(size = 16),
        aspect.ratio = 0.625, 
        axis.line = element_line(color = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, linewidth=2), 
        legend.key=element_blank(), 
        legend.key.height = unit(1.3, 'cm'), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.tag.position = c(0.15, 0.02), 
        axis.title.y.right = element_text(margin = margin(l = 83)),
        strip.background =element_rect(fill="white"),
        #legend.position = c(1.07, 0.52), 
        panel.spacing = unit(5, "mm")
    )
    ggsave(paste0('TsurfvWCbyAngle_', format(Sys.time(), "%y%m%d"), ".png"), height = 6, width = 8.5, unit = 'in', dpi = 300)
}
