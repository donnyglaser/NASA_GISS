## make plots of SummaryData and PlotData ##


library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(ggplot2) # package for plotting
library(reshape)

######### SUMMARY PLOTS #########

aijList <- list.files(getwd(), pattern = 'SummaryData_TopoEns_aij')
aij <- readRDS(aijList[length(aijList)])
oijList <- list.files(getwd(), pattern = 'SummaryData_TopoEns_oijl')
oijl <- readRDS(oijList[length(oijList)])

angList <- c('NA',0,22,45,60,90)
wcList <- c(0,1,5,10,20,30,40,50,100)

aijVar <- unique(aij$Diagnostic)
aijReg <- unique(aij$Region)

### AIJ PLOTS ###
for(ivar in 1:length(aijVar)) {
    varTab <- subset(aij, Diagnostic == aijVar[ivar])

    for(ireg in 1:length(aijReg)) {
        #### FIXED ANGLE ####
        regTab <- subset(varTab, Region == aijReg[ireg])
        #regTab$Angle <- factor(regTab$Angle, labels = c('0','22','45','60','90','NA'))
        regTab$Angle <- factor(regTab$Angle)


        ggplot() +
        geom_point(data = regTab, aes(x = WaterContent, y = Value, color = Angle, group = Angle), size = 3) +
        geom_line(data = regTab, aes(x = WaterContent, y = Value, color = Angle, group = Angle), size = 1) +
        xlab("Ocean Surface Area (%)") +
        ylab(aijVar[ivar]) +
        labs(title = aijReg[ireg]) +
        guides(color = 'legend') +
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
            #legend.position = c(1.07, 0.52), 
            panel.spacing = unit(5, "mm")
        )

        ggsave(paste0('TopoEns_Summary_', aijVar[ivar], '_', aijReg[ireg], '_fixAng_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)

        #### FIXED WATER CONTENT ####
        regTab <- subset(varTab, Region == aijReg[ireg] & Angle != 'NA')
        aquaTab <- subset(varTab, Region == aijReg[ireg] & Angle == 'NA')
        regTab$Angle <- as.numeric(regTab$Angle)
        #regTab$WaterContent <- factor(regTab$WaterContent, labels = c('0', '10', '20', '30', '40', '50', '100'))
        regTab$WaterContent <- factor(regTab$WaterContent)

        ggplot() +
        geom_point(data = regTab, aes(x = Angle, y = Value, color = WaterContent, group = WaterContent), size = 3) +
        geom_line(data = regTab, aes(x = Angle, y = Value, color = WaterContent, group = WaterContent), size = 1) +
        geom_hline(data = aquaTab, aes(yintercept = aquaTab$Value[1]), color = 'grey', size = 1, linetype = 'solid') +
        scale_x_continuous(expand = expansion(), limits = c(-5, 95), breaks = c(round(0, digits=0), 22.5, 45, 67.5, 90)) +
        xlab("TPW Angle (º)") +
        ylab(aijVar[ivar]) +
        labs(title = aijReg[ireg]) +
        guides(color = guide_legend(title = 'Water\nContent')) +
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
            #legend.position = c(1.07, 0.52), 
            panel.spacing = unit(5, "mm")
        )

        ggsave(paste0('TopoEns_Summary_', aijVar[ivar], '_', aijReg[ireg], '_fixWC_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)

    }
}

oijlVar <- unique(oijl$Diagnostic)
oijlReg <- unique(oijl$Region)

### OIJ PLOTS ###
for(ivar in 1:length(oijlVar)) {
    varTab <- subset(oijl, Diagnostic == oijlVar[ivar])

    for(ireg in 1:length(oijlReg)) {
        #### FIXED ANGLE ####
        regTab <- subset(varTab, Region == oijlReg[ireg])
        regTab$Angle <- factor(regTab$Angle) #, labels = c('0','22','45','60','90','NA')


        ggplot() +
        geom_point(data = regTab, aes(x = WaterContent, y = Value, color = Angle, group = Angle), size = 3) +
        geom_line(data = regTab, aes(x = WaterContent, y = Value, color = Angle, group = Angle), size = 1) +
        xlab("Ocean Surface Area (%)") +
        ylab(oijlVar[ivar]) +
        labs(title = oijlReg[ireg]) +
        guides(color = 'legend') +
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
            #legend.position = c(1.07, 0.52), 
            panel.spacing = unit(5, "mm")
        )

        ggsave(paste0('TopoEns_Summary_', oijlVar[ivar], '_', oijlReg[ireg], '_fixAng_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)

        #### FIXED WATER CONTENT ####
        regTab <- subset(varTab, Region == oijlReg[ireg] & Angle != 'NA')
        aquaTab <- subset(varTab, Region == oijlReg[ireg] & Angle == 'NA')
        regTab$Angle <- as.numeric(regTab$Angle)
        #regTab$WaterContent <- factor(regTab$WaterContent, labels = c('0', '10', '20', '30', '40', '50', '100'))
        regTab$WaterContent <- factor(regTab$WaterContent)

        ggplot() +
        geom_point(data = regTab, aes(x = Angle, y = Value, color = WaterContent, group = WaterContent), size = 3) +
        geom_line(data = regTab, aes(x = Angle, y = Value, color = WaterContent, group = WaterContent), size = 1) +
        geom_hline(data = aquaTab, aes(yintercept = aquaTab$Value[1]), color = 'grey', size = 1, linetype = 'dashed') +
        scale_x_continuous(expand = expansion(), limits = c(-5, 95), breaks = c(round(0, digits=0), 22.5, 45, 67.5, 90)) +
        xlab("TPW Angle (º)") +
        ylab(oijlVar[ivar]) +
        labs(title = oijlReg[ireg]) +
        guides(color = guide_legend(title = 'Water\nContent')) +
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
            #legend.position = c(1.07, 0.52), 
            panel.spacing = unit(5, "mm")
        )

        ggsave(paste0('TopoEns_Summary_', oijlVar[ivar], '_', oijlReg[ireg], '_fixWC_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)
    }
}
