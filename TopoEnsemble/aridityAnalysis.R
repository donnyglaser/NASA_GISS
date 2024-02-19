
aijList <- list.files(getwd(), pattern = 'SummaryData_TopoEns_aij')
aij <- readRDS(aijList[length(aijList)])
aridDat <- subset(aij, Diagnostic == 'AridityIndex' & Angle != 'NA')

angList <- unique(aridDat$Angle)

WCList <- c(20, 30, 40, 50)
globalArea <- 5.097581e+14

aridOut <- data.frame()
for(iWC in 1:length(WCList)) {
    wcDat <- subset(aridDat, WaterContent == WCList[iWC])

    for(iang in 1:length(angList)) {
        angTab <- subset(wcDat, Angle == angList[iang])

        ts <- (subset(angTab, Region == 'PctSemiaridAridHyper')[1,7] / subset(angTab, Region == 'AllLand')[1,7]) * 100
        tsg <- (subset(angTab, Region == 'PctSemiaridAridHyper')[1,7] / globalArea) * 100
        ta <- (subset(angTab, Region == 'PctAridHyper')[1,7] / subset(angTab, Region == 'AllLand')[1,7]) * 100
        tag <- (subset(angTab, Region == 'PctAridHyper')[1,7] / globalArea) * 100
        th <- (subset(angTab, Region == 'PctHyperarid')[1,7] / subset(angTab, Region == 'AllLand')[1,7]) * 100
        thg <- (subset(angTab, Region == 'PctHyperarid')[1,7] / globalArea) * 100
        thum <- (subset(angTab, Region == 'PctHumid')[1,7] / subset(angTab, Region == 'AllLand')[1,7]) * 100
        thumg <- (subset(angTab, Region == 'PctHumid')[1,7] / globalArea) * 100

        temp <- cbind(angList[iang], WCList[iWC], ts, tsg, ta, tag, th, thg, thum, thumg)
        colnames(temp) <- c('Angle', 'WaterContent', 'LandSemiaridAridHyper', 'GlobalSemiaridAridHyper', 'LandAridHyper', 'GlobalAridHyper', 'LandHyperarid', 'GlobalHyperarid', 'LandHumid', 'GlobalHumid')

        aridOut <- rbind(aridOut, temp)
    }
    
}
aridOut[,1:10] <- apply(aridOut[,1:10], 2, as.numeric)

aridOut <- aridOut %>% mutate(LandSemiarid = LandSemiaridAridHyper - LandAridHyper)
aridOut <- aridOut %>% mutate(GlobalSemiarid = GlobalSemiaridAridHyper - GlobalAridHyper)
aridOut <- aridOut %>% mutate(LandArid = LandAridHyper - LandHyperarid)
aridOut <- aridOut %>% mutate(GlobalArid = GlobalAridHyper - GlobalHyperarid)


pal <- c('red3', 'orange3', 'khaki3', 'palegreen3')
#pal <- c('#CC0000', '#FF9966', '#FFCC66', '#669900')
names(pal) <- c('LandHyperarid', 'LandArid', 'LandSemiarid', 'LandHumid')

for(iWC in 1:length(WCList)) {
    tPlot <- subset(aridOut, WaterContent == WCList[iWC])
    tPlot <- tPlot[,c(1:2,7,9,11,13)]
    tPlot <- melt(tPlot, id = c('Angle', 'WaterContent'))

    ggplot() +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    xlab("TPW Angle (º)") +
    ylab('Percent Land Area') +
    scale_x_continuous(expand = expansion(), limits = c(-5, 95), breaks = seq(0,90, 30)) +
    scale_y_continuous(expand = expansion(), limits = c(0,60), breaks = seq(0,60,20)) +
    labs(title = paste0(WCList[iWC],'% Surface Water')) +
    scale_color_manual(name = 'Aridity Index', limits = names(pal), values = pal, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Humid')) +
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

    ggsave(paste0('AridityIndexPlot_', WCList[iWC], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)

    ###########
    # with Earth references #
    ###########

    ggplot() +
    geom_hline(aes(yintercept = 8), color = pal[1], size = 1, linetype = 'dashed') +
    geom_hline(aes(yintercept = 12), color = pal[2], size = 1, linetype = 'dashed') +
    geom_hline(aes(yintercept = 18), color = pal[3], size = 1, linetype = 'dashed') +
    geom_hline(aes(yintercept = 52), color = pal[4], size = 1, linetype = 'dashed') +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
    xlab("TPW Angle (º)") +
    ylab('Percent Land Area') +
    scale_x_continuous(expand = expansion(), limits = c(-5, 95), breaks = seq(0,90, 30)) +
    scale_y_continuous(expand = expansion(), limits = c(0,60), breaks = seq(0,60,20)) +
    labs(title = paste0(WCList[iWC],'% Surface Water')) +
    scale_color_manual(name = 'Aridity Index', limits = names(pal), values = pal, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Humid')) +
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

    ggsave(paste0('AridityIndexPlot_wEarth_', WCList[iWC], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)

}


#####################################
# plots by water #
tPlot <- subset(aridOut, Angle == 0)
tPlot <- tPlot[,c(1:2,7,9,11,13)]
tPlot <- melt(tPlot, id = c('Angle', 'WaterContent'))

ggplot() +
geom_point(aes(x = 71, y = 8), color = pal[1], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 12), color = pal[2], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 18), color = pal[3], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 52), color = pal[4], size = 5, shape = 18) + #Earth water == 71%
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
xlab("Surface Water Area (%)") +
ylab('Land Area (%)') +
scale_x_continuous(expand = expansion(), limits = c(10,80), breaks = seq(10, 80, 10)) +
scale_y_continuous(expand = expansion(), limits = c(0,60), breaks = seq(0,60,20)) +
#labs(title = paste0(WCList[iWC],'% Surface Water')) +
scale_color_manual(name = 'Aridity Index', limits = names(pal), values = pal, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Humid')) +
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

#ggsave(paste0('AridityIndexPlot_wEarth_fixAngle_', WCList[iWC], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)





#####################################
# plots by global area #
tPlot <- subset(aridOut, Angle == 0)
tPlot <- tPlot[,c(1:2,8,10,12,14)]
tPlot <- melt(tPlot, id = c('Angle', 'WaterContent'))

pal <- c('red3', 'orange3', 'khaki3', 'palegreen3')
#pal <- c('#CC0000', '#FF9966', '#FFCC66', '#669900')
names(pal) <- c('GlobalHyperarid', 'GlobalArid', 'GlobalSemiarid', 'GlobalHumid')
#vs water content
ggplot() +
geom_point(aes(x = 71, y = 2), color = pal[1], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 4), color = pal[2], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 5), color = pal[3], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 16), color = pal[4], size = 5, shape = 18) + #Earth water == 71%
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 3) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = WaterContent, y = value, color = variable), size = 1) +
xlab("Surface Water Area (%)") +
ylab('Global Area (%)') +
annotate(geom = 'text', x = 71, y = 20, label = 'Earth', size = 5, color = 'grey10') +
scale_x_continuous(expand = expansion(), limits = c(10,80), breaks = seq(10, 80, 10)) +
scale_y_continuous(expand = expansion(), limits = c(0,50), breaks = seq(0,50,10)) +
#labs(title = paste0(WCList[iWC],'% Surface Water')) +
scale_color_manual(name = 'Aridity Index', limits = names(pal), values = pal, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Humid')) +
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

ggsave(paste0('AridityIndexPlot_Global_wEarth_fixAngle0_', format(Sys.time(), "%y%m%d"), ".png"), height = 4.5, width = 8, unit = 'in', dpi = 300)

tPlot <- subset(aridOut, WaterContent == 20)
tPlot <- tPlot[,c(1:2,8,10,12,14)]
tPlot <- melt(tPlot, id = c('Angle', 'WaterContent'))

#vs angle
ggplot() +
geom_point(aes(x = 71, y = 2), color = pal[1], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 4), color = pal[2], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 5), color = pal[3], size = 5, shape = 18) + #Earth water == 71%
geom_point(aes(x = 71, y = 16), color = pal[4], size = 5, shape = 18) + #Earth water == 71%
geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
geom_point(data = tPlot, aes(x = Angle, y = value, color = variable), size = 3) +
geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
geom_line(data = tPlot, aes(x = Angle, y = value, color = variable), size = 1) +
xlab("Surface Water Area (%)") +
ylab('Global Area (%)') +
scale_x_continuous(expand = expansion(), limits = c(-5,95), breaks = seq(10, 80, 10)) +
scale_y_continuous(expand = expansion(), limits = c(0,60), breaks = seq(0,60,20)) +
#labs(title = paste0(WCList[iWC],'% Surface Water')) +
scale_color_manual(name = 'Aridity Index', limits = names(pal), values = pal, labels = c('Hyperarid', 'Arid', 'Semiarid', 'Humid')) +
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

#ggsave(paste0('AridityIndexPlot_wEarth_fixAngle_', WCList[iWC], '_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8, unit = 'in', dpi = 300)
