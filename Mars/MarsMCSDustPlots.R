library(tidyverse)
library(scales)


MCSList <- list.files(getwd(), pattern = 'MCS Zonal Mean')
subDir <- paste0('Plots_', format(Sys.time(), "%y%m%d"))
dir.create(subDir)


varList <- seq(2,48,2)
TList <- seq(2,48,4)
TBreaks <- c(-300, seq(-140, -60, 10), Inf)
DBreaks <- c(0.00000001, 0.0001, seq(0.0005, 0.004, 0.0005), 0.01)
panoplyPAL <- c('#050fd9', '#2050ff', '#4297ff', '#6dc1ff', '#86daff', '#9defff', '#aff6ff', '#cfffff', '#ffffff', '#ffff66', '#ffec00', '#ffc400', '#ff9000', '#ff4900', '#ff0000', '#d50000', '#9f0000')
names(panoplyPAL) <- TBreaks
dList <- seq(4,48,4)
dBreaks <- 
latList <- seq(-89,89,2)
monRev <- c('Ls0-30'=1, 'Ls30-60'=2, 'Ls60-90'=3, 'Ls90-120'=4, 'Ls120-150'=5, 'Ls150-180'=6, 'Ls180-210'=7, 'Ls210-240'=8, 'Ls240-270'=9, 'Ls270-300'=10, 'Ls300-330'=11, 'Ls330-360'=12)


dataOut <- data.frame()
for(ifile in 1:length(MCSList)) {
    nc <- nc_open(MCSList[ifile])
    pList <- ncvar_get(nc, varid = "P")

    YR <- unlist(str_split(MCSList[ifile], pattern = '_'))
    YR <- YR[grep('MY', YR)]
    YR <- unlist(str_split(YR, '.nc'))[1]
    for(ivar in 1:length(varList)) {
        vID <- nc[['var']][[varList[ivar]]]$name
        temp <- ncvar_get(nc, varid = vID)
        
        colnames(temp) <- pList
        row.names(temp) <- latList
        tempm <- melt(temp)

        
        if(varList[ivar] %in% TList) {
            varN <- 'Temp'
            VAR <- 'Temperature (ºC)'
            tempm <- tempm %>% mutate(value = value - 273.15)
            PAL <- viridis(10)
            BREAK <- TBreaks
        } else {
            varN <- 'Dust'
            VAR <- expression("Dust Opacity (" * km^-1 * ")")
            PAL <- magma(10)
            BREAK <- DBreaks
        }
        
        MON <- unlist(str_split(vID, pattern = '_'))
        MON <- MON[grep('Ls', MON)]
        MONNUM <- revalue(MON, monRev)
        tempm <- cbind(tempm[,1:2], YR, MON, MONNUM, varN, tempm[,3])
        colnames(tempm) <- c('Lat', 'P', 'Year', 'Ls', 'MonthNumber', 'Variable', 'Value')
        tempm <- tempm %>% mutate(MonthNumber = revalue(MonthNumber, monRev))
        tempm <- tempm %>% mutate(P = P / 100)
        dataOut <- rbind(dataOut, tempm)


        ggplot(tempm, aes(x = Lat, y = P)) +
        geom_contour_filled(aes(x = Lat, y = P, z = Value), breaks = BREAK) + #
        #geom_tile(aes(fill = ValueCut)) +
        ggtitle(paste0(YR, ' ', MON)) +
        scale_y_continuous(name = 'Pressure (mB)', trans = c("log10", "reverse"), expand = expansion(), limits = c(12, 0.0001), breaks = c(1, 0.1, 0.01, 0.001, 0.0001), labels = function(x) sprintf("%g", x)) + #, labels = label_number()
        scale_x_continuous(name = 'Latitude', expand = expansion(), limits = c(-90,90), breaks = seq(-90,90,30)) +
        scale_fill_manual(values = PAL, drop = FALSE) +
        guides(fill = guide_colorsteps(title.position = "right", title = VAR)) +
        theme(plot.title = element_text(hjust = 0.5, size = 21,
            face = "bold"),
            text = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            aspect.ratio = 0.625,
            axis.line = element_line(color = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect('grey85'), #element_blank()
            panel.border = element_rect(color = "black", fill=NA, size=2),
            legend.title=element_text(angle = -90),
            legend.key=element_blank(),
            legend.key.height = unit(0.5, "inch"),
            legend.title.align = 0.5,
            plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
            plot.tag.position = c(0.15, 0.02),
            axis.title.y.right = element_text(margin = margin(l = 83)),
            #legend.position = c(1.2, 0.5)
        )

        ggsave(paste0(subDir, '/', varN, '_', YR, '-', MONNUM, '-', MON, '_', format(Sys.time(), "%y%m%d"), ".png"), height = 5, width = 8.25, unit = 'in', dpi = 300)
    }
    nc_close(nc)
}