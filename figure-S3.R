
library(ggplot2)
library(magrittr)

source("fig-theme.R")
source("budscar-count-utilities.R")

maxGrowth <- read.csv("2016-Maxwell-Magwene-100genomes-temperature-growth.csv")

averageGrowth <- maxGrowth %>%
    ddply(.(PMY, temp),
        plyr::summarize,
        maxGrowth = mean(maxGrowth, na.rm=T),
        maxOD = mean(maxOD, na.rm=T),
        pointOfMaxGrowthRate = mean(pointOfMaxGrowthRate, na.rm=T)) %>%
    transform( maxGrowth = maxGrowth * 3600) # OD/sec -> OD/hr

highlighted <- maxGrowth %>%
    subset( PMY %in% c("1587")) %>%
    transform( maxGrowth = maxGrowth * 3600)

figS3 <- averageGrowth %>%
    subset(PMY != 1577) %>% # This is a clear outlier with impossibly high growth
    ggplot(aes(temp,sqrt(maxGrowth),group=PMY))+
    geom_line(alpha=0.05)+
    geom_point(size=1, alpha=0.3)+
    stat_summary(fun.data="mean_cl_sem",
                 data=highlighted, aes(group=PMY),
                 alpha=1, col="darkred", geom="line")+
    stat_summary(fun.data="mean_cl_sem",
                 data=highlighted, aes(group=PMY),
                 alpha=1, col="darkred", geom="pointrange")+
    fig_theme+
    annotate( geom="text", x=23, y=c(0.25,0.22),
             label=c("S288C","All others"),
             col=c("darkred", "grey50"))

ggsave("figureS3.pdf", figS3, width=5, height=3)
