library(ggplot2)
library(magrittr)
library(scales)
library(wesanderson)

source("analyze-two-color-trackscar.R")
source("budscar-count-utilities.R")
source("fig-theme.R")

YJM <- read.csv("2016-Maxwell-Magwene-PMY-to-YJM.csv")
strainMapping <- YJM$Strain
names(strainMapping) <- as.character(YJM$PMY)


fig3 = heatStressCandidatesFreq %>%
    subset(strain %in% c(1513, 1535, 1523, 1578)) %>%
    ddply(.(temp,strain),
          function(x){
              fold = as.character(unique(x$experiment_ID))[1]
              subset( x, experiment_ID == fold)
          }) %>%
    subset( !is.na(growth)) %>%
    transform( strain= strainMapping[ as.character(strain) ]) %>%
    transform( strain = factor(strain,
                               levels=c(
                                   "YJM693", "YJM996", "YJM1248", "YJM1478"))) %>% 
    ddply(.(temp, strain), padFrequencies, 8) %>%
    ddply(.(temp, strain), plyr::mutate, freq.scaled = freq/sum(freq)) %>%
    ggplot(aes(x=growth, y=freq.scaled, col=temp, pch=temp))+
    geom_line()+geom_point()+
    facet_wrap(~strain,nrow=2)+
    scale_shape_manual(values=c(16,17))+
    scale_color_manual("", values=c("brown", wes_palette("Rushmore")[-2])[1:2])+
    scale_x_continuous("Daughters in 6hr", breaks=seq(0,10, by=2))+
    scale_y_continuous("Fraction of population", labels=percent)+fig_theme

ggsave("figure3.pdf", fig3, height=3, width=5)
