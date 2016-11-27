source("load-libraries.R")
source("analyze-two-color-trackscar.R")
source("fig-theme.R")

YJM <- read.csv("2016-Maxwell-Magwene-PMY-to-YJM.csv")

slowStrains <- subset(heatStressCandidatesMean,
                      mean_35_5C/mean_30C < 0.93)$strain

strainMapping <- YJM$Strain
names(strainMapping) <- as.character(YJM$PMY)

figS4 =
    heatStressCandidates %>%
    subset(temp %in% c("30C", "35.5C")) %>% 
    transform( sensitive = strain %in% slowStrains) %>%
    transform(sensitive = ifelse(sensitive, "sensitive", "robust")) %>%
    subset(strain != "1529") %>% # No 30C data
    subset(strain != "1554") %>% # Big increase at 37C -> probably artifact
    subset(growth > 0) %>%
    transform( strain= strainMapping[ as.character(strain) ]) %>%
    ggplot(aes(x=growth, col=temp))+
    geom_freqpoly(binwidth=1, aes(y=..density..))+
    facet_wrap(~ sensitive+strain,shrink=FALSE)+
    scale_shape_manual(values=c(16,17))+
    scale_color_manual("", values=c("brown", wes_palette("Rushmore")[-2])[1:2])+
    scale_x_continuous("Daughters in 6hr", breaks=c(0:10))+
    ylab("Frequency")+fig_theme

ggsave("figureS6.pdf", figS4, height=6, width=8)
