source("load-libraries.R")
source("fig-theme.R")

## run this to generate candidateGrowth
source("analyze-two-color-trackscar.R")


figS2 <- ggplot(candidateGrowth,
                aes(mean_35_5C/6, # convert to scars/hr
                    maxGrowth35halfC * 3600, # convert to OD/hr
                    label=PMY,
                    col = ratioMaxGrowth < 0.93))+
    geom_point()+fig_theme+
    xlab("Mean division rate (daughters per hour)")+
    ylab("Max population growth rate (OD/hour)")+
    scale_color_manual(values=c("blue", "darkorange"))


ggsave("figureS2.pdf", width=3, height=3)
