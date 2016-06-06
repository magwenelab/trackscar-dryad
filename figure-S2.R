source("load-libraries.R")
source("fig-theme.R")

## run this to generate candidateGrowth
source("analyze-two-color-trackscar.R")


figS2 <- ggplot(candidateGrowth,
                aes(mean_37C/6, # convert to scars/hr
                    maxGrowth35halfC * 3600, # convert to OD/hr
                    label=PMY))+
    geom_point()+fig_theme+
    stat_smooth(method="lm", se=FALSE)+
    xlab("Mean division rate (daughters per hour)")+
    ylab("Max population growth rate (OD/hour)")

ggsave("figures/figureS2.pdf", width=3, height=3)
