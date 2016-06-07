source("load-libraries.R")
source("fig-theme.R")
source("analyze-growth-curves.R")
source("analyze-S288C-trackscar.R")
source("budscar-count-utilities.R")

fig2a = ggplot(maxGrowthCast2, # defined in analyze-growth-curves.R
               aes(ratioMaxGrowth))+
    geom_histogram(binwidth=0.025)+
    geom_rug(sides="t", # annotate strains examined closely
             data=subset(maxGrowthCast2,
                         PMY %in% c("1587", "1523", "1513")),
             aes(label=PMY), col="red")+
    geom_text(
        data=subset(maxGrowthCast2,
                    PMY %in% c("1587", "1523","1513")),
        aes(y = 12.5, label=PMY), col="black", size=2)+
    xlab("Ratio of max growth at 35.5C to max growth at 30C")+
    ylab("Number of strains")+
    fig_theme

fig2b =
    subset(S288CCounts, growth < 9) %>% 
    ggplot(
      aes(x=growth,
          y = freq.scaled,
          col=factor(temp),
          pch=factor(temp),
          group=temp))+
      stat_summary(fun.data="mean_cl_sem")+
      stat_summary(fun.y="mean", geom="line")+
      ## geom_point(pch=1)+
      scale_x_continuous("Daughters in 6hr", breaks=0:10)+
      scale_y_continuous("Fraction of population", labels=percent)+
      fig_theme+
      scale_color_manual("temp",
                         values=c("brown", wes_palette("Rushmore")[-2]))


pdf("figure2.pdf", width=3.34, height=4, useDingbats=FALSE)
grid.arrange(fig2a,
             fig2b,
             nrow = 2, ncol=1
             )
dev.off()
