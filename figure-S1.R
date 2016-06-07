source("load-libraries.R")
source("fig-theme.R")
source("budscar-count-utilities.R")
source("analyze-two-color-trackscar.R")

timeseriesCounts <- read.csv("2016-Maxwell-Magwene-two-color-trackscar-timeseries.csv")

fig1Counts <-
    haploidCounts  %>% 
    subset((first %in% c(1,2))) %>%
    subset(!is.na(growth)) %>% 
    transform(
        group=factor(first,
                     labels = c(1,2))
    ) %>%
    count(vars=c("folder", "group", "growth")) %>% 
    ddply(c("folder","group"),
          padFrequencies,
          9) %>%
    ddply(c("group", "folder"),
          transform,
          freq = freq/sum(freq))

figS1b = ggplot(fig1Counts,
                aes(x=growth,
                    y = freq,
                    col=factor(group),
                    group=group,
                    pch=factor(group)))+
    stat_summary(fun.data="mean_cl_sem")+
    stat_summary(fun.y="mean", geom="line")+
    fig_theme+
    scale_x_continuous(limits=c(2,9),breaks=2:9)+
    scale_y_continuous(labels=percent)+
    scale_color_manual("", values=c("darkorange", "blue", "black"))+
    scale_shape_manual("", values=c(1,2,3))

meanByTime <- ddply(timeseriesCounts,
                        .(strain, time),
                        plyr::summarize,
                        m = mean(growth, na.rm=T)) %>%
      ddply(.(strain),
            plyr::mutate,
            change = c( m[1], m[2:length(m)]-m[1:(length(m)-1)]),
            interval = c( time[1], time[2:length(m)]-time[1:(length(m)-1)]),
            time = time) %>%
      transform(rate = change/interval)

    

figS1a = subset(meanByTime, time <= 6) %>%
      ggplot(aes(time,rate))+
      geom_point(pch=1)+
      scale_x_continuous("Hours after first stain", breaks=1:6)+
      ylab("Growth rate (buds/hour)")+
      stat_smooth(method="lm", lty=2, fill="grey90")+
      fig_theme


pdf("figureS1.pdf", width=4.75, height=2, useDingbats=FALSE)
grid.arrange(figS1a,
             figS1b, 
             nrow = 1, ncol=2
             )
dev.off()
