source("load-libraries.R")
source("analyze-two-color-trackscar.R")
source("fig-theme.R")

img060 =   heatStressCandidates %>%
    subset(strain %in% c(1587,1513,1523)) %>%
    transform(strain = factor(as.character(strain),
                              levels=c("1587", "1513", "1523"),
                              labels=c("S288C", "YJM693", "YJM996"))) %>% 
    count(c("strain","temp","first")) %>%
    subset(temp %in% c("30C", "35.5C")) %>% 
    ddply(.(strain, temp),
          plyr::mutate,
          scaled.freq=freq/sum(freq)) %>%
    subset(freq>5) %>% 
    transform(log2.freq=log2(scaled.freq)) %>%
    ggplot(aes(x=first,y=log2.freq,col=strain, shape=strain))+
    geom_line()+geom_point(size=1.7)+
    scale_color_manual(
        values = c(
            "darkred",
            "blue",
            "black"))+
    facet_grid(.~temp)+
    stat_function(fun=function(x) log2(1/(2^x)),col="grey80",lty=3)+ 
    scale_shape_manual( values=c(3,1,2))+
    theme_clean()+
    scale_y_continuous(limits=c(-9,0), breaks=seq(-9,0))+
    scale_x_continuous(breaks=seq(1,15,by=2))+
    fig_theme

ggsave("figureS8.pdf",img060,height=2, width=3)
