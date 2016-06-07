source("load-libraries.R")
source("analyze-three-color-trackscar.R")
source("analyze-two-color-trackscar.R")
source("budscar-count-utilities.R")
source("fig-theme.R")

fig4  = recoveryCounts %>%
    subset((temp %in% c("35.5C","40C")) &
           (recoveryTime == "6 hr recovery") &
           (recoveryTemp == "30C recovery")) %>%
    subset(!is.na(growth1) & !is.na(growth2)) %>%
    subset( strain %in% c(1513, 1587, 1523)) %>%
    transform( strain = factor( as.character(strain),
                               levels=c(1587, 1523, 1513),
                               labels=c("S288c", "YJM996", "YJM693"))) %>% 
    transform(live = growth2 > 0) %>% 
    count(vars=c("experiment", "strain", "live", "growth1")) %>%
    ddply(.(experiment, strain), plyr::mutate,
          freq.scaled = freq/sum(freq)) %>%
    subset(growth1 < 9) %>% 
    ggplot(aes(x=growth1, y = freq.scaled, col=live, group=live, pch=live))+
    stat_summary(fun.data="mean_cl_sem",lty=3)+
    stat_summary(fun.data="mean_cl_sem", geom="line")+
    facet_grid(strain~.)+
    stat_summary(
        data=
            transform(
                subset(heatStressCandidatesFreq,
                (growth < 9) &
                (strain %in% c(1513, 1587, 1523)) &
                (temp == "30C")),
                strain = factor( as.character(strain),
                               levels=c(1587, 1523, 1513),
                               labels=c("S288c", "YJM996", "YJM693")),
                live = TRUE),
        fun.data="mean_cl_sem",
        aes(x=growth, col=NULL),
        col="#009900", lty=3, pch=3)+
    stat_summary(
        data=
            transform(
                subset(heatStressCandidatesFreq,
                (growth < 9) &
                (strain %in% c(1513, 1587, 1523)) &
                (temp == "30C")),
                strain = factor( as.character(strain),
                               levels=c(1587, 1523, 1513),
                               labels=c("S288c", "YJM996", "YJM693")),
                live = TRUE),
        fun.data="mean_cl_sem",
        aes(x=growth, col=NULL),
        col="#009900",
        geom="line",
        lty=2)+
    scale_y_continuous("Within temperature frequency", labels=percent)+
    scale_x_continuous("Fecundity during heat stress", breaks=0:8)+
    scale_color_manual(values=c("black", "#CC33CC"))+
    scale_shape_manual(values=c(1,2))+
    fig_theme

ggsave("figure4.pdf", fig4, height=4, width=3.4)
