source("load-libraries.R")
source("analyze-two-color-trackscar.R")
source("fig-theme.R")

fig5a = heatStressCandidatesGrowthByFirstAndReplicate %>%
    subset(n >= 3) %>% # require three cells for each age in each replicate
    subset(strain %in% c(1523, 1513)) %>%
    ddply(.(strain, first,temp),
          function(x){
              if(nrow(x) < 3){ # require three replicates for each age
                  return(NULL)
              }
              x}) %>%
    transform(strain=factor(strain, levels=c(1513,1523), labels=c("YJM693", "YJM996"))) %>% 
    ggplot(aes(x=first, y=y, col=strain, group=strain, pch=strain))+
    stat_summary(fun.data="mean_cl_sem", lty=3)+
    stat_summary(fun.data="mean_cl_sem", geom="line")+
    facet_wrap(~temp, scale="free_x")+
    scale_color_manual("", values=c("blue", "black"))+
    scale_shape_manual(values=c(1,2))+
    scale_x_continuous("Cohort", breaks=1:10)+
    ylab("Fecundity")+
    fig_theme


fig5b =  heatStressCandidatesGrowthByFirstAndReplicate %>%
    subset(strain %in% c(1513, 1523)) %>%
    subset(n >= 5) %>% # require five cells for each age in each replicate
    ddply(.(strain, first, temp),
          function(x){
              if(length(unique(x$experiment_ID)) < 3){ # require three replicates for each age
                  return(NULL)
              }
              x})%>%
    subset(temp %in% c("37C")) %>%
    ddply( .(strain),
          function(x){
              if( unique(x$strain) == 1513){
                        data.frame(x, dead = x$percentUnder4)
                    }else{
                        data.frame(x, dead = x$percentZero)
                    }}) %>%
    transform(strain=factor(strain, levels=c(1513,1523), labels=c("YJM693", "YJM996"))) %>% 
    ggplot(aes(x=first, y=dead, col=strain, group=strain, shape=strain))+
    stat_summary(fun.data="mean_cl_sem",lty=3)+
    stat_summary(fun.data="mean_cl_sem", geom="line")+
    facet_wrap(~strain, scale="free_x")+
    scale_x_continuous("Cohort", breaks=0:10)+
    scale_y_continuous("Mortality (+/- SEM)", labels=percent)+
    scale_color_manual(values=c("blue", "black"))+
    scale_shape_manual(values=c(1,2))+
    fig_theme

pdf("figures/figure5.pdf", width=4.75, height=6)
grid.arrange(fig5a,
             fig5b,
             nrow = 2, ncol = 1
             )
dev.off()
