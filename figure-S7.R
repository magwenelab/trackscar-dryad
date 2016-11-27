
source("analyze-S288C-trackscar.R")
source("fig-theme.R")

figS5 = S288CGrowthByFirst %>%
    subset(n >= 3) %>%
    ddply(.(first,temp),
          function(x){
              if(nrow(x) < 2){
                  return(NULL)
              }
              x}) %>%
    ggplot(aes(x=first,
               y=y,
               col=factor(temp),
               group=factor(temp),
               pch=factor(temp)))+
    stat_summary(fun.data="mean_cl_sem", lty=3)+
    stat_summary(fun.data="mean_cl_sem", geom="line")+
    scale_x_continuous("Age at start of experiment", breaks=1:10)+
    ylab("Mean number of daughters in 6hr (+/- SEM)")+
    scale_color_manual("temp", values=c("brown", wes_palette("Rushmore")[-2]))+
    fig_theme

ggsave("figureS7.pdf", figS5, height=6, width=6)
