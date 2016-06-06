## Defines the ggplot theme for the figures. It's similar to theme_bw
## except it lacks the gridlines in the panels

library(ggplot2)

theme_clean <- function (base_size = 12, base_family = ""){
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(strip.background = element_rect(color='grey60', fill='grey95', size=0.2),
          panel.border = element_rect(fill=NA, color='grey60', size=0.2),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.key=element_blank())
}

fig_theme = theme_clean(10, base_family="sans")+
    theme(legend.position="bottom",
          legend.box="horizontal", legend.margin=unit(0.1, "mm"),
          plot.margin=unit(c(1,1,1,1), "mm"))
