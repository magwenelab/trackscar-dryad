source("load-libraries.R")
source("fig-theme.R")
source("budscar-count-utilities.R")

dat <- read.csv("2016-Maxwell-Magwene-mito-trackscar.csv")

datCount <- count( dat, vars=c("experiment_ID", "growth", "mitos", "temp")) %>%
  ddply(.(experiment_ID, mitos, temp),
        plyr::mutate,
        freq.scaled = freq/sum(freq, na.rm=T),
        n.tot = sum(freq)) %>%
    transform(
        mitos = factor(as.character(mitos),
                       levels=c("t", "c", "tc", "n"),
                       labels=c("Threads", "Clumps", "Threads & Clumps", "No mito.")))


p <- datCount %>%
  subset(!is.na(mitos)) %>%
  subset(n.tot > 5) %>% 
  ggplot(aes(x=growth, y=freq.scaled, col=mitos, lty=mitos, pch=mitos))+
  stat_summary(fun.y=mean, geom="line")+
  stat_summary(fun.data="mean_cl_sem", lty=1)+
  facet_wrap(~temp)+
  fig_theme+
  scale_y_continuous("Fraction of population", labels=percent)+
  xlab("Daughters in 6hr")+
  scale_color_manual(values=c("black", "darkorange", "blue", wes_palette("Royal1")[2]))

ggsave("figure6.pdf", p, width=4, height=3)
