source("load-libraries.R")
source("fig-theme.R")

# See labnotebook volume 5 pgs 12 and 14 for raw data

dat <- read.csv("2016-Maxwell-Magwene-mito-morphology-scoring.csv",
                as.is=T,
                skip=1)

dat.m <- melt(dat,
              id.vars=c("strain", "temp", "rep")) %>%
    transform(score = sapply(
                  strsplit(as.character(variable), "_"),
                  "[",
                  1),
              description = sapply(
                  strsplit(as.character(variable), "_"),
                  "[",
                  2)) %>%
    ddply(.(strain, temp, rep),
          plyr::mutate,
          normalized = value/sum(value))

datMeans <- ddply(dat.m,
                  .(strain, temp, description, score),
                  plyr::summarize,
                  mean=mean(normalized),
                  sem = sd(normalized)/sqrt(length(normalized))) %>%
    transform(strain = factor( as.character(strain),
                              levels=c("CMY178", "CMY177", "CMY182"),
                              labels=c("YJM996", "YJM693", "S288C")))

p <- datMeans %>%
  subset(description != "threads") %>%
  ggplot(aes(x=factor( temp ),
             y=mean, col=description,
             ymin=mean-sem, ymax=mean+sem,
             group=paste(strain, description),
             lty=description,
             pch=description))+
  geom_line()+facet_grid(.~strain)+geom_pointrange(lty=1)+
  scale_y_continuous("Percent of population", labels=percent)+
  xlab("Temperature")+
  theme_clean()+
    scale_color_manual("", values=c("red", "darkorange", "blue"))

ggsave("figureS9.pdf", p, height=2, width=7)
