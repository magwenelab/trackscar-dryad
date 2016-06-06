

library(ggplot2); library(plyr); library(magrittr); library(reshape2); library(grid); library(scales)
codeDir <- file.path(Sys.getenv("CM_GIT"), "Rcolin")
source(file.path(codeDir, "theme_clean.R"))
source(file.path(codeDir, "budscarCountUtilities.R"))


readFile <- function(theFile, ... ){
    read.csv(theFile, as.is=T) %>%
      transform(temp = sapply(strsplit(as.character(strain), "-"), "[", 2))
}

figTheme = theme_clean(10, base_family="sans")+
    theme(legend.position="bottom", legend.box="horizontal", legend.margin=unit(0.1, "mm"),
          plot.margin=unit(c(1,1,1,1), "mm"))

pheno <- data.frame( theFile = c("exp011616_colin.csv", "exp011816_colin.csv", "exp012016_colin.csv"), rep = c(1,2, 3), stringsAsFactors=FALSE)

dat <- ddply( pheno, .(rep, theFile), splat(readFile), .inform=T) %>%
  transform( growth = second - first)

datCount <- count( dat, vars=c("rep", "growth", "mitos", "temp")) %>%
  ddply(.(rep, mitos, temp),
        plyr::mutate,
        freq.scaled = freq/sum(freq, na.rm=T),
        n.tot = sum(freq)) %>%
  transform( mitos = factor(as.character(mitos), levels=c("t", "c", "tc", "n"),
                 labels=c("Threads", "Clumps", "Threads & Clumps", "No mito.")))
        

## ddply(dat, .(temp, mitos), plyr::summarize, noNucleus = sum(noNucleus, na.rm=T)/length(nuclei), n = length(nuclei)) %>% dcast(mitos ~ temp, value.var="noNucleus")


## ggplot( dat, aes( x=second-first, col=mitos))+
##   geom_freqpoly(binwidth=1)+facet_grid(temp~., scale="free_y")


## ggplot( dat, aes( x=factor(rep), col=mitos, y=second-first))+
##   geom_boxplot()+facet_grid(temp~., scale="free_y")

library(wesanderson)

pdf("mito_trackscar.pdf", width=6, height=3)
datCount %>%
  subset(!is.na(mitos)) %>%
  subset(n.tot > 5) %>% 
  ggplot(aes(x=growth, y=freq.scaled, col=mitos, lty=mitos, pch=mitos))+
  ## geom_point()+
  stat_summary(fun.y=mean, geom="line")+
  stat_summary(fun.data="mean_cl_sem", lty=1)+
  facet_wrap(~temp)+
  theme_clean()+
  scale_y_continuous("Fraction of population", labels=percent)+
  xlab("Daughters in 6hr")+
  scale_color_manual(values=c("black", "darkorange", "blue", wes_palette("Royal1")[2]))
dev.off()
