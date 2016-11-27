source("load-libraries.R")

source("fig-theme.R")
source("budscar-count-utilities.R")
source("analyze-growth-curves.R")

sem <- function(x){
    x <- x[!is.na(x)]
    s = sd(x)
    return( s/sqrt(length(x)))}

averageGrowth <- maxGrowth %>%
    transform( maxGrowth = maxGrowth * 3600) %>% # OD/sec -> OD/hr
    ddply(.(Strain, PMY, temp),
          plyr::summarize,
          se_growth = sem(maxGrowth),
          maxGrowth = mean(maxGrowth, na.rm=T),
          se_maxOD = sem(maxOD),
          maxOD = mean(maxOD, na.rm=T),
          pointOfMaxGrowthRate = mean(pointOfMaxGrowthRate, na.rm=T))

stress <- subset(averageGrowth, temp == 35.5)

stressOrder  <- stress[["PMY"]][order(stress[["maxGrowth"]])]
averageGrowth <- averageGrowth %>%
    transform( PMY = factor( as.character(PMY), levels = stressOrder))
gCastMean <- dcast(averageGrowth,
               PMY~temp, value.var="maxGrowth")
gCastSE <- dcast(averageGrowth,
                 PMY~temp, value.var="se_growth")
gCast <- merge(gCastMean, gCastSE, by="PMY", suffixes=c("mean", "se"))
colnames(gCast) <- gsub("\\.", "_", colnames(gCast))

sensitive <- maxGrowthCast2$PMY[maxGrowthCast2$ratioMaxGrowth < 0.93]

figS4 <- gCast %>% 
    subset(PMY != 1577) %>%
    transform(sensitive = PMY %in% sensitive) %>% 
    ggplot(aes(x=`X30mean`,
               xmin=`X30mean`-`X30se`,
               xmax=`X30mean`+`X30se`,
               y=`X35_5mean`,
               ymin=`X35_5mean` - `X35_5se`,
               ymax=`X35_5mean` + `X35_5se`,
               col=sensitive))+
    geom_errorbarh()+geom_errorbar()+geom_point()+fig_theme+
    scale_color_manual(values=c("blue", "darkorange"))+
    xlab("Max growth at 30C +/- SEM (OD/hr)")+
    ylab("Max growth at 35.5C +/- SEM (OD/hr)")

ggsave("figureS4-new.pdf", figS4, width=5, height=5)

maxGrowth <- transform(maxGrowth,
                       Strain =
                           factor(as.character(Strain),
                                  levels = maxGrowthCast2$Strain[
                                      order(maxGrowthCast2$ratioMaxGrowth)]))

figS5 <- maxGrowth %>%
    subset(PMY != 1577) %>% # This is a clear outlier with impossibly high growth%>%
    transform( maxGrowth = maxGrowth * 3600) %>% 
    transform(sensitive=PMY %in% sensitive) %>% 
    ggplot(aes(x=Strain,
               y=maxGrowth,
               col=sensitive))+
    geom_point(pch=1)+
    facet_grid(~temp)+coord_flip()+theme_bw(5)+
    scale_color_manual(values=c("blue", "darkorange"))+
    ylab("Max growth (OD/hr)")
ggsave("figureS5-new.pdf", figS5, width=10, height=5)
