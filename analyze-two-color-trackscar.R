source("load-libraries.R")
source("budscar-count-utilities.R")

heatStressCandidatesWithAge <- read.csv("dryad/2016-Maxwell-Magwene-two-color-trackscar.csv", as.is=T)

################## Random sampling only
## restrict to only experiments where random sampling was done

heatStressCandidates <- subset(heatStressCandidatesWithAge, sampling=="random")

heatStressCandidatesFreq <-
        heatStressCandidates %>%
        count(c("experiment_ID", "strain", "temp", "growth")) %>%
        ddply(c("experiment_ID", "strain", "temp"),
              plyr::mutate,
              freq.scaled = freq/sum(freq))

## mean fecundity of strains by their beginning age
heatStressCandidatesGrowthByFirstAndReplicate <-
    heatStressCandidatesWithAge %>%
    ddply(.(strain, temp, experiment_ID),
          getGrowthByFirst, sem=TRUE)


### merge together the max growth and trackscar data

## run this to generate the data.frame maxGrowthCast2
source("analyze-growth-curves.R")

heatStressCandidatesMean <-
    ddply(heatStressCandidates,
          .(strain, temp),
          plyr::summarize,
          mean = mean(growth, na.rm=T)) %>%
    melt(id.vars=c("strain", "temp")) %>%
    dcast(strain~variable+temp) %>%
    plyr::mutate(
        strain = strain,
        ratioMean = mean_37C/mean_30C)

candidateGrowth <- merge(maxGrowthCast2,
                         heatStressCandidatesMean,
                         by.x="PMY",
                         by.y="strain")

