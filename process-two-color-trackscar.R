source("budscar-count-utilities.R")
library(magrittr)

### Read in all the trackscar data

countsDir <- file.path("~/git", "budscar_counts")
index <- read.csv(file.path(countsDir, "index.csv"))
allCounts <- getAllCounts(index, countsDir)

### Make a list of all the strains examined during the project

heatCand <- read.csv("2016-Maxwell-Magwene-heat-stress-candidates.csv", skip=1) %>%
    subset(exclusion_reason == "")
candidateStrains <- rep("second_screening", nrow(heatCand))
names(candidateStrains) <- as.character(heatCand$PMY)

## These are a few extra strains examined at both 37C and 30C during
## the course of the experiments
additionalStrains <- rep("first_screening", 9)
names(additionalStrains) <- c("1560",
                              "1492",
                              "1549",
                              "1535",
                              "1587",
                              "1557",
                              "1504",
                              "1516")
candidateStrains <- c(candidateStrains, additionalStrains)

## A mapping from YJM to PMY numbers
## removing the ones that are haploid as per Dan's info on 1/24/15
strainMap <- read.csv(file.path("2015_screening_for_TS", "PMY_to_YJM.csv"))  %>%
    subset( !(Strain %in% c("YJM1250", "YJM1388", "YJM1419")))
 
## this will make the data 'recoveryCountsForMerge' which contains
## the first interval of three color trackscar data, which is
## just a two color trackscar experiment.
source("analyze-three-color-trackscar.R")


## To get accurate measurements for mortality and fecundity in older
## cohorts, some experiments specifically sought out the older cells

## For growth by first age, use all the data
heatStressCandidatesWithAge <-
    allCounts %>%
    subset((strain %in% names(candidateStrains)) &
           (media == "YPD") &
           ((strain == "1587") | (temp %in% c("30C", "37C"))) & # 37C and 30C. all temps for s288c
           (time == 6) & # only experiments that were 6hr
           (growth < 10))


## include the first interval of the recovery data
heatStressCandidatesWithAge <-
    rbind(heatStressCandidatesWithAge,
          recoveryCountsForMortality[,colnames(heatStressCandidatesWithAge)]) # defined in process-three-color-trackscar.R

q## include the haploid S288C data for fig S1

## These are the haploid S288C data shown to compare mother cell
## and daughter cell fecundity
haploidCounts <- subset(allCounts,
                        folder %in% c("2015-03-31_S288C_30C",
                                      "2015-04-01_S288C_30C",
                                      "2014_03_11_WGA_microscopy")) %>%
    subset(strain %in% c("CMY1","1638"))

heatStressCandidatesWithAge <- rbind(heatStressCandidatesWithAge,
                                     haploidCounts[,colnames(heatStressCandidatesWithAge)])

write.csv(transform(heatStressCandidatesWithAge),
          "2016-Maxwell-Magwene-two-color-trackscar.csv", row.names=FALSE)

## This is a timeseries experiment with different number of hours
## between first and second stains

timeseriesCounts <- read.csv(
    file.path(countsDir,
              "20140321_bud_counts_combined.csv")) %>%
    transform(growth=second-first)
with(timeseriesCounts,
     data.frame(
         folder="2014-03-21_WGA_calibration2",
         experiment_ID="timecourse2",
         counts_file="20140321_bud_counts_combined.csv",
         sampling="random",
         temp="30C",
         media="YPD",
         who_counted="CSM",
         number_of_colors=2,
         time=hours,
         strain=real_strain,
         replicate=replicate,
         growth=growth,
         first=first,
         last=second)) %>%
    write.csv("2016-Maxwell-Magwene-two-color-trackscar-timeseries.csv",row.names=F)
                                 

