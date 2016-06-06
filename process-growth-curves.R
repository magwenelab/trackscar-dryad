
source("tecan.R")

library(cellGrowth)
library(magrittr)
library(plyr)

## A mapping from YJM to PMY numbers
## removing the ones that are haploid as per Dan's info on 1/24/15
strainMap <- read.csv(file.path("2015_screening_for_TS", "PMY_to_YJM.csv")) %>%
    subset( !(Strain %in% c("YJM1250", "YJM1388", "YJM1419")))


growthCurves <-
    read.csv(file.path("2015_screening_for_TS", "TS_metadata.csv"))%>%
    transform(fn = file.path("2015_screening_for_TS", fn),
              pm = file.path("2015_screening_for_TS", pm)) %>%
    ddply(.(fn, pm, rep, temp),
          function(x){
              getColumnExportData(x$fn, x$pm) # defined in tecan.R
          }) %>%
    merge(strainMap, by = "Strain")

## write file of raw data for archiving in dryad
write.csv(
    plyr::summarize(
        .data=growthCurves,
        YJM = Strain,
        PMY = paste0("PMY", PMY),
        filename = fn,
        platemap = pm,
        replicate = rep,
        temperature = temp,
        row = row,
        col=col,
        seconds = seconds,
        OD),
    "2016-Maxwell-Magwene-100genomes-temperatures-raw.csv")

### getGrowthByVariables uses the non-parametric method described in the
### cellGrowth package to find the max growth

maxGrowth <- growthCurves %>%
    getGrowthByVariables( # defined in tecan.R
        c("fn", "temp", "rep", "row", "col", "Strain", "PMY")) %>%
    subset( !is.na(Strain) & (Strain != "BLANK"))

# write file of max growth data for archiving in dryad
write.csv(maxGrowth, "2016-Maxwell-Magwene-100genomes-temperature-growth.csv")
