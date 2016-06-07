library(plyr)
library(magrittr)
library(reshape2)

## This file contains the estimates of maximum population growth rate
## as calculated by the "cellGrowth" package in Bioconductor using a
## non-parametric fit

maxGrowth <- read.csv("dryad/2016-Maxwell-Magwene-100genomes-temperature-growth.csv")

## Calculate the average growth for each strain, temperature and replicate
maxGrowthCast <-
    maxGrowth %>% 
    dcast(PMY ~ temp + rep,
          value.var="maxGrowth",
          fun.aggregate= function(x) mean(x, na.rm=T))

## Calculate the ratios of growth for each strain and temperature
maxGrowthCast2 <-
    maxGrowth %>%
    dcast(PMY ~ temp,
          value.var="maxGrowth",
          fun.aggregate= function(x) mean(x, na.rm=T)) %>%
    transform(ratioMaxGrowth = `35.5`/`30`,
              maxGrowth30C = `30`,
              maxGrowth35halfC = `35.5`,
              maxGrowth37C = `37`,
              `37` = NULL,
              `30` = NULL,
              `35.5` = NULL)


## Identify candidate strains for screening
## This code generated the original file, but it's since been annotated

## it's now called 2016-Maxwell-Magwene-heat-stress-candidates.csv
## and contains information about which candidates were excluded from consideration
## for TrackScar and why

if( !file.exists("heatStressCandidates")){
    write.csv(subset(maxGrowthCast2, (ratioMaxGrowth < 0.93)),
              "heatStressCandidates.csv")
}
