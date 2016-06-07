source("budscar-count-utilities.R")

library(magrittr)
library(plyr)

countsDir <- file.path("~/git", "budscar_counts")

## The experiments were coded using the following scheme
mapping <- data.frame(
    treatment = c("A", "B", "C", "D"),
    recoveryTemp = c(37,30,37,30),
    recoveryTime = c(3,3,6,6))

## Read in the file using metadata in the recovery_index file
recoveryFiles <- read.csv( file.path(countsDir, "recovery_index.csv"), as.is=T) %>%
    transform(counts_file = file.path(countsDir, counts_file))

## Uses getThreeColorGrowth to calculate the fecundities
## Merges with the treatment information, and annotates it
recoveryCounts <- recoveryFiles %>%
    ddply(.(temp, folder, sampling,
            counts_file, who_counted,
            number_of_colors),
          function(x) read.csv(x$counts_file)) %>%
    ddply( c("temp", "folder", "counts_file",
             "experiment", "sampling",
             "strain", "treatment", "who_counted"),
          getThreeColorGrowth) %>% # defined in budscarCountUtilities.R
    merge(mapping, by = "treatment") %>%
    subset(recoveryTime == 6) %>% # don't consider the 3hr recovery data
    transform(recoveryTemp = factor(
                  recoveryTemp,
                  levels=c(30, 37),
                  labels=c("30C recovery", "37C recovery")),
              recoveryTime = paste(recoveryTime, "hr recovery"))

## Write a file for Dryad
plyr::summarize(recoveryCounts,
                folder,
                counts_file,
                experiment,
                sampling,
                who_counted,
                temp,
                strain,
                growth,
                growth1,
                growth2,
                first,
                last,
                recoveryTemp,
                recoveryTime) %>% 
    write.csv("2016-Maxwell-Magwene-three-color-trackscar.csv",
              row.names=FALSE)
