source("budscar-count-utilities.R")

library(magrittr)
library(plyr)

countsDir <- file.path("~/git", "budscar_counts")

## The experiments were coded using the following scheme
mapping <- data.frame(
    treatment = c("A", "B", "C", "D"),
    recoveryTemp = c(37,30,37,30),
    recoveryTime = c(3,3,6,6))
  
recoveryFiles <- read.csv( file.path(countsDir, "recovery_index.csv"), as.is=T) %>%
    transform(counts_file = file.path(countsDir, counts_file))

recoveryCounts <- recoveryFiles %>%
    ddply(.(temp, folder, sampling,
            counts_file, who_counted,
            number_of_colors),
          function(x) read.csv(x$counts_file)) %>%
    ddply( c("temp", "folder", "counts_file",
             "experiment", "sampling",
             "strain", "treatment", "who_counted"), getThreeColorGrowth) %>% # defined in budscarCountUtilities
    merge(mapping, by = "treatment") %>%
    subset(recoveryTime == 6) %>% # don't consider the 3hr recovery data
    transform(recoveryTemp = factor(
                  recoveryTemp,
                  levels=c(30, 37),
                  labels=c("30C recovery", "37C recovery")),
              recoveryTime = paste(recoveryTime, "hr recovery"))

write.csv(recoveryCounts, "dryad/2016-Maxwell-Magwene-three-color-trackscar.csv", row.names=FALSE)
