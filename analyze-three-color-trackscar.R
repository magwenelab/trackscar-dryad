source("load-libraries.R")

recoveryCounts <- read.csv("2016-Maxwell-Magwene-three-color-trackscar.csv")

## This makes a version of the counts to add to the two-color data
## This can be done because the first growth of the recovery data is basically
## a two-color trackscar experiment

recoveryCountsForMortality <-
    recoveryCounts %>%
    subset( temp %in% c("30C", "35.5C")) %>% 
    subset( !is.na(growth)) %>%
    transform(experiment_ID = experiment, growth = growth1,
              time=6, media="YPD", type="other", number_of_colors=2)

recoveryCountsForMerge <-
      recoveryCountsForMortality %>%
      subset(sampling == "random")
