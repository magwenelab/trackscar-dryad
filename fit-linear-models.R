source("analyze-S288C-trackscar.R")
source("analyze-two-color-trackscar.R")


#################### Fit regressions for fecundity ####################
### The regressions are fit to the mean fecundity of a cohort in a replicate


lm1 <- heatStressCandidatesGrowthByFirstAndReplicate %>%
    subset(n >= 3) %>% # require three observations per replicate
    subset(strain %in% c(1513, 1587, 1523)) %>%
    subset(first>1) %>% # exclude daughters
    ddply(.(strain, first,temp), # require three replicates per age
          function(x){ 
              if(nrow(x) < 3){
                  return(NULL)
              }
              x}) %>%
    dlply(.(strain, temp),
          with,
          lm(y ~ first)) # regress fecundity on age


lmSummaries <- ldply(lm1,
                     function(x){
                         out <- summary(x)$coefficients
                         data.frame( var = c("intercept", "first"), out)
                     }
                     )

lm2 <- S288CGrowthByFirst %>%
    subset( n > 3) %>% # require three observations per replicate
    subset(first>1) %>%  # exclude daughter cells
    ddply(.(first,temp), # require three replicates per age
          function(x){
              if(nrow(x) < 3){
                  return(NULL)
              }
              x}) %>%
    dlply(.(temp),
          with,
          lm(y ~ first))


