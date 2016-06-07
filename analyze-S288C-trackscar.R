
source("load-libraries.R")
source("budscar-count-utilities.R")

heatStressCandidatesWithAge <- read.csv("2016-Maxwell-Magwene-two-color-trackscar.csv")
recoveryCounts <- read.csv("2016-Maxwell-Magwene-three-color-trackscar.csv")

A <- heatStressCandidatesWithAge %>%
    subset(sampling == "random")%>%
    subset(strain == 1587) %>%
    plyr::summarize(
        temp = temp,
        growth=growth,
        first = first,
        experiment=experiment_ID)

B <- recoveryCounts %>%
    subset(strain == 1587)%>%
    plyr::summarize(
        temp = temp,
        growth=growth1,
        first = first,
        experiment = experiment)

S288C <- rbind(A,B) %>% # convert to temp to numeric
    transform(temp = as.numeric(gsub("C", "", as.character(temp)))) 

S288CCounts <-
    S288C %>%
    subset(!is.na(growth)) %>% 
    count(c("experiment", "temp", "growth")) %>%
    ddply(c("experiment", "temp"),
          padFrequencies, 9) %>%
    ddply(c("experiment", "temp"),
          plyr::mutate,
          freq.scaled=freq/sum(freq))

S288CGrowthByFirst <- ddply(S288C,
                            .(temp, experiment),
                            getGrowthByFirst,sem=TRUE)
