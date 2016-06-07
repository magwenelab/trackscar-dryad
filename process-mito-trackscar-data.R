source("load-libraries.R")

readFile <- function(theFile, ... ){
    ## Convenience function to read in the data and
    ## split the strain name from the temperature
    read.csv(
        file.path("2016-01-18_mitos_and_trackscar",theFile),
        as.is=T) %>%
        transform(
            temp = sapply(
                strsplit(
                    as.character(strain),
                    "-"),
                "[", 2))
}

## Meta data for the files to read in
pheno <- data.frame(
    theFile = c("exp011616_colin.csv",
                "exp011816_colin.csv",
                "exp012016_colin.csv"),
    experiment_ID = c("exp011616",
                      "exp011816",
                      "exp012016"),
    rep = c(1,2,3),
    stringsAsFactors=FALSE)

## Read in the files, calculate the fecundity 
dat <- ddply( pheno,
             .(rep, theFile, experiment_ID),
             splat(readFile),
             .inform=T) %>%
    transform( growth = second - first)

## Write a file for Dryad
dat <- with(dat,
    data.frame(
        experiment_ID = experiment_ID,
        counts_file = theFile,
        sampling = "random",
        temp,
        media = "SC-ura",
        who_counted = "CSM",
        number_of_colors=2,
        time = 6,
        strain = strain,
        growth=growth,
        first=first,
        second=second,
        mitos=mitos)) %>% # Fix the off by 1.5C error from incubator
    transform(temp = factor(
                  gsub("37",
                       "35.5",
                       as.character(temp))),
              strain = factor(gsub("37",
                                   "35.5",
                                   as.character(strain))))

write.csv(dat, "2016-Maxwell-Magwene-mito-trackscar.csv", row.names=FALSE)
