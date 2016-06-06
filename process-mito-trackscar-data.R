source("load-libraries.R")

readFile <- function(theFile, ... ){
    read.csv(file.path("2016-01-18_mitos_and_trackscar",theFile), as.is=T) %>%
      transform(temp = sapply(strsplit(as.character(strain), "-"), "[", 2))
}

pheno <- data.frame(
    theFile = c("exp011616_colin.csv",
                "exp011816_colin.csv",
                "exp012016_colin.csv"),
    experiment_ID = c("exp011616",
                      "exp011816",
                      "exp012016"),
    rep = c(1,2,3),
    stringsAsFactors=FALSE)

dat <- ddply( pheno, .(rep, theFile, experiment_ID),
             splat(readFile), .inform=T) %>%
    transform( growth = second - first)

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
        mitos=mitos))

write.csv(dat, "2016-Maxwell-Magwene-mito-trackscar.csv", row.names=FALSE)
