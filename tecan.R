tecan_version <- function(){
    "3"
}

getColumnExportData <- function(toRead, platemap){
    #This reads in the column export data from the Tecan
    #Returns a list with the raw data and melted data
    require(reshape2)
    #out <- list()
    
    dat <- readLines(toRead)
    dat <- dat[-length(dat)]

    datSplit <- lapply( dat, strsplit, '\t')
    datSplit <- lapply( datSplit, "[[", 1)
    names(datSplit) <- sapply( datSplit, "[", 1)

    rawData <- t(as.data.frame(lapply(lapply(datSplit, "[", -1), as.numeric)))

    colnames(rawData) <- c(
        lapply(c("A", "B", "C", "D", "E", "F", "G", "H"),
               function(x) paste( x, 1:12, sep="_")
               ),
        recursive=T)

    d.m <- melt(rawData)
    d.m$Var1 <- gsub("X", "", d.m$Var1)
    d.m$Var1 <- gsub("s", "", d.m$Var1)
    d.m$Var1 <- as.numeric(d.m$Var1)
    d.m$row <- sapply( strsplit(as.character(d.m$Var2), "_"), "[", 1)
    d.m$col <- as.numeric(sapply( strsplit(as.character(d.m$Var2), "_"), "[", 2))

    if( !missing(platemap) ){
        if( is.character(platemap)){
            platemap <- read.csv(platemap)
        }else if( is.data.frame(platemap) ){
        }else{
            stop( "Platemap must be either character or data.frame")
        }
        d.m <- merge(d.m, platemap, by = c("row", "col"))
    }
    #Added in v2. Rename columns to something relevant
    colnames(d.m)[ colnames(d.m) == "Var1" ] <- "seconds"
    colnames(d.m)[ colnames(d.m) == "Var2" ] <- "well"
    colnames(d.m)[ colnames(d.m) == "value" ] <- "OD"
    #out[["melted"]] <- d.m
    return(d.m)
}

convert96WellPlateMap <- function(thePlatemap,
                                  rowName = "row.96",
                                  colName = "column.96"){
    # Assuming that each of the 96 wells is replicated 16
    # times onto the big plate.
    out <- ddply(thePlatemap,
                 c(rowName, colName),
                 function(x){
                     expand.grid(
                         Row = split( 1:32, rep(LETTERS[1:8], each=4))[[ x[[rowName]] ]],
                         Column = split( 1:48, rep(1:12, each=4))[[ x[[colName]] ]]
                         )
                 })
    merge(out, thePlatemap, by = c(rowName, colName))
}

getGrowthByVariables <- function(theData, variables, timeName="seconds", odName="OD", model = "locfit", relative.height.at.lag = 0.1){
    require(plyr); require(cellGrowth)
    # The time should be in seconds
    if( !(odName %in% colnames(theData))){
        stop("OD name isn't in the columns of the data")
    }
    if( !(timeName %in% colnames(theData))){
        stop("Time name isn't in the columns of the data")
    }
    if( !( all(variables %in% colnames(theData)))){
        stop("Split variables aren't in the columns of the data")
    }
    ddply(theData,
          variables,
          function(x){
              theFit = fitCellGrowth( x[[timeName]], x[[odName]], model = model, relative.height.at.lag = 0.1)
              maxGrowth = attr(theFit, "maxGrowthRate")
              pointOfMaxGrowthRate = attr(theFit, "pointOfMaxGrowthRate")
              maxOD = max( x[[odName]] )
              if( is.null(maxGrowth)) maxGrowth <- NA
              if( is.null(pointOfMaxGrowthRate)) pointOfMaxGrowthRate <- NA
              c(maxGrowth = maxGrowth,
                pointOfMaxGrowthRate = pointOfMaxGrowthRate,
                maxOD = maxOD)
          })
}
