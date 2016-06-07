library(plyr)

getTotalGrowth <- function(rawFile, numColors ){
    # Extracts the strain and the total growth during experiment
    # Controls for the number of colors used in the experiment
    if( numColors == 2 ){
        growth = rawFile$second - rawFile$first
        last = rawFile$second
    }
    if( numColors == 3 ){
        growth = rawFile$third - rawFile$first
        last = rawFile$third
    }
    data.frame(
        strain = as.character(rawFile$strain),
        growth = growth,
        first = rawFile$first,
        last = last
        )
}

getThreeColorGrowth <- function(rawFile){
    plyr::summarize(
        rawFile,
        strain = as.character(strain),
        growth = third - first,
        growth1 = second - first,
        growth2 = third - second,
        first = first,
        last = third)
}
        
getAllCounts <- function( indexFile, cD){
    # Returns a data.frame with all of the budscar counts
    # read in that is determined by the index file you pass it
    # the first, last and growth is returned depending on
    # the number of colors as specified by the index
    # This argument needs changed if the index filenames are changed
    indexFile <- subset(indexFile, readable == "yes")
    ddply( indexFile,
          .(folder, experiment_ID, counts_file,
            sampling, temp, media,
            who_counted, number_of_colors, time), 
          function(x){
              if( x$counts_file == "" ){
                  return( NULL )
              }
              getTotalGrowth(
                  read.csv(
                      file.path( cD,
                                x$counts_file),
                      as.is=T
                      ),
                  x$number_of_colors)
          }
   )
}

mean_cl_sem <- function(x){
    ## This originally just computed the standard error of the mean
    ## for ggplot, but was subsequently supplemented to calculate some
    ## other things too. This is a kludge and should be fixed.
    x <- x[!is.na(x)]
    data.frame( y = mean(x),
               ymax = mean(x) + sd(x)/sqrt(length(x)),
               ymin = mean(x) - sd(x)/sqrt(length(x)),
               n = length(x),
               percentZero = sum(x == 0)/length(x),
               percentUnder3 = sum(x < 3)/length(x),
               percentUnder4 = sum(x < 4)/length(x),
               max = max(x),
               min = min(x)
               )
}
    
getGrowthByFirst <- function( anExperiment, whatOrder = "first", sem=FALSE){
    ## This expects to be passed an experiment's worth of data.
    ## It ignores all other columns in the data besides whatOrder and
    ## growth
    if(sem){
        ddply(
        anExperiment,
        c(whatOrder),
        with,
        mean_cl_sem(growth)
        )
    }else{
        ddply(
        anExperiment,
        c(whatOrder),
        with,
        mean_cl_sd(growth)
        )
    }
    
}

padFrequencies <- function(theData, maxDiv){
    ## This will add zeros to the frequency column until
    ## there is an entry up to "maxDiv"
    ## This will add zeros in the frequency column down
    ## to zero as well.
    while( max(theData$growth) < maxDiv ){
          toPad <- theData[1,]
          toPad["freq"] <- 0
          toPad["growth"] <- max(theData$growth) + 1
          theData <- rbind(theData, toPad)
    }
    while( min(theData$growth) > 0 ){
          toPad <- theData[1,]
          toPad["freq"] <- 0
          toPad["growth"] <- min(theData$growth) - 1
          theData <- rbind(toPad, theData)
    }
    theData
}

