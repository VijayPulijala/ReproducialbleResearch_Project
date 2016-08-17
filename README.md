##1. Assignment

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

##2. Synopsis

The National Oceanic and Atmospheric Administration (NOAA) maintains a public database for storm event. The data contains the type of storm event, details like location, date, estimates for damage to property as well as the number of human victims of the storm. In this report we investigate which type of events are the most harmful to the population and financially.

The conclusion is that the impact on humans, be it injuries or fatalities, isn't directly correlated to the ecomomic damage weather events cause. Tornado's are by far the highest cause for injuries (#1), and second in fatalities, whilst heat & drought cause the most fatalities, but fourth in injuries. Both are in the top 5 of injuries & fatalities next to Thunderstorms (resp #2 and #5), Flooding (#3 both) and Snow & Ice (resp. #5 and #4). In economic damages, only the property damage really factors in the total damage, except for Heat & Drought where more than 90% of damages is determined by crop damage. The #1 & #2 of weather damage sources, resp. Flooding & High Surf and Wind & Storm cover more than 80% of all economic cost, while Wind & Storm aren't even in the top 5 of victims.
  
##3. Data Processing
  #Load libraries
  
  Necessary libraries to perform loading, computation, transformation and plotting of data
  library(bitops)
  library(RCurl) # for loading external dataset (getBinaryURL)
  library(R.utils) # for bunzip2
  library(R.oo)
  library(R.methodsS3)
  library(Rcpp)
  library(plyr) # for count & aggregate method
  library(stringi)
  library(magrittr)
  library(stringr)
  library(reshape2) # for melt 
  
  
  library(ggplot2) # for plots
  library(grid) # for grids
  library(gridExtra) # for advanced plots
  library(scales) # for plot scaling
  
  
  setwd("C:/Users/vpulij001c/Desktop/Vj/++R++/ReProducibleResearch")
  

  dataProcess <- TRUE
  # check if csvStormData variable already exists
  if(file.exists("./StormData.RData")){
    load("./StormData.RData")
    dataProcess <- FALSE
  }
  
  
  # load file from URL to bz2 file in data dir
  if(!file.exists("./StormData.csv.bz2")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
    destPath <- "./StormData.csv.bz2"
    binData <- getBinaryURL(fileUrl, ssl.verifypeer=0L, followlocation=1L)
    destFileHandle <- file(destPath, open="wb")
    writeBin(binData,destFileHandle)
    close(destFileHandle)
  }
  # unzip bz2 file to csv
  if(!file.exists("./StormData.csv")){
    filePath <- "./StormData.csv.bz2"
    destPath <- "./StormData.csv"
    bunzip2(filePath,destPath,overwrite=TRUE, remove=FALSE)
  }
  
  #Read the source .csv file
  
  if(dataProcess){
    csvStormData <- read.csv("./StormData.csv")
  }

   #Refactor BGN_DATE, determine the offset year to use, and reduce the dataset
  
  Since the later years account for more observations, results could be skewed by the first years. By still using the majority of the observations, the cutoff point is arbritrarely set at 75%
  
  if(dataProcess){
    # get some counts
    totalNumberOfObservations <- nrow(csvStormData)
    cutOffPercentage = 0.75
    cutOffObservations = round(totalNumberOfObservations * cutOffPercentage)
    
    # add columns for date calculations based on BGN_DATEro
    csvStormData$year = as.numeric(format(as.Date(csvStormData$BGN_DATE, format = "%m/%d/%Y"), "%Y"))
    
    # create dataset with count per year, reverse the recordset, create running total 
    yearRecords <- count(csvStormData, "year")
    yearRecords <- yearRecords[order(yearRecords$year, decreasing=TRUE), ]
    yearRecords$runningTotal = cumsum(yearRecords$freq)
    cutOffYear <- min(yearRecords[yearRecords$runningTotal < cutOffObservations, 1])
    
    # reduce the dataset
    csvStormData <- csvStormData[csvStormData$year >= cutOffYear, ]
    endYear <- max(csvStormData$year)
    
    # clean csvStormData
    csvStormData$BGN_DATE <- NULL
    rownames(csvStormData) <- NULL
  }

  #Refactor EVTYPE into 11 levels
  
  The EVTYPE contains ca. 985 unique source events. Many of them can be reduced to similar instances. In this instance there are 11 levels defined, covering effectifly the majority and all useful data records (summaries and combinations are skipped)
  
  if(dataProcess){
    csvStormData$damageSource <- NA
    
    csvStormData[grepl("precipitation|rain|hail|drizzle|wet|percip|burst|depression|fog|wall cloud", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Precipitation & Fog"
    csvStormData[grepl("wind|storm|wnd|hurricane|typhoon", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Wind & Storm"
    csvStormData[grepl("slide|erosion|slump", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Landslide & Erosion"
    csvStormData[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|record temperature|record high", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Heat & Drought"
    csvStormData[grepl("cold|cool|ice|icy|frost|freeze|snow|winter|wintry|wintery|blizzard|chill|freezing|avalanche|glaze|sleet", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Snow & Ice"
    csvStormData[grepl("flood|surf|blow-out|swells|fld|dam break", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Flooding & High Surf"
    csvStormData[grepl("seas|high water|tide|tsunami|wave|current|marine|drowning", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "High seas"
    csvStormData[grepl("dust|saharan", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Dust & Saharan winds"  
    csvStormData[grepl("tstm|thunderstorm|lightning", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Thunderstorm & Lightning"
    csvStormData[grepl("tornado|spout|funnel|whirlwind", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Tornado"
    csvStormData[grepl("fire|smoke|volcanic", 
                           csvStormData$EVTYPE, ignore.case = TRUE), "damageSource"] <- "Fire & Volcanic activity"
    
    # remove uncategorized records (damageSource == NA) & cast as factor
    csvStormData <- csvStormData[complete.cases(csvStormData[, "damageSource"]), ]
    csvStormData$damageSource <- as.factor(csvStormData$damageSource)
    
    # clean csvStormData
    csvStormData$EVTYPE <- NULL
  }
  
  
  #Refactor PROPDMG, CROPDMG, PROPDMGEXP & CROPDMGEXP to absolute damage values
  
  Format the DMG and DMGEXP fields in absolute values. Undefined EXP properties, like +, ?, make the record NA
  
  if(dataProcess){
    # function to convert symbol to a power of 10 (for use with PROPDMGEXP & CROPDMGEXP)
    toTenPower <- function(x){
      if(is.numeric(x)) {
        x <- x
      }
      else if(grepl("h", x, ignore.case=TRUE)) {
        x <- 2
      }
      else if(grepl("k", x, ignore.case=TRUE)) {
        x <- 3
      }
      else if(grepl("m", x, ignore.case=TRUE)) {
        x <- 6
      }
      else if(grepl("b", x, ignore.case=TRUE)) {
        x <- 9
      }
      else if(x == "" || x == " "){
        x <- 0
      }
      else{
        x <- NA
      }
      x
    }
    
    # function to take two parameters num and exp and convert it to one absolute value. non integers become 0
    calculateAmount <- function(num, exp){
      pow <- toTenPower(exp)
      if(is.numeric(num)){
        num <- num * (10 ^ pow)
      }
      
      if(!is.numeric(num)){
        num <- 0
      }
      
      num
    }
    
    # create 2 new fields for calculated propDamage & cropDamage and add them to one damageTotal field
    csvStormData$propDamage <- mapply(calculateAmount, csvStormData$PROPDMG, csvStormData$PROPDMGEXP)
    csvStormData$cropDamage <- mapply(calculateAmount, csvStormData$CROPDMG, csvStormData$CROPDMGEXP)
    csvStormData$damageTotal = csvStormData$propDamage + csvStormData$cropDamage
    
    # clean csvStormData
    csvStormData$PROPDMG <- NULL
    csvStormData$PROPDMGEXP <- NULL
    csvStormData$CROPDMG <- NULL
    csvStormData$CROPDMGEXP <- NULL
  }
  
  #Create aggregated datasets and variables for plots
  
  #The final data frames must be recast to be used in certain plot funtions
  
  if(dataProcess){
    # aggregate economic damage per damageSource
    sumEconomicDamage <- aggregate(formula=cbind(propDamage, cropDamage, damageTotal) ~ damageSource, data=csvStormData, FUN=sum, na.rm=TRUE)
    sumEconomicDamage <- sumEconomicDamage[order(sumEconomicDamage$damageTotal, decreasing=TRUE),]
    rownames(sumEconomicDamage) <- NULL
    sumEconomicDamage$damageSource <- factor(sumEconomicDamage$damageSource, levels=rev(sumEconomicDamage$damageSource))
    
    # melt the sumEconomicDamage into data frame to be used as bar chart
    meltSumEconomicDamage <- melt(sumEconomicDamage, id.vars=c("damageSource"), measure.vars=c("propDamage","cropDamage"), variable.name="damageType", value.name="damage")
    levels(meltSumEconomicDamage$damageType)[levels(meltSumEconomicDamage$damageType)=="propDamage"] <- "property"
    levels(meltSumEconomicDamage$damageType)[levels(meltSumEconomicDamage$damageType)=="cropDamage"] <- "crops"
    
    # aggregate humanitarian damage per damageSource
    sumHumanDamage <-aggregate(formula=cbind(INJURIES, FATALITIES) ~ damageSource, data=csvStormData, FUN=sum, na.rm=TRUE) 
    sumHumanDamage <- sumHumanDamage[order(sumHumanDamage$INJURIES, decreasing=TRUE),]
    rownames(sumHumanDamage) <- NULL
    sumHumanDamage$damageSource <- factor(sumHumanDamage$damageSource, levels=rev(sumHumanDamage$damageSource))
    
    # define max values for bar chart scale
    maxInjuries <- max(sumHumanDamage$INJURIES)
    maxInjuries <- maxInjuries + round(maxInjuries * 0.25)
    
    maxFatalities <- max(sumHumanDamage$FATALITIES)
    maxFatalities <- maxFatalities + round(maxFatalities * 0.25)  
  }
  
  #Save reducedStormData et al to RData file
  
  #Save the processed data to an RData file
  
  if(dataProcess){
    save(csvStormData, 
         sumHumanDamage, 
         meltSumEconomicDamage,
         sumEconomicDamage, 
         maxInjuries, 
         maxFatalities,
         cutOffYear,
         endYear,
         file="./StormData.RData")
  }
  
  ##4. Results
  
  #Show the first & last 5 lines of the new data set
  
  #Display a few records of the cleaned, reformatted stormData to be used for analysis
  
  head(csvStormData, n=5L)
  tail(csvStormData, n=5L)
  
  #Injuries vs. Fatalities
  
  #Juxtaposed the injuries and fatalaties for each major weather event, orderd by number of injuries You can see that the top 5 for both contain the same events. But Heat & Drought has more casulties than Tornado's, by far the #1 in injuries. High seas have almost as much casulties as injuries, so it has the worst odds of survival of the list.
  
  # add middle column with just damageSource labels
  g.mid <- ggplot(data=sumHumanDamage, aes(x=1,y=damageSource)) +
  geom_text(aes(label=damageSource), size=4) +
  ggtitle("") +
  ylab(NULL) +
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065)) +
  theme(axis.title=element_blank(),
  panel.grid=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank(),
  panel.background=element_blank(),
  axis.text.x=element_text(color=NA),
  axis.ticks.x=element_line(color=NA),
  plot.margin = unit(c(1,-1,1,-1), "mm"))
  
  # add left chart with injuries
  g.injuries <- ggplot(data=sumHumanDamage, aes(x=damageSource, y=injuries)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=injuries), size=3, vjust=0.5, hjust=2.0) +
  ggtitle("Injuries") +
  scale_y_reverse(expand=c(0, 0), limits=c(maxInjuries,0)) + 
  coord_flip() +
  theme(axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  axis.text.y = element_blank(), 
  axis.ticks.y = element_blank(), 
  plot.margin = unit(c(1,-1,1,0), "mm")) 
  
  # add right chart with fatalities
  g.fatalities <- ggplot(data=sumHumanDamage, aes(x=damageSource, y=fatalities)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label=fatalities), size=3, vjust=0.5, hjust=-1.0) +
  ggtitle("Fatalities") +
  scale_y_continuous(expand=c(0, 0), limits=c(0,maxFatalities)) + 
  coord_flip() +
  theme(axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  axis.text.y = element_blank(), 
  axis.ticks.y = element_blank(), 
  plot.margin = unit(c(1,0,1,-1), "mm")) 
  
  # combine charts in one plot
  gg.injuries <- ggplot_gtable(ggplot_build(g.injuries))
  gg.fatalities <- ggplot_gtable(ggplot_build(g.fatalities))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))
  
  grid.arrange(gg.injuries,gg.mid,gg.fatalities,
  ncol=3,widths=c(4/10,2/10,4/10),
  main=paste("Aggregated human injuries & fatalities for weather events from ",cutOffYear," to ",endYear, sep=""))
  
  #The underlying data
  
  #sumHumanDamage
  
  #Economic Damage
  
  #Crop damage is hardly a factor in comparission to the total economic cost of certain weather events, except for Heat & Drought, where it effects more than 90% The real interesting ones are Wind & Storm and Flooding & High Surf covering together more than 80% of all economic damage over all the years. Tornado's,Thunderstorms and Snow & Ice, which have high impact in human costs, hardly matter in economic damages
  
  ggplot(meltSumEconomicDamage, aes(x=damageSource, y=damage/1000000)) + 
  geom_bar(stat = "identity", aes(fill=damageType)) +
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, size=8, hjust = 1, vjust = 1)) +
  ylab("Total Damage (millions of USD)") +
  ggtitle(paste("Aggregated property and crop damage for weather events from ",cutOffYear," to ",endYear, sep=""))
  
  #The underlying data
  
  #sumEconomicDamage
