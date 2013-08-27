## change this to TRUE to switch no name version, change ui.R and server.R
private = TRUE

################################################################################
## Define necessary global variables
################################################################################

# global vector for storing a list of conversations in a time period
conversationChoice <- c("None")

# global vector for storing a list of frequencies for all buildings
freqBuildings <- data.frame()
freqBuildingInit <- function(){
  freqBuildings <<- data.frame(Pos.Games = c(0),
                              Pos.Orientation = c(0),
                              Pos.Gym = c(0),
                              Pos.Classroom = c(0),
                              Pos.Restaurant = c(0),
                              Pos.Clothier = c(0),
                              Pos.Pharmacy = c(0),
                              Pos.Grocery = c(0),
                              Pos.Bookstore = c(0),
                              Pos.Outdoor = c(0))
}

# global vector for storing a list of 20 logKeys
logKeys <- read.csv("log_key_list.csv", header = FALSE, sep = ",")
logKeyTable <- as.character(logKeys$V1)

# global vector for storing a list of 20 subject names
subjectNames <- read.csv("name_of_subjects.csv", header = FALSE, sep = ",")
nameList <- as.character(subjectNames$V1)

################################################################################
## Functions dealing with all lists of necessary information
################################################################################

## idenfity Month from string
identifyMonth <- function(str){
  switch(str,
        'JAN' = 01,
        'FEB' = 02,
        'MAR' = 03,
        'APR' = 04,
        'MAY' = 05,
        'JUN' = 06,
        'JUL' = 07,
        'AUG' = 08,
        'SEP' = 09,
        'OCT' = 10,
        'NOV' = 11,
        'DEC' = 12
        )
}

identifyCategory <- function(str){
  switch(str,
        'BOOK' = 1,
        'DRUG' = 2,
        'PHAR' = 3,
        'MENU' = 4,
        'DOOR' = 5,
        'WBST' = 6,
        'FOOD' = 7,
        'VDEO' = 8,
        'OBJT' = 9,
        'Reserved' = 10
        )
}

## list of logType values
objTypeVector <- function(){
  return(c('BOOK',
          'DRUG',
          'PHAR',
          'MENU',
          'DOOR',
          'WBST',
          'FOOD',
          'VDEO',
          'OBJT'
          )
  )
}

## for object map legend, object type in English words
objTypeForLegend <- function(){
  return(c('Book',
          'Drug',
          'Pharmacy',
          'Menu in Restaurant',
          'Door',
          'Website',
          'Food',
          'Video',
          'Object'
          )
  )
}

distance <- function(x1, y1, x2, y2){
  a <- sqrt((as.numeric(x1)-as.numeric(x2))*(as.numeric(x1)-as.numeric(x2)) + (as.numeric(y1)-as.numeric(y2))*(as.numeric(y1)-as.numeric(y2)))
  return(as.numeric(a))
}

identifyLocation <- function(posX, posY){
  if ((distance(posX, posY, 37, 220)) < 25){
  	return(1) 
  	# "Games"
  }
  else if ((distance(posX, posY, 60, 190)) < 25){
  	return(2) 
  	# "Orientation"
  }
  else if ((distance(posX, posY, 45, 50)) < 40){
  	return(3) 
  	# "Gym"
  }
  else if ((distance(posX, posY, 125, 125)) < 30){
  	return(4) 
  	# "Classroom"
  }
  else if ((distance(posX, posY, 215, 50)) < 30){
  	return(5) 
  	# "Restaurant"
  }
  else if ((distance(posX, posY, 225, 140)) < 15){
  	return(6) 
  	# "Clothier"
  }
  else if ((distance(posX, posY, 225, 170)) < 15){
  	return(7) 
  	# "Pharmacy"
  }
  else if ((distance(posX, posY, 220, 205)) < 30){
  	return(8) 
  	# "Grocery"
  }
  else if ((distance(posX, posY, 180, 215)) < 20){
  	return(9) 
  	# "Bookstore"
  }
  else{
  	return(10) 
  	# "Outdoor"
  }
}

buildingToCoorID <- function(building){
  switch(building,
         "Clothier" = 1,
         "Pharmacy" = 2,
         "Grocery" = 3,
         "Bookstore" = 4,
         "Restaurant" = 5,
         "Classroom" = 6,
         "Gym" = 7,
         "Games" = 8,
         "Orientation" = 9,
         "Outdoor" = 10)
}

## identify subjects for name version
inputSubWithName <- function(subName){
  if (length(subName) > 0){
    if (subName == nameList[1]) return(1)
    else if (subName == nameList[2]) return(2)
    else if (subName == nameList[3]) return(3)
    else if (subName == nameList[4]) return(4)
    else if (subName == nameList[5]) return(5)
    else if (subName == nameList[6]) return(6)
    else if (subName == nameList[7]) return(7)
    else if (subName == nameList[8]) return(8)
    else if (subName == nameList[9]) return(9)
    else if (subName == nameList[10]) return(10)
    else if (subName == nameList[11]) return(11)
    else if (subName == nameList[12]) return(12)
    else if (subName == nameList[13]) return(13)
    else if (subName == nameList[14]) return(14)
    else if (subName == nameList[15]) return(15)
    else if (subName == nameList[16]) return(16)
    else if (subName == nameList[17]) return(17)
    else if (subName == nameList[18]) return(18)
    else if (subName == nameList[19]) return(19)
    else if (subName == nameList[20]) return(20)
  }
  else{
    return(21)
  }
}

## identify subjects for no name version
inputSubNoName <- function(subName){
  switch(subName,
         "S1" = 1, 
         "S2" = 2, 
         "S3" = 3,
         "S4" = 4,
         "S5" = 5,
         "S6" = 6,
         "S7" = 7,
         "S8" = 8,
         "S9" = 9,
         "S10" = 10,
         "S11" = 11,
         "S12" = 12,
         "S13" = 13,
         "S14" = 14,
         "S15" = 15,
         "S16" = 16,
         "S17" = 17,
         "S18" = 18,
         "S19" = 19,
         "S20" = 20)
}

## Given a logKey, identify the subject number 1-20
identifySub <- function(subLogKey){
  if (length(subLogKey) > 0){
    if (logKeyTable[1] == subLogKey) return(1)  #  1
    else if (logKeyTable[2] == subLogKey) return(2)  #  2
    else if (logKeyTable[3] == subLogKey) return(3)  #  3
    else if (logKeyTable[4] == subLogKey) return(4)  #  4
    else if (logKeyTable[5] == subLogKey) return(5)  #  5
    else if (logKeyTable[6] == subLogKey) return(6)  #  6
    else if (logKeyTable[7] == subLogKey) return(7)  #  7
    else if (logKeyTable[8] == subLogKey) return(8)  #  8
    else if (logKeyTable[9] == subLogKey) return(9)  #  9
    else if (logKeyTable[10] == subLogKey) return(10)  #  10
    else if (logKeyTable[11] == subLogKey) return(11)  #  11
    else if (logKeyTable[12] == subLogKey) return(12)  #  12
    else if (logKeyTable[13] == subLogKey) return(13)  #  13
    else if (logKeyTable[14] == subLogKey) return(14)  #  14
    else if (logKeyTable[15] == subLogKey) return(15)  #  15
    else if (logKeyTable[16] == subLogKey) return(16)  #  16
    else if (logKeyTable[17] == subLogKey) return(17)  #  17
    else if (logKeyTable[18] == subLogKey) return(18)  #  18
    else if (logKeyTable[19] == subLogKey) return(19)  #  19
    else if (logKeyTable[20] == subLogKey) return(20)  #  20
    else return(21)
  }
  else return(21)
}

## Given a logKey, idenfity the survey ID
logToSurvey <- function(subLogKey){
  if (length(subLogKey) > 0){
    if (logKeyTable[1] == subLogKey) return(47)  #  1
    else if (logKeyTable[2] == subLogKey) return(40)  #  2
    else if (logKeyTable[3] == subLogKey) return(0)  #  3
    else if (logKeyTable[4] == subLogKey) return(30)  #  4
    else if (logKeyTable[5] == subLogKey) return(44)  #  5
    else if (logKeyTable[6] == subLogKey) return(45)  #  6
    else if (logKeyTable[7] == subLogKey) return(50)  #  7
    else if (logKeyTable[8] == subLogKey) return(46)  #  8
    else if (logKeyTable[9] == subLogKey) return(26)  #  9
    else if (logKeyTable[10] == subLogKey) return(29)  #  10
    else if (logKeyTable[11] == subLogKey) return(42)  #  11
    else if (logKeyTable[12] == subLogKey) return(22)  #  12
    else if (logKeyTable[13] == subLogKey) return(16)  #  13
    else if (logKeyTable[14] == subLogKey) return(13)  #  14
    else if (logKeyTable[15] == subLogKey) return(28)  #  15
    else if (logKeyTable[16] == subLogKey) return(17)  #  16
    else if (logKeyTable[17] == subLogKey) return(48)  #  17
    else if (logKeyTable[18] == subLogKey) return(51)  #  18
    else if (logKeyTable[19] == subLogKey) return(15)  #  19
    else if (logKeyTable[20] == subLogKey) return(19)  #  20
    else return(0)
  }
  else return(0)
}

################################################################################
# Return vector of participants' color assignments. Note: there's an extra at 
# the end in case an "other” color is needed for non-participant logKeys.
################################################################################

pClrVector <- function() {
  a <- rainbow(21)
  return(a)
}

objClrVector <- function() {
  a <- rainbow(10)
  return(a)
}

################################################################################
# Returns vector of pch (shapes)
################################################################################

pPchVector <- function() {
  return(c(1, 15, 2, 16, 3,
           17, 4, 18, 5, 19,
           6, 20, 7, 15, 8,
           16, 9, 17, 10, 18,
           11)
  )
}

objPchVector <- function() {
  return(c(1, 15, 2, 16, 3,
           17, 4, 18, 5, 19)
  )
}

################################################################################
# Give it a week, it returns the start day and the end day (inclusive).
# NOTE: the "week" input parameter is 1-based but the output values are 0-based.
################################################################################

daysFromWeek <- function(week) {
  daysPerWeek = 7
  startDay = (week - 1) * daysPerWeek
  endDay = startDay + daysPerWeek - 1

  if (endDay > 179) {
    endDay = 179
  }

  return(c(startDay, endDay))
}

################################################################################
# Object decoding functions
# Look for detailed object information from TBAUTH files
################################################################################

decodeItem <- function(type, id){
  if (type == "BOOK"){
    data <- bookData
  }
  else if (type == "FOOD"){
    data <- groceryData
  }
  else if (type == "DRUG"){
    data <- drugData
  }
  else if (type == "PHAR"){
    data <- pharData
  }
  else if (type == "WBST"){
    data <- webData
  }
  else{
    data <- groceryData
  }
  
  firstCol <- as.character(data[ , 1])
  
  if (type == "DOOR"){
    str <- paste(id, sep = "")
  }
  else{
    str <- "Not Found"
  }
  for (i in 1:length(firstCol)){
    if (firstCol[i] == id){
      str <- paste(substr(as.character(data[i, 2]), 1, 40),
                   "__",
                   substr(as.character(data[i, 3]), 1, 70),
                   sep = "")
    }
  }
  return(str)
}


################################################################################
# Returns number of data points for this time period. 
# Warning: writing to pCount as a global variable.
# Plotting using Daycount or Real Dates based on user choice
# Date data correctness waiting to be verified as of 07/01
################################################################################

plotTimePeriod <- function(dayCountPeriod, datePeriod, timePeriod, 
                           periodType, pData, pClrs, pPch, plotTitle,
                           subjectKey, plotType) {
  
  if (periodType == "dayCount"){
    days = daysFromWeek(dayCountPeriod[1]) # find starting week
    startDay = days[1] # first day of starting week
    days2 = daysFromWeek(dayCountPeriod[2])  # find ending week
    endDay = days2[2]  # last day of ending week
  
    timePeriodLength = endDay - startDay + 1 # includes the last day
    plotTitle = paste(plotTitle, " (", timePeriodLength, " days)",
                      sep = "", collapse = NULL)
    
    datasubset <- subset(pData, (dayCount >= startDay & dayCount <= endDay))
  }
  
  else if (periodType == "realDate"){
    ## includsive of the boundaries, day difference + 1
    timePeriodLength = as.numeric(datePeriod[2] - datePeriod[1]) + 1
    plotTitle = paste(plotTitle, " (", timePeriodLength, " days)",
                      sep = "", collapse = NULL)
    
    ## filter data based on logDate
    datasubset <- subset(pData, (logDate >= datePeriod[1] &
                                 logDate <= datePeriod[2]))
    
    ## choose a specific time period in a day
    ## NOTE: this only gives correct data if the start and endDate is the same
    if (!is.na(timePeriod[1]) & !is.na(timePeriod[2])){
      if ((timePeriod[1] != 0) & (timePeriod[2] != 0)){
        datasubset <- subset(datasubset,
                             (logTime >= timePeriod[1] &
                              logTime <= timePeriod[2]))
        plotTitle = paste(plotTitle,
                          "\nTime From ",
                          timePeriod[1],
                          " to ",
                          timePeriod[2],
                          " (",
                          (timePeriod[2]-timePeriod[1]),
                          " in length)",
                          sep="",
                          collapse = NULL)
      }
    }
  }
  
  if (plotType == "object" | plotType == "objectByType" |
      plotType == "objShadesBuilding"){
    datasubset <- subset(datasubset, (as.character(logItem) != ""))
  }
  
  ## filter the data to get chosen participants, if not all chosen
  if (length(subjectKey) != 20){
  	## gather all the requested logKey
    participant <-c()
    for (i in 1:length(subjectKey)){
      participant <- c(participant,logKeyTable[subjectKey[i]])
    }
    datasubset <- subset(datasubset, logKey %in% participant)
  }
  
  ## Assign color to each point according to the participant it belongs to
  clrs <- c()
  pchVals <- c()      
  
  ## assign color and shape for each point
  if (length(datasubset$logKey) >= 1){
  	
  	## if plot by objectByType is requested
  	if (plotType == "objectByType"){
      for(j in 1:length(datasubset$logType)){        
        itemType <- as.character(datasubset$logType[j]) # Get logKey as String
        k <- identifyCategory(itemType)       # Find participant number
        clrs <- c(clrs, objClrs[k])         # Assign the color to the data pt.
        pchVals <- c(pchVals, objPchs[k])    # Assign shape   
      }
    }
    
    else if (plotType == "shadesBuilding" | plotType == "objShadesBuilding"){
      freqCount <- data.frame(Pos.Games = c(0),
                          Pos.Orientation = c(0),
                          Pos.Gym = c(0),
                          Pos.Classroom = c(0),
                          Pos.Restaurant = c(0),
                          Pos.Clothier = c(0),
                          Pos.Pharmacy = c(0),
                          Pos.Grocery = c(0),
                          Pos.Bookstore = c(0),
                          Pos.Outdoor = c(0))
      for (i in 1:length(datasubset$logPosX)){
        if (!is.na(datasubset$logPosX[i]) & !is.na(datasubset$logPosY[i])){
          loc <- identifyLocation(as.numeric(datasubset$logPosX[i]), 
                                  as.numeric(datasubset$logPosY[i]))
          freqCount[loc] <- freqCount[loc] + 1
        }
      }
      freqCount <- freqCount[order(freqCount, decreasing = TRUE)]
      freqBuildings <<- freqCount
    }
    
    ## otherwise, normal plot by subjects
  	else{
      for(j in 1:length(datasubset$logKey)){        
        subLogKey <- as.character(datasubset$logKey[j]) # Get logKey as String
        k <- identifySub(subLogKey)       # Find participant number
        clrs <- c(clrs, pClrs[k])         # Assign the color to the data pt.
        pchVals <- c(pchVals, pPch[k])    # Assign shape   
      }
    }
  }
  else{
    freqBuildingInit()
  }
  
  if (plotType != "shadesBuilding" & plotType != "objShadesBuilding"){
    ## actual plot function
    plot(datasubset$logPosX,
         datasubset$logPosY,
         ylim = c(0,256),
         xlim = c(0,256),
         cex = 1.5,
         xlab = "X",
         ylab = "Y",
         pch=pchVals,
         col=clrs,
         main = plotTitle
    )
  }
  else{
    plot(0,
         0,
         ylim = c(0,256),
         xlim = c(0,256),
         cex = 1.5,
         xlab = "X",
         ylab = "Y",
         pch = 1,
         col = "white",
         main = plotTitle
    )
  }
  
  ## if conversation plot, add in the icons @ location for conversations
  if (plotType == "conversation"){
  	
    ## subset based on real date chosen
    convSubset <- subset(conversationD,
                         (logDate >= datePeriod[1] &
                          logDate <= datePeriod[2]))
    if (!is.na(timePeriod[1]) & !is.na(timePeriod[2])){
      if ((timePeriod[1] != 0) & (timePeriod[2] != 0)){
        convSubset <- subset(convSubset, 
                             (logTime >= timePeriod[1] &
                              logTime <= timePeriod[2]))
      }
    }

    if (length(convSubset$Location) > 0){
      
      ## update conversationChoice vector
      conversationChoice <<- c(paste(convSubset$Location,
      	                      '__',
      	                      convSubset$logDate,
      	                      '__',
      	                      convSubset$logTime,
      	                      sep = "")
      	                     )
      
      ## count the number of each occurrence
      freqCount <- table(convSubset$Location)
      loc <- names(freqCount)
      
      for (i in 1:length(freqCount)){
      	if (freqCount[i] >0){
      	  addlogo(headPhone,
      	          c(xCoor(loc[i]), xCoor(loc[i])+20),
      	          c(yCoor(loc[i]), yCoor(loc[i])+20)
      	          )
      	  text(xCoor(loc[i])+20, yCoor(loc[i]), cex = 3, paste(freqCount[i]))
      	}
      }
    }
    else{
      conversationChoice <<- c("None")
    }
  }
  
  return(length(datasubset$logKey))
}

################################################################################
# Add formatted Date column into original data frame
################################################################################

## add fommated date to data frame, not unused since new csv file is used
# addFormatDate <- function(pData){
        
  # dateV <- c()
  # for (i in 1:length(pData$logKey)){
                
    # str <- as.character(pData$logDate[i])
                
    # day <- substr(str, 1, 2)
    # month <- as.character(identifyMonth(substr(str, 3, 5)))
    # year <- substr(str, 6, 9)
                
    # ## combine to get date
    # cDate <- paste(year, '-', month, '-', day, sep='')
                
    # dateV <- c(dateV, as.Date(0, origin=cDate))
  # }
  
  # pData$logDate <- dateV
  
  # return(pData)
# }

################################################################################
# Add building outlines and labels. Reference for outlines: 
# http://stat.ethz.ch/R-manual/R-patched/library/graphics/html/polygon.html)
################################################################################

xCoorText <- function(str){
  switch(str,
         'Classroom' = 82,
         'Games' = 37,
         'Orientation' = 89,
         'Bookstore' = 185,
         'Grocery' = 220,
         'Pharmacy' = 226,
         'Clothier' = 226,
         'Restaurant' = 229,
         'Gym' = 30,
         'Outdoor' = 180)
}

yCoorText <- function(str){
  switch(str,
         'Classroom' = 130,
         'Games' = 244,
         'Orientation' = 185,
         'Bookstore' = 230,
         'Grocery' = 230,
         'Pharmacy' = 187,
         'Clothier' = 120,
         'Restaurant' = 25,
         'Gym' = 25,
         'Outdoor' = 150)
}

## take "community" as "classroom"
xCoor <- function(str){
  switch(str,
         "Community" = 82,
         "Grocery" = 220,
         "Gym" = 30,
         "Patio" = 180,
         "Pharmacy" = 236,
         "Restaurant" = 229,
         "Welcome" = 89)
}

yCoor <- function(str){
  switch(str,
         "Community" = 130,
         "Grocery" = 230,
         "Gym" = 29,
         "Patio" = 150,
         "Pharmacy" = 178,
         "Restaurant" = 28,
         "Welcome" = 185)
}

addBuildings <- function(datacoord, shadesBoolean) {
  if (shadesBoolean == 0){
    for (m in 1:10) {
      datacoordsub <- subset(datacoord, ID == m)
      polygon(x = datacoordsub$X, y = datacoordsub$Y,
              density = 0,
              col = "blue",
              border = c("black", "yellow"),
              lwd = 1, lty = c("solid", "solid"))
    }
        
    text(82, 130, cex = 1.2, 'Classroom')
    text(37, 241, cex = 1.2, 'Games')
    text(89, 185, cex = 1.2, 'Orientation')
    text(185, 230, cex = 1.2, 'Bookstore')
    text(220, 230, cex = 1.2, 'Grocery')
    text(251, 173, cex = 1.2, 'Pharmacy')
    text(247, 143, cex = 1.2, 'Clothier')
    text(229, 28, cex = 1.2, 'Restaurant')
    text(30, 29, cex = 1.2, 'Gym')
  }
  else{
  	if (shadesBoolean == 1){
      colorPlates <- brewer.pal(9, "Blues")
    }else{
      colorPlates <- brewer.pal(9, "Reds")
    }
    colorIndex <- 9
    for (m in 1:10) {
      buildingNameExtra <- names(freqBuildings)[m]
      buildingName <- substr(buildingNameExtra, 5, 
                             nchar(buildingNameExtra, type = "chars", 
                                   allowNA = FALSE))
      buildingID <- buildingToCoorID(buildingName)
      datacoordsub <- subset(datacoord, ID == buildingID)
      polygon(x = datacoordsub$X, y = datacoordsub$Y,
              col = colorPlates[colorIndex],
              border = c("black", "yellow"),
              lwd = 1, lty = c("solid", "solid"))
      if (buildingID != 10){
        colorIndex <- colorIndex - 1
      }
      text(xCoorText(buildingName), yCoorText(buildingName), cex = 1.2, 
           paste(buildingName, freqBuildings[m]))
    }
  }
}

################################################################################
# Color legend plus statistics for each participant for each time period
################################################################################
displayColorLegend <- function(labels, colors,
                               shapes, legendType) {
  x = 0
  yTop = 250
  lineSpacing = 265 / (length(labels))
  cexVal = 1.55
  
  shapeX = c()
  shapeY = c()
  for (i in 1:length(labels)) {
    shapeX = c(shapeX, x - 5)
    shapeY = c(shapeY, yTop - ((i - 1) * lineSpacing) + 2 )  
  }
  
  ## create legend title based on private/public mode
  legendTitle <- ''
  
  if (legendType == "subject"){
    if (private == FALSE){
  	  legendTitle <- "Legend : with logKey and Survey ID (full mode)"
    }
    else {
      legendTitle <- "Legend : subject information not displayed (public mode)"
    }
  }
  else if (legendType == "object"){
    legendTitle <- "Legend : plot based on object type"
  }
  
  par(family="sans")
  plot(shapeX, shapeY, ylim = c(0,256), xlim = c(0,256),
       pch = shapes, col = colors,
       yaxt = 'n',
       ylab = "",
       xaxt = 'n',
       xlab = "",
       main = legendTitle,
       cex = cexVal,
       col.main = "#000055" #,
       #sub = "Includes numbers of data points per time period per participant"
  )

  par(family = "mono")

  for (i in 1:length(labels)) {
    
    if (legendType == "subject"){
      n <- labels[i]
      pStr = sprintf("S%s", labels[i])
      pStr2 = sprintf("%s", logKeyTable[n])
      pStr3 = sprintf("%s", logToSurvey(logKeyTable[n]))
    
      if (private == FALSE) {
        tStr = paste(pStr,": ", pStr2, " (Survey ID: ", pStr3, ")",
                     collapse = NULL)
      }
      else {
        tStr = paste(pStr, collapse = NULL)
      }
    }
    else if (legendType == "object"){       
      tStr = sprintf("Type %s -- %s", labels[i], objTypesLegend[i])
    }
    
    text(x ,
         yTop - ((i-1) * lineSpacing),
         labels = tStr,
         adj = c(0,0),
         col = colors[i],
         cex = 1.35
         )
  }
}

################################################################################
# Main program
# Define all global variables for use
################################################################################

library(shiny)        #load shiny library for interactive map
library(MASS)
library(chron)        #load chron library for handling time objects
library(pixmap)       #load pixmap library for handling headphone logo
library(RColorBrewer) #load colorBrewer library for diff shades of colors

# setwd('~/desktop/research/shiny_CheckBoxMap/')       # Set working directory
# pdf("ActivityMap-weekly.52.pdf")                     # Open output file

# read in headphone icon for conversation map
headPhone <- read.pnm("headphonePPM.ppm")

## 07/03 modified visdata.csv into visdata_updatedDate.csv, 
## with logDate transformed into R's format
data <- read.csv('visdata_updatedDate.csv', header = TRUE)  # Import data
datacoord <- read.csv('coordinates.csv', header = TRUE)     # Import map data

# Import survey answers data, final version
surveyQ <- read.csv('Final_Survey_Data.csv', header = TRUE) 

# Import conversation Location, Date, Time. The file was reformatted.
conversationD <- read.csv('conversation_Reformatted.csv', header = TRUE)
conversationD$logDate <- as.Date(conversationD$logDate,
  	                                 origin = "1970-01-01")
conversationD$logTime <- times(conversationD$logTime)
# A vector for possible subjects involved in a conversation
possibleSub <- c("None")

bookData <- read.csv("tbBookstore.csv")
groceryData <- read.csv("tbGrocery In Use.csv")
drugData <- read.csv("tbPrescriptions.csv")
pharData <- read.csv("tbPharmacy.csv")
webData <- read.csv("tbWebsites.csv")

userGuideInfo <- read.csv("userGuide.csv")
userGuide <- userGuideInfo$x
versionInfo <- read.csv("updateInfo.csv")
versionGuide <- versionInfo$x


participantClrs = pClrVector()      # Set up colors vector
participantShapes = pPchVector()    # Set up shapes vector
objTypes = objTypeVector()          # Set up a vector of objType values
objTypesLegend = objTypeForLegend() # Set up a vector for objType legend's use
objClrs = objClrVector()            # Set up colors vector for objType plot
objPchs = objPchVector()            # Set up shapes vector for objType plot

## other variabl initiations
# data frame for putting all requested usr together
plotData <- data.frame() 
plotDataSurvey <- data.frame() 

# vector for storing colors & shapes for subjects (weight and HBA1C plot)
clrs <- c()
pchVals <- c()
# for survey data plot
clrsS <- c()
pchValsS <- c()

## read in the clinical data and get rid of empty rows
clinicalData <- read.csv("Clinicaldata_activity_density_map.csv",
                         header=T,
                         sep=",")
                         
## delete empty rows from original data
clinicalSub <- subset(clinicalData, !is.na(BCALCULATED.BMI))  
## rename the rows by number
row.names(clinicalSub) <- c(1:length(clinicalSub$BCALCULATED.BMI)) 

## boolean for checking whether logDate is formatted
unformatted = TRUE 


# Select subset of data to plot - uncomment the desired subset:
# WARNING: May not work unless you pick “Only particpants” (SAC 2013/03/25) 
        #ALL DATA
        #        datasubset <- data
        #ONLY LANIEJ
        #        datasubset <- subset(data,
        #        logKey == 'b6efa60c-ef2c-43ec-9814-9b52f6bd12aa')
        #DATE RANGE
        #        datasubset <- subset(data,
        #        as.Date(data$logDate,format = '%m/%d/%Y') >= '2011-11-15' & 
        #        as.Date(data$logDate,format = '%m/%d/%Y') <= '2011-12-05')
        #ONLY PARTICIPANTS
                participantData <- subset(data, logKey %in% logKeyTable)
                # participantData <- data.frame(participantData$logKey,
                                              # participantData$logPosX,
                                              # participantData$logPosY,
                                              # participantData$logItem,
                                              # participantData$logDate,
                                              # participantData$logTime,
                                              # participantData$dayCount)
                # names(participantData) <- c("logKey",
                                            # "logPosX",
                                            # "logPosY",
                                            # "logItem",
                                            # "logDate",
                                            # "logTime",
                                            # "dayCount")
        
         #ONLY FLYING
        #        datasubset <- subset(data,
        #        logFly == 1)


## Unused/unchanged as of 06/29, by Ang
datasubsetLengths = c()
nTimePeriods = 26 # 180 days / 7 days/week = 25.71429 time periods
pCount = c(rep.int(0, (length(logKeyTable) * nTimePeriods)))
dim(pCount) = c(length(logKeyTable), nTimePeriods) # 2D: row: who; column: when


################################################################################
## shiny server code for interactive map starts here
################################################################################

shinyServer(function(input, output, session) {


################################################################################
## Input Updater for handling Check-All-Boxes
################################################################################
  
  choiceVecWithName <- c(nameList[1], 
                        nameList[2], 
                        nameList[3],
                        nameList[4],
                        nameList[5],
                        nameList[6],
                        nameList[7],
                        nameList[8],
                        nameList[9],
                        nameList[10],
                        nameList[11],
                        nameList[12],
                        nameList[13],
                        nameList[14],
                        nameList[15],
                        nameList[16],
                        nameList[17],
                        nameList[18],
                        nameList[19],
                        nameList[20])
               
  choiceVecNoName <- c("S1", 
                     "S2", 
                     "S3",
                     "S4",
                     "S5",
                     "S6",
                     "S7",
                     "S8",
                     "S9",
                     "S10",
                     "S11",
                     "S12",
                     "S13",
                     "S14",
                     "S15",
                     "S16",
                     "S17",
                     "S18",
                     "S19",
                     "S20")
  
  if (private == TRUE){
  	checkboxGroupName <- "subjectNoName"
  	choiceVec <- choiceVecNoName
  } else{
  	checkboxGroupName <- "subject"
  	choiceVec <- choiceVecWithName
  }
  
  ## boolean variable for keeping the status of the current checkAllBox system
  firstTime <- TRUE
  
  observe({
  	
  	## dealing with all-subjects checkbox
  	allSub <- input$checkAllSub
  	
  	## when "all subjects" is checked, check all boxes
  	if (allSub == TRUE){
  	  updateCheckboxGroupInput(session, 
  	                           checkboxGroupName, 
  	                           choices = choiceVec,
                               selected = choiceVec
      )
    }
    ## when "all subjects" is unchecked, uncheck all boxes
    else if (allSub == FALSE){        
      updateCheckboxGroupInput(session, 
  	                           checkboxGroupName, 
                               choices = choiceVec,
  	                           selected = NULL
      )
    }
  })

################################################################################
## output title text for subjects included/chosen
################################################################################
  
  ## Output String looks like "Subjects 1, 3" if subjects 1 and 3 are chosen
  
  formulaText <- reactive({
          
    inputSub <- subjectInput()
          
    subjects <- ''
          
    if (inputSub[1] == 21) {                      # no subject chosen
      subjects <- 'No Subjects Selected'
    } else if (length(inputSub) == 20) {          # all subjects were chosen
      subjects <- 'All Subjects'
    } else {                                      # some but not all were chosen
      if (length(inputSub) > 1) {                 # more than 1, less than 20
        subjects <- 'Subjects'
        for(i in 1:(length(inputSub) - 1)) {
          subjects <- paste(subjects, inputSub[i], ', ')
        }
      } else {                                    # just one participant
        subjects <- 'Subject'
      }

      subjects <- paste(subjects, inputSub[length(inputSub)])
    }
    
    # return final string
    paste(subjects)
  })

  ## Return the formula text for printing as a caption in the Main Window
  output$caption <- renderText({
    paste("Avatar Positions:", formulaText())
  })
  
  output$captionObject <- renderText({
    paste("Avatar Interactions with Virtual Objects:", formulaText())
  })
  
  output$captionConversation <- renderText({
    paste("Conversations with Position Data:", formulaText())
  })
  
################################################################################
## dynamically gather subject information according to user input
## dynamically set time period according to user input
## dynamically set choice of plot to view for Survey Plot
################################################################################
        
  ## subject choice
  subjectInput <- reactive({
  	
  	if (private == FALSE){
      n <- length(input$subject)
    }
    else{
      n <- length(input$subjectNoName)
    }
    
    # No subject chosen
    if (n == 0){
      return(21)
    } 
    else {  # Otherwise
  	  subAsNum <- c()
      if (private == FALSE){
        for (i in 1:length(input$subject)){
          subAsNum <- c(subAsNum, inputSubWithName(input$subject[i]))
        }
      } 
      else {
        for (i in 1:length(input$subjectNoName)){
          subAsNum <- c(subAsNum, inputSubNoName(input$subjectNoName[i]))
        }
      }
      return(subAsNum)    
    }
  })

  ## time period choice
  timeInput <- reactive({
    as.numeric(input$dayCountPeriod)
  })
        
  ## date period choice
  dateTimeInput <- reactive({
    return(input$dateRange)
  })
  
  dayTimeInput <- reactive({
  	## regex for the case when user enters a invalid time period
  	if (length(grep("[0-2][0-9]:[0-5][0-9]:[0-5][0-9]",
  	                input$startTime)) &
  	    length(grep("[0-2][0-9]:[0-5][0-9]:[0-5][0-9]",
  	                input$endTime))){
      return(c(times(input$startTime), times(input$endTime)))
  	}
  	else{
      return(c(0, 0))
    }
  })
  
  ## survey question plot choice, number corresponds to first col number to plot
  choiceReactive <- reactive({
    switch(input$choiceToView,
           "Perceived Usefulness" = 3, 
           "Perceived Ease Of Use" = 6, 
           "Diabetes Empowerment" = 9,
           "Diabetes Support" = 12,
           "Diet" = 15,
           "Exercise" = 18,
           "Blood Sugar Testing" = 21,
           "Diabetic Foot Care" = 24,
           "Diabetes Knowledge" = 27)
  })
  
  convInput <- reactive({
    str <- input$choiceConversation
    substr <- strsplit(str, "__")[[1]]
    
    if (substr[1] == "None"){
      substr <- c("____ place", as.character(input$dateRange[1]), "00:00:00")
    }
    
    return(substr)
  })
  
  objectPlotTypeInput <- reactive({
    switch(input$objPlotType,
           "bySubject" = "object",
           "byType" = "objectByType",
           "byFreq" = "objShadesBuilding"
    )
  })
  
  posPlotTypeInput <- reactive({
    switch(input$posPlotType,
           "bySubject" = "position",
           "byFreq" = "shadesBuilding"
    )
  })
  
  userRefChoiceInput <- reactive({
    return(as.character(input$userRefChoice))
  })
  
  versionChoiceInput <- reactive({
    return(as.character(input$versionChoice))
  })
        
################################################################################
## filter plot data from raw data
################################################################################

  ## Filter Clinial Data
  gatherData <- reactive({
                
    subjectID <- subjectInput() #interactive input from user
                
    ## <<- is assignment into global variable, 
    ## declared at the start of main program
  
    # empty data frame for putting all requested usr together
    plotData <<- data.frame()
    clrs <<- c()          # empty vector for storing colors for subjects
    pchVals <<- c()       # empty vector for storing shapes for subjects
    rName <- c()          # empty vector for updating row names
  
    ## obtaining all needed rows, assign color and shape
    for(i in 1:length(subjectID)){                         
      n <- subjectID[i]                # get subject number
      subKey <- logKeyTable[n]         # get subject's logKey
    
      ## row-combine all needed data
      plotData <<- rbind(plotData, subset(clinicalSub, logkey %in% subKey))                         
      # If this subject has valid clinal data on record
      if (subKey %in% clinicalSub$logkey){                                        
        rName <- c(rName, paste('S', n, 
                                sep = ""))  # Assign rowname: Subject Number
        clrs <<- c(clrs, participantClrs[n])  # Assign color to the data pt.
        pchVals <<- c(pchVals, participantShapes[n])  # Assign shape              
      }
    }
    row.names(plotData) <<- rName # Update all row names
  })
        
  ## Filter Survey Data
  gatherSurvey <- reactive({
                
    subjectID <- subjectInput() # interactive input from user
    choiceColNum <- choiceReactive() # choice of survey question plot
                
    ## <<- is assignment into global variable,
    ## declared at the start of main program
  
    # empty data frame for putting all requested usr together
    plotDataSurvey <<- data.frame()                 
    clrsS <<- c()         # empty vector for storing colors for subjects
    pchValsS <<- c() # empty vector for storing shapes for subjects
    rName <- c()          # empty vector for updating row names
        
    ## obtaining all needed rows, assign color and shape
    for(i in 1:length(subjectID)){                         
      n <- subjectID[i]                # get subject number
      logKey <- logKeyTable[n]         # get subject's logKey
      subID <- logToSurvey(logKey)     # get subject's survey ID
          
      plotDataSurvey <<- rbind(plotDataSurvey,
                               subset(surveyQ,
                               redcap_survey_identifier %in% subID)
                               )
      # If this subject has valid survey data         
      if (subID %in% surveyQ$redcap_survey_identifier){                                 
        rName <- c(rName, paste('S', n, sep = ""))  # Assign new row name
        clrsS <<- c(clrsS, participantClrs[n])    # Assign color to the data pt
        pchValsS <<- c(pchValsS, participantShapes[n])    # Assign shape      
      }
    }            
    row.names(plotDataSurvey) <<- rName # Updata all row names        
  })
        
  ## Filter item data, preliminary round
  gatherItem <- reactive({
        
    ## interactive input from user
    subjectID <- subjectInput()
        
    ## vector for gathering a list of logKeys for subjects chosen
    participant <-c() 
    for (i in 1:length(subjectID)){
      n <- subjectID[i]
      participant <- c(participant, logKeyTable[n])
    }

    datasubset <- subset(participantData, logKey %in% participant)
                  
    itemList <- c()
    if (length(datasubset$logKey) > 0){
      for(j in 1:length(datasubset$logKey)){
        subLogKey <- as.character(datasubset$logKey[j]) # get logKey as string
        k <- identifySub(subLogKey)                     # get subject number
            
        # If logItem is not empty, insert into the vector
        if ( !as.character(datasubset$logItem[j]) == "" ){
          itemN <- decodeItem(as.character(datasubset$logType[j]),
                              as.character(datasubset$logItem[j]))
          itemList <- c(itemList,
                        paste("S",
                              k,
                              "__",
                              as.character(datasubset$logType[j]),
                              "__",
                              itemN,
                              sep = ""))
        }
      }
    }           
    ## as.factor counts the items as factor levels instead of strings
    return(as.factor(itemList))
  })
  
  ## Filter item type
  gatherItemType <- reactive({
        
    ## interactive input from user
    subjectID <- subjectInput()
        
    ## vector for gathering a list of logKeys for subjects chosen
    participant <-c() 
    for (i in 1:length(subjectID)){
      n <- subjectID[i]
      participant <- c(participant, logKeyTable[n])
    }
  
    datasubset <- subset(participantData, logKey %in% participant)
                  
    itemList <- c()
    if (length(datasubset$logKey) > 0){

      for(j in 1:length(datasubset$logKey)){
        subLogKey <- as.character(datasubset$logKey[j]) # get logKey as string
        k <- identifySub(subLogKey)                     # get subject number
            
        # If logItem is not empty, insert into the vector
        if ( !as.character(datasubset$logType[j]) == "" ){
          itemList <- c(itemList,
                        paste("Total",
                              "__",
                              as.character(datasubset$logType[j]),
                              sep = "")
                        )
          itemList <- c(itemList,
                        paste("S",
                              k,
                              "__",
                              as.character(datasubset$logType[j]),
                              sep = "")
                        )
        }
      }
    }           
    ## as.factor counts the items as factor levels instead of strings
    return(as.factor(itemList))
  })
  
  gatherConversation <- reactive({
    
    choiceConv <- convInput()
    
    loc <- choiceConv[1]
    dateConv <- as.Date(0, origin = choiceConv[2])
    timeConv <- times(choiceConv[3])
    
    datasubset <- subset(participantData, logDate == dateConv)
    
    possibleSub <<- paste(
    "Based on the conversation you chose in the drop-down list on the left,",
    "\n\nNumber of data points on ", dateConv, " : ",
    length(datasubset$logKey), "\n", sep = "")
    
    datasubset <- subset(datasubset, (as.numeric(logTime - timeConv) < 0.042) &
                                     (as.numeric(timeConv - logTime) < 0.042))

    possibleSub <<- paste(possibleSub, 
           "Number of data points within one hour of the chosen conversation (",
                          timeConv,
                          "): ",
                          length(datasubset$logKey), "\n", sep = "")
    
    x = xCoor(loc)
    y = yCoor(loc)
    datasubset <- subset(datasubset, sqrt((logPosX - x) * (logPosX - x) +
                                     (logPosY - y) * (logPosY - y)) < 100)
    
    possibleSub <<- paste(possibleSub, 
                          "Number of data points within 100 distance from ",
                          loc,
                          " at that time: ",
                          length(datasubset$logKey), "\n\n", sep = "")
    
    ## for a chosen conversation, identify the possible subjects involved
    tempSub <- table(datasubset$logKey)
    nameSub <- names(tempSub)

    nameToDisplay <- ""
    for (i in 1:length(nameSub)){
      if (tempSub[i] > 0){
        nameToDisplay <- paste(nameToDisplay, 
                               " S",
                               identifySub(nameSub[i]),
                               sep = "")
      }
    }
    possibleSub <<- paste(possibleSub,
                          "Possible subjects involved: ",
                          c(nameToDisplay),
                          sep = "")
  })

################################################################################
## Main function for plotting heat maps
################################################################################
  plotHeatMap <- function(type){  
   
    subjectID <- subjectInput() #interactive subject input from user
    if (type == "activityPlot"){
      plotType <- posPlotTypeInput()
    }
    else if (type == "objectPlot"){
      plotType <- objectPlotTypeInput()
    }
    else if (type == "conversationPlot"){
      plotType <- "conversation"
    }
    
    if (as.character(input$timePeriod[1]) == "dayCount" &
        plotType != "conversation"){
      
      weeki <- timeInput()               # interactive time input from user

      day = daysFromWeek(weeki[1])       # find starting week
      day2 = daysFromWeek(weeki[2])      # find ending week
          
      ## dynamically create the title
      title = paste("Week # ",
                    as.character(weeki[1]),
                    "- ",
                    as.character(weeki[2]), 
                    ": day ",
                    as.character(day[[1]]),
                    " through day ",
                    as.character(day2[[2]]),
                    sep = "",
                    collapse = NULL)

      ## plot the activity for the chosen participants and week
      currDataSubsetLength = plotTimePeriod(weeki,
                                            NA,
                                            NA,
                                            "dayCount",
                                            participantData,
                                            participantClrs,
                                            participantShapes,
                                            title,
                                            subjectID,
                                            plotType)
    }
    else{
             
      if (unformatted == TRUE){
        ## convert into Date class from integer
        ## 1970-01-01 is R's default date origin for counting
        participantData$logDate <<- as.Date(participantData$logDate,
                                           origin = "1970-01-01")
                                           
        participantData$logTime <<- times(participantData$logTime)
        ## a <- strptime("2011-10-31 12:32:12", format = "%Y-%m-%d %H:%M:%S")
        ## line below not needed since visdata_updatedDate.csv is used
        # participantData <<- addFormatDate(participantData) 
        
        
        ## 07/09 add in hourly control, based on logTime
        ## it took 30 mins and still hasn't finished, so...
        
        # for (k in 1:length(participantData$logDate)){
          # participantData$logDate[k] <- strptime(paste(
                                            # participantData$logDate[k],
                                            # participantData$logTime[k]
                                                # ),
                                        # format = "%Y-%m-%d %H:%M:%S")
        # }
        unformatted <<- FALSE
      }
      
      datei <- dateTimeInput() # interactive date input from user
      timei <- dayTimeInput()  # interactive time input from user
      
      ## dynamically create the title
      title = paste("Date from ", as.character(datei[1])," to ",
                    as.character(datei[2]), sep = "", collapse = NULL)

      currDataSubsetLength = plotTimePeriod(NA,
                                            datei,
                                            timei,
                                            "realDate",
                                            participantData,                                        
                                            participantClrs,
                                            participantShapes,
                                            title,
                                            subjectID,
                                            plotType)
    }
        
    if (plotType == "conversation"){
      ## dynamic drop-down list for conversations in a specified time period
      updateSelectInput(session,
                        "choiceConversation",
                        choices = conversationChoice,
                        selected = NULL
                        )
    }
    
    ## add the buildings
    if (plotType == "shadesBuilding"){
      addBuildings(datacoord, 1)
    }
    else if (plotType == "objShadesBuilding"){
      addBuildings(datacoord, 2)
    }
    else{
      addBuildings(datacoord, 0)
    }
          
    ## unused by Ang 06/29
    datasubsetLengths = c(datasubsetLengths, currDataSubsetLength )
  }

################################################################################
## Activity Map Plot in Main Window
################################################################################
  
  output$activityPlot <- renderPlot({
    plotHeatMap("activityPlot")
  })

################################################################################
## Object Heat Map Plot in Object Interactions Tab
################################################################################
  
  output$objectHeatMap <- renderPlot({
    plotHeatMap("objectPlot")
  })

################################################################################
## Conversation Heat Map Plot in "Conversation between subjects" Tab
################################################################################
  
  output$conversationMap <- renderPlot({
    plotHeatMap("conversationPlot") 
  })

################################################################################
## Heatmap for position/obj stats for all subjects
################################################################################
  output$posStatsPlot <- renderPlot({
    posStats <- read.csv("position_Stats.csv", header = TRUE, sep = ",")
    row.names(posStats) <- paste('S', row.names(posStats), sep = "")
    matrix <- data.matrix(posStats)
    pos_heatmap <- heatmap(matrix[1:20, 2:15], Rowv = NA, Colv = NA,
                           col = brewer.pal(9, "Blues"), 
                           scale = "column", 
                           margin = c(10 , 10))
  })

  output$objStatsPlot <- renderPlot({
    itemStats <- read.csv("item_Stats.csv", header = TRUE, sep = ",")
    row.names(itemStats) <- paste('S', row.names(itemStats), sep = "")
    matrix <- data.matrix(itemStats)
    pos_heatmap <- heatmap(matrix[1:20, 2:14], Rowv = NA, Colv = NA,
                           col = brewer.pal(9, "Reds"), 
                           scale = "column", 
                           margin = c(10 , 10))
  })
  
################################################################################
## Define the set of y limits to use for various plots below
################################################################################
  
  yLimits <- function(plotVar){
    
    switch(plotVar,
           "Weight" = c(100, 320),
           "HBA1C" = c(5, 11.5),
           "Perceived Usefulness" = c(0.7, 5.2), 
           "Perceived Ease Of Use" = c(0.7, 6.2), 
           "Diabetes Empowerment" = c(1, 5.3),
           "Diabetes Support" = c(2.9, 7.3),
           "Diet" = c(0, 7.3),
           "Exercise" = c(-0.5, 7.3),
           "Blood Sugar Testing" = c(-0.5, 7.3),
           "Diabetic Foot Care" = c(-0.5, 7.3),
           "Diabetes Knowledge" = c(0.4, 1.1)
    )
  }

################################################################################
## Drawing the bubble chart
################################################################################
  output$bubbleChart <- renderPlot({
  	subjectID <- subjectInput() # interactive input from user
  	subjectID <- subjectID[which(!subjectID==3)] # remove subject 3: no clinical
    gatherData()
    
    baseWeight <- plotData[, 10]
    M6Weight <- plotData[, 24]
    baseHBA1C <- plotData[, 14]
    M6HBA1C <- plotData[, 28]
    subNum <- row.names(plotData)
    
    if (length(baseWeight) > 0){
      posStats <- read.csv("position_Stats.csv", header = TRUE, sep = ",")
      itemStats <- read.csv("item_Stats.csv", header = TRUE, sep = ",")
      radius <- 0.5*posStats$Pos.Total[subjectID] +
                0.5*itemStats$Item.Total[subjectID]
      symbols(baseWeight, baseHBA1C, circles=radius, inches = 0.35,
              fg = "black", bg = "red", xlab = "Weight/lb", ylab = "HBA1C",
              xlim = yLimits('Weight'), ylim=yLimits('HBA1C'))
      symbols(M6Weight, M6HBA1C, circles=radius, inches = 0.35,
              fg = "black", bg = "yellow", xlab = "Weight/lb", ylab = "HBA1C",
              add = TRUE)
      text(baseWeight, baseHBA1C, paste("Base", subNum), cex = 0.9)
      text(M6Weight, M6HBA1C, paste("M6", subNum), cex = 1.4)
    }
    else{
      symbols(0, 0, circles=1, inches = 0.35,
              fg = "white", bg = "white", xlab = "Weight/lb", ylab = "HBA1C",
              xlim = yLimits('Weight'), ylim=yLimits('HBA1C'))
    }
  })

################################################################################
## Drawing the weightPlot and HBA1CPlot
################################################################################

  clinicalPlot <- function(type){
    if (type == "weightPlot"){
      plotType <- 'Weight'
      units <- '/ lbs'
      colToUse <- c(10, 17, 24)
    }
    else if (type == "HBA1CPlot"){
      plotType <- 'HBA1C'
      units <- ''
      colToUse <- c(14, 21, 28)
    }
    
        
    ## update reactive data, which includes plotData used below
    gatherData()
        
    ## new data frame with only Weight Data
    clinData <- data.frame(plotData[, colToUse[1]],
                             plotData[, colToUse[2]],
                             plotData[, colToUse[3]]) 
    names(clinData) <- c(paste('AtBaseLine', sep = ""),
                         paste('AtM3', sep = ""),
                         paste('AtM6', sep = ""))
                
    clinNum <- data.matrix(clinData)    # turn into numerical form
    clinIndi <- t(clinNum)              # transpose of matrix
                
    ## if there's at least one data point at base line
    if (length(clinData$AtBaseLine) >= 1){ 
      ## plot first column, which is the first subject's 3 points,
      ## at 0, 3, and 6 months respectively
      plot(c(0, 3, 6),
           clinIndi[, 1],
           main=paste(plotType, "values for subjects at 0, 3, and 6 months"),
           xlab="Time / Month",
           ylab=paste(plotType, "of Subject", units),
           ylim=yLimits(plotType),
           col=clrs[1],
           pch=pchVals[1],
           cex=2.0)
                        
      ## plot subsequent subjects
      for(i in 1:length(clinData$AtBaseLine)){
        points(c(0, 3, 6), clinIndi[ , i], 
               col=clrs[i], pch=pchVals[i], cex=2.0)
                            
        ## if the point at 3 month is not NA, join first 2 pt by line
        if (!is.na(clinIndi[ , i][2])){
          segments(0, clinIndi[ , i][1], 3, clinIndi[ , i][2], col=clrs[i])
        }
                                
        ## if the point at 3 & 6 month are not NA, join last 2 pt by line
        if (!is.na(clinIndi[ , i][2]) & !is.na(clinIndi[ , i][3])){
          segments(3, clinIndi[ , i][2], 6, clinIndi[ , i][3], col=clrs[i])
        }
      }
    }
    else{ # when no subject is chosen, plot empty
      plot(c(0, 3, 6),
           c(1, 2, 3),
           main=paste(plotType, "values for subjects at 0, 3, and 6 months"),
           xlab="Time / Month",
           ylab=paste(plotType, "of Subject", units),
           ylim=yLimits(plotType),
           col="white",
           pch=1)
    } 
  }

  output$weightPlot <- renderPlot({
    clinicalPlot("weightPlot")
  })
  
  output$HBA1CPlot <- renderPlot({
    clinicalPlot("HBA1CPlot")
  })
  
################################################################################
## Drawing Survey Question Scores Plot based on user choice
################################################################################

  output$surveyPlot <- renderPlot({
          
    ## update reactive data, which includes plotDataSurvey used below
    gatherSurvey()          
    ## Column number to plot based on survey question choice   
    choiceColNum <- choiceReactive()             
    
    ## We don't currently have Co-presence and Presence data, 07/10
    # put all 0 for all subjects for base-line
    # if (input$choiceToView == "Co-Presence" |
    #     input$choiceToView == "Presence"){
      # surveyData <- data.frame(c(rep(0,length(plotDataSurvey[, 1]))),
                               # plotDataSurvey[,choiceColNum],
                               # plotDataSurvey[,choiceColNum + 1])
    # }
    ## Other survey questions
    surveyData <- data.frame(plotDataSurvey[,choiceColNum],
                             plotDataSurvey[,choiceColNum + 1],
                             plotDataSurvey[,choiceColNum + 2])
                             
    names(surveyData) <- c("AtBaseLine", "AtM3", "AtM6")
                
    surveyNum <- data.matrix(surveyData)
    surveyIndi <- t(surveyNum)
                
    if (length(surveyData$AtBaseLine) >= 1){
      plot(c(0, 3, 6),
           surveyIndi[ , 1],
           main=paste(input$choiceToView,
                      "Scores for subjects at 0, 3, and 6 months"),
           xlab="Time / Month",
           ylab=paste(input$choiceToView,"Score"),
           ylim=yLimits(input$choiceToView),
           col=clrsS[1],
           pch=pchValsS[1],
           cex=2.0)
                
      for(i in 1:length(surveyData$AtBaseLine)){
        points(c(0, 3, 6),surveyIndi[ , i],
               col=clrsS[i], pch=pchValsS[i], cex=2.0)
        if (!is.na(surveyIndi[ , i][2])){
          segments(0, surveyIndi[ , i][1], 3, surveyIndi[ , i][2], col=clrsS[i])
        }
        if (!is.na(surveyIndi[, i][2]) & !is.na(surveyIndi[, i][3])){      
          segments(3, surveyIndi[ , i][2], 6, surveyIndi[ , i][3], col=clrsS[i])
        }
      }
    }
    else{ # when no subject is chosen, plot empty
      plot(c(0, 3, 6),
           c(1,2,3),
           main=paste(input$choiceToView,
                      "Scores for subjects at 0, 3, and 6 months"),
           xlab="Time / Month",
           ylab=paste(input$choiceToView,"Score"),
           ylim=yLimits(input$choiceToView),
           col="white",
           pch=1)
    }       
  })
  
################################################################################
## Display Color Legend if requested
################################################################################

  output$legendForMain <- renderPlot({
                
    subjectID <- subjectInput() 
                
    clrs <- c()
    pchVals <- c()
                
    for(i in 1:length(subjectID)){
      n <- subjectID[i]
      clrs <- c(clrs, participantClrs[n])    # Assign the color to the data pt
      pchVals <- c(pchVals, participantShapes[n])    # Assign shape 
    }
    
    # pdf("Legend with logKey and Survey ID")
    displayColorLegend(subjectID, clrs, pchVals, "subject")
    # dev.off()  
  })

  output$legendForObject <- renderPlot({
    
    subjectID <- subjectInput() 
    plotType <- objectPlotTypeInput()
    
    clrs <- c()
    pchVals <- c()
    
    if (plotType == "object" | plotType == "objShadesBuilding"){
    	
      for(i in 1:length(subjectID)){
        n <- subjectID[i]
        clrs <- c(clrs, participantClrs[n])   # Assign the color to the data pt
        pchVals <- c(pchVals, participantShapes[n])    # Assign shape 
      }
    
      displayColorLegend(subjectID, clrs, pchVals, "subject")
    }
    else if (plotType == "objectByType"){
      displayColorLegend(objTypes, objClrs, objPchs, "object")
    }
  })
  
  output$legendForConversation <- renderPlot({
                
    subjectID <- c(seq(1, 20, by=1))
                
    clrs <- c()
    pchVals <- c()
                
    for(i in 1:length(subjectID)){
      n <- subjectID[i]
      clrs <- c(clrs, participantClrs[n])    # Assign the color to the data pt
      pchVals <- c(pchVals, participantShapes[n])    # Assign shape 
    }
    
    displayColorLegend(subjectID, clrs, pchVals, "subject")
  })
  
################################################################################
## View objects interacted with by subjects chosen
## View for Raw Data if necessary
## Provide text for user guide or updates information
################################################################################
  objDataToTable <- function(dataVec){
    
    freqCount <- table(dataVec)
  	
  	if (length(freqCount) > 0){
      frameOut <- data.frame(
        Object.Type.And.Info = names(freqCount)[1:length(freqCount)],
        Frequency = freqCount[1:length(freqCount)]
      )
    
      row.names(frameOut) <- seq(1, length(frameOut$Frequency))
      return(frameOut)
    }
    else{
      return(data.frame(Processing...Data... = "Please wait..."))
    }
  }
  
  objDataOut <- reactive({
  	objDataToTable(gatherItem())
  })
  output$objData <- renderTable({
    objDataOut()
  })
  
  objTypeOut <- reactive({
    objDataToTable(gatherItemType())
  })
  output$objType <- renderTable({
    objTypeOut()
  })
  
  output$clinData <- renderPrint({
    gatherData()
    plotData
  })
  
  output$survData <- renderPrint({
    gatherSurvey()
    plotDataSurvey
  })
  
  output$convData <- renderPrint({
  	gatherConversation()
    cat(possibleSub)
  })
  
  getUserGuideText <- function(choices){
  	textOut <- ''
    for (i in 1:length(choices)){
      textOut <- paste(textOut, choices[i], ":\n", sep = "")
      num <- switch(choices[i],
             "Introduction" = 1,
             "Avatar Positions" = 2,
             "Avatar Interactions with Virtual Objects" = 3,
             "Conversations with Position Data" = 4,
             "Raw Data" = 5)
      textOut <- paste(textOut, userGuide[num], "\n\n", sep = "")
    }
    return(textOut)
  }
  output$userRef <- renderPrint({
  	cat(getUserGuideText(userRefChoiceInput()))
  })
  
  getVersionText <- function(choices){
  	textOut <- ''
    for (i in 1:length(choices)){
      textOut <- paste(textOut, choices[i], ":\n", sep = "")
      num <- switch(choices[i],
                    "Version 8, 2013/07/25" = 1,
                    "Version 11, 2013/07/31" = 2,
                    "Version 12, 2013/07/31" = 3,
                    "Version 14, 2013/08/05" = 4,
                    "Version 15, 2013/08/12" = 5)
      textOut <- paste(textOut, versionGuide[num], "\n\n", sep = "")
    }
    return(textOut)
  }
  output$versionRef <- renderPrint({
  	cat(getVersionText(versionChoiceInput()))
  })
})
