##Trevor Harris
##2/2/2015
##Coursera -> Exploring Data Analysis

##FUNCTION PURPOSE: Explore household energy use over a 2-day period in February, 2007, by creating a plot.
##EXPECTED INPUT: N/A
##EXPECTED OUTPUT: A .png file is created
plot2 <- function() {
  
  ##Retrieve our data
  sourceFileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  targetFile <- "./data/Electric power consumption.zip"
  if(!file.exists("./data")){dir.create("./data")}
  if(!file.exists(targetFile)){
    download.file(sourceFileURL,targetFile)
    dateDownloaded <- date()
    unzip(targetFile,exdir = "./data")}
  
  ##Load our reference tables
  ##To speed things up, we'll grab the header, then just the rows we care about, then stitch things together
  dataSetHeader <- read.table("./data/household_power_consumption.txt", header = TRUE, sep=";",nrows = 1)
  dataSet <- read.table("./data/household_power_consumption.txt",
                        header = TRUE, sep=";", na.strings = c("?"),
                        colClasses = c("character", "character",rep("numeric",7)),  ##speeds up the initial load
                        skip = 66636, nrows = 2880)  ##speeds up the initial load.  Alternatively, we could specify 2075260 rows and filter later  21998:23437
  names(dataSet) = names(dataSetHeader)
  
  ##Replace the date and time columns with a single well-formatted column
  dateTime <- paste(dataSet[,1],dataSet[,2])
  dateTime <- strptime(dateTime, format = "%d/%m/%Y %H:%M:%S")
  dateTime <- format(dateTime, "%Y/%m/%d %H:%M:%S")
  dataSet <- cbind(dateTime,dataSet[,3:9])
  
  ##remove rows with missing data
  dataSet <- dataSet[complete.cases(dataSet),]

  ##create our plot using png as our device
  png(file = "./plot2.png", width = 480, height = 480)
  par(cex=.9)  ##make our font smaller globally
  plot(dataSet$dateTime,dataSet$Global_active_power, xaxt = "n", type = "n", ylab = "Global Active Power (kilowatts)")
  lines(dataSet$dateTime,dataSet$Global_active_power)
  
  ##also add a custom x axis
  labels = c("Thu","Fri","Sat")
  ticMarks = c(dataSet[1,1],dataSet[length(dataSet[,1])/2,1],dataSet[length(dataSet[,1]),1])
  axis(1,at=ticMarks,labels=labels)  
  dev.off()
}