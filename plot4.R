##Trevor Harris
##2/3/2015
##Coursera -> Exploring Data Analysis

##FUNCTION PURPOSE: Explore household energy use over a 2-day period in February, 2007, by creating a plot.
##EXPECTED INPUT: N/A
##EXPECTED OUTPUT: A .png file is created
plot4 <- function() {
  
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
  
  ##create our plots using png as our device
  png(file = "./plot4.png", width = 480, height = 480)
  par(cex=.9, mfrow = c(2,2))  ##make our font smaller globally, and setup a 2x2 plot grid
  
  timeMin = dataSet[1,1]
  timeMid = length(dataSet[,1])/2
  timeMax = length(dataSet[,1])
  ticMarks = c(dataSet[timeMin,1],dataSet[timeMid,1],dataSet[timeMax,1])
  labels = c("Thu","Fri","Sat")
  
  ##plot 1 (top left)
  
    plot(dataSet$dateTime,dataSet$Global_active_power, xaxt = "n", type = "n", ylab = "Global Active Power (kilowatts)")
    lines(dataSet$dateTime,dataSet$Global_active_power)
    axis(1,at=ticMarks,labels=labels)  
  
  ##plot 2 (top right)
    
    plot(dataSet$dateTime,dataSet$Voltage, xaxt = "n", type = "n",
         ylab = "Voltage", xlab = "datetime")
    lines(dataSet$dateTime,dataSet$Voltage)
    axis(1,at=ticMarks,labels=labels)  
  
  ##plot 3 (bottom left)
  
    plot(dataSet$dateTime,dataSet$Sub_metering_1, xaxt = "n", type = "n", ylab = "Energy sub metering")
    lines(dataSet$dateTime,dataSet$Sub_metering_1, col = "black")
    lines(dataSet$dateTime,dataSet$Sub_metering_2, col = "red")
    lines(dataSet$dateTime,dataSet$Sub_metering_3, col = "blue")
    axis(1,at=ticMarks,labels=labels)  
    legend("topright", lty = 1, col=c("black","red","blue"), bty = "n",
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  
  ##plot 4 (bottom right)
    
    plot(dataSet$dateTime,dataSet$Global_reactive_power, xaxt = "n", type = "n",
         ylab = "Global_reactive_power", xlab = "datetime")
    lines(dataSet$dateTime,dataSet$Global_reactive_power)
    axis(1,at=ticMarks,labels=labels) 
    
  dev.off()
}