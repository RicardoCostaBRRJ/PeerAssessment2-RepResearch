## Loads all required libraries (ggplot2)
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
  message("Required ggplot2 package is now in place.")
} else {
  message("Required ggplot2 package is already in place.")
}

## Tests if dataset was downloaded and unziped
## If can't find the unziped dataset, it unzips the downloaded zip file
## If can't find the unziped dataset, nor the zip file itself, it downloads and unzips it

workingDir <- getwd()
datasetFile <- paste0(workingDir,"/stormData.csv")
datasetZipFile <- paste0(workingDir,"/stormData.csv.bz2")
if (file.exists(datasetFile)) {
  message("Dataset is already in place.")
  message("All set. Moving forward with the script.")
} else {
  message("Unziped dataset not detected.")
  if (!file.exists(datasetZipFile)){
    message("Ziped dataset wasn't found. Please wait while it is downloaded and unziped.")
    TempFile <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",TempFile, method="curl")
    bzfile(TempFile)
    unlink(TempFile)
    message("All set. Moving forward with the script.")
  } else {
    message("Ziped dataset was found. Unziping its content. Please wait.")
    bzfile(datasetZipFile)
    message("All set. Moving forward with the script.")
  }
}

## Reads the dataset into a table and subsets it 
datasetStorm <- read.csv("stormData.csv", sep=",", header=TRUE)
datasetTidyStorm <- datasetStorm[,c('EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]

# Unit conversion for Property and Crop Damage calculations 
datasetTidyStorm$PROPDMGNUM = 0
datasetTidyStorm$CROPDMGNUM = 0
datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "H", ]$PROPDMGNUM = datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "H", ]$PROPDMG * 10^2
datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "K", ]$PROPDMGNUM = datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "K", ]$PROPDMG * 10^3
datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "M", ]$PROPDMGNUM = datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "M", ]$PROPDMG * 10^6
datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "B", ]$PROPDMGNUM = datasetTidyStorm[datasetTidyStorm$PROPDMGEXP == "B", ]$PROPDMG * 10^9
datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "H", ]$CROPDMGNUM = datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "H", ]$CROPDMG * 10^2
datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "K", ]$CROPDMGNUM = datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "K", ]$CROPDMG * 10^3
datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "M", ]$CROPDMGNUM = datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "M", ]$CROPDMG * 10^6
datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "B", ]$CROPDMGNUM = datasetTidyStorm[datasetTidyStorm$CROPDMGEXP == "B", ]$CROPDMG * 10^9

# Plot 1: fatalities with the most harmful event
png(filename = "./plot1.png", 
    width = 480, height = 480, 
    units = "px")
datasetStormFatalities <- aggregate(FATALITIES ~ EVTYPE, data=datasetTidyStorm, sum)
datasetStormFatalities <- datasetStormFatalities[order(-datasetStormFatalities$FATALITIES), ][1:10, ]
datasetStormFatalities$EVTYPE <- factor(datasetStormFatalities$EVTYPE, levels = datasetStormFatalities$EVTYPE)

ggplot(datasetStormFatalities, aes(x = EVTYPE, y = FATALITIES)) + 
  geom_bar(stat = "identity", fill = "red", las = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event Type") + ylab("Fatalities") + ggtitle("Number of fatalities by top 10 Weather Events")
dev.off()

# Plot2: injuries with the most harmful event
png(filename = "./plot2.png", 
    width = 480, height = 480, 
    units = "px")
datasetInjuries <- aggregate(INJURIES ~ EVTYPE, data=datasetTidyStorm, sum)
datasetInjuries <- datasetInjuries[order(-datasetInjuries$INJURIES), ][1:10, ]
datasetInjuries$EVTYPE <- factor(datasetInjuries$EVTYPE, levels = datasetInjuries$EVTYPE)

ggplot(datasetInjuries, aes(x = EVTYPE, y = INJURIES)) + 
  geom_bar(stat = "identity", fill = "red", las = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event Type") + ylab("Injuries") + ggtitle("Number of injuries by top 10 Weather Events")
dev.off()

# Plot3: damages with the most harmful event
png(filename = "./plot3.png", 
    width = 480, height = 480, 
    units = "px")
datasetDamages <- aggregate(PROPDMGNUM + CROPDMGNUM ~ EVTYPE, data=datasetTidyStorm, sum)
names(datasetDamages) = c("EVTYPE", "TOTALDAMAGE")
datasetDamages <- datasetDamages[order(-datasetDamages$TOTALDAMAGE), ][1:10, ]
datasetDamages$EVTYPE <- factor(datasetDamages$EVTYPE, levels = datasetDamages$EVTYPE)

ggplot(datasetDamages, aes(x = EVTYPE, y = TOTALDAMAGE)) + 
  geom_bar(stat = "identity", fill = "red", las = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event Type") + ylab("Damages ($)") + ggtitle("Property & Crop Damages by top 10 Weather Events")
dev.off()