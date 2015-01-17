library(reshape2)
library(dplyr)
library(lubridate)

#url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(url, "repdata-data-activity.zip", mode="wb")
#unzip("repdata-data-activity.zip")

# Assume working directory contains data folder
file <- "activity.csv"
activity <- read.csv(file, header=TRUE, na.strings="NA", 
                     colClasses=c("integer", "character", "integer"))
activity$date <- ymd(activity$date)
# Use reshape2 to build a matrix of dates vs. intervals I can use for the rest of the assignment
tidyActivity <- melt(activity, id=c("date", "interval", "steps"), na.rm=TRUE)
dateByinterval <- dcast(tidyActivity, date ~ interval, fun.aggregate=sum) # with the assignment data: 61 dates x 288 intervals
# need to have an intervals variable in there for when I have to plot

# What is mean total number of steps taken per day?
dailyStepswNA <- rowSums(select(dateByinterval, -date))
hist(dailyStepswNA)
meandailySteps <- mean(dailyStepswNA, na.rm=TRUE)
mediandailySteps <- median(dailyStepswNA, na.rm=TRUE)
print(meandailySteps)
print(mediandailySteps)

# Compute interval steps by adding all dates in a given interval
totalIntervalTime <- colSums(select(dateByinterval, -date), na.rm=TRUE)
averageIntervalTime <- totalIntervalTime/length(totalIntervalTime) 

# What is the average daily activity pattern?
plot(averageIntervalTime, type="l")
averageIntervalTime[which.max(averageIntervalTime)]  # returns 2 numbers, 2nd correct interval
  # need to print something here

#totalDateTime <- apply(dateByinterval, 1, sum)
#averageDateTime <- totalDateTime/length(uniqueDates)

# Inputting missing values
# A function to replace NA with the mean of the steps in an interval
from.NA.to.mean=function(x){
    x<-as.numeric(as.character(x))
    x[is.na(x)] <- mean(x, na.rm=TRUE) 
    x
}
# Apply the function
date <- select(dateByintervalnoNA, date)
dateByintervalnoNA <- data.frame(apply(select(dateByinterval, -date),2,from.NA.to.mean), stringsAsFactors=FALSE)
dateByintervalnoNA <- cbind(date, dateByintervalnoNA)

dailyStepsnoNA <- rowSums(select(dateByintervalnoNA, -date))
hist(dailyStepsnoNA)

# Are there differences in activity patterns between weekdays and weekends?
dayOfweek <- weekdays(as.Date(dateByintervalnoNA$date, format="%Y-%m-%d"))
weekEnd <- dayOfweek == "Saturday" | dayOfweek == "Sunday"
dayOfweek[weekEnd==TRUE] <- "Weekend"
dayOfweek[weekEnd==FALSE] <- "Weekday"
dayOfweek <- as.factor(dayOfweek)

temp <- cbind(dayOfweek, dateByintervalnoNA)
wDA <- temp[temp$dayOfweek=="Weekday",]
wEA <- temp[temp$dayOfweek=="Weekend",]
tswDA <- colSums(select(wDA, -(1:2)))/60
tswEA <- colSums(select(wEA, -(1:2)))/60
x <- seq(1,2355, length.out=288)


par(mfrow = c(2, 1), mar = c(4, 4, 2, 2), oma = c(0, 0, 0, 0)) 
with(temp, {
    plot(x, tswDA, ylim=c(0,150), xlab="Interval", ylab= "Avg Steps - Weekday", type="l")
    plot(x, tswEA, ylim=c(0,150), xlab="Interval", ylab= "Avg Steps - Weekend", type="l") 
})
