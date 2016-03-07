## 1. Code for reading in the dataset and/or processing the data
## Install knitr: install.packages('knitr', dependencies = TRUE)

library("knitr")
library(plyr)
library(ggplot2)
library(grid)
library(lattice)

## 1.1 Read in the data as is
activity <- read.csv("activity.csv", header = TRUE)


## 2.Histogram of the total number of steps taken each day

## 2.1 Add number of steps by each date
totalsteps <- aggregate(activity$steps, list(activity$date), sum)
names(totalsteps) <- c("eachdate","addsteps")

## 2.2 Make a histogram of the total number of steps taken each day
windows()

with(totalsteps, {
  par(mar=c(6,6,3,0), oma=c(2,0,0,0), mgp=c(5,0.5,0), las=2)
  barplot(
    height=addsteps,names.arg=eachdate,space=c(0),
    main="Total Steps taken Each Day with Missing steps",
    xlab="Dates",
    ylab="Number of Total Steps")
})


## 3.Mean and median number of steps taken each day.  one can also use "summary" funtion.
meansteps <- round(mean(totalsteps$addsteps, na.rm = TRUE))
print(sprintf("Mean of total steps taken per day: %i ", meansteps))
mediansteps <- median(totalsteps$addsteps, na.rm = TRUE)
print(sprintf("Median of total steps taken per day: %i ", mediansteps))

## 4.1 Time series plot of the average number of steps taken
intervalsteps <- aggregate(steps ~ interval,  data = activity, mean)

# create a time series plot 
windows()
plot(intervalsteps$interval, intervalsteps$steps, type='l', col = "blue",
     main="Time Series Plor of Average number of steps taken", xlab="Interval (5-mins)", 
     ylab="Average number of steps across all days")

## 5.The 5-minute interval that, on average, contains the maximum number of steps
maxsteps <- which.max(intervalsteps$steps)
intervalsteps[maxsteps,]

## 6.Code to describe and show a strategy for imputing missing data

## Remove missing values
activitynomissingvalues <- na.omit(activity)

## 6.1 Calculate the total number of rows with NAs
missingrows <- nrow(activity) - nrow(activitynomissingvalues)
print(sprintf("Total Row count with NA data: %i ", missingrows))

## 6.2 Strategy to fill in the missing values is with average values of 5 min interval
## 6.3 Creating the new data set and column bind on interval with intervalsteps which is already
## calcualted in step 4.1
missingactivity <- subset(activity,is.na(activity$steps))
datatopopulate <- rep(intervalsteps,8)
names(datatopopulate) < c("inerval", "steps")
missingactivity$steps <- round(datatopopulate$steps)
newactivity <- rbind(activitynomissingvalues,missingactivity)

## 6.4 Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day. Do these values differ 
## from the estimates from the first part of the assignment? What is the impact of imputing 
## missing data on the estimates of the total daily number of steps?

## Add number of steps by each date
totalstepsnomissinvalue <- aggregate(newactivity$steps, list(newactivity$date), sum)
names(totalstepsnomissinvalue) <- c("neweachdate","newaddsteps")

## 7. Make a histogram of the total number of steps taken each day
windows()

with(totalstepsnomissinvalue, {
  par(mar=c(6,6,3,0), oma=c(2,0,0,0), mgp=c(5,0.5,0), las=2)
  barplot(
    height=newaddsteps,names.arg=neweachdate,space=c(0),
    main="Total Steps taken Each Day with imputed Missing Steps",
    xlab="Dates",
    ylab="Number of Total Steps")
})

## mean and median total number of steps taken per day
newmeansteps <- round(mean(totalstepsnomissinvalue$newaddsteps))
print(sprintf("Mean of total steps taken per day: %i ", newmeansteps))
newmediansteps <- median(totalstepsnomissinvalue$newaddsteps)
print(sprintf("Median of total steps taken per day: %i ", newmediansteps))

## What is the impact of imputing 
## missing data on the estimates of the total daily number of steps?

## with missing values
summary(totalsteps)

## with no missing values
summary(totalstepsnomissinvalue)

print("Mean stay the same for both cases and Median is smaller for data without missing value")

## 8. Panel plot comparing the average number of steps taken per
## 5-minute interval across weekdays and weekends
newactivity$daytype <- weekdays(as.Date(newactivity$date))

newactivity$daytype[newactivity$daytype  %in% c('Saturday','Sunday') ] <- "weekend"
newactivity$daytype[newactivity$daytype != "weekend"] <- "weekday"

intervalstepsdaytype <- aggregate(steps ~ daytype + interval,data = newactivity,mean)

windows()
xyplot(
  type="l",
  data=intervalstepsdaytype,
  steps ~ interval | daytype,
  xlab="Interval for days of the weeks",
  ylab="Number of steps",
  main = "Comparing the average number of steps across the week",
  layout=c(1,2)
)

