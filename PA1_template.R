library(dplyr)
library(lubridate)
library(data.table)
library(lattice)

rm(list=ls(all=TRUE)) 

# Loading and preprocessing the data
unzip("activity.zip")
activities <- read.csv('activity.csv', header = T)
names(activities)
str(activities)
head(activities)

# What is mean total number of steps taken per day?
activities_per_day <- group_by(activities, date)
activities_per_day <- summarise(activities_per_day, total_steps = sum(steps))
mean_value <- mean(activities_per_day$total_steps, na.rm = T)
median_value <- median(activities_per_day$total_steps, na.rm = T)

plot(activities_per_day$total_steps, main="Histogram of steps taken each day", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="grey")

abline(h=mean_value, lwd =4, col = 'blue')
abline(h=median_value, lwd = 3, col = 'red')

legend('topright', lty = 1, lwd = 3, col = c("blue", "red"),bty="n",
       cex = .8, 
       legend = c(paste('Mean: ', mean_value),
                  paste('Median: ', median_value)))

# What is the average daily activity pattern?
activities_per_interval <- group_by(activities, interval)
activities_per_interval <- summarise(activities_per_interval[!is.na(activities_per_interval$steps),], mean_steps = mean(steps))

plot(activities_per_interval, type="l",
     main="Average number of steps per interval across all days", 
     xlab="Interval", ylab="Average # of steps across all days", 
     lwd=2, col="grey")

maxStepsByInterval <- max(activities_per_interval$mean_steps)
activities_per_interval_table <- data.table(activities_per_interval)
maxInterval <- activities_per_interval_table[mean_steps == maxStepsByInterval]$interval
abline(v=maxInterval, col="red", lwd=3)

# Imputing missing values
sum(is.na(activities$steps))

getSteps = function(steps,averageSteps){
  if(is.na(steps)){
    return(averageSteps)
  }
  return(steps)
}

activities_table <- data.table(activities)
activities_per_interval_table <- data.table(activities_per_interval)
activities_table$newSteps <- mapply(getSteps,activities_table$steps, activities_per_interval_table$mean_steps)

par(mfrow=c(2,1))
plot(activities_per_day$total_steps, main="Histogram of steps taken each day", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="grey")


activities_filled_per_day <- group_by(activities_table, date)
activities_filled_per_day <- summarise(activities_filled_per_day, total_steps = sum(newSteps))
mean_value_filled <- mean(activities_filled_per_day$total_steps, na.rm = T)
median_value_filled <- median(activities_filled_per_day$total_steps, na.rm = T)

plot(activities_filled_per_day$total_steps, main="Histogram of steps taken each day", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="grey")


# Are there differences in activity patterns between weekdays and weekends?

getDayOfTheWeekType = function(x){
  if(x %in% c('1', '7')){
    return('weekend')
  }
  
  return('weekday')
}

activities_table <- mutate(activities_table, day_of_the_week = wday(ymd(date)))
activities_table$day_of_the_week_type <- mapply(getDayOfTheWeekType, activities_table$day_of_the_week)
activities_table_processed = activities_table[, list(avg_steps = mean(newSteps, na.rm = T)), 
                                    by = list(interval, day_of_the_week_type)]

xyplot(avg_steps~interval | day_of_the_week_type, data = activities_table_processed,
       type = 'l',
       xlab = 'Interval',
       ylab = 'Number of Steps',
       layout = c(1,2))

