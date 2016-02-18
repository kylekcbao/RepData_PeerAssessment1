## Code for reading in the dataset and/or processing the data
if (!file.exists("activity.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                "activity.zip")
}
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
data <- read.csv("activity.csv", colClasses = c("numeric", "character"))
data$date <- as.Date(data$date)

## Histogram of the total number of steps taken each day
daily.steps <- aggregate(data$steps, by = list(data$date),
                         FUN = sum, na.rm = TRUE)
colnames(daily.steps) <- c("date", "steps")
with(daily.steps, hist(steps, breaks = 10, xlab = "Number of Steps",
                       main = "Total Number of Steps Taken Each Day"))

## Mean and median number of steps taken each day
mean(daily.steps$steps) # mean
median(daily.steps$steps) # median

## Time series plot of the average number of steps taken
average.steps <- aggregate(data$steps, by = list(data$interval),
                           FUN = mean, na.rm = TRUE)
colnames(average.steps) <- c("interval", "steps")
with(average.steps, plot(interval, steps, type = "l",
                         xlab = "5-minute interval",
                         ylab = "Steps",
                         main = "Average Daily Activity Pattern"))

## The 5-minute interval that, on average, contains the maximum number of steps
average.steps[which.max(average.steps$steps), "interval"]

## Total missing data
sum(is.na(data$steps))

## Function to change NA values to average steps for that interval of the day
InputMissing <- function(x) {
  idx <- which(is.na(x$steps))
  for (i in 1:length(idx)) {
    x[idx[i], "steps"] <- average.steps[average.steps$interval == 
                                        x[idx[i], "interval"], "steps"]
  }
  x
}

## Create a new dataset that is equal to the original dataset but with the 
## missing data filled in.
data.new <- InputMissing(data)

## Histogram of the total number of steps taken each day
daily.steps.new <- aggregate(data.new$steps, by = list(data.new$date), 
                             FUN = sum, na.rm = TRUE)
colnames(daily.steps.new) <- c("date", "steps")
with(daily.steps.new, hist(steps, breaks = 10, xlab = "Number of Steps", 
                           main = "Total Number of Steps Taken Each Day"))

mean(daily.steps.new$steps) # new mean
mean(daily.steps.new$steps) - mean(daily.steps$steps) # difference in mean
median(daily.steps.new$steps) # new median
median(daily.steps.new$steps) - median(daily.steps$steps) # difference in median

# Values differ from first part of assignment.
# Inputting missing data increases the estimates of the total daily number of
# steps.

## Function to create factor variable based on weekday/weekend in dataset
WhichDay <- function(x) {
  results <- vector()
  for (i in 1:nrow(x)) {
    if (weekdays(x[i, "date"]) %in% c("Saturday", "Sunday")) {
      results <- c(results, "Weekend")
    } else {
      results <- c(results, "Weekday")
    }
  }
  
  results
}

data.new$day <- WhichDay(data.new)
data.new$day <- factor(data.new$day, levels = c("Weekday", "Weekend"), 
                       labels = c("weekday", "weekend"))

## Creates time series plot comparing average steps across weekday/weekend days. 
average.steps.new <- aggregate(data.new$steps, 
                               by = list(data.new$interval, data.new$day), 
                               FUN = mean, na.rm=TRUE)
colnames(average.steps.new) <- c("interval", "day", "steps")

require(lattice)
xyplot(steps~interval | day, data = average.steps.new,
       type = "l",
       xlab = "Interval",
       ylab = "Number of Steps",
       layout = c(1,2))
