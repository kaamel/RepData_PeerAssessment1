---
output: html_document
---
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
If the file activity.csv is not found in the current directory, it is extracted from the activity.zip file and is loaded into "activity_data".


```r
if (!file.exists("./activity.csv")) {
  unzip("activity.zip", files = "activity.csv", overwrite = T)
}
activity_data <- read.csv("activity.csv")
```


By examining the activity_data we can see that it has the following columns: steps, date, interval.

## What is mean total number of steps taken per day?
We will need to first calculate the total number of steps per day before ploting the histogram.


```r
steps.date <- aggregate(steps ~ date, data = activity_data, FUN = sum)
hist(steps.date$steps, breaks = 10, xlab = "Daily Number of Steps", main = "Histogram of steps per day")
```

![plot of chunk histogram of steps per day](figure/histogram of steps per day.png) 


```r
mean_total_steps <- mean(steps.date$steps, na.rm = T)
median_total_steps <- median(steps.date$steps, na.rm = T)
```

The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> and the median is 10765.

## What is the average daily activity pattern?

```r
convert_to_time <- function(x) {
  paste0(as.integer(x/100),":", round((x/100 - as.integer(x/100)) * 100))
}
z <- tapply(na.omit(activity_data)$steps, FUN = function(x) {mean(x)}, INDEX = as.factor(na.omit(activity_data)$interval))
max_interval <- names(which(z == max(z)))
max_avg_steps <- z[(which(z == max(z)))[1]]
plot(z, xlab = "Interval", ylab = "Average number of steps", type ="l", x = levels(factor(na.omit(activity_data)$interval)))
abline(v = as.numeric(names(which(z == max(z)))), col=3)
```

![plot of chunk daily activity pattern](figure/daily activity pattern.png) 

The 5-minute interval in which on average across all the days contains the most number of steps is 835.

## Imputing missing values


```r
rows_missing_values <- nrow(activity_data) - nrow(na.omit(activity_data))
```

The activity data  has 2304 rows with missing values. Below, the NA's are replaced with the mean number of steps for the 5-minute interval.


```r
f_activity_data <- activity_data
for (j in 1:nrow(f_activity_data)){
  f_activity_data$steps[j] <- ifelse (is.na(f_activity_data$steps[j]), z[as.factor(f_activity_data$interval)][j], f_activity_data$steps[j])
  }
f_steps.date <- aggregate(steps ~ date, data = f_activity_data, FUN = sum)
hist(f_steps.date$steps, breaks = 10, xlab = "Daily Number of Steps", main = "Histogram of steps per day\nafter replacing NA\'s with the\naverage number of steps for that interval")
```

![plot of chunk replace the missing values with mean value and show histogram](figure/replace the missing values with mean value and show histogram.png) 

And now recalculate the mean and median.


```r
f_mean_total_steps <- mean(f_steps.date$steps)
f_median_total_steps <- median(f_steps.date$steps)
```

Once the NA's are replaced with average number of steps for that interval, the new average of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> (compared to 1.0766 &times; 10<sup>4</sup> before) and the new median is 1.0766 &times; 10<sup>4</sup> (compared to 10765 before).

The values don't change ina a meaningful way as expected, since the NA's were replaced by the averages.

## Are there differences in activity patterns between weekdays and weekends?

We will first separate the weekdays and the weekends and then create two plots, one for the weekends and one for the weekdays.


```r
weekdays <- function(x) {
  format(as.Date(x), "%a") %in% c("Mon", "Tue", "Wed", "Thu", "Fri")
}
w_activity_data <- cbind(activity_data, weekdays(activity_data$date))
names(w_activity_data) <- c(headers, "weekday")
z_weekday <- tapply(subset(na.omit(w_activity_data), weekday)$steps, mean, INDEX = as.factor(subset(na.omit(w_activity_data), weekday)$interval))
z_weekend <- tapply(subset(na.omit(w_activity_data), !weekday)$steps, mean, INDEX = as.factor(subset(na.omit(w_activity_data), !weekday)$interval))
par(mfrow = c(2, 1), oma = c(2, 2, 0, 0), mar=c(3,4,2,2), mgp = c(2, 1, 0), xpd = NA)
plot(z_weekend, xlab = NA, main="Weekend", ylab = "Average Daily Steps", type ="l", x = levels(factor(na.omit(w_activity_data)$interval)))
axis(side = 1, labels =F)
axis(side = 2, labels =F)
plot(z_weekday, xlab = "Interval", main="Weekday", ylab = "Average Daily Steps", type ="l", x = levels(factor(na.omit(w_activity_data)$interval)))
axis(side = 1, labels =T)
axis(side = 2, labels =T)
```

![plot of chunk are there differences between weekdays and weekends](figure/are there differences between weekdays and weekends.png) 

This plot indicates that the pattern for the weekdays and weekends are different. The primary difference seems to be that most of the steps are taken in morning, prior to work hours during the week, while during the weekend it is more evenly spread out.

