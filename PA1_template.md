# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1 - Load the data (i.e. read.csv())

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.0.3
```

```r
library(scales)
activity <- read.csv("C:/Users/Erich/Documents/datasciencecoursera/RepData_PeerAssessment1/activity.csv")
```



2 - Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
class(activity$date)
```

```
## [1] "Date"
```


## What is mean total number of steps taken per day?

1 - Make a histogram of the total number of steps taken each day

```r

stepsperdate <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(stepsperdate$steps, names.arg = stepsperdate$date, xlab = "Date", ylab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2 - Calculate and report the mean and median total number of steps taken per day

```r
mean(stepsperdate$steps)
```

```
## [1] 10766
```

```r

median(stepsperdate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across #all days (y-axis)


```r
SI <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(SI, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r

SI$interval[which.max(SI$steps)]
```

```
## [1] 835
```


## Imputing missing values



1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r

sum(is.na(activity))
```

```
## [1] 2304
```


2 -Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3 - Create a new dataset that is equal to the original dataset but with the missing data filled in


```r

activity <- merge(activity, SI, by = "interval", suffixes = c("", ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]
```


4 - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r

steps.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.date$steps, names.arg = steps.date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r

median(steps.date$steps)
```

```
## [1] 10766
```


imputing the missing values does not create a very substantial difference

## Are there differences in activity patterns between weekdays and weekends?

1 - Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r

weekday <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "Weekend"
    } else {
        "Weekday"
    }
}
activity$weekday <- as.factor(sapply(activity$date, weekday))
```



2 - Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r


par(mfrow = c(2, 1))
for (type in c("Weekend", "Weekday")) {
    ST <- aggregate(steps ~ interval, data = activity, subset = activity$weekday == 
        type, FUN = mean)
    plot(ST, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


