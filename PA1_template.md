---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
* Load the data (i.e. read.csv())

```r
setwd('C:/Users/Administrator/Desktop')
data <- read.csv('activity.csv', header = TRUE, sep = ',')
```

* Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(dplyr)
data_by_day <- summarise(group_by(data, date), total = sum(steps, na.rm = TRUE))
```




## What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day

```r
hist(data_by_day$total, main = 'Total number of steps each day', 
    xlab = 'number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

* Calculate and report the mean and median total number of steps taken per day

```r
mean(data_by_day$total, na.rm = TRUE)
```

```
## [1] 9354
```

```r
median(data_by_day$total, na.rm = TRUE)
```

```
## [1] 10395
```




## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data_by_interval <- summarise(group_by(data, interval), mean_step = mean(steps, na.rm = TRUE))
plot(mean_step~interval, data = data_by_interval, type = "l", main = 'Average steps in interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
data_by_interval[which.max(data_by_interval$mean_step), ]$interval
```

```
## [1] 835
```



-------------------------------------------------------------------------
## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc
* Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
get_interval_step <- function(interval){
    data_by_interval[data_by_interval$interval==interval,]$steps
}
filled_data <- data   
for(i in 1:nrow(filled_data)){
    if(is.na(filled_data[i,]$steps)) {
        filled_data[i,]$steps <- get_interval_step(filled_data[i,]$interval)
    }
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps

```r
totalSteps2 <- aggregate(steps~date, data = filled_data, sum)
hist(totalSteps2$steps, main = 'Total number of steps each day', 
    xlab = 'number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

```r
mean(totalSteps2$steps)
```

```
## [1] 10766
```

```r
median(totalSteps2$steps)
```

```
## [1] 10765
```



## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
filled_data$day <- ifelse(as.POSIXlt(as.Date(filled_data$date))$wday %% 6 == 0,
                          "weekend", "weekday")
filled_data$day <- factor(filled_data$day, levels = c("weekday", "weekend"))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data

```r
stepsInterval2 <- aggregate(steps~interval+day, filled_data, mean)
library(lattice)
xyplot(steps~interval|factor(day), data = stepsInterval2, aspect = 1/2, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

