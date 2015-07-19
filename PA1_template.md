# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
summary_activity <- aggregate(steps ~ date, data=activity, FUN=sum)
hist(as.numeric(summary_activity$steps))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
sum(summary_activity$steps)
```

```
## [1] 570608
```

```r
mean(summary_activity$steps)
```

```
## [1] 10766.19
```

```r
median(summary_activity$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
# For this I'll take the mean of the daily steps grouped by each interval, across all days

```r
daily <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(daily$interval, daily$steps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Interval with most activity across all days, on average
daily[which.max(daily$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
