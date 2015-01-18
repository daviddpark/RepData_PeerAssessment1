---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activities <- read.csv("activity.csv", colClasses=c("numeric","POSIXct","numeric"), stringsAsFactors=FALSE)
```

## What is mean total number of steps taken per day?

First we will summarize the data, grouping by date, then sum over all the intervals.


```r
day_groups <- group_by(activities, date)
bydate <- summarize(day_groups, total=sum(steps, na.rm=TRUE))
```

Here is a histogram of total steps taken per day:


```r
hist(bydate$total, main="Histogram of total steps taken per day", xlab="Total Steps Taken per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Now, to calculate both the mean value and the median value of the total steps taken per day:


```r
mean_steps <- as.integer(mean(bydate$total, na.rm=TRUE))
median_steps <- as.integer(median(bydate$total, na.rm=TRUE))
```

The mean value of the total steps taken per day is 9354.

The median value of the total steps taken per day is 10395.

## What is the average daily activity pattern?

First, let's group by interval, then summarize the total steps by interval.


```r
intervals <- group_by(activities, interval) 
byinterval <- summarize(intervals, avg=mean(steps, na.rm=TRUE))
```

Next, a timeseries plot (after transforming the intervals to POSIX time) will provide some insight into most active periods of the day:


```r
with(byinterval, plot(strptime(sprintf("%04s", interval), "%H%M"), avg, type="l", xlab="5 minute intervals", ylab="Average Steps Taken"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Let's pinpoint the interval that averaged the most steps in the summary:

```r
intvl_max <- sprintf("%04s", byinterval[which.max(byinterval$avg),]$interval)
```

So this individual seems to take the most steps in the five minute interval around 08:35

## Imputing missing values

Let's find how many step counts we are missing:


```r
missing <- count(filter(activities, is.na(steps)))$n
```
The total number of missing values for reported steps is 2304.

As a missing value imputation strategy, let's set any NA values in the imputed data set
to the mean value of the interval over all days in the data set.

```r
imputeStepsIfNA <- function(interval, steps) {
  if (is.na(steps)) {
    as.integer(filter(byinterval, interval==interval)[1,2])
  } else {
    steps
  }
}
```

Here is a new dataset that is identical to the original activities dataset,
but has missing steps replaced with imputed values from the average of the
given interval:


```r
imputed <- activities
imputed$steps <- mapply(imputeStepsIfNA, imputed$interval, imputed$steps)
```
In order to determine the mean and median values of total number of steps taken per day
given that missing values are filled in with imputed values, we will summarize the data,
grouping by date.

```r
imputed_day_groups <- group_by(imputed, date)
imputedbydate <- summarize(imputed_day_groups, total=sum(steps, na.rm=TRUE))
```

Here is a histogram of total steps taken per day, but this time including imputed values:


```r
hist(imputedbydate$total, main="Histogram of total steps taken per day", xlab="Total Steps Taken per Day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

Now, to calculate both the mean value and the median value of the total steps taken per day:


```r
imp_mean_steps <- as.integer(mean(imputedbydate$total))
imp_median_steps <- as.integer(median(imputedbydate$total))
```

The mean value of the total steps taken per day including imputed values is 9354.

The median value of the total steps taken per day including imputed values is 10395.

Considering that these values do not differ from the values determined when ignoring the missing data, we can determine that with this particular approach to imputing values, there is no impact to the mean and median of the steps.

## Are there differences in activity patterns between weekdays and weekends?

In order to determine the answer to this question, we must group our data differently, so we will add a new column to our dataset that identifies whether a given observation is on a weekday or a weekend.


```r
dayType <- function(date) {
  if (weekdays(date) %in% c("Saturday","Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
imputed$daytype <- factor(mapply(dayType, imputed$date))
```

Next, we will filter by weekday and create a data frame summarized by the grouping over intervals for all weekdays.


```r
weekday_ints <- group_by(filter(imputed, daytype=='weekday'), interval)
weekday_by_interval <- summarize(weekday_ints, avg=mean(steps))
```

Next, we will filter by weekend and create a data frame summarized by the grouping over intervals for all weekend days.


```r
weekend_ints <- group_by(filter(imputed, daytype=='weekend'), interval)
weekend_by_interval <- summarize(weekend_ints, avg=mean(steps))
```

Next, we will create two plots, showing how the weekend and weekday intervals compare.


```r
par(mfrow=c(2,1))
with(weekday_by_interval, plot(strptime(sprintf("%04s", interval), "%H%M"), avg, type="l",
    xlab="5 minute intervals", ylab="Average Steps Taken", main="Weekday Activity"))
with(weekend_by_interval, plot(strptime(sprintf("%04s", interval), "%H%M"), avg, type="l",
    xlab="5 minute intervals", ylab="Average Steps Taken", main="Weekend Activity"))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 
