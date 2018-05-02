---
title: "Reproducible Research: Peer Assessment 1- Yogesh Godbole"
output: 
  html_document:
    keep_md: true
    eval : true
---
===================================================================

## Loading and preprocessing the data
### set the working directory where the file is located
### invoke the libraries which are required

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
activity <- read.csv("activity.csv")
```

### check the contents using str, head and summary

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "10/1/2012","10/10/2012",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   10/1/2012 :  288   Min.   :   0.0  
##  1st Qu.:  0.00   10/10/2012:  288   1st Qu.: 588.8  
##  Median :  0.00   10/11/2012:  288   Median :1177.5  
##  Mean   : 37.38   10/12/2012:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   10/13/2012:  288   3rd Qu.:1766.2  
##  Max.   :806.00   10/14/2012:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(activity)
```

```
##   steps      date interval
## 1    NA 10/1/2012        0
## 2    NA 10/1/2012        5
## 3    NA 10/1/2012       10
## 4    NA 10/1/2012       15
## 5    NA 10/1/2012       20
## 6    NA 10/1/2012       25
```

## What is mean total number of steps taken per day?
### For the following tasks, we will need to remove missing values. So we create a second version of the data without missing values.


```r
act.complete <- na.omit(activity)
act.day <- group_by(act.complete, date)
act.day <- summarize(act.day, steps=sum(steps))
summary(act.day)
```

```
##          date        steps      
##  10/10/2012: 1   Min.   :   41  
##  10/11/2012: 1   1st Qu.: 8841  
##  10/12/2012: 1   Median :10765  
##  10/13/2012: 1   Mean   :10766  
##  10/14/2012: 1   3rd Qu.:13294  
##  10/15/2012: 1   Max.   :21194  
##  (Other)   :47
```

```r
qplot(steps, data=act.day)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(act.day$steps)
```

```
## [1] 10766.19
```

```r
median(act.day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

###First we create a data frame in which steps are aggregated into averages within each 5 minute interval:

```r
act.int <- group_by(act.complete, interval)
act.int <- summarize(act.int, steps=mean(steps))
ggplot(act.int, aes(interval, steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 5 minute interval with max steps

```r
act.int[act.int$steps==max(act.int$steps),]
```

```
## # A tibble: 1 x 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```

## Imputing missing values
### rows with missing values
### the mean doesnt change after imputing missing values with the mean.

```r
nrow(activity)-nrow(act.complete)
```

```
## [1] 2304
```

```r
names(act.int)[2] <- "mean.steps"
act.impute <- merge(activity, act.int)
act.impute$steps[is.na(act.impute$steps)] <- act.impute$mean.steps[is.na(act.impute$steps)]
act.day.imp <- group_by(act.impute, date)
act.day.imp <- summarize(act.day.imp, steps=sum(steps))
qplot(steps, data=act.day.imp)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
mean(act.day.imp$steps)
```

```
## [1] 10766.19
```

```r
median(act.day.imp$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
act.impute$dayofweek <- weekdays(as.Date(act.impute$date))
act.impute$weekend <-as.factor(act.impute$dayofweek=="Saturday"|act.impute$dayofweek=="Sunday")
levels(act.impute$weekend) <- c("Weekday", "Weekend")
act.weekday <- act.impute[act.impute$weekend=="Weekday",]
act.weekend <- act.impute[act.impute$weekend=="Weekend",]
act.int.weekday <- group_by(act.weekday, interval)
act.int.weekday <- summarize(act.int.weekday, steps=mean(steps))
act.int.weekday$weekend <- "Weekday"
act.int.weekend <- group_by(act.weekend, interval)
act.int.weekend <- summarize(act.int.weekend, steps=mean(steps))
act.int.weekend$weekend <- "Weekend"
act.int <- rbind(act.int.weekday, act.int.weekend)
act.int$weekend <- as.factor(act.int$weekend)
ggplot(act.int, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
