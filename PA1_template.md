---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data=read.csv("./activity.csv",header = TRUE)
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
StepsPerDay=aggregate(steps~date,data,sum,na.rm=TRUE)
hist(StepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
dev.copy(png,file="plot1.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```



## What is mean total number of steps taken per day?

```r
mean(StepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(StepsPerDay$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
StepsPerInterval=aggregate(steps~interval,data,mean,na.rm=TRUE)
plot(StepsPerInterval$steps~StepsPerInterval$interval,type="l",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
StepsPerInterval[which.max(StepsPerInterval$steps),]$interval
```

```
## [1] 835
```



## Imputing missing values

```r
totalMissing=sum(is.na(data$steps))
totalMissing
```

```
## [1] 2304
```
I decided to fill in all of the missing values in the dataset by the mean number of steps per interval.

```r
missingIndex<-is.na(data[,1])
m<-mean(StepsPerInterval$steps)
activity<-data
activity[missingIndex,1]<-m
head(activity)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
TotalSteps=aggregate(steps~date,activity,sum)

hist(TotalSteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
dev.copy(png,file="plot2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
mean(TotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(TotalSteps$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


```r
activity$date<-as.Date(activity$date)
library(dplyr)
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
activity2<-activity%>%
        mutate(dayType= ifelse(weekdays(activity$date)=="Saturday" | weekdays(activity$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)
```

```
##     steps       date interval dayType
## 1 37.3826 2012-10-01        0 Weekday
## 2 37.3826 2012-10-01        5 Weekday
## 3 37.3826 2012-10-01       10 Weekday
## 4 37.3826 2012-10-01       15 Weekday
## 5 37.3826 2012-10-01       20 Weekday
## 6 37.3826 2012-10-01       25 Weekday
```


```r
averageStepByDayTypeAndInterval<-activity2 %>%
  group_by(dayType, interval) %>%
  summarize(averageStepByDay=sum(steps))
```

```
## `summarise()` regrouping output by 'dayType' (override with `.groups` argument)
```

```r
head(averageStepByDayTypeAndInterval)
```

```
## # A tibble: 6 x 3
## # Groups:   dayType [1]
##   dayType interval averageStepByDay
##   <chr>      <int>            <dbl>
## 1 Weekday        0             315.
## 2 Weekday        5             242.
## 3 Weekday       10             231.
## 4 Weekday       15             232.
## 5 Weekday       20             228.
## 6 Weekday       25             283.
```

```r
library(lattice)
with(averageStepByDayTypeAndInterval, 
      xyplot(averageStepByDay ~ interval | dayType, 
      type = "l",      
      main = "Total Number of Steps within Intervals by dayType",
      xlab = "Daily Intervals",
      ylab = "Average Number of Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
dev.copy(png,file="plot3.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

