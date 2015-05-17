# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The repository came with zipped data. First of all, unzip csv file, load it and have a look at it, to be sure the dates and other variables are in the right format.


```r
unzip("./activity.zip")
activity <- read.csv("./activity.csv", colClasses = c("numeric", "Date", "factor"))
summary(activity)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   10     :   61  
##  Median :  0.00   Median :2012-10-31   100    :   61  
##  Mean   : 37.38   Mean   :2012-10-31   1000   :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   1005   :   61  
##  Max.   :806.00   Max.   :2012-11-30   1010   :   61  
##  NA's   :2304                          (Other):17202
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","10","100",..: 1 226 2 73 136 195 198 209 212 223 ...
```
Everything looks ok, we have number of steps as numeric values, dates represented in date format and interval variable is as factor. This will speed up the rest of computations. There is also 2304 NA's, but we will deal with it later.




## What is mean total number of steps taken per day?
Lets calculate the mean number of steps for each day, and save it to new data frame. Plot a histogram to visualize the result. 

```r
stepsMean <- aggregate(steps ~ date, data = activity, sum, na.action = na.omit)
hist(stepsMean$steps, breaks=50, main="Mean number of steps per day", xlab="Total number of steps", ylab="Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
## calculate the mean and median of the total number of steps taken per day
meanSteps <- as.character(mean(stepsMean$steps))
medianSteps <- as.character(median(stepsMean$steps))
```

The mean of the total number of steps taken per day is **10766.1886792453**, and median is **10765**.




## What is the average daily activity pattern?
Lets do some computation, shall we? After it's done, plot the result, to see whats the pattern like throughout a day.

```r
dailyPattern <- aggregate(steps ~ interval, data = activity, FUN = mean)

## we have to convert intervals to integer, otherwise it will not plot well..
library(ggplot2)
dailyPattern$interval <- as.integer(levels(dailyPattern$interval)[dailyPattern$interval])
ggplot(data=dailyPattern, aes(x=interval, y=steps)) +
        geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
