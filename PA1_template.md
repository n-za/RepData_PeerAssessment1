# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

There are 3 variables:
* steps: number of steps taken in the given 5 minute interval
* date: date stamp of the measurement. The character representation will be converted into a date using as.Date().
* interval: identifier of the 5 minute interval: five past midnight is represented by 5, 11:55 AM by 1155, for example. We will let the identifiers as they are.

The result of the load operation will be shown on the summary of the data frame.


```r
opts_chunk$set(echo = TRUE)
setwd("/Volumes/Gargantua/Coursera/Reproducible Research/Project1")
data <- read.csv("activity.csv", sep = ",", header = TRUE, na.strings = "NA", 
    colClasses = c("numeric", "character", "numeric"))
data$date <- as.Date(data$date, "%Y-%m-%d")
summary(data)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```


## What is mean total number of steps taken per day?


```r
# group by date
perDay <- split(data, as.factor(data$date), drop = TRUE)
# sum up by date
TotPerDay <- sapply(perDay, function(x) sum(x$steps))
# display the histogram
hist(TotPerDay, breaks = 15, main = "Histogram of Total Number of Steps per Day", 
    xlab = "Steps per Day")
# add the mean on the graphic
meanPerDay <- mean(TotPerDay, na.rm = TRUE)
abline(v = meanPerDay, col = "red")
# add the median on the graphic
medianPerDay <- median(TotPerDay, na.rm = TRUE)
abline(v = medianPerDay, col = "blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean and the median are very close:
* mean (in red) = 1.0766 &times; 10<sup>4</sup>
* median (in blue) = 1.0765 &times; 10<sup>4</sup>

On the histogram only the median is visible. We have used 15 bins for the histogram: it is more accurate than the default of 10.


## What is the average daily activity pattern?


```r
# group by interval identifier
perInterval <- split(data, data$interval, drop = TRUE)
# compute the mean by interval identifier
AveragePerInterval <- sapply(perInterval, function(x) mean(x$steps, na.rm = T))
# extract the labels (interval identifier) from the name of the result
# vector
xlabels <- names(AveragePerInterval)
# compute the maximum of the averages
maxindex = which.max(AveragePerInterval)
plot(xlabels, AveragePerInterval, type = "l", main = "Number of Steps per Interval Identifier")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The maximum number of steps per interval identifier is about 206.1698 steps and is reached in the 104-th interval identifier in the day. This interval is labeled 835 i.e. 8:35 AM.


## Imputing missing values
* The total number of observations is 17568.
* The total number of complete observations is 15264.
* The total number of incomplete observations (ie with some NA) is 2304.

```r
summary(data)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

There are only missing values in the *steps* column, as shown by the summary.

In order to choose a value for the NA, we take the average number of steps during the given interval over all the days. This is simple and will ensure that every NA will be eliminated (on this data set).


```r
# setup a data frame with the interval and the average number of steps
AveragePerInterval.as.frame <- data.frame(interval = names(AveragePerInterval), 
    steps = AveragePerInterval)
# merge the data set and the previous frame on the interval column
dataWithoutNA <- merge(data, AveragePerInterval.as.frame, by = "interval")
# let us have a look on the join
summary(dataWithoutNA)
```

```
##     interval       steps.x           date               steps.y      
##  Min.   :   0   Min.   :  0.0   Min.   :2012-10-01   Min.   :  0.00  
##  1st Qu.: 589   1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.:  2.49  
##  Median :1178   Median :  0.0   Median :2012-10-31   Median : 34.11  
##  Mean   :1178   Mean   : 37.4   Mean   :2012-10-31   Mean   : 37.38  
##  3rd Qu.:1766   3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.: 52.83  
##  Max.   :2355   Max.   :806.0   Max.   :2012-11-30   Max.   :206.17  
##                 NA's   :2304
```

```r
# derive the corrected number of steps (the column is simply called steps,
# where the column from the data set is named steps.x and steps.y is the
# number of steps on average during the given interval.
dataWithoutNA$steps <- ifelse(is.na(dataWithoutNA$steps.x), dataWithoutNA$steps.y, 
    dataWithoutNA$steps.x)
# remove steps.x and steps.y
cleandata <- dataWithoutNA[, c(-2, -4)]
summary(cleandata)
```

```
##     interval         date                steps      
##  Min.   :   0   Min.   :2012-10-01   Min.   :  0.0  
##  1st Qu.: 589   1st Qu.:2012-10-16   1st Qu.:  0.0  
##  Median :1178   Median :2012-10-31   Median :  0.0  
##  Mean   :1178   Mean   :2012-10-31   Mean   : 37.4  
##  3rd Qu.:1766   3rd Qu.:2012-11-15   3rd Qu.: 27.0  
##  Max.   :2355   Max.   :2012-11-30   Max.   :806.0
```

As shown by the summary eac NA has been overriden by a default value (average over days for the given interval identifier).

Let us have a look on the histogram of the first section, but with the corrected data.

```r
# compare the histogram gotten in the first section with the histogram of
# the data set without NA
perDay <- split(cleandata, as.factor(cleandata$date), drop = TRUE)
TotPerDay <- sapply(perDay, function(x) sum(x$steps))
hist(TotPerDay, breaks = 15, main = "Histogram of Total Number of Steps per Day", 
    xlab = "Steps per Day")
meanPerDay <- mean(TotPerDay, na.rm = TRUE)
abline(v = meanPerDay, col = "red")
medianPerDay <- median(TotPerDay, na.rm = TRUE)
abline(v = medianPerDay, col = "blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

The mean and the median are very close:
* mean (in red) = 1.0766 &times; 10<sup>4</sup>
* median (in blue) = 1.0766 &times; 10<sup>4</sup>

The two histograms have not the same shape. The correction has mainly an impact on days with a small number of steps per day (notice the median is now lower), or a very large number of steps per day.

## Are there differences in activity patterns between weekdays and weekends?
We build on the clean data set (without NA), and classify each date as a weekday or a weekend day ("Dimanche" and "Samedi").

```r
cleandata$weekday <- weekdays(cleandata$date)
cleandata$daytype <- factor(ifelse(cleandata$weekday == "Dimanche" | cleandata$weekday == 
    "Samedi", "weekend", "weekday"))
summary(cleandata$daytype)
```

```
## weekday weekend 
##   12960    4608
```

We split the NA-corrected data set into two data frames:
* work for the week days
* weekend for the weekend days.


```r
# create the data frame for week days
work <- cleandata[cleandata$daytype == "weekday", ]
# group by interval for work days
perIntervalWork <- split(work, work$interval, drop = TRUE)
# average for each interval
AveragePerIntervalWork <- sapply(perIntervalWork, function(x) mean(x$steps, 
    na.rm = T))
# retrieve the labels of the interval identifier
xlabelsWork <- names(AveragePerIntervalWork)
# create the data frame for weekend.
weekend <- cleandata[cleandata$daytype == "weekend", ]
# group by interval for weekend
perIntervalEnd <- split(weekend, weekend$interval, drop = TRUE)
# average for each interval
AveragePerIntervalEnd <- sapply(perIntervalEnd, function(x) mean(x$steps, na.rm = T))
# retrieve the labels of the interval identifier
xlabelsEnd <- names(AveragePerIntervalEnd)
# setup a common data frame of the results in order to display them in a
# common boxplot.
boxdataWork <- cbind(interval = names(AveragePerIntervalWork), steps = AveragePerIntervalWork, 
    daytype = "weekday")
boxdataEnd <- cbind(interval = names(AveragePerIntervalEnd), steps = AveragePerIntervalEnd, 
    daytype = "weekend")
boxdata <- data.frame(rbind(boxdataWork, boxdataEnd))
# an oddity: the steps data has been represented as a factor: convert them
# back into numeric data (otherwise the boxplot returns an error)
boxdata$steps <- as.numeric(boxdata$steps)
# plot the time series as required
par(mfrow = c(2, 1))
plot(xlabelsWork, AveragePerIntervalWork, type = "l", xlab = "Interval Index", 
    ylab = "Number of Steps", main = "Number of Steps per Interval Index (Weekday)")
plot(xlabelsEnd, AveragePerIntervalEnd, type = "l", xlab = "Interval Index", 
    ylab = "Number of Steps", main = "Number of Steps per Interval Index (Weekend)")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-81.png) 

```r
par(mfrow = c(1, 1))
# bonus: boxplot on the same data
boxplot(steps ~ daytype, data = boxdata, main = "Spread of Number of Steps per Interval for weekday and weekend")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-82.png) 

As we see the avergages are about the same, however the spread is higher on weekends (25%-75% quantile interval is larger for the weekend).
During the weekend the activity starts on average later, and more sustained during the day, whereas during the weekdays there is a clear activity peak in the morning.
