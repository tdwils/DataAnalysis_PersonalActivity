# Reproducible Research: Analysis of Personal Activity Data

This report analyzes data from a personal activity monitoring device. The device collects data at 5-minute intervals throughout the day. The dataset consist of two months of data from an anonymous individual collected during the months of October and November, 2012, and includes the number of steps taken in 5-minute intervals each day.


```r
# Set global options and load libraries
knitr::opts_chunk$set(echo = TRUE, comment = NA)
library(dplyr)
library(ggplot2)
```

## Load and preprocess the data

We load the dataset and convert the date values from character to date format so we can analyze them.



```r
actdata <- read.csv("activity.csv", sep = ",", header = TRUE)
actdata$date <- as.Date(actdata$date)
```


## What is the mean total number of steps taken per day?

We want to know what is the mean number of steps that the person takes in one day. First, we calculate the total number of steps taken each day and plot a histogram to visualize the results. Then, we calculate the mean and median of the total steps taken per day. 

For these calculations, we ignore missing values in the dataset.


```r
days <- group_by(actdata, date)
totals <- summarize(days, totalperday = sum(steps, na.rm=TRUE))

ggplot(data = totals, aes(totals$totalperday)) +
    geom_histogram(breaks=seq(0, 24000, by=2000), col="red") +
    labs(title="Histogram for Total Steps per Day") +
    labs(x = "Steps per day") + 
    scale_x_continuous(limits=c(0, 24000), breaks=seq(0,24000,4000))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

```r
meanperday <- mean(totals$totalperday)
medianperday <- median(totals$totalperday)
```

The mean of the total number of steps taken per day is 9354 steps.

The median of the total number of steps taken per day is 10395 steps.

## What is the average daily activity pattern?

Now we look at the activity over the course of a day, averaged across all days, to determine how the activity level varies throughout the day.  

We calculate the average number of steps per interval, averaged across all days, and plot this versus time (i.e., the intervals in one day).


```r
intervals <- group_by(actdata, interval)
avgintervals <- summarize(intervals, avg = mean(steps, na.rm=TRUE))

ggplot(avgintervals, aes(x = interval, y = avgintervals$avg)) +
    geom_line() +
    labs(x = "Interval", y = "Average number of steps") +
    labs(title="Steps per interval, averaged over all days") 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

```r
ind <- which(avgintervals$avg == max(avgintervals$avg))
maxinterval <- avgintervals$interval[ind]
```

The 5-minute interval that contains the maximum number of steps, on average across all the days, is interval #835.

## Imputing missing values

Now we want to replace missing values in a meaningful way. First, we calculate the number of missing values in the dataset.


```r
numnas <- sum(is.na(actdata))
```

The total number of missing values in the dataset is 2304.

This is a significant proportion of the data (13%), so we will replace each missing value with the mean for that 5-minute interval.


```r
data_imp <- actdata

# Indices of rows in original dataset with missing values
indna <- which(is.na(data_imp$steps)) 

# Indices of intervals corresponding to missing values
avgindices <- which(avgintervals$interval==data_imp$interval[indna])

# Replace missing values with the mean number of steps for that 5-minute interval
data_imp$steps[indna] <- avgintervals$avg[avgindices]
```

Now we create a histogram and compare to the results we got previously when we ignored the missing values.


```r
days_imp <- group_by(data_imp, date)
totals_imp <- summarize(days_imp, totalperday = sum(steps, na.rm=TRUE))

ggplot(data = totals_imp, aes(totals_imp$totalperday)) +
    geom_histogram(breaks=seq(0, 24000, by=2000), col="red") +
    labs(title="Histogram for Total Steps per Day (missing values imputed)") +
    labs(x = "Steps per day") + 
    scale_x_continuous(limits=c(0, 24000), breaks=seq(0,24000,4000))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)\

```r
meanperday_imp <- mean(totals_imp$totalperday)
medianperday_imp <- median(totals_imp$totalperday)
```

After imputing missing data, the mean of the total number of steps taken per day is 9530 steps.

After imputing missing data, the median of the total number of steps taken per day is 10439 steps.

The effect of imputing missing data is to increase the total number of steps per day.


## Are there differences in activity patterns between weekdays and weekends?

We create a plot of the number of steps per 5-minute interval, averaged across all weekday days or weekend days, to compare the activity levels on weekdays and weekends.


```r
data_imp <- mutate(data_imp, wday = weekdays(data_imp$date))
data_imp$wday[which(data_imp$wday=="Sunday" | data_imp$wday=="Saturday")] <- "weekend"
data_imp$wday[which(data_imp$wday!="weekend")] <- "weekday"
data_imp$wday <- factor(data_imp$wday)

wdaygroups <- group_by(data_imp, interval, wday)
avgwdays <- summarize(wdaygroups, avg = mean(steps, na.rm=TRUE))

ggplot(avgwdays, aes(x = interval, y = avg)) +
    geom_line() +
    labs(x = "Interval", y = "Average number of steps") +
    labs(title="Average Steps per Interval") +
    facet_grid(wday ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)\
