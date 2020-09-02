---
title: 'Reproducible Research:Peer Assessment'
author: "Juan David Rico Velasco"
date: "1 de septiembre de 2020"
output: html_document
---
--
loading necessary libraries
```{r}
library(lubridate)
library(dplyr)
library(lattice)
```
## Loading and preprocessing the data
1. reading the csv
```{r}
activity <- read.csv("activity.csv")
head(activity)
```
2. transforming the date column to date type
```{r}
activity$date <- ymd(activity$date)
class(activity$date)
```
 creating a separate dataset without NAs
```{r}
activity_no_NA <- activity[!is.na(activity$steps),]
head(activity_no_NA)
```
## What is mean total number of steps taken per day?

```{r}
total_by_day <- activity_no_NA %>%
  group_by(month = month(date),day = day(date)) %>%
  summarize(total = sum(steps))
head(total_by_day)
```
1. plotting a histogram
```{r}
hist(total_by_day$total, xlab='Total Steps per day', main='frequency of total steps per day', col='blue')
```
```{r}
mean.steps <- mean(total_by_day$total)
median.steps <- median(total_by_day$total)
mean.steps
```
2. So the mean number of total steps per day is `r mean.steps` and the median is `r median.steps` 

## What is the average daily activity pattern?
Separating data by group with dplyr
```{r}
average_by_interval <- activity_no_NA %>%
  group_by(interval = interval) %>%
  summarize( avg = mean(steps, na.rm=TRUE))
head(average_by_interval)
```
1. Now plotting the time series
```{r}
plot(average_by_interval$interval, average_by_interval$avg, main='Average number of steps in each interval',xlab='time interval', ylab='average number of steps',type='l')
```

```{r}
best.interval <- average_by_interval[which.max(average_by_interval$avg),1]
best.avg <- max(average_by_interval$avg)
```
2. The greatest average of steps happens on interval `r best.interval` with a value of `r best.avg`
## Imputing missing values
```{r}
totalNA <- sum(is.na(activity))
```
1. the total number of missing values is `r totalNA`.

2. filling missing values with the average for the interval
```{r}
mean_by_day <- activity_no_NA %>%
               group_by(Month = month(date), Day = day(date)) %>%
  summarise(steps = mean(steps))
activity_filled <- data.frame(activity)
for (i in 1:nrow(activity_filled)) {
  if(is.na(activity_filled[i, "steps"])){
    activity_filled[i,"steps"] <- mean(activity[activity_filled[i,"interval"]==activity$interval,"steps"], na.rm=TRUE)
  }
}
```
```{r}
total_filled <- activity_filled %>%
  group_by(month = month(date),day = day(date)) %>%
  summarize(total = sum(steps))
head(total_by_day)
```
4. plotting a histogram with new data
```{r}
hist(total_filled$total, xlab='Total Steps per day', main='frequency of total steps per day', col='blue')
```
4. the inputed data made the frequency of days with the steps near the mean skyrocket.

## Are there differences in activity patterns between weekdays and weekends?

1. creating factor variable 
```{r}
activity_no_NA <- activity_no_NA %>%
  mutate(weekFactor = factor(ifelse(weekdays(activity_no_NA$date) %in% c("Saturday","Sunday"),"weekend", "weekday"),c("weekday", "weekend")))
```
separate weekdays and weekends
```{r}
avg_week <- activity_no_NA %>%
  group_by(weekfactor = as.character(weekFactor), interval = interval) %>%
  summarise(steps = mean(steps, na.rm=TRUE))
head(avg_week)
```
2. plotting the time series
```{r}
xyplot(steps ~ interval | weekfactor, data=avg_week, type='l', layout=c(1,2))
```

