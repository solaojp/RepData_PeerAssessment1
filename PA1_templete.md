Reproducible Research course project 1

A) Loading and preprocessing data
Acitivity.csv is downloaded on my machine locally in working directory.
If you want to download data
click on https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
```{r,echo=TRUE}
library(ggplot2)
library(plyr)
library(lattice)
activity <- read.csv("activity.csv")
activity$weekdays <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date,format = "%Y-%m-%d")
new_activity <- activity[!is.na(activity$steps),]
```
1) What is mean total number of steps taken per day?
```{r,echo=TRUE}
steps_perday <- aggregate(steps ~ date,activity,"sum")
```
Plot for the function
```{r,echo=TRUE}
hist(steps_perday$steps ,xlab="steps",main = "Total Steps per Day")
```
Mean and median
```{r,echo=TRUE}
mean <- as.integer(mean(steps_perday$steps))
mean
median <- as.integer(median(steps_perday$steps))
median
```

2)What is the average daily activity pattern?
```{r,echo=TRUE}
steps_perinterval <- aggregate(steps ~ interval,activity , "mean")
```
Plot for the function
```{r,echo=TRUE}
plot(aggregate(steps ~interval, activity, "mean"), type = "l")
```
The plotshows that on an average acroos all days,maximum number of steps is 835
```{r,echo=TRUE}
max_interval <- steps_perinterval[which.max(steps_perinterval$steps),1]
max_interval
```
3)Imputing missing values

3.1) Number of missing values in orignal activity dataset
```{r,echo=TRUE}
sum(is.na(activity))
```

3.2)Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I intend to substitute each 'NA' with a 'value'. the 'value' will be overall mean of steps.

```{r,echo=TRUE}
activity_new <- transform(activity,steps = ifelse(is.na(activity$steps),steps_perinterval$steps[match(activity$interval,steps_perinterval$interval)],activity$steps))
```
3.3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r,echo=TRUE}
steps_perdaywoNA <-aggregate(steps ~ date,activity_new ,"sum")
```

3.4)Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r,echo=TRUE}
hist(steps_perdaywoNA$steps, breaks = 5,main ="Overlapping Histogram",col= "black", xlab="Number of steps")
hist(steps_perday$steps , breaks=5 , col = "grey" ,add=T)
legend("right", c("Imputed", "Non-imputed"), col=c("grey", "black"))
```
Mean and median
```{r,echo=TRUE}
rmean <- as.integer(mean(steps_perdaywoNA$steps))
rmean
rmedian <- as.integer(median(steps_perdaywoNA$steps))
rmedian
```
4) Are there differences in activity patterns between weekdays and weekends?
4.1)Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r,echo=TRUE}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity_new$dow = as.factor(ifelse(is.element(weekdays(as.Date(activity_new$date)),weekdays), "Weekday", "Weekend"))
steps_perintervalwoNA <- aggregate(steps ~ interval + dow, activity_new, "mean")
```
Plot for the function
```{r,echo=TRUE}
xyplot(steps_perintervalwoNA$steps ~ steps_perintervalwoNA$interval|steps_perintervalwoNA$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")