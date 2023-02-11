---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true 
--- 


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

```
## Loading required package: patchwork
```

## Loading and preprocessing the data
(1) Load the data (i.e. read.csv())

```r
data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
```


## What is mean total number of steps taken per day?
(1) Calculate the total number of steps taken per day
(2) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
(3) Calculate and report the mean and median of the total number of steps taken per day


```r
steps <- data %>% group_by(date) %>% summarise(steps_per_day = sum(steps,na.rm = TRUE), .groups = 'drop')
ggplot(steps, aes(x=steps_per_day)) + geom_histogram(binwidth = 500) 
```

![](PA1_template_files/figure-html/step_per_day-1.png)<!-- -->

```r
median(steps$steps_per_day)
```

```
## [1] 10395
```

```r
mean(steps$steps_per_day)
```

```
## [1] 9354.23
```


## What is the average daily activity pattern?
(1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
(2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
data_interval <- data %>% group_by(interval) %>% summarise(steps_by_interval=mean(steps, na.rm = TRUE)) 
n <- nrow(data_interval)
data_2 <- cbind(data_interval,indx=c(1:n))
ggplot(data=data_2,mapping = aes(x=indx,y=steps_by_interval)) + geom_line() +
        scale_x_continuous(breaks = data_2$indx[c(50,100,150,200,250)], 
                labels = data_2$interval[c(50,100,150,200,250)])
```

![](PA1_template_files/figure-html/plot_ts-1.png)<!-- -->

```r
max(data_interval$steps_by_interval)
```

```
## [1] 206.1698
```

## Imputing missing values
(1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
(2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
(3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
(4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
data_new <- left_join(data, data_interval, by="interval")  %>%
        mutate(steps=ifelse(is.na(steps),steps_by_interval,steps)) %>%
        select(-steps_by_interval)
        
steps_new <- data_new %>% group_by(date) %>% summarise(steps_per_day = sum(steps,na.rm = TRUE), .groups = 'drop')
ggplot(steps_new, aes(x=steps_per_day)) + geom_histogram(binwidth = 500) 
```

![](PA1_template_files/figure-html/imputing-1.png)<!-- -->

```r
median(steps_new$steps_per_day)
```

```
## [1] 10766.19
```

```r
mean(steps_new$steps_per_day)
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?
(1) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
(2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(chron)
data_w <- data_new  %>% 
        mutate(wday=ifelse(is.weekend(as.Date(date)),"weekend","weekday"))

step_weekday <- data_w[data_w$wday=="weekday",] %>% group_by(interval) %>%
        summarise(msteps=mean(steps))
step_weekday <- cbind(step_weekday,indx=c(1:nrow(step_weekday)))
p_weekday = ggplot(step_weekday, aes(x=indx,y=msteps)) + geom_line() +  
         scale_x_continuous(breaks = step_weekday$indx[c(50,100,150,200,250)], 
                          labels = step_weekday$interval[c(50,100,150,200,250)])

step_weekend <- data_w[data_w$wday=="weekend",] %>% group_by(interval) %>%
        summarise(msteps=mean(steps))        
step_weekend <- cbind(step_weekend,indx=c(1:nrow(step_weekend)))
p_weekend = ggplot(step_weekend, aes(x=indx,y=msteps)) + geom_line() +  
         scale_x_continuous(breaks = step_weekend$indx[c(50,100,150,200,250)], 
                          labels = step_weekend$interval[c(50,100,150,200,250)])

p_weekend / p_weekday
```

![](PA1_template_files/figure-html/2factors-1.png)<!-- -->




