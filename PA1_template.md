---
title: "PA1_template"
author: "SaharaWang"
date: "2022/2/9"
output: html_document
---
**Loading and preprocessing the data**

```r
library(tidyverse)
```

```
## -- Attaching packages -------------------
```

```
## √ ggplot2 3.3.5     √ purrr   0.3.4
## √ tibble  3.1.4     √ dplyr   1.0.7
## √ tidyr   1.1.4     √ stringr 1.4.0
## √ readr   2.1.1     √ forcats 0.5.1
```

```
## -- Conflicts --- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lattice)
activity <- read.csv("activity.csv", header = T)
```

**What is mean total number of steps taken per day?** 

```r
summary <- activity %>% group_by(date) %>% summarise(sum = sum(steps, na.rm = TRUE))
hist(summary$sum, xlab = "Number of steps", main = "Histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
mean <- mean(summary$sum)
median <- median(summary$sum)
```
The mean of the total number of steps taken per day is 9354.2295082, and the median is 10395.  
**What is the average daily activity pattern?**

```r
summary_daily <- activity %>% group_by(interval) %>% summarise(mean = mean(steps, na.rm = TRUE))
plot(summary_daily, type = "l", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
summary_daily[which.max(summary_daily$mean),]$interval
```

```
## [1] 835
```
**Imputing missing values**
I use the mean for the 5-minute interval to fill in the missing values.  

```r
total_NA <- sum(is.na(activity$steps))
nrow <- as.numeric(nrow(activity))
new_activity <- activity
for(i in 1:nrow){
        if(is.na(activity[i, "steps"]) == TRUE){
               inter <- activity[i, "interval"]
               new_activity[i, "steps"] <- summary_daily[which(summary_daily$interval == inter),]$mean
        }
}
new_summary <- new_activity %>% group_by(date) %>% summarise(sum = sum(steps, na.rm = TRUE))
hist(new_summary$sum, xlab = "Number of steps", main = "Histogram of the total number of steps taken each day -- NA imputed")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
mean <- mean(summary$sum)
median <- median(summary$sum)
```
The total number of missing values in the dataset is 2304.  
**Are there differences in activity patterns between weekdays and weekends?**

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
new_activity$weekdays <- weekdays(as.Date(new_activity$date))
new_activity$weekdays <- as.factor(ifelse(new_activity$weekdays == "Saturday" | new_activity$weekdays == "Sunday", "weekend", "weekday"))
table(new_activity$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
new_summary_daily <- new_activity %>% group_by(interval, weekdays) %>% summarise(mean = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
g <- ggplot(new_summary_daily, aes(interval, mean))
g + geom_line(color = "steelblue") + 
        facet_wrap(.~weekdays, nrow = 2, ncol = 1) +
        labs(x = "Interval", y = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
