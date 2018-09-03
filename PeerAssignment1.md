---
output: 
  html_document: 
    keep_md: yes
---
Peer Assignment 1 for Coursera's Reproducible Research
=================

* First set the working directory and read the data 

```r
setwd("~/Desktop/Coursera/Reproducible Research")
activity<- read.csv("activity.csv")
```

#I. What is the mean total number of steps taken per day?
* For this part of the assignment, you can ignore the missing values in the dataset.
* Make a histogram of the total number of steps taken each day
* Calculate and report the mean and median total number of steps taken per day


```r
#generate histogram
hist(aggregate(steps~ date,data=activity, FUN=sum)$steps, main='Steps by Day', xlab='Daily steps'
     , ylab='Frequency (number of days)', col='blue')
```

![steps per day histogram](PeerAssignment1_files/figure-html/unnamed-chunk-2-1.png)

```r
#take mean and median
mean(aggregate(steps~ date,data=activity, FUN=sum)[,2]) 
```

```
## [1] 10766.19
```

```r
median(aggregate(steps~ date,data=activity, FUN=sum)[,2]) 
```

```
## [1] 10765
```

```r
mean1<-mean(aggregate(steps~ date,data=activity, FUN=sum)[,2]) 
median1<-median(aggregate(steps~ date,data=activity, FUN=sum)[,2]) 
```

The average number of steps per day is 1.0766189\times 10^{4}. The median is 10765.

#What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
plot2<-aggregate(steps~ interval,data=activity, FUN=mean, na.rm=FALSE)
plot(plot2$interval, plot2$steps, type='l', xlab='Time Interval', ylab='Average Number of Steps',
     col='blue', main='Average Steps by Time of Day')
```

![average steps by time of day](PeerAssignment1_files/figure-html/unnamed-chunk-3-1.png)

```r
plot2[which.max(plot2$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
maxsteps<-plot2[which.max(plot2$steps),1]
```
The time of day with the maximum average number of steps is 835.


#Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
* Create a new dataset that is equal to the original dataset but with the missing data filled in.
* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#How many missing values are in the dataset?
length(which(is.na(activity$steps)))
```

```
## [1] 2304
```

```r
missing<-length(which(is.na(activity$steps)))
```
There are 2304 NA values in the dataset.


```r
#Fill in the missing values with the mean for that 5-minute interval
#creates dataframe of the means by interval
interval_means<-aggregate(steps~ interval,data=activity, FUN=mean, na.rm=FALSE)

#create new dataset equal to the original
df2<-activity

#merge NA rows in main dataset with interval_means. Drop NA means column and rename 'steps' column
new_means<-merge(x=df2[is.na(df2$steps),], y=interval_means, by='interval')
new_means$steps.x<-NULL
colnames(new_means)[colnames(new_means)=='steps.y']<-'steps'

#bind the updated NA rows with the non-NA rows from the original dataset 
df3<-rbind(new_means,df2[!is.na(df2$steps),])

#make a histogram and find the new median and mean
hist(aggregate(steps~ date,data=df3, FUN=sum)$steps,main='Steps by Day', xlab='Daily steps',
     ylab='Frequency (number of days)', col='blue')
```

![](PeerAssignment1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(aggregate(steps~ date,data=df3, FUN=sum)[,2]) 
```

```
## [1] 10766.19
```

```r
median(aggregate(steps~ date,data=df3, FUN=sum)[,2])
```

```
## [1] 10766.19
```

```r
mean2<-mean(aggregate(steps~ date,data=df3, FUN=sum)[,2]) 
median2<-median(aggregate(steps~ date,data=df3, FUN=sum)[,2]) 
```

The new average number of steps per day is 1.0766189\times 10^{4}. The new median is 1.0766189\times 10^{4}. These are identical to those from the first part of the assignment. Imputing the missing values using this manor (replacing missing values with the aveerage value by time interval) does not change the overall median and mean by day.

#Are there differences in activity patterns between weekdays and weekends?
* For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
#create a 'day of week' variable and weekend/weekday variable
df3$dayofweek <- weekdays(as.Date(df3$date))
df3$weekend <- ifelse(df3$dayofweek == "Sunday" | df3$dayofweek == "Saturday"  
                           ,yes="weekend", no="weekday")

#aggregate by interval and weekend/weekday
plot4<-aggregate(steps~ interval+ weekend,data=df3, FUN=mean, na.rm=FALSE)
library(ggplot2)

ggplot(plot4, aes(x = interval, y = steps)) + facet_grid(~ weekend) + geom_line(col='blue')
```

![](PeerAssignment1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

