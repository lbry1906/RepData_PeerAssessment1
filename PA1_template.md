# Activity Monitoring

### I will download the file and read in the data

```r
if (!file.exists("./Week2")) {
  dir.create("./Week2")
}
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "./Week2/Dataset.zip")
download <- date()
unzip(zipfile = "./Week2/Dataset.zip", exdir = "./Week2")
files <- list.files("./Week2", recursive = T)
activity <- read.csv("./Week2/activity.csv")
nwdate <- as.Date(activity$date)
steps <- activity$steps
interval <- activity$interval
```
### This is the histogram, mean, and median of the total number of steps per day


```r
totalSteps <- aggregate(steps~nwdate, data=activity, FUN=sum, na.rm=TRUE)
hist(totalSteps$steps, xlab="Steps", ylab="# of Days",col="blue",  breaks=10,main="Total # of Steps/Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
print(paste("Mean: ", as.character(mean(totalSteps$steps))))
```

```
## [1] "Mean:  10766.1886792453"
```

```r
print(paste("Median: ", as.character(median(totalSteps$steps))))
```

```
## [1] "Median:  10765"
```

### This is the time series plot of the average # of steps/day by interval

```r
stepsByInterval <- aggregate(steps~interval, activity, mean)
with(stepsByInterval, plot(interval, steps, xlab = "Interval", ylab = "Avg # of Steps", type = "l"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### This is the 5-minute interval that contains the most steps

```r
maxstep <- stepsByInterval[which.max(stepsByInterval[,2]),1]
print(maxstep)
```

```
## [1] 835
```

### I will fill in missing data from "steps" with the mean # of steps per interval

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
stepNA <- is.na(activity[,1]) # All NAs are in the steps column
countStepNA <- length(stepNA[stepNA==TRUE])
intmean <- mean(stepsByInterval$steps)
nwactivity <- activity
nwactivity[stepNA,1] <- intmean
head(nwactivity)
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

### I use the mean of steps as the impute value for the missing step values and then create a new Histogram, mean, and median

```r
nwTotalSteps <- aggregate(steps ~ nwdate, data=nwactivity, FUN=sum, na.rm=TRUE)
hist(nwTotalSteps$steps, xlab="Steps",col="blue",  ylab="# of Days", breaks=10,main="New Total # of Steps/Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
meanAfterImput <- mean(nwTotalSteps$steps)
print(meanAfterImput)
```

```
## [1] 10766.19
```

```r
print(mean(totalSteps$steps), na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
medAfterImput <- median(nwTotalSteps$steps)
print(medAfterImput)
```

```
## [1] 10766.19
```

```r
print(median(totalSteps$steps), na.rm=TRUE)
```

```
## [1] 10765
```

Imputing missing data has no impact on the mean, there is a minimal change in the median

### I create a mean variable for dayType (weekday, weekend) and interval and create a plot panel using lattice

```r
library(lattice)
library(dplyr)
nwactivity$date <- as.Date(nwactivity$date)
nwactivity2 <- nwactivity %>% mutate(dayType = ifelse(weekdays(nwactivity$date)=="Saturday"|weekdays(nwactivity$date)=="Sunday","Weekend","Weekday"))
dayTypeAndIntMn <- nwactivity2 %>% group_by(dayType, interval) %>% summarise(dayTypeAndIntMn=mean(steps))
with(dayTypeAndIntMn, xyplot(dayTypeAndIntMn~interval|dayType, type="l", xlab = "Daily Intervals",
                             ylab = "Avg # of Steps", main = "Total Steps by DayType"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Yes, there is a difference between the two plots.
