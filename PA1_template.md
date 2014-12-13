# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv(unz("activity.zip", "activity.csv"), header=TRUE ,sep=",")
unlink("acitivity.zip")
```
## What is mean total number of steps taken per day?
 

```r
daytotal<- tapply(data$steps,data$date,sum,na.rm=TRUE)
hist(daytotal, main = "Total Number of Steps taken per day", xlab= "", col="powderblue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meansteps<-format(round(mean(daytotal),0),scientific = FALSE)
mediansteps<-format(round(median(daytotal),0),scientific = FALSE)
```

The mean total number of steps taken per day is 9354.  
The median total number of steps taken per day is 10395.

## What is the average daily activity pattern?

```r
intervalmean<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
intervals<-as.numeric(names(intervalmean))
plot(intervals,intervalmean,type="l",xlab="5-minute interval",ylab="average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxinterval<- names(intervalmean)[intervalmean ==max(intervalmean)]
```

On average across all the days in the dataset, interval 835 contains the maximum number of steps.


## Imputing missing values

```r
missing<- sum(is.na(data$steps))
```
Total number of missing values in the dataset is 2304.

If a value is missing, I will use the mean for that 5-minute interval to fill in.

```r
obs<- nrow(data)
newsteps<-numeric(obs)

for (i in 1:obs){
  if(is.na(data$steps[i]))
    newsteps[i]=as.numeric(intervalmean[as.numeric(names(intervalmean))==data$interval[i]])
  else
    newsteps[i]=data$steps[i]
}

date<-data$date
interval<-data$interval
newdata<-data.frame(newsteps,date,interval)
newdaytotal<- tapply(newsteps,date,sum)
hist(newdaytotal, main = "New Total Number of Steps taken per day", xlab= "", col="rosybrown1")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
newmean<-format(round(mean(newdaytotal),0),scientific = FALSE)
newmedian<-format(round(median(newdaytotal),0),scientific = FALSE)
```
The mean total number of steps taken of the new dataset is 10766.  
The median total number of steps taken of the new dataset is 10766.   
Those values are different from the first part of the assignment. Imputing missing data increased the estimates of the total daily number of steps.


## Are there differences in activity patterns between weekdays and weekends?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
newdata1<-mutate(newdata,group=ifelse(weekdays(as.Date(newdata$date))=="Sunday"|weekdays(as.Date(newdata$date))=="Saturday","weekend","weekday"))
newdata1$group<-as.factor(newdata1$group)
newdata2<-group_by(newdata1, group,interval)
wkdy<-summarize(newdata2,mean(newsteps))
names(wkdy)<-c("group","interval","Mean")

library(lattice)
xyplot(wkdy$Mean ~ wkdy$interval | group, data = wkdy, layout= c(1,2),type="l",xlab="Interval",ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
