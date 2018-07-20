---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## About the data  
## Personal Monitoring Device Data
This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The original source can be downloaded here:  <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

## 1 Loading and tidying the data

Loading from the unzipped version:

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        tz <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",tz)
        unzip(tz)
        unlink(tz)
}

d<-read.csv("activity.csv", header = TRUE,fill=TRUE,blank.lines.skip = TRUE,stringsAsFactors =FALSE )
```


```r
str(d)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(d)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

Tidying the data

* Add a placeholder column for weekend to be used later
* Change the date column data type to Date

    

```r
td<-d
weekend<-td$date
td<-cbind(td,weekend)
td$date<-as.Date(td$date, format= "%Y-%m-%d")
```


```r
str(td)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekend : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(td)
```

```
##      steps             date               interval            weekend     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   2012-10-01:  288  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   2012-10-02:  288  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   2012-10-03:  288  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   2012-10-04:  288  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   2012-10-05:  288  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   2012-10-06:  288  
##  NA's   :2304                                           (Other)   :15840
```

## 2 Histogram of Steps per Day

Aggregate the steps per day  


```r
total_steps_day <- aggregate(steps ~ date, td, sum)
str(total_steps_day)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
summary(total_steps_day)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

Plot the histogram


```r
hist(total_steps_day$steps,main="Total Steps per Day",xlab="Steps", col="gray")
```

![](RR_Wk2_Project1_files/figure-html/stephistogram-1.png)<!-- -->

## 3 Mean and median number of steps taken each day 

Calculate the Mean and Median Steps per Day  


```r
spdmean <- mean(total_steps_day$steps)
spdmedian <- median(total_steps_day$steps)
```

Results:  
The mean Steps per Day is 1.0766189\times 10^{4}.  
The median Steps per Day is 10765.  


## 4 Time series plot of the average number of steps taken

Make a time series plot type="l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

Aggregate the steps per Interval


```r
total_steps_interval <- aggregate(steps ~ interval, td, mean)
str(total_steps_interval)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
summary(total_steps_interval)
```

```
##     interval          steps        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

Plot the interval


```r
plot(total_steps_interval$interval,total_steps_interval$steps, type = "l",ylab="Avg Steps",xlab="Interval")
```

![](RR_Wk2_Project1_files/figure-html/intervalplot-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
spimax <- which.max(total_steps_interval$steps)

im<-total_steps_interval[spimax,1]
```
Results:  
835.  

## 6 Find and impute missing step values

```r
##get the length / # of the missing step values
lm<-length(which(is.na(td$steps)))
```

There are 2304 missing values in the step column.

```r
##  fill in the missing steps with the average / mean step for that interval
id<-transform(td,steps=ifelse(is.na(td$steps)
                              ,total_steps_interval$steps[match(td$interval, total_steps_interval$interval)]
                              ,td$steps)
                )
```

## 7 Histogram of the total number of steps taken each day after missing values are imputed


```r
total_steps_day_id <- aggregate(steps ~ date, id, sum)
str(total_steps_day_id)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
##  $ steps: num  10766 126 11352 12116 13294 ...
```

```r
summary(total_steps_day_id)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

Plot the histogram


```r
hist(total_steps_day_id$steps,main="Total Steps per Day with Imputed Data",xlab="Steps", col="gray")
```

![](RR_Wk2_Project1_files/figure-html/stephistogramid-1.png)<!-- -->

Calculate the Mean and Median Steps per Day with the Imputed Data


```r
spdidmean <- mean(total_steps_day_id$steps)
spdidmedian <- median(total_steps_day_id$steps)
```

Results:  
The mean Steps per Day (Imputed) is 1.0766189\times 10^{4}.  
The median Steps per Day (Imputed) is 1.0766189\times 10^{4}.  
  
What is the difference between the mean and median values of the original and the imputed data


```r
mdiff<-spdmean-spdidmean  
meddiff<-spdmedian-spdidmedian  
```


* The difference between the original and imputed mean number of daily steps is 0.  

* The difference between the original and imputed median number of daily steps is -1.1886792.  


## 8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Calculate whether the date is a weekend or not

```r
##calculate weekend
library(chron)
id<-transform(id,weekend=ifelse(is.weekend(td$date)
                              ,"weekend"
                              ,"weekday")
                )
##calculate avg steps by interval and weekend
total_steps_interval_id <- aggregate(steps ~ interval + weekend, id, mean)
```


```r
library(lattice)

xyplot(total_steps_interval_id$steps ~ total_steps_interval_id$interval|total_steps_interval_id$weekend, main="Avg steps per day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](RR_Wk2_Project1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
