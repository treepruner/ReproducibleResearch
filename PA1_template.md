# PA1_template
treepruner  
November 2, 2015  

rm(list = ls())




#### Overview

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


The variables included in this dataset are:

 * steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

 * date: The date on which the measurement was taken in YYYY-MM-DD format

 * interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.



```r
library(dplyr)
library(ggplot2)
library(sqldf)
library(lubridate)
library(mosaic)
```




#### Get Data

Download, unzip and read in the file.


```r
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, "./proj1/repdata%2Fdata%2Factivity.zip", method = "curl")
unzip(zipfile = "repdata%2Fdata%2Factivity.zip")
activity <- read.csv("./proj1/activity.csv")
```

#### Exploratory Analyses

 * determine the # of records
 * determine layout of file
 * check for missing data
 * look at summaries
 * create exploratory plots


```r
activity_rowCnt <-nrow(activity)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity[5000:5005,]
```

```
##      steps       date interval
## 5000   757 2012-10-18      835
## 5001   608 2012-10-18      840
## 5002   568 2012-10-18      845
## 5003   571 2012-10-18      850
## 5004   355 2012-10-18      855
## 5005    55 2012-10-18      900
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```



```r
plot(activity$date, activity$steps)
```

![](PA1_template_files/figure-html/date_steps_plot-1.png) 


#### Clean and Preprocess Data

The date is currently a factor and needs to be converted to a date. Later in the analysis we need to determine a weekend day from a weekday. Use dplyr and mosaic packages to create a new factor variable to indicate weekend or weekday.



```r
activity$modDate <- as.Date(activity$date)

weekend <-c("Saturday", "Sunday")

activity <- mutate(activity,
  dayType = derivedFactor(
 "weekend" = (weekdays(activity$modDate) %in% weekend),
 .method = "first",
 .default = "weekday"
))
```


Create a new dataset with only the completed cases and keep track of the number of rows. 


```r
cc <-activity[complete.cases(activity),]
cc_rowCnt <- nrow(cc)
incomplete <- activity_rowCnt - cc_rowCnt
```


### What is mean total number of steps taken per day for complete cases?

dplyr is the most understandable and easiest way to do this:


```r
stepsByDate <- 
        cc %>% 
        group_by(modDate) %>% 
        summarise(n_date = n(), sum_dateSteps = sum(steps) , mean_dateSteps = mean(steps))
```


### Histogram of Complete Case Steps by Date


```r
hist(stepsByDate$sum_dateSteps,
     xlab = "Steps",
     breaks = 10,
     main = "Complete Cases Daily Steps Histogram"
     )
```

![](PA1_template_files/figure-html/cc_stepsByDate_histogram-1.png) 



```r
datasetMean <- mean(stepsByDate$sum_dateSteps)
datasetMedian <- median(stepsByDate$sum_dateSteps)
```

The mean of the total number of steps for complete cases taken per day is 10766.19.

The median of the total number of steps for complete cases taken per day is 10765.

### What is the average daily activity pattern for complete cases?

Use dplyr to create summaries by 5 minute interval:



```r
stepsByInterval <- 
        cc %>% 
        group_by(interval) %>% 
        summarise(n_interval = n(), sum_intervalSteps = sum(steps) , mean_intervalSteps = mean(steps))
```



### Times Series Plot

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsByInterval[which(stepsByInterval$mean_intervalSteps == max(stepsByInterval$mean_intervalSteps)),]
```

```
## Source: local data frame [1 x 4]
## 
##   interval n_interval sum_intervalSteps mean_intervalSteps
## 1      835         53             10927           206.1698
```

```r
maxInterval <- stepsByInterval[which(stepsByInterval$mean_intervalSteps == max(stepsByInterval$mean_intervalSteps)),1]
```



```r
plot(stepsByInterval$interval, stepsByInterval$mean_intervalSteps,
    type = "l",
    ylab = "Mean Steps",
    xlab = "Interval",
    main = "Time Series of Complete Cases")
    abline(v = maxInterval, col = "red")
```

![](PA1_template_files/figure-html/cc_interval_plot-1.png) 


### Impute Missing Values

The original dataset had 17568 rows. 
The data set after removing the incomplete cases had 15264 rows.

This is a difference of 2304 records

The NA records in the original data set have been defaulted to use the mean for the particular 5 minute interval and saved as the data frame activityImputed. The package sqldf was used to join to the interval summary dataframe.




```r
activityImputed1 <- sqldf( 
        "select a.*, i.mean_intervalSteps, i.sum_intervalSteps    
        from activity a join stepsByInterval i on a.interval = i.interval")

activityImputed2 <- sqldf(c("update activityImputed1 set steps = mean_intervalSteps where steps  is null", "select * from main.activityImputed1"))
```


#### Summarize the Imputed File


```r
imputedStepsByDate <- 
        activityImputed2 %>% 
        group_by(modDate) %>% 
        summarise(n_date = n(), sum_dateSteps = sum(steps) , mean_dateSteps = mean(steps))
```


Make a histogram of the steps by day including the imputed values


```r
hist(imputedStepsByDate$sum_dateSteps,
     xlab = "Steps",
     breaks = 10,
     main = "Histogram of Daily Steps \n with Imputed Values"
     )
```

![](PA1_template_files/figure-html/imputedStepsByDate_histogram-1.png) 




```r
imputedDatasetMean <- mean(imputedStepsByDate$sum_dateSteps)
imputedDatasetMedian <- median(imputedStepsByDate$sum_dateSteps)

mean_diff <- datasetMean - imputedDatasetMean
median_diff <- datasetMedian - imputedDatasetMedian
```

The mean of the total number of steps   taken per day is 10749.77 after imputation.

The median of the total number of steps  taken per day is 10641 after imputation.

The mean changed by 16.41819

The median changed by 124


### Are there differences in activity patterns between weekdays and weekends?

Yes! 

Use dplyr to summarize the data by the new variable dayType and calculate the means.


```r
imputedStepsByDayType <- 
        activityImputed2 %>% 
        group_by(interval,dayType) %>% 
        summarise(n_date = n(), sum_dayTypeSteps = sum(steps) , mean_dayTypeSteps = mean(steps))
```




### Panel Plot of Avg Steps Taken by Day Type



```r
g <- ggplot(imputedStepsByDayType, aes(x = interval, y = mean_dayTypeSteps, color = dayType))
g <- g + geom_line()
g <- g + facet_grid(. ~ dayType)
g <- g + ggtitle( "Average Steps by Day Type")
g <- g + xlab ("Interval") + ylab("Average Steps") 
g <- g + theme(legend.position = "bottom")
g <- g +  geom_line(stat = "hline", yintercept = "max", color = "black")
g <- g +  geom_line(stat = "hline", yintercept = "mean", color = "black", linetype = 2 )
g 
```

![](PA1_template_files/figure-html/imputed_dayType_plot-1.png) 

The solid black line indicates the maximum value. The dashed black line indicates the average.  

How are the weedays different from the weekend?
The weekday steps have a much higher spike. The weekends have a slighly higher average. There are multiple spikes for each day type, but there is one weekday spike that is MUCH higher than the other weekday spikes.
