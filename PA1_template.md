---
title: "Analyzing FitBit Data"
---
##Summary
The purpose of the project was to answer a series of questions using data collected from a [FitBit](http://en.wikipedia.org/wiki/Fitbit).


## Data
The data for this assignment was downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

Download, unzip and load data into data frame `data`. 

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        dummyfile <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",dummyfile)
        unzip(dummyfile)
        unlink(dummyfile)
}

data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
Sum steps per day, create Histogram, and calculate mean and median.

```r
stepsPerDay <- aggregate(steps ~ date, data, sum)
hist(stepsPerDay$steps, main = paste("Total Steps Each Day"), col=terrain.colors(6), xlab="Number of Steps")
```

<img src="Reproducible_research_assignment_1_files/figure-html/unnamed-chunk-2-1.png" title="" alt="" width="672" />

```r
rmean <- mean(stepsPerDay$steps)
rmedian <- median(stepsPerDay$steps)
```

The `mean` is 1.0766189\times 10^{4} and the `median` is 10765.

## What is the average daily activity pattern?

* Calculate average steps for each interval for all days. 
* Plot the Average Number Steps per Day by Interval. 
* Find interval with most average steps. 

```r
stepsByInterval <- aggregate(steps ~ interval, data, mean)

plot(stepsByInterval$interval,stepsByInterval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

<img src="Reproducible_research_assignment_1_files/figure-html/unnamed-chunk-3-1.png" title="" alt="" width="672" />

```r
maxInterval <- stepsByInterval[which.max(stepsByInterval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

## Impute missing values. Compare imputed to non-imputed data.
Missing data needed to be imputed. Only a simple imputation approach was required for this assignment. 
Missing values were imputed by inserting the average for each interval. Thus, if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA. 

```r
incomplete <- sum(!complete.cases(data))
imputedData <- transform(data, steps = ifelse(is.na(data$steps), stepsByInterval$steps[match(data$interval, stepsByInterval$interval)], data$steps))
```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data. 

```r
imputedData[as.character(imputedData$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram. 

```r
library(RColorBrewer)
stepsByDayImp <- aggregate(steps ~ date, imputedData, sum)
hist(stepsByDayImp$steps, main = paste("Total Steps Each Day"), col=brewer.pal(5, "PuBu")[4], xlab="Number of Steps")

#Create Histogram to show difference. 
hist(stepsPerDay$steps, main = paste("Total Steps Each Day"), col=brewer.pal(5, "Oranges")[4], xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c(brewer.pal(5, "PuBu")[4], col=brewer.pal(5, "Oranges")[4]), lwd=10)
```

<img src="Reproducible_research_assignment_1_files/figure-html/unnamed-chunk-6-1.png" title="" alt="" width="672" />

Calculate new mean and median for imputed data. 

```r
rmean.i <- mean(stepsByDayImp$steps)
rmedian.i <- median(stepsByDayImp$steps)
```

Calculate difference between imputed and non-imputed data.

```r
meanDiff <- rmean.i - rmean
medDiff <- rmedian.i - rmedian
```

Calculate total difference.

```r
totalDiff <- sum(stepsByDayImp$steps) - sum(stepsPerDay$steps)
```
* The imputed data mean is 1.0589694\times 10^{4}
* The imputed data median is 1.0766189\times 10^{4}
* The difference between the non-imputed mean and imputed mean is -176.4948964
* The difference between the non-imputed mean and imputed mean is 1.1886792
* The difference between total number of steps between imputed and non-imputed data is 7.5363321\times 10^{4}. Thus, there were 7.5363321\times 10^{4} more steps in the imputed data.


## Are there differences in activity patterns between weekdays and weekends?
Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.  

```r
library(ggplot2)
imputedData$dateType <-  ifelse(as.POSIXlt(imputedData$date)$wday %in% c(0,6), 'weekend', 'weekday')

averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=imputedData, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

<img src="Reproducible_research_assignment_1_files/figure-html/unnamed-chunk-10-1.png" title="" alt="" width="672" />
