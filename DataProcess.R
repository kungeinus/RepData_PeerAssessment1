
## Read in Data

activityData<-read.csv('activity.csv', header = TRUE,as.is = FALSE)

## Calculate the total number of steps taken each day and store the data in a new dataframe

activityEachDay<-aggregate(steps~date,activityData,sum,na.rm=TRUE)
activityEachDay$date<-as.POSIXlt(activityEachDay$date,"%Y-%m-%d")
names(activityEachDay)[2]<-"sum_steps"

##Make a histogram to show steps taken for each day

hist(activityEachDay$sum_steps,
     col = "red",
     main = "Histogram of the Total Number of Steps Each Day",
     xlab = "Total Number of Steps Each Day",
     breaks = 10)

## Calculate mean and median number of steps taken each day

mean(activityEachDay$sum_steps)
median(activityEachDay$sum_steps)

##Calculate the mean number of steps taken each 5-min interval and store the data in a new dataframe

activityEachInterval<-aggregate(steps~interval,activityData,mean,na.rm=TRUE)
names(activityEachInterval)[2]<-"mean_steps"

## Plot the dayly activity pattern

plot(
        x = activityEachInterval$interval,
        y = activityEachInterval$mean_steps,
        type = "l",
        main = "The Average Number of Steps for Each 5-Min Interval",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps"
)

## Find the time interval contains the maximum number of steps

activityEachInterval[activityEachInterval$mean_steps==max(activityEachInterval$mean_steps,na.rm = TRUE),]

## Calculate the number of missing values in the raw data

nrow(activityData[is.na(activityData$steps),])

## Use the mean value for each 5-min interval across all the days to replace the missing values

activityNoNA<-merge(activityData,activityEachInterval,all.y=FALSE, all.x = TRUE, by = "interval",sort = FALSE)
activityNoNA[is.na(activityNoNA$steps),"steps"]<-as.integer(round(activityNoNA$mean_steps[is.na(activityNoNA$steps)]))
activityNoNA<-activityNoNA[,c("date","interval","steps")]
activityNoNA<-activityNoNA[order(activityNoNA$date,activityNoNA$interval),]
rownames(activityNoNA)<-c()

#### Calculate the total number of steps taken each day with No NA data frame and store the data in a new dataframe

activityNNEachDay<-aggregate(steps~date,activityNoNA,sum)
activityNNEachDay$date<-as.POSIXlt(activityNNEachDay$date,"%Y-%m-%d")
names(activityNNEachDay)[2]<-"sum_steps"

##Make a histogram to show steps taken for each day

hist(activityNNEachDay$sum_steps,
     col = "red",
     main = "Histogram of the Total Number of Steps Each Day (NA romoved)",
     xlab = "Total Number of Steps Each Day",
     breaks = 10)

## Calculate mean and median number of steps taken each day with No NA data frame

mean(activityNNEachDay$sum_steps)
median(activityNNEachDay$sum_steps)

##Add a new factor variable to indentify whether a day is during weekdays or weekends.

activityNoNA$date<-as.POSIXlt(activityNoNA$date,"%Y-%m-%d")
activityNoNA$daytype<-"weekday"
activityNoNA[weekdays(activityNoNA$date, abbreviate = TRUE) %in% c("Sat","Sun"),"daytype"]<-"weekend"
activityNoNA$daytype<-as.factor(activityNoNA$daytype)

##Calculate the mean number of steps taken each 5-min interval for weekdays and weekends saperately
## and store the data in a new dataframe

activityNNEachInterval<-aggregate(steps~interval + daytype, data = activityNoNA, mean)
names(activityNNEachInterval)[3] <- "mean_steps"

## Plot the dayly activity pattern for weekdays and weekends

library(lattice)
xyplot(
        mean_steps ~ interval | daytype,
        activityNNEachInterval,
        type = "l",
        layout = c(1,2),
        main = "The Average Number of Steps for Each 5-Min Interval during Weekday Days or Weekend Days",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps"
)

