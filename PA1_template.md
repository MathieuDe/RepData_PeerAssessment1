setwd("E:/Coursera/Reproductible research/Peer assesment 1")


##Loading and preprocessing the data
##Show any code that is needed to
##Load the data (i.e. read.csv())
activity<-read.csv(activity.csv)
##Process/transform the data (if necessary) into a format suitable for your analysis
activity$date<-as.factor(activity$date)


##What is mean total number of steps taken per day?
##For this part of the assignment, you can ignore the missing values in the dataset.
##Make a histogram of the total number of steps taken each day
sum_steps<-tapply(activity$steps,activity$date,sum)
 barplot(sum_steps)
##Calculate and report the mean and median total number of steps taken per day
mean_steps_by_day<-tapply(activity$steps,activity$date,mean, na.rm=TRUE)
median_steps_by_day<-tapply(activity$steps,activity$date,median,na.rm=TRUE)


##What is the average daily activity pattern?

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
activity$interval<-as.factor(activity$interval)
mean_steps_by_interval<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)

plot(mean_steps_by_interval,type="l",xaxt = "n", xlab='Interval')
axis(1, at=1:length(mean_steps_by_interval), labels=rownames(mean_steps_by_interval))

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps<-max(mean_steps_by_interval)


##Imputing missing values
##Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nb_na<-sum(is.na(activity$steps)
##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity$steps[is.na(activity$steps)] =median(activity$steps, na.rm=TRUE)
##Make a histogram of the total number of steps taken each day 
sum_steps2<-tapply(activity$steps,activity$date,sum)
 barplot(sum_steps2)
##and Calculate and report the mean and median total number of steps taken per day. 

mean_steps_by_day2<-tapply(activity$steps,activity$date,mean, na.rm=TRUE)
median_steps_by_day2<-tapply(activity$steps,activity$date,median,na.rm=TRUE)

##{{Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

##Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
activity$date<-as.Date(activity$date)
activity$day<-NA
activity$day<-weekdays(activity$date)

##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity$weekday<-NA

for (i in 1:length(activity$day)){
      if(activity$day[i]=="samedi" | activity$day[i]=="dimanche") {
            activity$weekday[i] <- "weekend"
      } else {
            activity$weekday[i] <- "weekday"
      }
}
activity$weekday<-as.factor(activity$weekday)
##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
mean_steps_by_interval_weekend<-tapply(activity$steps[activity$weekday=="weekend"],activity$interval[activity$weekday=="weekend"],mean)
mean_steps_by_interval_weekday<-tapply(activity$steps[activity$weekday=="weekday"],activity$interval[activity$weekday=="weekday"],mean)

par(mfrow=c(2,1))
plot(mean_steps_by_interval_weekend,type="l",xaxt = "n", xlab='Interval')
plot(mean_steps_by_interval_weekday,type="l",xaxt = "n", xlab='Interval')
axis(1, at=1:length(mean_steps_by_interval), labels=rownames(mean_steps_by_interval))
