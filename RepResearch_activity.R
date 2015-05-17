##Loading and Processing Data##
library(sqldf)
library(ggplot2)

setwd("C:/Alex/Coursera/Reproducible Data Research")
activity<- read.csv("activity.csv", header= TRUE)
str(activity)

##Mean Total Number Steps Per Day##
#Total Steps Per Day#
stepsperday <- aggregate(steps ~ date, data = activity, FUN = sum)

#Histogram#
qplot(x = steps, data = stepsperday, binwidth = 1000, xlab = "Total Steps Per Day")

#Mean and Median#
mean(stepsperday$steps)
median(stepsperday$steps)

#Average Steps Per Interval#
int_avg <- aggregate(steps ~ interval, data = activity, FUN = mean)

#Adding formatted time#
int_avg$time <- ifelse((int_avg$interval < 10), paste("00",":","0",int_avg$interval,sep = ""), 
                       ifelse((int_avg$interval >= 10) & (int_avg$interval <100),paste("00",":",int_avg$interval, sep = ""),
                              ifelse((int_avg$interval >= 100) & (int_avg$interval <1000),paste("0",substr(int_avg$interval,1,1),":",substr(int_avg$interval,2,3), sep = ""),
                                     ifelse((int_avg$interval >= 1000), paste(substr(int_avg$interval,1,2),":",substr(int_avg$interval,3,4),sep = ""),"NA"))))
##Time Series Plot##
#Time Plot#
qplot(x = interval, y = steps, data = int_avg, geom = "line", xlab = "Average Steps Per Day By Time")

#Highest Steps Interval - Max#
int_avg[int_avg$steps == max(int_avg$steps),]

##Imputation##
#Missing number of rows#
nrow(activity[activity$steps == "NA",])

#Copying, Merging, Replacing NA Values with Mean Calculated Above#
activity_imp <- activity
activity_imp <- sqldf("SELECT a.*, b.steps as imp_steps FROM activity_imp a LEFT JOIN int_avg b USING(interval)")
activity_imp$steps <- ifelse(is.na(activity_imp$steps),activity_imp$imp_steps, activity_imp$steps)

#Histogram of Steps Per Day with Imputed Data#
stepsperday_imp <- aggregate(steps ~ date, data = activity_imp, FUN = sum)

qplot(x = steps, data = stepsperday_imp, binwidth = 1000, xlab = "Total Steps Per Day")

#Mean and Median of Imputed Data#
mean(stepsperday_imp$steps)
median(stepsperday_imp$steps)

#Weekdays and Weekends#
activity_imp$day <- weekdays(as.Date(activity_imp$date))
activity_imp$weekday <- ifelse((activity_imp$day == "Saturday" | activity_imp$day == "Sunday"),"Weekend","Weekday")

#Average Steps Per Interval - Weekday/Weekend#
activity_imp$steps <- round(activity_imp$steps, digits = 2)
week_avg <- aggregate(steps~interval + weekday, data = activity_imp, FUN = mean)

#Plot By Weekday/Weekend#
ggplot(week_avg, aes(interval, steps)) + geom_line() + facet_grid(weekday ~ .) + xlab("5-minute interval") + ylab("Number of Steps")


