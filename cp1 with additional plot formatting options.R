## Unzip and read data
unzip("activity.zip")
activity<-read.csv("activity.csv",stringsAsFactors=T)

## aggregate total steps by date
aggTotSteps<-aggregate(steps~date,data=activity,sum)

## produce histogram
hist(aggTotSteps$steps,main="Total Steps Taken Each Day",xlab="Total Steps")

## calculate mean and median of total steps (aggTotSteps)
meanSteps<-mean(aggTotSteps$steps)
medianSteps<-median(aggTotSteps$steps)

print(paste("Mean total steps per day: ",round(meanSteps,4)))
print(paste("Median total steps per day: ",medianSteps))


## aggregate mean of steps for all dates for each interval
aggAvgSteps<-aggregate(steps~interval,data=activity,mean)


## plot aggregate step data
timeTicks<-seq(from=0,to=2000, by=500)
plot(aggAvgSteps$interval,aggAvgSteps$steps,type="l",
     main="Steps Per 5-minute Interval, Averaged Across All Days",
     xlab="Time Interval (5-minute intervals, 24-hour clock",ylab="Average Steps",
     xaxt="n")
#     axis(1,at=timeTicks,labels=formatC(timeTicks,width=4,format="d",flag="0"))
axis(1,at=timeTicks,labels=c("00:00","05:00","10:00","15:00","20:00"))

maxInterval<-aggAvgSteps[aggAvgSteps$steps==max(aggAvgSteps$steps),]
print(paste("Averaged across all days, interval ",maxInterval$interval,
            " contains the max number of steps (",
            round(maxInterval$steps,0)," steps)"))

valNA<-nrow(activity[!complete.cases(activity),])
print(paste("There are ",valNA," observations with missing values"))

activityInt<-activity
activityDate<-activity

## correct the activity data by replacing NA values with the mean for the interval
activityInt[!complete.cases(activityInt),1]<-
     aggAvgSteps[match(activityInt[!complete.cases(activityInt),3],
                       aggAvgSteps$interval),2]
     
aggTotCorrectedIntSteps<-aggregate(steps~date,data=activityInt,sum)
hist(aggTotCorrectedIntSteps$steps,
     main="Total Steps Each Day (NAs replaced with mean interval value)",xlab="Total Steps")

## calculate mean and median of total steps corrected by 
## interval mean (aggTotCorrectedIntSteps)
meanCorrectedIntSteps<-mean(aggTotCorrectedIntSteps$steps)
medianCorrectedIntSteps<-median(aggTotCorrectedIntSteps$steps)

print(paste("Mean total steps per day (corrected with mean interval value): ",
            round(meanCorrectedIntSteps,4)))
print(paste("Median total steps per day (corrected with mean interval value): ",
            round(medianCorrectedIntSteps,4)))

## aggregate mean steps by date for all intervals
aggMeanSteps<-aggregate(steps~date,data=activity,mean)


## correct the activity data by replacing NA values with the mean for the date
activityDate[!complete.cases(activityDate),1]<-
     aggMeanSteps[match(activityDate[!complete.cases(activityDate),2],
                        aggMeanSteps$interval),1]

aggTotCorrectedDateSteps<-aggregate(steps~date,data=activityDate,sum)
hist(aggTotCorrectedDateSteps$steps,
     main="Total Steps Each Day (NAs replaced with mean date values)",
     xlab="Total Steps")

## calculate mean and median of total steps corrected by 
## date mean (aggTotCorrectedDateSteps)
meanCorrectedDateSteps<-mean(aggTotCorrectedDateSteps$steps)
medianCorrectedDateSteps<-median(aggTotCorrectedDateSteps$steps)

print(paste("Mean total steps per day (corrected with mean date value): ",
            round(meanCorrectedDateSteps,4)))
print(paste("Median total steps per day (corrected with mean date value): ",
            round(medianCorrectedDateSteps,4)))

## change date variable type from "chr" to "date"
activity$date<-as.Date(activity$date,"%Y-%m-%d")
## Add column with factors of weekday or weekend
activity$wday<-
     as.factor(ifelse(weekdays(activity$date) %in% 
                           c("Saturday","Sunday"), "Weekend", "Weekday"))

## aggregate mean of steps for all weekdays for each interval
aggAvgWdaySteps<-aggregate(steps~interval,
                           data=activity[activity$wday=="Weekday",],mean)
aggAvgWdaySteps$Dtype<-"Weekday"

aggAvgWendSteps<-aggregate(steps~interval,
                           data=activity[activity$wday=="Weekend",],mean)
aggAvgWendSteps$Dtype<-"Weekend"

aggAvgAllDaysSteps<-rbind(aggAvgWdaySteps,aggAvgWendSteps)


library(ggplot2)

## Create separate plots for weekday and weekend step totals
g<-ggplot(aggAvgAllDaysSteps,aes(x=interval,y=steps))
print(g
      +geom_line(col="blue") 
      + facet_wrap(~Dtype,ncol=1)
      +labs(y="Number of steps",x="Time Interval (5-minute intervals, 24-hour clock)")
      +theme(strip.background = element_rect(color="black",fill="#FFCC99"),
             strip.text.x = element_text(face="bold"))
      +theme(panel.background = element_rect(fill = 'white'))
#      +scale_x_continuous(breaks=timeTicks,
#                          labels=formatC(timeTicks,width=4,format="d",flag="0"))
      +scale_x_continuous(limits=c(0,2355),breaks=c(0,500,1000,1500,2000),
                          labels=c("00:00","05:00","10:00","15:00","20:00"))
      )

## Remove data file
unlink("activity.csv")