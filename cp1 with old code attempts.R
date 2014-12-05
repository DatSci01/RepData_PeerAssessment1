unzip("activity.zip")
activity<-read.csv("activity.csv",stringsAsFactors=T)

## aggregate total steps by date
aggTotSteps<-aggregate(steps~date,data=activity,sum)

## produce histogram
#hist(aggTotSteps$steps)
#########aves<-aggregate(steps~date,data=activity,mean)
#########print(aves)

## calculate mean and median of total steps (aggTotSteps)
meanSteps<-mean(aggTotSteps$steps)
medianSteps<-median(aggTotSteps$steps)

print(paste("Mean total steps per day: ",round(meanSteps,2)))
print(paste("Median total steps per day: ",medianSteps))



########finterval<-factor(activity$interval)
aggAvgSteps<-aggregate(steps~interval,data=activity,mean)


#plot(aggAvgSteps$interval,aggAvgSteps$steps,type="l")
maxInterval<-aggAvgSteps[aggAvgSteps$steps==max(aggAvgSteps$steps),]
print(paste("Averaged across all days, interval ",maxInterval$interval,
            " contains the max number of steps (",
            round(maxInterval$steps,0)," steps)"))

valNA<-nrow(activity[!complete.cases(activity),])
print(paste("There are ",valNA," observations with missing values"))

#activityNA<-activity[!complete.cases(activity),]

activity[!complete.cases(activity),1]<-
     aggAvgSteps[match(activity[!complete.cases(activity),3],aggAvgSteps$interval),2]
#order.interval<-order(activityNA$interval)
#activityNAsort<-activityNA[order.interval,]

#activityCorrected<-activity
#for (i in 1:nrow(activityCorrected)) 
#     if (is.na(activityCorrected[[i,1]])) 
#         activityCorrected[[i,1]]<-aggAvgSteps[activityCorrected[[i,3]]==aggAvgSteps$interval,2]
     
####activityNA$steps<-aggAvgSteps[activityNA$interval==aggAvgSteps$interval,]$steps
#activityNA$steps<-aggAvgSteps[activityNA$interval,2]

#aggAvgSteps[activityNA[[1,3]] %in% aggAvgSteps$interval,]$steps
#activityNA$steps[match(aggAvgSteps[activityNA[[1,3]],aggAvgSteps$interval]



#activityCorrected<-rbind(activityNA,activity[complete.cases(activity),])
#aggTotCorrected<-aggregate(steps~interval,data=activityCorrected,sum)
#hist(aggTotCorrected$steps)

#activity$steps[!complete.cases(activity),]<-aves[aves$interval==activity$interval,2]
unlink("activity.csv")