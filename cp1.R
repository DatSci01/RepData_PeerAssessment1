unzip("activity.zip")
activity<-read.csv("activity.csv")

## aggregate total steps by date
tots<-aggregate(steps~date,data=activity,sum)

## produce histogram
hist(tots$steps)
#aves<-aggregate(steps~date,data=activity,mean)
#print(aves)

## calculate mean and median of total steps (tots)
meanSteps<-mean(tots$steps)
medianSteps<-median(tots$steps)
print(paste("Mean total steps per day: ",meanSteps))
print(paste("Median total steps per day: ",medianSteps))
unlink("activity.csv")