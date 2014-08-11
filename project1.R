data <- read.csv("./data/activity.csv",header = TRUE)
data$date <- as.Date(data$date,format = "%Y-%m-%d")
#with(data,tapply(steps,date,mean,na.rm = FALSE))
library(ggplot2)

step_sums<-with(data,tapply(steps,date,sum,na.rm = FALSE))
barplot(step_sums,ylab="Number of Steps Recorded",xlab="Day (n=61 days)",names.arg = 1:61,main="Number of Steps Recorded on Each Day")
mean_step_sums <- mean(step_sums,na.rm=TRUE)
median_step_sums <- median(step_sums,na.rm=TRUE)

interval_avg <- with(data,tapply(steps,interval,mean,na.rm = TRUE))
plot(unique(data$interval),interval_avg,type="l",xlab="Interval (5 min)",ylab="Average Steps Recorded",main="Average Daily Activity Pattern")
mean_activity <- cbind(unique(data$interval),interval_avg)
colnames(mean_activity) <- c("Interval","Avg Steps")
mean_activity[mean_activity[,2]==max(mean_activity[,2]),]

## Calculate no. of NAs
sum(is.na(data$steps))
data2 <- data
for(i in 1:length(data$steps)){
    if(is.na(data[i,1])){
        data2[i,1] <- mean_activity[mean_activity[,1]==data[i,3],2]
    }
}
step_sums2<-with(data2,tapply(steps,date,sum,na.rm = FALSE))
barplot(step_sums2)
mean(step_sums2,na.rm=TRUE)
median(step_sums2,na.rm=TRUE)

wkdy <- weekdays(data[,2])
wkdy[!(wkdy %in% c("Saturday","Sunday"))] <- "weekday"
wkdy[(wkdy %in% c("Saturday","Sunday"))] <- "weekend"
data<-cbind(data,wkdy)
data2<-cbind(data2,wkdy)

interval_avg_wkdy <- with(data,tapply(steps,list(interval,wkdy),mean,na.rm = TRUE))
interval_avg_wkdy <- as.data.frame(cbind(as.numeric(row.names(interval_avg_wkdy)),interval_avg_wkdy))
colnames(interval_avg_wkdy) <- c("interval","weekday","weekend")
library(reshape)
interval_avg_wkdy <-melt(interval_avg_wkdy,id=c("interval"))
colnames(interval_avg_wkdy) <- c("interval","day_type","steps")

library(lattice)
xyplot(steps~interval|day_type,data=interval_avg_wkdy,type="l",xlab="Interval",ylab="Number of Steps",layout=c(1,2))
