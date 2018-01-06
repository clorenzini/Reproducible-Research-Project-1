setwd("C:/Users/Christopher Lorenzin/Desktop/Data Science Cert/Reproducible Research")

library(rmarkdown)
data<-read.csv("activity.csv")

head(data)

### Daily Mean and Median 

data$Date <- as.Date(data$date)

## Finding the total number of steps taken per day
a<- aggregate(data$steps, by=list(data$Date), FUN = sum)

## Creating a histogram of the total number of steps taken per day
hist(a$x, main="Histogram of Total Steps Taken per Day",ylab="Frequency",xlab=" ")

pdf("Hist1.pdf") 
hist(a$x, main="Histogram of Total Steps Taken per Day",ylab="Frequency",xlab=" ")
dev.off()

## Finding the mean number of steps taken per day
a1<- aggregate(data$steps, by=list(data$Date), FUN = mean, na.rm=TRUE, na.action=NULL)

## Finding the median number of steps taken per day
b1<- aggregate(data$steps, by=list(data$Date), FUN = median, na.rm=TRUE)

### Mean Activity

## Finding the mean number of steps taken per time stamp across days
c<- aggregate(data$steps, by=list(data$interval), FUN = mean, na.rm=TRUE, na.action=NULL)

## Finding the median number of steps taken per time stamp across days
d<- aggregate(data$steps, by=list(data$interval), FUN = median, na.rm=TRUE)

max(c$x)

#835 is the max


## Creating a time series plot to identify the highest volume of steps

pdf("Line1.pdf")
plot(c$Group.1,c$x,type="l",main="Steps Taken on Average v Time of Day",ylab="Steps",xlab="Time of Day")
dev.off()


### Replacing NAs

y<- which(is.na(data$steps)==TRUE)
length(y)
#2304 rows with NAs

## Replacing NA values with the mean at the specific time
data1<-data

for(i in 1:length(y)){
  #i=1
  aa<-y[i]
  
  bb<- data1$interval[aa]
  
  cc<-c$x[which(c$Group.1==bb)]
  
  data1$steps[aa]<-cc
  
  
}
  
e<- aggregate(data$steps, by=list(data$Date), FUN = sum)  

e$x

pdf("Hist2.pdf")
hist(e$x,main="Histogram of Total Steps Taken per Day",ylab="Frequency",xlab=" ")
dev.off()  
##No changes in the histogram when replacing the NA values with the statistic used in construction


### Day of Week trends

## Creating a new variable to determine Weekday v Weekend
data$day<-0
data$ind<-"Weekday"
data$day<-weekdays(data$Date)

data$ind[which(data$day=="Saturday")]<-"Weekend"


set1<-data[which(data$ind=="Weekday"),]
set2<-data[which(data$ind=="Weekend"),]

##Finding the mean number of steps taken per time stamp across weekdays and weekends

aaa<- aggregate(set1$steps, by=list(set1$interval), FUN = mean, na.rm=TRUE, na.action=NULL)
bbb<- aggregate(set2$steps, by=list(set2$interval), FUN = mean, na.rm=TRUE, na.action=NULL)


## Plotting them one on top of the other
pdf("Compare Line.pdf")
par(mfrow=c(2,1))

plot(aaa$Group.1,aaa$x,type="l",main="Steps Taken Over Time on Weekdays",ylab="Steps",xlab="Time")
plot(bbb$Group.1,bbb$x,type="l",main="Steps Taken Over Time on Weekends",ylab="Steps",xlab="Time")
dev.off()
# Activity starts later in the morning during the weekend, but is far greater during the afternoon


