setwd("C:/Users/IÑIGO/Documents/coursera/")


#Folder where I am going to work for project
folder <- "./Reproducible research project 1"


# I create folder 
 if(!file.exists(folder)){
+   dir.create(folder)}




#I download data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile="./Reproducible research project 1/data.zip",method="auto")
unzip(zipfile="./Reproducible research project 1/data.zip",exdir="./Reproducible research project 1")

#I set the WD where I am going to work
setwd("C:/Users/IÑIGO/Documents/coursera/Reproducible research project 1")

#Load the data into a variable
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)



###############
### Process/transform the data (if necessary) into a format suitable for analysis
#################

activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")



activity_raw <- data.frame(date=activity_raw$date, weekday=tolower(weekdays(activity_raw$date)),steps=activity_raw$steps,interval=activity_raw$interval)

activity_raw <- cbind(activity_raw, daytype=ifelse(activity_raw$weekday == "sabado" | activity_raw$weekday == "domingo", "weekend", "weekday"))



activity <- data.frame(date=activity_raw$date, weekday=activity_raw$weekday, daytype=activity_raw$daytype, interval=activity_raw$interval, steps=activity_raw$steps)


################
#What is the mean total number of steps taken per day?
################

######1. Make a histogram of the total number of steps taken each day


sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

names(sum_data) <- c("date", "total")



#2. if you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


hist(sum_data$total,  breaks=seq(from=0, to=25000, by=2500),col="blue", xlab="Total number of steps", ylim=c(0, 20), main="Histogram of the total number of steps taken each day\n(NA removed)")




####3. Calculate and report the mean and median total number of steps taken per day

mean(sum_data$total)
median(sum_data$total)



############
#What is the average daily activity pattern?
#################

###########  1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

mean_data <- aggregate(activity$steps,  by=list(activity$interval), FUN=mean, na.rm=TRUE)

names(mean_data) <- c("interval", "mean")


plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")




########## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_pos <- which(mean_data$mean == max(mean_data$mean))


max_interval <- mean_data[max_pos, 1]


print(max_interval)







####################
#Imputing missing values
#####################


##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

NA_count <- sum(is.na(activity$steps))



###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

na_pos <- which(is.na(activity$steps))

mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))



##3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity[na_pos, "steps"] <- mean_vec




###4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)

names(sum_data) <- c("date", "total")


hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")




#################
#Are there differences in activity patterns between weekdays and weekends?
################

###1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

head(activity)


#####2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


library(lattice)


mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)


names(mean_data) <- c("daytype", "weekday", "interval", "mean")




xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
