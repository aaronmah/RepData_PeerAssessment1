#Q1
unzip("activity.zip")
data <- read.csv("activity.csv")

#Q2
sumSteps <- tapply(data$steps, data$date, sum)
sumStepsDf <- data.frame(dates=names(sumSteps), totals=sumSteps)
sumStepsDf$dates <- as.Date(sumStepsDf$dates)
plot(sumStepsDf$dates, sumStepsDf$totals, type="h", xlab="Dates", ylab="Total Steps")

#Q3
median(sumStepsDf$totals, na.rm=TRUE)
mean(sumStepsDf$totals, na.rm=TRUE)

avgSteps <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
avgStepsDf <- data.frame(interval=names(avgSteps), averages=avgSteps)

avgStepsDf$interval <- as.numeric(as.character(avgStepsDf$interval))
avgStepsDf <- avgStepsDf[order(avgStepsDf$interval),]

#Q4
plot(avgStepsDf$interval, avgStepsDf$averages, type="l", xlab="Interval", ylab="Average Steps")

#Q5
max(avgStepsDf)
avgSteps[grepl(206.1698113, avgStepsDf$averages)]

#Q6
sum(is.na(data))

#Q7
imputedData <- merge(data, avgStepsDf, by.x = "interval", by.y="interval")
imputedData$date <- as.Date(imputedData$date)
imputedData$steps[is.na(imputedData$steps)] <- as.numeric(imputedData$averages)

imputedTotal <- tapply(imputedData$steps, imputedData$date, sum)
imputedDF <- data.frame(dates=names(imputedTotal), totals=imputedTotal)
imputedDF$dates <- as.Date(imputedDF$dates)

#Q8
plot(imputedDF$dates, imputedDF$totals, type="h", xlab="Dates", ylab="Totals")

#Q9
mean(imputedDF$totals)
median(imputedDF$totals)

#Q10
imputedData$weekday <- ifelse(weekdays(imputedData$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#Q11
library(lattice)
imputedWeekend <- subset(imputedData, imputedData$weekday=="Weekend")
imputedWeekday <- subset(imputedData, imputedData$weekday=="Weekday")
imputedWeekendData <- tapply(imputedWeekend$steps, imputedWeekend$interval, median)
imputedWeekdayData <- tapply(imputedWeekday$steps, imputedWeekday$interval, median)

weekendDF <- data.frame(interval=names(imputedWeekendData), average=imputedWeekendData)
weekdayDF <- data.frame(interval=names(imputedWeekdayData), average=imputedWeekdayData)

weekendDF$interval <- as.numeric(as.character(weekendDF$interval))
weekdayDF$interval <- as.numeric(as.character(weekdayDF$interval))

weekendDF <- weekendDF[order(weekendDF$interval),]
weekdayDF <- weekdayDF[order(weekdayDF$interval),]

par(mfrow=c(2,1), mar=c(3,4,2,1))
plot(weekendDF$interval, weekendDF$average, type="l", xlab="",ylab="Average Steps", main="Weekend")
mtext("Interval", side=1, padj=3)
plot(weekdayDF$interval, weekdayDF$average, type="l", xlab="",ylab="Average Steps", main="Weekday")
mtext("Interval", side=1, padj=3)

