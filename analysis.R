############################################################
## Reproducible Research : Week 2 Course Project
############################################################
if("ddplyr" %in% rownames(installed.packages()) == FALSE) { install.packages("ddplyr")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) { install.packages("ggplot2")}

library(ddplyr)
library(ggplot2)

setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )

thedata <- read.csv(unz("activity.zip","activity.csv"),na.strings = c("NA"),colClasses = c("numeric","myDate","factor"))


## Create the figures directory where we'll store all images in the figures directory 
if(!dir.exists("figures")){
	dir.create("figures")
}

## Get the total steps by day
steps_by_day <- thedata %>% group_by(date) %>% summarise(total_steps = sum(steps),avg_steps = mean(steps,na.rm=TRUE))
steps_by_day$source <- "original"

g <- ggplot(steps_by_day,aes(x=date,y=total_steps))
g <- g + geom_bar(stat="identity")
g <- g + labs(y="Total Steps",title="Total steps by Day")
plot(g)


### Average by time interval
avg_by_interval <- thedata %>% group_by(interval) %>% summarise(avg_steps = mean(steps,na.rm=TRUE))
avg_by_interval$interval <- as.numeric(as.character(avg_by_interval$interval))
avg_by_interval <- avg_by_interval %>% arrange(interval)

g <- ggplot(avg_by_interval,aes(interval,avg_steps)) + geom_line()
plot(g)

## Fill missing values by making it the average steps of that time interval
thedata_no_na <-thedata 
thedata_no_na$steps <- with(thedata_no_na,ifelse(is.na(steps),filter(avg_by_interval,interval == interval)$avg_steps,steps))

steps_by_day_2 <- thedata_no_na %>% group_by(date) %>% summarise(total_steps = sum(steps),avg_steps = mean(steps))
steps_by_day_2$source <- "filled_na"

## Let's combine the two steps by day into a single data set
merged <- rbind(steps_by_day,steps_by_day_2)

g <- ggplot(merged,aes(x=date,y=total_steps))
g <- g + facet_grid(. ~ source)
g <- g + geom_bar(stat="identity")
g <- g + labs(y="Total Steps",title="Total steps by Day")


### Let's now total steps across weekends vs. weekdays


thedata_no_na$day_of_week <- weekdays(thedata_no_na$date,abbreviate=TRUE)

thedata_no_na$wday_or_weekend <- ifelse(thedata_no_na$day_of_week == "Sun" | thedata_no_na$day_of_week == "Sat","weekend","weekday")

steps_by_day_type <- thedata_no_na %>% group_by(interval,wday_or_weekend) %>% summarise(total_steps = sum(steps),avg_steps = mean(steps))
steps_by_day_type$interval <- as.numeric(as.character(steps_by_day_type$interval))

g <- ggplot(steps_by_day_type,aes(interval,total_steps))
g <- g + geom_line()
g <- g + facet_grid(wday_or_weekend ~ .)
g <- g + labs(y="Total Steps",title="Total steps: Weekday vs. Weekend")+ theme(plot.title = element_text(hjust = 0.5))
plot(g)
