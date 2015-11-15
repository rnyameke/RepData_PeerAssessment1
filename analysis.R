library(dplyr)
library(ggplot2)
data$date <- as.POSIXct(data$date)
data_tbl <- tbl_df(data)
data_tbl_date <- group_by(data_tbl, date)
summarize(data_tbl, mean(steps, na.rm = T))

ggplot(data = data, aes(x = steps)) + geom_histogram()

#removing the nas
data_without_na <- na.omit(data)

#total number of steps per day
data_tbl_without_na <- tbl_df(data_without_na)
data_tbl_without_na <- group_by(data_tbl_without_na, date)
steps_per_day <- summarize(data_tbl_without_na, sum(steps))
names(steps_per_day)[2] <- "steps"

#histogram
ggplot(data = steps_per_day, aes(x = steps)) + geom_histogram()

#mean steps per day
mean(steps_per_day$steps)

#median steps per day
median(steps_per_day$steps)

#Question 2
#group table by interval
data_group_int <- group_by(data_tbl, interval)
data_mean_int <- summarize(data_group_int, mean(steps, na.rm=T))

#clean new table
names(data_mean_int)[2] <- "steps"

#graph, time series plot
ggplot(data = data_mean_int, aes(x = interval, y = steps)) + geom_line(aes(group = 1))

#5 minute interval with max number of steps
data_mean_int[which(data_mean_int$steps == max(data_mean_int$steps)),1] #104



