#Load Libraries
library(data.table)
library(dplyr)
library(ggplot2)

#Download Data (Used in all Plots)
path <- getwd()
url <-  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

#Load data
activity.df <- fread(file.path(path, "activity.csv"))

# 1. Calculate the total number of steps taken per day
total_steps.df <- activity.df %>% group_by(date) %>% summarize(total_steps = sum(steps))

# Plot the information
ggplot(total_steps.df, aes(x = total_steps)) +
  geom_histogram(fill = "blue", bindwidth = 1000) + 
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")

# Calculate are report the mean and median of the total number of steps taken per day
summary_steps.df <- total_steps.df %>% summarize(mean_steps = mean(total_steps, na.rm = TRUE), median_steps = median(total_steps, na.rm = TRUE))
print(summary_steps.df)

# What is the average daily activity pattern?
#make a time series plot
summary_steps_interval.df <- activity.df %>% group_by(interval) %>% summarize(mean_steps = mean(steps, na.rm = TRUE))
print(summary_steps_interval.df)

ggplot(summary_steps_interval.df, aes(x = interval , y = mean_steps)) + 
  geom_line(color="blue", size=1) + 
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

# which 5-minute interval, on average across all the days in the dataset, contains the masimum number of steps?
max_interval <- summary_steps_interval.df %>% filter(mean_steps == max(summary_steps_interval.df$mean_steps)) %>% select(interval)


#inputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). 
nrow(activityDT[is.na(steps),])

# The presence of missing days may introduce bias into some calculations or summaries of the data.
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create tidy data (copy data)
tidy_activity.df <- activity.df
# Fill in NAs with mean
tidy_activity.df[is.na(tidy_activity.df$steps), "steps"] <- mean(activity.df$steps, na.rm = TRUE)
# Create total steps by date
tidy_total_steps.df <- tidy_activity.df %>% group_by(date) %>% summarize(total_steps = sum(steps))

# Plot the information
ggplot(tidy_total_steps.df, aes(x = total_steps)) +
  geom_histogram(fill = "blue", bindwidth = 1000) + 
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")


tidy_summary_steps.df <- tidy_total_steps.df %>% summarize(mean_steps = mean(total_steps, na.rm = TRUE), median_steps = median(total_steps, na.rm = TRUE))
print(tidy_summary_steps.df)
print(summary_steps.df)


# Are there differences in activity patterns between weekdays and weekends?
activity.df[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activity.df[, 'day of week' := weekdays(x = date)]
setDT(activity.df)
activity.df[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = activity.df$`day of week`), "weekday or weekend"] <- "weekday"
activity.df[grepl(pattern = "Saturday|Sunday", x = activity.df$`day of week`), "weekday or weekend"] <- "weekend"
activity.df[ , `weekday or weekend` := as.factor(`weekday or weekend`)]

#Average Intervals
summary_steps_interval.df2 <- activity.df %>% group_by(`weekday or weekend`, interval) %>% summarize(mean_steps = mean(steps, na.rm = TRUE))
print(summary_steps_interval.df2)

# Make a panel plot containing a timer series
ggplot(summary_steps_interval.df2, aes(x = interval , y = mean_steps, color = `weekday or weekend`)) +
  facet_grid(`weekday or weekend`~.)+
  geom_line() + 
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

