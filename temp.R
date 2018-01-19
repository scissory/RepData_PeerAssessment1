library(stats)
library(dplyr)
activity_data <- read.csv("activity.csv", stringsAsFactors = F)
activity_data <- filter(activity_data, steps != "NA")



# Total steps per day
total_steps_day <- activity_data %>% 
    group_by(date) %>%
    summarise(total_steps = sum(steps) )

# Average and Median steps per day
print(total_steps_day %>%
    summarise(avg_steps = mean(total_steps), median_step = median(total_steps) )
)

#Histogram of Total steps per day
hist(as.numeric(total_steps_day$total_steps),  col="green", xlab = "Date", main = "Total Steps per Day")

#Average steps per time interval
average_steps_interval <- activity_data %>%
    group_by(interval) %>%
    summarise(avg_interval_steps = mean(steps) )
  
#Timeseries plot
with(average_steps_interval, plot(x=interval, y=avg_interval_steps, type = "l", xlab = "5 min interval", ylab = "Average Steps Taken"))

#Highest average interval  number of steps 
print(average_steps_interval %>%
      summarise(max_step_interval = max(avg_interval_steps))
      )




