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

print(average_steps_interval)
  
#Timeseries plot
with(average_steps_interval, plot(x=interval, y=avg_interval_steps, type = "l", xlab = "5 min interval", ylab = "Average Steps Taken"))

#Highest average interval  number of steps
max_steps <- average_steps_interval %>%
  summarise(max_step_interval = max(avg_interval_steps)) 


print( filter(average_steps_interval, avg_interval_steps==max_steps[[1,1]])  %>%
         select(interval)
      )
# 
# #Add weekday/weekend identifier
# activity_data_week <- activity_data %>%
#   mutate(weekend = as.factor( case_when( weekdays(as.Date(date, "%Y-%m-%d")) == "Saturday" | 
#                               weekdays(as.Date(date, "%Y-%m-%d")) == "Sunday" ~ "weekend",
#                               TRUE ~ "weekday" ) ) )
# 
# 
# 
# #Compute averages by weekend/weekday
# activity_data_weekend <- filter(activity_data_week, weekend=="weekend")
# average_steps_interval_weekend <- activity_data_weekend %>%
#   group_by(interval) %>%
#   summarise(avg_interval_steps = mean(steps) )
# 
# activity_data_weekday <- filter(activity_data_week, weekend=="weekday")
# average_steps_interval_weekday <- activity_data_weekday %>%
#   group_by(interval) %>%
#   summarise(avg_interval_steps = mean(steps) )
# 
# #Panel plot two time series
# par(mfrow=c(2,1))
# with(average_steps_interval_weekend, plot(x=interval, y=avg_interval_steps, type = "l", xlab = "5 min interval", ylab = "Average Steps Taken", main = "Weekend"))
# 
# with(average_steps_interval_weekday, plot(x=interval, y=avg_interval_steps, type = "l", xlab = "5 min interval", ylab = "Average Steps Taken", main = "Weekday"))
# 
# 
# #par(mfrow=c(2,1))
# 
# 
# 
# 
# 
# 
