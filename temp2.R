library(stats)
library(dplyr)
activity_data_full <- read.csv("activity.csv", stringsAsFactors = F)

average_steps_interval <- filter(activity_data_full, steps != "NA") %>%
  group_by(interval) %>%
  summarise(avg_interval_steps = mean(steps) )


 for(i in 1:nrow(activity_data_full)) {

      if(is.na(activity_data_full[i,1])) {
          activity_row <- filter(average_steps_interval, interval == activity_data_full[i,3])
          activity_data_full[[i,1]] =  activity_row[[1,2]]
      }
 }

# Total steps per day
total_steps_day <- activity_data_full %>% 
  group_by(date) %>%
  summarise(total_steps = sum(steps) )

# Average and Median steps per day
print(total_steps_day %>%
        summarise(avg_steps = mean(total_steps), median_step = median(total_steps) )
)

#Histogram of Total steps per day
hist(as.numeric(total_steps_day$total_steps),  col="green", xlab = "Date", main = "Total Steps per Day")





