library(stats)
library(dplyr)
activity_data <- read.csv("activity.csv", stringsAsFactors = F)
setTotal <- NROW(activity_data)
setComplete <- NROW(na.omit(activity_data))
print(setTotal - setComplete)





