
# Setup -------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Set working directory
setwd("~/Data Science Projects/gymtraffic")

# Load csv
gym_df <- read.csv("data/data.csv")

# Load libraries
library(lubridate)



# Remove Invalid Data -----------------------------------------------------

#Clean invalid data due to weird day of week
gym_df <- gym_df[1:26002, ]



# Create New Variables ----------------------------------------------------

# Create time
gym_df$time <- seconds_to_period(gym_df$timestamp)


# Create hour
gym_df$hour <- hour(gym_df$time)


# Create day id
gym_df$day_id[1] <- 1
for (i in 2:nrow(gym_df)) {
  gym_df$day_id[i] <- ifelse(gym_df$timestamp[i - 1] > gym_df$timestamp[i], gym_df$day_id[i - 1] + 1, gym_df$day_id[i - 1])
}


# Identify the start of each session
day_start <- sapply(unique(gym_df$day_id), function(x) min(gym_df$timestamp[gym_df$day_id == x & gym_df$hour >= 5]))
day_start_df <- data.frame(day_id = unique(gym_df$day_id), timestamp = day_start)


# Match with original data frame
gym_df$day_start <- 0
for (i in 1:nrow(day_start_df)) {
  gym_df$day_start[gym_df$day_id == day_start_df$day_id[i] & gym_df$timestamp == day_start_df$timestamp[i]] <- 1
}


# Create session id - each session ends at 1am
gym_df$session_id[1] <- 1
for (i in 2:nrow(gym_df)) {
  gym_df$session_id[i] <- ifelse(gym_df$day_start[i] == 1, gym_df$session_id[i - 1] + 1, gym_df$session_id[i - 1])
}


# Identify start days for semester
unique(gym_df$session_id[gym_df$is_start_of_semester == 1])
semester1_start <- 19
semester2_start <- 164


# Calculate actual dates
start_date <- as.Date("08/08/2015", "%m/%d/%Y")
gym_df$date <- start_date + gym_df$day_id - 1
gym_df$date[gym_df$date > "2016-04-15"] <- gym_df$date[gym_df$date > "2016-04-15"] + 1





