setwd("~/Data Science Projects/gymtraffic")

library(lubridate)

GymTraffic <- read.csv("Data/data.csv")


#plan/ideas
#identify day id, so we can use time series, from beginning of semester to end of semester with holidays as markers
#convert timestamp to hour
#days since beginning of semester
#create query of day id
#count actual days becaue they're not in order
#remove 43 and 253 for visualizations
#maybe consider day as going until 1am
#time open and close
#find a way to plot from 5am to 1am



# Create New Variables ----------------------------------------------------

#Create time
GymTraffic$time <- seconds_to_period(GymTraffic$timestamp)

#Create hour
GymTraffic$hour <- hour(GymTraffic$time)

#Create day_id
# GymTraffic$day_id[1] <- 1
# for (i in 2:nrow(GymTraffic)) {
#   GymTraffic$day_id[i] <- ifelse(GymTraffic$timestamp[i] < GymTraffic$timestamp[i - 1], GymTraffic$day_id[i - 1] + 1, GymTraffic$day_id[i - 1])
# }
GymTraffic$day_id[1] <- 1
for (i in 2:nrow(GymTraffic)) {
  GymTraffic$day_id[i] <- ifelse(GymTraffic$timestamp[i - 1] < 10000 & GymTraffic$timestamp[i] > 10000, GymTraffic$day_id[i - 1] + 1, GymTraffic$day_id[i - 1])
}

#Clean invalid data due to weird day of week
GymTraffic <- GymTraffic[1:26002, ]



# Initial Exploration -----------------------------------------------------

#Temperature by day - median, min, max
plot(aggregate(temperature ~ day_id, data = GymTraffic, FUN = median), type = "l")
plot(aggregate(temperature ~ day_id, data = GymTraffic, FUN = max), type = "l", col = "red", ylim = c(40, 90))
points(aggregate(temperature ~ day_id, data = GymTraffic, FUN = min), type = "l", col = "blue")

#Identify days that are beginning of semester
plot(GymTraffic$day_id, GymTraffic$is_start_of_semester, type = "l")
unique(GymTraffic$day_id[GymTraffic$is_start_of_semester == 1])

#Attendance by day, coloring beginning of semester
plot(aggregate(number_people ~ day_id, data = GymTraffic, FUN = median), type = "l")

#Attendance over time for a specific day id
plot(GymTraffic$number_people[GymTraffic$day_id == 4], type = "l")



# Query of Day ID ---------------------------------------------------------

#Create variables
people_max <- aggregate(number_people ~ day_id, GymTraffic, max)
time_open <- aggregate(timestamp ~ day_id, GymTraffic, function(x) which.max(x > 10000))

days_df <- data.frame(id = unique(GymTraffic$day_id), people_max)

hist(GymTraffic$number_people, breaks = 100)
max(GymTraffic$number_people)
median(GymTraffic$number_people)

hist(GymTraffic$timestamp, breaks = 50)
min(GymTraffic$timestamp[GymTraffic$timestamp > 10000])
max(GymTraffic$timestamp[GymTraffic$timestamp < 10000])

# gym opens at 5:15 am and closes at 1:00 am
# determine day #

plot(GymTraffic$timestamp, GymTraffic$number_people)

hist(GymTraffic$day_of_week, breaks = 9)

table(GymTraffic$is_weekend, GymTraffic$day_of_week)

#saturday is 5, sunday is 6

table(GymTraffic$day_of_week, GymTraffic$is_holiday)

plot(aggregate(number_people ~ day_of_week, data = GymTraffic, mean), type = "l", ylim = c(0, 50))

hist(GymTraffic$apparent_temperature, breaks = 30)
hist(GymTraffic$temperature, breaks = 30)

plot(aggregate(temperature ~ timestamp, data = GymTraffic, median), type = "l", ylim = c(50, 70))
