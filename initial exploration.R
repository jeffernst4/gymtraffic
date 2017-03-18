
# Setup -------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Set working directory
setwd("~/Data Science Projects/gymtraffic")

# Load csv
gym_df <- read.csv("data/data.csv")

# Load libraries
library(ggplot2)



# Individual Variable Exploration -----------------------------------------


# Number of people
summary(gym_df$number_people)
p1 <- ggplot(gym_df, aes(number_people))
p1 +
  geom_histogram(binwidth = 1) +
  labs(title = "Gym Traffic Distribution", x = "Number of People", y = "Count")


# Timestamp
summary(gym_df$timestamp)
p2 <- ggplot(gym_df, aes(timestamp))
p2 +
  geom_histogram(binwidth = 600) +
  labs(title = "Timestamps Distribution", x = "Timestamp", y = "Count")


# Day of week
summary(gym_df$day_of_week)
p3 <- ggplot(gym_df, aes(day_of_week))
p3 +
  geom_histogram(binwidth = 1) +
  labs(title = "Day of Week Distribution", x = "Day of Week", y = "Count")


# Weekend vs weekday
prop.table(table(gym_df$is_weekend))
p4 <- ggplot(gym_df, aes(factor(is_weekend, labels = c("Weekday", "Weekend"))))
p4 +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Day Type Distribution", x = "Day Type", y = "Proportion") +
  coord_flip()


# Holidays
prop.table(table(gym_df$is_holiday))
p5 <- ggplot(gym_df, aes(factor(is_holiday, labels = c("Not Holiday", "Holiday"))))
p5 +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Day Type Distribution", x = "Day Type", y = "Proportion") +
  coord_flip()


# Apparent temperature
summary(gym_df$apparent_temperature)
p2 <- ggplot(gym_df, aes(apparent_temperature))
p2 +
  geom_histogram(binwidth = 1) +
  labs(title = "Apparent Temperature Distribution", x = "Apparent Temperature", y = "Count")


# Temperature
summary(gym_df$temperature)
p2 <- ggplot(gym_df, aes(temperature))
p2 +
  geom_histogram(binwidth = 1) +
  labs(title = "Temperature Distribution", x = "Temperature", y = "Count")


# Start of semester
prop.table(table(gym_df$is_start_of_semester))
p5 <- ggplot(gym_df, aes(factor(is_start_of_semester, labels = c("Not Start", "Start"))))
p5 +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Semester Start Distribution", x = "Semester Start Status", y = "Proportion") +
  coord_flip()



# Relationships -----------------------------------------------------------


# Gym attendance by day type
p1 <- ggplot(gym_df, aes(factor(is_weekend, labels = c("Weekday", "Weekend")), number_people))
p1 +
  stat_summary(fun.y = "mean", geom = "bar") +
  labs(title = "Weekday vs Weekend Gym Attendance", x = "Day Type", y = "Mean Number of People")


# Relationship of temperature with gym traffic
p2 <- ggplot(gym_df, aes(temperature, number_people))
p2 +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps")) +
  labs(title = "Gym Attendance by Temperature", x = "Temperature", y = "Number of People")



#####################################################



# Start of semester effect
SemesterHist <- qplot(gym_df$number_people,
                      geom = "histogram",
                      binwidth = 1,
                      main = "Distribution of People in Gym by Part of Semester",
                      xlab = "Number of People",
                      color = factor(gym_df$is_start_of_semester))
SemesterHist + scale_colour_discrete(name = "Start of Semester", labels = c("No", "Yes"))

# Temperature
TempPlot <- ggplot(gym_df, aes(temperature, apparent_temperature))
TempPlot + geom_point()
TempPlot + geom_point(aes(color = gym_df$number_people))

# Relationship of temperature with gym traffic
TempPeoplePlot <- ggplot(gym_df, aes(temperature, number_people))
TempPeoplePlot +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps")) +
  labs(title = "Gym Traffic by Temperature", x = "Temperature", y = "Number of People")

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(7, "Set")
myColors <- c("#E41A1C", "#377EB8")
SmoothColors <- c("#66C2A5", "#FC8D62")
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp", values = myColors)

# Relationship of timestamp with gym traffic
TimePeoplePlot <- ggplot(gym_df, aes(timestamp, number_people, color = as.factor(is_weekend)))
TimePeoplePlot <- ggplot(gym_df, aes(timestamp, number_people, color = as.factor(day_of_week)))
TimePeoplePlot <- ggplot(gym_df, aes(timestamp, number_people, color = as.factor(is_holiday)))
TimePeoplePlot +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps")) +
  labs(title = "Gym Traffic by Timestamp", x = "Timestamp", y = "Number of People") +
  scale_color_manual(name = "Weekend", values = myColors)
