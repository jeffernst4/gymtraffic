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
  geom_bar() +
  labs(title = "Day of Week Distribution", x = "Day of Week", y = "Count")

# Holidays
prop.table(table(gym_df$is_holiday))
p5 <- ggplot(gym_df, aes(factor(is_holiday, labels = c("Not Holiday", "Holiday"))))
p5 +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Day Type Distribution", x = "Day Type", y = "Proportion") +
  coord_flip()

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

# Attendance by date with dashed lines showing breaks, holidays, and semester start/end for 15-16 school year
p1 <- ggplot(aggregate(number_people ~ date, gym_df, max), aes(date, number_people))
p1 + geom_line() +
  geom_vline(xintercept = c(as.numeric(as.Date("2016-12-25")), as.numeric(as.Date("2016-11-24")), as.numeric(as.Date("2015-11-26")), as.numeric(as.Date("2015-12-25"))), color = "red", linetype = "longdash") +
  geom_vline(xintercept = c(as.numeric(as.Date("2015-08-26")), as.numeric(as.Date("2015-12-18")), as.numeric(as.Date("2016-01-19")), as.numeric(as.Date("2016-05-13")), as.numeric(as.Date("2016-08-24")), as.numeric(as.Date("2016-12-16")), as.numeric(as.Date("2017-01-17")), as.numeric(as.Date("2017-05-05"))), color = "blue", linetype = "longdash") +
  geom_vline(xintercept = c(as.numeric(as.Date("2016-03-19")), as.numeric(as.Date("2016-03-27"))), color = "purple", linetype = "longdash") +
  scale_x_date(date_breaks = '1 month', limits = c(as.Date("2015-08-19"), as.Date("2016-08-16")), date_labels = "%B") +
  labs(title = "2015-2016 Berkeley Gym Attendance", x = "Month", y = "Max Gym Attendance")



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
TimePeoplePlot <- ggplot(gym_df, aes(timestamp, number_people))
TimePeoplePlot <- ggplot(gym_df, aes(timestamp, number_people, color = as.factor(day_of_week)))
TimePeoplePlot <- ggplot(gym_df, aes(timestamp, number_people, color = as.factor(is_holiday)))
TimePeoplePlot +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "ps")) +
  labs(title = "Gym Traffic by Timestamp", x = "Timestamp", y = "Number of People") +
  scale_color_manual(name = "Weekend", values = myColors)

