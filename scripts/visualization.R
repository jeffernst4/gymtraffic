
# Setup -------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Set working directory
setwd("~/Data Science Projects/gymtraffic")

# Load data
load("data/raw_data.RData")
load("data/ma_data.RData")

# Load libraries
library(ggplot2)
library(lubridate)



# Visualizations ----------------------------------------------------------

# Daily attendance visualization #1
p1 <- ggplot(aggregate(number_people ~ date, gym_df, mean), aes(date, number_people))
p1 +
  geom_area(color = "#AA4488", fill = "#AA4488", alpha = .3) +
  scale_x_date(
    date_breaks = '1 month',
    limits = c(ymd(20150819), ymd(20160816)),
    date_labels = "%B",
    expand = c(.02, 0)) +
  labs(title = "2015-16 UC Berkeley Gym Attendance", x = "Month", y = "Average Gym Attendance") +
  theme(
    axis.text = element_text(color = "#333333", size = 9),
    axis.title = element_text(color = "#333333", face = "bold", size = 12),
    plot.title = element_text(color = "#333333", face = "bold", size = 16),
    panel.background = element_rect(fill = "white"))

# Daily attendance visualization #2
p2 <- ggplot(aggregate(number_people ~ date, gym_df, mean), aes(date, number_people))
p2 +
  geom_line(color = "#3062B3") +
  scale_x_date(
    date_breaks = '1 month',
    limits = c(ymd(20150819), ymd(20160816)),
    date_labels = "%B",
    expand = c(.02, 0)) +
  labs(title = "2015-16 UC Berkeley Gym Attendance", x = "Month", y = "Average Gym Attendance") +
  theme(
    axis.text = element_text(color = "#333333", size = 9),
    axis.title = element_text(color = "#333333", face = "bold", size = 12),
    plot.title = element_text(color = "#333333", face = "bold", size = 16),
    panel.background = element_rect(fill = "white"))

# Daily attendance visualization #3
p3 <- ggplot(aggregate(number_people ~ date, gym_df_ma, mean), aes(date, number_people))
p3 <- p3 +
  geom_line(
    color = "#3062B3",
    size = 1) +
  scale_x_date(
    date_breaks = '1 month',
    limits = c(ymd(20150819), ymd(20160816)),
    date_labels = "%B",
    expand = c(.02, 0)) +
  scale_y_continuous(
    breaks = seq(0, 80, 10),
    limits = c(0, 80)) +
  labs(
    title = "",
    x = "",
    y = "") +
  theme(
    axis.text = element_text(color = "#333333", size = 30),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.text.y = element_text(margin = margin(0, 10, 0, 0)),
    axis.ticks = element_blank())



# Save Visualizations -----------------------------------------------------

# Set environment, select plot, and save plot
png(filename = "plot3.png", width = 3000, height = 1600)
p3
dev.off()







