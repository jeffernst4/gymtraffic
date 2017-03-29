
# Checking Fit ------------------------------------------------------------

predictions[gym_df$date == unique(gym_df$date)[1]]

for (i in 1:100) {
  plot(gym_df$timestamp[gym_df$date == unique(gym_df$date)[i]], predictions[gym_df$date == unique(gym_df$date)[i]], type = "l", col = "red", main = paste("day", i))
  lines(gym_df$timestamp[gym_df$date == unique(gym_df$date)[i]], gym_df$number_people[gym_df$date == unique(gym_df$date)[i]], type = "l", col = "black")
}

for (i in 1:100) {
  plot(test$timestamp[test$date == unique(test$date)[i]], rf_predictions[test$date == unique(test$date)[i]], type = "l", col = "red", main = paste("date:", unique(test$date)[i]))
  lines(test$timestamp[test$date == unique(test$date)[i]], test$number_people[test$date == unique(test$date)[i]], type = "l", col = "black")
}



# Start Application for Daily Gym Traffic ---------------------------------

df <- aggregate(predictions ~ hour, gym_df[gym_df$date == unique(test$date)[8], ], mean)

ggplot(df, aes(hour, predictions)) +
  geom_bar(stat="identity")
