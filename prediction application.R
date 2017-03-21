# Select features
model_df <- gym_df[, c("number_people", "day_of_week", "hour", "semester", "start_semester", "during_semester", "major_holiday", "holiday_recess", "academic_holiday", "week")]


model <- randomForest(number_people ~ ., data = model_df, ntree = 40)

predict_date <- ymd(20170321)

newdata_df <- model_df[0, ]
newdata_df[1:24, ] <- data.frame(0, 7, 0:23, 3, 0, 1, 0, 0, 0, 8)

predict(model, newdata_df)

plot(predict(model, newdata_df), type = "l")
