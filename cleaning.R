
# Setup -------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Set working directory
setwd("~/Data Science Projects/gymtraffic")

# Load csv
gym_df <- read.csv("data/data.csv")
precipitation <- read.csv("data/precipitation.csv")

# Load libraries
library(lubridate)
library(caret)
library(randomForest)
library(e1071)



# Pre-Processing ----------------------------------------------------------

# Clean date variable
gym_df$date <- as.Date(substr(gym_df$date, 1, 10))


# Limit date range
start_date <- ymd(20150819)
end_date <- ymd(20160816)
gym_df <- gym_df[gym_df$date %within% interval(start_date, end_date), ]


# Remove variables
# gym_df$is_holiday <- NULL
# gym_df$is_start_of_semester <- NULL
gym_df$is_during_semester <- NULL
gym_df$is_weekend <- NULL


# Prepare precipitation data
precipitation$date <- ymd(precipitation$DATE)
precipitation$precipitation <- precipitation$PRCP
precipitation <- precipitation[, c("date", "precipitation")]


# Merge precipitation data
gym_df <- merge(gym_df, precipitation, by.x = "date", by.y = "date")



# Create Variables --------------------------------------------------------

# Create semester variable
semester_interval <- list(interval("2015-08-26", "2015-12-18"),
                          interval("2016-01-17", "2016-05-13"),
                          interval("2016-05-23", "2016-08-12"))
semester_start <- as.Date(rapply(semester_interval, function(x) as.Date(int_start(x))), origin="1970-01-01")
gym_df$semester <- 0
gym_df$semester[gym_df$date %within% semester_interval[[1]]] <- 1
gym_df$semester[gym_df$date %within% semester_interval[[2]]] <- 2
gym_df$semester[gym_df$date %within% semester_interval[[3]]] <- 3


# Create during semester variable
during_semester_interval <- list(interval("2015-08-19", "2015-12-18"),
                                 interval("2016-01-12", "2016-05-13"),
                                 interval("2016-05-23", "2016-08-12"))
gym_df$during_semester <- 0
gym_df$during_semester[gym_df$date %within% during_semester_interval[[1]]] <- 1
gym_df$during_semester[gym_df$date %within% during_semester_interval[[2]]] <- 1
gym_df$during_semester[gym_df$date %within% during_semester_interval[[3]]] <- 1


# Create start of semester variable
start_semester_interval <- list(interval("2015-08-19", ymd(20150819) + 14),
                                interval("2016-01-12", ymd(20160112) + 14),
                                interval("2016-05-23", ymd(20160523) + 14))
gym_df$start_semester <- 0
gym_df$start_semester[gym_df$date %within% start_semester_interval[[1]]] <- 1
gym_df$start_semester[gym_df$date %within% start_semester_interval[[2]]] <- 1
gym_df$start_semester[gym_df$date %within% start_semester_interval[[3]]] <- 1


# Create major holiday variable
major_holidays <- c(ymd(20151126), ymd(20151225))
gym_df$major_holiday <- ifelse(gym_df$date %in% major_holidays, 1, 0)


# Create holiday recess variable
holiday_recess <- interval(ymd(20160320), ymd(20160326))
gym_df$holiday_recess <- ifelse(gym_df$date %within% holiday_recess, 1, 0)


# Create academic holiday variable
academic_holidays <- c(ymd(20150907), ymd(20151111), ymd(20151125), ymd(20151127), ymd(20160118), ymd(20160215))
gym_df$academic_holiday <- ifelse(gym_df$date %in% academic_holidays, 1, 0)


# Create academic variable
gym_df$holiday <- ifelse(rowSums(gym_df[, grep("holiday", names(gym_df))]) > 0, 1, 0)


# Create cumulative sum of holidays per semester
holiday_dates <- aggregate(cbind(semester, holiday) ~ date, gym_df, mean)
holiday_dates <- cbind(holiday_dates, holiday_sum = ave(holiday_dates$holiday, holiday_dates$semester, FUN = cumsum))
holiday_dates <- holiday_dates[, c("date", "holiday_sum")]
gym_df <- merge(gym_df, holiday_dates, by.x = "date", by.y = "date")


# Create week variable
gym_df$week <- 0
gym_df$week[gym_df$semester != 0] <- ceiling(difftime(gym_df$date[gym_df$semester != 0] - gym_df$holiday_sum[gym_df$semester != 0] + 1, semester_start[gym_df$semester[gym_df$semester != 0]]) / 7)


# Create year variable
gym_df$year <- year(gym_df$date)




# Modeling Test -----------------------------------------------------------

# Convert variables to factors
cols_factors <- c("day_of_week", "month", "hour", "semester", "week", "year")
gym_df[, cols_factors] <- lapply(gym_df[, cols_factors], as.factor)

#create a split index
split_index <- createDataPartition(gym_df$number_people, p = .75, list = FALSE)

#create train and test sets
train <- gym_df[split_index, ]
test  <- gym_df[-split_index, ]


#linear model
model <- lm(number_people ~ timestamp + year + month + day_of_week + temperature + hour + semester + major_holiday + holiday_recess + academic_holiday + week + is_holiday + is_start_of_semester + is_during_semester, data = gym_df)

#random forest
model <- randomForest(number_people ~ day_of_week + temperature + precipitation + hour + semester + start_semester + during_semester + major_holiday + holiday_recess + academic_holiday + week, data = train, ntree = 30)

predictions <- predict(model, test)
summary(lm(test$number_people ~ predictions))
varImpPlot(model)

#support vector regression
model <- svm(number_people ~ timestamp + year + month + day_of_week + temperature + hour + semester + major_holiday + holiday_recess + academic_holiday + week + is_start_of_semester + is_during_semester, data = train, scale = FALSE)




############################



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





