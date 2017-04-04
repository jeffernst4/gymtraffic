
# Setup -------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Set working directory
setwd("~/Data Science Projects/gymtraffic")

# Load csv
gym_df <- read.csv("data/data.csv")[, c(1:3, 7)]
precipitation <- read.csv("data/precipitation.csv")

# Load libraries
library(lubridate)



# Pre-Processing ----------------------------------------------------------

# Clean date variable
gym_df$date <- as.Date(substr(gym_df$date, 1, 10))

# Limit date range
year_interval <- interval(ymd(20150819), ymd(20160816))
gym_df <- gym_df[gym_df$date %within% year_interval, ]

# Remove low observation dates
gym_df <- gym_df[!(gym_df$date == ymd(20160616)), ]

# Prepare precipitation data
precipitation$date <- ymd(precipitation$DATE)
precipitation$precipitation <- precipitation$PRCP
precipitation <- precipitation[, c("date", "precipitation")]

# Merge precipitation data
gym_df <- merge(gym_df, precipitation, by.x = "date", by.y = "date")



# Create Variables --------------------------------------------------------

# Create semester variable
semester_interval <- list(interval(ymd(20150826), ymd(20151218)),
                          interval(ymd(20160117), ymd(20160513)),
                          interval(ymd(20160523), ymd(20160812)))
semester_start <- as.Date(rapply(semester_interval, function(x) as.Date(int_start(x))), origin="1970-01-01")
gym_df$semester <- 0
gym_df$semester[gym_df$date %within% semester_interval[[1]]] <- 1
gym_df$semester[gym_df$date %within% semester_interval[[2]]] <- 2
gym_df$semester[gym_df$date %within% semester_interval[[3]]] <- 3

# Create during semester variable
during_semester_interval <- list(interval(ymd(20150819), ymd(20151218)),
                                 interval(ymd(20160112), ymd(20160513)),
                                 interval(ymd(20160523), ymd(20160812)))
gym_df$during_semester <- 0
gym_df$during_semester[gym_df$date %within% during_semester_interval[[1]]] <- 1
gym_df$during_semester[gym_df$date %within% during_semester_interval[[2]]] <- 1
gym_df$during_semester[gym_df$date %within% during_semester_interval[[3]]] <- 1

# Create start of semester variable
start_semester_interval <- list(interval(ymd(20150819), ymd(20150819) + 14),
                                interval(ymd(20160112), ymd(20160112) + 14),
                                interval(ymd(20160523), ymd(20160523) + 14))
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

# Create semester week variable
gym_df$semester_week <- 0
gym_df$semester_week[gym_df$semester != 0] <- ceiling(difftime(gym_df$date[gym_df$semester != 0] - gym_df$holiday_sum[gym_df$semester != 0] + 1, semester_start[gym_df$semester[gym_df$semester != 0]]) / 7)

# Create day of the week variable
gym_df$day_of_week <- wday(gym_df$date)

# Create hour variable
gym_df$hour <- hour(seconds_to_period(gym_df$timestamp))



# Transform Variables -----------------------------------------------------

# Convert variables to factors
cols_factors <- c("semester", "semester_week", "day_of_week", "hour")
gym_df[, cols_factors] <- lapply(gym_df[, cols_factors], as.factor)



# Clean Dataset -----------------------------------------------------------

# Remove unnecessary variables
gym_df$holiday <- NULL
gym_df$holiday_sum <- NULL



# Save Dataset ------------------------------------------------------------

# Save file
save(gym_df, file = "data/raw_data.RData")



# Create 7-Day Rolling Average --------------------------------------------

# Create moving average function
ma <- function(x, n = 7){filter(x, rep(1/n, n), sides = 2)}

# Create 7-day moving average for gym attendance
gym_df_daily <- data.frame(aggregate(number_people ~ date, gym_df[gym_df$hour %in% 12:18, ], mean))
gym_df_ma <- data.frame(na.omit(ma(ts(gym_df_daily), 7)))
names(gym_df_ma) <- c("date", "number_people")
gym_df_ma$date <- as.Date(gym_df_ma$date, origin = "1970-01-01")



# Save Dataset ------------------------------------------------------------

# Save file
save(gym_df_ma, file = "data/ma_data.RData")



# Calculate Hourly Data ---------------------------------------------------

# Calculate hourly attendance and temperature
attendance_hourly <- aggregate(number_people ~ hour + date, gym_df, mean)
temperature_hourly <- aggregate(temperature ~ hour + date, gym_df, mean)
observations_hourly <- aggregate(temperature ~ hour + date, gym_df, length)

# Create data frame
gym_df_hourly <- data.frame(attendance_hourly, temperature = temperature_hourly[, 3], observations = observations_hourly[, 3])



# Save Dataset ------------------------------------------------------------

# Save file
save(gym_df_hourly, file = "data/hourly_data.RData")