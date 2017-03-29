
# Setup -------------------------------------------------------------------

# Clear environment
rm(list=ls())

# Set working directory
setwd("~/Data Science Projects/gymtraffic")

# Load data
load("data/raw_data.RData")

# Load libraries
library(lubridate)
library(caret)
library(randomForest)
library(e1071)



# Split Data into Training and Test ---------------------------------------

# Create split index
split_index <- createDataPartition(gym_df$number_people, p = .75, list = FALSE)

# Create train and test sets
train <- gym_df[split_index, ]
test  <- gym_df[-split_index, ]



# Random Forest Model -----------------------------------------------------

# Create model
rf_model <- randomForest(number_people ~ . - date - timestamp - temperature, data = train, ntree = 50)

# Examine important variables
varImpPlot(rf_model)



# Linear Model ------------------------------------------------------------

# Create model
linear_model <- lm(number_people ~ . - date, data = gym_df)

# Summarize model
summary(linear_model)



# Support Vector Regression Model -----------------------------------------

# Create model
svr_model <- svm(number_people ~ . - date, data = train, scale = TRUE)



# Evaluation --------------------------------------------------------------

# Create predictions
rf_predictions <- predict(rf_model, test)
linear_predictions <- predict(linear_model, test)
svr_predictions <- predict(svr_model, test)
predictions <- predict(rf_model, gym_df)

# Evaluate fit
summary(lm(test$number_people ~ rf_predictions))
summary(lm(test$number_people ~ linear_predictions))
summary(lm(test$number_people ~ svr_predictions))

