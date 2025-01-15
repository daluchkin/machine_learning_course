# Support Vector Regression (SVR)

# clear global environment
rm(list=ls())

install.packages('hardhat')
install.packages('caret')
install.packages('e1071')

# loading libs
library(tidyverse)
library(ggplot2)
library(caTools)
library(caret)
library(e1071)

# Loading Data
script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))
setwd('../00_data/')

data <- read_csv('Position_Salaries.csv')
data <- data[, 2:3]

data

# Fitting SVR
?svm
regressor <- svm(formula = Salary ~ ., 
                 data = data, 
                 type='eps-regression' # kernel
                 ) 

regressor

# Forecast on test set
predictions <- predict(regressor, newdata = data)

# Model Evaluation
mse <- mean((predictions - data$Salary)^2)
rmse <- sqrt(mse)
r2 <- cor(predictions, data$Salary)^2

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r2, "\n")

# Visualizing the SVR
ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(regressor, newdata=data)), color='blue') +
  ggtitle('Support Vector Regression') +
  xlab('Level') +
  ylab('Salary')

# Prediction a new result
y_pred <- predict(regressor, data.frame(Level=6.5))
y_pred  

# tuning SVR
tune_grid <- expand.grid(C = 10^(-2:2), 
                         sigma = 10^(-4:0))
svr_model <- train(Salary ~ .,
                  data=data,
                  method = "svmRadial",
                  tuneGrid = tune_grid,
                  trControl = trainControl(method = "cv", number = 5),
                  preProcess = c("center", "scale"))

svr_model 

predictions <- predict(svr_model, newdata = data)

mse <- mean((predictions - data$Salary)^2)
rmse <- sqrt(mse)
r2 <- cor(predictions, data$Salary)^2

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r2, "\n")

ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(regressor, newdata=data)), color='blue') +
  geom_line(aes(data$Level, predict(svr_model, newdata=data)), color='green') +
  ggtitle('Support Vector Regression') +
  xlab('Level') +
  ylab('Salary')
