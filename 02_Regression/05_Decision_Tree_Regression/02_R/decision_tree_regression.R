# Decision Tree Regression

# clear global environment
rm(list=ls())

# loading libs
library(tidyverse)
library(ggplot2)
library(rpart)

# 1. Loading Data
script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))
setwd('../00_data/')

data <- read_csv('Position_Salaries.csv')
data <- data[, 2:3]

data

# 2. Fitting
regressor <- rpart(Salary ~ .,
                   data = data,
                   control = rpart.control(minsplit = 1))

regressor

rpart.plot(regressor)

# 3. Predict on test set
predictions <- predict(regressor, newdata = data)
predictions

# 4. Evaluate the model
mse <- mean((predictions - data$Salary)^2)
rmse <- sqrt(mse)
r2 <- cor(predictions, data$Salary)^2

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r2, "\n")

# 5. Visualizing
ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(regressor, newdata=data)), color='blue') +
  ggtitle('Decision Tree Regression') +
  xlab('Level') +
  ylab('Salary')

x_grid <- seq(min(data$Level), max(data$Level), 0.01)
ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(regressor, newdata=data), color='Discrete')) +
  geom_line(aes(x=x_grid, y=predict(regressor, newdata=data.frame(Level=x_grid)), color='Continuous')) +
  scale_color_manual(name='Type', values = c('Discrete' = 'blue', 'Continuous' = 'green')) +
  ggtitle('Decision Tree Regression') +
  xlab('Level') +
  ylab('Salary')

# 6. Prediction a new result
y_pred <- predict(regressor, data.frame(Level=6.5))
y_pred  

# 7. tuning DTR
tune_grid <- expand.grid(cp = seq(0.005, 0.05, by = 0.005))
dtr_model <- train(Salary ~ .,
                  data=data,
                  method = "rpart",
                  tuneGrid = tune_grid,
                  trControl = trainControl(method = "cv", number = 5),
                  control=rpart.control(minsplit = 1, maxdepth = 4))

dtr_model$bestTune 

rpart.plot(dtr_model$finalModel)

predictions <- predict(dtr_model, newdata = data)

mse <- mean((predictions - data$Salary)^2)
rmse <- sqrt(mse)
r2 <- cor(predictions, data$Salary)^2

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r2, "\n")

ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(regressor, newdata=data)), color='blue') +
  geom_line(aes(x_grid, predict(dtr_model, newdata=data.frame(Level=x_grid))), color='green') +
  ggtitle('Decision Tree Regression') +
  xlab('Level') +
  ylab('Salary')
