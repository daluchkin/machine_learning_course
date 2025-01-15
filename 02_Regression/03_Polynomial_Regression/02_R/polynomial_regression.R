# Polynomial Regression

# clear global environment
rm(list=ls())

# loading libs
library(tidyverse)
library(ggplot2)
library(caTools)

# Loading Data
script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))
setwd('../00_data/')

data <- read_csv('Position_Salaries.csv')
data <- data[, 2:3]

data

# Fitting Linear Regression
lin_reg <- lm(Salary ~ ., data)
summary(lin_reg)

# Fitting Polynomial Regression
data$Level2 <- data$Level ^ 2
data$Level3 <- data$Level ^ 3
data
poly_reg <- lm(Salary ~ ., data)
summary(poly_reg)

# Visualising the Linear Regression
ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(lin_reg, newdata=data)), color='blue') +
  ggtitle('Linear Regression') +
  xlab('Level') +
  ylab('Salary')
  


# Visualising the Polynomial Regression
ggplot() +
  geom_point(aes(data$Level, data$Salary), color='red') +
  geom_line(aes(data$Level, predict(poly_reg, newdata=data)), color='blue') +
  ggtitle('Polynomial Regression') +
  xlab('Level') +
  ylab('Salary')

# Predicting Linear Regression
predict(lin_reg, data.frame(Level=6.5))

# Predicting Polynomial Regression
data2 <- data.frame(Level=6.5)
data2$Level2 <- data2$Level ^ 2
data2$Level3 <- data2$Level ^ 3
data2
predict(poly_reg, data2)
