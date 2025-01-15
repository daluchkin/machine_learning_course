#
# Simple Linear Regression
#

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

data = read_csv('Salary_Data.csv')
head(data)

# Data Structures
glimpse(data)
str(data)

summary(data)

# Missing values
sum(is.na(data))

# Split into training and test set
training_ids <- sample(1:nrow(data), replace = F, size = nrow(data)*0.8)
length(training_ids)

training_set <- data[training_ids, ]
test_set <- data[-training_ids, ]

training_set
test_set

# Fit the model
model = lm(Salary ~ YearsExperience, data=training_set)
summary(model)

# Prediction
pred_y <- predict(model, newdata = data.frame(YearsExperience=test_set$YearsExperience))
pred_y

# DataViz
ggplot(data, 
       aes(x=YearsExperience, y=Salary)) +
  geom_point(data=training_set, 
             aes(x=YearsExperience, 
                 y=Salary), 
             color='red', 
             size=3) +
  geom_point(data=test_set, aes(x=YearsExperience, y=Salary), color='green', size=3) +
  geom_line(data=data.frame(YearsExperience=test_set$YearsExperience, Salary=pred_y),
            aes(x=YearsExperience, y=Salary), color='orange') +
  geom_line(data=data.frame(YearsExperience=test_set$YearsExperience, Salary=pred_y),
            aes(x=YearsExperience, y=Salary), color='orange') +
  geom_line(data=data.frame(YearsExperience=training_set$YearsExperience, Salary=model$fitted.values),
            aes(x=YearsExperience, y=Salary), color='purple') +
  ggtitle('Salary vs Experience') +
  xlab('Years of Experience') +
  ylab('Salary')

# Metrics
MAPE <- function(actual, predicted){
  mean(abs((actual - predicted)/actual)) * 100
}

mape <- MAPE(test_set$Salary, pred_y)

print(paste("Mean Absolute Percentage Error (MAPE):", round(mape, 2), "%"))
