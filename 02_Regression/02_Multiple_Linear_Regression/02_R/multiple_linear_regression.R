# Multiple Linear Regression

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

data = read_csv('50_Startups.csv')
head(data)

# Data Preprocessing
data$State <- as.factor(data$State)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split <- sample.split(data$Profit, SplitRatio = 0.8)
training_set <- subset(data, split == T)
test_set <- subset(data, split == F)

# Fit model to the Training set
model <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend` + State, training_set)
summary(model)

# Predicting the Test set results
y_pred <- predict(model, newdata=test_set)


# Backward Elimination
# Step 1: Select a significant level to stay in the model (alpha = 0.05)
# Step 2: Fit the full model with all possible predictors
# Step 3: Consider the predictor with the highest p-value.
#         If p-value > alpha: go to STEP 4 else go to FIN
# Step 4: Remove the predictor
# Step 5: Fit model without this variable
# FIN: model is ready

alpha <- 0.05
df <- training_set

model0 <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend` + State, df)
summary(model0) # => remove State predictor

model1 <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend`, df)
summary(model1) # => remove Administration predictor

model2 <- lm(Profit ~ `R&D Spend` + `Marketing Spend`, df)
summary(model2) # => remove Marketing Spend predictor

model3 <- lm(Profit ~ `R&D Spend`, df)
summary(model3) # => final model

summary(step(model, direction = 'backward'))


