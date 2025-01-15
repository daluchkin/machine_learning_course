# Regression Model Selection

# clear global environment
rm(list=ls())

# Libraries loading
library(caret)
library(tidyverse)

# Data loading
script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))
setwd('../00_data/')

data <- read_csv('Data.csv')

glimpse(data)

GGally::ggpairs(data)

# Splitting into training and test set
set.seed(444)
indexes <- sample(1:nrow(data), size = 0.8*nrow(data))

train_data <- data[indexes,]
test_data <- data[-indexes,]

control <- trainControl(method = 'cv', number = 5)

# Models configurations
models_config <- list(
  'Multiple Linear Regression' = list(
    name = 'Multiple Linear Regression',
    train = function(){
      train(PE ~ ., 
            data = train_data,
            method = 'lm',
            trControl = control)
    }
  ),
  'Support Vector Regression' = list(
    name = 'Support Vector Regression',
    train = function(){
      train(PE ~ .,
            data = train_data,
            method = "svmRadial",
            tuneGrid = expand.grid(C = 10^(-2:2),
                                   sigma = 10^(-3:0)),
            trControl = control,
            preProcess = c("center", "scale"))
    }
  ),
  'Decision Tree Regression' = list(
    name = 'Decision Tree Regression',
    train = function(){
      train(PE ~ .,
            data = train_data,
            method = "rpart",
            tuneGrid = expand.grid(cp = seq(0.005, 0.05, by = 0.005)),
            trControl = control,
            control = rpart.control(
              minsplit = 10,
              maxdepth = 5 
            ))
    }
  ),
  'Random Forest Regression' = list(
    name = 'Random Forest Regression',
    train = function(){
      train(PE ~ .,
            data = train_data,
            method = "rf",
            tuneGrid = expand.grid(mtry = 1:4),
            trControl = control,
            ntree = 100)
    }
  )
)

# run grid search
best_models <- lapply(models_config, function(x){
  # Best params for models searching
  print(paste('Searching the best', x[['name']], 'model params...'))
  best_model <- x[['train']]()
  # Model evaluation
  print(paste('Evaluate', x[['name']], 'model...'))
  y_pred <- predict(best_model, newdata = test_data)
  print(paste0(x[['name']], ': Done!'))
  list(
    name = x[['name']],
    model = best_model,
    r2 = cor(test_data$PE, y_pred)^2)
})

# Best model selection

best_model_index <- which.max(sapply(best_models, function(x) x[['r2']]))
best_model <- best_models[[best_model_index]]

best_model
