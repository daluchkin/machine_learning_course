# Data Preprocessing

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

dataset = read_csv('Data.csv')
head(dataset)

# 2. Observe the data
glimpse(dataset)
str(dataset)
summary(dataset)

hist(dataset$Age)
plot(dataset)
hist(dataset$Salary)
table(dataset$Country)
table(dataset$Purchased)

# 3. Take care of the missing data

sum(is.na(dataset))
sapply(dataset, function(x) sum(is.na(x)))

# fill NA with mean
dataset$Age <- ifelse(is.na(dataset$Age), 
                      ave(dataset$Age, FUN=function(x) mean(x, na.rm=T)),
                      dataset$Age)

dataset$Salary <- ifelse(is.na(dataset$Salary), 
                      ave(dataset$Salary, FUN=function(x) mean(x, na.rm=T)),
                      dataset$Salary)

dataset

# Encoding categorical data
# identify the categorical variables and transform them into numerical type

dataset$Country <- factor(dataset$Country, 
                          levels = c('France', 'Spain', 'Germany'),
                          labels = c(1, 2, 3))

dataset$Purchased <- factor(dataset$Purchased, 
                          levels = c('No', 'Yes'),
                          labels = c(0, 1))

dataset

# Splitting the dataset into the training and test set

install.packages('caTools')
library(caTools)

set.seed(123)

split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

training_set
test_set

# Feature scaling

training_set[, 2:3] <- scale(training_set[, 2:3])
test_set[, 2:3] <- scale(test_set[, 2:3])

training_set
test_set







