# Random forests

# Loading packages --------------------------------------------------------

library(tidyverse)
library(randomForest) # masks 'combine' and 'margin' from ggplot2
library(caret) # masks 'lift' from purrr

# Reading data ------------------------------------------------------------

data_cred_score_train <- readRDS("data/processed/data_cred_score_train.rds") %>% 
  select(-c(acceptable_cred_score)) 

data_risk_score_train <- readRDS("data/processed/data_risk_score_train.rds") %>% 
  select(-c(acceptable_risk_score))

data_vid_hiring_train <- readRDS("data/processed/data_vid_hiring_train.rds") %>% 
  select(-c(acceptable_vid_hiring))

data_cv_hiring_train <- readRDS("data/processed/data_cv_hiring_train.rds") %>% 
  select(-c(acceptable_cv_hiring))

# Setting parameters ------------------------------------------------------

set.seed(2620)

train_control <- trainControl(method = "cv",
                              number = 5,
                              search = "grid")

tune_grid <- expand.grid(.mtry = c(1:100))

# cred_score --------------------------------------------------------------

cred_score_rf_mtry <- train(fair_cred_score_bin ~ .,
                            data = data_cred_score_train,
                           method = "rf",
                           metric = "Accuracy",
                           tuneGrid = tune_grid,
                           trControl = train_control,
                           importance = TRUE,
                           nodesize = 10) # makes computing faster and not super important to tune

print(cred_score_rf_mtry) # 91 best
# After 18 it's stable above 0.77. Perhaps go with 30 that's 0.7842

# risk_score --------------------------------------------------------------

risk_score_rf_mtry <- train(fair_risk_score_bin ~ .,
                            data = data_risk_score_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tune_grid,
                            trControl = train_control,
                            importance = TRUE,
                            nodesize = 10) # makes computing faster and not super important to tune

print(risk_score_rf_mtry) # 54 best
# aftwer 25 stable above 0.83. maybe go with 33 that's 0.8366

# vid_hiring --------------------------------------------------------------

vid_hiring_rf_mtry <- train(fair_vid_hiring_bin ~ .,
                            data = data_vid_hiring_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tune_grid,
                            trControl = train_control,
                            importance = TRUE,
                            nodesize = 10) # makes computing faster and not super important to tune

print(vid_hiring_rf_mtry) # best is 73
# after 35 stable above 0.80. maybe 54 that's 0.8092

# vid_hiring --------------------------------------------------------------

cv_hiring_rf_mtry <- train(fair_cv_hiring_bin ~ .,
                            data = data_cv_hiring_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tune_grid,
                            trControl = train_control,
                            importance = TRUE,
                            nodesize = 10) # makes computing faster and not super important to tune

print(cv_hiring_rf_mtry) # best is 74
# stable in 0.79 after 49. maybe 53 that's 0.797
