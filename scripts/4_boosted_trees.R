# Boosted trees

# Loading packages --------------------------------------------------------

library(tidyverse)
library(gbm)
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
                              number = 5)

gbm_grid <-  expand.grid(n.trees = (1:30)*50, 
                         shrinkage = seq(0.001, 0.1, length.out = 10),
                         n.minobsinnode = 20, # increases stability of trees
                         interaction.depth = 1) 

# cred_score --------------------------------------------------------------

cred_score_gbm <- train(fair_cred_score_bin ~ ., data = data_cred_score_train, 
                       method = "gbm", 
                       trControl = train_control,
                       verbose = FALSE,
                       tuneGrid = gbm_grid)
# shrinkage 0.012 trees 1100

ggplot(cred_score_gbm)  

# risk_score --------------------------------------------------------------

risk_score_gbm <- train(fair_risk_score_bin ~ ., data = data_risk_score_train, 
                        method = "gbm", 
                        trControl = train_control,
                        verbose = FALSE,
                        tuneGrid = gbm_grid)
# all shrinkage similar up to less than 500 trees
# shrinkage 0.001 trees 100

ggplot(risk_score_gbm)

# vid_hiring --------------------------------------------------------------

vid_hiring_gbm <- train(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, 
                        method = "gbm", 
                        trControl = train_control,
                        verbose = FALSE,
                        tuneGrid = gbm_grid)
# various shrinkage possible depending on trees
# shrinkage 0.001 trees 650

ggplot(vid_hiring_gbm)

# cv_hiring --------------------------------------------------------------

cv_hiring_gbm <- train(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, 
                        method = "gbm", 
                        trControl = train_control,
                        verbose = FALSE,
                        tuneGrid = gbm_grid)
# shrinkage 0.023 trees 500

ggplot(cv_hiring_gbm)
