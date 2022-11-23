# Regularized modeling

# Loading packages --------------------------------------------------------

library(tidyverse)
library(glmnet)
library(glmnetUtils)

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

lambda_grid <- 10^seq(-2, 10, length = 200)

# cred_score --------------------------------------------------------------

cred_score_ridge_cv <- cv.glmnet(formula = fair_cred_score_bin ~ ., 
            data = data_cred_score_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(cred_score_ridge_cv)

cred_score_lasso_cv <- cv.glmnet(
    formula = fair_cred_score_bin ~ ., 
    data = data_cred_score_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(cred_score_lasso_cv)

cred_score_ridge_cv$lambda.min # 0.1846425
cred_score_lasso_cv$lambda.min # 0.01397735

# risk_score --------------------------------------------------------------

risk_score_ridge_cv <- cv.glmnet(formula = fair_risk_score_bin ~ ., 
            data = data_risk_score_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(risk_score_ridge_cv)

risk_score_lasso_cv <- cv.glmnet(
    formula = fair_risk_score_bin ~ ., 
    data = data_risk_score_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(risk_score_lasso_cv)

risk_score_ridge_cv$lambda.min # 0.09221979
risk_score_lasso_cv$lambda.min # 0.01569086

# vid_hiring --------------------------------------------------------------

vid_hiring_ridge_cv <- cv.glmnet(formula = fair_vid_hiring_bin ~ ., 
            data = data_vid_hiring_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(vid_hiring_ridge_cv)

vid_hiring_lasso_cv <- cv.glmnet(
    formula = fair_vid_hiring_bin ~ ., 
    data = data_vid_hiring_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(vid_hiring_lasso_cv)

vid_hiring_ridge_cv$lambda.min # 0.1217383
vid_hiring_lasso_cv$lambda.min # 0.01452274

# cv_hiring ---------------------------------------------------------------

cv_hiring_ridge_cv <- cv.glmnet(formula = fair_cv_hiring_bin ~ ., 
            data = data_cv_hiring_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(cv_hiring_ridge_cv)

cv_hiring_lasso_cv <- cv.glmnet(
    formula = fair_cv_hiring_bin ~ ., 
    data = data_cv_hiring_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(cv_hiring_lasso_cv)

cv_hiring_ridge_cv$lambda.min # 0.105956
cv_hiring_lasso_cv$lambda.min # 0.01519515
