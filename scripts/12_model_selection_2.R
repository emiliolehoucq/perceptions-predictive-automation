# Model selection

# Setting parameters ------------------------------------------------------

options(java.parameters = "-Xmx54g")

# Loading packages --------------------------------------------------------

library(tidyverse)
library(glmnet) # masks 'expand', 'pack', and 'unpack' from tidyr
library(glmnetUtils)
library(randomForest) # masks 'combine' from 'dplyr' and 'margin' from ggplot2
library(caret) # masks 'lift' from purrr
library(gbm)
library(bartMachine)

# Reading data ------------------------------------------------------------

data_cred_score_train <- readRDS("data/processed/data_cred_score_train.rds") %>% 
  select(-c(effective_cred_score_bin, acceptable_cred_score))

data_risk_score_train <- readRDS("data/processed/data_risk_score_train.rds") %>% 
  select(-c(effective_risk_score_bin, acceptable_risk_score))

data_vid_hiring_train <- readRDS("data/processed/data_vid_hiring_train.rds") %>% 
  select(-c(effective_vid_hiring_bin, acceptable_vid_hiring))

data_cv_hiring_train <- readRDS("data/processed/data_cv_hiring_train.rds") %>% 
  select(-c(effective_cv_hiring_bin, acceptable_cv_hiring))

data_cred_score_test <- readRDS("data/processed/data_cred_score_test.rds") %>% 
  select(-c(effective_cred_score_bin, acceptable_cred_score))

data_risk_score_test <- readRDS("data/processed/data_risk_score_test.rds") %>% 
  select(-c(effective_risk_score_bin, acceptable_risk_score))

data_vid_hiring_test <- readRDS("data/processed/data_vid_hiring_test.rds") %>% 
  select(-c(effective_vid_hiring_bin, acceptable_vid_hiring))

data_cv_hiring_test <- readRDS("data/processed/data_cv_hiring_test.rds") %>% 
  select(-c(effective_cv_hiring_bin, acceptable_cv_hiring))

# Setting parameters ------------------------------------------------------

set.seed(2620) 

fit_control <- trainControl(method = "none", classProbs = TRUE)

# Helper function ---------------------------------------------------------

#' @name misclass_rate
misclass_rate <- function(outcome, model, dataset){
  
  p_hat <- predict(model, newdata = dataset, type = "response")
  class_hat <- if_else(p_hat > 0.5, 1, 0)
  misclass_rate <- mean(class_hat != outcome)
  misclass_rate
  
}

#' @name misclass_rate_rf
misclass_rate_rf <- function(outcome, model, dataset){
  
  class_hat <- predict(model, newdata = dataset, type = "response")
  misclass_rate <- mean(class_hat != outcome)
  misclass_rate
  
}

#' @name misclass_rate_bt
misclass_rate_bt <- function(outcome, model, dataset){
  
  p_hat <- predict(model, newdata = dataset, type = "prob")
  class_hat <- if_else(p_hat[1] > 0.5, 0, 1)
  misclass_rate <- mean(class_hat != outcome)
  misclass_rate
  
}

#' @name misclass_rate_bart
misclass_rate_bart <- function(outcome, model, x_test, y_test){
  
  predictions <- bart_predict_for_test_data(model, x_test, y_test)
  class_hat <- predictions$y_hat
  misclass_rate <- mean(class_hat != y_test)
  misclass_rate
  
}

# Logistic regressions ----------------------------------------------------

cred_score_logit <- glm(fair_cred_score_bin ~ ., data = data_cred_score_train, family = binomial(link = "logit")) 

cred_score_logit_mr <- misclass_rate(data_cred_score_test$fair_cred_score_bin, cred_score_logit, data_cred_score_test)

risk_score_logit <- glm(fair_risk_score_bin ~ ., data = data_risk_score_train, family = binomial(link = "logit")) 

risk_score_logit_mr <- misclass_rate(data_risk_score_test$fair_risk_score_bin, risk_score_logit, data_risk_score_test)

vid_hiring_logit <- glm(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, family = binomial(link = "logit")) 

vid_hiring_logit_mr <- misclass_rate(data_vid_hiring_test$fair_vid_hiring_bin, vid_hiring_logit, data_vid_hiring_test)

cv_hiring_logit <- glm(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, family = binomial(link = "logit")) 

cv_hiring_logit_mr <- misclass_rate(data_cv_hiring_test$fair_cv_hiring_bin, cv_hiring_logit, data_cv_hiring_test)

# Regularized regressions -------------------------------------------------

cred_score_ridge <- glmnet(fair_cred_score_bin ~ ., data = data_cred_score_train, alpha = 0, lambda = 0.4247572, family = "binomial")

cred_score_ridge_mr <- misclass_rate(data_cred_score_test$fair_cred_score_bin, cred_score_ridge, data_cred_score_test)

cred_score_lasso <- glmnet(fair_cred_score_bin ~ ., data = data_cred_score_train, alpha = 1, lambda = 0.01543331, family = "binomial")

cred_score_lasso_mr <- misclass_rate(data_cred_score_test$fair_cred_score_bin, cred_score_lasso, data_cred_score_test)

risk_score_ridge <- glmnet(fair_risk_score_bin ~ ., data = data_risk_score_train, alpha = 0, lambda = 0.3696913, family = "binomial")

risk_score_ridge_mr <- misclass_rate(data_risk_score_test$fair_risk_score_bin, risk_score_ridge, data_risk_score_test)

risk_score_lasso <- glmnet(fair_risk_score_bin ~ ., data = data_risk_score_train, alpha = 1, lambda = 0.01573379, family = "binomial")

risk_score_lasso_mr <- misclass_rate(data_risk_score_test$fair_risk_score_bin, risk_score_lasso, data_risk_score_test)

vid_hiring_ridge <- glmnet(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, alpha = 0, lambda = 0.3217642, family = "binomial")

vid_hiring_ridge_mr <- misclass_rate(data_vid_hiring_test$fair_vid_hiring_bin, vid_hiring_ridge, data_vid_hiring_test)

vid_hiring_lasso <- glmnet(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, alpha = 1, lambda = 0.01493088, family = "binomial")

vid_hiring_lasso_mr <- misclass_rate(data_vid_hiring_test$fair_vid_hiring_bin, vid_hiring_lasso, data_vid_hiring_test)

cv_hiring_ridge <- glmnet(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, alpha = 0, lambda = 0.560717, family = "binomial")

cv_hiring_ridge_mr <- misclass_rate(data_cv_hiring_test$fair_cv_hiring_bin, cv_hiring_ridge, data_cv_hiring_test)

cv_hiring_lasso <- glmnet(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, alpha = 1, lambda = 0.01900601, family = "binomial")

cv_hiring_lasso_mr <- misclass_rate(data_cv_hiring_test$fair_cv_hiring_bin, cv_hiring_lasso, data_cv_hiring_test)

# Random forests ----------------------------------------------------------

cred_score_rf <- randomForest(fair_cred_score_bin ~ ., data = data_cred_score_train, nodesize = 10, mtry = 85)

cred_score_rf_mr <- misclass_rate_rf(data_cred_score_test$fair_cred_score_bin, cred_score_rf, data_cred_score_test)

risk_score_rf <- randomForest(fair_risk_score_bin ~ ., data = data_risk_score_train, nodesize = 10, mtry = 38)

risk_score_rf_mr <- misclass_rate_rf(data_risk_score_test$fair_risk_score_bin, risk_score_rf, data_risk_score_test)

vid_hiring_rf <- randomForest(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, nodesize = 10, mtry = 45)

vid_hiring_rf_mr <- misclass_rate_rf(data_vid_hiring_test$fair_vid_hiring_bin, vid_hiring_rf, data_vid_hiring_test)

cv_hiring_rf <- randomForest(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, nodesize = 10, mtry = 24)

cv_hiring_rf_mr <- misclass_rate_rf(data_cv_hiring_test$fair_cv_hiring_bin, cv_hiring_rf, data_cv_hiring_test)

# Boosted trees -----------------------------------------------------------

cred_score_bt <- train(fair_cred_score_bin ~ ., data = data_cred_score_train %>% mutate(fair_cred_score_bin = factor(fair_cred_score_bin, 
                                                                                                             labels = make.names(levels(fair_cred_score_bin)))), 
                      method = "gbm", 
                      trControl = fit_control, 
                      verbose = FALSE, 
                      tuneGrid = data.frame(interaction.depth = 1,
                                            n.trees = 550,
                                            shrinkage = 0.045,
                                            n.minobsinnode = 10),
                      metric = "Accuracy")

cred_score_bt_mr <- misclass_rate_bt(data_cred_score_test$fair_cred_score_bin, cred_score_bt, data_cred_score_test)

risk_score_bt <- train(fair_risk_score_bin ~ ., data = data_risk_score_train %>% mutate(fair_risk_score_bin = factor(fair_risk_score_bin, 
                                                                                                                     labels = make.names(levels(fair_risk_score_bin)))), 
                       method = "gbm", 
                       trControl = fit_control, 
                       verbose = FALSE, 
                       tuneGrid = data.frame(interaction.depth = 1,
                                             n.trees = 750,
                                             shrinkage = 0.001,
                                             n.minobsinnode = 10),
                       metric = "Accuracy")

risk_score_bt_mr <- misclass_rate_bt(data_risk_score_test$fair_risk_score_bin, risk_score_bt, data_risk_score_test)

vid_hiring_bt <- train(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train %>% mutate(fair_vid_hiring_bin = factor(fair_vid_hiring_bin, 
                                                                                                                     labels = make.names(levels(fair_vid_hiring_bin)))), 
                       method = "gbm", 
                       trControl = fit_control, 
                       verbose = FALSE, 
                       tuneGrid = data.frame(interaction.depth = 1,
                                             n.trees = 150,
                                             shrinkage = 0.089,
                                             n.minobsinnode = 10),
                       metric = "Accuracy")

vid_hiring_bt_mr <- misclass_rate_bt(data_vid_hiring_test$fair_vid_hiring_bin, vid_hiring_bt, data_vid_hiring_test)

cv_hiring_bt <- train(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train %>% mutate(fair_cv_hiring_bin = factor(fair_cv_hiring_bin, 
                                                                                                                     labels = make.names(levels(fair_cv_hiring_bin)))), 
                       method = "gbm", 
                       trControl = fit_control, 
                       verbose = FALSE, 
                       tuneGrid = data.frame(interaction.depth = 1,
                                             n.trees = 1000,
                                             shrinkage = 0.012,
                                             n.minobsinnode = 10),
                       metric = "Accuracy")

cv_hiring_bt_mr <- misclass_rate_bt(data_cv_hiring_test$fair_cv_hiring_bin, cv_hiring_bt, data_cv_hiring_test)

# BART --------------------------------------------------------------------

cred_score_y <- data_cred_score_train$fair_cred_score_bin

cred_score_x <- data_cred_score_train

cred_score_x$fair_cred_score_bin <- NULL

risk_score_y <- data_risk_score_train$fair_risk_score_bin

risk_score_x <- data_risk_score_train

risk_score_x$fair_risk_score_bin <- NULL

vid_hiring_y <- data_vid_hiring_train$fair_vid_hiring_bin

vid_hiring_x <- data_vid_hiring_train

vid_hiring_x$fair_vid_hiring_bin <- NULL

cv_hiring_y <- data_cv_hiring_train$fair_cv_hiring_bin

cv_hiring_x <- data_cv_hiring_train

cv_hiring_x$fair_cv_hiring_bin <- NULL

cred_score_y_test <- data_cred_score_test$fair_cred_score_bin

cred_score_x_test <- data_cred_score_test

cred_score_x_test$fair_cred_score_bin <- NULL

risk_score_y_test <- data_risk_score_test$fair_risk_score_bin

risk_score_x_test <- data_risk_score_test

risk_score_x_test$fair_risk_score_bin <- NULL

vid_hiring_y_test <- data_vid_hiring_test$fair_vid_hiring_bin

vid_hiring_x_test <- data_vid_hiring_test

vid_hiring_x_test$fair_vid_hiring_bin <- NULL

cv_hiring_y_test <- data_cv_hiring_test$fair_cv_hiring_bin

cv_hiring_x_test <- data_cv_hiring_test

cv_hiring_x_test$fair_cv_hiring_bin <- NULL

cred_score_bart <- bartMachine(data.frame(cred_score_x), cred_score_y, k = 2, num_trees = 200)

cred_score_bart_mr <- misclass_rate_bart(data_cred_score_test$fair_cred_score_bin, cred_score_bart, cred_score_x_test, cred_score_y_test)

risk_score_bart <- bartMachine(data.frame(risk_score_x), risk_score_y, k = 5, num_trees = 50)

risk_score_bart_mr <- misclass_rate_bart(data_risk_score_test$fair_risk_score_bin, risk_score_bart, risk_score_x_test, risk_score_y_test)

vid_hiring_bart <- bartMachine(data.frame(vid_hiring_x), vid_hiring_y, k = 5, num_trees = 200)

vid_hiring_bart_mr <- misclass_rate_bart(data_vid_hiring_test$fair_vid_hiring_bin, vid_hiring_bart, vid_hiring_x_test, vid_hiring_y_test)

cv_hiring_bart <- bartMachine(data.frame(cv_hiring_x), cv_hiring_y, k = 5, num_trees = 50)

cv_hiring_bart_mr <- misclass_rate_bart(data_cv_hiring_test$fair_cv_hiring_bin, cv_hiring_bart, cv_hiring_x_test, cv_hiring_y_test)

# Visual model comparison -------------------------------------------------

misclass_rates <- tibble(models = rep(c("Logistic", "Ridge", "Lasso", "Random forest", "Boosted trees", "BART"), times=4),
                         response = c(rep("Credit score", times=6), rep("Risk score", times=6), rep("Video hiring", times=6), rep("Resume hiring", times=6)),
                         misclass_rate = c(cred_score_logit_mr, cred_score_ridge_mr, cred_score_lasso_mr, cred_score_rf_mr, cred_score_bt_mr, cred_score_bart_mr,
                                           risk_score_logit_mr, risk_score_ridge_mr, risk_score_lasso_mr, risk_score_rf_mr, risk_score_bt_mr, risk_score_bart_mr,
                                           vid_hiring_logit_mr, vid_hiring_ridge_mr, vid_hiring_lasso_mr, vid_hiring_rf_mr, vid_hiring_bt_mr, vid_hiring_bart_mr,
                                           cv_hiring_logit_mr, cv_hiring_ridge_mr, cv_hiring_lasso_mr, cv_hiring_rf_mr, cv_hiring_bt_mr, cv_hiring_bart_mr))

saveRDS(misclass_rates, file = "misclass_rates.rds")

misclass_rates_2 <- ggplot(misclass_rates) +
  aes(x=misclass_rate, y=models) +
  geom_point() +
  facet_wrap(~response) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Test misclassification rates across models and responses") +
  xlab("Misclassification rate") +
  ylab("Model") # 632 x 421

pdf("misclass_rates_2.pdf")

print(misclass_rates_2)

dev.off()

table(data_cred_score_test$fair_cred_score_bin)/nrow(data_cred_score_test) # 0.2759382
table(data_risk_score_test$fair_risk_score_bin)/nrow(data_risk_score_test) # 0.4847162
table(data_vid_hiring_test$fair_vid_hiring_bin)/nrow(data_vid_hiring_test) # 0.3535792
table(data_cv_hiring_test$fair_cv_hiring_bin)/nrow(data_cv_hiring_test) # 0.4635762
