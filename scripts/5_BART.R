# BART

# Setting parameters ------------------------------------------------------

options(java.parameters = "-Xmx54g")

# Loading packages --------------------------------------------------------

library(tidyverse)
library(bartMachine)

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

set_bart_machine_num_cores(4)

set.seed(2620)

# Training on full data ---------------------------------------------------

# Preparing data ----------------------------------------------------------

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

# Tuning through 5-fold CV ---------------------------------------------

cred_score_bart_cv <- bartMachineCV(data.frame(cred_score_x), cred_score_y) # best k=2 m=200

risk_score_bart_cv <- bartMachineCV(data.frame(risk_score_x), risk_score_y) # best k=5 m=50

vid_hiring_bart_cv <- bartMachineCV(data.frame(vid_hiring_x), vid_hiring_y) # best k=3 m=200

cv_hiring_bart_cv <- bartMachineCV(data.frame(cv_hiring_x), cv_hiring_y) # best k=5 m=50

# cred_score --------------------------------------------------------------

cred_score_bart <- bartMachine(data.frame(cred_score_x), cred_score_y, k = 2, num_trees = 200)

plot_convergence_diagnostics(cred_score_bart) # stationary process. model sufficiently burned-in.

# risk_score --------------------------------------------------------------

risk_score_bart <- bartMachine(data.frame(risk_score_x), risk_score_y, k = 5, num_trees = 50)

plot_convergence_diagnostics(risk_score_bart) # stationary process. model sufficiently burned-in.

# vid_hiring --------------------------------------------------------------

vid_hiring_bart <- bartMachine(data.frame(vid_hiring_x), vid_hiring_y, k =3 , num_trees = 200)

plot_convergence_diagnostics(vid_hiring_bart) # stationary process. model sufficiently burned-in.

# cv_hiring --------------------------------------------------------------

cv_hiring_bart <- bartMachine(data.frame(cv_hiring_x), cv_hiring_y, k =5 , num_trees = 50)

plot_convergence_diagnostics(cv_hiring_bart) # stationary process. model sufficiently burned-in.
