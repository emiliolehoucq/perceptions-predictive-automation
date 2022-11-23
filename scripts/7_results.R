# Results

# Loading packages --------------------------------------------------------

library(tidyverse)
library(glmnet) # masks 'expand', 'pack', and 'unpack' from tidyr
library(glmnetUtils)
library(broom)

# Reading data ------------------------------------------------------------

data_cred_score_train <- readRDS("data/processed/data_cred_score_train.rds") %>% 
  select(-c(acceptable_cred_score))

data_risk_score_train <- readRDS("data/processed/data_risk_score_train.rds") %>% 
  select(-c(acceptable_risk_score))

data_vid_hiring_train <- readRDS("data/processed/data_vid_hiring_train.rds") %>% 
  select(-c(acceptable_vid_hiring))

data_cv_hiring_train <- readRDS("data/processed/data_cv_hiring_train.rds") %>% 
  select(-c(acceptable_cv_hiring))

# Regularized regressions -------------------------------------------------

cred_score_lasso <- glmnet(fair_cred_score_bin ~ ., data = data_cred_score_train, alpha = 1, lambda = 0.01397735, family = "binomial")

cred_score_lasso_tidy <- tidy(cred_score_lasso, return_zeros = FALSE)

cred_score_lasso_tidy %>% 
  arrange(estimate) %>% 
  print(n = Inf)

cred_score_lasso_tidy <- cred_score_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "algo", "algo", "tech", "media", "dem", "media", "tech", "media", "media", "pol", "tech", "media", "pol", "pol", 
                          "pol", "tech", "media", "media", "media", "pol", "media", "media", "algo", "tech", "dem", "media", "dem", "media", "tech",
                          "tech", "tech", "media", "tech", "media", "tech", "media", "pol", "tech", "dem", "tech", "tech", "pol", "tech", "media",
                          "algo", NA))

overlap <- c("yes", "no", rep("yes", 20), "no", "yes", "no", "no", "yes", "no", "no", "yes", "no", "yes", "yes", "no", "no", "yes", 
            "yes", "no", rep("yes", 6), "no", "no")

cred_score_lasso_tidy <- ggplot(cred_score_lasso_tidy %>% filter(term != "(Intercept)")) +
  aes(x = estimate, y = reorder(term, - estimate)) +
  geom_point(aes(shape = category), size = 1.7) +
  geom_vline(xintercept = 0, size = 0.2) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(colour= rev(ifelse(overlap == "yes", "grey30", "black")))) +
  ggtitle("Predictors of thinking that a finance score is fair") +
  xlab("Magnitude and sign of lasso coefficient") +
  ylab("Predictor") +
  scale_shape_discrete(name = "Variable type",
                       labels = c("Algorithms", "Demographics", "Soc media", "Politics",
                                  "Technology", "Other")) +
  scale_y_discrete(labels = rev(c("Fair to mine videos", "Effective", "Fair to mine resumes", "No response tech corps protect data",
                              "Acceptable for soc media differentially remind vote", "Non-white", "Acceptable soc media differentially change site",
                              "Tech corps don't fail to anticipate soc impact", "Users lots control Newsfeed", "Most fb posts relevant",
                              "Finance doesn't have enough power", "Programs can be unbiased", "Acceptable soc media use data political adds",
                              "Pharmaceuticals have right amount of power", "Finance has right amount of power", "Conservative", 
                              "Tech corps don't benefit privileged", "Acceptable soc media experiment sentiments", "Soc media content doesn't make angry",
                              "Acceptable soc media use data advertise", "Advertisers have right amount power", "Hasn't taken break fb",
                              "Doesn't see deceiving soc media posts", "Acceptable mining resumes", "Tech corps more good than bad impact on soc",
                              "High school or less", "Hasn't changed privacy settings influence Newsfeed", "Rel attendance once a week",
                              "Much content soc media about immigration", "Programs are biased", "Tech corps benefit privileged",
                              "Tech corps support men views over women", "Has changed privacy influence Newsfeed", "Tech corps should be more regulated",
                              "Has taken break fb", "Tech corps equally support liberal and conservative", "No response soc media overestimates people's impact",
                              "Advertisers too much power", "Can't trust tech corps to do right", "No response race", "Unacceptable soc media data recommend friend",
                              "Uncceptable soc media data advertise", "Very liberal", "Tech corps don't do enough protect data", 
                              "No response how often soc media makes depressed", "Inffective"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Predictor names in black when not selected by lasso models fitted excluding 
       effectiveness of response as predictor.") # 670 x 550

pdf("cred_score_lasso_tidy.pdf")

print(cred_score_lasso_tidy)

dev.off()

risk_score_lasso <- glmnet(fair_risk_score_bin ~ ., data = data_risk_score_train, alpha = 1, lambda = 0.01569086, family = "binomial")

risk_score_lasso_tidy <- tidy(risk_score_lasso, return_zeros = FALSE)

risk_score_lasso_tidy <- risk_score_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "algo", "pol", "algo", "algo", "pol", "media", "media", "media", "dem", "pol", "tech", "media", "media", "media",
                          "media", "tech", "pol", "pol", "pol", "algo", NA, "algo"))

overlap <- c("no", rep("yes", 9), "no", "no", "yes", "yes", "no", "yes", "no", rep("yes", 4), "no")
  
risk_score_lasso_tidy <- ggplot(risk_score_lasso_tidy %>% filter(term != "(Intercept)")) +
  aes(x = estimate, y = reorder(term, - estimate)) +
  geom_point(aes(shape = category), size = 1.7) +
  geom_vline(xintercept = 0, size = 0.2) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(colour= rev(ifelse(overlap == "yes", "grey30", "black")))) +
  ggtitle("Predictors of thinking that a risk score is fair") +
  xlab("Magnitude and sign of lasso coefficient") +
  ylab("Predictor") +
  scale_shape_discrete(name = "Variable type",
                       labels = c("Algorithms", "Demographics", "Soc media", "Politics",
                                  "Technology", "Other")) +
  scale_y_discrete(labels = rev(c("Effective", "Fair mine videos", "Small businesses have too much power", "Fair mine resumes",
                                  "Acceptable mine videos", "Republican", "Yt videos important to pass time", 
                                  "Acceptable soc media differentially remind vote", "Acceptable soc media differentially experiment sentiments",
                                  "$30-$74,999 family income", "Very conservative", "Tech corps support conservative over liberal views",
                                  "Soc media important express pol opinions", "Hasn't looked info protests soc media", "Has (un)followed groups/orgs influence Newsfeed",
                                  "Hasn't liked, shared or commented influence Newsfeed", "Can't trust tech corps do what's right", "Very liberal",
                                  "Farming and agriculture have too much power", "Democrat", "Unfair mine videos", "Ineffective"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Predictor names in black when not selected by lasso models fitted excluding 
       effectiveness of response as predictor.") # 670 x 550

pdf("risk_score_lasso_tidy.pdf")

print(risk_score_lasso_tidy)

dev.off()

vid_hiring_lasso <- glmnet(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, alpha = 1, lambda = 0.01452274, family = "binomial")

vid_hiring_lasso_tidy <- tidy(vid_hiring_lasso, return_zeros = FALSE)

vid_hiring_lasso_tidy <- vid_hiring_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "algo", "algo", "algo", "media", "tech", "algo", "tech", "media", "pol", "dem", "media", "media", "media", "media",
                          "dem", "media", "dem", "media", "pol", "dem", "tech", "dem", "dem", "media", "tech", "algo", NA, "algo"))

overlap <- c("no", rep("yes", 13), "no", "yes", "no", "no", "yes", "no", "yes", "yes", "no", rep("yes", 4), "no")

vid_hiring_lasso_tidy <- ggplot(vid_hiring_lasso_tidy %>% filter(term != "(Intercept)")) +
  aes(x = estimate, y = reorder(term, - estimate)) +
  geom_point(aes(shape = category), size = 1.7) +
  geom_vline(xintercept = 0, size = 0.2) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(colour= rev(ifelse(overlap == "yes", "grey30", "black")))) +
  ggtitle("Predictors of thinking that mining videos is fair") +
  xlab("Magnitude and sign of lasso coefficient") +
  ylab("Predictor") +
  scale_shape_discrete(name = "Variable type",
                       labels = c("Algorithms", "Demographics", "Soc media", "Politics",
                                  "Technology", "Other")) +
  scale_y_discrete(labels = rev(c("Effective", "Fair finance score", "Fair risk score", "Acceptable risk score", 
                                  "Acceptable soc media differentially experiment sentiments", "Tech corps should be less regulated",
                                  "Acceptable finance score", "Tech corps have right amount of power", "Little content soc media about gun control/violence",
                                  "Very conservative", "Married or living with partner", "Acceptable soc media use data advertise",
                                  "Acceptable soc media differentially remind vote", "Users lots control over Newsfeed", "Notice Yt videos people in danger",
                                  "Male", "Little content soc media about race", "$30-$74,999 family income", "Has deleted Fb app phone",
                                  "Liberal", "50-64 years old", "Tech corps often fail anticipate soc impact", "Divorced, separated or widowed", "High school or less",
                                  "Soc media contents don't provide accurate picture society", "Can't trust tech corps do what's right", "Unfair risk score",
                                  "Ineffective"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Predictor names in black when not selected by lasso models fitted excluding 
       effectiveness of response as predictor.") # 670 x 550

pdf("vid_hiring_lasso_tidy.pdf")

print(vid_hiring_lasso_tidy)

dev.off()

cv_hiring_lasso <- glmnet(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, alpha = 1, lambda = 0.01519515, family = "binomial")

cv_hiring_lasso_tidy <- tidy(cv_hiring_lasso, return_zeros = FALSE)

cv_hiring_lasso_tidy <- cv_hiring_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "algo", "algo", "media", "media", "media", "algo", "tech", "media", "media", "pol", "algo", "dem",
                          "tech", "media", "pol", "media", "tech", "media", "algo", "pol", "dem", "algo", "tech", "media", "algo", NA))

overlap <- c("no", rep("yes", 9), "no", "yes", rep("no", 8), rep("yes", 5), "no")

cv_hiring_lasso_tidy <- ggplot(cv_hiring_lasso_tidy %>% filter(term != "(Intercept)")) +
  aes(x = estimate, y = reorder(term, - estimate)) +
  geom_point(aes(shape = category), size = 1.7) +
  geom_vline(xintercept = 0, size = 0.2) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(colour= rev(ifelse(overlap == "yes", "grey30", "black")))) +
  ggtitle("Predictors of thinking that mining resumes is fair") +
  xlab("Magnitude and sign of lasso coefficient") +
  ylab("Predictor") +
  scale_shape_discrete(name = "Variable type",
                       labels = c("Algorithms", "Demographics", "Soc media", "Politics",
                                  "Technology", "Other")) +
  scale_y_discrete(labels = rev(c("Effective", "Acceptable finance score", "Fair risk score", "Acceptable soc media differentially change site",
                                  "Hasn't liked, shared or commented influence Newsfeed", "Acceptable soc media differentially experiment sentiments",
                                  "Acceptable risk score", "Tech corps protect data", "Acceptable soc media use data advertise", 
                                  "Hasn't changed privacy/ad preferences influence Newsfeed", "Unions have right amount power", "Effective risk score",
                                  "Race missing data", "Tech corps don't benefit privileged", "Soc media more people deceptive", "Advertisers don't have enough power",
                                  "Missing data how often soc media content makes angry", "Tech corps do anticipate their soc impact", 
                                  "Soc media important find people who share views", "Fair finance score", "Advertisers have too much power", 
                                  "Non-white", "Ineffective risk score", "Tech corps have more bad than good soc impact", "Soc media content makes angry", "Ineffective"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Predictor names in black when not selected by lasso models fitted excluding 
       effectiveness of response as predictor.") # 670 x 550

pdf("cv_hiring_lasso_tidy.pdf")

print(cv_hiring_lasso_tidy)

dev.off()
