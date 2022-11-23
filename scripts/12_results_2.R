# Results

# Loading packages --------------------------------------------------------

library(tidyverse)
library(glmnet) # masks 'expand', 'pack', and 'unpack' from tidyr
library(glmnetUtils)
library(broom)

# Reading data ------------------------------------------------------------

data_cred_score_train <- readRDS("data/processed/data_cred_score_train.rds") %>% 
  select(-c(effective_cred_score_bin, acceptable_cred_score))

data_risk_score_train <- readRDS("data/processed/data_risk_score_train.rds") %>% 
  select(-c(effective_risk_score_bin, acceptable_risk_score))

data_vid_hiring_train <- readRDS("data/processed/data_vid_hiring_train.rds") %>% 
  select(-c(effective_vid_hiring_bin, acceptable_vid_hiring))

data_cv_hiring_train <- readRDS("data/processed/data_cv_hiring_train.rds") %>% 
  select(-c(effective_cv_hiring_bin, acceptable_cv_hiring))

# Regularized regressions -------------------------------------------------

cred_score_lasso <- glmnet(fair_cred_score_bin ~ ., data = data_cred_score_train, alpha = 1, lambda = 0.01543331, family = "binomial")

cred_score_lasso_tidy <- tidy(cred_score_lasso, return_zeros = FALSE)

cred_score_lasso_tidy <- cred_score_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "algo", "media", "media", "media", "dem", "media", "tech", "tech", "tech", "algo", "pol", "media",
                          "pol", "media", "pol", "pol", "tech", "algo", "pol", "media", "media", "algo", "media", "tech", "dem",
                          "media", "media", "dem", "dem", "tech", "media", "tech", "tech", "tech", "media", "media", "media", "media",
                          "tech", "pol", "tech", NA))

overlap <- c(rep("yes", 10), "no", rep("yes", 11), "no", "no", "yes", "yes", "yes", "no", "yes", "no", "no", rep("yes", 10))

cred_score_lasso_tidy_2 <- ggplot(cred_score_lasso_tidy %>% filter(term != "(Intercept)")) +
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
  scale_y_discrete(labels = rev(c("Fair mine videos", "Fair mine resumes", "Acceptable soc media differentially change site",
                                  "Users lot of control over Newsfeed", "Acceptable soc media differentially remind vote",
                                  "Non-white", "Acceptable soc media use data for political adds", "No response tech corps protect data",
                                  "Programs can be unbiased", "Tech corps don't fail to anticipate impact on soc", "Effective video mining",
                                  "Finance doesn't have enough power","Acceptable soc media experiment sentiments", "Advertisers have right amount power",
                                  "Most fb posts relevant", "Finance has right amount of power", "Pharmaceuticals have right amount of power",
                                  "Tech corps don't benefit privileged", "Acceptable mining resumes", "Conservative", "Acceptable soc media use data advertise",
                                  "Hasn't taken break fb", "Effective resume mining", "Acceptable soc media use data recommend friends",
                                  "Tech corps more good than bad impact on soc", "Male", "Soc media content doesn't make angry", 
                                  "Hasn't changed privacy settings influence Newsfeed", "Female", "No response race", "Tech corps less ethical than others",
                                  "Unacceptable soc media differentially remind vote", "Programs are biased", "Tech corps support men views over women",
                                  "Tech corps equally support liberal and conservative", "No response soc media overestimates people's impact",
                                  "Unacceptable soc media data advertise", "Has changed private privacy/ad settings influence Newsfeed",
                                  "Unccceptable soc media use data recommend friends",
                                  "Can't trust tech corps to do right", "Very liberal", 
                                  "Tech corps don't do enough to protect data"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Model fitted excluding effectiveness of response as predictor.") # 670 x 550

pdf("cred_score_lasso_tidy_2.pdf")

print(cred_score_lasso_tidy_2)

dev.off()

risk_score_lasso <- glmnet(fair_risk_score_bin ~ ., data = data_risk_score_train, alpha = 1, lambda = 0.01573379, family = "binomial")

risk_score_lasso_tidy <- tidy(risk_score_lasso, return_zeros = FALSE)

risk_score_lasso_tidy <- risk_score_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("pol", "algo", "algo", "pol", "tech", "algo", "media", "algo", "pol", "dem", "algo", "media", "media", "media",
                          "pol", "media", "media", "media", "media", "media", "media", "tech", "tech", "media", "media", "media", "media",
                          "media", "dem", "media", "media", "media", "media", "media", "tech", "pol", "tech", "media", "media", "media",
                          "media", "pol", "media", "pol", "algo", "media", "dem", NA))

overlap <- c(rep("yes", 4), "no", "yes", "yes", rep("no", 5), "yes", "no", "no", "yes", "yes", "no", "no", "yes", rep("no", 8), "yes",
             rep("no", 6), "yes", rep("no", 5), rep("yes", 4), "no", "no")

risk_score_lasso_tidy_2 <- ggplot(risk_score_lasso_tidy %>% filter(term != "(Intercept)")) +
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
  scale_y_discrete(labels = rev(c("Small businesses have too much power", "Fair mine videos", "Fair mine resumes", "Republican",
                                  "Can't trust tech corps to do right", "Acceptable mine videos", "Acceptable soc media differentially experiment sentiments",
                                  "Acceptable mine resumes", "Finance doesn't have enough power", "Never rel attendance", "Effective video mining",
                                  "Has changed pol views because soc media", "Soc media important express pol opinions", "Most Fb posts relevant/interesting",
                                  "Energy industry doesn't have enough power", "Acceptable soc media differentially remind vote", 
                                  "Hasn't looked info protests soc media", "Acceptable soc media use data recommend friends",
                                  "Soc media content makes feel lonely", "Yt videos important to pass time", "Hasn't taken part group for interest/cause",
                                  "Tech corps more good than bad personal impact", "Tech corps don't benefit privileged", 
                                  "Acceptable soc media use data political adds", "Yt videos important decide what buy", 
                                  "Hasn't adjusted Fb privacy", "Hasn't used pol hashtag", "Much content see soc media about race",
                                  "$30-$74,999 family income", "Much content see soc media about immigration", "Unacceptable soc media use data recommend friends",
                                  "Soc media doesn't help give voice underrepresented", "Has looked info protests soc media", "Has (un)friended influence Newsfeed",
                                  "Tech corps don't protect data", "Farming and agriculture have too much power", "Tech corps more bad than good personal impact",
                                  "No response encountered Yt content unsuitable children", "Soc media unimportant get attention elected officials",
                                  "Unacceptable soc media use data political adds", "See little content soc media about gun control/violence",
                                  "Very liberal", "Hasn't liked, shared or commented influence Newsfeed", "Democrat", "Unfair mine videos",
                                  "Uses Yt several times day", "Northeast"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Model fitted excluding effectiveness of response as predictor.") # 670 x 550

pdf("risk_score_lasso_tidy_2.pdf")

print(risk_score_lasso_tidy_2)

dev.off()

vid_hiring_lasso <- glmnet(fair_vid_hiring_bin ~ ., data = data_vid_hiring_train, alpha = 1, lambda = 0.01493088, family = "binomial")

vid_hiring_lasso_tidy <- tidy(vid_hiring_lasso, return_zeros = FALSE)

vid_hiring_lasso_tidy <- vid_hiring_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "algo", "algo", "media", "algo", "tech", "media", "dem", "media", "dem", "pol", "media", "algo",
                          "dem", "media", "dem", "algo", "pol", "dem", "dem", "dem", "media", "pol", "tech", "media", "media",
                          "media", "dem", "media", "media", "media", "dem", "tech", "media", "tech", "media", "media", "tech",
                          "media", "media", "media", "dem", "tech", "media", "algo", "tech", "dem", "algo", NA))

overlap <- c(rep("yes", 4), "no", "no", "yes", "no", "yes", "yes", "no", "yes", "yes", "yes", "no", "yes", rep("no", 5), "yes", "yes", 'yes',
             rep("no", 4), "yes", rep("no", 4), "yes", "yes", rep("no", 8), "yes", "yes", "no", "no", "yes", "yes")

vid_hiring_lasso_tidy_2 <- ggplot(vid_hiring_lasso_tidy %>% filter(term != "(Intercept)")) +
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
  scale_y_discrete(labels = rev(c("Fair finance score", "Fair risk score", "Acceptable risk score", "Acceptable soc media differentially experiment sentiments",
                                  "Effective finance score", "Tech corps should be less regulated", "Soc media content makes feel connected",
                                  "Male", "Users lots control over Newsfeed", "Rel attendance once or twice a month",
                                  "Very conservative", "Acceptable soc media use data advertise",
                                  "Acceptable finance score", "$75,000 + family income", "Little content soc media about gun control/violence",
                                  "South", "Effective risk score", "Unions have too much power", "Rel attendance more than once a week",
                                  "65+ years old", "Married or living with a partner", "Acceptable soc media differentially remind vote", 
                                  "Tech corps have right amount of power", "Tech corps more ethical than others", "Easy for soc media to figure rel based data",
                                  "Soc media content makes angry", "Soc media content mix of supportive and mean", "High school or less", 
                                  "Has adjusted Fb privacy last year", "Users no control over Newsfeed content", "Has liked, shared or comment influence Newsfeed",
                                  "West", "Tech corps often fail anticipate soc impact", "Has deleted Fb app phone last year",
                                  "Tech corps have more bad than good soc impact", "Few Fb posts relevant/interesting", 
                                  "Soc media doesn't highlight issues that would't get attention",
                                  "Programs are biased", "Not likely soc media intentionally censor", "Not easy for soc media to figure rel based data",
                                  "Indicated want to see on Newsfeed", "Other religion", "Can't trust tech corps do what's right", 
                                  "Soc media contents don't provide accurate picture society", "Ineffective finance score", "Tech corps don't protect data",
                                  "50-64 years old", "Unfair risk score"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Model fitted excluding effectiveness of response as predictor.") # 670 x 550

pdf("vid_hiring_lasso_tidy_2.pdf")

print(vid_hiring_lasso_tidy_2)

dev.off()

cv_hiring_lasso <- glmnet(fair_cv_hiring_bin ~ ., data = data_cv_hiring_train, alpha = 1, lambda = 0.01900601, family = "binomial")

cv_hiring_lasso_tidy <- tidy(cv_hiring_lasso, return_zeros = FALSE)

cv_hiring_lasso_tidy <- cv_hiring_lasso_tidy %>% 
  arrange(estimate) %>% 
  add_column(category = c("algo", "pol", "algo", "algo", "media", "media", "media", "algo", "media", "algo", "media", "media", "tech", "tech",
                          "tech", "pol", "tech", "media", "tech", "algo", "media", "dem", "tech", "pol", "media", "tech", "tech", "algo", "tech",
                          NA))

overlap <- c("yes", "no", rep("yes", 6), "no", "no", "yes", "yes", "yes", rep("no", 8), "yes", "no", "yes", "yes", "no", "no", "yes", "yes")

cv_hiring_lasso_tidy_2 <- ggplot(cv_hiring_lasso_tidy %>% filter(term != "(Intercept)")) +
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
  scale_y_discrete(labels = rev(c("Acceptable finance score", "Advertisers don't have enough power", "Effective risk score", "Fair risk score",
                                  "Acceptable soc media differentially experiment sentiments", "Acceptable soc media differentially change site",
                                  "Acceptable soc media use data advertise", "Acceptable risk score", "No response soc media content accurate picture society",
                                  "Fair finance score", "Hasn't changed privacy/ad preferences influence Newsfeed",
                                  "Hasn't liked, shared or commented influence Newsfeed", "Tech corps protect data", "Programs are unbiased",
                                  "Tech corps should be less regulated", "Energy industry has about right amount power", 
                                  "Tech corps more good than bad pers impact", "Soc media content makes feel depressed", 
                                  "Tech corps should be more regulated", "Ineffective finance score", "Users no control over content Newsfeed", "Non-white",
                                  "Programs are biased", "Advertisers too much power and influence",  "Soc media content makes angry",
                                  "Tech corps more bad than good pers impact", "No response how much tech corps regulated", "Ineffective risk score",
                                  "Tech corps have more bad than good soc impact"))) +
  labs(caption="Only coefficients different than zero displayed. Intercept not displayed.
       Model fitted excluding effectiveness of response as predictor.") # 670 x 550

pdf("cv_hiring_lasso_tidy_2.pdf")

print(cv_hiring_lasso_tidy_2)

dev.off()
