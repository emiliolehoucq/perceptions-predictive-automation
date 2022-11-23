# Cleaning data

# Loading packages --------------------------------------------------------

library(tidyverse)
library(haven)

# Setting parameters ------------------------------------------------------

set.seed(2620)

# Reading data ------------------------------------------------------------

data <- read_sav("data/unprocessed/ATP W35.sav")

# Recoding NAs ------------------------------------------------------------

data[data == 99] <- NA

# Renaming variables ------------------------------------------------------

data <- data %>% 
  rename(key = QKEY,
         comp_prog_biases = ALG1_W35,
         power_tech_corp = ECIMPA_W35,
         power_energy_industry = ECIMPB_W35,
         power_labor_unions = ECIMPC_W35,
         power_finance = ECIMPD_W35,
         power_advertisers = ECIMPE_W35,
         power_pharma = ECIMPF_W35,
         power_farm_agric = ECIMPG_W35,
         power_small_bus = ECIMPH_W35,
         trust_tech_corp = TC2A_W35,
         impact_tech_corp_soc = TC2B_W35,
         impact_tech_corp_pers = TC2C_W35,
         tech_corp_benefit_privileged = TC3A_W35,
         tech_corp_protect_data = TC3B_W35,
         tech_corp_fail_anticipate_impact = TC3C_W35,
         tech_corp_compared_others = TC4_W35,
         regulation_tech_corp = TC5_W35,
         tech_corp_views_gender = TC6A_W35,
         tech_corp_views_pol = TC6B_W35,
         use_fb = SNSA_W35,
         use_twitter = SNSB_W35,
         use_insta = SNSE_W35,
         use_youtube = SNSH_W35,
         use_snapchat = SNSJ_W35,
         use_other_social_media = SNSL_W35,
         use_any_social_media = SNSUSER_W35,
         how_often_fb = SNSFRA_W35,
         how_often_youtube = SNSFRB_W35,
         how_often_media_content_angry = SM1A_W35,
         how_often_media_content_inspired = SM1B_W35,
         how_often_media_content_amused = SM1C_W35,
         how_often_media_content_depressed = SM1D_W35,
         how_often_media_content_connected = SM1E_W35,
         how_often_media_content_lonely = SM1F_W35,
         how_often_dramatic_exag_media = SM2A_W35,
         how_often_deceiving_media = SM2B_W35,
         how_often_useful_media = SM2C_W35,
         how_often_factless_discuss_media = SM2D_W35,
         media_representative_society = SM3_W35,
         easy_media_figure_hobbies_interests = SM4A_W35,
         easy_media_figure_politics = SM4B_W35,
         easy_media_figure_relig = SM4C_W35,
         easy_media_figure_race = SM4D_W35,
         data_acceptable_recommend_events = SM5A_W35,
         data_acceptable_advertise = SM5B_W35,
         data_acceptable_recommend_friend = SM5C_W35,
         data_acceptable_political_add = SM5D_W35,
         common_behavior_social_media_supportive = SM6A_W35,
         common_behavior_social_media_accurate = SM6B_W35,
         likely_media_censor_pol = SM7_W35,
         acceptable_media_diff_change_site = SM8A_W35,
         acceptable_media_diff_remind_vote = SM8B_W35,
         acceptable_media_exper_sentiments = SM8C_W35,
         media_gives_voice_unrep = SM9A_W35,
         media_distracts_people_pol = SM9B_W35,
         media_highlights_important = SM9C_W35,
         media_promotes_accountability_people = SM9D_W35,
         media_overestimates_impact = SM9E_W35,
         change_profile_cause = SM10A_W35,
         use_hashtag_cause = SM10B_W35,
         join_group_cause = SM10C_W35,
         encourage_people_cause = SM10D_W35,
         look_info_protests = SM10E_W35,
         change_pol_view_cuz_meida = SM11_W35,
         imp_media_find_people_shared_views = SM12A_W35,
         imp_media_get_involved = SM12B_W35,
         imp_media_venue_express_pol = SM12C_W35,
         imp_media_getting_politicians_pay_attention = SM13A_W35,
         imp_media_policy_inf = SM13B_W35,
         imp_media_sustained_mov = SM13C_W35,
         content_media_race = SM14A_W35,
         content_media_sex_har = SM14B_W35,
         content_media_gun = SM14C_W35,
         content_media_immigration = SM14D_W35,
         understand_differential_newsfeed = FB1_W35,
         relevance_fb_posts = FB2_W35,
         control_newsfeed = FB3A_W35,
         tried_influence_newsfeed = FB3B_W35,
         influence_newsfeed_friend = FB3C1_W35,
         like_share_comment_influence_newsfeed = FB3C2_W35,
         indicate_influence_newsfeed = FB3C3_W35,
         privacy_influence_newsfeed = FB3C4_W35,
         groups_influence_newsfeed = FB3C5_W35,
         other_influence_newsfeed = FB3C6_W35,
         fb_remind_happy = FB4A_W35,
         fb_remind_sad = FB4B_W35,
         break_fb = FB5A_W35,
         delete_fb_app = FB5B_W35,
         adjust_privacy_fb = FB5C_W35,
         download_data_fb = FB5D_W35,
         imp_yt_understand_world = YT1A_W35,
         impt_yt_learning_things = YT1B_W35,
         impt_yt_decide_buy = YT1C_W35,
         impt_yt_pass_time = YT1D_W35,
         watch_recommended_yt = YT2_W35,
         how_often_fake_yt = YT3A_W35,
         how_often_abusive_yt = YT3B_W35,
         how_often_danger_trouble_yt = YT3C_W35,
         how_often_child_yt = YT4_W35,
         child_found_unsuitable_yt = YT5_W35,
         random_cred_score_risk_score = V1_W35,
         fair_cred_score = V1Q1_W35,
         effective_cred_score = V1Q2_W35,
         acceptable_cred_score = V1Q3_W35,
         cred_score_would_help_find_customers = V1Q4_POS1_W35,
         cred_score_equal_curr_cred_score = V1Q4_POS2_W35,
         cred_score_people_know_give_data = V1Q4_POS3_W35,
         cred_score_free_market = V1Q4_POS4_W35,
         cred_score_if_info_can_corrected = V1Q4_POS5_W35,
         cred_score_unfair_discr = V1Q4_NEG1_W35,
         cred_score_viol_privacy = V1Q4_NEG2_W35,
         cred_score_opaque_inacurate = V1Q4_NEG3_W35,
         cred_score_no_creditworthiness = V1Q4_NEG4_W35,
         cred_score_imposs_to_correct = V1Q4_NEG5_W35,
         fair_risk_score = V2Q1_W35,
         effective_risk_score = V2Q2_W35,
         acceptable_risk_score = V2Q3_W35,
         risk_score_fair_unbiased = V2Q4_POS1_W35,
         risk_score_good_protection = V2Q4_POS2_W35,
         risk_score_improve_current_sys = V2Q4_POS3_W35,
         risk_score_one_tool = V2Q4_POS4_W35,
         risk_score_gives_more_info = V2Q4_POS5_W35,
         risk_score_not_individuality = V2Q4_NEG1_W35,
         risk_score_needs_human = V2Q4_NEG2_W35,
         risk_score_bias_profiling = V2Q4_NEG3_W35,
         risk_score_change_not_current = V2Q4_NEG4_W35,
         risk_score_viol_privacy = V2Q4_NEG5_W35,
         random_hiring = V2_W35,
         fair_vid_hiring = V3Q1_W35,
         effective_vid_hiring = V3Q2_W35,
         acceptable_vid_hiring = V3Q3_W35,
         vid_hiring_free_market = V3Q4_POS1_W35,
         vid_hiring_part_interview = V3Q4_POS2_W35,
         vid_hiring_with_consent = V3Q4_POS3_W35,
         vid_hiring_more_objective = V3Q4_POS4_W35,
         vid_hiring_diff_interv_skills = V3Q4_NEG1_W35,
         vid_hiring_weird_uncomf = V3Q4_NEG2_W35,
         vid_hiring_unfair = V3Q4_NEG3_W35,
         vid_hiring_humanity = V3Q4_NEG4_W35,
         vid_hiring_flawed = V3Q4_NEG5_W35,
         fair_cv_hiring = V4Q1_W35,
         effective_cv_hiring = V4Q2_W35,
         acceptable_cv_hiring = V4Q3_W35,
         cv_hiring_free_market = V4Q4_POS1_W35,
         cv_hiring_unbiased = V4Q4_POS2_W35,
         cv_hiring_saves_time = V4Q4_POS3_W35,
         cv_hiring_more_accurate = V4Q4_POS4_W35,
         cv_hiring_part_process = V4Q4_POS5_W35,
         cv_hiring_unfair_not_best_person = V4Q4_NEG1_W35,
         cv_hiring_humanity_character = V4Q4_NEG2_W35,
         cv_hiring_cv_are_bad = V4Q4_NEG3_W35,
         census_region = F_CREGION_FINAL,
         age_cat = F_AGECAT_FINAL,
         sex = F_SEX_FINAL,
         edu_level = F_EDUCCAT_FINAL,
         edu_level_2 = F_EDUCCAT2_FINAL,
         hispanic = F_HISP_RECRUITMENT,
         race = F_RACECMB_RECRUITMENT,
         citizenship = F_CITIZEN_RECODE_FINAL,
         marital_status = F_MARITAL_FINAL,
         religion = F_RELIG_FINAL,
         rel_serv_attendance = F_ATTEND_FINAL,
         pol_party = F_PARTY_FINAL,
         family_income = F_INCOME_FINAL,
         family_income_2 = F_INCOME_RECODE_FINAL,
         registered_vote = F_REG_FINAL,
         ideology = F_IDEO_FINAL,
         household_internet = F_INTUSER_FINAL,
         volunteer = F_VOLSUM_FINAL,
         weight = WEIGHT_W35)

subset_data <- data %>% # I'm only keeping the variables I renamed
  select(matches("^[a-z]", ignore.case = FALSE))

# Recoding to factors -----------------------------------------------------
# keep weight and key as numeric

for (i in seq_along(subset_data)) {
  
  subset_data[[i]] <- factor(subset_data[[i]])
  
}

subset_data$weight <- as.numeric(subset_data$weight)
subset_data$key <- as.numeric(subset_data$key)

# Recoding factors --------------------------------------------------------
# Create a binary version: 
# TC2a, SM1, SM2, SM4, SM5, SM7, SM9, SM12, SM13, SM14, FB1, FB2, FB4, YT1, YT3
# V1Q1, V1Q2, V2Q1, V2Q2, V3Q1, V3Q2, V4Q1, V4Q2

binarize <- c("trust_tech_corp",
              "how_often_media_content_angry",
              "how_often_media_content_inspired",
              "how_often_media_content_amused",
              "how_often_media_content_depressed",
              "how_often_media_content_connected",
              "how_often_media_content_lonely",
              "how_often_dramatic_exag_media",
              "how_often_deceiving_media",
              "how_often_useful_media",
              "how_often_factless_discuss_media",
              "easy_media_figure_hobbies_interests",
              "easy_media_figure_politics",
              "easy_media_figure_relig",
              "easy_media_figure_race",
              "data_acceptable_recommend_events",
              "data_acceptable_advertise",
              "data_acceptable_recommend_friend",
              "data_acceptable_political_add",
              "likely_media_censor_pol",
              "media_gives_voice_unrep",
              "media_distracts_people_pol",
              "media_highlights_important",
              "media_promotes_accountability_people",
              "media_overestimates_impact",
              "imp_media_find_people_shared_views",
              "imp_media_get_involved",
              "imp_media_venue_express_pol",
              "imp_media_getting_politicians_pay_attention",
              "imp_media_policy_inf",
              "imp_media_sustained_mov",
              "content_media_race",
              "content_media_sex_har",
              "content_media_gun",
              "content_media_immigration",
              "understand_differential_newsfeed",
              "relevance_fb_posts",
              "fb_remind_happy",
              "fb_remind_sad",
              "imp_yt_understand_world",
              "impt_yt_learning_things",
              "impt_yt_decide_buy",
              "impt_yt_pass_time",
              "watch_recommended_yt",
              "how_often_fake_yt",
              "how_often_abusive_yt",
              "how_often_danger_trouble_yt",
              "fair_cred_score",
              "effective_cred_score",
              "fair_risk_score",
              "effective_risk_score",
              "fair_vid_hiring",
              "effective_vid_hiring",
              "fair_cv_hiring",
              "effective_cv_hiring")

temporary <- c(rep("_bin", 55))

new_names <- paste(binarize, temporary, sep = "")

for (i in 1:55) {
  
  subset_data[[new_names[i]]] <-  fct_collapse(subset_data[[binarize[i]]], "0" = c("1", "2"), "1" = c("3", "4"))
  
}

# Drop option 4 from YT4_W35

subset_data$how_often_child_yt <- ifelse(subset_data$how_often_child_yt == "4", NA, subset_data$how_often_child_yt)

# Choosing among factors ---------------------------------------------------

binaries <- subset_data %>% 
  select(key, ends_with("_bin")) %>% 
  select(-c(how_often_dramatic_exag_media_bin, 
            how_often_factless_discuss_media_bin, 
            easy_media_figure_race_bin, 
            fb_remind_happy_bin, 
            impt_yt_learning_things_bin, 
            watch_recommended_yt_bin))

binarize <- c("trust_tech_corp",
              "how_often_media_content_angry",
              "how_often_media_content_inspired",
              "how_often_media_content_amused",
              "how_often_media_content_depressed",
              "how_often_media_content_connected",
              "how_often_media_content_lonely",
              "how_often_dramatic_exag_media",
              "how_often_deceiving_media",
              "how_often_useful_media",
              "how_often_factless_discuss_media",
              "easy_media_figure_hobbies_interests",
              "easy_media_figure_politics",
              "easy_media_figure_relig",
              "easy_media_figure_race",
              "data_acceptable_recommend_events",
              "data_acceptable_advertise",
              "data_acceptable_recommend_friend",
              "data_acceptable_political_add",
              "likely_media_censor_pol",
              "media_gives_voice_unrep",
              "media_distracts_people_pol",
              "media_highlights_important",
              "media_promotes_accountability_people",
              "media_overestimates_impact",
              "imp_media_find_people_shared_views",
              "imp_media_get_involved",
              "imp_media_venue_express_pol",
              "imp_media_getting_politicians_pay_attention",
              "imp_media_policy_inf",
              "imp_media_sustained_mov",
              "content_media_race",
              "content_media_sex_har",
              "content_media_gun",
              "content_media_immigration",
              "understand_differential_newsfeed",
              "relevance_fb_posts",
              "fb_remind_happy",
              "fb_remind_sad",
              "imp_yt_understand_world",
              "impt_yt_learning_things",
              "impt_yt_decide_buy",
              "impt_yt_pass_time",
              "watch_recommended_yt",
              "how_often_fake_yt",
              "how_often_abusive_yt",
              "how_often_danger_trouble_yt",
              "fair_cred_score",
              "effective_cred_score",
              "fair_risk_score",
              "effective_risk_score",
              "fair_vid_hiring",
              "effective_vid_hiring",
              "fair_cv_hiring",
              "effective_cv_hiring")

without_binaries <- subset_data[, !(names(subset_data) %in% binarize)]

without_binaries <- without_binaries %>% 
  select(- ends_with("_bin"))

clean_data <- left_join(without_binaries, binaries, by = "key")

# Dealing with demographics -----------------------------------------------

clean_data <- clean_data %>% 
  select(- c(citizenship, hispanic, registered_vote, household_internet))

# Pending: recode race, marital_status, religion, religious attendance, pol_party (?), ideology

# Dealing with justifications about automation ----------------------------

without_justifications <- clean_data %>% 
  select(- starts_with("cred_score")) %>% 
  select(- starts_with("risk_score")) %>% 
  select(- starts_with("cv_hiring")) %>% 
  select(- starts_with("vid_hiring"))

# Dealing with demographics 2 ---------------------------------------------

without_justifications <- without_justifications %>% 
  select(- c(edu_level_2, family_income)) %>% 
  mutate(marital_status =  fct_collapse(marital_status, 
                                        "1" = c("1", "2"),
                                        "2" = c("3", "4", "5"),
                                        "3" = "6"),
         religion = fct_collapse(religion,
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = c("3", "4", "5", "6", "7", "8", "11"),
                                 "4" = c("9", "10"),
                                 "5" = "12"),
         race = fct_collapse(race,
                             "1" = "1",
                             "2" = c("2", "3", "4", "5"),
                             "NA" = "9"))

# rel_serv_attendance, ideology, and pol_party have enough observations per category (at least 400 or so)

# Reordering dataset ------------------------------------------------------

without_justifications <- without_justifications %>% 
  select(-c(census_region, key, weight, random_cred_score_risk_score, random_hiring), 
         c(census_region, key, weight)) # the two "random" are not useful

without_justifications <- without_justifications %>% 
  select(c(fair_cred_score_bin, effective_cred_score_bin, 
           fair_risk_score_bin, effective_risk_score_bin, 
           fair_vid_hiring_bin, effective_vid_hiring_bin, 
           fair_cv_hiring_bin, effective_cv_hiring_bin, 
           power_tech_corp, power_finance, power_labor_unions, power_energy_industry),
         everything())


# Dropping how_often_child_yt ---------------------------------------------

without_justifications <- without_justifications %>% 
  select(-c(how_often_child_yt, key, weight)) # poses problems later for sparsity

# Dropping NAs on responses -----------------------------------------------

data_cred_score <- without_justifications %>% 
  filter(!is.na(fair_cred_score_bin))

data_risk_score <- without_justifications %>% 
  filter(!is.na(fair_risk_score_bin))

data_vid_hiring <- without_justifications %>% 
  filter(!is.na(fair_vid_hiring_bin))

data_cv_hiring <- without_justifications %>% 
  filter(!is.na(fair_cv_hiring_bin))

# Imputing NAs ------------------------------------------------------------

for (i in seq_along(data_cred_score)) {
  
    data_cred_score[[i]] <- factor(data_cred_score[[i]], levels = c(levels(data_cred_score[[i]]), "No response"))
    data_cred_score[[i]][is.na(data_cred_score[[i]])] <- "No response"
    
}

for (i in seq_along(data_risk_score)) {
  
  data_risk_score[[i]] <- factor(data_risk_score[[i]], levels = c(levels(data_risk_score[[i]]), "No response"))
  data_risk_score[[i]][is.na(data_risk_score[[i]])] <- "No response"
  
}

for (i in seq_along(data_vid_hiring)) {
  
  data_vid_hiring[[i]] <- factor(data_vid_hiring[[i]], levels = c(levels(data_vid_hiring[[i]]), "No response"))
  data_vid_hiring[[i]][is.na(data_vid_hiring[[i]])] <- "No response"
  
}

for (i in seq_along(data_cv_hiring)) {
  
  data_cv_hiring[[i]] <- factor(data_cv_hiring[[i]], levels = c(levels(data_cv_hiring[[i]]), "No response"))
  data_cv_hiring[[i]][is.na(data_cv_hiring[[i]])] <- "No response"
  
}

# Dropping variables that are all "No response" ---------------------------

data_cred_score <- data_cred_score %>% 
  select(-c(fair_risk_score_bin, effective_risk_score_bin, acceptable_risk_score))

data_risk_score <- data_risk_score %>% 
  select(-c(fair_cred_score_bin, effective_cred_score_bin, acceptable_cred_score))

data_vid_hiring <- data_vid_hiring %>% 
  select(-c(fair_cv_hiring_bin, effective_cv_hiring_bin, acceptable_cv_hiring))

data_cv_hiring <- data_cv_hiring %>% 
  select(-c(fair_vid_hiring_bin, effective_vid_hiring_bin, acceptable_vid_hiring))

# Dropping "No response" level in variables with no "No response" ---------

for (i in seq_along(data_cred_score)) {
  
  data_cred_score[[i]] <- droplevels(data_cred_score[[i]])
  
}

for (i in seq_along(data_risk_score)) {
  
  data_risk_score[[i]] <- droplevels(data_risk_score[[i]])
  
}

for (i in seq_along(data_vid_hiring)) {
  
  data_vid_hiring[[i]] <- droplevels(data_vid_hiring[[i]])
  
}

for (i in seq_along(data_cv_hiring)) {
  
  data_cv_hiring[[i]] <- droplevels(data_cv_hiring[[i]])
  
}

# Partitioning data -------------------------------------------------------

data_cred_score_train <- data_cred_score %>% sample_frac(0.8)

data_cred_score_test <- data_cred_score %>% setdiff(data_cred_score_train)

data_risk_score_train <- data_risk_score %>% sample_frac(0.8)

data_risk_score_test <- data_risk_score %>% setdiff(data_risk_score_train)

data_vid_hiring_train <- data_vid_hiring %>% sample_frac(0.8)

data_vid_hiring_test <- data_vid_hiring %>% setdiff(data_vid_hiring_train)

data_cv_hiring_train <- data_cv_hiring %>% sample_frac(0.8)

data_cv_hiring_test <- data_cv_hiring %>% setdiff(data_cv_hiring_train)

# Saving data -------------------------------------------------------------

saveRDS(data_cred_score_train, file = "data_cred_score_train.rds")
saveRDS(data_cred_score_test, file = "data_cred_score_test.rds")
saveRDS(data_risk_score_train, file = "data_risk_score_train.rds")
saveRDS(data_risk_score_test, file = "data_risk_score_test.rds")
saveRDS(data_vid_hiring_train, file = "data_vid_hiring_train.rds")
saveRDS(data_vid_hiring_test, file = "data_vid_hiring_test.rds")
saveRDS(data_cv_hiring_train, file = "data_cv_hiring_train.rds")
saveRDS(data_cv_hiring_test, file = "data_cv_hiring_test.rds")
