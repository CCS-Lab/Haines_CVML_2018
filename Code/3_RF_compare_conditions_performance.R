rm(list=ls())

library(ggplot2)
library(dplyr)
library(easyml)
library(irr)
library(cowplot)

# Load scout, nate, or combined data
load("/Data/allDat_1segment_overallQuality3.RData")

# Remove low wality clips (face detected less than 10% of time)
allDat <- subset(allDat, allDat$tossIdx!=1)
allDat <- allDat[-which(allDat$rater1_negScore==8),]
# Remove data from subjects with less than 50% useable clips
low    <- which(table(allDat$subjNum)<21)
allDat <- allDat[-c(which(allDat$subjNum %in% names(low))), ]

# Set random seed
random_state <- 43201

# Fit RF models
dat_pos_1 <- allDat %>% 
  filter(condition==1) %>%
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(subjNum, avg_rating, contains("AU"))
fit_pos_1 <- easy_random_forest(dat_pos_1 %>% select(-subjNum), "avg_rating",
                         family = "gaussian", n_iterations = 10, n_samples = 1000, 
                         n_divisions = 1000, n_core = 20, measure = measure_correlation_score, 
                         resample = resample_stratified_simple_train_test_split, 
                         foldid = dat_pos_1$subjNum, random_state = random_state)

dat_pos_2 <- allDat %>% 
  filter(condition==2) %>%
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(subjNum, avg_rating, contains("AU"))
fit_pos_2 <- easy_random_forest(dat_pos_2 %>% select(-subjNum), "avg_rating",
                         family = "gaussian", n_iterations = 10, n_samples = 1000, 
                         n_divisions = 1000, n_core = 20, measure = measure_correlation_score, 
                         resample = resample_stratified_simple_train_test_split, 
                         foldid = dat_pos_2$subjNum, random_state = random_state)

dat_pos_3 <- allDat %>% 
  filter(condition==3) %>%
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(subjNum, avg_rating, contains("AU")) %>% 
  na.omit()
fit_pos_3 <- easy_random_forest(dat_pos_3 %>% select(-subjNum), "avg_rating",
                         family = "gaussian", n_iterations = 10, n_samples = 1000, 
                         n_divisions = 1000, n_core = 20, measure = measure_correlation_score, 
                         resample = resample_stratified_simple_train_test_split, 
                         foldid = dat_pos_3$subjNum, random_state = random_state)

dat_neg_1 <- allDat %>% 
  filter(condition==1) %>%
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(subjNum, avg_rating, contains("AU"))
fit_neg_1 <- easy_random_forest(dat_neg_1 %>% select(-subjNum), "avg_rating",
                         family = "gaussian", n_iterations = 10, n_samples = 1000, 
                         n_divisions = 1000, n_core = 20, measure = measure_correlation_score, 
                         resample = resample_stratified_simple_train_test_split, 
                         foldid = dat_neg_1$subjNum, random_state = random_state)

dat_neg_2 <- allDat %>% 
  filter(condition==2) %>%
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(subjNum, avg_rating, contains("AU"))
fit_neg_2 <- easy_random_forest(dat_neg_2 %>% select(-subjNum), "avg_rating",
                         family = "gaussian", n_iterations = 10, n_samples = 1000, 
                         n_divisions = 1000, n_core = 20, measure = measure_correlation_score, 
                         resample = resample_stratified_simple_train_test_split, 
                         foldid = dat_neg_2$subjNum, random_state = random_state)

dat_neg_3 <- allDat %>% 
  filter(condition==3) %>%
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(subjNum, avg_rating, contains("AU")) %>% 
  na.omit()
fit_neg_3 <- easy_random_forest(dat_neg_3 %>% select(-subjNum), "avg_rating",
                         family = "gaussian", n_iterations = 10, n_samples = 1000, 
                         n_divisions = 1000, n_core = 20, measure = measure_correlation_score, 
                         resample = resample_stratified_simple_train_test_split, 
                         foldid = dat_neg_3$subjNum, random_state = random_state)

plot_pos <- data.frame(AU = gsub(x = fit_pos_1$variable_importances_processed$predictor, 
                                 pattern = "overall",
                                 replacement = ""),
                       Express = fit_pos_1$variable_importances_processed$mean / sum(fit_pos_1$variable_importances_processed$mean),
                       Normal = fit_pos_2$variable_importances_processed$mean / sum(fit_pos_2$variable_importances_processed$mean),
                       Suppress = fit_pos_3$variable_importances_processed$mean / sum(fit_pos_3$variable_importances_processed$mean)) %>% 
  tidyr::gather(key = Condition, value = Gini, 2:4)

plot_neg <- data.frame(AU = gsub(x = fit_neg_1$variable_importances_processed$predictor, 
                                 pattern = "overall",
                                 replacement = ""),
                       Express = fit_neg_1$variable_importances_processed$mean / sum(fit_neg_1$variable_importances_processed$mean),
                       Normal = fit_neg_2$variable_importances_processed$mean/ sum(fit_neg_2$variable_importances_processed$mean),
                       Suppress = fit_neg_3$variable_importances_processed$mean/ sum(fit_neg_3$variable_importances_processed$mean)) %>% 
  tidyr::gather(key = Condition, value = Gini, 2:4)

# Plot performance
p1 <- ggplot(plot_pos, aes(x = reorder(AU, Gini), y = Gini, group = Condition, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  coord_flip() +     
  theme_minimal()
p2 <- ggplot(plot_neg, aes(x = reorder(AU, Gini), y = Gini, group = Condition, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  coord_flip() +     
  theme_minimal()
cowplot::plot_grid(p1,p2)

# Compute ICCs
icc(plot_pos %>% 
      tidyr::spread(key = Condition, value = Gini) %>%
      select(-AU), 
    type = "agreement", 
    unit = "average")
icc(plot_neg %>% 
      tidyr::spread(key = Condition, value = Gini) %>%
      select(-AU), 
    type = "agreement",
    unit = "average")
