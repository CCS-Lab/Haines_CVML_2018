rm(list=ls())

library(dplyr)
library(ggplot2)
library(randomForest)
library(doParallel)
library(foreach)
library(vip)

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
set.seed(random_state)

# Fit models
dat_pos <- allDat %>% 
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(avg_rating, contains("AU")) %>% 
  na.omit()
fit_pos <- randomForest(x = dat_pos[,-1],
                        y = dat_pos[,1],
                        ntree = 500,
                        replace = T, 
                        keep.forest = T)

dat_neg <- allDat %>% 
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(avg_rating, contains("AU")) %>% 
  na.omit()
fit_neg <- randomForest(x = dat_neg[,-1],
                        y = dat_neg[,1],
                        ntree = 500,
                        replace = T, 
                        keep.forest = T)


# Main effects
registerDoParallel(cores = 3)
vip_pos <- vip(fit_pos, method = "pdp", train = dat_pos, parallel = T, num_features = 20)
stopImplicitCluster()
p_imp <- vip_pos$data %>%
  mutate(Variable = gsub(x = Variable, pattern = "overall", replacement = ""),
         Importance = Importance / sum(Importance))
p_imp <- ggplot(p_imp, aes(reorder(Variable, Importance), Importance)) +
  geom_col(width = 0.75, fill = I("orange")) +
  labs(x = "", y = "Relative Importance") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  ylim(0, .32) +
  coord_flip() +
  theme_minimal()

registerDoParallel(cores = 3)
vip_neg <- vip(fit_neg, method = "pdp", train = dat_neg, parallel = T, num_features = 20)
stopImplicitCluster()
n_imp <- vip_neg$data %>%
  mutate(Variable = gsub(x = Variable, pattern = "overall", replacement = ""),
         Importance = Importance / sum(Importance))
n_imp <- ggplot(n_imp, aes(reorder(Variable, Importance), Importance)) +
  geom_col(width = 0.75, fill = I("blue")) +
  labs(x = "", y = "Relative Importance") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  ylim(0, .32) +
  coord_flip() +
  theme_minimal()

# Interaction effects
# Positive
registerDoParallel(cores = 25)
vint_pos <- vint(fit_pos, method = "pdp", train = dat_pos, parallel = T, 
                 feature_names = names(dat_pos)[-1])
stopImplicitCluster()
vint_pos <- vint_pos %>%
  mutate(Variables = gsub(x = Variables, pattern = "overall", replacement = ""),
         Interaction = Interaction / sum(Interaction))
p_int <- ggplot(vint_pos[1:20, ], aes(reorder(Variables, Interaction), Interaction)) +
  geom_col(width = 0.75, fill = I("orange")) +
  labs(x = "", y = "Interaction strength") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  ylim(0, .32) +
  coord_flip() +
  theme_minimal()

# Negative
registerDoParallel(cores = 25)
vint_neg <- vint(fit_neg, method = "pdp", train = dat_neg, parallel = T, 
                 feature_names = names(dat_neg)[-1])
stopImplicitCluster()
vint_neg <- vint_neg %>%
  mutate(Variables = gsub(x = Variables, pattern = "overall", replacement = ""),
         Interaction = Interaction / sum(Interaction))
n_int <- ggplot(vint_neg[1:20, ], aes(reorder(Variables, Interaction), Interaction)) +
  geom_col(width = 0.75, fill = I("blue")) +
  labs(x = "", y = "Interaction strength") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  ylim(0, .32) +
  coord_flip() +
  theme_minimal()

cowplot::plot_grid(p_imp, n_imp,
                   p_int, n_int, ncol = 2)

save.image(file = "/Data/image_rf_int_probe_v6.RData")
