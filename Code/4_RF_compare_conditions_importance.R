rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(irr)
library(cowplot)
library(randomForest)
library(doParallel)
library(vip)
library(pdp)

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


# Fit LASSO models
dat_pos_1 <- allDat %>% 
  filter(condition==1) %>%
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(avg_rating, contains("AU"))
fit_pos_1 <- randomForest(x = dat_pos_1[,-1],
                          y = dat_pos_1[,1],
                          ntree = 500,
                          replace = T, 
                          keep.forest = T)

dat_pos_2 <- allDat %>% 
  filter(condition==2) %>%
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(avg_rating, contains("AU"))
fit_pos_2 <- randomForest(x = dat_pos_2[,-1],
                          y = dat_pos_2[,1],
                          ntree = 500,
                          replace = T, 
                          keep.forest = T)

dat_pos_3 <- allDat %>% 
  filter(condition==3) %>%
  mutate(avg_rating = (rater1_posScore + rater2_posScore + rater3_posScore)/3) %>%
  select(avg_rating, contains("AU")) %>% 
  na.omit()
fit_pos_3 <- randomForest(x = dat_pos_3[,-1],
                          y = dat_pos_3[,1],
                          ntree = 500,
                          replace = T, 
                          keep.forest = T)

dat_neg_1 <- allDat %>% 
  filter(condition==1) %>%
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(avg_rating, contains("AU"))
fit_neg_1 <- randomForest(x = dat_neg_1[,-1],
                          y = dat_neg_1[,1],
                          ntree = 500,
                          replace = T, 
                          keep.forest = T)

dat_neg_2 <- allDat %>% 
  filter(condition==2) %>%
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(avg_rating, contains("AU"))
fit_neg_2 <- randomForest(x = dat_neg_2[,-1],
                          y = dat_neg_2[,1],
                          ntree = 500,
                          replace = T, 
                          keep.forest = T)

dat_neg_3 <- allDat %>% 
  filter(condition==3) %>%
  mutate(avg_rating = (rater1_negScore + rater2_negScore + rater3_negScore)/3) %>%
  select(avg_rating, contains("AU")) %>% 
  na.omit()
fit_neg_3 <- randomForest(x = dat_neg_3[,-1],
                          y = dat_neg_3[,1],
                          ntree = 500,
                          replace = T, 
                          keep.forest = T)


registerDoParallel(cores = 3)
pdp_pos_1 <- vi(fit_pos_1, method = "pdp", train = dat_pos_1[,-1], parallel = T, sort = F)
pdp_pos_2 <- vi(fit_pos_2, method = "pdp", train = dat_pos_2[,-1], parallel = T, sort = F)
pdp_pos_3 <- vi(fit_pos_3, method = "pdp", train = dat_pos_3[,-1], parallel = T, sort = F)
pdp_neg_1 <- vi(fit_neg_1, method = "pdp", train = dat_neg_1[,-1], parallel = T, sort = F)
pdp_neg_2 <- vi(fit_neg_2, method = "pdp", train = dat_neg_2[,-1], parallel = T, sort = F)
pdp_neg_3 <- vi(fit_neg_3, method = "pdp", train = dat_neg_3[,-1], parallel = T, sort = F)
stopImplicitCluster()


# Create plotting data.frames
plot_pos <- data.frame(AU = gsub(x = gsub(x = names(dat_pos_1)[-1], 
                                          pattern = "overall",
                                          replacement = ""), "AU", ""),
                       Enhance = pdp_pos_1$Importance / sum(pdp_pos_1$Importance),
                       Normal = pdp_pos_2$Importance / sum(pdp_pos_2$Importance),
                       Suppress = pdp_pos_3$Importance / sum(pdp_pos_3$Importance)) %>% 
  gather(key = Condition, value = PDP, 2:4)

plot_neg <- data.frame(AU = gsub(x = gsub(x = names(dat_neg_1)[-1], 
                                          pattern = "overall",
                                          replacement = ""), "AU", ""),
                       Enhance = pdp_neg_1$Importance / sum(pdp_neg_1$Importance),
                       Normal = pdp_neg_2$Importance / sum(pdp_neg_2$Importance),
                       Suppress = pdp_neg_3$Importance / sum(pdp_neg_3$Importance)) %>% 
  gather(key = Condition, value = PDP, 2:4)

# Plot importance
p1 <- ggplot(plot_pos, aes(y = PDP, x = reorder(AU, PDP), group = Condition, fill = Condition)) +
  geom_bar(stat = "identity",width = 0.7, position = position_dodge(width=0.8)) +
  scale_fill_brewer(type = "seq", palette = 4) +
  ylim(0,0.4) + 
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none")
p2 <- ggplot(plot_neg, aes(y = PDP, x = reorder(AU, PDP), group = Condition, fill = Condition)) +
  geom_bar(stat = "identity",width = 0.7, position = position_dodge(width=0.8)) +
  scale_fill_brewer(type = "seq", palette = 4) +
  ylim(0,0.4) + 
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none")
imp_cond <- plot_grid(p1,p2) 

ggsave(imp_cond, filename = "imp_cond_fig.pdf", 
       dpi = 300, units = "in", height = 6, width = 9)

# Compute ICCs
icc(plot_pos %>% 
      spread(key = Condition, value = PDP) %>%
      select(-AU), 
    type = "agreement", 
    unit = "average")
icc(plot_neg %>% 
      spread(key = Condition, value = PDP) %>%
      select(-AU), 
    type = "agreement",
    unit = "average")

## Differences across conditions?
p1 <- partial(fit_neg_1, train = dat_neg_1[,-1], pred.var = "AU9overall", chull = T)
p1 <- autoplot(p1, contour = T) + theme_minimal()
p2 <- partial(fit_neg_2, train = dat_neg_2[,-1], pred.var = "AU9overall", chull = T)
p2 <- autoplot(p2, contour = T) + theme_minimal()
p3 <- partial(fit_neg_3, train = dat_neg_3[,-1], pred.var = "AU9overall", chull = T)
p3 <- autoplot(p3, contour = T) + theme_minimal()
plot_grid(p1, p2, p3, ncol = 3)

