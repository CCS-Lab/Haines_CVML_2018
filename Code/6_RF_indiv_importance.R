rm(list=ls())

# Libraries 
library(randomForest) 
library(dplyr)
library(irr)
library(foreach)
library(doParallel)
library(vip)
library(ggplot2)

# Run parameters
random_state <- 43201
cores        <- 3

# Read in and format AU data --------------------------------------------------
# Load scout, nate, or combined data
load("/Data/allDat_1segment_overallQuality3.RData")

# Tidy data
allDat <- allDat %>%
  filter(tossIdx!=1 & rater1_negScore!=8) %>%
  group_by(subjNum) %>%
  mutate(n_clips = n()) %>%
  filter(n_clips>=21) %>%
  ungroup() %>%
  select(subjNum, contains("rater"), contains("AU")) %>%
  na.omit()

# Fit downsampled data models, extract partial dependence, correlate with full model
results <- foreach(coder=1:3, .combine = "rbind") %do% {
  foreach(emo=c("pos", "neg"), .combine = "rbind") %do% {
    tmp_dat <- allDat %>%
      select(contains(paste0("rater", coder, "_", emo, "Score")), contains("AU"))
    # "True" comparison model
    true_fit <- randomForest(x = tmp_dat[,-1], y = tmp_dat[[1]],
                             ntree = 500, replace = T, keep.forest = T)
    # "True" partial dependence metrics
    registerDoParallel(cores = cores)
    true_pdp <- vi(true_fit, method = "pdp", train = tmp_dat[,-1], parallel = T, sort = F)
    stopImplicitCluster()
    
    data.frame(coder = rep(coder, 20),
               emo   = rep(emo, 20),
               AU    = true_pdp$Variable,
               PDP   = true_pdp$Importance)
  }
}


icc(data.frame(results$PDP[results$coder==1 & results$emo=="pos"],
               results$PDP[results$coder==2 & results$emo=="pos"],
               results$PDP[results$coder==3 & results$emo=="pos"]), 
    unit = "average", type = "agreement")
icc(data.frame(results$PDP[results$coder==1 & results$emo=="neg"],
               results$PDP[results$coder==2 & results$emo=="neg"],
               results$PDP[results$coder==3 & results$emo=="neg"]), 
    unit = "average", type = "agreement")

results2 <- results %>%
  arrange(coder, emo, PDP) %>%
  mutate(order = row_number())

indiv_plot <- ggplot(results2, aes(x = order, y = PDP, fill = emo)) +
  geom_bar(stat = "identity") +
  facet_wrap(c("coder", "emo"), ncol = 2, scales = "free") +
  theme_minimal(base_size = 15) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("orange", "blue")) +
  scale_x_continuous(breaks = results2$order,
                     labels = gsub(x = gsub(x = results2$AU, 
                                            pattern = "overall$", 
                                            replacement = "\\1"),
                                   pattern = "AU",
                                   replacement = ""),
                     expand = c(0,0)) +
  xlab("") +
  ylab("") +
  coord_flip()

ggsave(indiv_plot, filename = "indiv_fig.pdf", 
       units = "in", height = 10, width = 7)
