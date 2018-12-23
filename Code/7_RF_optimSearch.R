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
num_samples  <- c(seq(10,100,5), seq(105,140,10), seq(150, 1000, 50),
                  seq(1200, 2000, 200), c(2500, 3000))
num_iters    <- 1:30
cores        <- 25
random_state <- 123

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
    
    # Iterate through subsamples and iterations within samples
    foreach(samp=num_samples, .combine = "rbind") %do% {
      foreach(iter=num_iters, .combine = "rbind") %do% {
        
        # Select randomized rows
        rand_check <- T
        while (rand_check) {
          rand_rows <- sample(1:nrow(tmp_dat), samp, replace = F)
          if (mean(tmp_dat[rand_rows,][[1]]) > 1) {
            rand_check <- F
          }
        }
        X <- tmp_dat[rand_rows,-1]
        y <- tmp_dat[rand_rows,][[1]]
        
        # Random forest model 
        fit <- randomForest(x = X,
                            y = y,
                            ntree = 500,
                            replace = T, 
                            keep.forest = T)
        
        # Partial dependence importance
        registerDoParallel(cores = cores)
        pdp <- vi(fit, method = "pdp", train = X, parallel = T, sort = F)
        stopImplicitCluster()
        
        cat(paste0("Coder: ", coder, "\n",
                   "Valence: ", emo, "\n",
                   "Sample: ", samp, "\n",
                   "Iteration: ", iter, "\n\n"))
        
        data.frame(coder = coder,
                   iter  = iter,
                   samp  = samp,
                   emo   = emo,
                   icc   = icc(data.frame(true_pdp$Importance / sum(true_pdp$Importance),
                                          pdp$Importance / sum(pdp$Importance)), 
                               type = "agreement", unit = "average")$value)
      }
    } 
  }
}

saveRDS(results, file = "/Data/rf_1seg_optimSearch_icc_average_pdp.RData")

plot_dat <- results %>%
  group_by(samp, emo, coder) %>%
  summarize(mean = mean(icc),
            sem  = sd(icc)/sqrt(n()))
optim_plot <- ggplot(plot_dat, aes(x = samp, y = mean)) +
  geom_hline(yintercept = 0.75, linetype = 2, color = "red") +
  geom_line() +
  geom_ribbon(aes(x = samp, ymin = mean - 2*sem, ymax = mean + 2*sem), alpha= 0.2) +
  facet_wrap(c("coder", "emo"), ncol = 2) +
  scale_x_discrete(limits=c(seq(10, 500, 50), seq(600, 2800, 200))) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=65, vjust = 0.85),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlab("") +
  ylab("")

ggsave(optim_plot, filename = "optim_fig.pdf", 
       units = "in", height = 6, width = 14)
