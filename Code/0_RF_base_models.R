rm(list=ls())

# Libraries -------------------------------------------------------------------
library(ggplot2)  # paper-ready figures
library(randomForest) 
library(dplyr)
library(hBayesDM)
library(magrittr)
library(easyml)
library(stringr)
library(irr)

# Run parameters
random_state  <- 123

# Read in and format AU data --------------------------------------------------
# Load scout, nate, or combined data
load("/Data/allDat_1segment_overallQuality3.RData")

# Remove low wality clips (face detected less than 10% of time)
allDat <- subset(allDat, allDat$tossIdx!=1)

# For clips 15 and 25, rater 1 entered 8 and 9 for negative ratings, respectively.
# Given that other coders rated these clips high, I assume that this was a data entry 
# error, and that both these ratings should be 7. Analyses show that removing or setting
# these ratings to 7 does not change model performance (within or across subjects)
allDat <- allDat[-which(allDat$rater1_negScore==8),]
#allDat <- allDat[-which(allDat$rater1_negScore==9),]

# # Remove data from subjects with less than 50% useable clips
low    <- which(table(allDat$subjNum)<21)
allDat <- allDat[-c(which(allDat$subjNum %in% names(low))), ]

# Create matrix for positive human ratings
posMatrix1 <- as.data.frame(cbind(subjID                   = allDat$subjNum,
                                  AVG_goodRaters_posRating = (allDat$rater1_posScore + allDat$rater2_posScore + allDat$rater3_posScore) / 3,
                                  allDat[,grep(x = names(allDat), pattern = "^AU.*")]))
# Create matrix for negative human ratings
negMatrix1 <- as.data.frame(cbind(subjID           = allDat$subjNum,
                                  AVG_goodRaters_negRating = (allDat$rater1_negScore + allDat$rater2_negScore + allDat$rater3_negScore) / 3,
                                  allDat[,grep(x = names(allDat), pattern = "^AU.*")]))

# Remove any rows with NA in coder rating column
posMatrixClean1 <- apply(posMatrix1[!is.na(posMatrix1[,2]),], 2, as.numeric)
negMatrixClean1 <- apply(negMatrix1[!is.na(negMatrix1[,2]),], 2, as.numeric)

# change back to data.frame -- by Young
posMatrixClean1 = as.data.frame(posMatrixClean1)
negMatrixClean1 = as.data.frame(negMatrixClean1)

# Random forest model ---------------------------------------------------------
fitPos <- easy_random_forest(posMatrixClean1[,-1], "AVG_goodRaters_posRating",
                             family = "gaussian", n_iterations = 10, n_samples = 1000, 
                             n_divisions = 1000, n_core = 50, measure = measure_correlation_score, 
                             resample = resample_fold_train_test_split, 
                             foldid = posMatrixClean1[,1], random_state = random_state)
fitPos$plot_metrics_test_mean
cor(fitPos$plot_predictions_test_mean$data$y_true, fitPos$plot_predictions_test_mean$data$y_pred)
# Random forest model ---------------------------------------------------------
fitNeg <- easy_random_forest(negMatrixClean1[,-1], "AVG_goodRaters_negRating",
                             family = "gaussian", n_iterations = 10, n_samples = 1000, 
                             n_divisions = 1000, n_core = 50, measure = measure_correlation_score, 
                             resample = resample_fold_train_test_split, 
                             foldid = negMatrixClean1[,1], random_state = random_state)
fitNeg$plot_metrics_test_mean
cor(fitNeg$plot_predictions_test_mean$data$y_true, fitNeg$plot_predictions_test_mean$data$y_pred)


save(fitPos, file = "/Data/fitPos_1seg_finalData_RF_strat_10iter_1000samp_1000div_ErBars.RData")
save(fitNeg, file = "/Data/fitNeg_1seg_finalData_RF_strat_10iter_1000samp_1000div_ErBars.RData")


### Plotting performance ### 

# Find rows used in training and test data
pos_train_rows <- rownames(posMatrixClean1) %in% rownames(fitPos$X_train)
pos_test_rows  <- rownames(posMatrixClean1) %in% rownames(fitPos$X_test)
neg_train_rows <- rownames(negMatrixClean1) %in% rownames(fitNeg$X_train)
neg_test_rows  <- rownames(negMatrixClean1) %in% rownames(fitNeg$X_test)

# Subject IDs for training and test sets
pos_train_ids <- posMatrixClean1[pos_train_rows, "subjID"]
pos_test_ids  <- posMatrixClean1[pos_test_rows, "subjID"]
neg_train_ids <- negMatrixClean1[neg_train_rows, "subjID"]
neg_test_ids  <- negMatrixClean1[neg_test_rows, "subjID"]

# After using easyml's new stratified resampling (simple) method
pos_train_ids <- str_match(rownames(fitPos$X_train), "([0-9]+)[.][0-9]+")[, 2]
pos_test_ids  <- str_match(rownames(fitPos$X_test), "([0-9]+)[.][0-9]+")[, 2]
neg_train_ids <- str_match(rownames(fitNeg$X_train), "([0-9]+)[.][0-9]+")[, 2]
neg_test_ids  <- str_match(rownames(fitNeg$X_test), "([0-9]+)[.][0-9]+")[, 2]

# Predicted y's
pos_train_pred <- fitPos$plot_predictions_train_mean$data$y_pred
pos_test_pred  <- fitPos$plot_predictions_test_mean$data$y_pred
neg_train_pred <- fitNeg$plot_predictions_train_mean$data$y_pred
neg_test_pred  <- fitNeg$plot_predictions_test_mean$data$y_pred

# True y's 
pos_train_true <- fitPos$plot_predictions_train_mean$data$y_true
pos_test_true  <- fitPos$plot_predictions_test_mean$data$y_true
neg_train_true <- fitNeg$plot_predictions_train_mean$data$y_true
neg_test_true  <- fitNeg$plot_predictions_test_mean$data$y_true

# Create data.frames with subject ID, true y, and predicted y
posTrain <- data.frame(subjID    = as.factor(pos_train_ids),
                       predicted = pos_train_pred,
                       true      = pos_train_true)
posTest  <- data.frame(subjID    = as.factor(pos_test_ids),
                       predicted = pos_test_pred,
                       true      = pos_test_true)
negTrain <- data.frame(subjID    = as.factor(neg_train_ids),
                       predicted = neg_train_pred,
                       true      = neg_train_true)
negTest  <- data.frame(subjID    = as.factor(neg_test_ids),
                       predicted = neg_test_pred,
                       true      = neg_test_true)

# For plotting model performance across iterations ------------------------
# For positive training set
posTrainSplitCor <- data.frame(correlation = fitPos$plot_metrics_train_mean$data$cor_scores)
# For positive test set
posTestSplitCor <- data.frame(correlation = fitPos$plot_metrics_test_mean$data$cor_scores)
# For negative training set
negTrainSplitCor <- data.frame(correlation = fitNeg$plot_metrics_train_mean$data$cor_scores)
# For negative test set
negTestSplitCor <- data.frame(correlation = fitNeg$plot_metrics_test_mean$data$cor_scores)

posTrC <- ggplot(posTrainSplitCor, aes(x=correlation)) +
  geom_histogram(bins=100, color = "orange", fill = "orange") + 
  geom_vline(xintercept = round(mean(posTrainSplitCor$correlation), 2), 
             colour="red", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(0.5, 1, .1)) + 
  coord_cartesian(xlim = c(0.5,1)) + #, ylim = c(0, 17)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")
posTsC <- ggplot(posTestSplitCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "orange", fill = "orange")+ 
  geom_vline(xintercept = round(mean(posTestSplitCor$correlation), 2), 
             colour="red", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(0.5, 1, .1)) + 
  coord_cartesian(xlim = c(0.5,1)) + #, ylim = c(0, 17)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")
negTrC <- ggplot(negTrainSplitCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "blue", fill = "blue")+ 
  geom_vline(xintercept = round(mean(negTrainSplitCor$correlation), 2), 
             colour="red", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(0.5, 1, .1)) + 
  coord_cartesian(xlim = c(0.5,1)) + #, ylim = c(0, 17)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")
negTsC <- ggplot(negTestSplitCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "blue", fill = "blue")+ 
  geom_vline(xintercept = round(mean(negTestSplitCor$correlation), 2), 
             colour="red", linetype = "longdash") +
  scale_x_continuous(breaks = seq(0.5, 1, .1)) + 
  coord_cartesian(xlim = c(0.5,1)) + #, ylim = c(0, 17)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")

# Correlation values
t.test(posTrainSplitCor$correlation)
t.test(posTestSplitCor$correlation)
t.test(negTrainSplitCor$correlation)
t.test(negTestSplitCor$correlation)

plot_grid(posTrC, negTrC,
          posTsC, negTsC,
          ncol = 2)


# For plotting within subject correlation histograms ------------------------
# For positive training set
posTrainCor <- posTrain %>% group_by(subjID) %>% 
  summarise(num.types = n(), 
            correlation = cor(predicted,true)) 
# For positive test set
posTestCor <- posTest %>% group_by(subjID) %>% 
  summarise(num.types = n(), 
            correlation = cor(predicted,true)) 
# For negative training set
negTrainCor <- negTrain %>% group_by(subjID) %>% 
  summarise(num.types = n(), 
            correlation = cor(predicted,true)) 
# For negative test set
negTestCor <- negTest %>% group_by(subjID) %>% 
  summarise(num.types = n(), 
            correlation = cor(predicted,true)) 

posTrC <- ggplot(posTrainCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "orange", fill = "orange") + 
  geom_vline(xintercept = round(median(posTrainCor$correlation, na.rm = T), 2), 
             colour="red", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(-0.3, 1, 0.2)) + 
  coord_cartesian(xlim = c(-0.1,1), ylim = c(0, 40)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")
posTsC <- ggplot(posTestCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "orange", fill = "orange")+ 
  geom_vline(xintercept = round(median(posTestCor$correlation, na.rm = T), 2), 
             colour="red", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(-0.3, 1, 0.2)) + 
  coord_cartesian(xlim = c(-0.1,1), ylim = c(0, 40)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")
negTrC <- ggplot(negTrainCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "blue", fill = "blue")+ 
  geom_vline(xintercept = round(median(negTrainCor$correlation, na.rm = T), 2), 
             colour="red", linetype = "longdash") + 
  scale_x_continuous(breaks = seq(-0.3, 1, 0.2)) + 
  coord_cartesian(xlim = c(-0.3,1), ylim = c(0, 40)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")
negTsC <- ggplot(negTestCor, aes(x=correlation)) +
  geom_histogram(bins=20, color = "blue", fill = "blue")+ 
  geom_vline(xintercept = round(median(negTestCor$correlation, na.rm = T), 2), 
             colour="red", linetype = "longdash") +
  scale_x_continuous(breaks = seq(-0.3, 1, 0.2)) + 
  coord_cartesian(xlim = c(-0.3,1), ylim = c(0, 40)) +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none")

# Correlation values
round(median(posTrainCor$correlation), 2)
round(median(posTestCor$correlation, na.rm = T), 2)
round(median(negTrainCor$correlation), 2)
round(median(negTestCor$correlation, na.rm = T), 2)

plot_grid(posTrC, negTrC,
          posTsC, negTsC,
          ncol = 2)

