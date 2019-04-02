# Haines_CVML_2018
Data and codes used for primary analyses in Haines et al. (2019, PLOS ONE) Using computer-vision and machine learning to automate facial coding of positive and negative affect intensity

The preprocessed data are included in an Rdata file in the "Data" directory. This file contains the AUC Evidence scores for each AU for all recordings. 

The "Code" directory contains the R code used to fit the RF models and plot the results as seen in the manuscript. The R scripts are numbered in the order that they should be run to reproduce analyses. 

The "trained_rf_models.RData" contains the final random forest models trained across all 4,648 facial expression recordings. After downloading the models, they can be loaded into R with the command `load("rf_models_1seg_allTrain.RData")`. Then, the positive and negative affect models can be accessed with `rf_models$rf_pos` and `rf_models$rf_neg`, respectively. These RF model objects can be used to make predictions on preprocessed action unit data to produce ratings of positive and negative affect. 
