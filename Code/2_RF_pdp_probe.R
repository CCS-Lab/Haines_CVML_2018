rm(list=ls())

library(dplyr)
library(randomForest)
library(doParallel)
library(foreach)
library(pdp)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
library(irr)

# Load scout, nate, or combined data
load("/Data/allDat_1segment_overallQuality3.RData")
load("/Data/image_rf_int_probe_v6.RData")

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


# Directional Effects
pos_AU12 <- partial(fit_pos, train = dat_pos[,-1], pred.var = "AU12overall", chull = F)
pos_AU12 <- autoplot(pos_AU12, contour = T) + theme_minimal()
pos_AU18 <- partial(fit_pos, train = dat_pos[,-1], pred.var = "AU18overall", chull = F)
pos_AU18 <- autoplot(pos_AU18, contour = T) + theme_minimal()
pos_AU6 <- partial(fit_pos, train = dat_pos[,-1], pred.var = "AU6overall", chull = F)
pos_AU6 <- autoplot(pos_AU6, contour = T) + theme_minimal()
pos_AU25 <- partial(fit_pos, train = dat_pos[,-1], pred.var = "AU25overall", chull = F)
pos_AU25 <- autoplot(pos_AU25, contour = T) + theme_minimal()
pos_AU2 <- partial(fit_pos, train = dat_pos[,-1], pred.var = "AU2overall", chull = F)
pos_AU2 <- autoplot(pos_AU2, contour = T) + theme_minimal()
cowplot::plot_grid(pos_AU12, pos_AU6, pos_AU25, pos_AU18, ncol = 2)

neg_AU4 <- partial(fit_neg, train = dat_neg[,-1], pred.var = "AU4overall", chull = F)
neg_AU4 <- autoplot(neg_AU4, contour = T) + theme_minimal()
neg_AU5 <- partial(fit_neg, train = dat_neg[,-1], pred.var = "AU5overall", chull = F)
neg_AU5 <- autoplot(neg_AU5, contour = T) + theme_minimal()
neg_AU1 <- partial(fit_neg, train = dat_neg[,-1], pred.var = "AU1overall", chull = F)
neg_AU1 <- autoplot(neg_AU1, contour = T) + theme_minimal()
neg_AU9 <- partial(fit_neg, train = dat_neg[,-1], pred.var = "AU9overall", chull = F)
neg_AU9 <- autoplot(neg_AU9, contour = T) + theme_minimal()
neg_AU10 <- partial(fit_neg, train = dat_neg[,-1], pred.var = "AU10overall", chull = F)
neg_AU10 <- autoplot(neg_AU10, contour = T) + theme_minimal()

# Interactive effects (positive)
pos_AU12_AU18 <- partial(fit_pos, pred.var = c("AU12overall", "AU18overall"), train = dat_pos[,-1], chull = F)
pos_AU12_AU18 <- autoplot(pos_AU12_AU18, contour = TRUE, legend.title = "Partial\ndependence")
pos_AU12_AU2 <- partial(fit_pos, pred.var = c("AU12overall", "AU2overall"), train = dat_pos[,-1], chull = F)
pos_AU12_AU2 <- autoplot(pos_AU12_AU2, contour = TRUE, legend.title = "Partial\ndependence")
pos_AU2_AU6 <- partial(fit_pos, pred.var = c("AU2overall", "AU6overall"), train = dat_pos[,-1], chull = F)
pos_AU2_AU6 <- autoplot(pos_AU2_AU6, contour = TRUE, legend.title = "Partial\ndependence")
pos_AU2_AU14 <- partial(fit_pos, pred.var = c("AU2overall", "AU14overall"), train = dat_pos[,-1], chull = F)
pos_AU2_AU14 <- autoplot(pos_AU2_AU14, contour = TRUE, legend.title = "Partial\ndependence")
pos_AU6_AU14 <- partial(fit_pos, pred.var = c("AU6overall", "AU14overall"), train = dat_pos[,-1], chull = F)
pos_AU6_AU14 <- autoplot(pos_AU6_AU14, contour = TRUE, legend.title = "Partial\ndependence")

# Interactive effects (negative)
neg_AU4_AU5 <- partial(fit_neg, pred.var = c("AU4overall", "AU5overall"), train = dat_neg[,-1], chull = F)
neg_AU4_AU5 <- autoplot(neg_AU4_AU5, contour = TRUE, legend.title = "Partial\ndependence")
neg_AU1_AU5 <- partial(fit_neg, pred.var = c("AU1overall", "AU5overall"), train = dat_neg[,-1], chull = F)
neg_AU1_AU5 <- autoplot(neg_AU1_AU5, contour = TRUE, legend.title = "Partial\ndependence")
neg_AU1_AU4 <- partial(fit_neg, pred.var = c("AU1overall", "AU4overall"), train = dat_neg[,-1], chull = F)
neg_AU1_AU4 <- autoplot(neg_AU1_AU4, contour = TRUE, legend.title = "Partial\ndependence")
neg_AU5_AU9 <- partial(fit_neg, pred.var = c("AU5overall", "AU9overall"), train = dat_neg[,-1], chull = F)
neg_AU5_AU9 <- autoplot(neg_AU5_AU9, contour = TRUE, legend.title = "Partial\ndependence")
neg_AU4_AU9 <- partial(fit_neg, pred.var = c("AU4overall", "AU9overall"), train = dat_neg[,-1], chull = F)
neg_AU4_AU9 <- autoplot(neg_AU4_AU9, contour = TRUE, legend.title = "Partial\ndependence")
neg_AU4_AU10 <- partial(fit_neg, pred.var = c("AU4overall", "AU10overall"), train = dat_neg[,-1], chull = F)
neg_AU4_AU10 <- autoplot(neg_AU4_AU10, contour = TRUE, legend.title = "Partial\ndependence")

save.image(file = "/Data/image_rf_direction_int_probe.RData")


### Plotting PDP ###

# For sharing legend across tile plots (see https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs)
grid_arrange_shared_legend <- function(..., 
                                       ncol     = length(list(...)), 
                                       nrow     = 1, 
                                       position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

# Variable importance plots
p_imp$data$Variable <- gsub(x = p_imp$data$Variable, pattern = "AU", replacement = "")
n_imp$data$Variable <- gsub(x = n_imp$data$Variable, pattern = "AU", replacement = "")
p_int$data$Variables <- gsub(x = p_int$data$Variables, pattern = "AU", replacement = "")
n_int$data$Variables <- gsub(x = n_int$data$Variables, pattern = "AU", replacement = "")
var_imp <-plot_grid(p_imp + ylab("") + theme_minimal(base_size = 20), 
                    n_imp + ylab("") + theme_minimal(base_size = 20), 
                    p_int + ylab("") + theme_minimal(base_size = 20),
                    n_int + ylab("") + theme_minimal(base_size = 20),  
                    ncol = 2, align = "hv")
ggsave(var_imp, filename = "var_imp.pdf", 
       dpi = 300, units = "in", width = 10, height = 10)

# Directional and interactive effects
pos_AU12$labels$x <- "AU12"
pos_AU18$labels$x <- "AU18"
pos_AU6$labels$x  <- "AU6"
pos_AU2$labels$x  <- "AU2"
pos_AU25$labels$x <- "AU25"
neg_AU4$labels$x  <- "AU4"
neg_AU5$labels$x  <- "AU5"
neg_AU1$labels$x  <- "AU1"
neg_AU9$labels$x  <- "AU9"
neg_AU10$labels$x <- "AU10"
imp_dir <- plot_grid(pos_AU12 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     neg_AU4 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15),
                     pos_AU18 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     neg_AU5 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     pos_AU6 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15),
                     neg_AU1 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     pos_AU2 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     neg_AU9 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     pos_AU25 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15),  
                     neg_AU10 + xlim(-5, 5) + ylim(1, 3.5) + ylab("") + theme_minimal(base_size = 15), 
                     ncol = 2, align = "hv", axis = "rltb")

# Hacky way to remove "overall" from labels
label_names <- names(pos_AU12_AU2$labels)
pos_AU12_AU18$labels <- as.list(gsub(x = pos_AU12_AU18$labels, pattern = "overall", replacement = ""))
pos_AU12_AU2$labels <- as.list(gsub(x = pos_AU12_AU2$labels, pattern = "overall", replacement = ""))
pos_AU2_AU6$labels <- as.list(gsub(x = pos_AU2_AU6$labels, pattern = "overall", replacement = ""))
pos_AU2_AU14$labels <- as.list(gsub(x = pos_AU2_AU14$labels, pattern = "overall", replacement = ""))
pos_AU6_AU14$labels <- as.list(gsub(x = pos_AU6_AU14$labels, pattern = "overall", replacement = ""))
neg_AU4_AU5$labels <- as.list(gsub(x = neg_AU4_AU5$labels, pattern = "overall", replacement = ""))
neg_AU1_AU5$labels <- as.list(gsub(x = neg_AU1_AU5$labels, pattern = "overall", replacement = ""))
neg_AU1_AU4$labels <- as.list(gsub(x = neg_AU1_AU4$labels, pattern = "overall", replacement = ""))
neg_AU5_AU9$labels <- as.list(gsub(x = neg_AU5_AU9$labels, pattern = "overall", replacement = ""))
neg_AU4_AU9$labels <- as.list(gsub(x = neg_AU4_AU9$labels, pattern = "overall", replacement = ""))
names(pos_AU12_AU18$labels) <- label_names
names(pos_AU12_AU2$labels) <- label_names
names(pos_AU2_AU6$labels) <- label_names
names(pos_AU2_AU14$labels) <- label_names
names(pos_AU6_AU14$labels) <- label_names
names(neg_AU4_AU5$labels) <- label_names
names(neg_AU1_AU5$labels) <- label_names
names(neg_AU1_AU4$labels) <- label_names
names(neg_AU5_AU9$labels) <- label_names
names(neg_AU4_AU9$labels) <- label_names

imp_int <- grid_arrange_shared_legend(pos_AU12_AU18 + theme_minimal(base_size = 15), 
                                      neg_AU4_AU5 + theme_minimal(base_size = 15),
                                      pos_AU12_AU2 + theme_minimal(base_size = 15), 
                                      neg_AU1_AU5 + theme_minimal(base_size = 15),
                                      pos_AU2_AU6 + theme_minimal(base_size = 15),
                                      neg_AU1_AU4 + theme_minimal(base_size = 15),
                                      pos_AU2_AU14 + theme_minimal(base_size = 15), 
                                      neg_AU5_AU9 + theme_minimal(base_size = 15),
                                      pos_AU6_AU14 + theme_minimal(base_size = 15), 
                                      neg_AU4_AU9 + theme_minimal(base_size = 15),
                                      ncol = 2, nrow = 5, position = "right")

imp_fig <- plot_grid(imp_dir, imp_int, ncol = 2, 
                     rel_widths = c(1, 1.3),
                     rel_heights = c(1, 1.05))

ggsave(imp_fig, filename = "imp_fig.pdf", 
       dpi = 300, units = "in", height = 8, width = 11)

## ICC between positive and neagtive importance profiles
icc(data.frame(p_imp$data$Importance[order(p_imp$data$Variable)],
               n_imp$data$Importance[order(n_imp$data$Variable)]), 
    unit = "average", type = "agreement")



