#~~~~~~~~~~~~~~~~#
#### Packages ####
#~~~~~~~~~~~~~~~~#

rm(list=ls())
library(lattice)  ## Load a package for graphing
library(MASS)     ## to draw sample from MVN(beta,vcov)
library(VGAM)
library(corrplot) ## for correlation matrix plot
library(ggplot2)
library(grid)
library(haven)
library(tidyverse)
library(ggpubr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Set Working Directory ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("/Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data_Analysis/R_Figures")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Figure 5: RR X Time ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~#
#### RR X Best Time ####
#~~~~~~~~~~~~~~~~~~~~~~#

### Import data ###
dat_bd_t <- read_dta("data/RR_X_bd_t.dta")


# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_bd_t.txt", header = T))


# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_bd_t.txt", header = T))


### Simulate coefficients
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

### Set values of Z (best-time) ###
z.of.i <- seq(from = 0, to = 240, by = 10)


## Representative observations ##
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_bd_t$lag_risk_ratio, na.rm = TRUE)
rep_inter1 <- mean(dat_bd_t$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_lag_best <- mean(dat_bd_t$lag_best, na.rm = TRUE)
rep_lag_duration <- mean(dat_bd_t$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_bd_t$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_bd_t$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_bd_t$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_bd_t$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_bd_t$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_bd_t$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_bd_t$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_bd_t$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_bd_t$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_bd_t$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_bd_t$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_bd_t$lag_no_troops, na.rm = TRUE)


### Create matrix of representative values and the incremental change in Z ###
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter1, rep_lag_best, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)


### Multiply representative matrix and simulated coefficients for xB ###
xB <- set.x %*% t(simb[,1:18])


### Exponentiation of xB for predicted values ### 
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}


### Gather X coefficients for partial derivative ###
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}


### Gather X*Z coefficients for partial derivative ###
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}


### Multiply X*Z coefficients by set values of Z ###
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}


### Add X coefficients and multiplications for analytical solution of partial derivative ###
right <- coefs_rr + mult


### Multiply partial derivative and exp(xB) ###
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


### Get median mfx's and middle 95% of simulated mfx's ###
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


### Data frame of simulated mfx ###
plot_bd_t <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                     Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_bd_t <- plot_bd_t %>%
  mutate(group = "Battle Deaths")

### Arrow point to insignifiance ###
arrows_bd_t <- tibble (
  x1 = 160,
  x2 = 130,
  y1 = 0,
  y2 = -10,
  group = "Battle Deaths",
  label = "Statistical Insignificance (X = 160)"
)

### Plot mfx of X ###
plot <- ggplot(data = plot_bd_t) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_bd_t, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240)) +
  scale_y_continuous(limits = c(-25, 5)) +
  geom_rug(data = dat_bd_t, aes(x = best_time, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.005) +
  xlab(expression("Months Since Last Battle Death")) +
  ylab("Marginal Effect of Risk Ratio") +
  ggtitle("Effect of Risk Ratio and Time on Troop Counts") +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5, size = 35),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 35, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman")) +
  geom_segment(x = 130, y = -5, xend = 150, yend = -0.5,
               color = "black", arrow = arrow(angle = 20)) +
  geom_text(aes(x = 130, y = -6), label = "Statistical Insignificance (X = 150)",
            size = 8, family = "Times New Roman") 
ggsave("Graphs/gg_H4_bd.jpg", width = 14, height = 12, dpi = 400)


plot <- ggplot(data = plot_bd_t) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_bd_t, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = c(0, 40, 80, 120, 160, 200, 240)) +
  scale_y_continuous(limits = c(-25, 5)) +
  geom_rug(data = dat_bd_t, aes(x = best_time, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.005) +
  xlab(expression("Months Since Last Battle Death")) +
  ylab("Marginal Effect of Risk Ratio") +
  ggtitle("Effect of Risk Ratio and Time on Troop Counts") +
  geom_hline(yintercept = 0, linewidth = 2) +
  theme(plot.title = element_text(hjust = 0.5, size = 35),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title.y = element_text(size = 45, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 45, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman")) +
  geom_segment(x = 130, y = -5, xend = 150, yend = -0.5,
               color = "black", arrow = arrow(angle = 20)) +
  geom_text(aes(x = 130, y = -6), label = "Statistical Insignificance (X = 150)",
            size = 12, family = "Times New Roman") 
ggsave("Graphs/gg_H4_bd_pres.jpg", width = 14, height = 12, dpi = 400)



#~~~~~~~~~~~~~~~~~~~~~#
#### RR X OSV Time ####
#~~~~~~~~~~~~~~~~~~~~~#

### Import data ###
dat_OSV_t <- read_dta("data/RR_X_OSV_t.dta")


# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_OSV_t.txt", header = T))


# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_OSV_t.txt", header = T))


### Simulate coefficients
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

### Set values of Z (OSV time) ###
z.of.i <- seq(from = 0, to = 240, by = 10)


## Representative observations ##
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_OSV_t$lag_risk_ratio, na.rm = TRUE)
rep_inter2 <- mean(dat_OSV_t$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_lag_OSV_total <- mean(dat_OSV_t$lag_OSV_total, na.rm = TRUE)
rep_lag_duration <- mean(dat_OSV_t$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_OSV_t$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_OSV_t$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_OSV_t$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_OSV_t$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_OSV_t$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_OSV_t$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_OSV_t$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_OSV_t$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_OSV_t$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_OSV_t$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_OSV_t$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_OSV_t$lag_no_troops, na.rm = TRUE)


### Create matrix of representative values and the incremental change in Z ###
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter2, rep_lag_OSV_total, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)


### Multiply representative matrix and simulated coefficients for xB ###
xB <- set.x %*% t(simb[,1:18])


### Exponentiation of xB for predicted values ### 
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}


### Gather X coefficients for partial derivative ###
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}


### Gather X*Z coefficients for partial derivative ###
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}


### Multiply X*Z coefficients by set values of Z ###
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}


### Add X coefficients and multiplications for analytical solution of partial derivative ###
right <- coefs_rr + mult


### Multiply partial derivative and exp(xB) ###
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


### Get median mfx's and middle 95% of simulated mfx's ###
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


### Data frame of simulated mfx ###
plot_OSV_t <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                        Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_OSV_t <- plot_OSV_t %>%
  mutate(group = "All OSV")


### Arrow point to insignifiance ###
arrows_OSV_t <- tibble (
  x1 = 150,
  x2 = 130,
  y1 = 0,
  y2 = -10,
  group = "All OSV",
  label = "Statistical Insignificance (X = 150)"
)

### Plot mfx of X ###
plot <- ggplot(data = plot_OSV_t) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_OSV_t, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240)) +
  scale_y_continuous(limits = c(-30, 5)) +
  geom_rug(data = dat_OSV_t, aes(x = OSV_total_time, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.005) +
  xlab(expression("Months Since Last Act of OSV")) +
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Time on Troop Contributions") +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5, size = 35),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 35, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman")) +
  geom_segment(x = 130, y = -5, xend = 150, yend = -0.5,
               color = "black", arrow = arrow(angle = 20)) +
  geom_text(aes(x = 130, y = -6), label = "Statistical Insignificance (X = 150)",
            size = 8, family = "Times New Roman") 
ggsave("Graphs/gg_H4_OSV.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~~~~~#
#### RR X Reb Time ####
#~~~~~~~~~~~~~~~~~~~~~#

### Import data ###
dat_Reb_t <- read_dta("data/RR_X_Rebs_t.dta")


# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_Rebs_t.txt", header = T))


# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_Rebs_t.txt", header = T))


### Simulate coefficients
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

### Set values of Z (Reb time) ###
z.of.i <- seq(from = 0, to = 240, by = 10)


## Representative observations ##
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_Reb_t$lag_risk_ratio, na.rm = TRUE)
rep_inter3 <- mean(dat_Reb_t$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_lag_OSV_Rebs <- mean(dat_Reb_t$lag_OSV_Rebs, na.rm = TRUE)
rep_lag_duration <- mean(dat_Reb_t$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_Reb_t$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_Reb_t$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_Reb_t$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_Reb_t$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_Reb_t$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_Reb_t$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_Reb_t$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_Reb_t$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_Reb_t$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_Reb_t$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_Reb_t$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_Reb_t$lag_no_troops, na.rm = TRUE)


### Create matrix of representative values and the incremental change in Z ###
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter3, rep_lag_OSV_Rebs, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)


### Multiply representative matrix and simulated coefficients for xB ###
xB <- set.x %*% t(simb[,1:18])


### Exponentiation of xB for predicted values ### 
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}


### Gather X coefficients for partial derivative ###
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}


### Gather X*Z coefficients for partial derivative ###
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}


### Multiply X*Z coefficients by set values of Z ###
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}


### Add X coefficients and multiplications for analytical solution of partial derivative ###
right <- coefs_rr + mult


### Multiply partial derivative and exp(xB) ###
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


### Get median mfx's and middle 95% of simulated mfx's ###
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


### Data frame of simulated mfx ###
plot_Reb_t <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                         Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_Reb_t <- plot_Reb_t %>%
  mutate(group = "Rebel OSV")


### Arrow point to insignifiance ###
arrows_Reb_t <- tibble (
  x1 = 170,
  x2 = 130,
  y1 = 0,
  y2 = -10,
  group = "Rebel OSV",
  label = "Statistical Insignificance (X = 170)"
)

### Plot mfx of X ###
plot <- ggplot(data = plot_Reb_t) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_Reb_t, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240)) +
  scale_y_continuous(limits = c(-30, 5)) +
  geom_rug(data = dat_OSV_t, aes(x = OSV_Rebs_time, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.005) +
  xlab(expression("Months Since Last Act of Rebel OSV")) +
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Time on Troop Contributions") +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5, size = 35),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 35, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman")) +
  geom_segment(x = 130, y = -5, xend = 150, yend = -0.5,
               color = "black", arrow = arrow(angle = 20)) +
  geom_text(aes(x = 130, y = -6), label = "Statistical Insignificance (X = 160)",
            size = 8, family = "Times New Roman") 
ggsave("Graphs/gg_H4_Reb.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~~~~~#
#### RR X Gov Time ####
#~~~~~~~~~~~~~~~~~~~~~#

### Import data ###
dat_Gov_t <- read_dta("data/RR_X_Gov_t.dta")


# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_Gov_t.txt", header = T))


# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_Gov_t.txt", header = T))


### Simulate coefficients
nsims <- 1000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

### Set values of Z (Gov time) ###
z.of.i <- seq(from = 0, to = 240, by = 10)


## Representative observations ##
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_Gov_t$lag_risk_ratio, na.rm = TRUE)
rep_inter4 <- mean(dat_Gov_t$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_lag_OSV_GOV <- mean(dat_Gov_t$lag_OSV_GOV, na.rm = TRUE)
rep_lag_duration <- mean(dat_Gov_t$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_Gov_t$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_Gov_t$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_Gov_t$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_Gov_t$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_Gov_t$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_Gov_t$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_Gov_t$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_Gov_t$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_Gov_t$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_Gov_t$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_Gov_t$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_Gov_t$lag_no_troops, na.rm = TRUE)


### Create matrix of representative values and the incremental change in Z ###
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter4, rep_lag_OSV_GOV, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)


### Multiply representative matrix and simulated coefficients for xB ###
xB <- set.x %*% t(simb[,1:18])


### Exponentiation of xB for predicted values ### 
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}


### Gather X coefficients for partial derivative ###
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}


### Gather X*Z coefficients for partial derivative ###
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}


### Multiply X*Z coefficients by set values of Z ###
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}


### Add X coefficients and multiplications for analytical solution of partial derivative ###
right <- coefs_rr + mult


### Multiply partial derivative and exp(xB) ###
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


### Get median mfx's and middle 95% of simulated mfx's ###
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


### Data frame of simulated mfx ###
plot_Gov_t <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                         Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_Gov_t <- plot_Gov_t %>%
  mutate(group = "Government OSV")


### Arrow point to insignifiance ###
arrows_Gov_t <- tibble (
  x1 = 160,
  x2 = 130,
  y1 = 0,
  y2 = -10,
  group = "Government OSV",
  label = "Statistical Insignificance (X = 160)"
)

### Plot mfx of X ###
plot <- ggplot(data = plot_Gov_t) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_Gov_t, aes(x = X, y = Median), size = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240)) +
  scale_y_continuous(limits = c(-30, 5)) +
  geom_rug(data = dat_OSV_t, aes(x = OSV_GOV_time, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.005) +
  xlab(expression("Months Since Last Act of Government OSV")) +
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Time on Troop Contributions") +
  geom_hline(yintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5, size = 35),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 35, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman")) +
  geom_segment(x = 130, y = -5, xend = 150, yend = -0.5,
               color = "black", arrow = arrow(angle = 20)) +
  geom_text(aes(x = 130, y = -6), label = "Statistical Insignificance (X = 160)",
            size = 8, family = "Times New Roman") 
ggsave("Graphs/gg_H4_Gov.jpg", width = 14, height = 12, dpi = 400)



#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### All Plots Together ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot_RR_X_Time <- rbind(plot_bd_t, plot_OSV_t, plot_Reb_t, plot_Gov_t)

plot_RR_X_Time$group <- factor(plot_RR_X_Time$group,
                                 levels = c("Battle Deaths", "All OSV", "Rebel OSV", "Government OSV"))


best <- as_tibble(dat_bd_t$best_time)
best <- best %>%
  rename(time = value) %>%
  mutate(group = "Battle Deaths")


OSV <- as_tibble(dat_OSV_t$OSV_total_time)
OSV <- OSV %>%
  rename(time = value) %>%
  mutate(group  = "All OSV")


Reb <- as_tibble(dat_Reb_t$OSV_Rebs_time)
Reb <- Reb %>%
  rename(time = value) %>%
  mutate(group = "Rebel OSV")


Gov <- as_tibble(dat_Gov_t$OSV_GOV_time)
Gov <- Gov %>%
  rename(time = value) %>%
  mutate(group = "Government OSV")


rug <- rbind(best, OSV, Reb, Gov)

rug$group <- factor(rug$group,
                    levels = c("Battle Deaths", "All OSV", "Rebel OSV", "Government OSV"))

arrow <- rbind(arrows_bd_t, arrows_OSV_t, arrows_Reb_t, arrows_Gov_t)

arrow$group <- factor(arrow$group,
                      levels = c("Battle Deaths", "All OSV", "Rebel OSV", "Government OSV"))

plot <- ggplot(data = plot_RR_X_Time) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_X_Time, aes(x = X, y = Median), size = 1.5, lty = 1) +
  geom_rug(data = rug, aes(x = time, y = 0), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.15) +
  scale_x_continuous(limits = c(0, 240),
                     breaks = c(0, 40, 80, 120, 160, 200, 240)) +
  scale_y_continuous(limits = c(-40, 2)) +
  coord_cartesian(ylim = c(-26, 2)) +
  geom_hline(yintercept = 0) +
  ylab(expression("Marginal Effect of Risk Ratio on Troop Counts")) + 
  xlab(expression("Months Since Last Act of Conflict Danger"[italic("t-1")])) + 
  ggtitle("Risk Ratio and Time Since Act of Conflict Danger on Troops Counts") +
  facet_wrap(vars(group), scales = "fixed") +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 20, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman"),
        strip.text.x = element_text(size = 20)) +
  geom_segment(data = arrow, aes(x = x2, y = y2, xend = x1, yend = y1), arrow = arrow(angle = 20)) +
  geom_text(data = arrow, aes(x = x2, y = (y2-1)), label = arrow$label, size = 5, family = "Times New Roman")
ggsave("gg_H4.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper",
       width = 14, height = 12, dpi = 400)
ggsave("Graphs/gg_H4.jpg", width = 14, height = 12, dpi = 400)


