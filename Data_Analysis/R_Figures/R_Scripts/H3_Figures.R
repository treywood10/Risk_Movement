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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Figure 4: RR X Deaths ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~#
#### Battle Deaths ####
#~~~~~~~~~~~~~~~~~~~~~#

# Import dataset #
dat_bd <- read_dta("data/RR_X_bd.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_bd.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_bd.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of Z (best) #
z.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_bd$lag_risk_ratio, na.rm = TRUE)
rep_inter1 <- mean(dat_bd$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_best_time <- mean(dat_bd$best_time, na.rm = TRUE)
rep_lag_duration <- mean(dat_bd$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_bd$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_bd$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_bd$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_bd$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_bd$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_bd$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_bd$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_bd$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_bd$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_bd$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_bd$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_bd$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in Z #
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter1, rep_best_time, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop,rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:18])


# Exponentiation of xB for predicted values #
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Gather X coefficients for partial derivative #
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}

# Gather X*Z coefficients for partial derivative #
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}

# Multiply X*Z coefficients by set values of Z #
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}

# Add X coefficients and multiplications for analytical solution of partial derivative #
right <- coefs_rr + mult


# Multiply partial derivative and exp(xB) #
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


# Get median mfx's and middle 95% of simulated mfx's # 
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_bd <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                     Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_bd <- plot_bd %>%
  mutate(group = "Battle Deaths")

### Plot mfx of X ###
plot <- ggplot(data = plot_bd) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_bd, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 200),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  #scale_y_continuous(limits = c(-2000, 5)) +
  geom_rug(data = dat_bd, aes(x = lag_best, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.5) +
  xlab(expression("Battle Deaths"[italic("t-1")])) + 
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Battle Deaths on Troop Counts") +
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
        text = element_text(family = "Times New Roman")) 
ggsave("Graphs/gg_H3_bd.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~#
#### Total OSV ####
#~~~~~~~~~~~~~~~~~#

# Import dataset #
dat_OSV <- read_dta("data/RR_X_OSV.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_OSV.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_OSV.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of Z (lag_OSV_total) #
z.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_OSV$lag_risk_ratio, na.rm = TRUE)
rep_inter2 <- mean(dat_OSV$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_OSV_total_time <- mean(dat_OSV$OSV_total_time, na.rm = TRUE)
rep_lag_duration <- mean(dat_OSV$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_OSV$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_OSV$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_OSV$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_OSV$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_OSV$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_OSV$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_OSV$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_OSV$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_OSV$lag_zone_de_confidence)
rep_lag_quality <- mean(dat_OSV$lag_quality, na.rm = TRUE)
rep_lag_neigh_troops <- mean(dat_OSV$lag_neigh_troops, na.rm = TRUE)

rep_lag_no_troops <- mean(dat_OSV$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in Z #
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter2, rep_OSV_total_time, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:18])


# Exponentiation of xB for predicted values #
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Gather X coefficients for partial derivative #
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}

# Gather X*Z coefficients for partial derivative #
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}

# Multiply X*Z coefficients by set values of Z #
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}

# Add X coefficients and multiplications for analytical solution of partial derivative #
right <- coefs_rr + mult


# Multiply partial derivative and exp(xB) #
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


# Get median mfx's and middle 95% of simulated mfx's # 
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_OSV <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                      Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_OSV <- plot_OSV %>%
  mutate(group = "All OSV")

### Plot mfx of X ###
plot <- ggplot(data = plot_OSV) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_OSV, aes(x = X, y = Median), size = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 200),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  #scale_y_continuous(limits = c(-2000, 5)) +
  geom_rug(data = dat_OSV, aes(x = lag_OSV_Rebs, y = 10), sides = "b", 
           position = position_jitter(w = 0, h = 0), alpha = 0.5) +
  xlab(expression("Total OSV"[italic("t-1")])) + 
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Total OSV on Troop Contributions") +
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
        text = element_text(family = "Times New Roman")) 
ggsave("Graphs/gg_H3_OSV.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~#
#### Rebs OSV ####
#~~~~~~~~~~~~~~~~~#

# Import dataset #
dat_Rebs <- read_dta("data/RR_X_Rebs.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_Rebs.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_Rebs.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of Z (lag_OSV_Rebs) #
z.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_Rebs$lag_risk_ratio, na.rm = TRUE)
rep_inter3 <- mean(dat_Rebs$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_OSV_Rebs_time <- mean(dat_Rebs$OSV_Rebs_time, na.rm = TRUE)
rep_lag_duration <- mean(dat_Rebs$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_Rebs$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_Rebs$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_Rebs$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_Rebs$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_Rebs$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_Rebs$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_Rebs$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_Rebs$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_Rebs$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_Rebs$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_Rebs$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_Rebs$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in Z #
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter3, rep_OSV_Rebs_time, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:18])


# Exponentiation of xB for predicted values #
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Gather X coefficients for partial derivative #
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}

# Gather X*Z coefficients for partial derivative #
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}

# Multiply X*Z coefficients by set values of Z #
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}

# Add X coefficients and multiplications for analytical solution of partial derivative #
right <- coefs_rr + mult


# Multiply partial derivative and exp(xB) #
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


# Get median mfx's and middle 95% of simulated mfx's # 
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_Rebs <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                       Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_Rebs <- plot_Rebs %>%
  mutate(group = "Rebel OSV")

### Plot mfx of X ###
plot <- ggplot(data = plot_Rebs) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_Rebs, aes(x = X, y = Median), size = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 200),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  #scale_y_continuous(limits = c(-2000, 5)) +
  geom_rug(data = dat_Rebs, aes(x = lag_OSV_Rebs, y = 10), sides = "b", 
           position = position_jitter(w = 0.2, h = 0), alpha = 0.5) +
  xlab(expression("Rebel OSV"[italic("t-1")])) + 
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Rebel OSV on Troop Contributions") +
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
        text = element_text(family = "Times New Roman")) 
ggsave("Graphs/gg_H3_Rebs.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~#
#### Gov OSV ####
#~~~~~~~~~~~~~~~~~#

# Import dataset #
dat_Gov <- read_dta("data/RR_X_Gov.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_X_Gov.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_X_Gov.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of Z (lag_OSV_Gov) #
z.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_Gov$lag_risk_ratio, na.rm = TRUE)
rep_inter4 <- mean(dat_Gov$lag_risk_ratio, na.rm = TRUE) * z.of.i
rep_OSV_GOV_time <- mean(dat_Gov$OSV_GOV_time, na.rm = TRUE)
rep_lag_duration <- mean(dat_Gov$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_Gov$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_Gov$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_Gov$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_Gov$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_Gov$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_Gov$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_Gov$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_Gov$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_Gov$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_Gov$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_Gov$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_Gov$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in Z #
set.x <- cbind(rep_lag_risk_ratio, z.of.i, rep_inter4, rep_OSV_GOV_time, rep_lag_duration, rep_lag_nlights_mean, 
               rep_lag_drought_prop, rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own,
               rep_lag_dist_cap_hun, rep_lag_days_to_urban, rep_lag_hq, 
               rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality,
               rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:18])


# Exponentiation of xB for predicted values #
for(i in 1:length(z.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Gather X coefficients for partial derivative #
coefs_rr <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_rr[i,] <- t(simb[,1])
}

# Gather X*Z coefficients for partial derivative #
coefs_inter <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:length(z.of.i)) {
  coefs_inter[i,] <- t(simb[,3])
}

# Multiply X*Z coefficients by set values of Z #
mult <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:ncol(mult)) {
  mult[,i] <- coefs_inter[,i] * t(z.of.i)
}

# Add X coefficients and multiplications for analytical solution of partial derivative #
right <- coefs_rr + mult


# Multiply partial derivative and exp(xB) #
mfx <- matrix(NA, nrow = length(z.of.i), ncol = nsims)

for(i in 1:nsims) {
  mfx[,i] <- right[,i] * xB[,i]
}


# Get median mfx's and middle 95% of simulated mfx's # 
mfx_95 <- apply(X = mfx, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_Gov <- data.frame(X = z.of.i, Lower = mfx_95[1,], Median = mfx_95[2,],
                        Upper = mfx_95[3,])

# Add factor for facet wrap #
plot_Gov <- plot_Gov %>%
  mutate(group = "Government OSV")

### Plot mfx of X ###
plot <- ggplot(data = plot_Gov) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_Gov, aes(x = X, y = Median), size = 1.5, lty = 1) +
  scale_x_continuous(limits = c(0, 200),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) +
  scale_y_continuous(limits = c(-3, 1)) +
  geom_rug(data = dat_Gov, aes(x = lag_OSV_GOV, y = 10), sides = "b", 
           position = position_jitter(w = 0.2, h = 0), alpha = 0.5) +
  xlab(expression("Government OSV"[italic("t-1")])) + 
  ylab("Marginal Effect of Risk Ratio on Contributions") +
  ggtitle("Effect of Risk Ratio and Government OSV on Troop Contributions") +
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
        text = element_text(family = "Times New Roman")) 
ggsave("Graphs/gg_H3_Gov.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### All Plots Together ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot_RR_X_Deaths <- rbind(plot_bd, plot_OSV, plot_Rebs, plot_Gov)

plot_RR_X_Deaths$group <- factor(plot_RR_X_Deaths$group,
                               levels = c("Battle Deaths", "All OSV", "Rebel OSV", "Government OSV"))

best <- as_tibble(dat_bd$lag_best)
best <- best %>%
  rename(death = value) %>%
  mutate(group = "Battle Deaths")


OSV <- as_tibble(dat_OSV$lag_OSV_total)
OSV <- OSV %>%
  rename(death = value) %>%
  mutate(group  = "All OSV")


Reb <- as_tibble(dat_Rebs$lag_OSV_Rebs)
Reb <- Reb %>%
  rename(death = value) %>%
  mutate(group = "Rebel OSV")


Gov <- as_tibble(dat_Gov$lag_OSV_GOV)
Gov <- Gov %>%
  rename(death = value) %>%
  mutate(group = "Government OSV")

rug <- rbind(best, OSV, Reb, Gov)

rug$group <- factor(rug$group,
                    levels = c("Battle Deaths", "All OSV", "Rebel OSV", "Government OSV"))

plot <- ggplot(data = plot_RR_X_Deaths) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_X_Deaths, aes(x = X, y = Median), size = 1.5, lty = 1) +
  geom_rug(data = rug, aes(x = death, y = 0), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  ylab(expression("Marginal Effect of Risk Ratio on Troop Counts")) + 
  xlab(expression("Count of Conflict Danger"[italic("t-1")])) + 
  ggtitle("Risk Ratio and Conflict Danger on Troops Counts") +
  geom_hline(yintercept = 0) + 
  facet_wrap(vars(group), scales = "free_y") +
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
        strip.text.x = element_text(size = 20)) 
ggsave("gg_H3.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper",
       width = 14, height = 12, dpi = 400)
ggsave("Graphs/gg_H3.jpg", width = 14, height = 12, dpi = 400)

