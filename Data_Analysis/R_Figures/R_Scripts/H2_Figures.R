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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Figure 3: RR and Deaths ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~#
#### Battle Deaths ####
#~~~~~~~~~~~~~~~~~~~~~#


# Import dataset #
dat <- read_dta("data/RR_bd.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_bd.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_bd.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Risk Ratio) #
x.of.i <- seq(from = 0.4, to = 1, by = 0.005)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_best <- mean(dat$lag_best, na.rm = TRUE)
rep_best_time <- mean(dat$best_time, na.rm = TRUE)
rep_lag_duration <- mean(dat$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(x.of.i, rep_lag_best, rep_best_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop,
               rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own, rep_lag_dist_cap_hun, rep_lag_days_to_urban,
               rep_lag_hq, rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality, rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:17])

# Exponentiation of xB for predicted values #
for(i in 1:length(x.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Get median mfx's and middle 95% of simulated mfx's #
margins <- apply(X = xB, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_RR_bd <- data.frame(X = x.of.i, Lower = margins[1,], Median = margins[2,],
                         Upper = margins[3,])

# Add factor for facet wrap #
plot_RR_bd <- plot_RR_bd %>%
  mutate(group = "Battle Deaths")

### Plot margins ###
plot <- ggplot(data = plot_RR_bd) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_bd, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat, aes(x = lag_risk_ratio, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Risk Ratio")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  ggtitle("Effect of Risk Ratio on Troop Counts, Battle Deaths") +
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
ggsave("Graphs/gg_H2_RR_bd.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~#
#### Total OSV ####
#~~~~~~~~~~~~~~~~~#


# Import dataset #
dat <- read_dta("data/RR_OSV.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_OSV.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_OSV.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Risk Ratio) #
x.of.i <- seq(from = 0.4, to = 1, by = 0.005)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_OSV_total <- mean(dat$lag_OSV_total, na.rm = TRUE)
rep_OSV_total_time <- mean(dat$OSV_total_time, na.rm = TRUE)
rep_lag_duration <- mean(dat$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(x.of.i, rep_lag_OSV_total, rep_OSV_total_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop,
               rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own, rep_lag_dist_cap_hun, rep_lag_days_to_urban,
               rep_lag_hq, rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality, rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:17])

# Exponentiation of xB for predicted values #
for(i in 1:length(x.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Get median mfx's and middle 95% of simulated mfx's #
margins <- apply(X = xB, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_RR_OSV <- data.frame(X = x.of.i, Lower = margins[1,], Median = margins[2,],
                          Upper = margins[3,])

# Add factor for facet wrap #
plot_RR_OSV <- plot_RR_OSV %>%
  mutate(group = "All OSV")

### Plot margins ###
plot <- ggplot(data = plot_RR_OSV) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_OSV, aes(x = X, y = Median), size = 1.5, lty = 1) +
  geom_rug(data = dat, aes(x = lag_risk_ratio, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Risk Ratio")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  ggtitle("Effect of Risk Ratio on Troop Counts, All OSV") +
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
ggsave("Graphs/gg_H2_RR_OSV.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~#
#### Rebs OSV ####
#~~~~~~~~~~~~~~~~#


# Import dataset #
dat <- read_dta("data/RR_Rebs.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_Rebs.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_Rebs.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Risk Ratio) #
x.of.i <- seq(from = 0.4, to = 1, by = 0.005)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_OSV_Rebs <- mean(dat$lag_OSV_Rebs, na.rm = TRUE)
rep_OSV_Rebs_time <- mean(dat$OSV_Rebs_time, na.rm = TRUE)
rep_lag_duration <- mean(dat$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(x.of.i, rep_lag_OSV_Rebs, rep_OSV_Rebs_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop,
               rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own, rep_lag_dist_cap_hun, rep_lag_days_to_urban,
               rep_lag_hq, rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality, rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:17])

# Exponentiation of xB for predicted values #
for(i in 1:length(x.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Get median mfx's and middle 95% of simulated mfx's #
margins <- apply(X = xB, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_RR_Rebs <- data.frame(X = x.of.i, Lower = margins[1,], Median = margins[2,],
                           Upper = margins[3,])

# Add factor for facet wrap #
plot_RR_Rebs <- plot_RR_Rebs %>%
  mutate(group = "Rebel OSV")

### Plot margins ###
plot <- ggplot(data = plot_RR_Rebs) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_Rebs, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat, aes(x = lag_risk_ratio, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Risk Ratio")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  ggtitle("Effect of Risk Ratio on Troop Counts, Rebel OSV") +
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
ggsave("Graphs/gg_H2_RR_Rebs.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~#
#### Gov OSV ####
#~~~~~~~~~~~~~~~#


# Import dataset #
dat <- read_dta("data/RR_Gov.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_Gov.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_Gov.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Risk Ratio) #
x.of.i <- seq(from = 0.4, to = 1, by = 0.005)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_OSV_GOV <- mean(dat$lag_OSV_GOV, na.rm = TRUE)
rep_OSV_GOV_time <- mean(dat$OSV_GOV_time, na.rm = TRUE)
rep_lag_duration <- mean(dat$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(x.of.i, rep_lag_OSV_GOV, rep_OSV_GOV_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop,
               rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own, rep_lag_dist_cap_hun, rep_lag_days_to_urban,
               rep_lag_hq, rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality, rep_lag_no_troops, 1)

# Multiply representative matrix and simulated coefficients for xB #
xB <- set.x %*% t(simb[,1:17])

# Exponentiation of xB for predicted values #
for(i in 1:length(x.of.i)) {
  xB[i, ] <- exp(xB[i,]) 
}

# Get median mfx's and middle 95% of simulated mfx's #
margins <- apply(X = xB, MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975))


# Data frame of simulated mfx #
plot_RR_Gov <- data.frame(X = x.of.i, Lower = margins[1,], Median = margins[2,],
                          Upper = margins[3,])

# Add factor for facet wrap #
plot_RR_Gov <- plot_RR_Gov %>%
  mutate(group = "Government OSV")

### Plot margins ###
plot <- ggplot(data = plot_RR_Gov) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_Gov, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat, aes(x = lag_risk_ratio, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Risk Ratio")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
  ggtitle("Effect of Risk Ratio on Troop Counts, Government OSV") +
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
ggsave("Graphs/gg_H2_RR_Gov.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### All Plots Together ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot_RR_Deaths <- rbind(plot_RR_bd, plot_RR_OSV, plot_RR_Rebs, plot_RR_Gov)

plot_RR_Deaths$group <- factor(plot_RR_Deaths$group,
                               levels = c("Battle Deaths", "All OSV", "Rebel OSV", "Government OSV"))

plot <- ggplot(data = plot_RR_Deaths) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_Deaths, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat, aes(x = lag_risk_ratio, y = 0), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.05) +
  scale_y_continuous(limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10)) + 
  coord_cartesian(ylim = c(0, 10)) +
  ylab(expression("Prediced Number of Troops in Cell")) + 
  xlab(expression("Risk Ratio"[italic("t-1")])) + 
  geom_hline(yintercept = 0) +
  ggtitle("Effect of Risk Ratio on Troop Counts with Various Conflict Danger Measures") +
  facet_wrap(vars(group), scales = "free_y") +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
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
ggsave("gg_H2.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper",
       width = 14, height = 12, dpi = 400)
ggsave("Graphs/gg_H2.jpg", width = 14, height = 12, dpi = 400)

