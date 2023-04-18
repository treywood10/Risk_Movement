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


#~~~~~~~~~~~~~~~~~~~~~~~~#
#### Figure 2: Deaths ####
#~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~#
#### Battle Deaths ####
#~~~~~~~~~~~~~~~~~~~~~#


# Import dataset #
dat_bd <- read_dta("data/RR_bd.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_bd.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_bd.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Battle Deaths) #
x.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_bd$lag_risk_ratio, na.rm = TRUE)
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

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(rep_lag_risk_ratio, x.of.i, rep_best_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop, 
               rep_lag_mountains_prop, rep_lag_dist_unit, rep_lag_dist_border_own, rep_lag_dist_cap_hun, rep_lag_days_to_urban,
               rep_lag_hq,rep_lag_zone_de_confidence, rep_lag_neigh_troops, rep_lag_quality, rep_lag_no_troops, 1)

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
plot_bd <- ggplot(data = plot_RR_bd) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_bd, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat_bd, aes(x = lag_best, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Battle Deaths")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  ggtitle("Effect of Battle Deaths on Troop Counts") +
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
ggsave("Graphs/gg_H1_bd.jpg", width = 14, height = 12, dpi = 400)


#~~~~~~~~~~~~~~~~~#
#### Total OSV ####
#~~~~~~~~~~~~~~~~~#


# Import dataset #
dat_OSV <- read_dta("data/RR_OSV.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_OSV.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_OSV.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (OSV Total) #
x.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_OSV$lag_risk_ratio, na.rm = TRUE)
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
rep_lag_neigh_troops <- mean(dat_OSV$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_OSV$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_OSV$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(rep_lag_risk_ratio, x.of.i, rep_OSV_total_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop,
               rep_lag_mountains_prop, rep_lag_dist_unit,rep_lag_dist_border_own, rep_lag_dist_cap_hun, rep_lag_days_to_urban,
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
plot_OSV <- ggplot(data = plot_RR_OSV) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_OSV, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat_OSV, aes(x = lag_OSV_total, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Total One Sided Violence")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  ggtitle("Effect of Total One-Sided Violence on Troop Counts") +
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
dat_Reb <- read_dta("data/RR_Rebs.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_Rebs.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_Rebs.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Risk Ratio) #
x.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_Reb$lag_risk_ratio, na.rm = TRUE)
rep_OSV_Rebs_time <- mean(dat_Reb$OSV_Rebs_time, na.rm = TRUE)
rep_lag_duration <- mean(dat_Reb$lag_duration, na.rm = TRUE)
rep_lag_nlights_mean <- mean(dat_Reb$lag_nlights_mean, na.rm = TRUE)
rep_lag_drought_prop <- mean(dat_Reb$lag_drought_prop, na.rm = TRUE)
rep_lag_mountains_prop <- mean(dat_Reb$lag_mountains_prop, na.rm = TRUE)
rep_lag_dist_unit <- mean(dat_Reb$lag_dist_unit, na.rm = TRUE)
rep_lag_dist_border_own <- mean(dat_Reb$lag_dist_border_own, na.rm = TRUE)
rep_lag_dist_cap_hun <- mean(dat_Reb$lag_dist_cap_hun, na.rm = TRUE)
rep_lag_days_to_urban <- mean(dat_Reb$lag_days_to_urban, na.rm = TRUE)
rep_lag_hq <- getmode(dat_Reb$lag_hq)
rep_lag_zone_de_confidence <- getmode(dat_Reb$lag_zone_de_confidence)
rep_lag_neigh_troops <- mean(dat_Reb$lag_neigh_troops, na.rm = TRUE)
rep_lag_quality <- mean(dat_Reb$lag_quality, na.rm = TRUE)
rep_lag_no_troops <- mean(dat_Reb$lag_no_troops, na.rm = TRUE)

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(rep_lag_risk_ratio, x.of.i, rep_OSV_Rebs_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop, 
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
plot_Rebs <- ggplot(data = plot_RR_Rebs) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_Rebs, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat_Reb, aes(x = lag_OSV_Rebs, y = -2), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Risk Ratio")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  ggtitle("Effect of Rebel One-Sided Violence on Troop Counts") +
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
dat_Gov <- read_dta("data/RR_Gov.dta")

# Betas #
betas <- as.matrix(read.table("betas/betas_RR_Gov.txt", header = T))

# var-cov #
vcov <- as.matrix(read.table("vcovs/vcovs_RR_Gov.txt", header = T))

# Simulate Coefficients #
nsims <- 10000
set.seed(1234)
simb <- mvrnorm(n = nsims, mu = betas, Sigma = vcov)

# Set values of X (Risk Ratio) #
x.of.i <- seq(from = 0, to = 200, by = 10)

# Representative observations #
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

rep_lag_risk_ratio <- mean(dat_Gov$lag_risk_ratio, na.rm = TRUE)
rep_OSV_GOV_time <- mean(dat_Gov$OSV_GOV_time, na.rm = TRUE)
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

# Create matrix of representative values and the incremental change in X #
set.x <- cbind(rep_lag_risk_ratio, x.of.i, rep_OSV_GOV_time, rep_lag_duration, rep_lag_nlights_mean, rep_lag_drought_prop,
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
plot_Gov <- ggplot(data = plot_RR_Gov) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_Gov, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = dat_Gov, aes(x = lag_OSV_GOV, y = 0), sides = "b", 
           position = position_jitter(w = 0.005, h = 0), alpha = 0.15) +
  xlab(expression("Risk Ratio")) +
  ylab(expression("Predicted Number of Troops in Cell")) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
  ggtitle("Effect of Government One-Sided Violence on Troop Counts") +
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


best <- as_tibble(dat_bd$lag_best)
best <- best %>%
  rename(death = value) %>%
  mutate(group = "Battle Deaths")


OSV <- as_tibble(dat_OSV$lag_OSV_total)
OSV <- OSV %>%
  rename(death = value) %>%
  mutate(group  = "All OSV")


Reb <- as_tibble(dat_Reb$lag_OSV_Rebs)
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

plot <- ggplot(data = plot_RR_Deaths) +
  geom_ribbon(alpha = .5, aes(x = X, ymin = Lower, ymax = Upper), fill = "gray50") +
  geom_line(data = plot_RR_Deaths, aes(x = X, y = Median), linewidth = 1.5, lty = 1) +
  geom_rug(data = rug, aes(x = death, y = 0), sides = "b", 
           position = position_jitter(w = 0.008, h = 0), alpha = 0.15) +
  geom_hline(yintercept = 0) +
  ylab(expression("Prediced Number of Troops in Cell")) + 
  xlab(expression("Count of Conflict Danger"[italic("t-1")])) + 
  ggtitle("Effect of Various Violent Actions on Troop Counts") +
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
ggsave("gg_H1.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper",
       width = 14, height = 12, dpi = 400)
ggsave("Graphs/gg_H1.jpg", width = 14, height = 12, dpi = 400)


