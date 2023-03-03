### Histogram of DV ###


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Figure 3: DV Histogram ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Data for histogram #
DV_Hist <- read_dta("data/DV_Hist.dta")


# Histogram of All Counts #
hist1 <- ggplot(DV_Hist, aes(x = no_troops)) +
  geom_histogram(binwidth = 200, color = "black") +
  xlab("Number of Troops in Grid-Cell") +
  ylab("Frequency of Troop Deployment Counts") +
  ggtitle("Histogram of Military Troops in Grid-Cell") +
  scale_x_continuous(n.breaks = 7) +
  theme(plot.title = element_text(hjust = 0.5, size = 40),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 35, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman"))
ggsave("gg_Hist_DV.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper",
       width = 14, height = 12, dpi = 400)
ggsave("Graphs/gg_Hist_DV.jpg", width = 14, height = 12, dpi = 400)
       


# Histogram of Non-0 Counts #
DV_Hist_2 <- subset(DV_Hist, no_troops > 0)

hist2 <- ggplot(DV_Hist_2, aes(x = no_troops)) +
  geom_histogram(binwidth = 200, color = "black") +
  scale_x_continuous(n.breaks = 7, breaks = c(1, 2000, 4000, 6000, 8000, 10000)) +
  #xlim(1, NA) + 
  xlab("Number of Troops in Grid-Cell") +
  ylab("Frequency of Troop Deployment Counts") +
  ggtitle("Histogram of Military Troops in Grid-Cell, No 0's") +
  theme(plot.title = element_text(hjust = 0.5, size = 40),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.5, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 35, margin = margin(15, 0, 10, 0)),
        axis.ticks.length = unit(10, "pt"),
        text = element_text(family = "Times New Roman"))
ggsave("gg_Hist_DV_No_0.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper",
       width = 14, height = 12, dpi = 400)
ggsave("Graphs/gg_Hist_DV_No_0.jpg", width = 14, height = 12, dpi = 400)

