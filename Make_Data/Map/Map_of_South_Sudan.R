rm(list=ls())
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(ggplot2)
library(units)
library(ggpubr)

#~~~~~~~~~~~~~~#
#### Set WD ####
#~~~~~~~~~~~~~~#

setwd("/Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Make_Data")


#~~~~~~~~~~~~~~~~~~~#
#### Import Data ####
#~~~~~~~~~~~~~~~~~~~#

PKO <- readRDS("Geo_PKO.rds")


#~~~~~~~~~~~~~~~~~~~~~~#
#### Get Shape File ####
#~~~~~~~~~~~~~~~~~~~~~~#

world <- ne_countries(scale = "medium", returnclass = "sf")

AF_sf <- world %>% filter(region_un == "Africa")


#~~~~~~~~~~~~~~~~~~~~~~~#
#### Get Troops Data ####
#~~~~~~~~~~~~~~~~~~~~~~~#

# South Sudan 2014 #
PKO2 <- subset(PKO, COW == 626)
PKO2 <- subset(PKO2, no_troops > 0)
PKO2 <- subset(PKO2, year == 2014)


# Make troops layer #
PKO2_troops <- PKO2 %>%
  select(mission, gid, xcoord, ycoord, no_troops) %>%
  mutate_at(vars(xcoord, ycoord), as.numeric) %>%
  group_by_at(vars(-no_troops)) %>%
  summarise(ave_no_troops = round(mean(as.numeric(no_troops), na.rm = TRUE)))


# Make battle deaths layer #
PKO2_best <- PKO2 %>%
  filter(best > 0) %>%
  select(mission, gid, xcoord, ycoord, best) %>%
  mutate_at(vars(xcoord, ycoord), as.numeric) %>%
  group_by_at(vars(-best)) %>%
  summarise(ave_best = round(mean(as.numeric(best), na.rm = TRUE))) %>%
  mutate(best_bin = cut(ave_best, breaks = c(0, 50, 100, Inf), labels = c("50", "100", "150"))) %>%
  mutate(best_bin = factor(x = best_bin, levels = c("50", "100", "150")))


# Subset AF_sf for South Sudan #
S_SUD <- subset(AF_sf, formal_en == 'Republic of South Sudan')

#~~~~~~~~~~~~~~~~~~#
#### Make Graph ####
#~~~~~~~~~~~~~~~~~~#

# Make map of South Sudan #
plot2 <- ggplot(data = S_SUD) + 
         geom_sf(data = S_SUD, color = "black", linetype = "solid", linewidth = 1.4) +
         geom_point(data = PKO2_troops, aes(x = xcoord, y = ycoord, size = ave_no_troops, color = factor(mission)),
                    shape = 2, stroke = 5) +
         scale_size(range = c(12, 42),
                    name = "Number of Troops") +
         labs(color = "Mission", shape = "Battle Deaths") +
         scale_color_manual(values = c("black", "snow4")) +
         geom_point(data = PKO2_best, aes(x = xcoord, y = ycoord, shape = best_bin), size = 10) + 
         ggtitle("Average Troop Deployments and Battle Deaths in South Sudan, 2014") +
         guides(color = guide_legend(override.aes = list(size = 12)),
                size = guide_legend(override.aes = list(size = c(5, 10, 15, 20))),
                shape = guide_legend(override.aes = list(size = 10))) +
         theme(plot.title = element_text(hjust = 0.5, size = 70),
               text = element_text(family = "Times New Roman"),
               axis.title.x = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               plot.margin = margin(1, 1, 1, 1, "mm"),
               legend.key.size = unit(3, "cm"),
               legend.title = element_text(size = 50),
               legend.text = element_text(size = 50),
               panel.border = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_rect(fill = "white")) 
ggsave("S_Sudan_map.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper", width = 40, height = 25, dpi = 300)
ggsave("S_Sudan_map.jpg", path = "/Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement", width = 40, height = 25, dpi = 300)

