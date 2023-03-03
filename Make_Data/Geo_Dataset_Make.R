#### Make data for Risk_Movement ####

#~~~~~~~~~~~~~~~~#
#### Packages ####
#~~~~~~~~~~~~~~~~#

rm(list=ls())
library(tidyverse)
library(readr)
library(ggplot2)
library(ggspatial)
library(countrycode)
library(foreign)
library(janitor)
library(stringr)
library(spdep)
library(sf)
library(expp)
library(RANN)
library(lubridate)
library(spatstat)
library(units)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(peacesciencer)
library(stringi)
library(readxl)
library(tictoc)
library(zoo)

#~~~~~~~~~~~~~~~~~~~~~~~~#
#### Import PRIO GRID ####
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Variables needed when importing prio grid yearly data, 1994 - 2014
# bdist1
# bdist2
# bdist3
# capdist
# droughtyr_speigdm
# excluded
# prec_gpcp
# gwno
# gwarea 
# nlights_mean


# Set working directory #
setwd("/Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Make_Data")


# Prio yearly variables #
PRIO_GRID <- read_csv("Prio_Grid/PRIO-GRID.csv")


# Prio static variables #
PRIO_GRID_STATIC <- read_csv("Prio_Grid/PRIO-GRID-Static.csv")


# Merge yearly and static variables #
PRIO_GRID <- PRIO_GRID %>%
  full_join(., PRIO_GRID_STATIC, by = c("gid"))


# Drop extra dataframe #
remove(PRIO_GRID_STATIC)


# Expand dataset, rename variables #
PRIO_GRID <- PRIO_GRID %>%
  mutate(month = 12) %>%
  uncount(month) %>%
  arrange(gid, year) %>%
  mutate(month = rep(1:12, times = 1361178)) %>%
  mutate(dist_cont_state = bdist1,
         dist_border_neigh = bdist2,
         dist_border_own = bdist3,
         dist_cap = capdist,
         total_land_area = landarea,
         mountains_prop = mountains_mean,
         time_nearest_urban = ttime_min,
         drought_prop = droughtyr_speigdm,
         excluded_groups = excluded,
         total_rain = prec_gpcp) %>%
  select(gid, month, year, gwno, gwarea, nlights_mean,
         dist_cont_state, dist_border_neigh, dist_border_own,
         dist_cap, time_nearest_urban, mountains_prop, total_land_area,
         drought_prop, excluded_groups, total_rain,
         -c(bdist1, bdist2, bdist3, capdist, row,
            col, landarea, ttime_min, mountains_mean,
            droughtyr_speigdm, excluded, prec_gpcp))


# Get ccodes #
PRIO_GRID <- PRIO_GRID %>%
  mutate(COW = countrycode(gwno, origin = "gwn", destination = "cown"))


# The coder does not have ccode for microstates #
PRIO_GRID$COW[PRIO_GRID$gwno == 54] <- 54
PRIO_GRID$COW[PRIO_GRID$gwno == 55] <- 55
PRIO_GRID$COW[PRIO_GRID$gwno == 56] <- 56
PRIO_GRID$COW[PRIO_GRID$gwno == 57] <- 57
PRIO_GRID$COW[PRIO_GRID$gwno == 58] <- 58
PRIO_GRID$COW[PRIO_GRID$gwno == 60] <- 60
PRIO_GRID$COW[PRIO_GRID$gwno == 340] <- 345
PRIO_GRID$COW[PRIO_GRID$gwno == 403] <- 403
PRIO_GRID$COW[PRIO_GRID$gwno == 491] <- 591
PRIO_GRID$COW[PRIO_GRID$gwno == 816] <- 816
PRIO_GRID$COW[PRIO_GRID$gwno == 935] <- 935
PRIO_GRID$COW[PRIO_GRID$gwno == 970] <- 946
PRIO_GRID$COW[PRIO_GRID$gwno == 971] <- 970
PRIO_GRID$COW[PRIO_GRID$gwno == 972] <- 955
PRIO_GRID$COW[PRIO_GRID$gwno == 973] <- 947
PRIO_GRID$COW[PRIO_GRID$gwno == 983] <- 983
PRIO_GRID$COW[PRIO_GRID$gwno == 987] <- 987
PRIO_GRID$COW[PRIO_GRID$gwno == 990] <- 990


# Refine dataset to essentials #
PRIO_GRID <- PRIO_GRID %>%
  select(-gwno) %>%
  select(gid, month, year, COW, gwarea, nlights_mean,
         dist_cont_state, dist_border_neigh,
         dist_border_own, dist_cap, mountains_prop, 
         total_land_area, mountains_prop,
         time_nearest_urban, drought_prop,
         excluded_groups, total_rain,)


#~~~~~~~~~~~~~~~~~~~~~~#
#### Import GEO-PKO ####
#~~~~~~~~~~~~~~~~~~~~~~#

# Geographical PKO dataset #
Geo <- readRDS("Geo_PKO/Geo-PKO-v-2-1.rds")


# Clean Geo dataset #
Geo <- Geo %>%
  select(prioid, mission, month, year, location, country,
         cow_code,latitude, longitude, zone.de.confidence,
         hq, no.troops, no.tcc, tcc1, tcc2, tcc3, tcc4, tcc5,
         tcc6, tcc7, tcc8, tcc9, tcc10, tcc11, tcc12, tcc13,
         tcc14, tcc15, tcc16, tcc17) %>%
  filter(year >= 1994 & year <= 2014)


# Fix Geo where there is 531,530 #
# These locations are in Eritrea
Geo$cow_code[Geo$location == "Badme" & Geo$prioid == 150916 & Geo$year == 2006] <- 531
Geo$cow_code[Geo$location == "Badme" & Geo$prioid == 150916 & Geo$year == 2007] <- 531
Geo$cow_code[Geo$location == "Badme" & Geo$prioid == 150916 & Geo$year == 2008] <- 531
Geo$cow_code[Geo$location == "Deda Lala" & Geo$prioid == 150916 & Geo$year == 2006] <- 531
Geo$cow_code[Geo$location == "Deda Lala" & Geo$prioid == 150916 & Geo$year == 2007] <- 531
Geo$cow_code[Geo$location == "Deda Lala" & Geo$prioid == 150916 & Geo$year == 2008] <- 531


# Remove cells where no.troops is unknown #
Geo <- subset(Geo, no.troops != "unknown")


# Alter variable classes #
Geo <- Geo %>%
  mutate(cow_code = as.numeric(cow_code),
         no.troops = as.numeric(no.troops),
         no.tcc = as.numeric(no.tcc))


# COW vector to filter for mission hosts in PRIO_GRID #
host <- as.numeric(unique(Geo$cow_code))
PRIO_GRID <- PRIO_GRID %>%
  filter(COW %in% host)
remove(host)


# Collapse by PRIO GRID ID, fix TCCs #
Geo <- Geo %>%
  group_by(prioid, month, year) %>%
  summarize(prioid = first(prioid),
            mission = first(mission),
            month = first(month),
            year = first(year),
            country = first(country),
            cow_code = first(cow_code),
            latitude = mean(latitude),
            longitude = mean(longitude),
            zone.de.confidence = first(zone.de.confidence),
            hq = max(hq),
            no.troops = sum(no.troops),
            no.tcc = sum(no.tcc),
            tcc1 = paste(tcc1, collapse = ","), tcc2 = paste(tcc2, collapse = ","),
            tcc3 = paste(tcc3, collapse = ","), tcc4 = paste(tcc4, collapse = ","),
            tcc5 = paste(tcc5, collapse = ","), tcc6 = paste(tcc6, collapse = ","),
            tcc7 = paste(tcc7, collapse = ","), tcc8 = paste(tcc8, collapse = ","),
            tcc9 = paste(tcc9, collapse = ","), tcc10 = paste(tcc10, collapse = ","),
            tcc11 = paste(tcc11, collapse = ","), tcc12 = paste(tcc12, collapse = ","),
            tcc13 = paste(tcc13, collapse = ","), tcc14 = paste(tcc14, collapse = ","),
            tcc15 = paste(tcc15, collapse = ","), tcc16 = paste(tcc16, collapse = ","),
            tcc17 = paste(tcc17, collapse = ",")) %>%
  ungroup()


# Need to separate the TCCs from the collapsed columns #
# Remove NA's. First as beginning or middle of series.
Geo$tcc1 <- str_replace_all(Geo$tcc1, pattern = "NA,", replacement = "")
Geo$tcc2 <- str_replace_all(Geo$tcc2, pattern = "NA,", replacement = "")
Geo$tcc3 <- str_replace_all(Geo$tcc3, pattern = "NA,", replacement = "")
Geo$tcc4 <- str_replace_all(Geo$tcc4, pattern = "NA,", replacement = "")
Geo$tcc5 <- str_replace_all(Geo$tcc5, pattern = "NA,", replacement = "")
Geo$tcc6 <- str_replace_all(Geo$tcc6, pattern = "NA,", replacement = "")
Geo$tcc7 <- str_replace_all(Geo$tcc7, pattern = "NA,", replacement = "")
Geo$tcc8 <- str_replace_all(Geo$tcc8, pattern = "NA,", replacement = "")
Geo$tcc9 <- str_replace_all(Geo$tcc9, pattern = "NA,", replacement = "")
Geo$tcc10 <- str_replace_all(Geo$tcc10, pattern = "NA,", replacement = "")
Geo$tcc11 <- str_replace_all(Geo$tcc11, pattern = "NA,", replacement = "")
Geo$tcc12 <- str_replace_all(Geo$tcc12, pattern = "NA,", replacement = "")
Geo$tcc13 <- str_replace_all(Geo$tcc13, pattern = "NA,", replacement = "")
Geo$tcc14 <- str_replace_all(Geo$tcc14, pattern = "NA,", replacement = "")
Geo$tcc15 <- str_replace_all(Geo$tcc15, pattern = "NA,", replacement = "")
Geo$tcc16 <- str_replace_all(Geo$tcc16, pattern = "NA,", replacement = "")
Geo$tcc17 <- str_replace_all(Geo$tcc17, pattern = "NA,", replacement = "")


# Second at the end of the series #
Geo$tcc1 <- str_replace_all(Geo$tcc1, pattern = ",NA", replacement = "")
Geo$tcc2 <- str_replace_all(Geo$tcc2, pattern = ",NA", replacement = "")
Geo$tcc3 <- str_replace_all(Geo$tcc3, pattern = ",NA", replacement = "")
Geo$tcc4 <- str_replace_all(Geo$tcc4, pattern = ",NA", replacement = "")
Geo$tcc5 <- str_replace_all(Geo$tcc5, pattern = ",NA", replacement = "")
Geo$tcc6 <- str_replace_all(Geo$tcc6, pattern = ",NA", replacement = "")
Geo$tcc7 <- str_replace_all(Geo$tcc7, pattern = ",NA", replacement = "")
Geo$tcc8 <- str_replace_all(Geo$tcc8, pattern = ",NA", replacement = "")
Geo$tcc9 <- str_replace_all(Geo$tcc9, pattern = ",NA", replacement = "")
Geo$tcc10 <- str_replace_all(Geo$tcc10, pattern = ",NA", replacement = "")
Geo$tcc11 <- str_replace_all(Geo$tcc11, pattern = ",NA", replacement = "")
Geo$tcc12 <- str_replace_all(Geo$tcc12, pattern = ",NA", replacement = "")
Geo$tcc13 <- str_replace_all(Geo$tcc13, pattern = ",NA", replacement = "")
Geo$tcc14 <- str_replace_all(Geo$tcc14, pattern = ",NA", replacement = "")
Geo$tcc15 <- str_replace_all(Geo$tcc15, pattern = ",NA", replacement = "")
Geo$tcc16 <- str_replace_all(Geo$tcc16, pattern = ",NA", replacement = "")
Geo$tcc17 <- str_replace_all(Geo$tcc17, pattern = ",NA", replacement = "")


# Split tcc variables to have codes one at a time #
Geo <- Geo %>%
  separate(tcc1, paste('tcc1', 1:19, sep = "_"), sep = ",") %>%
  separate(tcc2, paste('tcc2', 1:4, sep = "_"), sep = ",") %>%
  separate(tcc3, paste('tcc3', 1:3, sep = "_"), sep = ",") %>%
  separate(tcc4, paste('tcc4', 1:3, sep = "_"), sep = ",") %>%
  separate(tcc5, paste('tcc5', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc6, paste('tcc6', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc7, paste('tcc7', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc8, paste('tcc8', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc9, paste('tcc9', 1:2, sep = "_"), sep = ",")


# Rename the variables made from the splits #
Geo <- Geo %>%
  mutate(tcc1 = tcc1_1, tcc2 = tcc1_2, tcc3 = tcc1_3, tcc4 = tcc1_4,
         tcc5 = tcc1_5, tcc6 = tcc1_6, tcc7 = tcc1_7, tcc8 = tcc1_8,
         tcc9 = tcc1_9, tcc10 = tcc1_10, tcc11 = tcc1_11, tcc12 = tcc1_12,
         tcc13 = tcc1_13, tcc14 = tcc1_14, tcc15 = tcc1_15, tcc16 = tcc1_16,
         tcc17 = tcc1_17, tcc18 = tcc1_18, tcc19 = tcc1_19, tcc20 = tcc2_1,
         tcc21 = tcc2_2, tcc22 = tcc2_3, tcc23 = tcc2_4, tcc24 = tcc3_1,
         tcc25 = tcc3_2, tcc26 = tcc3_3, tcc27 = tcc4_1, tcc28 = tcc4_2,
         tcc29 = tcc4_3, tcc30 = tcc5_1, tcc31 = tcc5_2, tcc32 = tcc6_1,
         tcc33 = tcc6_2, tcc34 = tcc7_1, tcc35 = tcc7_2, tcc36 = tcc8_1, 
         tcc37 = tcc8_2, tcc38 = tcc9_1, tcc39 = tcc9_2, tcc40 = tcc10,
         tcc41 = tcc11, tcc42 = tcc12, tcc43 = tcc13, tcc44 = tcc14,
         tcc45 = tcc15, tcc46 = tcc16, tcc47 = tcc17) %>%
  select(-tcc1_1, -tcc1_2, -tcc1_3, -tcc1_4, -tcc1_5, -tcc1_6, -tcc1_7,
         -tcc1_8, -tcc1_9, -tcc1_10, -tcc1_11, -tcc1_12, -tcc1_13, -tcc1_14,
         -tcc1_15, -tcc1_16, -tcc1_17, -tcc1_18, -tcc1_19, -tcc2_1, -tcc2_2,
         -tcc2_3, -tcc2_4, -tcc3_1, -tcc3_2, -tcc3_3, -tcc4_1, -tcc4_2, -tcc4_3,
         -tcc5_1, -tcc5_2, -tcc6_1, -tcc6_2, -tcc7_1, -tcc7_2, -tcc8_1, -tcc8_2,
         -tcc9_1, -tcc9_2)


# Merge PKOs with PRIO_GRID #
Geo_PKO <- PRIO_GRID %>%
  left_join(Geo, by = c("gid" = "prioid", "month", "year"))
remove(Geo, PRIO_GRID)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Remove Cells Without Mission Presence ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Only include observations within year scope of mission 
### I will include 1 month pre-deployment and 1 month after leaving

#~~~~~~~~#
### 41 ###
#~~~~~~~~#

### UNMIH 9 1993 - 6 1996 ###
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 41 & year < 1993)) %>%
  filter(!(COW == 41 & year == 1993 & month < 8))

Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year <= 1995 & is.na(Geo_PKO$mission)] <- "UNMIH"
Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 1996 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "UNMIH"

# Did not set ending range due to start of next mission #

### UNSMIH 7 1996 - 7 1997 ###
# No mission start since it starts right after UNMIH

Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 1996 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "UNSMIH"
Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 1997 & Geo_PKO$month <= 8 & is.na(Geo_PKO$mission)] <- "UNSMIH"

# Drop months 9 and 10 to keep one month after UNSMIH and one before MIPONUH
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 41 & year == 1997 & month == 9)) %>%
  filter(!(COW == 41 & year == 1997 & month == 10))

### MIPONUH 12 1997 - 3 2000 ###
# No start portion since already set up with previous line of code

Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 1997 & Geo_PKO$month >= 11 & is.na(Geo_PKO$mission)] <- "MIPONUH"
Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 1998 & is.na(Geo_PKO$mission)] <- "MIPONUH"
Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 1999 & is.na(Geo_PKO$mission)] <- "MIPONUH"
Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 2000 & Geo_PKO$month <= 4 & is.na(Geo_PKO$mission)] <- "MIPONUH"


# Remove time in between MIPONUH and MINUSTAH 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 41 & year == 2000 & month > 4)) %>%
  filter(!(COW == 41 & year == 2001)) %>%
  filter(!(COW == 41 & year == 2002)) %>%
  filter(!(COW == 41 & year == 2003)) %>%
  filter(!(COW == 41 & year == 2004 & month < 5))

### MINUSTAH 6 2004 - 10 2017 ###
# No ending time since dataset ends in 2014

Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year == 2004 & Geo_PKO$month >= 5 & is.na(Geo_PKO$mission)] <- "MINUSTAH"
Geo_PKO$mission[Geo_PKO$COW == 41 & Geo_PKO$year >= 2005 & is.na(Geo_PKO$mission)] <- "MINUSTAH"


#~~~~~~~~#
### 90 ###
#~~~~~~~~#

### MINUGUA 9 1994 - 11 2004
# Pre 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 90 & year < 1994)) %>%
  filter(!(COW == 90 & year == 1994 & month < 8))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 90 & year >= 2005))

Geo_PKO$mission[Geo_PKO$COW == 90 & is.na(Geo_PKO$mission)] <- "MINUGUA"


#~~~~~~~~~#
### 341 ###
#~~~~~~~~~#

# Remove 341

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 341))


#~~~~~~~~~#
### 343 ###
#~~~~~~~~~#

### UNPROFOR 12 1992 - 3 1995 ###
# No start entry since the mission is ongoing pre-1994

Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1994 & is.na(Geo_PKO$mission)] <- "UNPROFOR"
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1995 & Geo_PKO$month <= 3 & is.na(Geo_PKO$mission)] <- "UNPROFOR"

### UNPREDEP 4 1995 - 5 1999 ###
# No start time since mission begins right after UNPREDEP

Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1995 & Geo_PKO$month >= 4 & is.na(Geo_PKO$mission)] <- "UNPREDEP"
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1996 & is.na(Geo_PKO$mission)] <- "UNPREDEP"
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1997 & is.na(Geo_PKO$mission)] <- "UNPREDEP"
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1998 & is.na(Geo_PKO$mission)] <- "UNPREDEP"
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1999 & Geo_PKO$month <= 5 & is.na(Geo_PKO$mission)] <- "UNPREDEP"

### UNMIK 6 1999 - 2022 ###
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year == 1999 & Geo_PKO$month >= 6 & is.na(Geo_PKO$mission)] <- "UNMIK"
Geo_PKO$mission[Geo_PKO$COW == 343 & Geo_PKO$year >= 2000 & is.na(Geo_PKO$mission)] <- "UNMIK"


#~~~~~~~~~#
### 344 ###
#~~~~~~~~~#

### UNPROFOR 12 1992 - 2 1995 ###
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year <= 1994 & is.na(Geo_PKO$mission)] <- "UNPROFOR"
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year == 1995 & Geo_PKO$month <= 2& is.na(Geo_PKO$mission)] <- "UNPROFOR"


### UNCRO 3 1995 - 1 1996 ###
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year == 1995 & Geo_PKO$month >= 3 & is.na(Geo_PKO$mission)] <- "UNCRO"
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year == 1996 & Geo_PKO$month == 1 & is.na(Geo_PKO$mission)] <- "UNCRO"


### UNMIBH 12 1995 - 12 2002 ### 
grid_list <- Geo_PKO %>%
  filter(COW == 344 & mission == "UNMIBH") %>%
  distinct(gid)

grid_list <- grid_list %>%
  pull(var = gid)

for(g in grid_list) {
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 1995 & Geo_PKO$month >= 12 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 1996 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 1997 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 1998 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 1999 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 2000 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 2001 & is.na(Geo_PKO$mission)] <- "UNMIBH"
  Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$gid == g & Geo_PKO$year == 2002 & is.na(Geo_PKO$mission)] <- "UNMIBH"
}

remove(grid_list)


### UNTAES 2 1996 - 1 1998 ###
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year == 1996 & Geo_PKO$month >= 2 & is.na(Geo_PKO$mission)] <- "UNTAES"
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year == 1997 & is.na(Geo_PKO$mission)] <- "UNTAES"
Geo_PKO$mission[Geo_PKO$COW == 344 & Geo_PKO$year == 1998 & Geo_PKO$month == 1 & is.na(Geo_PKO$mission)] <- "UNTAES"

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 344 & year == 2003 & month > 1)) %>%
  filter(!(COW == 344 & year >=2004))


#~~~~~~~~~#
### 346 ###
#~~~~~~~~~#

### UNPROFOR 12 1992 - 12 1995 ###
Geo_PKO$mission[Geo_PKO$COW == 346 & Geo_PKO$year <= 1995 & is.na(Geo_PKO$mission)] <- "UNPROFOR"


### UNMIBH 1 1996 - 12 2002 ###
# Will be assumed to preside over the mission host
Geo_PKO$mission[Geo_PKO$COW == 346 & Geo_PKO$year == 1996 & Geo_PKO$gid != 191197 & Geo_PKO$month >= 1 & is.na(Geo_PKO$mission)] <- "UNMIBH"
Geo_PKO$mission[Geo_PKO$COW == 346 & Geo_PKO$year >= 1997 & Geo_PKO$gid != 191197 & is.na(Geo_PKO$mission)] <- "UNMIBH"


### UNMOP 2 1996 - 12 2002 ### 
# Only gets cell 191197
Geo_PKO$mission[Geo_PKO$COW == 346 & Geo_PKO$year == 1996 & Geo_PKO$gid == 191197 & Geo_PKO$month >= 2 & is.na(Geo_PKO$mission)] <- "UNMOP"
Geo_PKO$mission[Geo_PKO$COW == 346 & Geo_PKO$year >= 1997 & Geo_PKO$gid == 191197 & is.na(Geo_PKO$mission)] <- "UNMOP"

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 346 & year >= 2003))

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 346 & year == 1996 & month == 1 & gid == 191197))


#~~~~~~~~~#
### 347 ###
#~~~~~~~~~#

### UNMIK 6 1999 - 2022 ###

# Pre
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 347 & year < 1999)) %>%
  filter(!(COW == 347 & year == 1999 & month < 5))

Geo_PKO$mission[Geo_PKO$COW == 347 & Geo_PKO$year == 1999 & Geo_PKO$month >= 5 & is.na(Geo_PKO$mission)] <- "UNMIK"
Geo_PKO$mission[Geo_PKO$COW == 347 & Geo_PKO$year >= 2000 & is.na(Geo_PKO$mission)] <- "UNMIK"


#~~~~~~~~~#
### 352 ###
#~~~~~~~~~#

### UNFICYP 3 1964 - 2022 ###
Geo_PKO$mission[Geo_PKO$COW == 352 & is.na(Geo_PKO$mission)] <- "UNFICYP"


#~~~~~~~~~#
### 372 ###
#~~~~~~~~~#

### UNOMIG 8 1993 - 6 2003 ###

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 372 & year == 2003 & month > 7)) %>%
  filter(!(COW == 372 & year > 2003))

Geo_PKO$mission[Geo_PKO$COW == 372 & is.na(Geo_PKO$mission)] <- "UNOMIG"


#~~~~~~~~~#
### 432 ###
#~~~~~~~~~#

### MINUSMA 4 2013 - Today ###
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 432 & year < 2013)) %>%
  filter(!(COW == 432 & year == 2013 & month < 3))

Geo_PKO$mission[Geo_PKO$COW == 432 & is.na(Geo_PKO$mission)] <- "MINUSMA"


#~~~~~~~~~#
### 435 ###
#~~~~~~~~~#

### MINURSO 9 1991 - 2022 ###
Geo_PKO$mission[Geo_PKO$COW == 435 & is.na(Geo_PKO$mission)] <- "MINURSO"


#~~~~~~~~~#
### 437 ###
#~~~~~~~~~#

### MINUCI 5 2003 - 4 2004 ###
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 437 & year < 2003)) %>%
  filter(!(COW == 437 & year == 2003 & month < 4))

Geo_PKO$mission[Geo_PKO$COW == 437 & Geo_PKO$year == 2003 & is.na(Geo_PKO$mission)] <- "MINUCI"
Geo_PKO$mission[Geo_PKO$COW == 437 & Geo_PKO$year == 2004 & Geo_PKO$month <= 4 & is.na(Geo_PKO$mission)] <- "MINUCI"


### UNOCI 4 2004 - 6 2017 ###
Geo_PKO$mission[Geo_PKO$COW == 437 & Geo_PKO$year == 2004 & Geo_PKO$month > 4 & is.na(Geo_PKO$mission)] <- "UNOCI"
Geo_PKO$mission[Geo_PKO$COW == 437 & Geo_PKO$year >= 2005 & is.na(Geo_PKO$mission)] <- "UNOCI"

### UNOMIL and UNMIL were listed as taking place in the gridsquare within the Ivory Coast, but 437 
### naming will only apply to UNOCI and MINUCI


#~~~~~~~~~#
### 450 ###
#~~~~~~~~~#

### UNOMIL 9 1993 - 9 1997 ###
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 450 & year == 1997 & month > 10)) %>%
  filter(!(COW == 450 & year == 1998)) %>%
  filter(!(COW == 450 & year == 1999)) %>%
  filter(!(COW == 450 & year == 2000)) %>%
  filter(!(COW == 450 & year == 2001)) %>%
  filter(!(COW == 450 & year == 2002)) %>%
  filter(!(COW == 450 & year == 2003 & month < 10))

Geo_PKO$mission[Geo_PKO$COW == 450 & Geo_PKO$year < 1997 & is.na(Geo_PKO$mission)] <- "UNOMIL"
Geo_PKO$mission[Geo_PKO$COW == 450 & Geo_PKO$year == 1997 & Geo_PKO$month <= 10 & is.na(Geo_PKO$mission)] <- "UNOMIL"
  

### UNMIL 9 2003 - 3 2018 ###
Geo_PKO$mission[Geo_PKO$COW == 450 & Geo_PKO$year == 2003 & Geo_PKO$month >= 9 & is.na(Geo_PKO$mission)] <- "UNMIL"
Geo_PKO$mission[Geo_PKO$COW == 450 & Geo_PKO$year >= 2004 & is.na(Geo_PKO$mission)] <- "UNMIL"


### UNAMSIL
## Sierra Leone, but I'll leave it, but not name any others with the mission name

#~~~~~~~~~#
### 451 ###
#~~~~~~~~~#

### UNOMSIL 7 1998 - 10 1999

Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 451 & year < 1998)) %>%
  filter(!(COW == 451 & year == 1998 & month < 6))

Geo_PKO$mission[Geo_PKO$COW == 451 & Geo_PKO$year == 1998 & Geo_PKO$month >= 6 & is.na(Geo_PKO$mission)] <- "UNOMSIL"
Geo_PKO$mission[Geo_PKO$COW == 451 & Geo_PKO$year == 1999 & Geo_PKO$month <= 9 & is.na(Geo_PKO$mission)] <- "UNOMSIL"


### UNAMSIL 10 1999 - 12 2005
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 451 & year == 2006 & month > 1)) %>%
  filter(!(COW == 451 & year > 2007))

Geo_PKO$mission[Geo_PKO$COW == 451 & Geo_PKO$year == 1999 & Geo_PKO$month >= 10 & is.na(Geo_PKO$mission)] <- "UNAMSIL"
Geo_PKO$mission[Geo_PKO$COW == 451 & Geo_PKO$year >= 2000 & is.na(Geo_PKO$mission)] <- "UNAMSIL"


### UNIOSIL 
### Observer mission. Not likely to move troops. 


### UNOMIL 
## Mostly in Liberia


### UNMIL
## Mostly in Liberia. 


#~~~~~~~~~#
### 482 ###
#~~~~~~~~~#

### MINURCAT 3 2008 - 12 2010 ###
#Pre
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 482 & year <= 2006)) %>%
  filter(!(COW == 482 & year == 2007 & month < 9))

# Post 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 482 & year == 2011 & month >= 1)) %>%
  filter(!(COW == 482 & year == 2012)) %>%
  filter(!(COW == 482 & year == 2013)) %>%
  filter(!(COW == 482 & year == 2014 & month < 3))

Geo_PKO$mission[Geo_PKO$COW == 482 & Geo_PKO$year == 2008 & Geo_PKO$month >= 2 & is.na(Geo_PKO$mission)] <- "MINURCAT"
Geo_PKO$mission[Geo_PKO$COW == 482 & Geo_PKO$year == 2009 & is.na(Geo_PKO$mission)] <- "MINURCAT"
Geo_PKO$mission[Geo_PKO$COW == 482 & Geo_PKO$year == 2010 & is.na(Geo_PKO$mission)] <- "MINURCAT"


### MINUSCA 4 2014 - 2022 ###
Geo_PKO$mission[Geo_PKO$COW == 482 & Geo_PKO$year == 2014 & Geo_PKO$month >= 3 & is.na(Geo_PKO$mission)] <- "MINUSCA"


### UNMISS
## Another cross over group of cells


#~~~~~~~~~#
### 483 ###
#~~~~~~~~~#

### MINURCAT 3 2008 - 12 2010 ###
# Pre
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 483 & year <= 2007)) %>%
  filter(!(COW == 483 & year == 2008 & month < 2))

# Post
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 483 & year == 2011 & month > 1)) %>%
  filter(!(COW == 483 & year >= 2012))

Geo_PKO$mission[Geo_PKO$COW == 483 & Geo_PKO$year == 2008 & is.na(Geo_PKO$mission)] <- "MINURCAT"
Geo_PKO$mission[Geo_PKO$COW == 483 & Geo_PKO$year == 2009 & is.na(Geo_PKO$mission)] <- "MINURCAT"
Geo_PKO$mission[Geo_PKO$COW == 483 & Geo_PKO$year == 2010 & is.na(Geo_PKO$mission)] <- "MINURCAT"
Geo_PKO$mission[Geo_PKO$COW == 483 & Geo_PKO$year == 2011 & Geo_PKO$month == 1 & is.na(Geo_PKO$mission)] <- "MINURCAT"


### UNAMID ###
# Spill over cells from Sudan 


#~~~~~~~~~#
### 490 ###
#~~~~~~~~~#

### MONUC 11 1999 - 6 2010
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 490 & year < 1999)) %>%
  filter(!(COW == 490 & year == 1999 & month < 5))

Geo_PKO$mission[Geo_PKO$COW == 490 & Geo_PKO$year < 2010 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 490 & Geo_PKO$year == 2010 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "MONUC"


### MONUSCO 7 2010 - 2022
# No need to add code. The mission goes past 2014
Geo_PKO$mission[Geo_PKO$COW == 490 & Geo_PKO$year == 2010 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "MONUSCO"
Geo_PKO$mission[Geo_PKO$COW == 490 & Geo_PKO$year >= 2011 & is.na(Geo_PKO$mission)] <- "MONUSCO"


### UNAVEM III ###
# Spill over from Angola 


### UNAMIR
# Spill over from Rwanda


### MINURCA 
# Spill over from CAR


### MONUA 
# Spill over form Angola 


### MINUSCA 
# Spill over from CAR


#~~~~~~~~~#
### 500 ###
#~~~~~~~~~#

### UNOMUR 6 1993 - 9 1994 
# No pre code

# Post
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 500 & year == 1994 & month > 10)) %>%
  filter(!(COW == 500 & year == 1995)) %>%
  filter(!(COW == 500 & year == 1996)) %>%
  filter(!(COW == 500 & year == 1997)) %>%
  filter(!(COW == 500 & year == 1998)) %>%
  filter(!(COW == 500 & year == 1999 & month < 10))

Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 1994 & Geo_PKO$month <= 10 & is.na(Geo_PKO$mission)] <- "UNOMUR"


### MONUC 11 1999 - 6 2010 ###
# No pre code 

# No post code
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 1999 & Geo_PKO$month >= 10 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2000 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2001 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2002 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2003 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2004 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2005 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2006 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2007 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2008 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2009 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2010 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "MONUC"


### MONUSCO 7 2010 - 2022
# No need to add code. The mission goes past 2014
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year == 2010 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "MONUSCO"
Geo_PKO$mission[Geo_PKO$COW == 500 & Geo_PKO$year >= 2011 & is.na(Geo_PKO$mission)] <- "MONUSCO"


#~~~~~~~~~#
### 516 ###
#~~~~~~~~~#

### ONUB 6 2004 - 12 2006
# Being an observer mission, I will assume that all cells have the potential 
# to be MONUC cells rather than ONUB cells


### MONUC 11 1999 - 6 2010
# Pre
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 516 & year < 1999)) %>%
  filter(!(COW == 516 & year == 1999 & month < 10))

Geo_PKO$mission[Geo_PKO$COW == 516 & Geo_PKO$year < 2010 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 516 & Geo_PKO$year == 2010 & Geo_PKO$month <= 7 & is.na(Geo_PKO$mission)] <- "MONUC"

# Post
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 516 & year == 2010 & month > 7)) %>%
  filter(!(COW == 516 & year >= 2011))


### BINUB 
### Observer mission. Likely to not be mission responsible for troops. 


### UNAMIR
# Spill over from Rwanda


### MONUSCO
# Spill over from DRC


#~~~~~~~~~#
### 517 ###
#~~~~~~~~~#

### UNAMIR 10 1993 - 3 1996 
# Post
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 517 & year == 1996 & month > 4)) %>%
  filter(!(COW == 517 & year == 1997)) %>%
  filter(!(COW == 517 & year == 1998)) %>%
  filter(!(COW == 517 & year == 1999 & month < 10))

Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year <= 1996 & is.na(Geo_PKO$mission)] <- "UNAMIR"


### MONUC 11 1999 - 6 2010
# No pre code needed
# No post coded needed

Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 1999 & Geo_PKO$month >= 10 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2000 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2001 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2002 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2003 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2004 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2005 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2006 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2007 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2008 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2009 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2010 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "MONUC"


### MONUSCO 7 2010 - 2022
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year == 2010 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "MONUSCO"
Geo_PKO$mission[Geo_PKO$COW == 517 & Geo_PKO$year >= 2011 & is.na(Geo_PKO$mission)] <- "MONUSCO"


### UNOMUR
# Mostly Uganda. Observer mission. 


#~~~~~~~~~#
### 520 ###
#~~~~~~~~~#

### UNOSOM II 3 1993 - 3 1995 ###
# No pre code

# Post
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 520 & year == 1995 & month > 4)) %>%
  filter(!(COW == 520 & year >= 1996))

Geo_PKO$mission[Geo_PKO$COW == 520 & is.na(Geo_PKO$mission)] <- "UNOSOM II"


#~~~~~~~~~#
### 530 ###
#~~~~~~~~~#

### MONUC 11 1999 - 6 2010 ###
# In 1 cell
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 530 & year < 1999 & is.na(mission))) %>%
  filter(!(COW == 530 & year == 1999 & month < 10 & is.na(mission)))

Geo_PKO$mission[Geo_PKO$COW == 530 & Geo_PKO$year == 1999 & is.na(Geo_PKO$mission)] <- "MONUC"


### UNMIS 3 2005 - 7 2011 ###
# In 1 cell


### UNMEE 9 2000 - 7 2008 ###
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 530 & year == 2000 & month < 9))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 530 & year == 2008 & month > 7 & is.na(mission))) %>%
  filter(!(COW == 530 & year >= 2009 & is.na(mission)))

Geo_PKO$mission[Geo_PKO$COW == 530 & Geo_PKO$year >= 2000 & is.na(Geo_PKO$mission)] <- "UNMEE"


#~~~~~~~~~#
### 531 ###
#~~~~~~~~~#

### UNMEE 7 2000 - 7 2008 ###
# Pre
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 531 & year < 2000)) %>%
  filter(!(COW == 531 & year == 2000 & month < 6))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 531 & year == 2008 & month > 8)) %>%
  filter(!(COW == 531 & year >= 2009))
  
Geo_PKO$mission[Geo_PKO$COW == 531 & is.na(Geo_PKO$mission)] <- "UNMEE"


#~~~~~~~~~#
### 540 ###
#~~~~~~~~~#

### UNAVEM II 5 1991 - 2 1995 ###
# No pre code
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year < 1995 & is.na(Geo_PKO$mission)] <- "UNAVEM II"
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1995 & Geo_PKO$month <= 2 & is.na(Geo_PKO$mission)] <- "UNAVEM II"


### UNAVEM III 2 1995 - 6 1997 ### 
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1995 & Geo_PKO$month >= 3 & is.na(Geo_PKO$mission)] <- "UNAVEM III"
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1996 & is.na(Geo_PKO$mission)] <- "UNAVEM III"
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1997 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "UNAVEM III"


### MONUA 6 1997 - 2 1999 ###
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1997 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "MONUA"
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1998 & is.na(Geo_PKO$mission)] <- "MONUA"
Geo_PKO$mission[Geo_PKO$COW == 540 & Geo_PKO$year == 1999 & Geo_PKO$month <= 3 & is.na(Geo_PKO$mission)] <- "MONUA"

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 540 & year == 1999 & month > 3)) %>%
  filter(!(COW == 540 & year >= 2000))


#~~~~~~~~~#
### 541 ###
#~~~~~~~~~#

### ONUMOZ 12 1992 - 12 1994 ###
# No pre code

# Post
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 541 & year == 1995 & month > 1)) %>%
  filter(!(COW == 541 & year >= 1996))

Geo_PKO$mission[Geo_PKO$COW == 541 & is.na(Geo_PKO$mission)] <- "ONUMOZ"


#~~~~~~~~~#
### 551 ###
#~~~~~~~~~#

### MONUC 11 1999 - 6 2010 ###
# But MONUC only in Zambia 5 2004
Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 551 & year < 1999)) %>%
  filter(!(COW == 551 & year == 1999 & month < 10))

Geo_PKO <- Geo_PKO %>% 
  filter(!(COW == 551 & year == 2004 & month > 6)) %>%
  filter(!(COW == 551 & year >= 2005))

Geo_PKO$mission[Geo_PKO$COW == 551 & is.na(Geo_PKO$mission)] <- "MONUC"


#~~~~~~~~~#
### 552 ###
#~~~~~~~~~#

### MONUC 11 1999 - 6 2010 ###
# But MONUC only in Zimbabwe 11 1999 - 12 2002
# Pre
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 552 & year < 1999 )) %>%
  filter(!(COW == 552 & year == 1999 & month < 10))
  
# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 552 & year == 2003 & month > 1)) %>%
  filter(!(COW == 552 & year >= 2004)) 

Geo_PKO$mission[Geo_PKO$COW == 552 & is.na(Geo_PKO$mission)] <- "MONUC"
  

#~~~~~~~~~#
### 565 ###
#~~~~~~~~~#

### UNAVEM III 2 1995 - 6 1997 ### 
# Pre
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 565 & year < 1995)) %>%
  filter(!(COW == 565 & year == 1995 & month < 1))

Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1995 & is.na(Geo_PKO$mission)] <- "UNAVEM III"
Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1996 & is.na(Geo_PKO$mission)] <- "UNAVEM III"
Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1997 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "UNAVEM III"


### MONUA 6 1997 - 2 1999 ###
# No pre code
Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1997 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "MONUA"
Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1998 & is.na(Geo_PKO$mission)] <- "MONUA"
Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1999 & Geo_PKO$month <=3 & is.na(Geo_PKO$mission)] <- "MONUA"

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 565 & year == 1999 & month > 3 & month < 10))


### MONUC 11 1999 - 6 2010 ###
# No pre code

# Post 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 565 & year == 2010 & month > 7)) %>%
  filter(!(COW == 565 & year > 2011))

Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year == 1999 & Geo_PKO$month >= 10 & is.na(Geo_PKO$mission)] <- "MONUC"
Geo_PKO$mission[Geo_PKO$COW == 565 & Geo_PKO$year >= 2000 & is.na(Geo_PKO$mission)] <- "MONUC"


#~~~~~~~~~#
### 600 ###
#~~~~~~~~~#

### MINURSO 9 1991 to 2022 ###
# No pre or post code

Geo_PKO$mission[Geo_PKO$COW == 600 & is.na(Geo_PKO$mission)] <- "MINURSO"


#~~~~~~~~~#
### 615 ###
#~~~~~~~~~#

### MINURSO 9 1991 to 2022 ###
# Mostly in 600, but consistently in a few cells 
# No pre or post code

Geo_PKO$mission[Geo_PKO$COW == 615 & is.na(Geo_PKO$mission)] <- "MINURSO"


#~~~~~~~~~#
### 625 ###
#~~~~~~~~~#

### UNISFA 6 2011 - today ###
# Since regional, I will limit to grid cells 143697, 143698, 144417, 145860
grid_list <- Geo_PKO %>%
  filter(COW == 625 & mission == "UNISFA") %>%
  distinct(gid)

grid_list <- grid_list %>%
  pull(var = gid)

for(g in grid_list) {
  Geo_PKO <- Geo_PKO %>%
    filter(!(COW == 625 & gid == g & year < 2011)) %>%
    filter(!(COW == 625 & gid == g & year == 2011 & month < 5))
Geo_PKO$mission[Geo_PKO$COW == 625 & Geo_PKO$gid == g & Geo_PKO$year == 2011 & Geo_PKO$month >= 5 & is.na(Geo_PKO$mission)] <- "UNISFA"
Geo_PKO$mission[Geo_PKO$COW == 625 & Geo_PKO$gid == g & Geo_PKO$year == 2012 & is.na(Geo_PKO$mission)] <- "UNISFA"
Geo_PKO$mission[Geo_PKO$COW == 625 & Geo_PKO$gid == g & Geo_PKO$year == 2013 & is.na(Geo_PKO$mission)] <- "UNISFA"
Geo_PKO$mission[Geo_PKO$COW == 625 & Geo_PKO$gid == g & Geo_PKO$year == 2014 & is.na(Geo_PKO$mission)] <- "UNISFA"
}


### UNAMID Authorized 7 2007, but deployed 1 2008 - 2022 ###
# Since regional, I will limit to grid cells in list

grid_list2 <- Geo_PKO %>%
  filter(COW == 625 & mission == "UNAMID") %>%
  distinct(gid)

grid_list2 <- grid_list2 %>%
  pull(var = gid)

for(g in grid_list2) 
{
  Geo_PKO <- Geo_PKO %>%
    filter(!(COW == 625 & gid == g & year < 2007)) %>%
    filter(!(COW == 625 & gid == g & year == 2007 & month < 7))
Geo_PKO$mission[Geo_PKO$COW == 625 & Geo_PKO$gid == g & Geo_PKO$year == 2007 & Geo_PKO$month >= 7 & is.na(Geo_PKO$mission)] <- "UNAMID"
Geo_PKO$mission[Geo_PKO$COW == 625 & Geo_PKO$gid == g & Geo_PKO$year >= 2008 & is.na(Geo_PKO$mission)] <- "UNAMID"
}


### UNMIS 3 2005 - 7 2011 ###
# Assume larger reach through Sudan
grid_list_sudan <- c(grid_list, grid_list2)

for(g in grid_list_sudan)
{
  ifelse(Geo_PKO$gid[Geo_PKO$COW == 625] %in% g,
         print("Next"),
         Geo_PKO <- Geo_PKO %>%
           filter(!(COW == 625 & year < 2005 & gid != g)) %>%
           filter(!(COW == 625 & year == 2005 & gid != g & month <= 2)) %>%
           filter(!(COW == 625 & year == 2011 & gid != g & month > 8)) %>%
           filter(!(COW == 625 & year >= 2012 & gid != g)))
}
  
Geo_PKO$mission[Geo_PKO$COW == 625 & is.na(Geo_PKO$mission)] <- "UNMIS"

remove(g, grid_list, grid_list2, grid_list_sudan)


#~~~~~~~~~#
### 626 ###
#~~~~~~~~~#

### UNMIS 3 2005 - 7 2011 ###
# Pre
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 626 & year <= 2004)) %>%
  filter(!(COW == 626 & year == 2005 & month < 2))

Geo_PKO$mission[Geo_PKO$COW == 626 & Geo_PKO$year <= 2010  & is.na(Geo_PKO$mission)] <- "UNMIS"
Geo_PKO$mission[Geo_PKO$COW == 626 & Geo_PKO$year == 2011 & Geo_PKO$month <= 6 & is.na(Geo_PKO$mission)] <- "UNMIS"


### UNISFA 6 2011 - today ###
grid_list_UNISFA <- Geo_PKO %>%
  filter(COW == 626 & mission == "UNISFA") %>%
  distinct(gid)

grid_list_UNISFA <- grid_list_UNISFA %>%
  pull(var = gid)

for(g in grid_list_UNISFA) {
  ifelse(Geo_PKO$gid[Geo_PKO$COW == 626] %in% g,
         Geo_PKO$mission[(Geo_PKO$COW == 626 & Geo_PKO$year == 2011 & Geo_PKO$month >= 6 & is.na(Geo_PKO$mission)) |
           (Geo_PKO$COW == 626 & Geo_PKO$year >= 2012 & is.na(Geo_PKO$mission))] <- "UNISFA",
         Geo_PKO$mission[(Geo_PKO$COW == 626 & Geo_PKO$year == 2011 & Geo_PKO$month >= 6 & is.na(Geo_PKO$mission)) |
           (Geo_PKO$COW == 626 & Geo_PKO$year >= 2012 & is.na(Geo_PKO$mission))] <- "UNMISS")
}


### UNMISS 7 2011 - today ### 
# Code above names the cells for UNMISS. 
# If in list, named UNISFA. If not, named UNMISS.

remove(g, grid_list_UNISFA)

#~~~~~~~~~#
### 645 ###
#~~~~~~~~~#

### UNIKOM 4 1991 - 10 2003 ###
# Pre 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 645 & year <= 1990)) %>%
  filter(!(COW == 645 & year == 1991 & month < 3))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 645 & year == 2003 & month > 11)) %>%
  filter(!(COW == 645 & year >= 2004))

Geo_PKO$mission[Geo_PKO$COW == 645 & is.na(Geo_PKO$mission)] <- "UNIKOM"


#~~~~~~~~~#
### 651 ###
#~~~~~~~~~#

### UNTSO May 1948 - Today ###
# Only existed in cell 173945

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 651 & gid != 173945))

Geo_PKO$mission[Geo_PKO$COW == 651 & is.na(Geo_PKO$mission)] <- "UNTSO"


#~~~~~~~~~#
### 652 ###
#~~~~~~~~~#

### UNTSO May 1948 - Today ###
# Only existed in cell 178273

Geo_PKO$mission[Geo_PKO$COW == 652 & Geo_PKO$gid == 178273 & is.na(Geo_PKO$mission)] <- "UNTSO"


### UNSMIS 4 2012 - 8 2012 ###

Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 652 & gid != 178273 & year <= 2011)) %>%
  filter(!(COW == 652 & gid != 178273 & year == 2012 & month < 3)) %>%
  filter(!(COW == 652 & gid != 178273 & year == 2012 & month > 9 )) %>%
  filter(!(COW == 652 & gid != 178273 & year >= 2013))
Geo_PKO$mission[Geo_PKO$COW == 652 & Geo_PKO$gid != 178273 & is.na(Geo_PKO$mission)] <- "UNSMIS"


#~~~~~~~~~#
### 660 ###
#~~~~~~~~~#

### UNIFIL March 1978 - Today ###
# I assume that UNIFIL has a larger role in Lebanon than UNTSO
# As a result, I will not add names for UNTSO but I will
# name all unfilled cells for UNIFIL
Geo_PKO$mission[Geo_PKO$COW == 660 & is.na(Geo_PKO$mission)] <- "UNIFIL"

### UNTSO May 1948 - Today ###


#~~~~~~~~~#
### 666 ###
#~~~~~~~~~#

### UNDOF May 1974 - Today ###
# UNDOF is more active in cells 176832 and 177552 and will get those unnamed cells

Geo_PKO$mission[Geo_PKO$COW == 666 & Geo_PKO$gid == 176832 & is.na(Geo_PKO$mission)] <- "UNDOF"
Geo_PKO$mission[Geo_PKO$COW == 666 & Geo_PKO$gid == 177552 & is.na(Geo_PKO$mission)] <- "UNDOF"

### UNTSO May 1948 - Today ###
# UNTSO will get the rest of Israel 
Geo_PKO$mission[Geo_PKO$COW == 666 & Geo_PKO$gid != 176832 & Geo_PKO$gid != 177552 & is.na(Geo_PKO$mission)] <- "UNTSO"

### UNIFIL March 1978 - Today
# I assume that UNIFIL has a lesser role within Israel 


#~~~~~~~~~#
### 690 ###
#~~~~~~~~~#

### UNIKOM 4 1991 - 10 2003 ###
# Pre 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 690 & year <= 1990)) %>%
  filter(!(COW == 690 & year == 1991 & month < 3))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 690 & year == 2003 & month > 11)) %>%
  filter(!(COW == 690 & year >= 2004))

Geo_PKO$mission[Geo_PKO$COW == 690 & is.na(Geo_PKO$mission)] <- "UNIKOM"


#~~~~~~~~~#
### 700 ###
#~~~~~~~~~#

### UNMOT 12 1994 - 5 2000

# Pre 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 700 & year < 1994)) %>%
  filter(!(COW == 700 & year == 1994 & month < 11))

# Post 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 700 & year == 2000 & month > 6)) %>%
  filter(!(COW == 700 & year >= 2001))

Geo_PKO$mission[Geo_PKO$COW == 700 & is.na(Geo_PKO$mission)] <- "UNMOT"


#~~~~~~~~~#
### 702 ###
#~~~~~~~~~#

### UNMOT 12 1994 - 5 2000

# Pre 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 702 & year < 1994)) %>%
  filter(!(COW == 702 & year == 1994 & month < 11))

# Post 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 702 & year == 2000 & month > 6)) %>%
  filter(!(COW == 702 & year >= 2001))

Geo_PKO$mission[Geo_PKO$COW == 702 & is.na(Geo_PKO$mission)] <- "UNMOT"


#~~~~~~~~~#
### 750 ###
#~~~~~~~~~#

### UNMOGIP 1 1949 - Today ###
Geo_PKO$mission[Geo_PKO$COW == 750 & is.na(Geo_PKO$mission)] <- "UNMOGIP"


#~~~~~~~~~#
### 770 ###
#~~~~~~~~~#

### UNMOGIP 1 1949 - Today ### 
Geo_PKO$mission[Geo_PKO$COW == 770 & is.na(Geo_PKO$mission)] <- "UNMOGIP"


#~~~~~~~~~#
### 860 ###
#~~~~~~~~~#

### UNMISET 5 2002 - 5 2005 
# Pre
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 860 & year < 2002)) %>%
  filter(!(COW == 860 & year == 2002 & month < 4))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 860 & year == 2005 & month > 6))

Geo_PKO$mission[Geo_PKO$COW == 860 & Geo_PKO$year <= 2005 & is.na(Geo_PKO$mission)] <- "UNMISET"


### UNMIT 8 2006 - 12 2012
# Pre 
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 860 & year == 2006 & month < 7))

# Post
Geo_PKO <- Geo_PKO %>%
  filter(!(COW == 860 & year == 2013 & month > 1)) %>%
  filter(!(COW == 860 & year >= 2014))

Geo_PKO$mission[Geo_PKO$COW == 860 & Geo_PKO$year >= 2006 & is.na(Geo_PKO$mission)] <- "UNMIT"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Drop Excess variables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Geo_PKO <- Geo_PKO %>%
  select(-country, -cow_code)


#~~~~~~~~~~~~~~~~~#
### Fill in 0's ###
#~~~~~~~~~~~~~~~~~#

Geo_PKO <- Geo_PKO %>%
  mutate(zone.de.confidence = as.numeric(zone.de.confidence)) %>%
  mutate_at(c("hq", "no.troops", "no.tcc", "zone.de.confidence"), ~replace(., is.na(.), 0))


#~~~~~~~~~~~~~~~~~~~~~~~#
#### PRIO Shape File ####
#~~~~~~~~~~~~~~~~~~~~~~~#

# Import PRIO shape file #
Prio <- st_read("Prio_Shape/priogrid_cell.shp",
                layer = "priogrid_cell",
                stringsAsFactors = F)


# Set shape file as spatial file #
Prio_sp <- as(Prio, Class = "Spatial")


# Find neighbors with queen's contiguity #
nb_queen <- poly2nb(Prio_sp,
              queen = TRUE, 
              row.names = Prio_sp$gid)


# Cut nb_queen into smaller pieces to speed up process #
chunk_number <- 10 
q_list <- split(nb_queen, cut(seq_along(nb_queen), chunk_number, labels = FALSE))

# Make the smaller lists into dataframes #
m1 <- as.matrix(q_list[[1]])

m2 <- as.matrix(q_list[[2]])

m3 <- as.matrix(q_list[[3]])

m4 <- as.matrix(q_list[[4]])

m5 <- as.matrix(q_list[[5]])

m6 <- as.matrix(q_list[[6]])

m7 <- as.matrix(q_list[[7]])

m8 <- as.matrix(q_list[[8]])

m9 <- as.matrix(q_list[[9]])

m10 <- as.matrix(q_list[[10]])


# Put back into lists #
m1$V1 <- stri_replace_all_regex(m1$V1,
                                 pattern = c('c', '(', ')'),
                                 replacement = c('', '', ''),
                                 vectorize=TRUE)

m2$V1 <- stri_replace_all_regex(m2$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m3$V1 <- stri_replace_all_regex(m3$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m4$V1 <- stri_replace_all_regex(m4$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m5$V1 <- stri_replace_all_regex(m5$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m6$V1 <- stri_replace_all_regex(m6$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m7$V1 <- stri_replace_all_regex(m7$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m8$V1 <- stri_replace_all_regex(m8$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m9$V1 <- stri_replace_all_regex(m9$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)

m10$V1 <- stri_replace_all_regex(m10$V1,
                                pattern = c('c', '(', ')'),
                                replacement = c('', '', ''),
                                vectorize=TRUE)


# Make into dataframes with columns #
x1 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m1)) {
  v <- m1[[i]]
  x1 <- rbind(x1, v)
  i <- i + 1
}

x1 <- x1 %>%
  rename(V1 = X2L,
         V2 = X73L,
         V3 = X714L,
         V4 = X715L,
         V5 = X793L)


x2 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m2)) {
  v <- m2[[i]]
  x2 <- rbind(x2, v)
  i <- i + 1
}

x2 <- x2 %>%
  rename(V1 = X25200L,
         V2 = X25201L,
         V3 = X25202L,
         V4 = X25920L,
         V5 = X25922L,
         V6 = X26640L,
         V7 = X26641L,
         V8 = X26642L)

x3 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m3)) {
  v <- m3[[i]]
  x3 <- rbind(x3, v)
  i <- i + 1
}

x3 <- x3 %>%
  rename(V1 = X51120L,
         V2 = X51121L,
         V3 = X51122L,
         V4 = X51840L,
         V5 = X51842L,
         V6 = X52560L,
         V7 = X52561L,
         V8 = X52562L)

x4 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m4)) {
  v <- m4[[i]]
  x4 <- rbind(x4, v)
  i <- i + 1
}

x4 <- x4 %>%
  rename(V1 = X77040L,
         V2 = X77041L,
         V3 = X77042L,
         V4 = X77760L,
         V5 = X77762L,
         V6 = X78480L,
         V7 = X78481L,
         V8 = X78482L)

x5 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m5)) {
  v <- m5[[i]]
  x5 <- rbind(x5, v)
  i <- i + 1
}

x5 <- x5 %>%
  rename(V1 = X102960L,
         V2 = X102961L,
         V3 = X102962L,
         V4 = X103680L,
         V5 = X103682L,
         V6 = X104400L,
         V7 = X104401L,
         V8 = X104402L)

x6 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m6)) {
  v <- m6[[i]]
  x6 <- rbind(x6, v)
  i <- i + 1
}

x6 <- x6 %>%
  rename(V1 = X128880L,
         V2 = X128881L,
         V3 = X128882L,
         V4 = X129600L,
         V5 = X129602L,
         V6 = X130320L,
         V7 = X130321L,
         V8 = X130322L)

x7 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m7)) {
  v <- m7[[i]]
  x7 <- rbind(x7, v)
  i <- i + 1
}

x7 <- x7 %>%
  rename(V1 = X154800L,
         V2 = X154801L,
         V3 = X154802L,
         V4 = X155520L,
         V5 = X155522L,
         V6 = X156240L,
         V7 = X156241L,
         V8 = X156242L)

x8 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m8)) {
  v <- m8[[i]]
  x8 <- rbind(x8, v)
  i <- i + 1
}

x8 <- x8 %>%
  rename(V1 = X180720L,
         V2 = X180721L,
         V3 = X180722L,
         V4 = X181440L,
         V5 = X181442L,
         V6 = X182160L,
         V7 = X182161L,
         V8 = X182162L)

x9 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m9)) {
  v <- m9[[i]]
  x9 <- rbind(x9, v)
  i <- i + 1
}

x9 <- x9 %>%
  rename(V1 = X206640L,
         V2 = X206641L,
         V3 = X206642L,
         V4 = X207360L,
         V5 = X207362L,
         V6 = X208080L,
         V7 = X208081L,
         V8 = X208082L)

x10 <- as.data.frame(matrix(ncol = 1, nrow = 0))
i <- 1 
while(i <= length(m10)) {
  v <- m10[[i]]
  x10 <- rbind(x10, v)
  i <- i + 1
}

x10 <- x10 %>%
  rename(V1 = X232560L,
         V2 = X232561L,
         V3 = X232562L,
         V4 = X233280L,
         V5 = X233282L,
         V6 = X234000L,
         V7 = X234001L,
         V8 = X234002L)


# Combine datasets into one #
x <- as.data.frame(matrix(ncol = 8, nrow = 0))
x <- bind_rows(x1, x2, x3, x4, x5, x6 , x7, x8, x9, x10)


# Specify neighbors in dataset #
x <- x %>%
  mutate(gid = 1:n(),
         Neigh_1 = V1,
         Neigh_2 = V2,
         Neigh_3 = V3,
         Neigh_4 = V4,
         Neigh_5 = V5,
         Neigh_6 = V6,
         Neigh_7 = V7,
         Neigh_8 = V8) %>%
  select(-V1, -V2, -V3, -V4, -V5, -V6, -V7, -V8)


# Merge Prio with neighbors #
Prio <- merge(x = Prio, y = x, by = "gid")
remove(x, i, v, chunk_number)


# Merge Geo_PKO and Prio #
Geo_PKO <- merge(y = Geo_PKO, x = Prio, by = "gid")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Find Neighbor Troop Counts ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(tidyverse)

# Make datasets with neighbor info #
N1 <- Geo_PKO %>%
  dplyr::select(gid, month, year, no.troops) %>%
  mutate(no.troops_N1 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N2 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N2 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N3 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N3 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N4 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N4 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N5 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N5 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N6 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N6 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N7 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N7 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)

N8 <- Geo_PKO %>%
  select(gid, month, year, no.troops) %>%
  mutate(no.troops_N8 = no.troops) %>%
  select(-no.troops) %>%
  st_set_geometry(., NULL)


# Spatial merge Geo_PKO and neighbor datasets #
Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N1, by = c("Neigh_1" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N2, by = c("Neigh_2" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N3, by = c("Neigh_3" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N4, by = c("Neigh_4" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N5, by = c("Neigh_5" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N6, by = c("Neigh_6" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N7, by = c("Neigh_7" = "gid", "month", "year"))

Geo_PKO <- Geo_PKO %>%
  merge(x = ., y = N8, by = c("Neigh_8" = "gid", "month", "year"))


# Remove excess datasets #
remove(N1, N2, N3, N4, N5, N6, N7, N8, Prio, nb_queen, Prio_sp,
       m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, x1, x2, x3,
       x4, x5, x6, x7, x8, x9, x10, q_list)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Add Contiguous Troops ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Geo_PKO <- Geo_PKO %>% 
  mutate(neigh_troops = no.troops_N1 + no.troops_N2 +
           no.troops_N3 + no.troops_N4 + no.troops_N5 +
           no.troops_N6 + no.troops_N7 + no.troops_N8) %>%
  select(-no.troops_N1, -no.troops_N2, -no.troops_N3, -no.troops_N4,
         -no.troops_N5, -no.troops_N6, -no.troops_N7, -no.troops_N8 ,
         -Neigh_1, -Neigh_2, -Neigh_3, -Neigh_4, -Neigh_5, -Neigh_6,
         -Neigh_7, -Neigh_8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Problems with TAMM Merge ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

miss <- unique(Geo_PKO$mission[is.na(Geo_PKO$risk_ratio)])


# UNAVEM III #
# TAMM does not include Jan 1995. Remove from Geo_PKO
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNAVEM III" & year == 1995 & month == 1))


# MONUC #
# Make observations of MONUC into MONUSCO after 6 2010
Geo_PKO$mission[Geo_PKO$mission == "MONUC" & Geo_PKO$year == 2010 & Geo_PKO$month > 6] <- "MONUSCO"
Geo_PKO$mission[Geo_PKO$mission == "MONUC" & Geo_PKO$year >= 2011] <- "MONUSCO"

# Remove observations before 9 1999
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MONUC" & year == 1999 & month < 9))


# MONUA #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MONUA" & year == 1999 & month > 2))


# ONUMOZ #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "ONUMOZ" & year >= 1995))


# UNMIT #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNMIT" & year == 2006 & month < 8)) %>%
  filter(!(mission == "UNMIT" & year > 2012))


# UNMISET #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNMISET" & year == 2002 & month < 5)) %>%
  filter(!(mission == "UNMISET" & year == 2005 & month > 5))


# ONUB #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "ONUB" & year == 2004 & month < 5))


# UNAMIR #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNAMIR" & year == 1996 & month > 3))


# UNOSOM II #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNOSOM II" & year == 1995 & month > 3))


# UNOMUR #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNOMUR" & year == 1994 & month > 9))


# MINUSCA #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MINUSCA" & year == 2014 & month < 4))

# UNMEE #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNMEE" & year == 2000 & month < 9)) %>%
  filter(!(mission == "UNMEE" & year == 2008 & month > 7))


# UNOMIL #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNOMIL" & year == 1997 & month > 9))


# MINUCI #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MINUCI" & year == 2003 & month < 5))


# UNAMSIL #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNAMSIL" & year >= 2006))


# UNOMSIL #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNOMSIL" & year == 1998 & month < 7))


# UNMOGIP #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNMOGIP"))


# UNIOSIL #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNIOSIL"))


# UNMIS #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNMIS" & year == 2011 & month > 7))


# UNISFA #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNISFA" & year == 2011 & month < 6))


# MINUSMA #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MINUSMA" & year == 2013 & month < 4))


# UNAMID #
# Removed front excess observations earlier in code


# MINUGUA #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MINUGUA" & year < 1997)) %>%
  filter(!(mission == "MINUGUA" & year == 1997 & month > 5)) %>%
  filter(!(mission == "MINUGUA" & year > 1997))


# MIPONUH #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MIPONUH" & year == 2000 & month > 3))


# UNSMIH #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNSMIH" & year == 1997 & month > 7))


# UNIKOM #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNIKOM" & year == 2003 & month > 10))


# UNMOT #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNMOT" & year == 1994 & month < 12)) %>%
  filter(!(mission == "UNMOT" & year == 2000 & month > 5))


# UNSMIS #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNSMIS" & year == 2012 & month < 4))

# UNPREDEP #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNPREDEP" & year == 1999 & month > 2))


# UNPROFOR #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "UNPROFOR" & year == 1995 & month > 3))


# MINURCAT #
Geo_PKO <- Geo_PKO %>%
  filter(!(mission == "MINURCAT" & year == 2011))


#~~~~~~~~~~~~#
#### TAMM ####
#~~~~~~~~~~~~#

# Import TAMM
TAMM <- read_csv("TAMM/TAMM.csv")


# Merge with Geo_PKO #
Geo_PKO <- Geo_PKO %>%
  full_join(TAMM, by = c("mission", "year", "month"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Construct Risk Ratio Measure ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Mutate variable #
Geo_PKO <- Geo_PKO %>%
  mutate(risky_count =  peaceag_cease_monitor + buffer_monitor + liaise_warpart +
           peaceag_cease_assist + humrts_monitor + refugees_monitor + humrts_protect +
           children_protect + women_protect + prociv + unpersonnel_protect + 
           demining_assist + refugees_assist + humaid_assist + humpersonnel_protect +
           borders_monitor + ch7 + securitysectorreform_assist + policereform_assist + 
           police_monitor + police_jointpatrols + ddr_monitor + ddr_assist,
         less_risky_count = weaponstrade_monitor + weaponsembargo_monitor + goodoffices +
           cargoinspections + resources_monitor + election_monitor + election_security + 
           election_assist + govcap_assist + govpolicies_assist + cultural_pres + qip_assist +
           justice_assist + reconciliation + justice_warcrim + mission_pr + freepress,
         risk_ratio = risky_count/(less_risky_count + risky_count)) %>%
select(-risky_count, -less_risky_count)


# Drop TAMM dataframe #
remove(TAMM)


# Remove extra missions and years introduced by TAMM 
Geo_PKO <- Geo_PKO %>%
  drop_na(gid)


#~~~~~~~~~~~~~~~~~~#
#### Import GED ####
#~~~~~~~~~~~~~~~~~~#

# Import GED #
GED <- read_csv("GED/GED.csv")


# Get month #
GED <- GED %>%
  mutate(month = month(date_end))


## Get OSV ##
GED <- GED %>%
  mutate(OSV_GOV = ifelse(type_of_violence == 3,
                          ifelse(str_detect(side_a, "Government of"),
                                 best,
                                 0),
                          0),
         OSV_Rebs = ifelse(type_of_violence == 3,
                           ifelse(str_detect(side_a, "Government of"),
                                  0,
                                  best),
                           0),
         OSV_total = OSV_GOV + OSV_Rebs)

GED2 <- GED %>%
  group_by(priogrid_gid, year, month) %>%
  summarise(best = sum(best),
            OSV_GOV = sum(OSV_GOV),
            OSV_Rebs = sum(OSV_Rebs),
            OSV_total = sum(OSV_total))


# Join with Geo_PKO #
Geo_PKO <- Geo_PKO %>%
  left_join(GED2, by = c("gid" = "priogrid_gid", "year", "month"))

remove(GED, GED2)


# Fill OSV's with 0's #
Geo_PKO <- Geo_PKO %>%
  mutate_at(c("OSV_GOV", "OSV_Rebs", "OSV_total", "best"), ~replace(., is.na(.), 0))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Distance to Nearest Deployment ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Mark cells with units #
Geo_PKO$unit <- ifelse(Geo_PKO$no.troops > 0, 1, 0)


# Dataframe of cells with units #
units <- Geo_PKO[Geo_PKO$unit == 1,]


# Keep essentials #
units <- units %>%
  select(gid, month, year, mission, no.troops, geometry,
         unit, longitude, latitude, hq) %>%
  mutate(x = make_date(year = year, month = month))


# Duplicate observations for 3 months #
units <- units[rep(seq_len(nrow(units)), each = 3), ]


# Necessary package #
require(lubridate)


# Create units df with current, 1 month, and 2 month previous to ensure a match #
units <- units %>%
  add_rownames(var = "time") %>%
  separate(time, paste('time', 1:2, sep = ".")) %>%
  select(-time.1) %>%
  mutate(time = as.numeric(ifelse(is.na(time.2), 0, time.2))) %>%
  select(-time.2)

units$date <- data.table::fifelse(units$time == 0,
              units$x,
                  data.table::fifelse(units$time == 1,
                  units$x + months(-1),
                  units$x + months(-2)))

units <- units %>%
  mutate(month = month(date)) %>%
  select(-x, -time, -date)

units <- st_sf(units)


# Function to split month-year into datasets #
split_fun <- function(df1, df2) {
  # Required packages
  require(tidyverse)
  
  # Geo_PKO part
  df1 <- df1 %>% select(gid, month, year, no.troops, geometry) %>% mutate(PKO = "PKO")
  x <- split(df1, list(df1$PKO, df1$year, df1$month), sep = "_")
  list2env(x, .GlobalEnv)
  
  # units part
  df2 <- df2 %>% mutate(df = "units")
  y <- split(df2, list(df2$df, df2$year, df2$month), sep = "_")
  list2env(y, .GlobalEnv)
}


# Split into month-year datasets #
split_fun(df1 = Geo_PKO, df2 = units)

#~~~~~~~~~~~~~~~~~~~~~~#
#### Find Distances ####
#~~~~~~~~~~~~~~~~~~~~~~#

## 1994 ##

nearest <- st_nearest_feature(PKO_1994_1, units_1994_1)
PKO_1994_1$dist <- st_distance(PKO_1994_1, units_1994_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_2, units_1994_2)
PKO_1994_2$dist <- st_distance(PKO_1994_2, units_1994_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_3, units_1994_3)
PKO_1994_3$dist <- st_distance(PKO_1994_3, units_1994_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_4, units_1994_4)
PKO_1994_4$dist <- st_distance(PKO_1994_4, units_1994_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_5, units_1994_5)
PKO_1994_5$dist <- st_distance(PKO_1994_5, units_1994_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_6, units_1994_6)
PKO_1994_6$dist <- st_distance(PKO_1994_6, units_1994_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_7, units_1994_7)
PKO_1994_7$dist <- st_distance(PKO_1994_7, units_1994_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_8, units_1994_8)
PKO_1994_8$dist <- st_distance(PKO_1994_8, units_1994_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_9, units_1994_9)
PKO_1994_9$dist <- st_distance(PKO_1994_9, units_1994_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_10, units_1994_10)
PKO_1994_10$dist <- st_distance(PKO_1994_10, units_1994_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_11, units_1994_11)
PKO_1994_11$dist <- st_distance(PKO_1994_11, units_1994_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1994_12, units_1994_12)
PKO_1994_12$dist <- st_distance(PKO_1994_12, units_1994_12[nearest,], by_element = TRUE)

PKO_1994 <- bind_rows(PKO_1994_1, PKO_1994_2, PKO_1994_3, PKO_1994_4,
                      PKO_1994_5, PKO_1994_6, PKO_1994_7, PKO_1994_8,
                      PKO_1994_9, PKO_1994_10, PKO_1994_11, PKO_1994_12)

remove(PKO_1994_1, PKO_1994_2, PKO_1994_3, PKO_1994_4,
       PKO_1994_5, PKO_1994_6, PKO_1994_7, PKO_1994_8,
       PKO_1994_9, PKO_1994_10, PKO_1994_11, PKO_1994_12,
       units_1994_1, units_1994_2, units_1994_3, units_1994_4,
       units_1994_5, units_1994_6, units_1994_7, units_1994_8,
       units_1994_9, units_1994_10, units_1994_11, units_1994_12)

## 1995 ##

nearest <- st_nearest_feature(PKO_1995_1, units_1995_1)
PKO_1995_1$dist <- st_distance(PKO_1995_1, units_1995_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_2, units_1995_2)
PKO_1995_2$dist <- st_distance(PKO_1995_2, units_1995_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_3, units_1995_3)
PKO_1995_3$dist <- st_distance(PKO_1995_3, units_1995_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_4, units_1995_4)
PKO_1995_4$dist <- st_distance(PKO_1995_4, units_1995_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_5, units_1995_5)
PKO_1995_5$dist <- st_distance(PKO_1995_5, units_1995_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_6, units_1995_6)
PKO_1995_6$dist <- st_distance(PKO_1995_6, units_1995_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_7, units_1995_7)
PKO_1995_7$dist <- st_distance(PKO_1995_7, units_1995_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_8, units_1995_8)
PKO_1995_8$dist <- st_distance(PKO_1995_8, units_1995_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_9, units_1995_9)
PKO_1995_9$dist <- st_distance(PKO_1995_9, units_1995_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_10, units_1995_10)
PKO_1995_10$dist <- st_distance(PKO_1995_10, units_1995_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_11, units_1995_11)
PKO_1995_11$dist <- st_distance(PKO_1995_11, units_1995_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1995_12, units_1995_12)
PKO_1995_12$dist <- st_distance(PKO_1995_12, units_1995_12[nearest,], by_element = TRUE)

PKO_1995 <- bind_rows(PKO_1995_1, PKO_1995_2, PKO_1995_3, PKO_1995_4,
                      PKO_1995_5, PKO_1995_6, PKO_1995_7, PKO_1995_8,
                      PKO_1995_9, PKO_1995_10, PKO_1995_11, PKO_1995_12)

remove(PKO_1995_1, PKO_1995_2, PKO_1995_3, PKO_1995_4,
       PKO_1995_5, PKO_1995_6, PKO_1995_7, PKO_1995_8,
       PKO_1995_9, PKO_1995_10, PKO_1995_11, PKO_1995_12,
       units_1995_1, units_1995_2, units_1995_3, units_1995_4,
       units_1995_5, units_1995_6, units_1995_7, units_1995_8,
       units_1995_9, units_1995_10, units_1995_11, units_1995_12)

## 1996 ##

nearest <- st_nearest_feature(PKO_1996_1, units_1996_1)
PKO_1996_1$dist <- st_distance(PKO_1996_1, units_1996_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_2, units_1996_2)
PKO_1996_2$dist <- st_distance(PKO_1996_2, units_1996_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_3, units_1996_3)
PKO_1996_3$dist <- st_distance(PKO_1996_3, units_1996_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_4, units_1996_4)
PKO_1996_4$dist <- st_distance(PKO_1996_4, units_1996_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_5, units_1996_5)
PKO_1996_5$dist <- st_distance(PKO_1996_5, units_1996_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_6, units_1996_6)
PKO_1996_6$dist <- st_distance(PKO_1996_6, units_1996_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_7, units_1996_7)
PKO_1996_7$dist <- st_distance(PKO_1996_7, units_1996_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_8, units_1996_8)
PKO_1996_8$dist <- st_distance(PKO_1996_8, units_1996_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_9, units_1996_9)
PKO_1996_9$dist <- st_distance(PKO_1996_9, units_1996_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_10, units_1996_10)
PKO_1996_10$dist <- st_distance(PKO_1996_10, units_1996_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_11, units_1996_11)
PKO_1996_11$dist <- st_distance(PKO_1996_11, units_1996_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1996_12, units_1996_12)
PKO_1996_12$dist <- st_distance(PKO_1996_12, units_1996_12[nearest,], by_element = TRUE)

PKO_1996 <- bind_rows(PKO_1996_1, PKO_1996_2, PKO_1996_3, PKO_1996_4,
                      PKO_1996_5, PKO_1996_6, PKO_1996_7, PKO_1996_8,
                      PKO_1996_9, PKO_1996_10, PKO_1996_11, PKO_1996_12)

remove(PKO_1996_1, PKO_1996_2, PKO_1996_3, PKO_1996_4,
       PKO_1996_5, PKO_1996_6, PKO_1996_7, PKO_1996_8,
       PKO_1996_9, PKO_1996_10, PKO_1996_11, PKO_1996_12,
       units_1996_1, units_1996_2, units_1996_3, units_1996_4,
       units_1996_5, units_1996_6, units_1996_7, units_1996_8,
       units_1996_9, units_1996_10, units_1996_11, units_1996_12)

## 1997 ##

nearest <- st_nearest_feature(PKO_1997_1, units_1997_1)
PKO_1997_1$dist <- st_distance(PKO_1997_1, units_1997_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_2, units_1997_2)
PKO_1997_2$dist <- st_distance(PKO_1997_2, units_1997_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_3, units_1997_3)
PKO_1997_3$dist <- st_distance(PKO_1997_3, units_1997_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_4, units_1997_4)
PKO_1997_4$dist <- st_distance(PKO_1997_4, units_1997_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_5, units_1997_5)
PKO_1997_5$dist <- st_distance(PKO_1997_5, units_1997_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_6, units_1997_6)
PKO_1997_6$dist <- st_distance(PKO_1997_6, units_1997_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_7, units_1997_7)
PKO_1997_7$dist <- st_distance(PKO_1997_7, units_1997_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_8, units_1997_8)
PKO_1997_8$dist <- st_distance(PKO_1997_8, units_1997_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_9, units_1997_9)
PKO_1997_9$dist <- st_distance(PKO_1997_9, units_1997_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_10, units_1997_10)
PKO_1997_10$dist <- st_distance(PKO_1997_10, units_1997_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_11, units_1997_11)
PKO_1997_11$dist <- st_distance(PKO_1997_11, units_1997_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1997_12, units_1997_12)
PKO_1997_12$dist <- st_distance(PKO_1997_12, units_1997_12[nearest,], by_element = TRUE)

PKO_1997 <- bind_rows(PKO_1997_1, PKO_1997_2, PKO_1997_3, PKO_1997_4,
                      PKO_1997_5, PKO_1997_6, PKO_1997_7, PKO_1997_8,
                      PKO_1997_9, PKO_1997_10, PKO_1997_11, PKO_1997_12)

remove(PKO_1997_1, PKO_1997_2, PKO_1997_3, PKO_1997_4,
       PKO_1997_5, PKO_1997_6, PKO_1997_7, PKO_1997_8,
       PKO_1997_9, PKO_1997_10, PKO_1997_11, PKO_1997_12,
       units_1997_1, units_1997_2, units_1997_3, units_1997_4,
       units_1997_5, units_1997_6, units_1997_7, units_1997_8,
       units_1997_9, units_1997_10, units_1997_11, units_1997_12)

## 1998 ##

nearest <- st_nearest_feature(PKO_1998_1, units_1998_1)
PKO_1998_1$dist <- st_distance(PKO_1998_1, units_1998_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_2, units_1998_2)
PKO_1998_2$dist <- st_distance(PKO_1998_2, units_1998_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_3, units_1998_3)
PKO_1998_3$dist <- st_distance(PKO_1998_3, units_1998_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_4, units_1998_4)
PKO_1998_4$dist <- st_distance(PKO_1998_4, units_1998_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_5, units_1998_5)
PKO_1998_5$dist <- st_distance(PKO_1998_5, units_1998_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_6, units_1998_6)
PKO_1998_6$dist <- st_distance(PKO_1998_6, units_1998_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_7, units_1998_7)
PKO_1998_7$dist <- st_distance(PKO_1998_7, units_1998_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_8, units_1998_8)
PKO_1998_8$dist <- st_distance(PKO_1998_8, units_1998_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_9, units_1998_9)
PKO_1998_9$dist <- st_distance(PKO_1998_9, units_1998_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_10, units_1998_10)
PKO_1998_10$dist <- st_distance(PKO_1998_10, units_1998_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_11, units_1998_11)
PKO_1998_11$dist <- st_distance(PKO_1998_11, units_1998_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1998_12, units_1998_12)
PKO_1998_12$dist <- st_distance(PKO_1998_12, units_1998_12[nearest,], by_element = TRUE)

PKO_1998 <- bind_rows(PKO_1998_1, PKO_1998_2, PKO_1998_3, PKO_1998_4,
                      PKO_1998_5, PKO_1998_6, PKO_1998_7, PKO_1998_8,
                      PKO_1998_9, PKO_1998_10, PKO_1998_11, PKO_1998_12)

remove(PKO_1998_1, PKO_1998_2, PKO_1998_3, PKO_1998_4,
       PKO_1998_5, PKO_1998_6, PKO_1998_7, PKO_1998_8,
       PKO_1998_9, PKO_1998_10, PKO_1998_11, PKO_1998_12,
       units_1998_1, units_1998_2, units_1998_3, units_1998_4,
       units_1998_5, units_1998_6, units_1998_7, units_1998_8,
       units_1998_9, units_1998_10, units_1998_11, units_1998_12)

## 1999 ##

nearest <- st_nearest_feature(PKO_1999_1, units_1999_1)
PKO_1999_1$dist <- st_distance(PKO_1999_1, units_1999_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_2, units_1999_2)
PKO_1999_2$dist <- st_distance(PKO_1999_2, units_1999_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_3, units_1999_3)
PKO_1999_3$dist <- st_distance(PKO_1999_3, units_1999_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_4, units_1999_4)
PKO_1999_4$dist <- st_distance(PKO_1999_4, units_1999_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_5, units_1999_5)
PKO_1999_5$dist <- st_distance(PKO_1999_5, units_1999_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_6, units_1999_6)
PKO_1999_6$dist <- st_distance(PKO_1999_6, units_1999_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_7, units_1999_7)
PKO_1999_7$dist <- st_distance(PKO_1999_7, units_1999_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_8, units_1999_8)
PKO_1999_8$dist <- st_distance(PKO_1999_8, units_1999_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_9, units_1999_9)
PKO_1999_9$dist <- st_distance(PKO_1999_9, units_1999_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_10, units_1999_10)
PKO_1999_10$dist <- st_distance(PKO_1999_10, units_1999_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_11, units_1999_11)
PKO_1999_11$dist <- st_distance(PKO_1999_11, units_1999_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_1999_12, units_1999_12)
PKO_1999_12$dist <- st_distance(PKO_1999_12, units_1999_12[nearest,], by_element = TRUE)

PKO_1999 <- bind_rows(PKO_1999_1, PKO_1999_2, PKO_1999_3, PKO_1999_4,
                      PKO_1999_5, PKO_1999_6, PKO_1999_7, PKO_1999_8,
                      PKO_1999_9, PKO_1999_10, PKO_1999_11, PKO_1999_12)

remove(PKO_1999_1, PKO_1999_2, PKO_1999_3, PKO_1999_4,
       PKO_1999_5, PKO_1999_6, PKO_1999_7, PKO_1999_8,
       PKO_1999_9, PKO_1999_10, PKO_1999_11, PKO_1999_12,
       units_1999_1, units_1999_2, units_1999_3, units_1999_4,
       units_1999_5, units_1999_6, units_1999_7, units_1999_8,
       units_1999_9, units_1999_10, units_1999_11, units_1999_12)

## 2000 ##

nearest <- st_nearest_feature(PKO_2000_1, units_2000_1)
PKO_2000_1$dist <- st_distance(PKO_2000_1, units_2000_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_2, units_2000_2)
PKO_2000_2$dist <- st_distance(PKO_2000_2, units_2000_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_3, units_2000_3)
PKO_2000_3$dist <- st_distance(PKO_2000_3, units_2000_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_4, units_2000_4)
PKO_2000_4$dist <- st_distance(PKO_2000_4, units_2000_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_5, units_2000_5)
PKO_2000_5$dist <- st_distance(PKO_2000_5, units_2000_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_6, units_2000_6)
PKO_2000_6$dist <- st_distance(PKO_2000_6, units_2000_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_7, units_2000_7)
PKO_2000_7$dist <- st_distance(PKO_2000_7, units_2000_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_8, units_2000_8)
PKO_2000_8$dist <- st_distance(PKO_2000_8, units_2000_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_9, units_2000_9)
PKO_2000_9$dist <- st_distance(PKO_2000_9, units_2000_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_10, units_2000_10)
PKO_2000_10$dist <- st_distance(PKO_2000_10, units_2000_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_11, units_2000_11)
PKO_2000_11$dist <- st_distance(PKO_2000_11, units_2000_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2000_12, units_2000_12)
PKO_2000_12$dist <- st_distance(PKO_2000_12, units_2000_12[nearest,], by_element = TRUE)

PKO_2000 <- bind_rows(PKO_2000_1, PKO_2000_2, PKO_2000_3, PKO_2000_4,
                      PKO_2000_5, PKO_2000_6, PKO_2000_7, PKO_2000_8,
                      PKO_2000_9, PKO_2000_10, PKO_2000_11, PKO_2000_12)

remove(PKO_2000_1, PKO_2000_2, PKO_2000_3, PKO_2000_4,
       PKO_2000_5, PKO_2000_6, PKO_2000_7, PKO_2000_8,
       PKO_2000_9, PKO_2000_10, PKO_2000_11, PKO_2000_12,
       units_2000_1, units_2000_2, units_2000_3, units_2000_4,
       units_2000_5, units_2000_6, units_2000_7, units_2000_8,
       units_2000_9, units_2000_10, units_2000_11, units_2000_12)

## 2001 ##

nearest <- st_nearest_feature(PKO_2001_1, units_2001_1)
PKO_2001_1$dist <- st_distance(PKO_2001_1, units_2001_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_2, units_2001_2)
PKO_2001_2$dist <- st_distance(PKO_2001_2, units_2001_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_3, units_2001_3)
PKO_2001_3$dist <- st_distance(PKO_2001_3, units_2001_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_4, units_2001_4)
PKO_2001_4$dist <- st_distance(PKO_2001_4, units_2001_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_5, units_2001_5)
PKO_2001_5$dist <- st_distance(PKO_2001_5, units_2001_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_6, units_2001_6)
PKO_2001_6$dist <- st_distance(PKO_2001_6, units_2001_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_7, units_2001_7)
PKO_2001_7$dist <- st_distance(PKO_2001_7, units_2001_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_8, units_2001_8)
PKO_2001_8$dist <- st_distance(PKO_2001_8, units_2001_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_9, units_2001_9)
PKO_2001_9$dist <- st_distance(PKO_2001_9, units_2001_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_10, units_2001_10)
PKO_2001_10$dist <- st_distance(PKO_2001_10, units_2001_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_11, units_2001_11)
PKO_2001_11$dist <- st_distance(PKO_2001_11, units_2001_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2001_12, units_2001_12)
PKO_2001_12$dist <- st_distance(PKO_2001_12, units_2001_12[nearest,], by_element = TRUE)

PKO_2001 <- bind_rows(PKO_2001_1, PKO_2001_2, PKO_2001_3, PKO_2001_4,
                      PKO_2001_5, PKO_2001_6, PKO_2001_7, PKO_2001_8,
                      PKO_2001_9, PKO_2001_10, PKO_2001_11, PKO_2001_12)

remove(PKO_2001_1, PKO_2001_2, PKO_2001_3, PKO_2001_4,
       PKO_2001_5, PKO_2001_6, PKO_2001_7, PKO_2001_8,
       PKO_2001_9, PKO_2001_10, PKO_2001_11, PKO_2001_12,
       units_2001_1, units_2001_2, units_2001_3, units_2001_4,
       units_2001_5, units_2001_6, units_2001_7, units_2001_8,
       units_2001_9, units_2001_10, units_2001_11, units_2001_12)

## 2002 ##

nearest <- st_nearest_feature(PKO_2002_1, units_2002_1)
PKO_2002_1$dist <- st_distance(PKO_2002_1, units_2002_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_2, units_2002_2)
PKO_2002_2$dist <- st_distance(PKO_2002_2, units_2002_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_3, units_2002_3)
PKO_2002_3$dist <- st_distance(PKO_2002_3, units_2002_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_4, units_2002_4)
PKO_2002_4$dist <- st_distance(PKO_2002_4, units_2002_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_5, units_2002_5)
PKO_2002_5$dist <- st_distance(PKO_2002_5, units_2002_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_6, units_2002_6)
PKO_2002_6$dist <- st_distance(PKO_2002_6, units_2002_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_7, units_2002_7)
PKO_2002_7$dist <- st_distance(PKO_2002_7, units_2002_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_8, units_2002_8)
PKO_2002_8$dist <- st_distance(PKO_2002_8, units_2002_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_9, units_2002_9)
PKO_2002_9$dist <- st_distance(PKO_2002_9, units_2002_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_10, units_2002_10)
PKO_2002_10$dist <- st_distance(PKO_2002_10, units_2002_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_11, units_2002_11)
PKO_2002_11$dist <- st_distance(PKO_2002_11, units_2002_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2002_12, units_2002_12)
PKO_2002_12$dist <- st_distance(PKO_2002_12, units_2002_12[nearest,], by_element = TRUE)

PKO_2002 <- bind_rows(PKO_2002_1, PKO_2002_2, PKO_2002_3, PKO_2002_4,
                      PKO_2002_5, PKO_2002_6, PKO_2002_7, PKO_2002_8,
                      PKO_2002_9, PKO_2002_10, PKO_2002_11, PKO_2002_12)

remove(PKO_2002_1, PKO_2002_2, PKO_2002_3, PKO_2002_4,
       PKO_2002_5, PKO_2002_6, PKO_2002_7, PKO_2002_8,
       PKO_2002_9, PKO_2002_10, PKO_2002_11, PKO_2002_12,
       units_2002_1, units_2002_2, units_2002_3, units_2002_4,
       units_2002_5, units_2002_6, units_2002_7, units_2002_8,
       units_2002_9, units_2002_10, units_2002_11, units_2002_12)

## 2003 ##

nearest <- st_nearest_feature(PKO_2003_1, units_2003_1)
PKO_2003_1$dist <- st_distance(PKO_2003_1, units_2003_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_2, units_2003_2)
PKO_2003_2$dist <- st_distance(PKO_2003_2, units_2003_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_3, units_2003_3)
PKO_2003_3$dist <- st_distance(PKO_2003_3, units_2003_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_4, units_2003_4)
PKO_2003_4$dist <- st_distance(PKO_2003_4, units_2003_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_5, units_2003_5)
PKO_2003_5$dist <- st_distance(PKO_2003_5, units_2003_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_6, units_2003_6)
PKO_2003_6$dist <- st_distance(PKO_2003_6, units_2003_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_7, units_2003_7)
PKO_2003_7$dist <- st_distance(PKO_2003_7, units_2003_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_8, units_2003_8)
PKO_2003_8$dist <- st_distance(PKO_2003_8, units_2003_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_9, units_2003_9)
PKO_2003_9$dist <- st_distance(PKO_2003_9, units_2003_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_10, units_2003_10)
PKO_2003_10$dist <- st_distance(PKO_2003_10, units_2003_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_11, units_2003_11)
PKO_2003_11$dist <- st_distance(PKO_2003_11, units_2003_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2003_12, units_2003_12)
PKO_2003_12$dist <- st_distance(PKO_2003_12, units_2003_12[nearest,], by_element = TRUE)

PKO_2003 <- bind_rows(PKO_2003_1, PKO_2003_2, PKO_2003_3, PKO_2003_4,
                      PKO_2003_5, PKO_2003_6, PKO_2003_7, PKO_2003_8,
                      PKO_2003_9, PKO_2003_10, PKO_2003_11, PKO_2003_12)

remove(PKO_2003_1, PKO_2003_2, PKO_2003_3, PKO_2003_4,
       PKO_2003_5, PKO_2003_6, PKO_2003_7, PKO_2003_8,
       PKO_2003_9, PKO_2003_10, PKO_2003_11, PKO_2003_12,
       units_2003_1, units_2003_2, units_2003_3, units_2003_4,
       units_2003_5, units_2003_6, units_2003_7, units_2003_8,
       units_2003_9, units_2003_10, units_2003_11, units_2003_12)

## 2004 ##

nearest <- st_nearest_feature(PKO_2004_1, units_2004_1)
PKO_2004_1$dist <- st_distance(PKO_2004_1, units_2004_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_2, units_2004_2)
PKO_2004_2$dist <- st_distance(PKO_2004_2, units_2004_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_3, units_2004_3)
PKO_2004_3$dist <- st_distance(PKO_2004_3, units_2004_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_4, units_2004_4)
PKO_2004_4$dist <- st_distance(PKO_2004_4, units_2004_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_5, units_2004_5)
PKO_2004_5$dist <- st_distance(PKO_2004_5, units_2004_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_6, units_2004_6)
PKO_2004_6$dist <- st_distance(PKO_2004_6, units_2004_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_7, units_2004_7)
PKO_2004_7$dist <- st_distance(PKO_2004_7, units_2004_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_8, units_2004_8)
PKO_2004_8$dist <- st_distance(PKO_2004_8, units_2004_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_9, units_2004_9)
PKO_2004_9$dist <- st_distance(PKO_2004_9, units_2004_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_10, units_2004_10)
PKO_2004_10$dist <- st_distance(PKO_2004_10, units_2004_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_11, units_2004_11)
PKO_2004_11$dist <- st_distance(PKO_2004_11, units_2004_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2004_12, units_2004_12)
PKO_2004_12$dist <- st_distance(PKO_2004_12, units_2004_12[nearest,], by_element = TRUE)

PKO_2004 <- bind_rows(PKO_2004_1, PKO_2004_2, PKO_2004_3, PKO_2004_4,
                      PKO_2004_5, PKO_2004_6, PKO_2004_7, PKO_2004_8,
                      PKO_2004_9, PKO_2004_10, PKO_2004_11, PKO_2004_12)

remove(PKO_2004_1, PKO_2004_2, PKO_2004_3, PKO_2004_4,
       PKO_2004_5, PKO_2004_6, PKO_2004_7, PKO_2004_8,
       PKO_2004_9, PKO_2004_10, PKO_2004_11, PKO_2004_12,
       units_2004_1, units_2004_2, units_2004_3, units_2004_4,
       units_2004_5, units_2004_6, units_2004_7, units_2004_8,
       units_2004_9, units_2004_10, units_2004_11, units_2004_12)

## 2005 ##

nearest <- st_nearest_feature(PKO_2005_1, units_2005_1)
PKO_2005_1$dist <- st_distance(PKO_2005_1, units_2005_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_2, units_2005_2)
PKO_2005_2$dist <- st_distance(PKO_2005_2, units_2005_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_3, units_2005_3)
PKO_2005_3$dist <- st_distance(PKO_2005_3, units_2005_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_4, units_2005_4)
PKO_2005_4$dist <- st_distance(PKO_2005_4, units_2005_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_5, units_2005_5)
PKO_2005_5$dist <- st_distance(PKO_2005_5, units_2005_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_6, units_2005_6)
PKO_2005_6$dist <- st_distance(PKO_2005_6, units_2005_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_7, units_2005_7)
PKO_2005_7$dist <- st_distance(PKO_2005_7, units_2005_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_8, units_2005_8)
PKO_2005_8$dist <- st_distance(PKO_2005_8, units_2005_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_9, units_2005_9)
PKO_2005_9$dist <- st_distance(PKO_2005_9, units_2005_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_10, units_2005_10)
PKO_2005_10$dist <- st_distance(PKO_2005_10, units_2005_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_11, units_2005_11)
PKO_2005_11$dist <- st_distance(PKO_2005_11, units_2005_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2005_12, units_2005_12)
PKO_2005_12$dist <- st_distance(PKO_2005_12, units_2005_12[nearest,], by_element = TRUE)

PKO_2005 <- bind_rows(PKO_2005_1, PKO_2005_2, PKO_2005_3, PKO_2005_4,
                      PKO_2005_5, PKO_2005_6, PKO_2005_7, PKO_2005_8,
                      PKO_2005_9, PKO_2005_10, PKO_2005_11, PKO_2005_12)

remove(PKO_2005_1, PKO_2005_2, PKO_2005_3, PKO_2005_4,
       PKO_2005_5, PKO_2005_6, PKO_2005_7, PKO_2005_8,
       PKO_2005_9, PKO_2005_10, PKO_2005_11, PKO_2005_12,
       units_2005_1, units_2005_2, units_2005_3, units_2005_4,
       units_2005_5, units_2005_6, units_2005_7, units_2005_8,
       units_2005_9, units_2005_10, units_2005_11, units_2005_12)

## 2006 ##

nearest <- st_nearest_feature(PKO_2006_1, units_2006_1)
PKO_2006_1$dist <- st_distance(PKO_2006_1, units_2006_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_2, units_2006_2)
PKO_2006_2$dist <- st_distance(PKO_2006_2, units_2006_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_3, units_2006_3)
PKO_2006_3$dist <- st_distance(PKO_2006_3, units_2006_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_4, units_2006_4)
PKO_2006_4$dist <- st_distance(PKO_2006_4, units_2006_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_5, units_2006_5)
PKO_2006_5$dist <- st_distance(PKO_2006_5, units_2006_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_6, units_2006_6)
PKO_2006_6$dist <- st_distance(PKO_2006_6, units_2006_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_7, units_2006_7)
PKO_2006_7$dist <- st_distance(PKO_2006_7, units_2006_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_8, units_2006_8)
PKO_2006_8$dist <- st_distance(PKO_2006_8, units_2006_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_9, units_2006_9)
PKO_2006_9$dist <- st_distance(PKO_2006_9, units_2006_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_10, units_2006_10)
PKO_2006_10$dist <- st_distance(PKO_2006_10, units_2006_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_11, units_2006_11)
PKO_2006_11$dist <- st_distance(PKO_2006_11, units_2006_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2006_12, units_2006_12)
PKO_2006_12$dist <- st_distance(PKO_2006_12, units_2006_12[nearest,], by_element = TRUE)

PKO_2006 <- bind_rows(PKO_2006_1, PKO_2006_2, PKO_2006_3, PKO_2006_4,
                      PKO_2006_5, PKO_2006_6, PKO_2006_7, PKO_2006_8,
                      PKO_2006_9, PKO_2006_10, PKO_2006_11, PKO_2006_12)

remove(PKO_2006_1, PKO_2006_2, PKO_2006_3, PKO_2006_4,
       PKO_2006_5, PKO_2006_6, PKO_2006_7, PKO_2006_8,
       PKO_2006_9, PKO_2006_10, PKO_2006_11, PKO_2006_12,
       units_2006_1, units_2006_2, units_2006_3, units_2006_4,
       units_2006_5, units_2006_6, units_2006_7, units_2006_8,
       units_2006_9, units_2006_10, units_2006_11, units_2006_12)

## 2007 ##

nearest <- st_nearest_feature(PKO_2007_1, units_2007_1)
PKO_2007_1$dist <- st_distance(PKO_2007_1, units_2007_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_2, units_2007_2)
PKO_2007_2$dist <- st_distance(PKO_2007_2, units_2007_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_3, units_2007_3)
PKO_2007_3$dist <- st_distance(PKO_2007_3, units_2007_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_4, units_2007_4)
PKO_2007_4$dist <- st_distance(PKO_2007_4, units_2007_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_5, units_2007_5)
PKO_2007_5$dist <- st_distance(PKO_2007_5, units_2007_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_6, units_2007_6)
PKO_2007_6$dist <- st_distance(PKO_2007_6, units_2007_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_7, units_2007_7)
PKO_2007_7$dist <- st_distance(PKO_2007_7, units_2007_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_8, units_2007_8)
PKO_2007_8$dist <- st_distance(PKO_2007_8, units_2007_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_9, units_2007_9)
PKO_2007_9$dist <- st_distance(PKO_2007_9, units_2007_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_10, units_2007_10)
PKO_2007_10$dist <- st_distance(PKO_2007_10, units_2007_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_11, units_2007_11)
PKO_2007_11$dist <- st_distance(PKO_2007_11, units_2007_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2007_12, units_2007_12)
PKO_2007_12$dist <- st_distance(PKO_2007_12, units_2007_12[nearest,], by_element = TRUE)

PKO_2007 <- bind_rows(PKO_2007_1, PKO_2007_2, PKO_2007_3, PKO_2007_4,
                      PKO_2007_5, PKO_2007_6, PKO_2007_7, PKO_2007_8,
                      PKO_2007_9, PKO_2007_10, PKO_2007_11, PKO_2007_12)

remove(PKO_2007_1, PKO_2007_2, PKO_2007_3, PKO_2007_4,
       PKO_2007_5, PKO_2007_6, PKO_2007_7, PKO_2007_8,
       PKO_2007_9, PKO_2007_10, PKO_2007_11, PKO_2007_12,
       units_2007_1, units_2007_2, units_2007_3, units_2007_4,
       units_2007_5, units_2007_6, units_2007_7, units_2007_8,
       units_2007_9, units_2007_10, units_2007_11, units_2007_12)

## 2008 ##

nearest <- st_nearest_feature(PKO_2008_1, units_2008_1)
PKO_2008_1$dist <- st_distance(PKO_2008_1, units_2008_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_2, units_2008_2)
PKO_2008_2$dist <- st_distance(PKO_2008_2, units_2008_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_3, units_2008_3)
PKO_2008_3$dist <- st_distance(PKO_2008_3, units_2008_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_4, units_2008_4)
PKO_2008_4$dist <- st_distance(PKO_2008_4, units_2008_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_5, units_2008_5)
PKO_2008_5$dist <- st_distance(PKO_2008_5, units_2008_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_6, units_2008_6)
PKO_2008_6$dist <- st_distance(PKO_2008_6, units_2008_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_7, units_2008_7)
PKO_2008_7$dist <- st_distance(PKO_2008_7, units_2008_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_8, units_2008_8)
PKO_2008_8$dist <- st_distance(PKO_2008_8, units_2008_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_9, units_2008_9)
PKO_2008_9$dist <- st_distance(PKO_2008_9, units_2008_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_10, units_2008_10)
PKO_2008_10$dist <- st_distance(PKO_2008_10, units_2008_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_11, units_2008_11)
PKO_2008_11$dist <- st_distance(PKO_2008_11, units_2008_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2008_12, units_2008_12)
PKO_2008_12$dist <- st_distance(PKO_2008_12, units_2008_12[nearest,], by_element = TRUE)

PKO_2008 <- bind_rows(PKO_2008_1, PKO_2008_2, PKO_2008_3, PKO_2008_4,
                      PKO_2008_5, PKO_2008_6, PKO_2008_7, PKO_2008_8,
                      PKO_2008_9, PKO_2008_10, PKO_2008_11, PKO_2008_12)

remove(PKO_2008_1, PKO_2008_2, PKO_2008_3, PKO_2008_4,
       PKO_2008_5, PKO_2008_6, PKO_2008_7, PKO_2008_8,
       PKO_2008_9, PKO_2008_10, PKO_2008_11, PKO_2008_12,
       units_2008_1, units_2008_2, units_2008_3, units_2008_4,
       units_2008_5, units_2008_6, units_2008_7, units_2008_8,
       units_2008_9, units_2008_10, units_2008_11, units_2008_12)

## 2009 ##

nearest <- st_nearest_feature(PKO_2009_1, units_2009_1)
PKO_2009_1$dist <- st_distance(PKO_2009_1, units_2009_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_2, units_2009_2)
PKO_2009_2$dist <- st_distance(PKO_2009_2, units_2009_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_3, units_2009_3)
PKO_2009_3$dist <- st_distance(PKO_2009_3, units_2009_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_4, units_2009_4)
PKO_2009_4$dist <- st_distance(PKO_2009_4, units_2009_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_5, units_2009_5)
PKO_2009_5$dist <- st_distance(PKO_2009_5, units_2009_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_6, units_2009_6)
PKO_2009_6$dist <- st_distance(PKO_2009_6, units_2009_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_7, units_2009_7)
PKO_2009_7$dist <- st_distance(PKO_2009_7, units_2009_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_8, units_2009_8)
PKO_2009_8$dist <- st_distance(PKO_2009_8, units_2009_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_9, units_2009_9)
PKO_2009_9$dist <- st_distance(PKO_2009_9, units_2009_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_10, units_2009_10)
PKO_2009_10$dist <- st_distance(PKO_2009_10, units_2009_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_11, units_2009_11)
PKO_2009_11$dist <- st_distance(PKO_2009_11, units_2009_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2009_12, units_2009_12)
PKO_2009_12$dist <- st_distance(PKO_2009_12, units_2009_12[nearest,], by_element = TRUE)

PKO_2009 <- bind_rows(PKO_2009_1, PKO_2009_2, PKO_2009_3, PKO_2009_4,
                      PKO_2009_5, PKO_2009_6, PKO_2009_7, PKO_2009_8,
                      PKO_2009_9, PKO_2009_10, PKO_2009_11, PKO_2009_12)

remove(PKO_2009_1, PKO_2009_2, PKO_2009_3, PKO_2009_4,
       PKO_2009_5, PKO_2009_6, PKO_2009_7, PKO_2009_8,
       PKO_2009_9, PKO_2009_10, PKO_2009_11, PKO_2009_12,
       units_2009_1, units_2009_2, units_2009_3, units_2009_4,
       units_2009_5, units_2009_6, units_2009_7, units_2009_8,
       units_2009_9, units_2009_10, units_2009_11, units_2009_12)

## 2010 ##

nearest <- st_nearest_feature(PKO_2010_1, units_2010_1)
PKO_2010_1$dist <- st_distance(PKO_2010_1, units_2010_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_2, units_2010_2)
PKO_2010_2$dist <- st_distance(PKO_2010_2, units_2010_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_3, units_2010_3)
PKO_2010_3$dist <- st_distance(PKO_2010_3, units_2010_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_4, units_2010_4)
PKO_2010_4$dist <- st_distance(PKO_2010_4, units_2010_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_5, units_2010_5)
PKO_2010_5$dist <- st_distance(PKO_2010_5, units_2010_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_6, units_2010_6)
PKO_2010_6$dist <- st_distance(PKO_2010_6, units_2010_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_7, units_2010_7)
PKO_2010_7$dist <- st_distance(PKO_2010_7, units_2010_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_8, units_2010_8)
PKO_2010_8$dist <- st_distance(PKO_2010_8, units_2010_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_9, units_2010_9)
PKO_2010_9$dist <- st_distance(PKO_2010_9, units_2010_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_10, units_2010_10)
PKO_2010_10$dist <- st_distance(PKO_2010_10, units_2010_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_11, units_2010_11)
PKO_2010_11$dist <- st_distance(PKO_2010_11, units_2010_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2010_12, units_2010_12)
PKO_2010_12$dist <- st_distance(PKO_2010_12, units_2010_12[nearest,], by_element = TRUE)

PKO_2010 <- bind_rows(PKO_2010_1, PKO_2010_2, PKO_2010_3, PKO_2010_4,
                      PKO_2010_5, PKO_2010_6, PKO_2010_7, PKO_2010_8,
                      PKO_2010_9, PKO_2010_10, PKO_2010_11, PKO_2010_12)

remove(PKO_2010_1, PKO_2010_2, PKO_2010_3, PKO_2010_4,
       PKO_2010_5, PKO_2010_6, PKO_2010_7, PKO_2010_8,
       PKO_2010_9, PKO_2010_10, PKO_2010_11, PKO_2010_12,
       units_2010_1, units_2010_2, units_2010_3, units_2010_4,
       units_2010_5, units_2010_6, units_2010_7, units_2010_8,
       units_2010_9, units_2010_10, units_2010_11, units_2010_12)

## 2011 ##

nearest <- st_nearest_feature(PKO_2011_1, units_2011_1)
PKO_2011_1$dist <- st_distance(PKO_2011_1, units_2011_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_2, units_2011_2)
PKO_2011_2$dist <- st_distance(PKO_2011_2, units_2011_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_3, units_2011_3)
PKO_2011_3$dist <- st_distance(PKO_2011_3, units_2011_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_4, units_2011_4)
PKO_2011_4$dist <- st_distance(PKO_2011_4, units_2011_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_5, units_2011_5)
PKO_2011_5$dist <- st_distance(PKO_2011_5, units_2011_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_6, units_2011_6)
PKO_2011_6$dist <- st_distance(PKO_2011_6, units_2011_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_7, units_2011_7)
PKO_2011_7$dist <- st_distance(PKO_2011_7, units_2011_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_8, units_2011_8)
PKO_2011_8$dist <- st_distance(PKO_2011_8, units_2011_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_9, units_2011_9)
PKO_2011_9$dist <- st_distance(PKO_2011_9, units_2011_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_10, units_2011_10)
PKO_2011_10$dist <- st_distance(PKO_2011_10, units_2011_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_11, units_2011_11)
PKO_2011_11$dist <- st_distance(PKO_2011_11, units_2011_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2011_12, units_2011_12)
PKO_2011_12$dist <- st_distance(PKO_2011_12, units_2011_12[nearest,], by_element = TRUE)

PKO_2011 <- bind_rows(PKO_2011_1, PKO_2011_2, PKO_2011_3, PKO_2011_4,
                      PKO_2011_5, PKO_2011_6, PKO_2011_7, PKO_2011_8,
                      PKO_2011_9, PKO_2011_10, PKO_2011_11, PKO_2011_12)

remove(PKO_2011_1, PKO_2011_2, PKO_2011_3, PKO_2011_4,
       PKO_2011_5, PKO_2011_6, PKO_2011_7, PKO_2011_8,
       PKO_2011_9, PKO_2011_10, PKO_2011_11, PKO_2011_12,
       units_2011_1, units_2011_2, units_2011_3, units_2011_4,
       units_2011_5, units_2011_6, units_2011_7, units_2011_8,
       units_2011_9, units_2011_10, units_2011_11, units_2011_12)

## 2012 ##

nearest <- st_nearest_feature(PKO_2012_1, units_2012_1)
PKO_2012_1$dist <- st_distance(PKO_2012_1, units_2012_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_2, units_2012_2)
PKO_2012_2$dist <- st_distance(PKO_2012_2, units_2012_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_3, units_2012_3)
PKO_2012_3$dist <- st_distance(PKO_2012_3, units_2012_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_4, units_2012_4)
PKO_2012_4$dist <- st_distance(PKO_2012_4, units_2012_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_5, units_2012_5)
PKO_2012_5$dist <- st_distance(PKO_2012_5, units_2012_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_6, units_2012_6)
PKO_2012_6$dist <- st_distance(PKO_2012_6, units_2012_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_7, units_2012_7)
PKO_2012_7$dist <- st_distance(PKO_2012_7, units_2012_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_8, units_2012_8)
PKO_2012_8$dist <- st_distance(PKO_2012_8, units_2012_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_9, units_2012_9)
PKO_2012_9$dist <- st_distance(PKO_2012_9, units_2012_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_10, units_2012_10)
PKO_2012_10$dist <- st_distance(PKO_2012_10, units_2012_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_11, units_2012_11)
PKO_2012_11$dist <- st_distance(PKO_2012_11, units_2012_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2012_12, units_2012_12)
PKO_2012_12$dist <- st_distance(PKO_2012_12, units_2012_12[nearest,], by_element = TRUE)

PKO_2012 <- bind_rows(PKO_2012_1, PKO_2012_2, PKO_2012_3, PKO_2012_4,
                      PKO_2012_5, PKO_2012_6, PKO_2012_7, PKO_2012_8,
                      PKO_2012_9, PKO_2012_10, PKO_2012_11, PKO_2012_12)

remove(PKO_2012_1, PKO_2012_2, PKO_2012_3, PKO_2012_4,
       PKO_2012_5, PKO_2012_6, PKO_2012_7, PKO_2012_8,
       PKO_2012_9, PKO_2012_10, PKO_2012_11, PKO_2012_12,
       units_2012_1, units_2012_2, units_2012_3, units_2012_4,
       units_2012_5, units_2012_6, units_2012_7, units_2012_8,
       units_2012_9, units_2012_10, units_2012_11, units_2012_12)

## 2013 ##

nearest <- st_nearest_feature(PKO_2013_1, units_2013_1)
PKO_2013_1$dist <- st_distance(PKO_2013_1, units_2013_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_2, units_2013_2)
PKO_2013_2$dist <- st_distance(PKO_2013_2, units_2013_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_3, units_2013_3)
PKO_2013_3$dist <- st_distance(PKO_2013_3, units_2013_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_4, units_2013_4)
PKO_2013_4$dist <- st_distance(PKO_2013_4, units_2013_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_5, units_2013_5)
PKO_2013_5$dist <- st_distance(PKO_2013_5, units_2013_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_6, units_2013_6)
PKO_2013_6$dist <- st_distance(PKO_2013_6, units_2013_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_7, units_2013_7)
PKO_2013_7$dist <- st_distance(PKO_2013_7, units_2013_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_8, units_2013_8)
PKO_2013_8$dist <- st_distance(PKO_2013_8, units_2013_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_9, units_2013_9)
PKO_2013_9$dist <- st_distance(PKO_2013_9, units_2013_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_10, units_2013_10)
PKO_2013_10$dist <- st_distance(PKO_2013_10, units_2013_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_11, units_2013_11)
PKO_2013_11$dist <- st_distance(PKO_2013_11, units_2013_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2013_12, units_2013_12)
PKO_2013_12$dist <- st_distance(PKO_2013_12, units_2013_12[nearest,], by_element = TRUE)

PKO_2013 <- bind_rows(PKO_2013_1, PKO_2013_2, PKO_2013_3, PKO_2013_4,
                      PKO_2013_5, PKO_2013_6, PKO_2013_7, PKO_2013_8,
                      PKO_2013_9, PKO_2013_10, PKO_2013_11, PKO_2013_12)

remove(PKO_2013_1, PKO_2013_2, PKO_2013_3, PKO_2013_4,
       PKO_2013_5, PKO_2013_6, PKO_2013_7, PKO_2013_8,
       PKO_2013_9, PKO_2013_10, PKO_2013_11, PKO_2013_12,
       units_2013_1, units_2013_2, units_2013_3, units_2013_4,
       units_2013_5, units_2013_6, units_2013_7, units_2013_8,
       units_2013_9, units_2013_10, units_2013_11, units_2013_12)

## 2014 ##

nearest <- st_nearest_feature(PKO_2014_1, units_2014_1)
PKO_2014_1$dist <- st_distance(PKO_2014_1, units_2014_1[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_2, units_2014_2)
PKO_2014_2$dist <- st_distance(PKO_2014_2, units_2014_2[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_3, units_2014_3)
PKO_2014_3$dist <- st_distance(PKO_2014_3, units_2014_3[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_4, units_2014_4)
PKO_2014_4$dist <- st_distance(PKO_2014_4, units_2014_4[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_5, units_2014_5)
PKO_2014_5$dist <- st_distance(PKO_2014_5, units_2014_5[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_6, units_2014_6)
PKO_2014_6$dist <- st_distance(PKO_2014_6, units_2014_6[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_7, units_2014_7)
PKO_2014_7$dist <- st_distance(PKO_2014_7, units_2014_7[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_8, units_2014_8)
PKO_2014_8$dist <- st_distance(PKO_2014_8, units_2014_8[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_9, units_2014_9)
PKO_2014_9$dist <- st_distance(PKO_2014_9, units_2014_9[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_10, units_2014_10)
PKO_2014_10$dist <- st_distance(PKO_2014_10, units_2014_10[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_11, units_2014_11)
PKO_2014_11$dist <- st_distance(PKO_2014_11, units_2014_11[nearest,], by_element = TRUE)

nearest <- st_nearest_feature(PKO_2014_12, units_2014_12)
PKO_2014_12$dist <- st_distance(PKO_2014_12, units_2014_12[nearest,], by_element = TRUE)

PKO_2014 <- bind_rows(PKO_2014_1, PKO_2014_2, PKO_2014_3, PKO_2014_4,
                      PKO_2014_5, PKO_2014_6, PKO_2014_7, PKO_2014_8,
                      PKO_2014_9, PKO_2014_10, PKO_2014_11, PKO_2014_12)

remove(PKO_2014_1, PKO_2014_2, PKO_2014_3, PKO_2014_4,
       PKO_2014_5, PKO_2014_6, PKO_2014_7, PKO_2014_8,
       PKO_2014_9, PKO_2014_10, PKO_2014_11, PKO_2014_12,
       units_2014_1, units_2014_2, units_2014_3, units_2014_4,
       units_2014_5, units_2014_6, units_2014_7, units_2014_8,
       units_2014_9, units_2014_10, units_2014_11, units_2014_12)

### Combine mini-datasets back into a big one ###

Geo_PKO2 <- bind_rows(PKO_1994, PKO_1995, PKO_1996, PKO_1997,
                      PKO_1998, PKO_1999, PKO_2000, PKO_2001, 
                      PKO_2002, PKO_2003, PKO_2004, PKO_2005, 
                      PKO_2006, PKO_2007, PKO_2008, PKO_2009, 
                      PKO_2010, PKO_2011, PKO_2012, PKO_2013, 
                      PKO_2014)

remove(PKO_1994, PKO_1995, PKO_1996, PKO_1997,
       PKO_1998, PKO_1999, PKO_2000, PKO_2001,
       PKO_2002, PKO_2003, PKO_2004, PKO_2005,
       PKO_2006, PKO_2007, PKO_2008, PKO_2009, 
       PKO_2010, PKO_2011, PKO_2012, PKO_2013,
       PKO_2014, units)


# Merge onto original dataset #
Geo_PKO2 <- Geo_PKO2 %>%
  select(-no.troops, -PKO)

Geo_PKO2 <- st_set_geometry(Geo_PKO2, NULL)

Geo_PKO <- Geo_PKO %>%
  full_join(Geo_PKO2, by = c("gid", "month", "year"))

remove(Geo_PKO2, nearest)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Drop TCC Codes for Ease ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Geo_PKO <- Geo_PKO %>%
select(-tcc1, -tcc2, -tcc3, -tcc4, -tcc5, -tcc6, -tcc7, -tcc8,
       -tcc9, -tcc10, -tcc11, -tcc12, -tcc13, -tcc14, -tcc15, -tcc16,
       -tcc17, -tcc18, -tcc19, -tcc20, -tcc21, -tcc22, -tcc23, -tcc24,
       -tcc25, -tcc26, -tcc27, -tcc28, -tcc29, -tcc30, -tcc31, -tcc32,
       -tcc33, -tcc34, -tcc35, -tcc36, -tcc37, -tcc38, -tcc39, -tcc40,
       -tcc41, -tcc42, -tcc43, -tcc44, -tcc45, -tcc46, -tcc47)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Prep for export to STATA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(foreign)
library(haven)

Geo_PKO <- Geo_PKO %>%
  mutate(zone_de_confidence = zone.de.confidence,
         no_troops = no.troops,
         no_tcc = no.tcc) %>%
  select(-c(zone.de.confidence, col, row,
            latitude, longitude, ccode, ccode2,
            ccode3, ccode4, ccode5, missionen, no.troops,
            no.tcc))

Geo_PKO <- Geo_PKO %>%
  mutate(tcc_hq = ifelse(hq == 1, 1, 0),
         sector_hq = ifelse(hq == 2, 1, 0),
         mis_hq = ifelse(hq == 3, 1, 0 ),
         days_to_urban = ((time_nearest_urban/60)/24),
         dist_unit = dist/100000,
         dist_cap_hun = dist_cap/100,
         dist_cont_state = dist_cont_state/100,
         dist_border_neigh = dist_border_neigh/100,
         dist_border_own = dist_border_own/100,
         neigh_troops_thou = neigh_troops/1000,
         total_land_area = total_land_area/1000) %>%
  select(-c(dist, hq, time_nearest_urban, dist_cap, gwarea))

# Remove geometry to export as .dta file #
Geo_PKO <- st_set_geometry(Geo_PKO, NULL)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### First Occurrence of Violent Action SampleS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Any battle death #

# Mark cells that have ever had violence #
Geo_PKO <- Geo_PKO %>%
  mutate(best_dum = ifelse(best > 0,
                           1,
                           0))

# Create a date variable #
Geo_PKO <- Geo_PKO %>%
  mutate(date = zoo::as.yearmon(paste(year, month, sep = "-")),
         date = zoo:: as.Date(date))

# Data.frame of first time cell experienced violent action #
Gid_min <- Geo_PKO %>%
  filter(best_dum == 1) %>%
  select(gid, month, year, COW, best, best_dum, date) %>%
  group_by(gid) %>%
  summarise(date_min_best = min(date))

# Merge first time with main dataframe #
Geo_PKO <- Geo_PKO %>%
  left_join(x = . , y = Gid_min, by = c("gid"))

# Input maximum date into cells without violent action #
Geo_PKO <- Geo_PKO %>%
  mutate(date_min_best = ifelse(is.na(Geo_PKO$date_min_best),
                                max(Geo_PKO$date),
                                Geo_PKO$date_min_best),
         date_min_best = as.Date(date_min_best, origin = "1970-01-01"),
         best_ever = ifelse(date < date_min_best,
                            0,
                            1))


# Any OSV #

# Mark cells that have ever had violence #
Geo_PKO <- Geo_PKO %>%
  mutate(OSV_total_dum = ifelse(OSV_total > 0,
                           1,
                           0))

# Create a date variable #
Geo_PKO <- Geo_PKO %>%
  mutate(date = zoo::as.yearmon(paste(year, month, sep = "-")),
         date = zoo:: as.Date(date))

# Data.frame of first time cell experienced violent action #
Gid_min <- Geo_PKO %>%
  filter(OSV_total_dum == 1) %>%
  select(gid, month, year, COW, OSV_total, OSV_total_dum, date) %>%
  group_by(gid) %>%
  summarise(date_min_OSV_total = min(date))

# Merge first time with main dataframe #
Geo_PKO <- Geo_PKO %>%
  left_join(x = . , y = Gid_min, by = c("gid"))

# Input maximum date into cells without violent action #
Geo_PKO <- Geo_PKO %>%
  mutate(date_min_OSV_total = ifelse(is.na(Geo_PKO$date_min_OSV_total),
                           max(Geo_PKO$date),
                           Geo_PKO$date_min_OSV_total),
         date_min_OSV_total = as.Date(date_min_OSV_total, origin = "1970-01-01"),
         OSV_total_ever = ifelse(date < date_min_OSV_total,
                            0,
                            1))

# GOV OSV #

# Mark cells that have ever had violence #
Geo_PKO <- Geo_PKO %>%
  mutate(OSV_GOV_dum = ifelse(OSV_GOV > 0,
                                1,
                                0))

# Create a date variable #
Geo_PKO <- Geo_PKO %>%
  mutate(date = zoo::as.yearmon(paste(year, month, sep = "-")),
         date = zoo:: as.Date(date))

# Data.frame of first time cell experienced violent action #
Gid_min <- Geo_PKO %>%
  filter(OSV_GOV_dum == 1) %>%
  select(gid, month, year, COW, OSV_GOV, OSV_GOV_dum, date) %>%
  group_by(gid) %>%
  summarise(date_min_OSV_GOV = min(date))

# Merge first time with main dataframe #
Geo_PKO <- Geo_PKO %>%
  left_join(x = . , y = Gid_min, by = c("gid"))

# Input maximum date into cells without violent action #
Geo_PKO <- Geo_PKO %>%
  mutate(date_min_OSV_GOV = ifelse(is.na(Geo_PKO$date_min_OSV_GOV),
                                         max(Geo_PKO$date),
                                         Geo_PKO$date_min_OSV_GOV),
         date_min_OSV_GOV = as.Date(date_min_OSV_GOV, origin = "1970-01-01"),
         OSV_GOV_ever = ifelse(date < date_min_OSV_GOV,
                            0,
                            1))

# Rebs OSV #

# Mark cells that have ever had violence #
Geo_PKO <- Geo_PKO %>%
  mutate(OSV_Rebs_dum = ifelse(OSV_Rebs > 0,
                              1,
                              0))

# Create a date variable #
Geo_PKO <- Geo_PKO %>%
  mutate(date = zoo::as.yearmon(paste(year, month, sep = "-")),
         date = zoo:: as.Date(date))

# Data.frame of first time cell experienced violent action #
Gid_min <- Geo_PKO %>%
  filter(OSV_Rebs_dum == 1) %>%
  select(gid, month, year, COW, OSV_Rebs, OSV_Rebs_dum, date) %>%
  group_by(gid) %>%
  summarise(date_min_OSV_Rebs = min(date))

# Merge first time with main dataframe #
Geo_PKO <- Geo_PKO %>%
  left_join(x = . , y = Gid_min, by = c("gid"))

# Input maximum date into cells without violent action #
Geo_PKO <- Geo_PKO %>%
  mutate(date_min_OSV_Rebs = ifelse(is.na(Geo_PKO$date_min_OSV_Rebs),
                                   max(Geo_PKO$date),
                                   Geo_PKO$date_min_OSV_Rebs),
         date_min_OSV_Rebs = as.Date(date_min_OSV_Rebs, origin = "1970-01-01"),
         OSV_Rebs_ever = ifelse(date < date_min_OSV_Rebs,
                               0,
                               1))

### Drop Unneeded Variables from previous sample generation ###
Geo_PKO <- Geo_PKO %>%
  select(-c(best_dum, date, date_min_best, OSV_total_dum, date_min_OSV_total,
            OSV_GOV_dum, date_min_OSV_GOV, OSV_Rebs_dum, date_min_OSV_Rebs))

remove(Gid_min)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Government Overwatch Measure ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Geo_PKO <- Geo_PKO %>%
  mutate(Gov_watch = (liaise_warpart + goodoffices + election_monitor + election_security + election_assist +
           govcap_assist + govpolicies_assist + justice_assist + securitysectorreform_assist + policereform_assist +
           police_monitor + police_jointpatrols) / 12)


#~~~~~~~~~~~~~~~~~~~~~#
#### Troop Quality ####
#~~~~~~~~~~~~~~~~~~~~~#

# Geographical PKO dataset #
Geo <- readRDS("Geo_PKO/Geo-PKO-v-2-1.rds")


# Clean Geo dataset #
Geo <- Geo %>%
  select(prioid, mission, year, cow_code, month, no.troops, location,
         comment.on.unit, troop.type, comments,
         no.tcc, tcc1, tcc2, tcc3, tcc4, tcc5, tcc6, tcc7, tcc8, 
         tcc9, tcc10, tcc11, tcc12, tcc13, tcc14, tcc15,
         tcc16, tcc17, notroopspertcc_1, notroopspertcc_2,
         notroopspertcc_3, notroopspertcc_4, notroopspertcc_5,
         notroopspertcc_6, notroopspertcc_7, notroopspertcc_8,
         notroopspertcc_9, notroopspertcc_10, notroopspertcc_11,
         notroopspertcc_12, notroopspertcc_13, notroopspertcc_14,
         notroopspertcc_15, notroopspertcc_16, notroopspertcc_17) %>%
  filter(year >= 1994 & year <= 2014)

# Remove cells where no.troops is unknown #
Geo <- subset(Geo, no.troops != "unknown")

# Make troop unknowns equal to 0 #
Geo$notroopspertcc_1 <- str_replace_all(Geo$notroopspertcc_1, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_2 <- str_replace_all(Geo$notroopspertcc_2, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_3 <- str_replace_all(Geo$notroopspertcc_3, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_4 <- str_replace_all(Geo$notroopspertcc_4, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_5 <- str_replace_all(Geo$notroopspertcc_5, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_6 <- str_replace_all(Geo$notroopspertcc_6, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_7 <- str_replace_all(Geo$notroopspertcc_7, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_8 <- str_replace_all(Geo$notroopspertcc_8, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_9 <- str_replace_all(Geo$notroopspertcc_9, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_10 <- str_replace_all(Geo$notroopspertcc_10, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_11 <- str_replace_all(Geo$notroopspertcc_11, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_12 <- str_replace_all(Geo$notroopspertcc_12, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_13 <- str_replace_all(Geo$notroopspertcc_13, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_14 <- str_replace_all(Geo$notroopspertcc_14, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_15 <- str_replace_all(Geo$notroopspertcc_15, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_16 <- str_replace_all(Geo$notroopspertcc_16, pattern = "unknown", replacement = "NA")
Geo$notroopspertcc_17 <- str_replace_all(Geo$notroopspertcc_17, pattern = "unknown", replacement = "NA")


# Fix Geo where there is 531,530 #
# These locations are in Eritrea
Geo$cow_code[Geo$location == "Badme" & Geo$prioid == 150916 & Geo$year == 2006] <- 531
Geo$cow_code[Geo$location == "Badme" & Geo$prioid == 150916 & Geo$year == 2007] <- 531
Geo$cow_code[Geo$location == "Badme" & Geo$prioid == 150916 & Geo$year == 2008] <- 531
Geo$cow_code[Geo$location == "Deda Lala" & Geo$prioid == 150916 & Geo$year == 2006] <- 531
Geo$cow_code[Geo$location == "Deda Lala" & Geo$prioid == 150916 & Geo$year == 2007] <- 531
Geo$cow_code[Geo$location == "Deda Lala" & Geo$prioid == 150916 & Geo$year == 2008] <- 531



# Alter variable classes #
Geo <- Geo %>%
  mutate(cow_code = as.numeric(cow_code),
         no.troops = as.numeric(no.troops),
         no.tcc = as.numeric(no.tcc))


# Collapse by PRIO GRID ID, fix TCCs #
Geo <- Geo %>%
  group_by(prioid, month, year) %>%
  summarize(prioid = first(prioid),
            mission = first(mission), month = first(month),
            year = first(year), cow_code = first(cow_code),
            no.troops = sum(no.troops), no.tcc = sum(no.tcc),
            tcc1 = paste(tcc1, collapse = ","), tcc2 = paste(tcc2, collapse = ","),
            tcc3 = paste(tcc3, collapse = ","), tcc4 = paste(tcc4, collapse = ","),
            tcc5 = paste(tcc5, collapse = ","), tcc6 = paste(tcc6, collapse = ","),
            tcc7 = paste(tcc7, collapse = ","), tcc8 = paste(tcc8, collapse = ","),
            tcc9 = paste(tcc9, collapse = ","), tcc10 = paste(tcc10, collapse = ","),
            tcc11 = paste(tcc11, collapse = ","), tcc12 = paste(tcc12, collapse = ","),
            tcc13 = paste(tcc13, collapse = ","), tcc14 = paste(tcc14, collapse = ","),
            tcc15 = paste(tcc15, collapse = ","), tcc16 = paste(tcc16, collapse = ","),
            tcc17 = paste(tcc17, collapse = ","), notroopspertcc_1 = paste(notroopspertcc_1, collapse = ","),
            notroopspertcc_2 = paste(notroopspertcc_2, collpase = ","), notroopspertcc_3 = paste(notroopspertcc_3, collapse = ","),
            notroopspertcc_4 = paste(notroopspertcc_4, collapse = ","), notroopspertcc_5 = paste(notroopspertcc_5, collapse = ","),
            notroopspertcc_6 = paste(notroopspertcc_6, collapse = ","), notroopspertcc_7 = paste(notroopspertcc_7, collapse = ","),
            notroopspertcc_8 = paste(notroopspertcc_8, collapse = ","), notroopspertcc_9 = paste(notroopspertcc_9, collapse = ","),
            notroopspertcc_10 = paste(notroopspertcc_10, collpase = ","), notroopspertcc_11 = paste(notroopspertcc_11, collapse = ","),
            notroopspertcc_12 = paste(notroopspertcc_12, collapse = ","), notroopspertcc_13 = paste(notroopspertcc_13, collapse = ","),
            notroopspertcc_14 = paste(notroopspertcc_14, collapse = ","), notroopspertcc_15 = paste(notroopspertcc_15, collapse = ","),
            notroopspertcc_16 = paste(notroopspertcc_16, collapse = ","), notroopspertcc_17 = paste(notroopspertcc_17, collapse = ",")) %>%
  ungroup()


### Drop cells without troops ###
Geo <- subset(Geo, no.troops != 0)


### TCCs ###


# Need to separate the TCCs from the collapsed columns #
# Remove NA's. First as beginning or middle of series.
Geo$tcc1 <- str_replace_all(Geo$tcc1, pattern = "NA,", replacement = "")
Geo$tcc2 <- str_replace_all(Geo$tcc2, pattern = "NA,", replacement = "")
Geo$tcc3 <- str_replace_all(Geo$tcc3, pattern = "NA,", replacement = "")
Geo$tcc4 <- str_replace_all(Geo$tcc4, pattern = "NA,", replacement = "")
Geo$tcc5 <- str_replace_all(Geo$tcc5, pattern = "NA,", replacement = "")
Geo$tcc6 <- str_replace_all(Geo$tcc6, pattern = "NA,", replacement = "")
Geo$tcc7 <- str_replace_all(Geo$tcc7, pattern = "NA,", replacement = "")
Geo$tcc8 <- str_replace_all(Geo$tcc8, pattern = "NA,", replacement = "")
Geo$tcc9 <- str_replace_all(Geo$tcc9, pattern = "NA,", replacement = "")
Geo$tcc10 <- str_replace_all(Geo$tcc10, pattern = "NA,", replacement = "")
Geo$tcc11 <- str_replace_all(Geo$tcc11, pattern = "NA,", replacement = "")
Geo$tcc12 <- str_replace_all(Geo$tcc12, pattern = "NA,", replacement = "")
Geo$tcc13 <- str_replace_all(Geo$tcc13, pattern = "NA,", replacement = "")
Geo$tcc14 <- str_replace_all(Geo$tcc14, pattern = "NA,", replacement = "")
Geo$tcc15 <- str_replace_all(Geo$tcc15, pattern = "NA,", replacement = "")
Geo$tcc16 <- str_replace_all(Geo$tcc16, pattern = "NA,", replacement = "")
Geo$tcc17 <- str_replace_all(Geo$tcc17, pattern = "NA,", replacement = "")


# Second at the end of the series #
Geo$tcc1 <- str_replace_all(Geo$tcc1, pattern = ",NA", replacement = "")
Geo$tcc2 <- str_replace_all(Geo$tcc2, pattern = ",NA", replacement = "")
Geo$tcc3 <- str_replace_all(Geo$tcc3, pattern = ",NA", replacement = "")
Geo$tcc4 <- str_replace_all(Geo$tcc4, pattern = ",NA", replacement = "")
Geo$tcc5 <- str_replace_all(Geo$tcc5, pattern = ",NA", replacement = "")
Geo$tcc6 <- str_replace_all(Geo$tcc6, pattern = ",NA", replacement = "")
Geo$tcc7 <- str_replace_all(Geo$tcc7, pattern = ",NA", replacement = "")
Geo$tcc8 <- str_replace_all(Geo$tcc8, pattern = ",NA", replacement = "")
Geo$tcc9 <- str_replace_all(Geo$tcc9, pattern = ",NA", replacement = "")
Geo$tcc10 <- str_replace_all(Geo$tcc10, pattern = ",NA", replacement = "")
Geo$tcc11 <- str_replace_all(Geo$tcc11, pattern = ",NA", replacement = "")
Geo$tcc12 <- str_replace_all(Geo$tcc12, pattern = ",NA", replacement = "")
Geo$tcc13 <- str_replace_all(Geo$tcc13, pattern = ",NA", replacement = "")
Geo$tcc14 <- str_replace_all(Geo$tcc14, pattern = ",NA", replacement = "")
Geo$tcc15 <- str_replace_all(Geo$tcc15, pattern = ",NA", replacement = "")
Geo$tcc16 <- str_replace_all(Geo$tcc16, pattern = ",NA", replacement = "")
Geo$tcc17 <- str_replace_all(Geo$tcc17, pattern = ",NA", replacement = "")


# Split tcc variables to have codes one at a time #
Geo <- Geo %>%
  separate(tcc1, paste('tcc1', 1:19, sep = "_"), sep = ",") %>%
  separate(tcc2, paste('tcc2', 1:4, sep = "_"), sep = ",") %>%
  separate(tcc3, paste('tcc3', 1:3, sep = "_"), sep = ",") %>%
  separate(tcc4, paste('tcc4', 1:3, sep = "_"), sep = ",") %>%
  separate(tcc5, paste('tcc5', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc6, paste('tcc6', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc7, paste('tcc7', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc8, paste('tcc8', 1:2, sep = "_"), sep = ",") %>%
  separate(tcc9, paste('tcc9', 1:2, sep = "_"), sep = ",")


# Rename the variables made from the splits #
Geo <- Geo %>%
  mutate(tcc1 = tcc1_1, tcc2 = tcc1_2, tcc3 = tcc1_3, tcc4 = tcc1_4,
         tcc5 = tcc1_5, tcc6 = tcc1_6, tcc7 = tcc1_7, tcc8 = tcc1_8,
         tcc9 = tcc1_9, tcc10 = tcc1_10, tcc11 = tcc1_11, tcc12 = tcc1_12,
         tcc13 = tcc1_13, tcc14 = tcc1_14, tcc15 = tcc1_15, tcc16 = tcc1_16,
         tcc17 = tcc1_17, tcc18 = tcc1_18, tcc19 = tcc1_19, tcc20 = tcc2_1,
         tcc21 = tcc2_2, tcc22 = tcc2_3, tcc23 = tcc2_4, tcc24 = tcc3_1,
         tcc25 = tcc3_2, tcc26 = tcc3_3, tcc27 = tcc4_1, tcc28 = tcc4_2,
         tcc29 = tcc4_3, tcc30 = tcc5_1, tcc31 = tcc5_2, tcc32 = tcc6_1,
         tcc33 = tcc6_2, tcc34 = tcc7_1, tcc35 = tcc7_2, tcc36 = tcc8_1, 
         tcc37 = tcc8_2, tcc38 = tcc9_1, tcc39 = tcc9_2, tcc40 = tcc10,
         tcc41 = tcc11, tcc42 = tcc12, tcc43 = tcc13, tcc44 = tcc14,
         tcc45 = tcc15, tcc46 = tcc16, tcc47 = tcc17) %>%
  select(-tcc1_1, -tcc1_2, -tcc1_3, -tcc1_4, -tcc1_5, -tcc1_6, -tcc1_7,
         -tcc1_8, -tcc1_9, -tcc1_10, -tcc1_11, -tcc1_12, -tcc1_13, -tcc1_14,
         -tcc1_15, -tcc1_16, -tcc1_17, -tcc1_18, -tcc1_19, -tcc2_1, -tcc2_2,
         -tcc2_3, -tcc2_4, -tcc3_1, -tcc3_2, -tcc3_3, -tcc4_1, -tcc4_2, -tcc4_3,
         -tcc5_1, -tcc5_2, -tcc6_1, -tcc6_2, -tcc7_1, -tcc7_2, -tcc8_1, -tcc8_2,
         -tcc9_1, -tcc9_2)



### Troops ###

# Need to separate the TCCs from the collapsed columns #
# Remove NA's. First as beginning or middle of series.
Geo$notroopspertcc_1 <- str_replace_all(Geo$notroopspertcc_1, pattern = "NA,", replacement = "")
Geo$notroopspertcc_2 <- str_replace_all(Geo$notroopspertcc_2, pattern = "NA,", replacement = "")
Geo$notroopspertcc_3 <- str_replace_all(Geo$notroopspertcc_3, pattern = "NA,", replacement = "")
Geo$notroopspertcc_4 <- str_replace_all(Geo$notroopspertcc_4, pattern = "NA,", replacement = "")
Geo$notroopspertcc_5 <- str_replace_all(Geo$notroopspertcc_5, pattern = "NA,", replacement = "")
Geo$notroopspertcc_6 <- str_replace_all(Geo$notroopspertcc_6, pattern = "NA,", replacement = "")
Geo$notroopspertcc_7 <- str_replace_all(Geo$notroopspertcc_7, pattern = "NA,", replacement = "")
Geo$notroopspertcc_8 <- str_replace_all(Geo$notroopspertcc_8, pattern = "NA,", replacement = "")
Geo$notroopspertcc_9 <- str_replace_all(Geo$notroopspertcc_9, pattern = "NA,", replacement = "")
Geo$notroopspertcc_10 <- str_replace_all(Geo$notroopspertcc_10, pattern = "NA,", replacement = "")
Geo$notroopspertcc_11 <- str_replace_all(Geo$notroopspertcc_11, pattern = "NA,", replacement = "")
Geo$notroopspertcc_12 <- str_replace_all(Geo$notroopspertcc_12, pattern = "NA,", replacement = "")
Geo$notroopspertcc_13 <- str_replace_all(Geo$notroopspertcc_13, pattern = "NA,", replacement = "")
Geo$notroopspertcc_14 <- str_replace_all(Geo$notroopspertcc_14, pattern = "NA,", replacement = "")
Geo$notroopspertcc_15 <- str_replace_all(Geo$notroopspertcc_15, pattern = "NA,", replacement = "")
Geo$notroopspertcc_16 <- str_replace_all(Geo$notroopspertcc_16, pattern = "NA,", replacement = "")
Geo$notroopspertcc_17 <- str_replace_all(Geo$notroopspertcc_17, pattern = "NA,", replacement = "")


# Second at the end of the series #
Geo$notroopspertcc_1 <- str_replace_all(Geo$notroopspertcc_1, pattern = ",NA", replacement = "")
Geo$notroopspertcc_2 <- str_replace_all(Geo$notroopspertcc_2, pattern = ",NA", replacement = "")
Geo$notroopspertcc_3 <- str_replace_all(Geo$notroopspertcc_3, pattern = ",NA", replacement = "")
Geo$notroopspertcc_4 <- str_replace_all(Geo$notroopspertcc_4, pattern = ",NA", replacement = "")
Geo$notroopspertcc_5 <- str_replace_all(Geo$notroopspertcc_5, pattern = ",NA", replacement = "")
Geo$notroopspertcc_6 <- str_replace_all(Geo$notroopspertcc_6, pattern = ",NA", replacement = "")
Geo$notroopspertcc_7 <- str_replace_all(Geo$notroopspertcc_7, pattern = ",NA", replacement = "")
Geo$notroopspertcc_8 <- str_replace_all(Geo$notroopspertcc_8, pattern = ",NA", replacement = "")
Geo$notroopspertcc_9 <- str_replace_all(Geo$notroopspertcc_9, pattern = ",NA", replacement = "")
Geo$notroopspertcc_10 <- str_replace_all(Geo$notroopspertcc_10, pattern = ",NA", replacement = "")
Geo$notroopspertcc_11 <- str_replace_all(Geo$notroopspertcc_11, pattern = ",NA", replacement = "")
Geo$notroopspertcc_12 <- str_replace_all(Geo$notroopspertcc_12, pattern = ",NA", replacement = "")
Geo$notroopspertcc_13 <- str_replace_all(Geo$notroopspertcc_13, pattern = ",NA", replacement = "")
Geo$notroopspertcc_14 <- str_replace_all(Geo$notroopspertcc_14, pattern = ",NA", replacement = "")
Geo$notroopspertcc_15 <- str_replace_all(Geo$notroopspertcc_15, pattern = ",NA", replacement = "")
Geo$notroopspertcc_16 <- str_replace_all(Geo$notroopspertcc_16, pattern = ",NA", replacement = "")
Geo$notroopspertcc_17 <- str_replace_all(Geo$notroopspertcc_17, pattern = ",NA", replacement = "")


# Split troop count variables to have counts one at a time #
Geo <- Geo %>%
  separate(notroopspertcc_1, paste('notroopspertcc_1', 1:19, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_2, paste('notroopspertcc_2', 1:4, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_3, paste('notroopspertcc_3', 1:3, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_4, paste('notroopspertcc_4', 1:3, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_5, paste('notroopspertcc_5', 1:2, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_6, paste('notroopspertcc_6', 1:2, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_7, paste('notroopspertcc_7', 1:2, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_8, paste('notroopspertcc_8', 1:2, sep = "_"), sep = ",") %>%
  separate(notroopspertcc_9, paste('notroopspertcc_9', 1:2, sep = "_"), sep = ",")


# Rename the variables made from the splits #
Geo <- Geo %>%
  mutate(notroopspertcc1 = notroopspertcc_1_1, notroopspertcc2 = notroopspertcc_1_2, notroopspertcc3 = notroopspertcc_1_3, notroopspertcc4 = notroopspertcc_1_4,
         notroopspertcc5 = notroopspertcc_1_5, notroopspertcc6 = notroopspertcc_1_6, notroopspertcc7 = notroopspertcc_1_7, notroopspertcc8 = notroopspertcc_1_8,
         notroopspertcc9 = notroopspertcc_1_9, notroopspertcc10 = notroopspertcc_1_10, notroopspertcc11 = notroopspertcc_1_11, notroopspertcc12 = notroopspertcc_1_12,
         notroopspertcc13 = notroopspertcc_1_13, notroopspertcc14 = notroopspertcc_1_14, notroopspertcc15 = notroopspertcc_1_15, notroopspertcc16 = notroopspertcc_1_16,
         notroopspertcc17 = notroopspertcc_1_17, notroopspertcc18 = notroopspertcc_1_18, notroopspertcc19 = notroopspertcc_1_19, notroopspertcc20 = notroopspertcc_2_1,
         notroopspertcc21 = notroopspertcc_2_2, notroopspertcc22 = notroopspertcc_2_3, notroopspertcc23 = notroopspertcc_2_4, notroopspertcc24 = notroopspertcc_3_1,
         notroopspertcc25 = notroopspertcc_3_2, notroopspertcc26 = notroopspertcc_3_3, notroopspertcc27 = notroopspertcc_4_1, notroopspertcc28 = notroopspertcc_4_2,
         notroopspertcc29 = notroopspertcc_4_3, notroopspertcc30 = notroopspertcc_5_1, notroopspertcc31 = notroopspertcc_5_2, notroopspertcc32 = notroopspertcc_6_1,
         notroopspertcc33 = notroopspertcc_6_2, notroopspertcc34 = notroopspertcc_7_1, notroopspertcc35 = notroopspertcc_7_2, notroopspertcc36 = notroopspertcc_8_1, 
         notroopspertcc37 = notroopspertcc_8_2, notroopspertcc38 = notroopspertcc_9_1, notroopspertcc39 = notroopspertcc_9_2, notroopspertcc40 = notroopspertcc_10,
         notroopspertcc41 = notroopspertcc_11, notroopspertcc42 = notroopspertcc_12, notroopspertcc43 = notroopspertcc_13, notroopspertcc44 = notroopspertcc_14,
         notroopspertcc45 = notroopspertcc_15, notroopspertcc46 = notroopspertcc_16, notroopspertcc47 = notroopspertcc_17) %>%
  select(-notroopspertcc_1_1, -notroopspertcc_1_2, -notroopspertcc_1_3, -notroopspertcc_1_4, -notroopspertcc_1_5, -notroopspertcc_1_6, -notroopspertcc_1_7,
         -notroopspertcc_1_8, -notroopspertcc_1_9, -notroopspertcc_1_10, -notroopspertcc_1_11, -notroopspertcc_1_12, -notroopspertcc_1_13, -notroopspertcc_1_14,
         -notroopspertcc_1_15, -notroopspertcc_1_16, -notroopspertcc_1_17, -notroopspertcc_1_18, -notroopspertcc_1_19, -notroopspertcc_2_1, -notroopspertcc_2_2,
         -notroopspertcc_2_3, -notroopspertcc_2_4, -notroopspertcc_3_1, -notroopspertcc_3_2, -notroopspertcc_3_3, -notroopspertcc_4_1, -notroopspertcc_4_2, -notroopspertcc_4_3,
         -notroopspertcc_5_1, -notroopspertcc_5_2, -notroopspertcc_6_1, -notroopspertcc_6_2, -notroopspertcc_7_1, -notroopspertcc_7_2, -notroopspertcc_8_1, -notroopspertcc_8_2,
         -notroopspertcc_9_1, -notroopspertcc_9_2, -notroopspertcc_10, -notroopspertcc_11, -notroopspertcc_12, -notroopspertcc_13,
         -notroopspertcc_14, -notroopspertcc_15, -notroopspertcc_16, -notroopspertcc_17)


# Convert into numeric data #
Geo <- Geo %>%
  mutate(tcc1 = as.numeric(tcc1), tcc2 = as.numeric(tcc2), tcc3 = as.numeric(tcc3), tcc4 = as.numeric(tcc4), tcc5 = as.numeric(tcc5),
         tcc6 = as.numeric(tcc6), tcc7 = as.numeric(tcc7), tcc8 = as.numeric(tcc8), tcc9 = as.numeric(tcc9), tcc10 = as.numeric(tcc10),
         tcc11 = as.numeric(tcc11), tcc12 = as.numeric(tcc12), tcc13 = as.numeric(tcc13), tcc14 = as.numeric(tcc14), tcc15 = as.numeric(tcc15),
         tcc16 = as.numeric(tcc16), tcc17 = as.numeric(tcc17), tcc18 = as.numeric(tcc18), tcc19 = as.numeric(tcc19), tcc20 = as.numeric(tcc20),
         tcc21 = as.numeric(tcc21), tcc22 = as.numeric(tcc22), tcc23 = as.numeric(tcc23), tcc24 = as.numeric(tcc24), tcc25 = as.numeric(tcc25),
         tcc26 = as.numeric(tcc26), tcc27 = as.numeric(tcc27), tcc28 = as.numeric(tcc28), tcc29 = as.numeric(tcc29), tcc30 = as.numeric(tcc30),
         tcc31 = as.numeric(tcc31), tcc32 = as.numeric(tcc32), tcc33 = as.numeric(tcc33), tcc34 = as.numeric(tcc34), tcc35 = as.numeric(tcc35),
         tcc36 = as.numeric(tcc36), tcc37 = as.numeric(tcc37), tcc38 = as.numeric(tcc38), tcc39 = as.numeric(tcc39), tcc40 = as.numeric(tcc40),
         tcc41 = as.numeric(tcc41), tcc42 = as.numeric(tcc42), tcc43 = as.numeric(tcc43), tcc44 = as.numeric(tcc44), tcc45 = as.numeric(tcc45),
         tcc46 = as.numeric(tcc46), tcc47 = as.numeric(tcc47))

Geo <- Geo %>%
  mutate(notroopspertcc1 = as.numeric(notroopspertcc1), notroopspertcc2 = as.numeric(notroopspertcc2), notroopspertcc3 = as.numeric(notroopspertcc3),
         notroopspertcc4 = as.numeric(notroopspertcc4), notroopspertcc5 = as.numeric(notroopspertcc5), notroopspertcc6 = as.numeric(notroopspertcc6),
         notroopspertcc7 = as.numeric(notroopspertcc7), notroopspertcc8 = as.numeric(notroopspertcc8), notroopspertcc9 = as.numeric(notroopspertcc9),
         notroopspertcc10 = as.numeric(notroopspertcc10), notroopspertcc11 = as.numeric(notroopspertcc11), notroopspertcc12 = as.numeric(notroopspertcc12),
         notroopspertcc13 = as.numeric(notroopspertcc13), notroopspertcc14 = as.numeric(notroopspertcc14), notroopspertcc15 = as.numeric(notroopspertcc15),
         notroopspertcc16 = as.numeric(notroopspertcc16), notroopspertcc17 = as.numeric(notroopspertcc17), notroopspertcc18 = as.numeric(notroopspertcc18),
         notroopspertcc19 = as.numeric(notroopspertcc19), notroopspertcc20 = as.numeric(notroopspertcc20), notroopspertcc21 = as.numeric(notroopspertcc21),
         notroopspertcc22 = as.numeric(notroopspertcc22), notroopspertcc23 = as.numeric(notroopspertcc23), notroopspertcc24 = as.numeric(notroopspertcc24),
         notroopspertcc25 = as.numeric(notroopspertcc25), notroopspertcc26 = as.numeric(notroopspertcc26), notroopspertcc27 = as.numeric(notroopspertcc27),
         notroopspertcc28 = as.numeric(notroopspertcc28), notroopspertcc29 = as.numeric(notroopspertcc29), notroopspertcc30 = as.numeric(notroopspertcc30),
         notroopspertcc31 = as.numeric(notroopspertcc31), notroopspertcc32 = as.numeric(notroopspertcc32), notroopspertcc33 = as.numeric(notroopspertcc33),
         notroopspertcc34 = as.numeric(notroopspertcc34), notroopspertcc35 = as.numeric(notroopspertcc35), notroopspertcc36 = as.numeric(notroopspertcc36),
         notroopspertcc37 = as.numeric(notroopspertcc37), notroopspertcc38 = as.numeric(notroopspertcc38), notroopspertcc39 = as.numeric(notroopspertcc39),
         notroopspertcc40 = as.numeric(notroopspertcc40), notroopspertcc41 = as.numeric(notroopspertcc41), notroopspertcc42 = as.numeric(notroopspertcc42),
         notroopspertcc43 = as.numeric(notroopspertcc43), notroopspertcc44 = as.numeric(notroopspertcc44), notroopspertcc45 = as.numeric(notroopspertcc45),
         notroopspertcc46 = as.numeric(notroopspertcc46), notroopspertcc47 = as.numeric(notroopspertcc47))

# Order variables #
Geo <- Geo[, c("prioid", "month", "year", "mission", "tcc1", "tcc2", "tcc3",
               "tcc4", "tcc5", "tcc6", "tcc7", "tcc8", "tcc9", "tcc10",
               "tcc11", "tcc12", "tcc13", "tcc14", "tcc15", "tcc16", "tcc17",
               "tcc18", "tcc19", "tcc20", "tcc21", "tcc22", "tcc23", "tcc24",
               "tcc25", "tcc26", "tcc27", "tcc28", "tcc29", "tcc30", "tcc31",
               "tcc32", "tcc33", "tcc34", "tcc35", "tcc36", "tcc37", "tcc38",
               "tcc39", "tcc40", "tcc41", "tcc42", "tcc43", "tcc44", "tcc45",
               "tcc46", "tcc47",
               "notroopspertcc1", "notroopspertcc2", "notroopspertcc3", "notroopspertcc4",
               "notroopspertcc5", "notroopspertcc6", "notroopspertcc7", "notroopspertcc8",
               "notroopspertcc9", "notroopspertcc10", "notroopspertcc11", "notroopspertcc12",
               "notroopspertcc13", "notroopspertcc14", "notroopspertcc15", "notroopspertcc16",
               "notroopspertcc17", "notroopspertcc18", "notroopspertcc19", "notroopspertcc20",
               "notroopspertcc21", "notroopspertcc22", "notroopspertcc23", "notroopspertcc24",
               "notroopspertcc25", "notroopspertcc26", "notroopspertcc27", "notroopspertcc28",
               "notroopspertcc29", "notroopspertcc30", "notroopspertcc31", "notroopspertcc32",
               "notroopspertcc33", "notroopspertcc34", "notroopspertcc35", "notroopspertcc36",
               "notroopspertcc37", "notroopspertcc38", "notroopspertcc39", "notroopspertcc40",
               "notroopspertcc41", "notroopspertcc42", "notroopspertcc43", "notroopspertcc44",
               "notroopspertcc45", "notroopspertcc46", "notroopspertcc47")]

## Get CINC scores from peacesicencer ##
CINC <- create_stateyears(system = "cow", subset_years = c(1994:2014)) %>%
  add_nmc() %>%
  mutate(qual = milex / milper) %>%
  select(ccode, year, qual) %>%
  mutate(cinc = qual) %>%
  select(-c(qual))

## Match Each TCC code for CINC. 1 - 47 ##
Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc1" = "ccode", "year")) %>%
  mutate(cinc_tcc1 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc2" = "ccode", "year")) %>%
  mutate(cinc_tcc2 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc3" = "ccode", "year")) %>%
  mutate(cinc_tcc3 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc4" = "ccode", "year")) %>%
  mutate(cinc_tcc4 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc5" = "ccode", "year")) %>%
  mutate(cinc_tcc5 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc6" = "ccode", "year")) %>%
  mutate(cinc_tcc6 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc7" = "ccode", "year")) %>%
  mutate(cinc_tcc7 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc8" = "ccode", "year")) %>%
  mutate(cinc_tcc8 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc9" = "ccode", "year")) %>%
  mutate(cinc_tcc9 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc10" = "ccode", "year")) %>%
  mutate(cinc_tcc10 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc11" = "ccode", "year")) %>%
  mutate(cinc_tcc11 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc12" = "ccode", "year")) %>%
  mutate(cinc_tcc12 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc13" = "ccode", "year")) %>%
  mutate(cinc_tcc13 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc14" = "ccode", "year")) %>%
  mutate(cinc_tcc14 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc15" = "ccode", "year")) %>%
  mutate(cinc_tcc15 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc16" = "ccode", "year")) %>%
  mutate(cinc_tcc16 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc17" = "ccode", "year")) %>%
  mutate(cinc_tcc17 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc18" = "ccode", "year")) %>%
  mutate(cinc_tcc18 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc19" = "ccode", "year")) %>%
  mutate(cinc_tcc19 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc20" = "ccode", "year")) %>%
  mutate(cinc_tcc20 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc21" = "ccode", "year")) %>%
  mutate(cinc_tcc21 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc22" = "ccode", "year")) %>%
  mutate(cinc_tcc22 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc23" = "ccode", "year")) %>%
  mutate(cinc_tcc23 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc24" = "ccode", "year")) %>%
  mutate(cinc_tcc24 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc25" = "ccode", "year")) %>%
  mutate(cinc_tcc25 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc26" = "ccode", "year")) %>%
  mutate(cinc_tcc26 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc27" = "ccode", "year")) %>%
  mutate(cinc_tcc27 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc28" = "ccode", "year")) %>%
  mutate(cinc_tcc28 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc29" = "ccode", "year")) %>%
  mutate(cinc_tcc29 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc30" = "ccode", "year")) %>%
  mutate(cinc_tcc30 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc31" = "ccode", "year")) %>%
  mutate(cinc_tcc31 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc32" = "ccode", "year")) %>%
  mutate(cinc_tcc32 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc33" = "ccode", "year")) %>%
  mutate(cinc_tcc33 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc34" = "ccode", "year")) %>%
  mutate(cinc_tcc34 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc35" = "ccode", "year")) %>%
  mutate(cinc_tcc35 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc36" = "ccode", "year")) %>%
  mutate(cinc_tcc36 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc37" = "ccode", "year")) %>%
  mutate(cinc_tcc37 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc38" = "ccode", "year")) %>%
  mutate(cinc_tcc38 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc39" = "ccode", "year")) %>%
  mutate(cinc_tcc39 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc40" = "ccode", "year")) %>%
  mutate(cinc_tcc22 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc41" = "ccode", "year")) %>%
  mutate(cinc_tcc41 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc42" = "ccode", "year")) %>%
  mutate(cinc_tcc42 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc43" = "ccode", "year")) %>%
  mutate(cinc_tcc43 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc44" = "ccode", "year")) %>%
  mutate(cinc_tcc44 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc45" = "ccode", "year")) %>%
  mutate(cinc_tcc45 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc46" = "ccode", "year")) %>%
  mutate(cinc_tcc46 = cinc) %>%
  select(-cinc)

Geo <- Geo %>%
  left_join(x = Geo, y = CINC, by = c("tcc47" = "ccode", "year")) %>%
  mutate(cinc_tcc47 = cinc) %>%
  select(-cinc)

## Weighted average of troop quality ##
Geo <- Geo %>%
  rowwise()%>%
  mutate(quality = weighted.mean(x = across(starts_with("cinc_tcc")), wt = across(starts_with("notroopspertcc")), na.rm = T))

## Select variables #
Geo <- Geo %>%
  select(prioid, month, year, mission, quality)

## Merge on Troop Quality ##
Geo_PKO <- Geo_PKO %>%
  left_join(x = ., y = Geo, by = c("year", "month", "gid" = "prioid"))

Geo_PKO <- Geo_PKO %>%
  distinct() %>%
  mutate_at(c("quality"), ~replace(., is.na(.), 0)) %>%
  mutate(mission = mission.x) %>%
  select(-mission.y, - mission.x)

remove(CINC, Geo, miss)


#~~~~~~~~~~~~~~~~~~~~~~~#
#### Add FC Duration ####
#~~~~~~~~~~~~~~~~~~~~~~~#

# FC overlaps. Make the last FC end on the 31st or 30th of the previous month to avoid double counting.

FC <- read_excel("Leader/UN_Leader.xlsx", sheet = "months")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Keep only FC observations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Remove periods in variable #
FC$`Appointment II` <- gsub(x = FC$`Appointment II`, "\\.", ',')


# Split Appointment 2 to see all jobs #
FC <- FC %>%
  separate(`Appointment II`, c('APP1', 'APP2')) %>%
  mutate(APP1 = as.numeric(APP1),
         APP2 = as.numeric(APP2)) %>%
  rename(mission_region = Region...7,
         FC_region = Region...15)


# Create FC indicator #
FC <- FC %>%
  mutate(FC = ifelse(Appointment == 3, 1, 
                     ifelse(APP1 == 3, 1, 
                            ifelse(APP2 == 3, 1, 0)))) %>%
  mutate(FC = replace_na(FC, 0)) 


# Subset for only FC's #
FC <- subset(FC, FC == 1)


# Remove whitespace #
FC <- FC %>%
  mutate(across(where(is.character), str_trim))


# Mark observations that are right truncated #
FC <- FC %>%
  mutate(right_trunc = ifelse(end_mm_yyyy == "99-9999", 1, 0))

# Make date variables #
FC <- FC %>%
  mutate(end_mm_yyyy = ifelse(end_mm_yyyy == "99-9999", '9-2019', end_mm_yyyy),
         end_mm_yyyy = my(end_mm_yyyy),
         start_mm_yyyy = my(start_mm_yyyy))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Collapse FCs that served back-to-back terms ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Split dataframe into missions #
x <- split(FC, list(FC$Mission))
list2env(x, .GlobalEnv)
remove(x)


# DOMREP #
# No change #
DOMREP <- DOMREP %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number())
# Will be dropped with contribution dataset #
# Drop since the mission never had troops #
remove(DOMREP)


# MINURCA #
MINURCA <- MINURCA %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 12/1998 - 12/1999 #
MINURCA$`Mission end month` <- 12
MINURCA$`Mission end year` <- 1999
MINURCA$`Mandate end month` <- 12
MINURCA$`Mandate end year` <- 1999

MINURCA$end_mm_yyyy <- as.Date('1999-12-01')


# MINURCAT #
# No change #
MINURCAT <- MINURCAT %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# MINURSO #
# No change #
MINURSO <- MINURSO %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Luis Block Urban as acting FC #
MINURSO$acting[MINURSO$Name == "Luis" & MINURSO$`Last name(s)` == "Block Urban"] <- 1

# FC Overlap fixes #
MINURSO$end_mm_yyyy[MINURSO$Name == "Armand" & MINURSO$`Last name(s)` == "Roy"] <- '1992-03-31'
MINURSO$end_mm_yyyy[MINURSO$Name == "André" & MINURSO$`Last name(s)` == "Van Baelen"] <- '1996-02-28'
MINURSO$end_mm_yyyy[MINURSO$Name == "Claude" & MINURSO$`Last name(s)` == "Buze"] <- '2002-07-31'
MINURSO$end_mm_yyyy[MINURSO$Name == "Imam" & MINURSO$`Last name(s)` == "Edy  Mulyono"] <- '2015-08-30'
MINURSO$end_mm_yyyy[MINURSO$Name == "Wang" & MINURSO$`Last name(s)` == "Xiaojun"] <- '2019-01-31'


# MINUSCA #
# No change #
MINUSCA <- MINUSCA %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# MINUSMA #
# No change #
MINUSMA <- MINUSMA %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# MINUSTAH #
MINUSTAH <- MINUSTAH %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

MINUSTAH$`Mandate end month`[MINUSTAH$Name == "Ajax"] <- 9
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Ajax"] <- as.Date('2017-09-01')

# FC Overlap fixes #
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Augusto"] <- '2005-08-30'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Urano Teixeira"] <- '2005-12-30'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Jose"] <- '2006-12-30'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Carlos Alberto"] <- '2009-03-30'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Floriano"] <- '2010-02-28'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Luiz Eduardo"] <- '2012-02-28'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Fernando"] <- '2013-02-28'
MINUSTAH$end_mm_yyyy[MINUSTAH$Name == "Edson"] <- '2014-02-28'


# MONUA #
# No change #
MONUA <- MONUA %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# MONUC #
# No change #
MONUC <- MONUC %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Babacar Gaye ended at 6 2010 #
MONUC$`Mandate end month`[MONUC$Name == "Babacar" & MONUC$`Mandate start month` == 11] <- 6
MONUC$end_mm_yyyy[MONUC$Name == "Babacar" & MONUC$`Mandate start month` == 11] <- as.Date('2010-06-01')

# Fix FC overlaps #
MONUC$end_mm_yyyy[MONUC$Name == "Babacar" & MONUC$`Last name(s)` == "Gaye" & MONUC$`Mandate start year` == "2005"] <- '2008-08-30'



# MONUSCO #
# No change #
MONUSCO <- MONUSCO %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
MONUSCO$end_mm_yyyy[MONUSCO$Name == "Carlos Alberto"] <- '2015-11-30'



# ONUB #
# No change #
ONUB <- ONUB %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# Mission ends at 2006/11 #
ONUB$`Mission end month` <- 11
ONUB$`Mandate end month` <- 11
ONUB$end_mm_yyyy <- as.Date('2006-11-01')


# ONUC #
# No change #
ONUC <- ONUC %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Will be dropped after merge with troops #


# ONUMOZ #
# No change #
ONUMOZ <- ONUMOZ %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# UNAMIC #
UNAMIC <- UNAMIC %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 1991/11 - 1992/01 #
UNAMIC$`Mission end month` <- 1
UNAMIC$`Mandate end month` <- 1
UNAMIC$end_mm_yyyy <- as.Date('1992-01-01')


# UNAMID #
# Malik was a deputy force commander, not force commander #
UNAMID <- UNAMID %>%
  filter(Name != "Fida Hussain" & `Last name(s)` != "Malik") %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
UNAMID$end_mm_yyyy[UNAMID$Name == "Martin Luther"] <- '2009-07-30'
UNAMID$end_mm_yyyy[UNAMID$Name == "Paul"] <- '2015-11-30'
UNAMID$end_mm_yyyy[UNAMID$Name == "Frank"] <- '2017-07-30'


# UNAMIR #
# No change #
UNAMIR <- UNAMIR %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
UNAMIR$end_mm_yyyy[UNAMIR$Name == "Romeo"] <- '1994-07-30'
UNAMIR$end_mm_yyyy[UNAMIR$Name == "Guy"] <- '1995-11-30'


# UNAMISL # 
# Akram Sajjad listed twice #
UNAMSIL <- UNAMSIL %>%
  arrange(start_mm_yyyy, Appointment) %>%
  mutate(row_id = row_number(),
         acting = 0) %>%
  filter(row_id != 4) %>%
  rename(FC_mis_count = row_id)

# Fix FC overlaps #
UNAMSIL$end_mm_yyyy[UNAMSIL$Name == "Daniel"] <- '2003-10-30'
UNAMSIL$start_mm_yyyy[UNAMSIL$Name == "Sajjad"] <- '2003-11-01'


# UNAVEM III #
# No change # 
`UNAVEM III` <- `UNAVEM III` %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# UNCRO #
# No change #
UNCRO <- UNCRO %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 1995/04 - 1995/12 #
UNCRO$`Mission end month` <- 12
UNCRO$`Mission end year` <- 1995
UNCRO$end_mm_yyyy <- as.Date('1995-12-01')


# UNDOF #
UNDOF <- UNDOF %>%
  arrange(start_mm_yyyy, Appointment) %>%
  filter(Appointment != 2) %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
UNDOF$end_mm_yyyy[UNDOF$Name == "Gonzalo"] <- '1974-11-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Erkki"] <- '1982-05-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Gustaw"] <- '1988-08-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Adolf"] <- '1991-08-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Bo"] <- '2003-07-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Franciszek"] <- '2003-12-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Bala Nanda"] <- '2006-12-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Natalio"] <- '2012-07-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Iqbal"] <- '2014-12-30'
UNDOF$end_mm_yyyy[UNDOF$Name == "Purna"] <- '2016-01-30'


# UNEF I #
# No change #
`UNEF I` <- `UNEF I` %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0) 
# Will get dropped #

# UNEF II #
# No change #
`UNEF II` <- `UNEF II` %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Make mandate end year and month match TAMM #
`UNEF II`$`Mandate end month`[`UNEF II`$Name == "Rais" & `UNEF II`$`Last name(s)` == "Abin"] <- 7
`UNEF II`$end_mm_yyyy[`UNEF II`$Name == "Rais" & `UNEF II`$`Last name(s)` == "Abin"] <- as.Date('1979-07-01')
# Will get dropped #


# UNFICYP #
# No change $
UNFICYP <- UNFICYP %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
UNFICYP$end_mm_yyyy[UNFICYP$Name == "P. S."] <- '1964-05-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "K. S."] <- '1965-11-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "A. J."] <- '1966-04-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "A. E."] <- '1969-11-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "Dewan Prem"] <- '1976-11-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "James J."] <- '1981-02-28'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "Gunther"] <- '1988-03-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "Clive"] <- '1992-03-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "Evergisto Arturo"] <- '1999-11-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "Chao"] <- '2014-07-30'
UNFICYP$end_mm_yyyy[UNFICYP$Name == "Kristin"] <- '2016-06-30'



# UNIFIL #
UNIFIL <- UNIFIL %>%
  arrange(start_mm_yyyy, Appointment) %>%
  filter(Appointment != 2) %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Emmanuel"] <- '1981-01-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Gustaw"] <- '1988-06-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Lars-Eric"] <- '1993-01-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Lalit"] <- '2004-01-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Alain"] <- '2007-01-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Paolo"] <- '2014-06-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Luciano"] <- '2016-06-30'
UNIFIL$end_mm_yyyy[UNIFIL$Name == "Michael"] <- '2018-07-30'


# UNIKOM #
# No change #
UNIKOM <- UNIKOM %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 1991/04 - 2003/09#
UNIKOM$`Mission end month` <- 9
UNIKOM$`Mandate end month`[UNIKOM$Name == "Upinder"] <- 9
UNIKOM$end_mm_yyyy[UNIKOM$Name == "Upinder"] <- as.Date('2003-09-01')

# Upinder Singh Klair as acting FC #
UNIKOM$acting[UNIKOM$Name == "Upinder" & UNIKOM$`Last name(s)` == "Singh Klair"] <- 1


# UNISFA #
UNISFA <- UNISFA %>%
  arrange(start_mm_yyyy, Appointment) %>%
  filter(Appointment != 2) %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 2011/08 - today #
UNISFA$`Mission start month` <- 8
UNISFA$start_mm_yyyy[UNISFA$Name == "Tadesse"] <- as.Date('2011-08-01')


# Halefom Moges as acting FC #
UNISFA$acting[UNISFA$Name == "Halefom" & UNISFA$`Last name(s)` == "Moges"] <- 1


# UNMEE #
# No change #
UNMEE <- UNMEE %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 09/2000 - 2008/07 #
UNMEE$`Mission start month` <- 9


# UNMIH #
# No change #
UNMIH <- UNMIH %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Remove Greg Pulley #
UNMIH <- UNMIH %>%
  filter(Name != "Greg" & `Last name(s)` != "Pulley")

# Fix FC overlaps #
UNMIH$end_mm_yyyy[UNMIH$Name == "Joseph"] <- '1996-02-28'


# UNMIL #
UNMIL <- UNMIL %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 2003/09 - 2018/02 #
UNMIL$`Mission end month` <- 2
UNMIL$end_mm_yyyy[UNMIL$Name == "Salihu"] <- as.Date('2018-02-01')

# Fix FC overlaps #
UNMIL$end_mm_yyyy[UNMIL$Name == "Zahirul"] <- '2009-09-30'
UNMIL$end_mm_yyyy[UNMIL$Name == "Sikander"] <- '2010-10-30'
UNMIL$start_mm_yyyy[UNMIL$Name == "Muhammad"] <- '2010-11-01'
UNMIL$end_mm_yyyy[UNMIL$Name == "Muhammad"] <- '2012-10-30'
UNMIL$end_mm_yyyy[UNMIL$Name == "Leonard Muriuki"] <- '2015-01-30'


# UNMIS #
UNMIS <- UNMIS %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# Fix FC overlaps #
UNMIS$end_mm_yyyy[UNMIS$Name == "Fazle"] <- '2005-12-30'
UNMIS$end_mm_yyyy[UNMIS$Name == "Jasbir"] <- '2008-04-30'


# UNMISET #
UNMISET <- UNMISET %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 2002/05 - 2005/04
UNMISET$`Mission end month` <- 4
UNMISET$`Mandate end month`[UNMISET$Name == "Khairuddin"] <- 4
UNMISET$end_mm_yyyy[UNMISET$Name == "Khairuddin"] <- as.Date('2005-04-01')

# Fix FC overlaps #
UNMISET$end_mm_yyyy[UNMISET$Name == "Winai"] <- '2002-07-30'
UNMISET$end_mm_yyyy[UNMISET$Name == "Tan Huck"] <- '2003-07-30'


# UNMISS #
UNMISS <- UNMISS %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 2011/08 - Today #
UNMISS$`Mission start month` <- 8
UNMISS$`Mandate start month`[UNMISS$Name == "Moses"] <- 8
UNMISS$start_mm_yyyy[UNMISS$Name == "Moses"] <- as.Date('2011-08-01')


# Chaoying Yang as acting FC #
UNMISS$acting[UNMISS$Name == "Chaoying" & UNMISS$`Last name(s)` == "Yang"] <- 1


# Fix FC overlaps #
UNMISS$end_mm_yyyy[UNMISS$Name == "Delali"] <- '2014-05-30'
UNMISS$end_mm_yyyy[UNMISS$Name == "Yohannes"] <- '2016-05-30'
UNMISS$end_mm_yyyy[UNMISS$Name == "Frank"] <- '2019-04-30'



# UNMIT #
UNMIT <- UNMIT %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 2006/08 - 2012/11 #
UNMIT$`Mission end month` <- 11
UNMIT$`Mandate end month`[UNMIT$Name == "Martin"] <- 11
UNMIT$end_mm_yyyy[UNMIT$Name == "Martin"] <- as.Date('2012-11-01')


# Fix FC overlaps #
UNMIT$end_mm_yyyy[UNMIT$Name == "Jeremy"] <- '2010-12-30'


# UNOCI #
UNOCI <- UNOCI %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 2004/03 - 2017/03 #
UNOCI$`Mission end month` <- 3
UNOCI$`Mandate end month`[UNOCI$Name == "Didier"] <- 3
UNOCI$end_mm_yyyy[UNOCI$Name == "Didier"] <- as.Date('2017-03-01')


# Fix FC overlaps #
UNOCI$end_mm_yyyy[UNOCI$Name == "Abdul"] <- '2011-02-28'
UNOCI$end_mm_yyyy[UNOCI$Name == "Gnakoudè"] <- '2012-04-30'


# UNOSOM I #
`UNOSOM I` <- `UNOSOM I` %>%
  filter(Appointment == 3) %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)
# Will be dropped due to TAMM not including the mission #


# UNOSOM II #
`UNOSOM II` <- `UNOSOM II` %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 1993/03 - 1995/01 #
`UNOSOM II`$`Mission end month` <- 1
`UNOSOM II`$`Mandate end month`[`UNOSOM II`$Name == "Aboo Samah Bin"] <- 1
`UNOSOM II`$end_mm_yyyy[`UNOSOM II`$Name == "Aboo Samah Bin"] <- as.Date('1995-01-01')


# UNPF #
UNPF <- UNPF %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)
# Will be dropped since not in TAMM #


# UNPREDEP #
UNPREDEP <- UNPREDEP %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# Fix FC overlaps #
UNPREDEP$end_mm_yyyy[UNPREDEP$Name == "Bo"] <- '1997-05-30'
UNPREDEP$end_mm_yyyy[UNPREDEP$Name == "Bent"] <- '1998-08-30'


# UNPROFOR #
UNPROFOR <- UNPROFOR %>%
  filter(duration_months != 0) %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(), 
         acting = 0)

# Mission ends on 3 1995 #
UNPROFOR <- UNPROFOR %>%
  filter(Name != "Rupert" & `Last name(s)` != "Smith") %>%
  filter(Name != "Bernard")


# Lars-Eric Wahlgren as acting FC #
UNPROFOR$acting[UNPROFOR$Name == "Lars-Eric" & UNPROFOR$`Last name(s)` == "Wahlgren"] <- 1


# Fix FC overlaps #
UNPROFOR$end_mm_yyyy[UNPROFOR$Name == "Jean"] <- '1994-02-28'


# UNSF #
UNSF <- UNSF %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)
# Will be dropped since not in contribution dataset #


# UNSMIH #
UNSMIH <- UNSMIH %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 1996/08 - 1997/07 #
UNSMIH$`Mandate start month` <- 8
UNSMIH$`Mission start month` <- 8
UNSMIH$start_mm_yyyy <- as.Date('1996-08-01')


# UNTAC #
UNTAC <- UNTAC %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# UNTAES #
UNTAES <- UNTAES %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)


# Fix FC overlaps #
UNTAES$end_mm_yyyy[UNTAES$Name == "Jozef"] <- '1996-12-30'


# UNTAET #
UNTAET <- UNTAET %>%
  filter(Appointment != 7) %>%
  # Remove Rezaqul Haider. They were not a force commander #
  filter(Name != "Rezaqul") %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)

# 1999/10 - 2002/04 #
UNTAET$`Mission end month` <- 4
UNTAET$`Mandate end month`[UNTAET$Name == "Winai"] <- 4
UNTAET$end_mm_yyyy[UNTAET$Name == "Winai"] <- as.Date('2002-04-01')


# UNTAG #
UNTAG <- UNTAG %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0) %>%
  # Remove earlier FC that TAMM doesn't capture #
  filter(`Mandate end month` != 1)
# Will be dropped due to contributions dataset #


# UNTMIH #
UNTMIH <- UNTMIH %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number(),
         acting = 0)
# Mission was 3 months #
remove(UNTMIH)

# 1997/09 - 1997/11 #
#UNTMIH$`Mission start month` <- 9
#UNTMIH$start_mm_yyyy <- as.Date('1997-09-01')

# UNYOM #
UNYOM <- UNYOM %>%
  arrange(start_mm_yyyy) %>%
  mutate(FC_mis_count = row_number())
# Will be dropped due to no contribution data #


FC_build <- bind_rows(MINURCA, MINURCAT, MINURSO, MINUSCA, MINUSMA,
                      MINUSTAH, MONUA, MONUC, MONUSCO, ONUB, ONUC, ONUMOZ, 
                      UNAMIC, UNAMID, UNAMIR, UNAMSIL, `UNAVEM III`, UNCRO, 
                      UNDOF, `UNEF I`, `UNEF II`, UNFICYP, UNIFIL, UNIKOM,
                      UNISFA, UNMEE, UNMIH, UNMIL, UNMIS, UNMISET, UNMISS,
                      UNMIT, UNOCI, `UNOSOM I`, `UNOSOM II`, UNPF, UNPREDEP,
                      UNPROFOR, UNSF, UNSMIH, UNTAC, UNTAES, UNTAET, UNTAG,
                      UNYOM)

remove(MINURCA, MINURCAT, MINURSO, MINUSCA, MINUSMA, MINUSTAH, MONUA,
       MONUC, MONUSCO, ONUB, ONUC, ONUMOZ, UNAMIC, UNAMID, UNAMIR, UNAMSIL,
       `UNAVEM III`, UNCRO, UNDOF, `UNEF I`, `UNEF II`, UNFICYP, UNIFIL,
       UNIKOM, UNISFA, UNMEE, UNMIH, UNMIL, UNMIS, UNMISET, UNMISS, UNMIT,
       UNOCI, `UNOSOM I`, `UNOSOM II`, UNPF, UNPREDEP, UNPROFOR, UNSF,
       UNSMIH, UNTAC, UNTAES, UNTAET, UNTAG, UNYOM)


# Remove acting FCs #
FC <- subset(FC_build, acting == 0)
remove(FC_build)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### Expand to monthly data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Get indicator of whether the FC previously served #
FC <- FC %>%
  group_by(Name, `Last name(s)`) %>%
  mutate(FC_count = length(unique(as.character(end_mm_yyyy)))) %>%
  ungroup() %>%
  select(Name, `Last name(s)`, Mission, FC_count, end_mm_yyyy, Nationality, Country, Rank, start_mm_yyyy, FC,
         Country, mission_region, Nationality, FC_region, `Mission end year`, `Mission end month`, duration_months,
         right_trunc, FC_mis_count, `Mandate end year`, `Mandate end month`, `Mission start month`, `Mission start year`) %>%
  arrange(end_mm_yyyy) %>%
  group_by(Name, `Last name(s)`) %>%
  mutate(id = row_number(),
         FC_prev = id - 1) %>%
  ungroup() %>%
  select(-c(FC_count, id))


# Expand observations into monthly data #
FC <- data.table::setDT(FC)[ , list(mission = Mission, last_name = `Last name(s)`,
                        name = Name, end_date = end_mm_yyyy, start_date = start_mm_yyyy, FC_home = Nationality, FC_prev = FC_prev,
                        rank = Rank, mission_host = Country, mission_region = mission_region, FC_region = FC_region,
                        mission_end_month = `Mission end month`, mission_end_year = `Mission end year`, mission_start_month = `Mission start month`,
                        mission_start_year = `Mission start year`,  max_dur = duration_months, 
                        right_trunc = right_trunc, FC_mis_count = FC_mis_count, mandate_end_month = `Mandate end month`, 
                        mandate_end_year = `Mandate end year`, date = seq(start_mm_yyyy, end_mm_yyyy, by = "month")), by = 1:length(FC)]


# Get duration #
FC <- FC %>%
  group_by(mission, name, last_name, length) %>%
  mutate(FC_dur = row_number() - 1) %>%
  select(-length) %>%
  ungroup()


# Get month and year #
FC <- FC %>%
  mutate(month = month(date),
         year = year(date))


# Keep only needed variables #
FC <- FC %>%
  select(mission, month, year, FC_dur)


# Merge #
Geo_PKO <- Geo_PKO %>%
  left_join(x = ., y = FC, by = c("year", "month", "mission"))


# Fill duration with 0's. Assume no FC or in between #
Geo_PKO <- Geo_PKO %>%
  mutate(FC_dur = ifelse(is.na(FC_dur), 0, FC_dur))
remove(FC)


# Fix name of variable #
Geo_PKO <- Geo_PKO %>%
  mutate(duration = FC_dur) %>%
  select(-FC_dur)


# Save for Map #
saveRDS(Geo_PKO, file = "Geo_PKO.rds")


# Drop coordinates #
Geo_PKO <- Geo_PKO %>%
  select(-c(xcoord, ycoord))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### Write data to STATA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

write.dta(Geo_PKO, "STATA_Prep/Geo_PKO_nr.dta")

