##############################################################################################################
#######################################  Beta  ###########################################################
##############################################################################################################
# Step 1) Load in the storm files
# Step 2) normalize discharge and concentration for each solute
# Step 3) calculate Beta for each solute for each site
# Step 4) plot against HI 

# Load Libraries
library(here)
library(tidyverse)
library(boot)
library(broom)
library(purrr)
library(viridis)
library(readr)
library(lubridate)
library(data.table)
library(rio)
library(ggplot2)
library(scales)
library(psych)
library(googledrive)
library(readxl)
library(cowplot)
library(zoo)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(ggpmisc)
library(SLOPE)
library(wesanderson)
library(ggpubr)
library(dataRetrieval)

########################################## 2018 ##########################################################
storm_file_list_beta <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2018/FRCH_MOOS_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("~/Documents/Storms_clean_repo/Storm_Events/2018/FRCH_MOOS_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 


#  organize storm data by site and solute # 5 for each storm 
CARI_storm_list_beta = storm_list_beta[c(1:80)] #80
FRCH_storm_list_beta = storm_list_beta[c(81:155)] #75
MOOS_storm_list_beta = storm_list_beta[c(156:235)] #80

CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("Turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("Turb", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("Turb", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

# normalize Q data 
# FRCH
for(i in 1:length(FRCH_Q_storm_list_beta)){
  FRCH_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_Q_storm_list_beta[[i]][["datavalue"]]-min(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# MOOS
for(i in 1:length(MOOS_Q_storm_list_beta)){
  MOOS_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_Q_storm_list_beta[[i]][["datavalue"]]-min(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# CARI
for(i in 1:length(CARI_Q_storm_list_beta)){
  CARI_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_Q_storm_list_beta[[i]][["datavalue"]]-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# normalize solute data 
#
#NO3
for(i in 1:length(FRCH_NO3_storm_list_beta)){
  FRCH_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list_beta[[i]][["datavalue"]]-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list_beta)){
  MOOS_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list_beta[[i]][["datavalue"]]-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_NO3_storm_list_beta)){
  CARI_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_NO3_storm_list_beta[[i]][["datavalue"]]-min(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#fDOM
for(i in 1:length(FRCH_fDOM_storm_list_beta)){
  FRCH_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_fDOM_storm_list_beta[[i]][["datavalue"]]-min(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_fDOM_storm_list_beta)){
  MOOS_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_fDOM_storm_list_beta[[i]][["datavalue"]]-min(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_fDOM_storm_list_beta)){
  CARI_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_fDOM_storm_list_beta[[i]][["datavalue"]]-min(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#SPC
for(i in 1:length(FRCH_SpCond_storm_list_beta)){
  FRCH_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_SpCond_storm_list_beta[[i]][["datavalue"]]-min(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_SpCond_storm_list_beta)){
  MOOS_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_SpCond_storm_list_beta[[i]][["datavalue"]]-min(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_SpCond_storm_list_beta)){
  CARI_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_SpCond_storm_list_beta[[i]][["datavalue"]]-min(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#Turb
for(i in 1:length(FRCH_turb_storm_list_beta)){
  FRCH_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_turb_storm_list_beta[[i]][["datavalue"]]-min(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_turb_storm_list_beta)){
  MOOS_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_turb_storm_list_beta[[i]][["datavalue"]]-min(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}
###### NO3  #######
FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 285),
                            rep("storm10", 1379),
                            rep("storm11a", 155),
                            rep("storm11b", 498),
                            rep("storm2a", 500),
                            rep("storm2b", 344),
                            rep("storm3", 393),
                            rep("storm4a", 177),
                            rep("storm4b", 734),
                            rep("storm5", 662),
                            rep("storm6", 605),
                            rep("storm7", 213),
                            rep("storm8a", 159),
                            rep("storm8b", 191),
                            rep("storm9", 196))

names(FRCH_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
FRCH_NO3_storm$site.ID <- "FRCH"

cols <- c("NO3.norm","Q.norm")
FRCH_NO3_storm[cols] <- log(FRCH_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_NO3_storm <- FRCH_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_NO3_storm_ascending <- filter(FRCH_NO3_storm, limb == "ascending")

FRCH_NO3_storm_ascending <- FRCH_NO3_storm_ascending[is.finite(FRCH_NO3_storm_ascending$Q.norm) & is.finite(FRCH_NO3_storm_ascending$NO3.norm), ]

beta.all.no3 <- FRCH_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(NO3.norm, Q.norm)) # this works just like the beta one that is for an individual site

frch.2018.ci <- FRCH_NO3_storm_ascending %>%
  group_by(storm.ID) %>%
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 116),
                            rep("storm10", 642),
                            rep("storm11a", 179),
                            rep("storm11b", 71),
                            rep("storm12", 584),
                            rep("storm2a", 148),
                            rep("storm2b", 291),
                            rep("storm2c", 363),
                            rep("storm3", 459),
                            rep("storm4", 487),
                            rep("storm5", 563),
                            rep("storm6", 663),
                            rep("storm7", 255),
                            rep("storm8a", 155),
                            rep("storm8b", 195),
                            rep("storm9", 211))

names(MOOS_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
MOOS_NO3_storm$site.ID <- "MOOS"

MOOS_NO3_storm[cols] <- log(MOOS_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_NO3_storm <- MOOS_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_NO3_storm_ascending <- filter(MOOS_NO3_storm, limb == "ascending")

MOOS_NO3_storm_ascending <- MOOS_NO3_storm_ascending[is.finite(MOOS_NO3_storm_ascending$Q.norm) & is.finite(MOOS_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- MOOS_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

moos.2018.ci <- MOOS_NO3_storm_ascending %>%
  group_by(storm.ID) %>%
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm10", 248),
                            rep("storm11", 191),
                            rep("storm12a", 418),
                            rep("storm12b", 517),
                            rep("storm2", 181),
                            rep("storm3", 24),
                            rep("storm5a", 77),
                            rep("storm5b", 121),
                            rep("storm5c", 575),
                            rep("storm6", 644),
                            rep("storm7", 167),
                            rep("storm8", 191),
                            rep("storm9", 359))

names(CARI_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
CARI_NO3_storm$site.ID <- "CARI"

CARI_NO3_storm[cols] <- log(CARI_NO3_storm[cols]) # making concentrations and Q log transformed

CARI_NO3_storm <- CARI_NO3_storm[is.finite(CARI_NO3_storm$Q.norm),] # removing +- inf
CARI_NO3_storm <- CARI_NO3_storm[is.finite(CARI_NO3_storm$NO3.norm),]# removing +- inf

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

CARI_NO3_storm <- CARI_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

CARI_NO3_storm_ascending <- filter(CARI_NO3_storm, limb == "ascending")

CARI_NO3_storm_ascending <- CARI_NO3_storm_ascending[is.finite(CARI_NO3_storm_ascending$Q.norm) & is.finite(CARI_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.cari <- CARI_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       CARI_NO3_storm_ascending)

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

cari.2018.ci <- CARI_NO3_storm_ascending %>%
  group_by(storm.ID) %>%
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2018.ci.no3 <- All_NO3_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2018.ci.no3$response_var <- "NO3"


##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
CARI_fDOM_storm <- map2_df(CARI_Q_storm_list_beta, CARI_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 285),
                            rep("storm10", 1379),
                            rep("storm11a", 155),
                            rep("storm11b", 498),
                            rep("storm2a", 500),
                            rep("storm2b", 344),
                            rep("storm3", 393),
                            rep("storm4a", 177),
                            rep("storm4b", 734),
                            rep("storm5", 662),
                            rep("storm6", 605),
                            rep("storm7", 213),
                            rep("storm8a", 159),
                            rep("storm8b", 191),
                            rep("storm9", 196))

names(FRCH_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
FRCH_fDOM_storm$site.ID <- "FRCH"

cols <- c("fDOM.norm","Q.norm")
FRCH_fDOM_storm[cols] <- log(FRCH_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_fDOM_storm <- FRCH_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_fDOM_storm_ascending <- filter(FRCH_fDOM_storm, limb == "ascending")

FRCH_fDOM_storm_ascending <- FRCH_fDOM_storm_ascending[is.finite(FRCH_fDOM_storm_ascending$Q.norm) & is.finite(FRCH_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM <- FRCH_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 116),
                            rep("storm10", 642),
                            rep("storm11a", 179),
                            rep("storm11b", 71),
                            rep("storm12", 584),
                            rep("storm2a", 148),
                            rep("storm2b", 291),
                            rep("storm2c", 363),
                            rep("storm3", 459),
                            rep("storm4", 487),
                            rep("storm5", 563),
                            rep("storm6", 663),
                            rep("storm7", 255),
                            rep("storm8a", 155),
                            rep("storm8b", 195),
                            rep("storm9", 211))

names(MOOS_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
MOOS_fDOM_storm$site.ID <- "MOOS"

MOOS_fDOM_storm[cols] <- log(MOOS_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_fDOM_storm <- MOOS_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_fDOM_storm_ascending <- filter(MOOS_fDOM_storm, limb == "ascending")

MOOS_fDOM_storm_ascending <- MOOS_fDOM_storm_ascending[is.finite(MOOS_fDOM_storm_ascending$Q.norm) & is.finite(MOOS_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.moos.with.all <- MOOS_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_fDOM_storm$storm.ID = c(rep("storm10", 248),
                             rep("storm11", 191),
                             rep("storm12a", 418),
                             rep("storm12b", 517),
                             rep("storm2", 181),
                             rep("storm3", 24),
                             rep("storm5a", 77),
                             rep("storm5b", 121),
                             rep("storm5c", 575),
                             rep("storm6", 644),
                             rep("storm7", 167),
                             rep("storm8", 191),
                             rep("storm9", 359))

names(CARI_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
CARI_fDOM_storm$site.ID <- "CARI"

CARI_fDOM_storm[cols] <- log(CARI_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
CARI_fDOM_storm <- CARI_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

CARI_fDOM_storm_ascending <- filter(CARI_fDOM_storm, limb == "ascending")

CARI_fDOM_storm_ascending <- CARI_fDOM_storm_ascending[is.finite(CARI_fDOM_storm_ascending$Q.norm) & is.finite(CARI_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fdom.cari <- CARI_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                        CARI_fDOM_storm_ascending)

beta.all.fDOM <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fDOM$response_var <- "fDOM"

all.2018.ci.fDOM <- All_fDOM_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(fDOM.norm ~ Q.norm, data = .x)))

all.2018.ci.fDOM$response_var <- "fDOM"


##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
CARI_SPC_storm <- map2_df(CARI_Q_storm_list_beta, CARI_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 285),
                            rep("storm10", 1379),
                            rep("storm11a", 155),
                            rep("storm11b", 498),
                            rep("storm2a", 500),
                            rep("storm2b", 344),
                            rep("storm3", 393),
                            rep("storm4a", 177),
                            rep("storm4b", 734),
                            rep("storm5", 662),
                            rep("storm6", 605),
                            rep("storm7", 213),
                            rep("storm8a", 159),
                            rep("storm8b", 191),
                            rep("storm9", 196))

names(FRCH_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
FRCH_SPC_storm$site.ID <- "FRCH"

cols <- c("SPC.norm","Q.norm")
FRCH_SPC_storm[cols] <- log(FRCH_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_SPC_storm <- FRCH_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_SPC_storm_ascending <- filter(FRCH_SPC_storm, limb == "ascending")

FRCH_SPC_storm_ascending <- FRCH_SPC_storm_ascending[is.finite(FRCH_SPC_storm_ascending$Q.norm) & is.finite(FRCH_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC <- FRCH_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_SPC_storm$storm.ID = c(rep("storm1", 116),
                            rep("storm10", 642),
                            rep("storm11a", 179),
                            rep("storm11b", 71),
                            rep("storm12", 584),
                            rep("storm2a", 148),
                            rep("storm2b", 291),
                            rep("storm2c", 363),
                            rep("storm3", 459),
                            rep("storm4", 487),
                            rep("storm5", 563),
                            rep("storm6", 663),
                            rep("storm7", 255),
                            rep("storm8a", 155),
                            rep("storm8b", 195),
                            rep("storm9", 211))

names(MOOS_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
MOOS_SPC_storm$site.ID <- "MOOS"

MOOS_SPC_storm[cols] <- log(MOOS_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_SPC_storm <- MOOS_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_SPC_storm_ascending <- filter(MOOS_SPC_storm, limb == "ascending")

MOOS_SPC_storm_ascending <- MOOS_SPC_storm_ascending[is.finite(MOOS_SPC_storm_ascending$Q.norm) & is.finite(MOOS_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.moos.with.all <- MOOS_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_SPC_storm$storm.ID = c(rep("storm10", 248),
                            rep("storm11", 191),
                            rep("storm12a", 418),
                            rep("storm12b", 517),
                            rep("storm2", 181),
                            rep("storm3", 24),
                            rep("storm5a", 77),
                            rep("storm5b", 121),
                            rep("storm5c", 575),
                            rep("storm6", 644),
                            rep("storm7", 167),
                            rep("storm8", 191),
                            rep("storm9", 359))

names(CARI_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
CARI_SPC_storm$site.ID <- "CARI"

CARI_SPC_storm[cols] <- log(CARI_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
CARI_SPC_storm <- CARI_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

CARI_SPC_storm_ascending <- filter(CARI_SPC_storm, limb == "ascending")

CARI_SPC_storm_ascending <- CARI_SPC_storm_ascending[is.finite(CARI_SPC_storm_ascending$Q.norm) & is.finite(CARI_SPC_storm_ascending$SPC.norm), ]

beta.all.spc.cari <- CARI_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending, 
                       CARI_SPC_storm_ascending)

beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

all.2018.ci.SPC <- All_SPC_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(SPC.norm ~ Q.norm, data = .x)))

all.2018.ci.SPC$response_var <- "SPC"


##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
CARI_turb_storm <- map2_df(CARI_Q_storm_list_beta, CARI_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 285),
                            rep("storm10", 1379),
                            rep("storm11a", 155),
                            rep("storm11b", 498),
                            rep("storm2a", 500),
                            rep("storm2b", 344),
                            rep("storm3", 393),
                            rep("storm4a", 177),
                            rep("storm4b", 734),
                            rep("storm5", 662),
                            rep("storm6", 605),
                            rep("storm7", 213),
                            rep("storm8a", 159),
                            rep("storm8b", 191),
                            rep("storm9", 196))

names(FRCH_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
FRCH_turb_storm$site.ID <- "FRCH"

cols <- c("turb.norm","Q.norm")
FRCH_turb_storm[cols] <- log(FRCH_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_turb_storm <- FRCH_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_turb_storm_ascending <- filter(FRCH_turb_storm, limb == "ascending")

FRCH_turb_storm_ascending <- FRCH_turb_storm_ascending[is.finite(FRCH_turb_storm_ascending$Q.norm) & is.finite(FRCH_turb_storm_ascending$turb.norm), ]

beta.all.turb <- FRCH_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_turb_storm$storm.ID = c(rep("storm1", 116),
                            rep("storm10", 642),
                            rep("storm11a", 179),
                            rep("storm11b", 71),
                            rep("storm12", 584),
                            rep("storm2a", 148),
                            rep("storm2b", 291),
                            rep("storm2c", 363),
                            rep("storm3", 459),
                            rep("storm4", 487),
                            rep("storm5", 563),
                            rep("storm6", 663),
                            rep("storm7", 255),
                            rep("storm8a", 155),
                            rep("storm8b", 195),
                            rep("storm9", 211))

names(MOOS_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
MOOS_turb_storm$site.ID <- "MOOS"

MOOS_turb_storm[cols] <- log(MOOS_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_turb_storm <- MOOS_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_turb_storm_ascending <- filter(MOOS_turb_storm, limb == "ascending")

MOOS_turb_storm_ascending <- MOOS_turb_storm_ascending[is.finite(MOOS_turb_storm_ascending$Q.norm) & is.finite(MOOS_turb_storm_ascending$turb.norm), ]

beta.all.turb.moos.with.all <- MOOS_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_turb_storm$storm.ID = c(rep("storm10", 248),
                             rep("storm11", 191),
                             rep("storm12a", 418),
                             rep("storm12b", 517),
                             rep("storm2", 181),
                             rep("storm3", 24),
                             rep("storm5a", 77),
                             rep("storm5b", 121),
                             rep("storm5c", 575),
                             rep("storm6", 644),
                             rep("storm7", 167),
                             rep("storm8", 191),
                             rep("storm9", 359))

names(CARI_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
CARI_turb_storm$site.ID <- "CARI"

CARI_turb_storm[cols] <- log(CARI_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
CARI_turb_storm <- CARI_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

CARI_turb_storm_ascending <- filter(CARI_turb_storm, limb == "ascending")

CARI_turb_storm_ascending <- CARI_turb_storm_ascending[is.finite(CARI_turb_storm_ascending$Q.norm) & is.finite(CARI_turb_storm_ascending$turb.norm), ]

beta.all.turb.cari <- CARI_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

All_turb_storm <- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending, 
                        CARI_turb_storm_ascending)

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

all.2018.ci.turb <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2018.ci.turb$response_var <- "turb"


beta.all.2018 <- rbind(all.2018.ci.no3, all.2018.ci.fDOM,
                       all.2018.ci.SPC, all.2018.ci.turb)
write.csv(beta.all.2018, "~/Documents/Storms_clean_repo/Output_from_analysis/06_BETA/beta.2018.csv")

beta.all.2018 <- beta.all.2018 %>% 
  filter(Parameter != "(Intercept)")


#### plot ####
# HI_2018 <- read_csv("~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2018/HI.2018.csv")
# names(HI_2018)[names(HI_2018) == 'storm.num'] <- 'storm.ID'
# names(HI_2018)[names(HI_2018) == 'response'] <- 'response_var'
# 
# HI_beta = left_join(HI_2018, beta.all.2018, by=c("site.ID", "storm.ID", "response_var"))
# 
# # HI_FI <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results.2018.csv")
# # 
# # HI_FI_beta = left_join(HI_FI, beta.all.2018, by=c("site.ID", "storm.ID", "response_var"))
# # 
# # write.csv(HI_FI_beta, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI_beta.diff_results_2018.csv")
# 
# # NO3 #
# #HI_FI_NO3 = subset(HI_FI_beta, response_var == "NO3")
# #HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))
# 
# #HI_FI_NO3.p = 
# # ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
# #  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
# #  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
# #  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
# ##  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
# #  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
# #  ggtitle("a) NO3-")+ 
# #  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# #HI_FI_NO3.p
# 
# # NO3 #
# HI_beta_NO3 = subset(HI_beta, response_var == "NO3")
# HI_beta_NO3$site.ID <- factor(HI_beta_NO3$site.ID, levels = c('FRCH','MOOS', 'CARI'))
# 
# HI_beta_NO3.p = 
#   ggplot(HI_beta_NO3, aes(Coefficient, HI)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("a) NO3-")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
# HI_beta_NO3.p
# 
# # fDOM #
# HI_FI_fDOM = subset(HI_FI_beta, response_var == "fDOM")
# HI_FI_fDOM$site.ID <- factor(HI_FI_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))
# 
# HI_FI_fDOM.p = 
#   ggplot(HI_FI_fDOM, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("b) fDOM")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_fDOM.p
# 
# HI_beta_fDOM = subset(HI_beta, response_var == "fDOM")
# HI_beta_fDOM$site.ID <- factor(HI_beta_fDOM$site.ID, levels = c('FRCH','MOOS','CARI'))
# 
# HI_beta_fDOM.p = 
#   ggplot(HI_beta_fDOM, aes(beta, HI)) + geom_point(aes(colour=factor(site.ID), alpha = doy, shape = burn), size = 4)+
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("b) fDOM")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
# HI_beta_fDOM.p
# 
# # SPC#
# HI_FI_SPC = subset(HI_FI_beta, response_var == "SPC")
# HI_FI_SPC$site.ID <- factor(HI_FI_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))
# 
# HI_FI_SPC.p = 
#   ggplot(HI_FI_SPC, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("c) SPC")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_SPC.p
# 
# 
# HI_beta_SPC = subset(HI_beta, response_var == "SPC")
# HI_beta_SPC$site.ID <- factor(HI_beta_SPC$site.ID, levels = c('FRCH','MOOS','CARI'))
# 
# HI_beta_SPC.p = 
#   ggplot(HI_beta_SPC, aes(beta, HI)) + geom_point(aes(colour=factor(site.ID), alpha = doy, shape = burn), size = 4)+
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("c) SPC")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
# HI_beta_SPC.p
# 
# # turb #
# HI_FI_turb = subset(HI_FI_beta, response_var == "turb")
# HI_FI_turb$site.ID <- factor(HI_FI_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL'))
# 
# HI_FI_turb.p = 
#   ggplot(HI_FI_turb, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("d) Turb")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_turb.p
# 
# 
# HI_beta_turb = subset(HI_beta, response_var == "turb")
# HI_beta_turb$site.ID <- factor(HI_beta_turb$site.ID, levels = c('FRCH','MOOS','CARI'))
# 
# HI_beta_turb.p = 
#   ggplot(HI_beta_turb, aes(beta, HI)) + geom_point(aes(colour=factor(site.ID), alpha = doy, shape = burn), size = 4)+
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("d) turb")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20), legend.title = element_blank()) 
# HI_beta_turb.p
# 
# grid.arrange(HI_beta_NO3.p, HI_beta_fDOM.p, HI_beta_SPC.p, HI_beta_turb.p)
# 
# # #### Regression between beta and FI #######
# # FI_beta_comp = 
# #   ggplot(HI_FI_NO3, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
# #   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
# #   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
# #   stat_poly_eq(formula = y~x,
# #                label.y = "top", label.x = "right",
# #                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
# #                parse = TRUE)
# # FI_beta_comp
# # 
# # FI_beta_comp_fDOM = 
# #   ggplot(HI_FI_fDOM, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
# #   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
# #   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
# #   stat_poly_eq(formula = y~x,
# #                label.y = "top", label.x = "right",
# #                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
# #                parse = TRUE)
# # FI_beta_comp_fDOM
# # 
# # FI_beta_comp_SPC = 
# #   ggplot(HI_FI_SPC, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
# #   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
# #   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
# #   stat_poly_eq(formula = y~x,
# #                label.y = "top", label.x = "right",
# #                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
# #                parse = TRUE)
# # FI_beta_comp_SPC
# # 
# # FI_beta_comp_turb = 
# #   ggplot(HI_FI_turb, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
# #   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
# #   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
# #   stat_poly_eq(formula = y~x,
# #                label.y = "top", label.x = "right",
# #                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
# #                parse = TRUE)
# # FI_beta_comp_turb
# # 
# # 
# # grid.arrange(FI_beta_comp_NO3, FI_beta_comp_fDOM, FI_beta_comp_SPC, FI_beta_comp_turb)
# # 
