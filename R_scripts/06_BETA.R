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

########################################### 2015 ##########################################

storm_file_list_beta <- list.files(path="FRCH_MOOS/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)


storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("FRCH_MOOS//", storm_file_list_beta, replacement = "")

storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 

#  organize storm data by site and solute 
FRCH_storm_list_beta = storm_list_beta[c(1:42)] #42
MOOS_storm_list_beta = storm_list_beta[c(43:78)] #36

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("turb", names(FRCH_storm_list_beta)))]
FRCH_abs_storm_list_beta = FRCH_storm_list_beta[c(grep("abs", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("turb", names(MOOS_storm_list_beta)))]
MOOS_abs_storm_list_beta = MOOS_storm_list_beta[c(grep("abs", names(MOOS_storm_list_beta)))]
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

#abs
for(i in 1:length(FRCH_abs_storm_list_beta)){
  FRCH_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_abs_storm_list_beta[[i]][["datavalue"]]-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_abs_storm_list_beta)){
  MOOS_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_abs_storm_list_beta[[i]][["datavalue"]]-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

###### NO3  #######

FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 287),
                            rep("storm2", 331),
                            rep("storm3", 383),
                            rep("storm4", 299),
                            rep("storm5a", 453),
                    
                            rep("storm6a", 1295),
                            
                            rep("storm7", 242)) 


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
  dplyr::summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 383),
                            rep("storm2", 575),
                            rep("storm3a", 611),
                            
                            rep("storm4", 191),
                            rep("storm5", 455),
                            rep("storm6", 178))

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


# ALL # 

FRCH_NO3_storm_ascending$DateTime <- as.POSIXct(FRCH_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_NO3_storm_ascending$DateTime <- as.POSIXct(MOOS_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending)
                       

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

all.2015.ci.no3 <- All_NO3_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2015.ci.no3$response_var <- "NO3"


##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 287),
                             rep("storm2", 331),
                             rep("storm3", 383),
                             rep("storm4", 299),
                             rep("storm5a", 453),
                             
                             rep("storm6a", 1295),
                             
                             rep("storm7", 242))

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
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 383),
                             rep("storm2", 575),
                             rep("storm3a", 611),
                             
                             rep("storm4", 191),
                             rep("storm5", 455),
                             rep("storm6", 178))

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

# ALL # 
FRCH_fDOM_storm_ascending$DateTime <- as.POSIXct(FRCH_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_fDOM_storm_ascending$DateTime <- as.POSIXct(MOOS_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending)
                        

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

all.2015.ci.fDOM <- All_fDOM_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(fDOM.norm ~ Q.norm, data = .x)))

all.2015.ci.fDOM$response_var <- "fDOM"


##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 287),
                            rep("storm2", 331),
                            rep("storm3", 383),
                            rep("storm4", 299),
                            rep("storm5a", 453),
                            
                            rep("storm6a", 1295),
                            
                            rep("storm7", 242))

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
MOOS_SPC_storm$storm.ID = c(rep("storm1", 383),
                            rep("storm2", 575),
                            rep("storm3a", 611),
                            
                            rep("storm4", 191),
                            rep("storm5", 455),
                            rep("storm6", 178))

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


# ALL # 
FRCH_SPC_storm_ascending$DateTime <- as.POSIXct(FRCH_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_SPC_storm_ascending$DateTime <- as.POSIXct(MOOS_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")


All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending)
                       


beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

all.2015.ci.SPC <- All_SPC_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(SPC.norm ~ Q.norm, data = .x)))

all.2015.ci.SPC$response_var <- "SPC"


##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 287),
                             rep("storm2", 331),
                             rep("storm3", 383),
                             rep("storm4", 299),
                             rep("storm5a", 453),
                             
                             rep("storm6a", 1295),
                             
                             rep("storm7", 242))

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
MOOS_turb_storm$storm.ID = c(rep("storm1", 383),
                             rep("storm2", 575),
                             rep("storm3a", 611),
                             
                             rep("storm4", 191),
                             rep("storm5", 455),
                             rep("storm6", 178))

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


# ALL # 
FRCH_turb_storm_ascending$DateTime <- as.POSIXct(FRCH_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_turb_storm_ascending$DateTime <- as.POSIXct(MOOS_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm<- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending)
                       

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

all.2015.ci.turb <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2015.ci.turb$response_var <- "turb"


##### ABS #####
FRCH_abs_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_abs_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_abs_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_abs_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_abs_storm$storm.ID = c(rep("storm1", 287),
                            rep("storm2", 331),
                            rep("storm3", 383),
                            rep("storm4", 299),
                            rep("storm5a", 453),
                            
                            rep("storm6a", 1295),
                            
                            rep("storm7", 242))

names(FRCH_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
FRCH_abs_storm$site.ID <- "FRCH"

cols <- c("abs.norm","Q.norm")
FRCH_abs_storm[cols] <- log(FRCH_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_abs_storm <- FRCH_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_abs_storm_ascending <- filter(FRCH_abs_storm, limb == "ascending")

FRCH_abs_storm_ascending <- FRCH_abs_storm_ascending[is.finite(FRCH_abs_storm_ascending$Q.norm) & is.finite(FRCH_abs_storm_ascending$abs.norm), ]

beta.all.abs <- FRCH_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_abs_storm$storm.ID = c(rep("storm1", 383),
                            rep("storm2", 575),
                            rep("storm3a", 611),
                            
                            rep("storm4", 191),
                            rep("storm5", 455),
                            rep("storm6", 178))

names(MOOS_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
MOOS_abs_storm$site.ID <- "MOOS"

MOOS_abs_storm[cols] <- log(MOOS_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_abs_storm <- MOOS_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_abs_storm_ascending <- filter(MOOS_abs_storm, limb == "ascending")

MOOS_abs_storm_ascending <- MOOS_abs_storm_ascending[is.finite(MOOS_abs_storm_ascending$Q.norm) & is.finite(MOOS_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- MOOS_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


# ALL # 
FRCH_abs_storm_ascending$DateTime <- as.POSIXct(FRCH_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_abs_storm_ascending$DateTime <- as.POSIXct(MOOS_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_abs_storm<- rbind(FRCH_abs_storm_ascending, MOOS_abs_storm_ascending)


beta.all.abs <- All_abs_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


beta.all.abs$response_var <- "abs"

all.2015.ci.abs <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2015.ci.abs$response_var <- "abs"


beta.all.2015 <- rbind(all.2015.ci.no3, all.2015.ci.fDOM,
                       all.2015.ci.SPC, all.2015.ci.turb,
                       all.2015.ci.abs)

beta.all.2015 <- beta.all.2015 %>%
  dplyr::mutate(across(c(Coefficient), 
                ~ifelse(Coefficient > 20, NA, .))) # remove outliers 

write.csv(beta.all.2015, here("Output_from_analysis", "06_BETA", "beta.2015.csv"))

beta.all.2015 <- beta.all.2015 %>% 
  filter(Parameter != "(Intercept)")

########################################## 2018 ##########################################################
setwd("Storm_Events/2018")
storm_file_list_beta <- list.files(path="FRCH_MOOS_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("FRCH_MOOS_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 


#  organize storm data by site and solute # 5 for each storm 
CARI_storm_list_beta = storm_list_beta[c(1:65)] #65
FRCH_storm_list_beta = storm_list_beta[c(66:137)] #72
MOOS_storm_list_beta = storm_list_beta[c(138:191)] #54

CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("Turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("Turb", names(FRCH_storm_list_beta)))]
FRCH_abs_storm_list_beta = FRCH_storm_list_beta[c(grep("abs", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("Turb", names(MOOS_storm_list_beta)))]
MOOS_abs_storm_list_beta = MOOS_storm_list_beta[c(grep("abs", names(MOOS_storm_list_beta)))]
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

#abs
for(i in 1:length(FRCH_abs_storm_list_beta)){
  FRCH_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_abs_storm_list_beta[[i]][["datavalue"]]-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_abs_storm_list_beta)){
  MOOS_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_abs_storm_list_beta[[i]][["datavalue"]]-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}


###### NO3  #######
FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 142),
                            rep("storm10", 689),
                            rep("storm11a", 353),
                            
                            rep("storm2a", 364),
                            
                            rep("storm3", 196),
                            rep("storm4a", 88),
                            rep("storm4b", 153),
                            rep("storm5", 331),
                            rep("storm6", 303),
                            rep("storm7", 129),
                            rep("storm8a", 175),
                            
                            rep("storm9", 99))

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
MOOS_NO3_storm$storm.ID = c(rep("storm10", 432),
                            rep("storm11a", 420),
                            
                            rep("storm2a", 412),
                            
                            rep("storm3", 198),
                            
                            rep("storm5", 282),
                            rep("storm6", 335),
                            rep("storm7", 176),
                            rep("storm8a", 181),
                            
                            rep("storm9", 109))

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
CARI_NO3_storm$storm.ID = c(rep("storm1", 317),
                            rep("storm10", 254),
                            rep("storm11", 215),
                            rep("storm12a", 419),
                            rep("storm12b", 519),
                            rep("storm2", 181),
                            rep("storm3", 121),
                            rep("storm4a", 277),
                            
                            rep("storm5a", 777),
                            
                            rep("storm6", 650),
                            rep("storm7", 155),
                            rep("storm8", 191),
                            rep("storm9", 367))

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

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 142),
                             rep("storm10", 689),
                             rep("storm11a", 353),
                             
                             rep("storm2a", 364),
                             
                             rep("storm3", 196),
                             rep("storm4a", 88),
                             rep("storm4b", 153),
                             rep("storm5", 331),
                             rep("storm6", 303),
                             rep("storm7", 129),
                             rep("storm8a", 175),
                             
                             rep("storm9", 99))

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
MOOS_fDOM_storm$storm.ID = c(rep("storm10", 432),
                             rep("storm11a", 420),
                             
                             rep("storm2a", 412),
                             
                             rep("storm3", 198),
                             
                             rep("storm5", 282),
                             rep("storm6", 335),
                             rep("storm7", 176),
                             rep("storm8a", 181),
                             
                             rep("storm9", 109))

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
CARI_fDOM_storm$storm.ID = c(rep("storm1", 317),
                             rep("storm10", 254),
                             rep("storm11", 215),
                             rep("storm12a", 419),
                             rep("storm12b", 519),
                             rep("storm2", 181),
                             rep("storm3", 121),
                             rep("storm4a", 277),
                             
                             rep("storm5a", 777),
                             
                             rep("storm6", 650),
                             rep("storm7", 155),
                             rep("storm8", 191),
                             rep("storm9", 367))

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

FRCH_SPC_storm$storm.ID = c(rep("storm1", 142),
                            rep("storm10", 689),
                            rep("storm11a", 353),
                            
                            rep("storm2a", 364),
                            
                            rep("storm3", 196),
                            rep("storm4a", 88),
                            rep("storm4b", 153),
                            rep("storm5", 331),
                            rep("storm6", 303),
                            rep("storm7", 129),
                            rep("storm8a", 175),
                            
                            rep("storm9", 99))

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
MOOS_SPC_storm$storm.ID = c(rep("storm10", 432),
                            rep("storm11a", 420),
                            
                            rep("storm2a", 412),
                            
                            rep("storm3", 198),
                            
                            rep("storm5", 282),
                            rep("storm6", 335),
                            rep("storm7", 176),
                            rep("storm8a", 181),
                            
                            rep("storm9", 109))

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
CARI_SPC_storm$storm.ID = c(rep("storm1", 317),
                            rep("storm10", 254),
                            rep("storm11", 215),
                            rep("storm12a", 419),
                            rep("storm12b", 519),
                            rep("storm2", 181),
                            rep("storm3", 121),
                            rep("storm4a", 277),
                            
                            rep("storm5a", 777),
                            
                            rep("storm6", 650),
                            rep("storm7", 155),
                            rep("storm8", 191),
                            rep("storm9", 367))

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

FRCH_turb_storm$storm.ID = c(rep("storm1", 142),
                             rep("storm10", 689),
                             rep("storm11a", 353),
                             
                             rep("storm2a", 364),
                             
                             rep("storm3", 196),
                             rep("storm4a", 88),
                             rep("storm4b", 153),
                             rep("storm5", 331),
                             rep("storm6", 303),
                             rep("storm7", 129),
                             rep("storm8a", 175),
                             
                             rep("storm9", 99))

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
MOOS_turb_storm$storm.ID = c(rep("storm10", 432),
                             rep("storm11a", 420),
                             
                             rep("storm2a", 412),
                             
                             rep("storm3", 198),
                             
                             rep("storm5", 282),
                             rep("storm6", 335),
                             rep("storm7", 176),
                             rep("storm8a", 181),
                             
                             rep("storm9", 109))

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
CARI_turb_storm$storm.ID = c(rep("storm1", 317),
                             rep("storm10", 254),
                             rep("storm11", 215),
                             rep("storm12a", 419),
                             rep("storm12b", 519),
                             rep("storm2", 181),
                             rep("storm3", 121),
                             rep("storm4a", 277),
                             
                             rep("storm5a", 777),
                             
                             rep("storm6", 650),
                             rep("storm7", 155),
                             rep("storm8", 191),
                             rep("storm9", 367))

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


##### ABS #####
FRCH_abs_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_abs_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_abs_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_abs_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_abs_storm$storm.ID = c(rep("storm1", 142),
                            rep("storm10", 689),
                            rep("storm11a", 353),
                            
                            rep("storm2a", 364),
                            
                            rep("storm3", 196),
                            rep("storm4a", 88),
                            rep("storm4b", 153),
                            rep("storm5", 331),
                            rep("storm6", 303),
                            rep("storm7", 129),
                            rep("storm8a", 175),
                            
                            rep("storm9", 99))

names(FRCH_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
FRCH_abs_storm$site.ID <- "FRCH"

cols <- c("abs.norm","Q.norm")
FRCH_abs_storm[cols] <- log(FRCH_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_abs_storm <- FRCH_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_abs_storm_ascending <- filter(FRCH_abs_storm, limb == "ascending")

FRCH_abs_storm_ascending <- FRCH_abs_storm_ascending[is.finite(FRCH_abs_storm_ascending$Q.norm) & is.finite(FRCH_abs_storm_ascending$abs.norm), ]

beta.all.abs <- FRCH_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_abs_storm$storm.ID = c(rep("storm10", 432),
                            rep("storm11a", 420),
                            
                            rep("storm2a", 412),
                            
                            rep("storm3", 198),
                            
                            rep("storm5", 282),
                            rep("storm6", 335),
                            rep("storm7", 176),
                            rep("storm8a", 181),
                            
                            rep("storm9", 109))

names(MOOS_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
MOOS_abs_storm$site.ID <- "MOOS"

MOOS_abs_storm[cols] <- log(MOOS_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_abs_storm <- MOOS_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_abs_storm_ascending <- filter(MOOS_abs_storm, limb == "ascending")

MOOS_abs_storm_ascending <- MOOS_abs_storm_ascending[is.finite(MOOS_abs_storm_ascending$Q.norm) & is.finite(MOOS_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- MOOS_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site



All_abs_storm <- rbind(FRCH_abs_storm_ascending, MOOS_abs_storm_ascending)

beta.all.abs <- All_abs_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


beta.all.abs$response_var <- "abs"

all.2018.ci.abs <- All_abs_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(abs.norm ~ Q.norm, data = .x)))

all.2018.ci.abs$response_var <- "abs"


beta.all.2018 <- rbind(all.2018.ci.no3, all.2018.ci.fDOM,
                       all.2018.ci.SPC, all.2018.ci.turb,
                       all.2018.ci.abs)

write.csv(beta.all.2018, here("Output_from_analysis", "06_BETA", "beta.2018.csv"))

beta.all.2018 <- beta.all.2018 %>% 
  filter(Parameter != "(Intercept)")




########################################## 2019 ##########################################################
setwd("Storm_Events/2019")
storm_file_list_beta <- list.files(path="FRCH_MOOS_VAUL_POKE_STRT_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("FRCH_MOOS_VAUL_POKE_STRT_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

#  organize storm data by site and solute 
CARI_storm_list_beta = storm_list_beta[c(1:50)] # 50
FRCH_storm_list_beta = storm_list_beta[c(51:122)] #72
MOOS_storm_list_beta = storm_list_beta[c(123:188)] #66
POKE_storm_list_beta = storm_list_beta[c(189:254)] # 66
STRT_storm_list_beta = storm_list_beta[c(255:320)] # 66
VAUL_storm_list_beta = storm_list_beta[c(321:380)] # 60


CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("Turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("Turb", names(FRCH_storm_list_beta)))]
FRCH_abs_storm_list_beta = FRCH_storm_list_beta[c(grep("abs", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("Turb", names(MOOS_storm_list_beta)))]
MOOS_abs_storm_list_beta = MOOS_storm_list_beta[c(grep("abs", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

POKE_NO3_storm_list_beta = POKE_storm_list_beta[c(grep("NO3", names(POKE_storm_list_beta)))]
POKE_fDOM_storm_list_beta = POKE_storm_list_beta[c(grep("fDOM", names(POKE_storm_list_beta)))]
POKE_SpCond_storm_list_beta = POKE_storm_list_beta[c(grep("SPC", names(POKE_storm_list_beta)))]
POKE_turb_storm_list_beta = POKE_storm_list_beta[c(grep("Turb", names(POKE_storm_list_beta)))]
POKE_abs_storm_list_beta = POKE_storm_list_beta[c(grep("abs", names(POKE_storm_list_beta)))]
POKE_Q_storm_list_beta = POKE_storm_list_beta[c(grep("Q", names(POKE_storm_list_beta)))]

STRT_NO3_storm_list_beta = STRT_storm_list_beta[c(grep("NO3", names(STRT_storm_list_beta)))]
STRT_fDOM_storm_list_beta = STRT_storm_list_beta[c(grep("fDOM", names(STRT_storm_list_beta)))]
STRT_SpCond_storm_list_beta = STRT_storm_list_beta[c(grep("SPC", names(STRT_storm_list_beta)))]
STRT_turb_storm_list_beta = STRT_storm_list_beta[c(grep("Turb", names(STRT_storm_list_beta)))]
STRT_abs_storm_list_beta = STRT_storm_list_beta[c(grep("abs", names(STRT_storm_list_beta)))]
STRT_Q_storm_list_beta = STRT_storm_list_beta[c(grep("Q", names(STRT_storm_list_beta)))]

VAUL_NO3_storm_list_beta = VAUL_storm_list_beta[c(grep("NO3", names(VAUL_storm_list_beta)))]
VAUL_fDOM_storm_list_beta = VAUL_storm_list_beta[c(grep("fDOM", names(VAUL_storm_list_beta)))]
VAUL_SpCond_storm_list_beta = VAUL_storm_list_beta[c(grep("SPC", names(VAUL_storm_list_beta)))]
VAUL_turb_storm_list_beta = VAUL_storm_list_beta[c(grep("turb", names(VAUL_storm_list_beta)))]
VAUL_abs_storm_list_beta = VAUL_storm_list_beta[c(grep("abs", names(VAUL_storm_list_beta)))]
VAUL_Q_storm_list_beta = VAUL_storm_list_beta[c(grep("Q", names(VAUL_storm_list_beta)))]

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

# POKE
for(i in 1:length(POKE_Q_storm_list_beta)){
  POKE_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_Q_storm_list_beta[[i]][["datavalue"]]-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# STRT
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_Q_storm_list_beta[[i]][["datavalue"]]-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# VAUL
for(i in 1:length(VAUL_Q_storm_list_beta)){
  VAUL_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_Q_storm_list_beta[[i]][["datavalue"]]-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# CARI
for(i in 1:length(CARI_Q_storm_list_beta)){
  CARI_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_Q_storm_list_beta[[i]][["datavalue"]]-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# normalize solute data 
#  remove NAs associated with the CARI data due to Q data and solute data being on different temporal resolutions (NO3 on 15 min and Q on 1 min)
# CARI_NO3_storm_list_beta <- lapply(CARI_NO3_storm_list_beta, na.omit) 
# CARI_fDOM_storm_list_beta <- lapply(CARI_fDOM_storm_list_beta, na.omit)
# CARI_SpCond_storm_list_beta <- lapply(CARI_SpCond_storm_list_beta, na.omit)
# CARI_turb_storm_list_beta <- lapply(CARI_turb_storm_list_beta, na.omit)

#NO3
for(i in 1:length(FRCH_NO3_storm_list_beta)){
  FRCH_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list_beta[[i]][["datavalue"]]-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list_beta)){
  MOOS_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list_beta[[i]][["datavalue"]]-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))/
    (max(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE)-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))
}

for(i in 1:length(POKE_NO3_storm_list_beta)){
  POKE_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list_beta[[i]][["datavalue"]]-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))/
    (max(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE)-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))
}

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list_beta[[i]][["datavalue"]]-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list_beta)){
  VAUL_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list_beta[[i]][["datavalue"]]-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_fDOM_storm_list_beta)){
  POKE_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list_beta[[i]][["datavalue"]]-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list_beta[[i]][["datavalue"]]-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list_beta)){
  VAUL_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list_beta[[i]][["datavalue"]]-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_SpCond_storm_list_beta)){
  POKE_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_SpCond_storm_list_beta[[i]][["datavalue"]]-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_SpCond_storm_list_beta[[i]][["datavalue"]]-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SpCond_storm_list_beta)){
  VAUL_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_SpCond_storm_list_beta[[i]][["datavalue"]]-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_turb_storm_list_beta)){
  POKE_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list_beta[[i]][["datavalue"]]-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list_beta[[i]][["datavalue"]]-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list_beta)){
  VAUL_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list_beta[[i]][["datavalue"]]-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#ABS
for(i in 1:length(FRCH_abs_storm_list_beta)){
  FRCH_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_abs_storm_list_beta[[i]][["datavalue"]]-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_abs_storm_list_beta)){
  MOOS_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_abs_storm_list_beta[[i]][["datavalue"]]-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_abs_storm_list_beta)){
  POKE_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_abs_storm_list_beta[[i]][["datavalue"]]-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_abs_storm_list_beta)){
  STRT_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_abs_storm_list_beta[[i]][["datavalue"]]-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_abs_storm_list_beta)){
  VAUL_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_abs_storm_list_beta[[i]][["datavalue"]]-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}


###### NO3  #######
FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
POKE_NO3_storm <- map2_df(POKE_Q_storm_list_beta, POKE_NO3_storm_list_beta, inner_join, by = "valuedatetime")
STRT_NO3_storm <- map2_df(STRT_Q_storm_list_beta, STRT_NO3_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_NO3_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 425),
                            
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            
                            rep("storm12c", 1375),
                            
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 133),
                            rep("storm6", 289))
                            

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
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 702),
                            rep("storm3", 250),
                            rep("storm4", 228),
                            rep("storm5", 266),
                            rep("storm6a", 434),
                            
                            rep("storm6d", 479),
                            rep("storm7a", 166),
                            rep("storm7b", 84),
                            rep("storm7c", 430),
                            rep("storm8", 174),
                            rep("storm9", 530))

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

# POKE # 
POKE_NO3_storm$storm.ID = c(rep("storm1", 103),
                            rep("storm2", 91),
                            rep("storm3", 147),
                            rep("storm4", 115),
                            rep("storm5a", 327),
                            
                            rep("storm5c", 111),
                            rep("storm5d", 99),
                            rep("storm6a", 283),
                            
                            rep("storm7", 235),
                            rep("storm8", 95),
                            rep("storm9", 211))

names(POKE_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
POKE_NO3_storm$site.ID <- "POKE"

POKE_NO3_storm[cols] <- log(POKE_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_NO3_storm <- POKE_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_NO3_storm_ascending <- filter(POKE_NO3_storm, limb == "ascending")

POKE_NO3_storm_ascending <- POKE_NO3_storm_ascending[is.finite(POKE_NO3_storm_ascending$Q.norm) & is.finite(POKE_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.poke.with.all <- POKE_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_NO3_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 246),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
STRT_NO3_storm$site.ID <- "STRT"

STRT_NO3_storm[cols] <- log(STRT_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_NO3_storm <- STRT_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_NO3_storm_ascending <- filter(STRT_NO3_storm, limb == "ascending")

STRT_NO3_storm_ascending <- STRT_NO3_storm_ascending[is.finite(STRT_NO3_storm_ascending$Q.norm) & is.finite(STRT_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.strt <- STRT_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_NO3_storm$storm.ID = c(rep("storm1", 191),
                            rep("storm2", 207),
                            rep("storm3", 191),
                            rep("storm4a", 307),
                            
                            rep("storm4c", 227),
                            rep("storm5", 275),
                            rep("storm6", 263),
                            rep("storm7", 107),
                            rep("storm8a", 455),
                            
                            rep("storm8c", 191))

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm1", 371),
                            rep("storm2", 143),
                            rep("storm3", 83),
                            rep("storm4", 147),
                            rep("storm5", 135),
                            rep("storm6a", 319),
                            
                            rep("storm6c", 481),
                            rep("storm6d", 129),
                            rep("storm7a", 271),
                            
                            rep("storm8", 267))
                           

names(CARI_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
CARI_NO3_storm$site.ID <- "CARI"

CARI_NO3_storm[cols] <- log(CARI_NO3_storm[cols]) # making concentrations and Q log transformed

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

# ALL # 
FRCH_NO3_storm_ascending$DateTime <- as.POSIXct(FRCH_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_NO3_storm_ascending$DateTime <- as.POSIXct(MOOS_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
STRT_NO3_storm_ascending$DateTime <- as.POSIXct(STRT_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_NO3_storm_ascending$DateTime <- as.POSIXct(VAUL_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_NO3_storm_ascending$DateTime <- as.POSIXct(CARI_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_NO3_storm_ascending$DateTime <- as.POSIXct(POKE_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       STRT_NO3_storm_ascending, VAUL_NO3_storm_ascending, 
                       CARI_NO3_storm_ascending, POKE_NO3_storm_ascending)

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

all.2019.ci.no3 <- All_NO3_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2019.ci.no3$response_var <- "NO3"


##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
POKE_fDOM_storm <- map2_df(POKE_Q_storm_list_beta, POKE_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
STRT_fDOM_storm <- map2_df(STRT_Q_storm_list_beta, STRT_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_fDOM_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
CARI_fDOM_storm <- map2_df(CARI_Q_storm_list_beta, CARI_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 993),
                             rep("storm10a", 425),
                             
                             rep("storm11", 479),
                             rep("storm12a", 183),
                             
                             rep("storm12c", 1375),
                             
                             rep("storm13", 391),
                             rep("storm14", 631),
                             rep("storm2", 165),
                             rep("storm3", 201),
                             rep("storm4", 193),
                             rep("storm5", 133),
                             rep("storm6", 289))

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
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 702),
                             rep("storm3", 250),
                             rep("storm4", 228),
                             rep("storm5", 266),
                             rep("storm6a", 434),
                             
                             rep("storm6d", 479),
                             rep("storm7a", 166),
                             rep("storm7b", 84),
                             rep("storm7c", 430),
                             rep("storm8", 174),
                             rep("storm9", 530))
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

# POKE # 
POKE_fDOM_storm$storm.ID = c(rep("storm1", 103),
                             rep("storm2", 91),
                             rep("storm3", 147),
                             rep("storm4", 115),
                             rep("storm5a", 327),
                             
                             rep("storm5c", 111),
                             rep("storm5d", 99),
                             rep("storm6a", 283),
                             
                             rep("storm7", 235),
                             rep("storm8", 95),
                             rep("storm9", 211))

names(POKE_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
POKE_fDOM_storm$site.ID <- "POKE"

POKE_fDOM_storm[cols] <- log(POKE_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_fDOM_storm <- POKE_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_fDOM_storm_ascending <- filter(POKE_fDOM_storm, limb == "ascending")

POKE_fDOM_storm_ascending <- POKE_fDOM_storm_ascending[is.finite(POKE_fDOM_storm_ascending$Q.norm) & is.finite(POKE_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.poke.with.all <- POKE_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_fDOM_storm$storm.ID = c(rep("storm1", 638),
                             rep("storm2", 274),
                             rep("storm3a", 1035),
                             rep("storm3b", 286),
                             rep("storm3c", 174),
                             rep("storm4", 466),
                             rep("storm5", 98),
                             rep("storm6", 246),
                             rep("storm7", 246),
                             rep("storm7b", 266),
                             rep("storm7c", 258))

names(STRT_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
STRT_fDOM_storm$site.ID <- "STRT"

STRT_fDOM_storm[cols] <- log(STRT_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_fDOM_storm <- STRT_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_fDOM_storm_ascending <- filter(STRT_fDOM_storm, limb == "ascending")

STRT_fDOM_storm_ascending <- STRT_fDOM_storm_ascending[is.finite(STRT_fDOM_storm_ascending$Q.norm) & is.finite(STRT_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.strt <- STRT_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_fDOM_storm$storm.ID = c(rep("storm1", 191),
                             rep("storm2", 207),
                             rep("storm3", 191),
                             rep("storm4a", 307),
                             
                             rep("storm4c", 227),
                             rep("storm5", 275),
                             rep("storm6", 263),
                             rep("storm7", 107),
                             rep("storm8a", 455),
                             
                             rep("storm8c", 191))

names(VAUL_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
VAUL_fDOM_storm$site.ID <- "VAUL"

VAUL_fDOM_storm[cols] <- log(VAUL_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_fDOM_storm <- VAUL_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_fDOM_storm_ascending <- filter(VAUL_fDOM_storm, limb == "ascending")

VAUL_fDOM_storm_ascending <- VAUL_fDOM_storm_ascending[is.finite(VAUL_fDOM_storm_ascending$Q.norm) & is.finite(VAUL_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.vaul <- VAUL_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_fDOM_storm$storm.ID = c(rep("storm1", 371),
                             rep("storm2", 143),
                             rep("storm3", 83),
                             rep("storm4", 147),
                             rep("storm5", 135),
                             rep("storm6a", 319),
                             
                             rep("storm6c", 481),
                             rep("storm6d", 129),
                             rep("storm7a", 271),
                             
                             rep("storm8", 267))


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

beta.all.fDOM.cari <- CARI_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


# ALL # 
FRCH_fDOM_storm_ascending$DateTime <- as.POSIXct(FRCH_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_fDOM_storm_ascending$DateTime <- as.POSIXct(MOOS_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_fDOM_storm_ascending$DateTime <- as.POSIXct(STRT_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_fDOM_storm_ascending$DateTime <- as.POSIXct(VAUL_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

CARI_fDOM_storm_ascending$DateTime <- as.POSIXct(CARI_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_fDOM_storm_ascending$DateTime <- as.POSIXct(POKE_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                        STRT_fDOM_storm_ascending, VAUL_fDOM_storm_ascending, 
                        CARI_fDOM_storm_ascending, POKE_fDOM_storm_ascending)

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

all.2019.ci.fDOM <- All_fDOM_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(fDOM.norm ~ Q.norm, data = .x)))

all.2019.ci.fDOM$response_var <- "fDOM"


##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
POKE_SPC_storm <- map2_df(POKE_Q_storm_list_beta, POKE_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
STRT_SPC_storm <- map2_df(STRT_Q_storm_list_beta, STRT_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_SPC_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
CARI_SPC_storm <- map2_df(CARI_Q_storm_list_beta, CARI_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 425),
                            
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            
                            rep("storm12c", 1375),
                            
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 133),
                            rep("storm6", 289))

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
MOOS_SPC_storm$storm.ID = c(rep("storm1", 702),
                            rep("storm3", 250),
                            rep("storm4", 228),
                            rep("storm5", 266),
                            rep("storm6a", 434),
                            
                            rep("storm6d", 479),
                            rep("storm7a", 166),
                            rep("storm7b", 84),
                            rep("storm7c", 430),
                            rep("storm8", 174),
                            rep("storm9", 530))
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

# POKE # 
POKE_SPC_storm$storm.ID = c(rep("storm1", 103),
                            rep("storm2", 91),
                            rep("storm3", 147),
                            rep("storm4", 115),
                            rep("storm5a", 327),
                            
                            rep("storm5c", 111),
                            rep("storm5d", 99),
                            rep("storm6a", 283),
                            
                            rep("storm7", 235),
                            rep("storm8", 95),
                            rep("storm9", 211))
names(POKE_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
POKE_SPC_storm$site.ID <- "POKE"

POKE_SPC_storm[cols] <- log(POKE_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_SPC_storm <- POKE_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_SPC_storm_ascending <- filter(POKE_SPC_storm, limb == "ascending")

POKE_SPC_storm_ascending <- POKE_SPC_storm_ascending[is.finite(POKE_SPC_storm_ascending$Q.norm) & is.finite(POKE_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.poke.with.all <- POKE_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_SPC_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 246),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
STRT_SPC_storm$site.ID <- "STRT"

STRT_SPC_storm[cols] <- log(STRT_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_SPC_storm <- STRT_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_SPC_storm_ascending <- filter(STRT_SPC_storm, limb == "ascending")

STRT_SPC_storm_ascending <- STRT_SPC_storm_ascending[is.finite(STRT_SPC_storm_ascending$Q.norm) & is.finite(STRT_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.strt <- STRT_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_SPC_storm$storm.ID = c(rep("storm1", 191),
                            rep("storm2", 207),
                            rep("storm3", 191),
                            rep("storm4a", 307),
                            
                            rep("storm4c", 227),
                            rep("storm5", 275),
                            rep("storm6", 263),
                            rep("storm7", 107),
                            rep("storm8a", 455),
                            
                            rep("storm8c", 191))

names(VAUL_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
VAUL_SPC_storm$site.ID <- "VAUL"

VAUL_SPC_storm[cols] <- log(VAUL_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_SPC_storm <- VAUL_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_SPC_storm_ascending <- filter(VAUL_SPC_storm, limb == "ascending")

VAUL_SPC_storm_ascending <- VAUL_SPC_storm_ascending[is.finite(VAUL_SPC_storm_ascending$Q.norm) & is.finite(VAUL_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.vaul <- VAUL_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# CARI #
CARI_SPC_storm$storm.ID = c(rep("storm1", 371),
                            rep("storm2", 143),
                            rep("storm3", 83),
                            rep("storm4", 147),
                            rep("storm5", 135),
                            rep("storm6a", 319),
                            
                            rep("storm6c", 481),
                            rep("storm6d", 129),
                            rep("storm7a", 271),
                            
                            rep("storm8", 267))
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

beta.all.SPC.cari <- CARI_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


# ALL # 
FRCH_SPC_storm_ascending$DateTime <- as.POSIXct(FRCH_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_SPC_storm_ascending$DateTime <- as.POSIXct(MOOS_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_SPC_storm_ascending$DateTime <- as.POSIXct(STRT_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_SPC_storm_ascending$DateTime <- as.POSIXct(VAUL_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

CARI_SPC_storm_ascending$DateTime <- as.POSIXct(CARI_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_SPC_storm_ascending$DateTime <- as.POSIXct(POKE_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending, 
                       STRT_SPC_storm_ascending, VAUL_SPC_storm_ascending,
                       CARI_SPC_storm_ascending, POKE_SPC_storm_ascending)

beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

all.2019.ci.SPC <- All_SPC_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(SPC.norm ~ Q.norm, data = .x)))

all.2019.ci.SPC$response_var <- "SPC"


##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
POKE_turb_storm <- map2_df(POKE_Q_storm_list_beta, POKE_turb_storm_list_beta, inner_join, by = "valuedatetime")
STRT_turb_storm <- map2_df(STRT_Q_storm_list_beta, STRT_turb_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_turb_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_turb_storm_list_beta, inner_join, by = "valuedatetime")
CARI_turb_storm <- map2_df(CARI_Q_storm_list_beta, CARI_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 993),
                             rep("storm10a", 425),
                             
                             rep("storm11", 479),
                             rep("storm12a", 183),
                             
                             rep("storm12c", 1375),
                             
                             rep("storm13", 391),
                             rep("storm14", 631),
                             rep("storm2", 165),
                             rep("storm3", 201),
                             rep("storm4", 193),
                             rep("storm5", 133),
                             rep("storm6", 289))

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
MOOS_turb_storm$storm.ID = c(rep("storm1", 702),
                             rep("storm3", 250),
                             rep("storm4", 228),
                             rep("storm5", 266),
                             rep("storm6a", 434),
                             
                             rep("storm6d", 479),
                             rep("storm7a", 166),
                             rep("storm7b", 84),
                             rep("storm7c", 430),
                             rep("storm8", 174),
                             rep("storm9", 530))
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

# POKE # 
POKE_turb_storm$storm.ID = c(rep("storm1", 103),
                             rep("storm2", 91),
                             rep("storm3", 147),
                             rep("storm4", 115),
                             rep("storm5a", 327),
                             
                             rep("storm5c", 111),
                             rep("storm5d", 99),
                             rep("storm6a", 283),
                             
                             rep("storm7", 235),
                             rep("storm8", 95),
                             rep("storm9", 211))
names(POKE_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
POKE_turb_storm$site.ID <- "POKE"

POKE_turb_storm[cols] <- log(POKE_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_turb_storm <- POKE_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_turb_storm_ascending <- filter(POKE_turb_storm, limb == "ascending")

POKE_turb_storm_ascending <- POKE_turb_storm_ascending[is.finite(POKE_turb_storm_ascending$Q.norm) & is.finite(POKE_turb_storm_ascending$turb.norm), ]

beta.all.poke.moos.with.all <- POKE_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_turb_storm$storm.ID = c(rep("storm1", 638),
                             rep("storm2", 274),
                             rep("storm3a", 1035),
                             rep("storm3b", 286),
                             rep("storm3c", 174),
                             rep("storm4", 466),
                             rep("storm5", 98),
                             rep("storm6", 246),
                             rep("storm7", 246),
                             rep("storm7b", 266),
                             rep("storm7c", 258))

names(STRT_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
STRT_turb_storm$site.ID <- "STRT"

STRT_turb_storm[cols] <- log(STRT_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_turb_storm <- STRT_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_turb_storm_ascending <- filter(STRT_turb_storm, limb == "ascending")

STRT_turb_storm_ascending <- STRT_turb_storm_ascending[is.finite(STRT_turb_storm_ascending$Q.norm) & is.finite(STRT_turb_storm_ascending$turb.norm), ]

beta.all.turb.strt <- STRT_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_turb_storm$storm.ID = c(rep("storm1", 191),
                             rep("storm2", 207),
                             rep("storm3", 191),
                             rep("storm4a", 307),
                             
                             rep("storm4c", 227),
                             rep("storm5", 275),
                             rep("storm6", 263),
                             rep("storm7", 107),
                             rep("storm8a", 455),
                             
                             rep("storm8c", 191))

names(VAUL_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
VAUL_turb_storm$site.ID <- "VAUL"

VAUL_turb_storm[cols] <- log(VAUL_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_turb_storm <- VAUL_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_turb_storm_ascending <- filter(VAUL_turb_storm, limb == "ascending")

VAUL_turb_storm_ascending <- VAUL_turb_storm_ascending[is.finite(VAUL_turb_storm_ascending$Q.norm) & is.finite(VAUL_turb_storm_ascending$turb.norm), ]

beta.all.turb.vaul <- VAUL_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# CARI #
CARI_turb_storm$storm.ID = c(rep("storm1", 371),
                             rep("storm2", 143),
                             rep("storm3", 83),
                             rep("storm4", 147),
                             rep("storm5", 135),
                             rep("storm6a", 319),
                             
                             rep("storm6c", 481),
                             rep("storm6d", 129),
                             rep("storm7a", 271),
                             
                             rep("storm8", 267))

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

# ALL # 
FRCH_turb_storm_ascending$DateTime <- as.POSIXct(FRCH_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_turb_storm_ascending$DateTime <- as.POSIXct(MOOS_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_turb_storm_ascending$DateTime <- as.POSIXct(STRT_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_turb_storm_ascending$DateTime <- as.POSIXct(VAUL_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_turb_storm_ascending$DateTime <- as.POSIXct(CARI_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_turb_storm_ascending$DateTime <- as.POSIXct(POKE_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm <- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending, 
                        STRT_turb_storm_ascending, VAUL_turb_storm_ascending,
                        CARI_turb_storm_ascending, POKE_turb_storm_ascending)

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

all.2019.ci.turb <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2019.ci.turb$response_var <- "turb"

##### ABS #####
FRCH_abs_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_abs_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_abs_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_abs_storm_list_beta, inner_join, by = "valuedatetime")
POKE_abs_storm <- map2_df(POKE_Q_storm_list_beta, POKE_abs_storm_list_beta, inner_join, by = "valuedatetime")
STRT_abs_storm <- map2_df(STRT_Q_storm_list_beta, STRT_abs_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_abs_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_abs_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_abs_storm$storm.ID = c(rep("storm1", 993),
                            rep("storm10a", 425),
                            
                            rep("storm11", 479),
                            rep("storm12a", 183),
                            
                            rep("storm12c", 1375),
                            
                            rep("storm13", 391),
                            rep("storm14", 631),
                            rep("storm2", 165),
                            rep("storm3", 201),
                            rep("storm4", 193),
                            rep("storm5", 133),
                            rep("storm6", 289))

names(FRCH_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
FRCH_abs_storm$site.ID <- "FRCH"

cols <- c("abs.norm","Q.norm")
FRCH_abs_storm[cols] <- log(FRCH_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_abs_storm <- FRCH_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_abs_storm_ascending <- filter(FRCH_abs_storm, limb == "ascending")

FRCH_abs_storm_ascending <- FRCH_abs_storm_ascending[is.finite(FRCH_abs_storm_ascending$Q.norm) & is.finite(FRCH_abs_storm_ascending$abs.norm), ]

beta.all.abs <- FRCH_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_abs_storm$storm.ID = c(rep("storm1", 702),
                            rep("storm3", 250),
                            rep("storm4", 228),
                            rep("storm5", 266),
                            rep("storm6a", 434),
                            
                            rep("storm6d", 479),
                            rep("storm7a", 166),
                            rep("storm7b", 84),
                            rep("storm7c", 430),
                            rep("storm8", 174),
                            rep("storm9", 530))
names(MOOS_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
MOOS_abs_storm$site.ID <- "MOOS"

MOOS_abs_storm[cols] <- log(MOOS_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_abs_storm <- MOOS_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_abs_storm_ascending <- filter(MOOS_abs_storm, limb == "ascending")

MOOS_abs_storm_ascending <- MOOS_abs_storm_ascending[is.finite(MOOS_abs_storm_ascending$Q.norm) & is.finite(MOOS_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- MOOS_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_abs_storm$storm.ID = c(rep("storm1", 103),
                            rep("storm2", 91),
                            rep("storm3", 147),
                            rep("storm4", 115),
                            rep("storm5a", 327),
                            
                            rep("storm5c", 111),
                            rep("storm5d", 99),
                            rep("storm6a", 283),
                            
                            rep("storm7", 235),
                            rep("storm8", 95),
                            rep("storm9", 211))
names(POKE_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
POKE_abs_storm$site.ID <- "POKE"

POKE_abs_storm[cols] <- log(POKE_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_abs_storm <- POKE_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_abs_storm_ascending <- filter(POKE_abs_storm, limb == "ascending")

POKE_abs_storm_ascending <- POKE_abs_storm_ascending[is.finite(POKE_abs_storm_ascending$Q.norm) & is.finite(POKE_abs_storm_ascending$abs.norm), ]

beta.all.poke.moos.with.all <- POKE_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_abs_storm$storm.ID = c(rep("storm1", 638),
                            rep("storm2", 274),
                            rep("storm3a", 1035),
                            rep("storm3b", 286),
                            rep("storm3c", 174),
                            rep("storm4", 466),
                            rep("storm5", 98),
                            rep("storm6", 246),
                            rep("storm7", 246),
                            rep("storm7b", 266),
                            rep("storm7c", 258))

names(STRT_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
STRT_abs_storm$site.ID <- "STRT"

STRT_abs_storm[cols] <- log(STRT_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_abs_storm <- STRT_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_abs_storm_ascending <- filter(STRT_abs_storm, limb == "ascending")

STRT_abs_storm_ascending <- STRT_abs_storm_ascending[is.finite(STRT_abs_storm_ascending$Q.norm) & is.finite(STRT_abs_storm_ascending$abs.norm), ]

beta.all.abs.strt <- STRT_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_abs_storm$storm.ID = c(rep("storm1", 191),
                            rep("storm2", 207),
                            rep("storm3", 191),
                            rep("storm4a", 307),
                            
                            rep("storm4c", 227),
                            rep("storm5", 275),
                            rep("storm6", 263),
                            rep("storm7", 107),
                            rep("storm8a", 455),
                            
                            rep("storm8c", 191))

names(VAUL_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
VAUL_abs_storm$site.ID <- "VAUL"

VAUL_abs_storm[cols] <- log(VAUL_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_abs_storm <- VAUL_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_abs_storm_ascending <- filter(VAUL_abs_storm, limb == "ascending")

VAUL_abs_storm_ascending <- VAUL_abs_storm_ascending[is.finite(VAUL_abs_storm_ascending$Q.norm) & is.finite(VAUL_abs_storm_ascending$abs.norm), ]

beta.all.abs.vaul <- VAUL_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# ALL # 
FRCH_abs_storm_ascending$DateTime <- as.POSIXct(FRCH_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_abs_storm_ascending$DateTime <- as.POSIXct(MOOS_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_abs_storm_ascending$DateTime <- as.POSIXct(STRT_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_abs_storm_ascending$DateTime <- as.POSIXct(VAUL_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_abs_storm_ascending$DateTime <- as.POSIXct(POKE_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_abs_storm <- rbind(FRCH_abs_storm_ascending, MOOS_abs_storm_ascending, 
                        STRT_abs_storm_ascending, VAUL_abs_storm_ascending,
                        POKE_abs_storm_ascending)

beta.all.abs <- All_abs_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


beta.all.abs$response_var <- "abs"

all.2019.ci.abs <- All_abs_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(abs.norm ~ Q.norm, data = .x)))

all.2019.ci.abs$response_var <- "abs"


beta.all.2019 <- rbind(all.2019.ci.no3, all.2019.ci.fDOM,
                       all.2019.ci.SPC, all.2019.ci.turb,
                       all.2019.ci.abs)

write.csv(beta.all.2019, here("Output_from_analysis", "06_BETA", "beta.2019.csv"))

# beta.all.2019 <- beta.all.2019 %>% 
#   filter(Parameter != "(Intercept)")


########################################## 2020 ##########################################################
setwd("Storm_Events/2020")
storm_file_list_beta <- list.files(path="FRCH_MOOS_VAUL_POKE_STRT_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("FRCH_MOOS_VAUL_POKE_STRT_CARI//", storm_file_list_beta, replacement = "")

storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

#  organize storm data by site and solute # 5 for each storm 
CARI_storm_list_beta = storm_list_beta[c(1:55)] #55
FRCH_storm_list_beta = storm_list_beta[c(56:139)] #84
MOOS_storm_list_beta = storm_list_beta[c(140:205)] #66
POKE_storm_list_beta = storm_list_beta[c(206:313)]# 150
STRT_storm_list_beta = storm_list_beta[c(314:391)] #78
VAUL_storm_list_beta = storm_list_beta[c(392:469)] #102


CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
for(i in 1:length(CARI_NO3_storm_list_beta)){
  CARI_NO3_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(CARI_NO3_storm_list_beta[[i]][["valuedatetime"]],
                                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
for(i in 1:length(CARI_fDOM_storm_list_beta)){
  CARI_fDOM_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(CARI_fDOM_storm_list_beta[[i]][["valuedatetime"]],
                                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
for(i in 1:length(CARI_SpCond_storm_list_beta)){
  CARI_SpCond_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(CARI_SpCond_storm_list_beta[[i]][["valuedatetime"]],
                                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("Turb", names(CARI_storm_list_beta)))]
for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(CARI_turb_storm_list_beta[[i]][["valuedatetime"]],
                                                                   "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]
for(i in 1:length(CARI_Q_storm_list_beta)){
  CARI_Q_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(CARI_Q_storm_list_beta[[i]][["valuedatetime"]],
                                                              "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
}
FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("Turb", names(FRCH_storm_list_beta)))]
FRCH_abs_storm_list_beta = FRCH_storm_list_beta[c(grep("abs", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("Turb", names(MOOS_storm_list_beta)))]
MOOS_abs_storm_list_beta = MOOS_storm_list_beta[c(grep("abs", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

POKE_NO3_storm_list_beta = POKE_storm_list_beta[c(grep("NO3", names(POKE_storm_list_beta)))]
POKE_fDOM_storm_list_beta = POKE_storm_list_beta[c(grep("fDOM", names(POKE_storm_list_beta)))]
POKE_SpCond_storm_list_beta = POKE_storm_list_beta[c(grep("SPC", names(POKE_storm_list_beta)))]
POKE_turb_storm_list_beta = POKE_storm_list_beta[c(grep("turb", names(POKE_storm_list_beta)))]
POKE_abs_storm_list_beta = POKE_storm_list_beta[c(grep("abs", names(POKE_storm_list_beta)))]
POKE_Q_storm_list_beta = POKE_storm_list_beta[c(grep("Q", names(POKE_storm_list_beta)))]

STRT_NO3_storm_list_beta = STRT_storm_list_beta[c(grep("NO3", names(STRT_storm_list_beta)))]

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(STRT_NO3_storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
STRT_fDOM_storm_list_beta = STRT_storm_list_beta[c(grep("fDOM", names(STRT_storm_list_beta)))]
for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(STRT_fDOM_storm_list_beta[[i]][["valuedatetime"]],
                                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
STRT_SpCond_storm_list_beta = STRT_storm_list_beta[c(grep("SPC", names(STRT_storm_list_beta)))]
for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(STRT_SpCond_storm_list_beta[[i]][["valuedatetime"]],
                                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
STRT_turb_storm_list_beta = STRT_storm_list_beta[c(grep("Turb", names(STRT_storm_list_beta)))]
for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(STRT_turb_storm_list_beta[[i]][["valuedatetime"]],
                                                                   "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
STRT_abs_storm_list_beta = STRT_storm_list_beta[c(grep("abs", names(STRT_storm_list_beta)))]
for(i in 1:length(STRT_abs_storm_list_beta)){
  STRT_abs_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(STRT_abs_storm_list_beta[[i]][["valuedatetime"]],
                                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
STRT_Q_storm_list_beta = STRT_storm_list_beta[c(grep("Q", names(STRT_storm_list_beta)))]
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(STRT_Q_storm_list_beta[[i]][["valuedatetime"]],
                                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} 
VAUL_NO3_storm_list_beta = VAUL_storm_list_beta[c(grep("NO3", names(VAUL_storm_list_beta)))]
VAUL_fDOM_storm_list_beta = VAUL_storm_list_beta[c(grep("fDOM", names(VAUL_storm_list_beta)))]
VAUL_SpCond_storm_list_beta = VAUL_storm_list_beta[c(grep("SPC", names(VAUL_storm_list_beta)))]
VAUL_turb_storm_list_beta = VAUL_storm_list_beta[c(grep("Turb", names(VAUL_storm_list_beta)))]
VAUL_abs_storm_list_beta = VAUL_storm_list_beta[c(grep("abs", names(VAUL_storm_list_beta)))]
VAUL_Q_storm_list_beta = VAUL_storm_list_beta[c(grep("Q", names(VAUL_storm_list_beta)))]

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

# POKE
for(i in 1:length(POKE_Q_storm_list_beta)){
  POKE_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_Q_storm_list_beta[[i]][["datavalue"]]-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# STRT
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_Q_storm_list_beta[[i]][["datavalue"]]-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# VAUL
for(i in 1:length(VAUL_Q_storm_list_beta)){
  VAUL_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_Q_storm_list_beta[[i]][["datavalue"]]-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_NO3_storm_list_beta)){
  POKE_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list_beta[[i]][["datavalue"]]-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list_beta[[i]][["datavalue"]]-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list_beta)){
  VAUL_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list_beta[[i]][["datavalue"]]-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_fDOM_storm_list_beta)){
  POKE_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list_beta[[i]][["datavalue"]]-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list_beta[[i]][["datavalue"]]-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list_beta)){
  VAUL_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list_beta[[i]][["datavalue"]]-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_SpCond_storm_list_beta)){
  POKE_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_SpCond_storm_list_beta[[i]][["datavalue"]]-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_SpCond_storm_list_beta[[i]][["datavalue"]]-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SpCond_storm_list_beta)){
  VAUL_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_SpCond_storm_list_beta[[i]][["datavalue"]]-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_turb_storm_list_beta)){
  POKE_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list_beta[[i]][["datavalue"]]-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list_beta[[i]][["datavalue"]]-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list_beta)){
  VAUL_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list_beta[[i]][["datavalue"]]-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#ABS
for(i in 1:length(FRCH_abs_storm_list_beta)){
  FRCH_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_abs_storm_list_beta[[i]][["datavalue"]]-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_abs_storm_list_beta)){
  MOOS_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_abs_storm_list_beta[[i]][["datavalue"]]-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_abs_storm_list_beta)){
  POKE_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_abs_storm_list_beta[[i]][["datavalue"]]-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_abs_storm_list_beta)){
  STRT_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_abs_storm_list_beta[[i]][["datavalue"]]-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_abs_storm_list_beta)){
  VAUL_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_abs_storm_list_beta[[i]][["datavalue"]]-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}


###### NO3  #######

FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
POKE_NO3_storm <- map2_df(POKE_Q_storm_list_beta, POKE_NO3_storm_list_beta, inner_join, by = "valuedatetime")
STRT_NO3_storm <- map2_df(STRT_Q_storm_list_beta, STRT_NO3_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_NO3_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 487),
                            rep("storm10a", 255),
                            rep("storm10b", 151),
                            rep("storm11", 91),
                            
                            rep("storm2", 123),
                            rep("storm3a", 1463),
                            
                            rep("storm4a", 187),
                            rep("storm4b", 203),
                            rep("storm5", 59),
                            rep("storm6", 103),
                            rep("storm7", 339),
                            rep("storm8", 383),
                            rep("storm9a", 139),
                            rep("storm9b", 286))


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
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 723),
                            rep("storm2", 327),
                            rep("storm3", 129),
                            rep("storm4", 321),
                            rep("storm5", 252),
                            rep("storm6a", 108),
                            rep("storm6b", 288),
                            rep("storm7a", 276),
                            rep("storm7b", 186),
                            rep("storm8", 195),
                            rep("storm9", 405))

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

# POKE # 
POKE_NO3_storm$storm.ID = c(
  
  rep("storm11", 199),
  rep("storm12", 307),
  rep("storm13", 87),
  rep("storm14", 383),
  rep("storm15", 335),
  
  rep("storm19", 135),
  
  rep("storm20", 139),
  rep("storm21", 227),
  rep("storm22a", 107),
  rep("storm22b", 212),
  rep("storm3", 119),
  rep("storm4a", 98),
  rep("storm4b", 95),
  rep("storm4c", 159),
  rep("storm5", 219),
  
  rep("storm7", 127),
  rep("storm8", 135),
  rep("storm9", 263))

names(POKE_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
POKE_NO3_storm$site.ID <- "POKE"

POKE_NO3_storm[cols] <- log(POKE_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_NO3_storm <- POKE_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_NO3_storm_ascending <- filter(POKE_NO3_storm, limb == "ascending")

POKE_NO3_storm_ascending <- POKE_NO3_storm_ascending[is.finite(POKE_NO3_storm_ascending$Q.norm) & is.finite(POKE_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- POKE_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_NO3_storm$storm.ID = c(rep("storm10", 246),
                            rep("storm1a", 969),
                            
                            rep("storm2", 166),
                            rep("storm3", 386),
                            rep("storm4a", 140),
                            rep("storm4b", 322),
                            rep("storm5", 250),
                            rep("storm6", 122),
                            rep("storm7a", 98),
                            
                            rep("storm8", 162),
                            rep("storm9a", 294),
                            rep("storm9b", 134),
                            rep("storm9c", 482))

names(STRT_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
STRT_NO3_storm$site.ID <- "STRT"

STRT_NO3_storm[cols] <- log(STRT_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_NO3_storm <- STRT_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_NO3_storm_ascending <- filter(STRT_NO3_storm, limb == "ascending")

STRT_NO3_storm_ascending <- STRT_NO3_storm_ascending[is.finite(STRT_NO3_storm_ascending$Q.norm) & is.finite(STRT_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.strt <- STRT_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_NO3_storm$storm.ID = c(rep("storm10", 195),
                            rep("storm11", 399),
                            rep("storm12", 171),
                            rep("storm13", 222),
                            rep("storm14", 211),
                            rep("storm1a", 111),
                            rep("storm1b", 234),
                            rep("storm1c", 406),
                            
                            rep("storm3", 342),
                            rep("storm4", 318),
                            
                            rep("storm6a", 107),
                            rep("storm6b", 511),
                            
                            rep("storm8", 91))

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm1", 203),
                            rep("storm2a", 103),
                            rep("storm2b", 251),
                            
                            rep("storm3", 291),
                            rep("storm4", 156),
                            rep("storm5", 220),
                            rep("storm6", 184),
                            rep("storm7", 308),
                            rep("storm8a", 111),
                            rep("storm8b", 481),
                            rep("storm9", 99))

names(CARI_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
CARI_NO3_storm$site.ID <- "CARI"

CARI_NO3_storm[cols] <- log(CARI_NO3_storm[cols]) # making concentrations and Q log transformed

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

# ALL # 

FRCH_NO3_storm_ascending$DateTime <- as.POSIXct(FRCH_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_NO3_storm_ascending$DateTime <- as.POSIXct(MOOS_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
STRT_NO3_storm_ascending$DateTime <- as.POSIXct(STRT_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_NO3_storm_ascending$DateTime <- as.POSIXct(VAUL_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_NO3_storm_ascending$DateTime <- as.POSIXct(CARI_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_NO3_storm_ascending$DateTime <- as.POSIXct(POKE_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")


All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       POKE_NO3_storm_ascending, VAUL_NO3_storm_ascending, 
                       STRT_NO3_storm_ascending, CARI_NO3_storm_ascending)

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

all.2020.ci.no3 <- All_NO3_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2020.ci.no3$response_var <- "NO3"


##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
POKE_fDOM_storm <- map2_df(POKE_Q_storm_list_beta, POKE_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
STRT_fDOM_storm <- map2_df(STRT_Q_storm_list_beta, STRT_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_fDOM_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
CARI_fDOM_storm <- map2_df(CARI_Q_storm_list_beta, CARI_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 487),
                             rep("storm10a", 255),
                             rep("storm10b", 151),
                             rep("storm11", 91),
                             
                             rep("storm2", 123),
                             rep("storm3a", 1463),
                             
                             rep("storm4a", 187),
                             rep("storm4b", 203),
                             rep("storm5", 59),
                             rep("storm6", 103),
                             rep("storm7", 339),
                             rep("storm8", 383),
                             rep("storm9a", 139),
                             rep("storm9b", 286))

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
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 723),
                             rep("storm2", 327),
                             rep("storm3", 129),
                             rep("storm4", 321),
                             rep("storm5", 252),
                             rep("storm6a", 108),
                             rep("storm6b", 288),
                             rep("storm7a", 276),
                             rep("storm7b", 186),
                             rep("storm8", 195),
                             rep("storm9", 405))
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

# POKE # 
POKE_fDOM_storm$storm.ID = c(
  
  rep("storm11", 199),
  rep("storm12", 307),
  rep("storm13", 87),
  rep("storm14", 383),
  rep("storm15", 335),
  
  rep("storm19", 135),
  
  rep("storm20", 139),
  rep("storm21", 227),
  rep("storm22a", 107),
  rep("storm22b", 212),
  rep("storm3", 119),
  rep("storm4a", 98),
  rep("storm4b", 95),
  rep("storm4c", 159),
  rep("storm5", 219),
  
  rep("storm7", 127),
  rep("storm8", 135),
  rep("storm9", 263))

names(POKE_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
POKE_fDOM_storm$site.ID <- "POKE"

POKE_fDOM_storm[cols] <- log(POKE_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_fDOM_storm <- POKE_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_fDOM_storm_ascending <- filter(POKE_fDOM_storm, limb == "ascending")

POKE_fDOM_storm_ascending <- POKE_fDOM_storm_ascending[is.finite(POKE_fDOM_storm_ascending$Q.norm) & is.finite(POKE_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.moos.with.all <- POKE_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_fDOM_storm$storm.ID = c(rep("storm10", 246),
                             rep("storm1a", 969),
                             
                             rep("storm2", 166),
                             rep("storm3", 386),
                             rep("storm4a", 140),
                             rep("storm4b", 322),
                             rep("storm5", 250),
                             rep("storm6", 122),
                             rep("storm7a", 98),
                             
                             rep("storm8", 162),
                             rep("storm9a", 294),
                             rep("storm9b", 134),
                             rep("storm9c", 482))

names(STRT_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
STRT_fDOM_storm$site.ID <- "STRT"

STRT_fDOM_storm[cols] <- log(STRT_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

STRT_fDOM_storm <- na.omit(STRT_fDOM_storm)

STRT_fDOM_storm <- STRT_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_fDOM_storm_ascending <- filter(STRT_fDOM_storm, limb == "ascending")

STRT_fDOM_storm_ascending <- STRT_fDOM_storm_ascending[is.finite(STRT_fDOM_storm_ascending$Q.norm) & is.finite(STRT_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.strt <- STRT_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_fDOM_storm$storm.ID = c(rep("storm10", 195),
                             rep("storm11", 399),
                             rep("storm12", 171),
                             rep("storm13", 222),
                             rep("storm14", 211),
                             rep("storm1a", 111),
                             rep("storm1b", 234),
                             rep("storm1c", 406),
                             
                             rep("storm3", 342),
                             rep("storm4", 318),
                             
                             rep("storm6a", 107),
                             rep("storm6b", 511),
                             
                             rep("storm8", 91))

names(VAUL_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
VAUL_fDOM_storm$site.ID <- "VAUL"

VAUL_fDOM_storm[cols] <- log(VAUL_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_fDOM_storm <- VAUL_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_fDOM_storm_ascending <- filter(VAUL_fDOM_storm, limb == "ascending")

VAUL_fDOM_storm_ascending <- VAUL_fDOM_storm_ascending[is.finite(VAUL_fDOM_storm_ascending$Q.norm) & is.finite(VAUL_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.vaul <- VAUL_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_fDOM_storm$storm.ID = c(rep("storm1", 203),
                             rep("storm2a", 103),
                             rep("storm2b", 251),
                             
                             rep("storm3", 291),
                             rep("storm4", 156),
                             rep("storm5", 220),
                             rep("storm6", 184),
                             rep("storm7", 308),
                             rep("storm8a", 111),
                             rep("storm8b", 481),
                             rep("storm9", 99))


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


# ALL # 
FRCH_fDOM_storm_ascending$DateTime <- as.POSIXct(FRCH_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_fDOM_storm_ascending$DateTime <- as.POSIXct(MOOS_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
STRT_fDOM_storm_ascending$DateTime <- as.POSIXct(STRT_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_fDOM_storm_ascending$DateTime <- as.POSIXct(VAUL_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_fDOM_storm_ascending$DateTime <- as.POSIXct(CARI_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_fDOM_storm_ascending$DateTime <- as.POSIXct(POKE_fDOM_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                        POKE_fDOM_storm_ascending, VAUL_fDOM_storm_ascending,
                        STRT_fDOM_storm_ascending, CARI_fDOM_storm_ascending)

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

all.2020.ci.fDOM <- All_fDOM_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(fDOM.norm ~ Q.norm, data = .x)))

all.2020.ci.fDOM$response_var <- "fDOM"


##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
POKE_SPC_storm <- map2_df(POKE_Q_storm_list_beta, POKE_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
STRT_SPC_storm <- map2_df(STRT_Q_storm_list_beta, STRT_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_SPC_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
CARI_SPC_storm <- map2_df(CARI_Q_storm_list_beta, CARI_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 487),
                            rep("storm10a", 255),
                            rep("storm10b", 151),
                            rep("storm11", 91),
                            
                            rep("storm2", 123),
                            rep("storm3a", 1463),
                            
                            rep("storm4a", 187),
                            rep("storm4b", 203),
                            rep("storm5", 59),
                            rep("storm6", 103),
                            rep("storm7", 339),
                            rep("storm8", 383),
                            rep("storm9a", 139),
                            rep("storm9b", 286))

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
MOOS_SPC_storm$storm.ID = c(rep("storm1", 723),
                            rep("storm2", 327),
                            rep("storm3", 129),
                            rep("storm4", 321),
                            rep("storm5", 252),
                            rep("storm6a", 108),
                            rep("storm6b", 288),
                            rep("storm7a", 276),
                            rep("storm7b", 186),
                            rep("storm8", 195),
                            rep("storm9", 405))

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

# POKE # 
POKE_SPC_storm$storm.ID = c(
  
  rep("storm11", 199),
  rep("storm12", 307),
  rep("storm13", 87),
  rep("storm14", 383),
  rep("storm15", 335),
  
  rep("storm19", 135),
  
  rep("storm20", 139),
  rep("storm21", 227),
  rep("storm22a", 107),
  rep("storm22b", 212),
  rep("storm3", 119),
  rep("storm4a", 98),
  rep("storm4b", 95),
  rep("storm4c", 159),
  rep("storm5", 219),
  
  rep("storm7", 127),
  rep("storm8", 135),
  rep("storm9", 263))

names(POKE_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
POKE_SPC_storm$site.ID <- "POKE"

POKE_SPC_storm[cols] <- log(POKE_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_SPC_storm <- POKE_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_SPC_storm_ascending <- filter(POKE_SPC_storm, limb == "ascending")

POKE_SPC_storm_ascending <- POKE_SPC_storm_ascending[is.finite(POKE_SPC_storm_ascending$Q.norm) & is.finite(POKE_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.moos.with.all <- POKE_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_SPC_storm$storm.ID = c(rep("storm10", 246),
                            rep("storm1a", 969),
                            
                            rep("storm2", 166),
                            rep("storm3", 386),
                            rep("storm4a", 140),
                            rep("storm4b", 322),
                            rep("storm5", 250),
                            rep("storm6", 122),
                            rep("storm7a", 98),
                            
                            rep("storm8", 162),
                            rep("storm9a", 294),
                            rep("storm9b", 134),
                            rep("storm9c", 482))

names(STRT_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
STRT_SPC_storm$site.ID <- "STRT"

STRT_SPC_storm[cols] <- log(STRT_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_SPC_storm <- STRT_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_SPC_storm_ascending <- filter(STRT_SPC_storm, limb == "ascending")

STRT_SPC_storm_ascending <- STRT_SPC_storm_ascending[is.finite(STRT_SPC_storm_ascending$Q.norm) & is.finite(STRT_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.strt <- STRT_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_SPC_storm$storm.ID = c(rep("storm10", 195),
                            rep("storm11", 399),
                            rep("storm12", 171),
                            rep("storm13", 222),
                            rep("storm14", 211),
                            rep("storm1a", 111),
                            rep("storm1b", 234),
                            rep("storm1c", 406),
                            
                            rep("storm3", 342),
                            rep("storm4", 318),
                            
                            rep("storm6a", 107),
                            rep("storm6b", 511),
                            
                            rep("storm8", 91))

names(VAUL_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
VAUL_SPC_storm$site.ID <- "VAUL"

VAUL_SPC_storm[cols] <- log(VAUL_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_SPC_storm <- VAUL_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_SPC_storm_ascending <- filter(VAUL_SPC_storm, limb == "ascending")

VAUL_SPC_storm_ascending <- VAUL_SPC_storm_ascending[is.finite(VAUL_SPC_storm_ascending$Q.norm) & is.finite(VAUL_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.vaul <- VAUL_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_SPC_storm$storm.ID = c(rep("storm1", 203),
                            rep("storm2a", 103),
                            rep("storm2b", 251),
                            
                            rep("storm3", 291),
                            rep("storm4", 156),
                            rep("storm5", 220),
                            rep("storm6", 184),
                            rep("storm7", 308),
                            rep("storm8a", 111),
                            rep("storm8b", 481),
                            rep("storm9", 99))

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


# ALL # 
FRCH_SPC_storm_ascending$DateTime <- as.POSIXct(FRCH_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_SPC_storm_ascending$DateTime <- as.POSIXct(MOOS_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_SPC_storm_ascending$DateTime <- as.POSIXct(STRT_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_SPC_storm_ascending$DateTime <- as.POSIXct(VAUL_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

CARI_SPC_storm_ascending$DateTime <- as.POSIXct(CARI_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_SPC_storm_ascending$DateTime <- as.POSIXct(POKE_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending,
                       POKE_SPC_storm_ascending, VAUL_SPC_storm_ascending,
                       STRT_SPC_storm_ascending, CARI_SPC_storm_ascending)


beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

all.2020.ci.SPC <- All_SPC_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(SPC.norm ~ Q.norm, data = .x)))

all.2020.ci.SPC$response_var <- "SPC"


##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
POKE_turb_storm <- map2_df(POKE_Q_storm_list_beta, POKE_turb_storm_list_beta, inner_join, by = "valuedatetime")
STRT_turb_storm <- map2_df(STRT_Q_storm_list_beta, STRT_turb_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_turb_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_turb_storm_list_beta, inner_join, by = "valuedatetime")
CARI_turb_storm <- map2_df(CARI_Q_storm_list_beta, CARI_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 487),
                             rep("storm10a", 255),
                             rep("storm10b", 151),
                             rep("storm11", 91),
                             
                             rep("storm2", 123),
                             rep("storm3a", 1463),
                             
                             rep("storm4a", 187),
                             rep("storm4b", 203),
                             rep("storm5", 59),
                             rep("storm6", 103),
                             rep("storm7", 339),
                             rep("storm8", 383),
                             rep("storm9a", 139),
                             rep("storm9b", 286))

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
MOOS_turb_storm$storm.ID = c(rep("storm1", 723),
                             rep("storm2", 327),
                             rep("storm3", 129),
                             rep("storm4", 321),
                             rep("storm5", 252),
                             rep("storm6a", 108),
                             rep("storm6b", 288),
                             rep("storm7a", 276),
                             rep("storm7b", 186),
                             rep("storm8", 195),
                             rep("storm9", 405))

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

# POKE # 
POKE_turb_storm$storm.ID = c(
  
  rep("storm11", 199),
  rep("storm12", 307),
  rep("storm13", 87),
  rep("storm14", 383),
  rep("storm15", 335),
  
  rep("storm19", 135),
  
  rep("storm20", 139),
  rep("storm21", 227),
  rep("storm22a", 107),
  rep("storm22b", 212),
  rep("storm3", 119),
  rep("storm4a", 98),
  rep("storm4b", 95),
  rep("storm4c", 159),
  rep("storm5", 219),
  
  rep("storm7", 127),
  rep("storm8", 135),
  rep("storm9", 263))

names(POKE_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
POKE_turb_storm$site.ID <- "POKE"

POKE_turb_storm[cols] <- log(POKE_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_turb_storm <- POKE_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_turb_storm_ascending <- filter(POKE_turb_storm, limb == "ascending")

POKE_turb_storm_ascending <- POKE_turb_storm_ascending[is.finite(POKE_turb_storm_ascending$Q.norm) & is.finite(POKE_turb_storm_ascending$turb.norm), ]

beta.all.turb.moos.with.all <- POKE_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_turb_storm$storm.ID = c(rep("storm10", 246),
                             rep("storm1a", 969),
                             
                             rep("storm2", 166),
                             rep("storm3", 386),
                             rep("storm4a", 140),
                             rep("storm4b", 322),
                             rep("storm5", 250),
                             rep("storm6", 122),
                             rep("storm7a", 98),
                             
                             rep("storm8", 162),
                             rep("storm9a", 294),
                             rep("storm9b", 134),
                             rep("storm9c", 482))

names(STRT_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
STRT_turb_storm$site.ID <- "STRT"

STRT_turb_storm[cols] <- log(STRT_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_turb_storm <- STRT_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_turb_storm_ascending <- filter(STRT_turb_storm, limb == "ascending")

STRT_turb_storm_ascending <- STRT_turb_storm_ascending[is.finite(STRT_turb_storm_ascending$Q.norm) & is.finite(STRT_turb_storm_ascending$turb.norm), ]

beta.all.turb.strt <- STRT_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_turb_storm$storm.ID = c(rep("storm10", 195),
                             rep("storm11", 399),
                             rep("storm12", 171),
                             rep("storm13", 222),
                             rep("storm14", 211),
                             rep("storm1a", 111),
                             rep("storm1b", 234),
                             rep("storm1c", 406),
                             
                             rep("storm3", 342),
                             rep("storm4", 318),
                             
                             rep("storm6a", 107),
                             rep("storm6b", 511),
                             
                             rep("storm8", 91))

names(VAUL_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
VAUL_turb_storm$site.ID <- "VAUL"

VAUL_turb_storm[cols] <- log(VAUL_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_turb_storm <- VAUL_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_turb_storm_ascending <- filter(VAUL_turb_storm, limb == "ascending")

VAUL_turb_storm_ascending <- VAUL_turb_storm_ascending[is.finite(VAUL_turb_storm_ascending$Q.norm) & is.finite(VAUL_turb_storm_ascending$turb.norm), ]

beta.all.turb.vaul <- VAUL_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_turb_storm$storm.ID = c(rep("storm1", 203),
                             rep("storm2a", 103),
                             rep("storm2b", 251),
                             
                             rep("storm3", 291),
                             rep("storm4", 156),
                             rep("storm5", 220),
                             rep("storm6", 184),
                             rep("storm7", 308),
                             rep("storm8a", 111),
                             rep("storm8b", 481),
                             rep("storm9", 99))

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


# ALL # 
FRCH_turb_storm_ascending$DateTime <- as.POSIXct(FRCH_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_turb_storm_ascending$DateTime <- as.POSIXct(MOOS_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_turb_storm_ascending$DateTime <- as.POSIXct(STRT_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_turb_storm_ascending$DateTime <- as.POSIXct(VAUL_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_turb_storm_ascending$DateTime <- as.POSIXct(CARI_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_turb_storm_ascending$DateTime <- as.POSIXct(POKE_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm<- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending,
                       POKE_turb_storm_ascending, VAUL_turb_storm_ascending,
                       STRT_turb_storm_ascending, CARI_turb_storm_ascending )

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

all.2020.ci.turb <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2020.ci.turb$response_var <- "turb"


##### ABS #####
FRCH_abs_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_abs_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_abs_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_abs_storm_list_beta, inner_join, by = "valuedatetime")
POKE_abs_storm <- map2_df(POKE_Q_storm_list_beta, POKE_abs_storm_list_beta, inner_join, by = "valuedatetime")
STRT_abs_storm <- map2_df(STRT_Q_storm_list_beta, STRT_abs_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_abs_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_abs_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_abs_storm$storm.ID = c(rep("storm1", 487),
                            rep("storm10a", 255),
                            rep("storm10b", 151),
                            rep("storm11", 91),
                            
                            rep("storm2", 123),
                            rep("storm3a", 1463),
                            
                            rep("storm4a", 187),
                            rep("storm4b", 203),
                            rep("storm5", 59),
                            rep("storm6", 103),
                            rep("storm7", 339),
                            rep("storm8", 383),
                            rep("storm9a", 139),
                            rep("storm9b", 286))

names(FRCH_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
FRCH_abs_storm$site.ID <- "FRCH"

cols <- c("abs.norm","Q.norm")
FRCH_abs_storm[cols] <- log(FRCH_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_abs_storm <- FRCH_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_abs_storm_ascending <- filter(FRCH_abs_storm, limb == "ascending")

FRCH_abs_storm_ascending <- FRCH_abs_storm_ascending[is.finite(FRCH_abs_storm_ascending$Q.norm) & is.finite(FRCH_abs_storm_ascending$abs.norm), ]

beta.all.abs <- FRCH_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_abs_storm$storm.ID = c(rep("storm1", 723),
                            rep("storm2", 327),
                            rep("storm3", 129),
                            rep("storm4", 321),
                            rep("storm5", 252),
                            rep("storm6a", 108),
                            rep("storm6b", 288),
                            rep("storm7a", 276),
                            rep("storm7b", 186),
                            rep("storm8", 195),
                            rep("storm9", 405))

names(MOOS_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
MOOS_abs_storm$site.ID <- "MOOS"

MOOS_abs_storm[cols] <- log(MOOS_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_abs_storm <- MOOS_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_abs_storm_ascending <- filter(MOOS_abs_storm, limb == "ascending")

MOOS_abs_storm_ascending <- MOOS_abs_storm_ascending[is.finite(MOOS_abs_storm_ascending$Q.norm) & is.finite(MOOS_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- MOOS_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_abs_storm$storm.ID = c(
  
  rep("storm11", 199),
  rep("storm12", 307),
  rep("storm13", 87),
  rep("storm14", 383),
  rep("storm15", 335),
  
  rep("storm19", 135),
  
  rep("storm20", 139),
  rep("storm21", 227),
  rep("storm22a", 107),
  rep("storm22b", 212),
  rep("storm3", 119),
  rep("storm4a", 98),
  rep("storm4b", 95),
  rep("storm4c", 159),
  rep("storm5", 219),
  
  rep("storm7", 127),
  rep("storm8", 135),
  rep("storm9", 263))

names(POKE_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
POKE_abs_storm$site.ID <- "POKE"

POKE_abs_storm[cols] <- log(POKE_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_abs_storm <- POKE_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_abs_storm_ascending <- filter(POKE_abs_storm, limb == "ascending")

POKE_abs_storm_ascending <- POKE_abs_storm_ascending[is.finite(POKE_abs_storm_ascending$Q.norm) & is.finite(POKE_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- POKE_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_abs_storm$storm.ID = c(rep("storm10", 246),
                            rep("storm1a", 969),
                            
                            rep("storm2", 166),
                            rep("storm3", 386),
                            rep("storm4a", 140),
                            rep("storm4b", 322),
                            rep("storm5", 250),
                            rep("storm6", 122),
                            rep("storm7a", 98),
                            
                            rep("storm8", 162),
                            rep("storm9a", 294),
                            rep("storm9b", 134),
                            rep("storm9c", 482))

names(STRT_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
STRT_abs_storm$site.ID <- "STRT"

STRT_abs_storm[cols] <- log(STRT_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_abs_storm <- STRT_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_abs_storm_ascending <- filter(STRT_abs_storm, limb == "ascending")

STRT_abs_storm_ascending <- STRT_abs_storm_ascending[is.finite(STRT_abs_storm_ascending$Q.norm) & is.finite(STRT_abs_storm_ascending$abs.norm), ]

beta.all.abs.strt <- STRT_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_abs_storm$storm.ID = c(rep("storm10", 195),
                            rep("storm11", 399),
                            rep("storm12", 171),
                            rep("storm13", 222),
                            rep("storm14", 211),
                            rep("storm1a", 111),
                            rep("storm1b", 234),
                            rep("storm1c", 406),
                            
                            rep("storm3", 342),
                            rep("storm4", 318),
                            
                            rep("storm6a", 107),
                            rep("storm6b", 511),
                            
                            rep("storm8", 91))

names(VAUL_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
VAUL_abs_storm$site.ID <- "VAUL"

VAUL_abs_storm[cols] <- log(VAUL_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_abs_storm <- VAUL_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_abs_storm_ascending <- filter(VAUL_abs_storm, limb == "ascending")

VAUL_abs_storm_ascending <- VAUL_abs_storm_ascending[is.finite(VAUL_abs_storm_ascending$Q.norm) & is.finite(VAUL_abs_storm_ascending$abs.norm), ]

beta.all.abs.vaul <- VAUL_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# ALL # 
FRCH_abs_storm_ascending$DateTime <- as.POSIXct(FRCH_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_abs_storm_ascending$DateTime <- as.POSIXct(MOOS_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_abs_storm_ascending$DateTime <- as.POSIXct(STRT_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_abs_storm_ascending$DateTime <- as.POSIXct(VAUL_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_abs_storm_ascending$DateTime <- as.POSIXct(POKE_abs_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_abs_storm<- rbind(FRCH_abs_storm_ascending, MOOS_abs_storm_ascending,
                       POKE_abs_storm_ascending, VAUL_abs_storm_ascending,
                       STRT_abs_storm_ascending)

beta.all.abs <- All_abs_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


beta.all.abs$response_var <- "abs"

all.2020.ci.abs <- All_abs_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(abs.norm ~ Q.norm, data = .x)))

all.2020.ci.abs$response_var <- "abs"


beta.all.2020 <- rbind(all.2020.ci.no3, all.2020.ci.fDOM,
                       all.2020.ci.SPC, all.2020.ci.turb,
                       all.2020.ci.abs)

write.csv(beta.all.2020, here("Output_from_analysis", "06_BETA", "beta.2020.csv"))

beta.all.2020 <- beta.all.2020 %>% 
  filter(Parameter != "(Intercept)")


########################################## 2021 ##########################################################
setwd("Storm_Events/2021")
storm_file_list_beta <- list.files(path="Test_2/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("Test_2//", storm_file_list_beta, replacement = "")

storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta


#  organize storm data by site and solute 
CARI_storm_list_beta = storm_list_beta[c(1:50)] # 50
FRCH_storm_list_beta = storm_list_beta[c(51:104)] 
MOOS_storm_list_beta = storm_list_beta[c(1:48)] #48 
POKE_storm_list_beta = storm_list_beta[c(165:224)] 
STRT_storm_list_beta = storm_list_beta[c(225:254)] 
VAUL_storm_list_beta = storm_list_beta[c(49:84)] #36


CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("turb", names(FRCH_storm_list_beta)))]
FRCH_abs_storm_list_beta = FRCH_storm_list_beta[c(grep("abs", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("turb", names(MOOS_storm_list_beta)))]
MOOS_abs_storm_list_beta = MOOS_storm_list_beta[c(grep("abs", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

POKE_NO3_storm_list_beta = POKE_storm_list_beta[c(grep("NO3", names(POKE_storm_list_beta)))]
POKE_fDOM_storm_list_beta = POKE_storm_list_beta[c(grep("fDOM", names(POKE_storm_list_beta)))]
POKE_SpCond_storm_list_beta = POKE_storm_list_beta[c(grep("SPC", names(POKE_storm_list_beta)))]
POKE_turb_storm_list_beta = POKE_storm_list_beta[c(grep("turb", names(POKE_storm_list_beta)))]
POKE_abs_storm_list_beta = POKE_storm_list_beta[c(grep("abs", names(POKE_storm_list_beta)))]
POKE_Q_storm_list_beta = POKE_storm_list_beta[c(grep("Q", names(POKE_storm_list_beta)))]

STRT_NO3_storm_list_beta = STRT_storm_list_beta[c(grep("NO3", names(STRT_storm_list_beta)))]
STRT_fDOM_storm_list_beta = STRT_storm_list_beta[c(grep("fDOM", names(STRT_storm_list_beta)))]
STRT_SpCond_storm_list_beta = STRT_storm_list_beta[c(grep("SPC", names(STRT_storm_list_beta)))]
STRT_turb_storm_list_beta = STRT_storm_list_beta[c(grep("turb", names(STRT_storm_list_beta)))]
STRT_abs_storm_list_beta = STRT_storm_list_beta[c(grep("abs", names(STRT_storm_list_beta)))]
STRT_Q_storm_list_beta = STRT_storm_list_beta[c(grep("Q", names(STRT_storm_list_beta)))]

VAUL_NO3_storm_list_beta = VAUL_storm_list_beta[c(grep("NO3", names(VAUL_storm_list_beta)))]
VAUL_fDOM_storm_list_beta = VAUL_storm_list_beta[c(grep("fDOM", names(VAUL_storm_list_beta)))]
VAUL_SpCond_storm_list_beta = VAUL_storm_list_beta[c(grep("SPC", names(VAUL_storm_list_beta)))]
VAUL_turb_storm_list_beta = VAUL_storm_list_beta[c(grep("turb", names(VAUL_storm_list_beta)))]
VAUL_abs_storm_list_beta = VAUL_storm_list_beta[c(grep("abs", names(VAUL_storm_list_beta)))]
VAUL_Q_storm_list_beta = VAUL_storm_list_beta[c(grep("Q", names(VAUL_storm_list_beta)))]

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

# POKE
for(i in 1:length(POKE_Q_storm_list_beta)){
  POKE_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_Q_storm_list_beta[[i]][["datavalue"]]-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# STRT
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_Q_storm_list_beta[[i]][["datavalue"]]-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# VAUL
for(i in 1:length(VAUL_Q_storm_list_beta)){
  VAUL_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_Q_storm_list_beta[[i]][["datavalue"]]-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# CARI
for(i in 1:length(CARI_Q_storm_list_beta)){
  CARI_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_Q_storm_list_beta[[i]][["datavalue"]]-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# normalize solute data 
#  remove NAs associated with the CARI data due to Q data and solute data being on different temporal resolutions (NO3 on 15 min and Q on 1 min)
# CARI_NO3_storm_list_beta <- lapply(CARI_NO3_storm_list_beta, na.omit) 
# CARI_fDOM_storm_list_beta <- lapply(CARI_fDOM_storm_list_beta, na.omit)
# CARI_SpCond_storm_list_beta <- lapply(CARI_SpCond_storm_list_beta, na.omit)
# CARI_turb_storm_list_beta <- lapply(CARI_turb_storm_list_beta, na.omit)

#NO3
for(i in 1:length(FRCH_NO3_storm_list_beta)){
  FRCH_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list_beta[[i]][["datavalue"]]-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list_beta)){
  MOOS_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list_beta[[i]][["datavalue"]]-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))/
    (max(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE)-min(MOOS_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))
}

for(i in 1:length(POKE_NO3_storm_list_beta)){
  POKE_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list_beta[[i]][["datavalue"]]-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))/
    (max(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE)-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=TRUE))
}

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list_beta[[i]][["datavalue"]]-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list_beta)){
  VAUL_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list_beta[[i]][["datavalue"]]-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_fDOM_storm_list_beta)){
  POKE_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list_beta[[i]][["datavalue"]]-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list_beta[[i]][["datavalue"]]-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list_beta)){
  VAUL_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list_beta[[i]][["datavalue"]]-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_SpCond_storm_list_beta)){
  POKE_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_SpCond_storm_list_beta[[i]][["datavalue"]]-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_SpCond_storm_list_beta[[i]][["datavalue"]]-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SpCond_storm_list_beta)){
  VAUL_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_SpCond_storm_list_beta[[i]][["datavalue"]]-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_turb_storm_list_beta)){
  POKE_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list_beta[[i]][["datavalue"]]-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list_beta[[i]][["datavalue"]]-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list_beta)){
  VAUL_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list_beta[[i]][["datavalue"]]-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#ABS
for(i in 1:length(FRCH_abs_storm_list_beta)){
  FRCH_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_abs_storm_list_beta[[i]][["datavalue"]]-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_abs_storm_list_beta)){
  MOOS_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_abs_storm_list_beta[[i]][["datavalue"]]-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_abs_storm_list_beta)){
  POKE_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_abs_storm_list_beta[[i]][["datavalue"]]-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_abs_storm_list_beta)){
  STRT_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_abs_storm_list_beta[[i]][["datavalue"]]-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_abs_storm_list_beta)){
  VAUL_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_abs_storm_list_beta[[i]][["datavalue"]]-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}


###### NO3  #######
FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
POKE_NO3_storm <- map2_df(POKE_Q_storm_list_beta, POKE_NO3_storm_list_beta, inner_join, by = "valuedatetime")
STRT_NO3_storm <- map2_df(STRT_Q_storm_list_beta, STRT_NO3_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_NO3_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm2", 303),
                          rep("storm3", 207),
                          rep("storm4", 223),
                          rep("storm5a", 183),
                          rep("storm5b", 259),
                          rep("storm6a", 111),
                          rep("storm6b", 311),
                          rep("storm7", 139),
                          rep("storm8", 467))

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
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 191),
                            rep("storm2", 251),
                            rep("storm3a", 115),
                            rep("storm3b", 359),
                            rep("storm4a", 415),
                            
                            rep("storm5a", 315),
                            
                            rep("storm6", 127),
                            rep("storm7", 259))

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

# POKE # 
POKE_NO3_storm$storm.ID = c(rep("storm1", 235),
                            rep("storm2", 191),
                            rep("storm3", 167),
                            rep("storm4", 191),
                            rep("storm5", 367),
                            rep("storm6", 159),
                            rep("storm7a", 451),
                            rep("storm7b", 263),
                            rep("storm7c", 99),
                            rep("storm7d", 147))

names(POKE_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
POKE_NO3_storm$site.ID <- "POKE"

POKE_NO3_storm[cols] <- log(POKE_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_NO3_storm <- POKE_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_NO3_storm_ascending <- filter(POKE_NO3_storm, limb == "ascending")

POKE_NO3_storm_ascending <- POKE_NO3_storm_ascending[is.finite(POKE_NO3_storm_ascending$Q.norm) & is.finite(POKE_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.poke.with.all <- POKE_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_NO3_storm$storm.ID = c(rep("storm1a", 191),
                            rep("storm1b", 255),
                            rep("storm2a", 95),
                            rep("storm2b", 211),
                            rep("storm3", 127))

names(STRT_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
STRT_NO3_storm$site.ID <- "STRT"

STRT_NO3_storm[cols] <- log(STRT_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_NO3_storm <- STRT_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_NO3_storm_ascending <- filter(STRT_NO3_storm, limb == "ascending")

STRT_NO3_storm_ascending <- STRT_NO3_storm_ascending[is.finite(STRT_NO3_storm_ascending$Q.norm) & is.finite(STRT_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.strt <- STRT_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_NO3_storm$storm.ID = c(rep("storm1a", 375),
                            rep("storm1b", 267),
                            
                            rep("storm3", 667),
                            rep("storm4a", 427),
                            rep("storm4b", 319),
                            rep("storm5a", 715))

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm1", 167),
                            rep("storm2", 139),
                            rep("storm3", 159),
                            rep("storm4", 127),
                            rep("storm5", 395),
                            rep("storm6", 395),
                            rep("storm7", 447),
                            rep("storm8", 323),
                            rep("storm9", 107),
                            rep("storm10", 243))


names(CARI_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
CARI_NO3_storm$site.ID <- "CARI"

CARI_NO3_storm[cols] <- log(CARI_NO3_storm[cols]) # making concentrations and Q log transformed

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

# ALL # 
FRCH_NO3_storm_ascending$DateTime <- as.POSIXct(FRCH_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_NO3_storm_ascending$DateTime <- as.POSIXct(MOOS_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
STRT_NO3_storm_ascending$DateTime <- as.POSIXct(STRT_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_NO3_storm_ascending$DateTime <- as.POSIXct(VAUL_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_NO3_storm_ascending$DateTime <- as.POSIXct(CARI_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_NO3_storm_ascending$DateTime <- as.POSIXct(POKE_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       STRT_NO3_storm_ascending, VAUL_NO3_storm_ascending, 
                       CARI_NO3_storm_ascending, POKE_NO3_storm_ascending)

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

all.2021.ci.no3 <- All_NO3_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2021.ci.no3$response_var <- "NO3"


##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
POKE_fDOM_storm <- map2_df(POKE_Q_storm_list_beta, POKE_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
STRT_fDOM_storm <- map2_df(STRT_Q_storm_list_beta, STRT_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_fDOM_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
CARI_fDOM_storm <- map2_df(CARI_Q_storm_list_beta, CARI_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm2", 303),
                             rep("storm3", 207),
                             rep("storm4", 223),
                             rep("storm5a", 183),
                             rep("storm5b", 259),
                             rep("storm6a", 111),
                             rep("storm6b", 311),
                             rep("storm7", 139),
                             rep("storm8", 467))

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
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 191),
                             rep("storm2", 251),
                             rep("storm3a", 115),
                             rep("storm3b", 359),
                             rep("storm4a", 415),
                             
                             rep("storm5a", 315),
                             
                             rep("storm6", 127),
                             rep("storm7", 259))

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

# POKE # 
POKE_fDOM_storm$storm.ID = c(rep("storm1", 235),
                             rep("storm2", 191),
                             rep("storm3", 167),
                             rep("storm4", 191),
                             rep("storm5", 367),
                             rep("storm6", 159),
                             rep("storm7a", 451),
                             rep("storm7b", 263),
                             rep("storm7c", 99),
                             rep("storm7d", 147))

names(POKE_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
POKE_fDOM_storm$site.ID <- "POKE"

POKE_fDOM_storm[cols] <- log(POKE_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_fDOM_storm <- POKE_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_fDOM_storm_ascending <- filter(POKE_fDOM_storm, limb == "ascending")

POKE_fDOM_storm_ascending <- POKE_fDOM_storm_ascending[is.finite(POKE_fDOM_storm_ascending$Q.norm) & is.finite(POKE_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.poke.with.all <- POKE_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_fDOM_storm$storm.ID = c(rep("storm1a", 191),
                             rep("storm1b", 255),
                             rep("storm2a", 95),
                             rep("storm2b", 211),
                             rep("storm3", 127))

names(STRT_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
STRT_fDOM_storm$site.ID <- "STRT"

STRT_fDOM_storm[cols] <- log(STRT_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_fDOM_storm <- STRT_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_fDOM_storm_ascending <- filter(STRT_fDOM_storm, limb == "ascending")

STRT_fDOM_storm_ascending <- STRT_fDOM_storm_ascending[is.finite(STRT_fDOM_storm_ascending$Q.norm) & is.finite(STRT_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.strt <- STRT_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_fDOM_storm$storm.ID = c(rep("storm1a", 375),
                             rep("storm1b", 267),
                             
                             rep("storm3", 667),
                             rep("storm4a", 427),
                             rep("storm4b", 319),
                             rep("storm5a", 715))

names(VAUL_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
VAUL_fDOM_storm$site.ID <- "VAUL"

VAUL_fDOM_storm[cols] <- log(VAUL_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_fDOM_storm <- VAUL_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_fDOM_storm_ascending <- filter(VAUL_fDOM_storm, limb == "ascending")

VAUL_fDOM_storm_ascending <- VAUL_fDOM_storm_ascending[is.finite(VAUL_fDOM_storm_ascending$Q.norm) & is.finite(VAUL_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.vaul <- VAUL_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_fDOM_storm$storm.ID = c(rep("storm1", 167),
                             rep("storm2", 139),
                             rep("storm3", 159),
                             rep("storm4", 127),
                             rep("storm5", 395),
                             rep("storm6", 395),
                             rep("storm7", 447),
                             rep("storm8", 323),
                             rep("storm9", 107),
                             rep("storm10", 243))


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

beta.all.fDOM.cari <- CARI_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


# ALL # 
FRCH_fDOM_storm_ascending$DateTime <- as.POSIXct(FRCH_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_fDOM_storm_ascending$DateTime <- as.POSIXct(MOOS_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_fDOM_storm_ascending$DateTime <- as.POSIXct(STRT_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_fDOM_storm_ascending$DateTime <- as.POSIXct(VAUL_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

CARI_fDOM_storm_ascending$DateTime <- as.POSIXct(CARI_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_fDOM_storm_ascending$DateTime <- as.POSIXct(POKE_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                        STRT_fDOM_storm_ascending, VAUL_fDOM_storm_ascending, 
                        CARI_fDOM_storm_ascending, POKE_fDOM_storm_ascending)

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

all.2021.ci.fDOM <- All_fDOM_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(fDOM.norm ~ Q.norm, data = .x)))

all.2021.ci.fDOM$response_var <- "fDOM"


##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
POKE_SPC_storm <- map2_df(POKE_Q_storm_list_beta, POKE_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
STRT_SPC_storm <- map2_df(STRT_Q_storm_list_beta, STRT_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_SPC_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
CARI_SPC_storm <- map2_df(CARI_Q_storm_list_beta, CARI_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm2", 303),
                            rep("storm3", 207),
                            rep("storm4", 223),
                            rep("storm5a", 183),
                            rep("storm5b", 259),
                            rep("storm6a", 111),
                            rep("storm6b", 311),
                            rep("storm7", 139),
                            rep("storm8", 467))

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
MOOS_SPC_storm$storm.ID = c(rep("storm1", 191),
                            rep("storm2", 251),
                            rep("storm3a", 115),
                            rep("storm3b", 359),
                            rep("storm4a", 415),
                            
                            rep("storm5a", 315),
                            
                            rep("storm6", 127),
                            rep("storm7", 259))

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

# POKE # 
POKE_SPC_storm$storm.ID = c(rep("storm1", 235),
                            rep("storm2", 191),
                            rep("storm3", 167),
                            rep("storm4", 191),
                            rep("storm5", 367),
                            rep("storm6", 159),
                            rep("storm7a", 451),
                            rep("storm7b", 263),
                            rep("storm7c", 99),
                            rep("storm7d", 147))

names(POKE_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
POKE_SPC_storm$site.ID <- "POKE"

POKE_SPC_storm[cols] <- log(POKE_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_SPC_storm <- POKE_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_SPC_storm_ascending <- filter(POKE_SPC_storm, limb == "ascending")

POKE_SPC_storm_ascending <- POKE_SPC_storm_ascending[is.finite(POKE_SPC_storm_ascending$Q.norm) & is.finite(POKE_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.poke.with.all <- POKE_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_SPC_storm$storm.ID = c(rep("storm1a", 191),
                            rep("storm1b", 255),
                            rep("storm2a", 95),
                            rep("storm2b", 211),
                            rep("storm3", 127))

names(STRT_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
STRT_SPC_storm$site.ID <- "STRT"

STRT_SPC_storm[cols] <- log(STRT_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_SPC_storm <- STRT_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_SPC_storm_ascending <- filter(STRT_SPC_storm, limb == "ascending")

STRT_SPC_storm_ascending <- STRT_SPC_storm_ascending[is.finite(STRT_SPC_storm_ascending$Q.norm) & is.finite(STRT_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.strt <- STRT_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_SPC_storm$storm.ID = c(rep("storm1a", 375),
                            rep("storm1b", 267),
                            
                            rep("storm3", 667),
                            rep("storm4a", 427),
                            rep("storm4b", 319),
                            rep("storm5a", 715))

names(VAUL_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
VAUL_SPC_storm$site.ID <- "VAUL"

VAUL_SPC_storm[cols] <- log(VAUL_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_SPC_storm <- VAUL_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_SPC_storm_ascending <- filter(VAUL_SPC_storm, limb == "ascending")

VAUL_SPC_storm_ascending <- VAUL_SPC_storm_ascending[is.finite(VAUL_SPC_storm_ascending$Q.norm) & is.finite(VAUL_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.vaul <- VAUL_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# CARI #
CARI_SPC_storm$storm.ID = c(rep("storm1", 167),
                            rep("storm2", 139),
                            rep("storm3", 159),
                            rep("storm4", 127),
                            rep("storm5", 395),
                            rep("storm6", 395),
                            rep("storm7", 447),
                            rep("storm8", 323),
                            rep("storm9", 107),
                            rep("storm10", 243))

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

beta.all.SPC.cari <- CARI_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


# ALL # 
FRCH_SPC_storm_ascending$DateTime <- as.POSIXct(FRCH_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_SPC_storm_ascending$DateTime <- as.POSIXct(MOOS_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_SPC_storm_ascending$DateTime <- as.POSIXct(STRT_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_SPC_storm_ascending$DateTime <- as.POSIXct(VAUL_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

CARI_SPC_storm_ascending$DateTime <- as.POSIXct(CARI_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_SPC_storm_ascending$DateTime <- as.POSIXct(POKE_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending, 
                       STRT_SPC_storm_ascending, VAUL_SPC_storm_ascending,
                       CARI_SPC_storm_ascending, POKE_SPC_storm_ascending)

beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

all.2021.ci.SPC <- All_SPC_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(SPC.norm ~ Q.norm, data = .x)))

all.2021.ci.SPC$response_var <- "SPC"


##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
POKE_turb_storm <- map2_df(POKE_Q_storm_list_beta, POKE_turb_storm_list_beta, inner_join, by = "valuedatetime")
STRT_turb_storm <- map2_df(STRT_Q_storm_list_beta, STRT_turb_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_turb_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_turb_storm_list_beta, inner_join, by = "valuedatetime")
CARI_turb_storm <- map2_df(CARI_Q_storm_list_beta, CARI_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm2", 303),
                             rep("storm3", 207),
                             rep("storm4", 223),
                             rep("storm5a", 183),
                             rep("storm5b", 259),
                             rep("storm6a", 111),
                             rep("storm6b", 311),
                             rep("storm7", 139),
                             rep("storm8", 467))

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
MOOS_turb_storm$storm.ID = c(rep("storm1", 191),
                             rep("storm2", 251),
                             rep("storm3a", 115),
                             rep("storm3b", 359),
                             rep("storm4a", 415),
                             
                             rep("storm5a", 315),
                             
                             rep("storm6", 127),
                             rep("storm7", 259))

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

# POKE # 
POKE_turb_storm$storm.ID = c(rep("storm1", 235),
                             rep("storm2", 191),
                             rep("storm3", 167),
                             rep("storm4", 191),
                             rep("storm5", 367),
                             rep("storm6", 159),
                             rep("storm7a", 451),
                             rep("storm7b", 263),
                             rep("storm7c", 99),
                             rep("storm7d", 147))

names(POKE_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
POKE_turb_storm$site.ID <- "POKE"

POKE_turb_storm[cols] <- log(POKE_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_turb_storm <- POKE_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_turb_storm_ascending <- filter(POKE_turb_storm, limb == "ascending")

POKE_turb_storm_ascending <- POKE_turb_storm_ascending[is.finite(POKE_turb_storm_ascending$Q.norm) & is.finite(POKE_turb_storm_ascending$turb.norm), ]

beta.all.poke.moos.with.all <- POKE_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_turb_storm$storm.ID = c(rep("storm1a", 191),
                             rep("storm1b", 255),
                             rep("storm2a", 95),
                             rep("storm2b", 211),
                             rep("storm3", 127))

names(STRT_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
STRT_turb_storm$site.ID <- "STRT"

STRT_turb_storm[cols] <- log(STRT_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_turb_storm <- STRT_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_turb_storm_ascending <- filter(STRT_turb_storm, limb == "ascending")

STRT_turb_storm_ascending <- STRT_turb_storm_ascending[is.finite(STRT_turb_storm_ascending$Q.norm) & is.finite(STRT_turb_storm_ascending$turb.norm), ]

beta.all.turb.strt <- STRT_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_turb_storm$storm.ID = c(rep("storm1a", 375),
                             rep("storm1b", 267),
                             
                             rep("storm3", 667),
                             rep("storm4a", 427),
                             rep("storm4b", 319),
                             rep("storm5a", 715))

names(VAUL_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
VAUL_turb_storm$site.ID <- "VAUL"

VAUL_turb_storm[cols] <- log(VAUL_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_turb_storm <- VAUL_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_turb_storm_ascending <- filter(VAUL_turb_storm, limb == "ascending")

VAUL_turb_storm_ascending <- VAUL_turb_storm_ascending[is.finite(VAUL_turb_storm_ascending$Q.norm) & is.finite(VAUL_turb_storm_ascending$turb.norm), ]

beta.all.turb.vaul <- VAUL_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# CARI #
CARI_turb_storm$storm.ID = c(rep("storm1", 167),
                             rep("storm2", 139),
                             rep("storm3", 159),
                             rep("storm4", 127),
                             rep("storm5", 395),
                             rep("storm6", 395),
                             rep("storm7", 447),
                             rep("storm8", 323),
                             rep("storm9", 107),
                             rep("storm10", 243))

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

# ALL # 
FRCH_turb_storm_ascending$DateTime <- as.POSIXct(FRCH_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_turb_storm_ascending$DateTime <- as.POSIXct(MOOS_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_turb_storm_ascending$DateTime <- as.POSIXct(STRT_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_turb_storm_ascending$DateTime <- as.POSIXct(VAUL_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_turb_storm_ascending$DateTime <- as.POSIXct(CARI_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_turb_storm_ascending$DateTime <- as.POSIXct(POKE_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm <- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending, 
                        STRT_turb_storm_ascending, VAUL_turb_storm_ascending,
                        CARI_turb_storm_ascending, POKE_turb_storm_ascending)

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

all.2021.ci.turb <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2021.ci.turb$response_var <- "turb"

##### ABS #####
FRCH_abs_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_abs_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_abs_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_abs_storm_list_beta, inner_join, by = "valuedatetime")
POKE_abs_storm <- map2_df(POKE_Q_storm_list_beta, POKE_abs_storm_list_beta, inner_join, by = "valuedatetime")
STRT_abs_storm <- map2_df(STRT_Q_storm_list_beta, STRT_abs_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_abs_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_abs_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_abs_storm$storm.ID = c(rep("storm2", 303),
                            rep("storm3", 207),
                            rep("storm4", 223),
                            rep("storm5a", 183),
                            rep("storm5b", 259),
                            rep("storm6a", 111),
                            rep("storm6b", 311),
                            rep("storm7", 139),
                            rep("storm8", 467))

names(FRCH_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
FRCH_abs_storm$site.ID <- "FRCH"

cols <- c("abs.norm","Q.norm")
FRCH_abs_storm[cols] <- log(FRCH_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_abs_storm <- FRCH_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_abs_storm_ascending <- filter(FRCH_abs_storm, limb == "ascending")

FRCH_abs_storm_ascending <- FRCH_abs_storm_ascending[is.finite(FRCH_abs_storm_ascending$Q.norm) & is.finite(FRCH_abs_storm_ascending$abs.norm), ]

beta.all.abs <- FRCH_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_abs_storm$storm.ID = c(rep("storm1", 191),
                            rep("storm2", 251),
                            rep("storm3a", 115),
                            rep("storm3b", 359),
                            rep("storm4a", 415),
                            
                            rep("storm5a", 315),
                            
                            rep("storm6", 127),
                            rep("storm7", 259))

names(MOOS_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
MOOS_abs_storm$site.ID <- "MOOS"

MOOS_abs_storm[cols] <- log(MOOS_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_abs_storm <- MOOS_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_abs_storm_ascending <- filter(MOOS_abs_storm, limb == "ascending")

MOOS_abs_storm_ascending <- MOOS_abs_storm_ascending[is.finite(MOOS_abs_storm_ascending$Q.norm) & is.finite(MOOS_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- MOOS_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_abs_storm$storm.ID = c(rep("storm1", 235),
                            rep("storm2", 191),
                            rep("storm3", 167),
                            rep("storm4", 191),
                            rep("storm5", 367),
                            rep("storm6", 159),
                            rep("storm7a", 451),
                            rep("storm7b", 263),
                            rep("storm7c", 99),
                            rep("storm7d", 147))

names(POKE_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
POKE_abs_storm$site.ID <- "POKE"

POKE_abs_storm[cols] <- log(POKE_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_abs_storm <- POKE_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_abs_storm_ascending <- filter(POKE_abs_storm, limb == "ascending")

POKE_abs_storm_ascending <- POKE_abs_storm_ascending[is.finite(POKE_abs_storm_ascending$Q.norm) & is.finite(POKE_abs_storm_ascending$abs.norm), ]

beta.all.poke.moos.with.all <- POKE_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_abs_storm$storm.ID = c(rep("storm1a", 191),
                            rep("storm1b", 255),
                            rep("storm2a", 95),
                            rep("storm2b", 211),
                            rep("storm3", 127))

names(STRT_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
STRT_abs_storm$site.ID <- "STRT"

STRT_abs_storm[cols] <- log(STRT_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_abs_storm <- STRT_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_abs_storm_ascending <- filter(STRT_abs_storm, limb == "ascending")

STRT_abs_storm_ascending <- STRT_abs_storm_ascending[is.finite(STRT_abs_storm_ascending$Q.norm) & is.finite(STRT_abs_storm_ascending$abs.norm), ]

beta.all.abs.strt <- STRT_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_abs_storm$storm.ID = c(rep("storm1a", 375),
                            rep("storm1b", 267),
                            
                            rep("storm3", 667),
                            rep("storm4a", 427),
                            rep("storm4b", 319),
                            rep("storm5a", 715))

names(VAUL_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
VAUL_abs_storm$site.ID <- "VAUL"

VAUL_abs_storm[cols] <- log(VAUL_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_abs_storm <- VAUL_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_abs_storm_ascending <- filter(VAUL_abs_storm, limb == "ascending")

VAUL_abs_storm_ascending <- VAUL_abs_storm_ascending[is.finite(VAUL_abs_storm_ascending$Q.norm) & is.finite(VAUL_abs_storm_ascending$abs.norm), ]

beta.all.abs.vaul <- VAUL_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# ALL # 
FRCH_abs_storm_ascending$DateTime <- as.POSIXct(FRCH_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_abs_storm_ascending$DateTime <- as.POSIXct(MOOS_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_abs_storm_ascending$DateTime <- as.POSIXct(STRT_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_abs_storm_ascending$DateTime <- as.POSIXct(VAUL_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_abs_storm_ascending$DateTime <- as.POSIXct(POKE_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_abs_storm <- rbind(FRCH_abs_storm_ascending, MOOS_abs_storm_ascending, 
                       STRT_abs_storm_ascending, VAUL_abs_storm_ascending,
                       POKE_abs_storm_ascending)

beta.all.abs <- All_abs_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


beta.all.abs$response_var <- "abs"

all.2021.ci.abs <- All_abs_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(abs.norm ~ Q.norm, data = .x)))

all.2021.ci.abs$response_var <- "abs"


beta.all.2021 <- rbind(all.2021.ci.no3, all.2021.ci.fDOM,
                       all.2021.ci.SPC, all.2021.ci.turb,
                       all.2021.ci.abs)

write.csv(here("Storms_clean_repo", "Output_from_analysis", "06_BETA", "beta.2021.csv"))

# write.csv(beta.all.2021, "~/Documents/Storms_clean_repo/Output_from_analysis/06_BETA/beta.2021.csv")

# beta.all.2019 <- beta.all.2019 %>% 
#   filter(Parameter != "(Intercept)")




################################### 2022 ################################################
setwd("Storm_Events/2022")
storm_file_list_beta <- list.files(path="FRCH_MOOS_VAUL_POKE_STRT_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

# storm_file_list_beta <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/FRCH_MOOS_VAUL_POKE_STRT_CARI/", 
#                                    recursive=F, 
#                                    pattern=".csv", 
#                                    full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("FRCH_MOOS_VAUL_POKE_STRT_CARI//", storm_file_list_beta, replacement = "")
# storm_file_list_beta = sub("~/Documents/Storms_clean_repo/Storm_Events/2022/FRCH_MOOS_VAUL_POKE_STRT_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 

#  organize storm data by site and solute 
CARI_storm_list_beta = storm_list_beta[c(1:45)] #45
FRCH_storm_list_beta = storm_list_beta[c(46:69)] #24
MOOS_storm_list_beta = storm_list_beta[c(70:99)] #30
POKE_storm_list_beta = storm_list_beta[c(100:123)]# 24
STRT_storm_list_beta = storm_list_beta[c(124:141)] #18
VAUL_storm_list_beta = storm_list_beta[c(142:153)] #12

FRCH_NO3_storm_list_beta = FRCH_storm_list_beta[c(grep("NO3", names(FRCH_storm_list_beta)))]
FRCH_fDOM_storm_list_beta = FRCH_storm_list_beta[c(grep("fDOM", names(FRCH_storm_list_beta)))]
FRCH_SpCond_storm_list_beta = FRCH_storm_list_beta[c(grep("SPC", names(FRCH_storm_list_beta)))]
FRCH_turb_storm_list_beta = FRCH_storm_list_beta[c(grep("turb", names(FRCH_storm_list_beta)))]
FRCH_abs_storm_list_beta = FRCH_storm_list_beta[c(grep("abs", names(FRCH_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]

MOOS_NO3_storm_list_beta = MOOS_storm_list_beta[c(grep("NO3", names(MOOS_storm_list_beta)))]
MOOS_fDOM_storm_list_beta = MOOS_storm_list_beta[c(grep("fDOM", names(MOOS_storm_list_beta)))]
MOOS_SpCond_storm_list_beta = MOOS_storm_list_beta[c(grep("SPC", names(MOOS_storm_list_beta)))]
MOOS_turb_storm_list_beta = MOOS_storm_list_beta[c(grep("turb", names(MOOS_storm_list_beta)))]
MOOS_abs_storm_list_beta = MOOS_storm_list_beta[c(grep("abs", names(MOOS_storm_list_beta)))]
MOOS_Q_storm_list_beta = MOOS_storm_list_beta[c(grep("Q", names(MOOS_storm_list_beta)))]

POKE_NO3_storm_list_beta = POKE_storm_list_beta[c(grep("NO3", names(POKE_storm_list_beta)))]
POKE_fDOM_storm_list_beta = POKE_storm_list_beta[c(grep("fDOM", names(POKE_storm_list_beta)))]
POKE_SpCond_storm_list_beta = POKE_storm_list_beta[c(grep("SPC", names(POKE_storm_list_beta)))]
POKE_turb_storm_list_beta = POKE_storm_list_beta[c(grep("turb", names(POKE_storm_list_beta)))]
POKE_abs_storm_list_beta = POKE_storm_list_beta[c(grep("abs", names(POKE_storm_list_beta)))]
POKE_Q_storm_list_beta = POKE_storm_list_beta[c(grep("Q", names(POKE_storm_list_beta)))]

STRT_NO3_storm_list_beta = STRT_storm_list_beta[c(grep("NO3", names(STRT_storm_list_beta)))]
STRT_fDOM_storm_list_beta = STRT_storm_list_beta[c(grep("fDOM", names(STRT_storm_list_beta)))]
STRT_SpCond_storm_list_beta = STRT_storm_list_beta[c(grep("SPC", names(STRT_storm_list_beta)))]
STRT_turb_storm_list_beta = STRT_storm_list_beta[c(grep("turb", names(STRT_storm_list_beta)))]
STRT_abs_storm_list_beta = STRT_storm_list_beta[c(grep("abs", names(STRT_storm_list_beta)))]
STRT_Q_storm_list_beta = STRT_storm_list_beta[c(grep("Q", names(STRT_storm_list_beta)))]

VAUL_NO3_storm_list_beta = VAUL_storm_list_beta[c(grep("NO3", names(VAUL_storm_list_beta)))]
VAUL_fDOM_storm_list_beta = VAUL_storm_list_beta[c(grep("fDOM", names(VAUL_storm_list_beta)))]
VAUL_SpCond_storm_list_beta = VAUL_storm_list_beta[c(grep("SPC", names(VAUL_storm_list_beta)))]
VAUL_turb_storm_list_beta = VAUL_storm_list_beta[c(grep("turb", names(VAUL_storm_list_beta)))]
VAUL_abs_storm_list_beta = VAUL_storm_list_beta[c(grep("abs", names(VAUL_storm_list_beta)))]
VAUL_Q_storm_list_beta = VAUL_storm_list_beta[c(grep("Q", names(VAUL_storm_list_beta)))]

CARI_NO3_storm_list_beta = CARI_storm_list_beta[c(grep("NO3", names(CARI_storm_list_beta)))]
CARI_fDOM_storm_list_beta = CARI_storm_list_beta[c(grep("fDOM", names(CARI_storm_list_beta)))]
CARI_SpCond_storm_list_beta = CARI_storm_list_beta[c(grep("SPC", names(CARI_storm_list_beta)))]
CARI_turb_storm_list_beta = CARI_storm_list_beta[c(grep("turb", names(CARI_storm_list_beta)))]
CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]

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

# POKE
for(i in 1:length(POKE_Q_storm_list_beta)){
  POKE_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_Q_storm_list_beta[[i]][["datavalue"]]-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# STRT
for(i in 1:length(STRT_Q_storm_list_beta)){
  STRT_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_Q_storm_list_beta[[i]][["datavalue"]]-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

# VAUL
for(i in 1:length(VAUL_Q_storm_list_beta)){
  VAUL_Q_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_Q_storm_list_beta[[i]][["datavalue"]]-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_Q_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_NO3_storm_list_beta)){
  POKE_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_NO3_storm_list_beta[[i]][["datavalue"]]-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_NO3_storm_list_beta)){
  STRT_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_NO3_storm_list_beta[[i]][["datavalue"]]-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_NO3_storm_list_beta)){
  VAUL_NO3_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_NO3_storm_list_beta[[i]][["datavalue"]]-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_NO3_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_fDOM_storm_list_beta)){
  POKE_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_fDOM_storm_list_beta[[i]][["datavalue"]]-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_fDOM_storm_list_beta)){
  STRT_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_fDOM_storm_list_beta[[i]][["datavalue"]]-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_fDOM_storm_list_beta)){
  VAUL_fDOM_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_fDOM_storm_list_beta[[i]][["datavalue"]]-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_fDOM_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_SpCond_storm_list_beta)){
  POKE_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_SpCond_storm_list_beta[[i]][["datavalue"]]-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_SpCond_storm_list_beta)){
  STRT_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_SpCond_storm_list_beta[[i]][["datavalue"]]-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_SpCond_storm_list_beta)){
  VAUL_SpCond_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_SpCond_storm_list_beta[[i]][["datavalue"]]-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_SpCond_storm_list_beta[[i]][["datavalue"]], na.rm=T))
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

for(i in 1:length(POKE_turb_storm_list_beta)){
  POKE_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_turb_storm_list_beta[[i]][["datavalue"]]-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_turb_storm_list_beta)){
  STRT_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_turb_storm_list_beta[[i]][["datavalue"]]-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_turb_storm_list_beta)){
  VAUL_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_turb_storm_list_beta[[i]][["datavalue"]]-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(CARI_turb_storm_list_beta)){
  CARI_turb_storm_list_beta[[i]][["datavalue.norm"]] = 
    (CARI_turb_storm_list_beta[[i]][["datavalue"]]-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(CARI_turb_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

#abs
for(i in 1:length(FRCH_abs_storm_list_beta)){
  FRCH_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (FRCH_abs_storm_list_beta[[i]][["datavalue"]]-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(FRCH_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_abs_storm_list_beta)){
  MOOS_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (MOOS_abs_storm_list_beta[[i]][["datavalue"]]-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(MOOS_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(POKE_abs_storm_list_beta)){
  POKE_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (POKE_abs_storm_list_beta[[i]][["datavalue"]]-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(POKE_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(STRT_abs_storm_list_beta)){
  STRT_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (STRT_abs_storm_list_beta[[i]][["datavalue"]]-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(STRT_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(VAUL_abs_storm_list_beta)){
  VAUL_abs_storm_list_beta[[i]][["datavalue.norm"]] = 
    (VAUL_abs_storm_list_beta[[i]][["datavalue"]]-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))/
    (max(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T)-min(VAUL_abs_storm_list_beta[[i]][["datavalue"]], na.rm=T))
}



###### NO3  #######

FRCH_NO3_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_NO3_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_NO3_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_NO3_storm_list_beta, inner_join, by = "valuedatetime")
POKE_NO3_storm <- map2_df(POKE_Q_storm_list_beta, POKE_NO3_storm_list_beta, inner_join, by = "valuedatetime")
STRT_NO3_storm <- map2_df(STRT_Q_storm_list_beta, STRT_NO3_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_NO3_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_NO3_storm_list_beta, inner_join, by = "valuedatetime")
CARI_NO3_storm <- map2_df(CARI_Q_storm_list_beta, CARI_NO3_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_NO3_storm$storm.ID = c(rep("storm1", 219),
                            rep("storm2", 235),
                            rep("storm3", 223),
                            rep("storm4", 167))
                            

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
  dplyr::summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_NO3_storm$storm.ID = c(rep("storm1", 199),
                            rep("storm2a", 71),
                            rep("storm2b", 151),
                            rep("storm3", 99),
                            rep("storm4", 215))

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

# POKE # 
POKE_NO3_storm$storm.ID = c(rep("storm1", 139),
                            rep("storm2", 119),
                            rep("storm3", 95),
                            rep("storm4", 187))

names(POKE_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
POKE_NO3_storm$site.ID <- "POKE"

POKE_NO3_storm[cols] <- log(POKE_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_NO3_storm <- POKE_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_NO3_storm_ascending <- filter(POKE_NO3_storm, limb == "ascending")

POKE_NO3_storm_ascending <- POKE_NO3_storm_ascending[is.finite(POKE_NO3_storm_ascending$Q.norm) & is.finite(POKE_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.moos.with.all <- POKE_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_NO3_storm$storm.ID = c(rep("storm1", 103),
                            rep("storm2", 191),
                            rep("storm3", 107))

names(STRT_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
STRT_NO3_storm$site.ID <- "STRT"

STRT_NO3_storm[cols] <- log(STRT_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_NO3_storm <- STRT_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_NO3_storm_ascending <- filter(STRT_NO3_storm, limb == "ascending")

STRT_NO3_storm_ascending <- STRT_NO3_storm_ascending[is.finite(STRT_NO3_storm_ascending$Q.norm) & is.finite(STRT_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.strt <- STRT_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_NO3_storm$storm.ID = c(rep("storm1", 127),
                            rep("storm2", 763))

names(VAUL_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
VAUL_NO3_storm$site.ID <- "VAUL"

VAUL_NO3_storm[cols] <- log(VAUL_NO3_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_NO3_storm <- VAUL_NO3_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_NO3_storm_ascending <- filter(VAUL_NO3_storm, limb == "ascending")

VAUL_NO3_storm_ascending <- VAUL_NO3_storm_ascending[is.finite(VAUL_NO3_storm_ascending$Q.norm) & is.finite(VAUL_NO3_storm_ascending$NO3.norm), ]

beta.all.no3.vaul <- VAUL_NO3_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_NO3_storm$storm.ID = c(rep("storm1", 231),
                            rep("storm2", 190),
                            rep("storm3", 204),
                            rep("storm4a", 119),
                            rep("storm4b", 167),
                            rep("storm5", 379),
                            rep("storm6", 91),
                            rep("storm7", 191),
                            rep("storm8", 103))

names(CARI_NO3_storm) <- c("DateTime", "Q", "Q.norm", "NO3", "NO3.norm", "storm.ID")
CARI_NO3_storm$site.ID <- "CARI"

CARI_NO3_storm[cols] <- log(CARI_NO3_storm[cols]) # making concentrations and Q log transformed

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

# ALL # 

FRCH_NO3_storm_ascending$DateTime <- as.POSIXct(FRCH_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_NO3_storm_ascending$DateTime <- as.POSIXct(MOOS_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
STRT_NO3_storm_ascending$DateTime <- as.POSIXct(STRT_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_NO3_storm_ascending$DateTime <- as.POSIXct(VAUL_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_NO3_storm_ascending$DateTime <- as.POSIXct(CARI_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_NO3_storm_ascending$DateTime <- as.POSIXct(POKE_NO3_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")


All_NO3_storm <- rbind(FRCH_NO3_storm_ascending, MOOS_NO3_storm_ascending, 
                       POKE_NO3_storm_ascending, VAUL_NO3_storm_ascending, 
                       STRT_NO3_storm_ascending, CARI_NO3_storm_ascending)

beta.all.no3 <- All_NO3_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, NO3.norm)) # this works just like the beta one that is for an individual site


beta.all.no3$response_var <- "NO3"

all.2022.ci.no3 <- All_NO3_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(NO3.norm ~ Q.norm, data = .x)))

all.2022.ci.no3$response_var <- "NO3"


##### fDOM #####
FRCH_fDOM_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_fDOM_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
POKE_fDOM_storm <- map2_df(POKE_Q_storm_list_beta, POKE_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
STRT_fDOM_storm <- map2_df(STRT_Q_storm_list_beta, STRT_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_fDOM_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_fDOM_storm_list_beta, inner_join, by = "valuedatetime")
CARI_fDOM_storm <- map2_df(CARI_Q_storm_list_beta, CARI_fDOM_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_fDOM_storm$storm.ID = c(rep("storm1", 219),
                             rep("storm2", 235),
                             rep("storm3", 223),
                             rep("storm4", 167))

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
MOOS_fDOM_storm$storm.ID = c(rep("storm1", 199),
                             rep("storm2a", 71),
                             rep("storm2b", 151),
                             rep("storm3", 99),
                             rep("storm4", 215))

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

# POKE # 
POKE_fDOM_storm$storm.ID = c(rep("storm1", 139),
                             rep("storm2", 119),
                             rep("storm3", 95),
                             rep("storm4", 187))

names(POKE_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
POKE_fDOM_storm$site.ID <- "POKE"

POKE_fDOM_storm[cols] <- log(POKE_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_fDOM_storm <- POKE_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_fDOM_storm_ascending <- filter(POKE_fDOM_storm, limb == "ascending")

POKE_fDOM_storm_ascending <- POKE_fDOM_storm_ascending[is.finite(POKE_fDOM_storm_ascending$Q.norm) & is.finite(POKE_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.moos.with.all <- POKE_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_fDOM_storm$storm.ID = c(rep("storm1", 103),
                             rep("storm2", 191),
                             rep("storm3", 107))

names(STRT_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
STRT_fDOM_storm$site.ID <- "STRT"

STRT_fDOM_storm[cols] <- log(STRT_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

STRT_fDOM_storm <- na.omit(STRT_fDOM_storm)

STRT_fDOM_storm <- STRT_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_fDOM_storm_ascending <- filter(STRT_fDOM_storm, limb == "ascending")

STRT_fDOM_storm_ascending <- STRT_fDOM_storm_ascending[is.finite(STRT_fDOM_storm_ascending$Q.norm) & is.finite(STRT_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.strt <- STRT_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_fDOM_storm$storm.ID = c(rep("storm1", 127),
                             rep("storm2", 763))

names(VAUL_fDOM_storm) <- c("DateTime", "Q", "Q.norm", "fDOM", "fDOM.norm", "storm.ID")
VAUL_fDOM_storm$site.ID <- "VAUL"

VAUL_fDOM_storm[cols] <- log(VAUL_fDOM_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_fDOM_storm <- VAUL_fDOM_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_fDOM_storm_ascending <- filter(VAUL_fDOM_storm, limb == "ascending")

VAUL_fDOM_storm_ascending <- VAUL_fDOM_storm_ascending[is.finite(VAUL_fDOM_storm_ascending$Q.norm) & is.finite(VAUL_fDOM_storm_ascending$fDOM.norm), ]

beta.all.fDOM.vaul <- VAUL_fDOM_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_fDOM_storm$storm.ID = c(rep("storm1", 231),
                             rep("storm2", 190),
                             rep("storm3", 204),
                             rep("storm4a", 119),
                             rep("storm4b", 167),
                             rep("storm5", 379),
                             rep("storm6", 91),
                             rep("storm7", 191),
                             rep("storm8", 103))


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


# ALL # 
FRCH_fDOM_storm_ascending$DateTime <- as.POSIXct(FRCH_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_fDOM_storm_ascending$DateTime <- as.POSIXct(MOOS_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
STRT_fDOM_storm_ascending$DateTime <- as.POSIXct(STRT_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_fDOM_storm_ascending$DateTime <- as.POSIXct(VAUL_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_fDOM_storm_ascending$DateTime <- as.POSIXct(CARI_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_fDOM_storm_ascending$DateTime <- as.POSIXct(POKE_fDOM_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_fDOM_storm <- rbind(FRCH_fDOM_storm_ascending, MOOS_fDOM_storm_ascending, 
                        POKE_fDOM_storm_ascending, VAUL_fDOM_storm_ascending,
                        STRT_fDOM_storm_ascending, CARI_fDOM_storm_ascending)

beta.all.fdom <- All_fDOM_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, fDOM.norm)) # this works just like the beta one that is for an individual site


beta.all.fdom$response_var <- "fDOM"

all.2022.ci.fDOM <- All_fDOM_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(fDOM.norm ~ Q.norm, data = .x)))

all.2022.ci.fDOM$response_var <- "fDOM"


##### SPC #####
FRCH_SPC_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_SPC_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
POKE_SPC_storm <- map2_df(POKE_Q_storm_list_beta, POKE_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
STRT_SPC_storm <- map2_df(STRT_Q_storm_list_beta, STRT_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_SPC_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_SpCond_storm_list_beta, inner_join, by = "valuedatetime")
CARI_SPC_storm <- map2_df(CARI_Q_storm_list_beta, CARI_SpCond_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_SPC_storm$storm.ID = c(rep("storm1", 219),
                            rep("storm2", 235),
                            rep("storm3", 223),
                            rep("storm4", 167))

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
MOOS_SPC_storm$storm.ID = c(rep("storm1", 199),
                            rep("storm2a", 71),
                            rep("storm2b", 151),
                            rep("storm3", 99),
                            rep("storm4", 215))

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

# POKE # 
POKE_SPC_storm$storm.ID = c(rep("storm1", 139),
                            rep("storm2", 119),
                            rep("storm3", 95),
                            rep("storm4", 187))

names(POKE_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
POKE_SPC_storm$site.ID <- "POKE"

POKE_SPC_storm[cols] <- log(POKE_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_SPC_storm <- POKE_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_SPC_storm_ascending <- filter(POKE_SPC_storm, limb == "ascending")

POKE_SPC_storm_ascending <- POKE_SPC_storm_ascending[is.finite(POKE_SPC_storm_ascending$Q.norm) & is.finite(POKE_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.moos.with.all <- POKE_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_SPC_storm$storm.ID = c(rep("storm1", 103),
                            rep("storm2", 191),
                            rep("storm3", 107))

names(STRT_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
STRT_SPC_storm$site.ID <- "STRT"

STRT_SPC_storm[cols] <- log(STRT_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_SPC_storm <- STRT_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_SPC_storm_ascending <- filter(STRT_SPC_storm, limb == "ascending")

STRT_SPC_storm_ascending <- STRT_SPC_storm_ascending[is.finite(STRT_SPC_storm_ascending$Q.norm) & is.finite(STRT_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.strt <- STRT_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_SPC_storm$storm.ID = c(rep("storm1", 127),
                            rep("storm2", 763))

names(VAUL_SPC_storm) <- c("DateTime", "Q", "Q.norm", "SPC", "SPC.norm", "storm.ID")
VAUL_SPC_storm$site.ID <- "VAUL"

VAUL_SPC_storm[cols] <- log(VAUL_SPC_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_SPC_storm <- VAUL_SPC_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_SPC_storm_ascending <- filter(VAUL_SPC_storm, limb == "ascending")

VAUL_SPC_storm_ascending <- VAUL_SPC_storm_ascending[is.finite(VAUL_SPC_storm_ascending$Q.norm) & is.finite(VAUL_SPC_storm_ascending$SPC.norm), ]

beta.all.SPC.vaul <- VAUL_SPC_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_SPC_storm$storm.ID = c(rep("storm1", 231),
                            rep("storm2", 190),
                            rep("storm3", 204),
                            rep("storm4a", 119),
                            rep("storm4b", 167),
                            rep("storm5", 379),
                            rep("storm6", 91),
                            rep("storm7", 191),
                            rep("storm8", 103))

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


# ALL # 
FRCH_SPC_storm_ascending$DateTime <- as.POSIXct(FRCH_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_SPC_storm_ascending$DateTime <- as.POSIXct(MOOS_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_SPC_storm_ascending$DateTime <- as.POSIXct(STRT_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_SPC_storm_ascending$DateTime <- as.POSIXct(VAUL_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

CARI_SPC_storm_ascending$DateTime <- as.POSIXct(CARI_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_SPC_storm_ascending$DateTime <- as.POSIXct(POKE_SPC_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_SPC_storm <- rbind(FRCH_SPC_storm_ascending, MOOS_SPC_storm_ascending,
                       POKE_SPC_storm_ascending, VAUL_SPC_storm_ascending,
                       STRT_SPC_storm_ascending, CARI_SPC_storm_ascending)


beta.all.SPC <- All_SPC_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, SPC.norm)) # this works just like the beta one that is for an individual site


beta.all.SPC$response_var <- "SPC"

all.2022.ci.SPC <- All_SPC_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(SPC.norm ~ Q.norm, data = .x)))

all.2022.ci.SPC$response_var <- "SPC"


##### Turb #####
FRCH_turb_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_turb_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_turb_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_turb_storm_list_beta, inner_join, by = "valuedatetime")
POKE_turb_storm <- map2_df(POKE_Q_storm_list_beta, POKE_turb_storm_list_beta, inner_join, by = "valuedatetime")
STRT_turb_storm <- map2_df(STRT_Q_storm_list_beta, STRT_turb_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_turb_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_turb_storm_list_beta, inner_join, by = "valuedatetime")
CARI_turb_storm <- map2_df(CARI_Q_storm_list_beta, CARI_turb_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_turb_storm$storm.ID = c(rep("storm1", 219),
                             rep("storm2", 235),
                             rep("storm3", 223),
                             rep("storm4", 167))

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
MOOS_turb_storm$storm.ID = c(rep("storm1", 199),
                             rep("storm2a", 71),
                             rep("storm2b", 151),
                             rep("storm3", 99),
                             rep("storm4", 215))

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

# POKE # 
POKE_turb_storm$storm.ID = c(rep("storm1", 139),
                             rep("storm2", 119),
                             rep("storm3", 95),
                             rep("storm4", 187))

names(POKE_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
POKE_turb_storm$site.ID <- "POKE"

POKE_turb_storm[cols] <- log(POKE_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_turb_storm <- POKE_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_turb_storm_ascending <- filter(POKE_turb_storm, limb == "ascending")

POKE_turb_storm_ascending <- POKE_turb_storm_ascending[is.finite(POKE_turb_storm_ascending$Q.norm) & is.finite(POKE_turb_storm_ascending$turb.norm), ]

beta.all.turb.moos.with.all <- POKE_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_turb_storm$storm.ID = c(rep("storm1", 103),
                             rep("storm2", 191),
                             rep("storm3", 107))

names(STRT_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
STRT_turb_storm$site.ID <- "STRT"

STRT_turb_storm[cols] <- log(STRT_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_turb_storm <- STRT_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_turb_storm_ascending <- filter(STRT_turb_storm, limb == "ascending")

STRT_turb_storm_ascending <- STRT_turb_storm_ascending[is.finite(STRT_turb_storm_ascending$Q.norm) & is.finite(STRT_turb_storm_ascending$turb.norm), ]

beta.all.turb.strt <- STRT_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_turb_storm$storm.ID = c(rep("storm1", 127),
                             rep("storm2", 763))

names(VAUL_turb_storm) <- c("DateTime", "Q", "Q.norm", "turb", "turb.norm", "storm.ID")
VAUL_turb_storm$site.ID <- "VAUL"

VAUL_turb_storm[cols] <- log(VAUL_turb_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_turb_storm <- VAUL_turb_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_turb_storm_ascending <- filter(VAUL_turb_storm, limb == "ascending")

VAUL_turb_storm_ascending <- VAUL_turb_storm_ascending[is.finite(VAUL_turb_storm_ascending$Q.norm) & is.finite(VAUL_turb_storm_ascending$turb.norm), ]

beta.all.turb.vaul <- VAUL_turb_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site

# CARI # 
CARI_turb_storm$storm.ID = c(rep("storm1", 231),
                             rep("storm2", 190),
                             rep("storm3", 204),
                             rep("storm4a", 119),
                             rep("storm4b", 167),
                             rep("storm5", 379),
                             rep("storm6", 91),
                             rep("storm7", 191),
                             rep("storm8", 103))

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


# ALL # 
FRCH_turb_storm_ascending$DateTime <- as.POSIXct(FRCH_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_turb_storm_ascending$DateTime <- as.POSIXct(MOOS_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_turb_storm_ascending$DateTime <- as.POSIXct(STRT_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_turb_storm_ascending$DateTime <- as.POSIXct(VAUL_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
CARI_turb_storm_ascending$DateTime <- as.POSIXct(CARI_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_turb_storm_ascending$DateTime <- as.POSIXct(POKE_turb_storm_ascending$DateTime, 
                                                 "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_turb_storm<- rbind(FRCH_turb_storm_ascending, MOOS_turb_storm_ascending,
                       POKE_turb_storm_ascending, VAUL_turb_storm_ascending,
                       STRT_turb_storm_ascending, CARI_turb_storm_ascending )

beta.all.turb <- All_turb_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, turb.norm)) # this works just like the beta one that is for an individual site


beta.all.turb$response_var <- "turb"

all.2022.ci.turb <- All_turb_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(turb.norm ~ Q.norm, data = .x)))

all.2022.ci.turb$response_var <- "turb"

### ABS ####

FRCH_abs_storm <- map2_df(FRCH_Q_storm_list_beta, FRCH_abs_storm_list_beta, inner_join, by = "valuedatetime")
MOOS_abs_storm <- map2_df(MOOS_Q_storm_list_beta, MOOS_abs_storm_list_beta, inner_join, by = "valuedatetime")
POKE_abs_storm <- map2_df(POKE_Q_storm_list_beta, POKE_abs_storm_list_beta, inner_join, by = "valuedatetime")
STRT_abs_storm <- map2_df(STRT_Q_storm_list_beta, STRT_abs_storm_list_beta, inner_join, by = "valuedatetime")
VAUL_abs_storm <- map2_df(VAUL_Q_storm_list_beta, VAUL_abs_storm_list_beta, inner_join, by = "valuedatetime")

FRCH_abs_storm$storm.ID = c(rep("storm1", 219),
                            rep("storm2", 235),
                            rep("storm3", 223),
                            rep("storm4", 167))

names(FRCH_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
FRCH_abs_storm$site.ID <- "FRCH"

cols <- c("abs.norm","Q.norm")
FRCH_abs_storm[cols] <- log(FRCH_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
FRCH_abs_storm <- FRCH_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

FRCH_abs_storm_ascending <- filter(FRCH_abs_storm, limb == "ascending")

FRCH_abs_storm_ascending <- FRCH_abs_storm_ascending[is.finite(FRCH_abs_storm_ascending$Q.norm) & is.finite(FRCH_abs_storm_ascending$abs.norm), ]

beta.all.abs <- FRCH_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# MOOS # 
MOOS_abs_storm$storm.ID = c(rep("storm1", 199),
                            rep("storm2a", 71),
                            rep("storm2b", 151),
                            rep("storm3", 99),
                            rep("storm4", 215))

names(MOOS_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
MOOS_abs_storm$site.ID <- "MOOS"

MOOS_abs_storm[cols] <- log(MOOS_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
MOOS_abs_storm <- MOOS_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

MOOS_abs_storm_ascending <- filter(MOOS_abs_storm, limb == "ascending")

MOOS_abs_storm_ascending <- MOOS_abs_storm_ascending[is.finite(MOOS_abs_storm_ascending$Q.norm) & is.finite(MOOS_abs_storm_ascending$abs.norm), ]

beta.all.abs.moos.with.all <- MOOS_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# POKE # 
POKE_abs_storm$storm.ID = c(rep("storm1", 139),
                            rep("storm2", 119),
                            rep("storm3", 95),
                            rep("storm4", 187))

names(POKE_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
POKE_abs_storm$site.ID <- "POKE"

POKE_abs_storm[cols] <- log(POKE_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
POKE_abs_storm <- POKE_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

POKE_abs_storm_ascending <- filter(POKE_abs_storm, limb == "ascending")

POKE_abs_storm_ascending <- POKE_abs_storm_ascending[is.finite(POKE_abs_storm_ascending$Q.norm) & is.finite(POKE_abs_storm_ascending$abs.norm), ]

beta.all.poke.moos.with.all <- POKE_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# STRT # 
STRT_abs_storm$storm.ID = c(rep("storm1", 103),
                            rep("storm2", 191),
                            rep("storm3", 107))

names(STRT_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
STRT_abs_storm$site.ID <- "STRT"

STRT_abs_storm[cols] <- log(STRT_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
STRT_abs_storm <- STRT_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

STRT_abs_storm_ascending <- filter(STRT_abs_storm, limb == "ascending")

STRT_abs_storm_ascending <- STRT_abs_storm_ascending[is.finite(STRT_abs_storm_ascending$Q.norm) & is.finite(STRT_abs_storm_ascending$abs.norm), ]

beta.all.abs.strt <- STRT_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# VAUL # 
VAUL_abs_storm$storm.ID = c(rep("storm1", 127),
                            rep("storm2", 763))

names(VAUL_abs_storm) <- c("DateTime", "Q", "Q.norm", "abs", "abs.norm", "storm.ID")
VAUL_abs_storm$site.ID <- "VAUL"

VAUL_abs_storm[cols] <- log(VAUL_abs_storm[cols]) # making concentrations and Q log transformed

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}
VAUL_abs_storm <- VAUL_abs_storm %>% group_by(storm.ID) %>% 
  mutate(limb = ifelse(DateTime < DateTime[which.max(Q.norm)], "ascending", "descending"))

VAUL_abs_storm_ascending <- filter(VAUL_abs_storm, limb == "ascending")

VAUL_abs_storm_ascending <- VAUL_abs_storm_ascending[is.finite(VAUL_abs_storm_ascending$Q.norm) & is.finite(VAUL_abs_storm_ascending$abs.norm), ]

beta.all.abs.vaul <- VAUL_abs_storm_ascending %>% group_by(storm.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site

# ALL # 
FRCH_abs_storm_ascending$DateTime <- as.POSIXct(FRCH_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
MOOS_abs_storm_ascending$DateTime <- as.POSIXct(MOOS_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

STRT_abs_storm_ascending$DateTime <- as.POSIXct(STRT_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
VAUL_abs_storm_ascending$DateTime <- as.POSIXct(VAUL_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
POKE_abs_storm_ascending$DateTime <- as.POSIXct(POKE_abs_storm_ascending$DateTime, 
                                                "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

All_abs_storm <- rbind(FRCH_abs_storm_ascending, MOOS_abs_storm_ascending, 
                       STRT_abs_storm_ascending, VAUL_abs_storm_ascending,
                       POKE_abs_storm_ascending)

beta.all.abs <- All_abs_storm %>% group_by(storm.ID, site.ID) %>% 
  summarize(beta = slope(Q.norm, abs.norm)) # this works just like the beta one that is for an individual site


beta.all.abs$response_var <- "abs"

all.2022.ci.abs <- All_abs_storm %>% 
  group_by(site.ID, storm.ID) %>% 
  group_modify(~ parameters::model_parameters(stats::lm(abs.norm ~ Q.norm, data = .x)))

all.2022.ci.abs$response_var <- "abs"

beta.all.2022 <- rbind(all.2022.ci.no3, all.2022.ci.fDOM,
                       all.2022.ci.SPC, all.2022.ci.turb,
                       all.2022.ci.abs)

write.csv(here("Storms_clean_repo", "Output_from_analysis", "06_BETA", "beta.2022.csv"))

# write.csv(beta.all.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/06_BETA/beta.2022.csv")

beta.all.2022 <- beta.all.2022 %>% 
  filter(Parameter != "(Intercept)")




