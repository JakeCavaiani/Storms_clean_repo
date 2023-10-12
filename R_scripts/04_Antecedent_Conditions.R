### The purpose of this script is to regress mean HI at each site per year against burn extent (percentage burned)
# and against permafrost extent (% coverage OR soil temperature profile)
# Input: HI.dat 
# Step 1) import HI.dat file which is HI for individual storms in 2018-2021 across DoD sites
# Step 2) Calculate mean HI at each site for each year
# Step 3) run a linear model of mean HI against burn extent 
# Step 4) run a linear model of mean HI against permafrost extent
# Output: linear model plot 

# % of most recent burn # 
# Poker Creek 33%
# Vault Creek: NA
# French Creek: 7.3% 
# Moose Creek: 65.7%
# Stuart Creek: 67.4%
# Caribou Creek: 0 % 

# Permafrost extent # 
# Poker Creek Low
# Vault Creek: High- continuous (100%)
# French Creek: Medium
# Moose Creek: Medium
# Stuart Creek: High

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

# Import data #
# 2015
FRCH_HI_doy_df_2015 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2015", "FRCH","FRCH.HI.df.doy.csv"))
MOOS_HI_doy_df_2015 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2015", "MOOS","MOOS.HI.df.doy.csv"))

# 2018
FRCH_HI_doy_df_2018 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2018", "FRCH","FRCH.HI.df.doy.csv"))
MOOS_HI_doy_df_2018 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2018", "MOOS","MOOS.HI.df.doy.csv"))
CARI_HI_doy_df_2018 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2018", "CARI","CARI.HI.df.doy.csv"))


# 2019
FRCH_HI_doy_df_2019 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2019", "FRCH","FRCH.HI.df.doy.csv"))
MOOS_HI_doy_df_2019 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2019", "MOOS","MOOS.HI.df.doy.csv"))
POKE_HI_doy_df_2019 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2019", "POKE","POKE.HI.df.doy.csv"))
STRT_HI_doy_df_2019 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2019", "STRT","STRT.HI.df.doy.csv"))
VAUL_HI_doy_df_2019 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2019", "VAUL","VAUL.HI.df.doy.csv"))
CARI_HI_doy_df_2019 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2019", "CARI","CARI.HI.df.doy.csv"))



# 2020
FRCH_HI_doy_df_2020 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2020", "FRCH","FRCH.HI.df.doy.csv"))
MOOS_HI_doy_df_2020 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2020", "MOOS","MOOS.HI.df.doy.csv"))
POKE_HI_doy_df_2020 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2020", "POKE","POKE.HI.df.doy.csv"))
STRT_HI_doy_df_2020 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2020", "STRT","STRT.HI.df.doy.csv"))
VAUL_HI_doy_df_2020 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2020", "VAUL","VAUL.HI.df.doy.csv"))
CARI_HI_doy_df_2020 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2020", "CARI","CARI.HI.df.doy.csv"))

# 2021
FRCH_HI_doy_df_2021 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2021", "FRCH","FRCH.HI.df.doy.csv"))
MOOS_HI_doy_df_2021 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2021", "MOOS","MOOS.HI.df.doy.csv"))
POKE_HI_doy_df_2021 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2021", "POKE","POKE.HI.df.doy.csv"))
STRT_HI_doy_df_2021 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2021", "STRT","STRT.HI.df.doy.csv"))
VAUL_HI_doy_df_2021 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2021", "VAUL","VAUL.HI.df.doy.csv"))
CARI_HI_doy_df_2021 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2021", "CARI","CARI.HI.df.doy.csv"))

# 2022
FRCH_HI_doy_df_2022 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2022", "FRCH","FRCH.HI.df.doy.csv"))
MOOS_HI_doy_df_2022 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2022", "MOOS","MOOS.HI.df.doy.csv"))
POKE_HI_doy_df_2022 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2022", "POKE","POKE.HI.df.doy.csv"))
STRT_HI_doy_df_2022 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2022", "STRT","STRT.HI.df.doy.csv"))
VAUL_HI_doy_df_2022 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2022", "VAUL","VAUL.HI.df.doy.csv"))
CARI_HI_doy_df_2022 <- read.csv(here("Output_from_analysis", "03_HI_FI", "2022", "CARI","CARI.HI.df.doy.csv"))


HI.dat_2015 <- rbind(FRCH_HI_doy_df_2015, MOOS_HI_doy_df_2015)
HI.dat_2015$year <- "2015"


HI.dat_2018 <- rbind(FRCH_HI_doy_df_2018, MOOS_HI_doy_df_2018, CARI_HI_doy_df_2018)
HI.dat_2018$year <- "2018"

HI.dat_2019 <- rbind(FRCH_HI_doy_df_2019, MOOS_HI_doy_df_2019, POKE_HI_doy_df_2019,
                     STRT_HI_doy_df_2019, VAUL_HI_doy_df_2019, CARI_HI_doy_df_2019)
HI.dat_2019$year <- "2019"

HI.dat_2020 <- rbind(FRCH_HI_doy_df_2020, MOOS_HI_doy_df_2020, POKE_HI_doy_df_2020, 
                     STRT_HI_doy_df_2020, VAUL_HI_doy_df_2020, CARI_HI_doy_df_2020)
HI.dat_2020$year <- "2020"

HI.dat_2021 <- rbind(FRCH_HI_doy_df_2021, MOOS_HI_doy_df_2021, POKE_HI_doy_df_2021, 
                     STRT_HI_doy_df_2021, VAUL_HI_doy_df_2021, CARI_HI_doy_df_2021) 
HI.dat_2021$year <- "2021"

HI.dat_2022 <- rbind(FRCH_HI_doy_df_2022, MOOS_HI_doy_df_2022, POKE_HI_doy_df_2022, 
                     STRT_HI_doy_df_2022, VAUL_HI_doy_df_2022, CARI_HI_doy_df_2022)
HI.dat_2022$year <- "2022"



HI.dat <- rbind(HI.dat_2015, HI.dat_2018, HI.dat_2019, HI.dat_2020, HI.dat_2021, HI.dat_2022)
# write.csv(HI.dat, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/HI.dat.csv")

#HI.dat <- read_csv("~/Documents/Storms/Output_from_analysis/HI.dat.csv")


HI.mean<- HI.dat %>% group_by(site.ID, response, year) %>%  
  summarise_at(vars(HI), list(HI = median)) # takes the median by site response and year 

# merged # 
# By site and response
FRCH.fDOM <- subset(HI.mean, site.ID == "FRCH" & response == "fDOM")
POKE.fDOM <- subset(HI.mean, site.ID == "POKE" & response == "fDOM")
MOOS.fDOM <- subset(HI.mean, site.ID == "MOOS" & response == "fDOM")
STRT.fDOM <- subset(HI.mean, site.ID == "STRT" & response == "fDOM")
VAUL.fDOM <- subset(HI.mean, site.ID == "VAUL" & response == "fDOM")
CARI.fDOM <- subset(HI.mean, site.ID == "CARI" & response == "fDOM")

FRCH.NO3 <- subset(HI.mean, site.ID == "FRCH" & response == "NO3")
POKE.NO3 <- subset(HI.mean, site.ID == "POKE" & response == "NO3")
MOOS.NO3 <- subset(HI.mean, site.ID == "MOOS" & response == "NO3")
STRT.NO3 <- subset(HI.mean, site.ID == "STRT" & response == "NO3")
VAUL.NO3 <- subset(HI.mean, site.ID == "VAUL" & response == "NO3")
CARI.NO3 <- subset(HI.mean, site.ID == "CARI" & response == "NO3")

FRCH.fDOM$burn <- "unburned"

POKE.fDOM$burn <- "burned"

MOOS.fDOM$burn <- "burned"

STRT.fDOM$burn <- "burned"

VAUL.fDOM$burn <- "unburned"

CARI.fDOM$burn <- "unburned"

FRCH.NO3$burn <- "unburned"

POKE.NO3$burn <- "burned"

MOOS.NO3$burn <- "burned"

STRT.NO3$burn <- "burned"

VAUL.NO3$burn <- "unburned"

CARI.NO3$burn <- "unburned"

fdom.hi <- rbind(FRCH.fDOM, POKE.fDOM, MOOS.fDOM, STRT.fDOM, VAUL.fDOM, CARI.fDOM)
no3.hi <- rbind(FRCH.NO3, POKE.NO3, MOOS.NO3, STRT.NO3, VAUL.NO3, CARI.NO3)

fdom.hi$year <- as.character(fdom.hi$year)
no3.hi$year <- as.character(no3.hi$year)


fdom.lm <- lm(fdom.hi$HI ~ fdom.hi$burn)
no3.lm <- lm(no3.hi$HI ~ no3.hi$burn)

fdom.hi %>%
  ggplot(aes(x=burn, 
             y=HI, 
             color=year))+
  geom_boxplot() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("DOC") +
  xlab("Catchment burned (%)") +
  ylab("HI-Solute Storage")

no3.hi %>%
  ggplot(aes(x=burn, 
             y=HI, 
             color=year))+
  geom_boxplot() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("NO3") +
  xlab("Catchment burned (%)") +
  ylab("HI-Solute Storage")

# Permafrost #
FRCH.fDOM$pf <- "Moderate"

POKE.fDOM$pf <- "Moderate"

MOOS.fDOM$pf <- "Moderate"

STRT.fDOM$pf <- "High"

VAUL.fDOM$pf <- "High"

CARI.fDOM$pf <- "Moderate"

FRCH.NO3$pf <- "Moderate"

POKE.NO3$pf <- "Moderate"

MOOS.NO3$pf <- "Moderate"

STRT.NO3$pf <- "High"

VAUL.NO3$pf <- "High"

CARI.NO3$pf <- "Moderate"


pf.fdom.hi <- rbind(FRCH.fDOM, POKE.fDOM, MOOS.fDOM, STRT.fDOM, VAUL.fDOM, CARI.fDOM)
pf.no3.hi <- rbind(FRCH.NO3, POKE.NO3, MOOS.NO3, STRT.NO3, VAUL.NO3, CARI.NO3)

pf.fdom.hi$year <- as.character(pf.fdom.hi$year)
pf.no3.hi$year <- as.character(pf.no3.hi$year)


pf.fdom.lm <- lm(pf.fdom.hi$HI ~ pf.fdom.hi$burn)
pf.no3.lm <- lm(pf.no3.hi$HI ~ pf.no3.hi$burn)

pf.fdom.hi %>%
  ggplot(aes(x=pf, 
             y=HI, 
             color=year))+
  geom_boxplot() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("DOC") +
  xlab("Permafrost Extent (%)") +
  ylab("HI-Solute Storage")

pf.no3.hi %>%
  ggplot(aes(x=pf, 
             y=HI, 
             color=year))+
  geom_boxplot() +
  geom_smooth(method = "lm") + 
  ylim(-1,1) + 
  ggtitle("NO3") +
  xlab("Permafrost Extent (%)") +
  ylab("HI-Solute Storage")


### H 1.1: HI against precip ###
HI.mean.precip <- HI.dat %>% group_by(site.ID, year, storm.num) %>%  
  summarise_at(vars(HI), list(HI = median)) # take mean by site response and year 

HI.mean.precip.response <- HI.dat %>% group_by(site.ID, year, storm.num, response) %>%  
  summarise_at(vars(HI), list(HI = median)) # take mean by site response and year 


### USGS pulled data ###
# Peak stream flow for the Chena River
# 2015-05-12
# 2018-05-22
# 2019-05-12
# 2020-05-12
#2021-05-12
##############################################################################################################
#################################### Antecedent conditions #####################################################################
##############################################################################################################
setwd("Storm_Events/2015")
########################################## 2015 ########################################
### Import climate ###
CPCRW <- read_csv(here("Climate", "Precip", "CPCRW.RainGauge.2015.final.csv"))
CPCRW$datetimeAK <- force_tz(CPCRW$datetimeAK, "America/Anchorage") # it already is in AK time so I want to make it recognize it without changing the actually time value 

# this needs to be updated to include 2015 data
airtempmean <- read.csv(here("Climate", "airtempmean.csv"))
airtempmean$datetimeAK <- airtempmean$date_timeAK
airtempmean$datetimeAK <- as.POSIXct(airtempmean$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 

### FRCH ####
FRCHstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

# FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2015/All_sites/", 
#                                   recursive=F, 
#                                   pattern="FRCH", 
#                                   full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 287),
                          rep("storm2", 331),
                          rep("storm3", 383),
                          rep("storm4", 299),
                          rep("storm5a", 449),
                          
                          rep("storm6a", 1295),
                          
                          rep("storm7", 240))

FRCH_storms$datetimeAK <- as.POSIXct(FRCH_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH.2015.storms.1<- left_join(FRCH_storms, CPCRW, by = "datetimeAK")
FRCH.2015.storms.1<- left_join(FRCH.2015.storms.1, airtempmean, by = "datetimeAK")

names(FRCH.2015.storms.1)[names(FRCH.2015.storms.1) == ''] <- 'x'

FRCH.2015.per.storm.1 <- FRCH.2015.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- FRCH.2015.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

FRCH.2015.per.storm.1$temp <- temp$temp


# Reading in chem data to join with the antecedent moisture condition data 
DOD.2015 <- read.csv(here("processed_sensor_data", "2015", "SUNA.EXO.int.corr.lab_2015.csv")) 
DOD.2015 <- DOD.2015[c("datetimeAK", "Site", "fDOM.QSU.T.turb.col", "SpCond.uScm.adj",
                       "Turbidity.FNU.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
names(DOD.2015) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")


# summing up week/month/threemonth antecedent precip
DOD.2015$datetimeAK <- ymd_hms(DOD.2015$datetimeAK) # converting character to datetime
DOD.2015$datetimeAK <- force_tz(DOD.2015$datetimeAK, "America/Anchorage") # converting character to datetime

DOD.2015 <- left_join(DOD.2015, CPCRW, by = "datetimeAK")

FRCH.2015 <- subset(DOD.2015, site.ID == "FRCH")
MOOS.2015 <- subset(DOD.2015, site.ID == "MOOS")


#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2015-05-01", tz = "America/Anchorage"),
          as.POSIXct("2015-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
FRCH.2015 <- left_join(ts, FRCH.2015, by = "datetimeAK")
FRCH.2015 <- left_join(FRCH.2015, airtempmean, by = "datetimeAK")


FRCH.2015$week <- rollapplyr(FRCH.2015$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2015$month <- rollapplyr(FRCH.2015$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2015$ThreeMonth <- rollapplyr(FRCH.2015$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2015$temp.week <- rollapplyr(FRCH.2015$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms
FRCH.2015.storms.1 <- FRCH.2015.storms.1[,-c(1,3:9,11:20)]

# joining with storms 
FRCH.2015.1 <- left_join(FRCH.2015.storms.1, FRCH.2015, by = c("datetimeAK")) # week month and 3 month precip totals 

FRCH.2015.per.storm.2 <- FRCH.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2015.per.storm.3 <- FRCH.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2015.per.storm.4 <- FRCH.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# FRCH.2022.per.storm.5 <- FRCH.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2015" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2015" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2015" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2015" & site.ID == "FRCH" & response == "turb")
HI.mean.precip.frch.abs <- subset(HI.mean.precip.response, year == "2015" & site.ID == "FRCH" & response == "abs")

# NO3
HI.frch.no3.2015 <- left_join(HI.mean.precip.frch.NO3, FRCH.2015.per.storm.1, by = "storm.num")
HI.frch.no3.2015 <- left_join(HI.frch.no3.2015, FRCH.2015.per.storm.2, by = "storm.num")
HI.frch.no3.2015 <- left_join(HI.frch.no3.2015, FRCH.2015.per.storm.3, by = "storm.num")
HI.frch.no3.2015 <- left_join(HI.frch.no3.2015, FRCH.2015.per.storm.4, by = "storm.num")
#HI.frch.no3.2015 <- left_join(HI.frch.no3.2015, FRCH.2015.per.storm.5, by = "storm.num")

frch.lm.no3 <- lm(HI.frch.no3.2015$HI ~ HI.frch.no3.2015$precip) # model one with just total precip
frch.lm.no3.2 <- lm(HI.frch.no3.2015$HI ~ HI.frch.no3.2015$precip.week) # model one with just total precip
frch.lm.no3.3 <- lm(HI.frch.no3.2015$HI ~ HI.frch.no3.2015$precip.month) # model one with just total precip
frch.lm.no3.4 <- lm(HI.frch.no3.2015$HI ~ HI.frch.no3.2015$ThreeMonth) # model one with just total precip

# fDOM #
HI.frch.fDOM.2015 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2015.per.storm.1, by = "storm.num")
HI.frch.fDOM.2015 <- left_join(HI.frch.fDOM.2015, FRCH.2015.per.storm.2, by = "storm.num")
HI.frch.fDOM.2015 <- left_join(HI.frch.fDOM.2015, FRCH.2015.per.storm.3, by = "storm.num")
HI.frch.fDOM.2015 <- left_join(HI.frch.fDOM.2015, FRCH.2015.per.storm.4, by = "storm.num")
#HI.frch.fDOM.2021 <- left_join(HI.frch.fDOM.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2015$HI ~ HI.frch.fDOM.2015$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2015$HI ~ HI.frch.fDOM.2015$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2015$HI ~ HI.frch.fDOM.2015$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2015$HI ~ HI.frch.fDOM.2015$ThreeMonth) # model one with just total precip
#frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2022$HI ~ HI.frch.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.frch.SPC.2015 <- left_join(HI.mean.precip.frch.SPC, FRCH.2015.per.storm.1, by = "storm.num")
HI.frch.SPC.2015 <- left_join(HI.frch.SPC.2015, FRCH.2015.per.storm.2, by = "storm.num")
HI.frch.SPC.2015 <- left_join(HI.frch.SPC.2015, FRCH.2015.per.storm.3, by = "storm.num")
HI.frch.SPC.2015 <- left_join(HI.frch.SPC.2015, FRCH.2015.per.storm.4, by = "storm.num")
#HI.frch.SPC.2021 <- left_join(HI.frch.SPC.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2015$HI ~ HI.frch.SPC.2015$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2015$HI ~ HI.frch.SPC.2015$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2015$HI ~ HI.frch.SPC.2015$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2015$HI ~ HI.frch.SPC.2015$ThreeMonth) # model one with just total precip
#frch.lm.SPC.5 <- lm(HI.frch.SPC.2022$HI ~ HI.frch.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.frch.turb.2015 <- left_join(HI.mean.precip.frch.turb, FRCH.2015.per.storm.1, by = "storm.num")
HI.frch.turb.2015 <- left_join(HI.frch.turb.2015, FRCH.2015.per.storm.2, by = "storm.num")
HI.frch.turb.2015 <- left_join(HI.frch.turb.2015, FRCH.2015.per.storm.3, by = "storm.num")
HI.frch.turb.2015 <- left_join(HI.frch.turb.2015, FRCH.2015.per.storm.4, by = "storm.num")
#HI.frch.turb.2021 <- left_join(HI.frch.turb.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2015$HI ~ HI.frch.turb.2015$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2015$HI ~ HI.frch.turb.2015$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2015$HI ~ HI.frch.turb.2015$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2015$HI ~ HI.frch.turb.2015$ThreeMonth) # model one with just total precip
#frch.lm.turb.5 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$temp.week) # model one with just total precip


# abs #
HI.frch.abs.2015 <- left_join(HI.mean.precip.frch.abs, FRCH.2015.per.storm.1, by = "storm.num")
HI.frch.abs.2015 <- left_join(HI.frch.abs.2015, FRCH.2015.per.storm.2, by = "storm.num")
HI.frch.abs.2015 <- left_join(HI.frch.abs.2015, FRCH.2015.per.storm.3, by = "storm.num")
HI.frch.abs.2015 <- left_join(HI.frch.abs.2015, FRCH.2015.per.storm.4, by = "storm.num")
#HI.frch.abs.2021 <- left_join(HI.frch.abs.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.abs <- lm(HI.frch.abs.2015$HI ~ HI.frch.abs.2015$precip) # model one with just total precip
frch.lm.abs.2 <- lm(HI.frch.abs.2015$HI ~ HI.frch.abs.2015$precip.week) # model one with just total precip
frch.lm.abs.3 <- lm(HI.frch.abs.2015$HI ~ HI.frch.abs.2015$precip.month) # model one with just total precip
frch.lm.abs.4 <- lm(HI.frch.abs.2015$HI ~ HI.frch.abs.2015$ThreeMonth) # model one with just total precip
#frch.lm.abs.5 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
FRCH.2015.1$day <- julian(FRCH.2015.1$datetimeAK, origin = as.POSIXct('2015-01-01', tz = 'America/Anchorage')) # making a fractional day column 
FRCH.2015.1$day <- as.numeric(FRCH.2015.1$day)

FRCH.2015.per.storm.5 <- FRCH.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.frch.fDOM.2.2015 <- left_join(HI.frch.fDOM.2015, FRCH.2015.per.storm.5, by = "storm.num")

frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2015$HI ~ HI.frch.fDOM.2.2015$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.frch.SPC.2.2015 <- left_join(HI.frch.SPC.2015, FRCH.2015.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2015$HI ~ HI.frch.SPC.2.2015$doy)

HI.frch.turb.2.2015 <- left_join(HI.frch.turb.2015, FRCH.2015.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2015$HI ~ HI.frch.turb.2.2015$doy)


HI.frch.no3.2.2015 <- left_join(HI.frch.no3.2015, FRCH.2015.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2015$HI ~ HI.frch.no3.2.2015$doy)

HI.frch.abs.2.2015 <- left_join(HI.frch.abs.2015, FRCH.2015.per.storm.5, by = "storm.num")
frch.lm.abs.5 <- lm(HI.frch.turb.2.2015$HI ~ HI.frch.turb.2.2015$doy)



HI.frch.2015 <- rbind(HI.frch.no3.2.2015, HI.frch.fDOM.2.2015, HI.frch.SPC.2.2015, HI.frch.turb.2.2015, HI.frch.abs.2.2015) # merging all responses together


HI.frch.2015$date <- as.Date(HI.frch.2015$doy, origin = "2015-01-01")
origin_date <- as.Date("2015-05-12")
HI.frch.2015$TimeSinceChena <- julian(HI.frch.2015$date, origin_date)

write.csv(HI.frch.2015, here("Output_from_analysis", "04_Antecedent_Conditions", "2015", "HI.frch.2015.csv"))


### MOOS ####
MOOSstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)


# MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2015/All_sites/", 
#                                   recursive=F, 
#                                   pattern="MOOS", 
#                                   full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 383),
                          rep("storm2", 575),
                          rep("storm3a", 611),
                          
                          rep("storm4", 191),
                          rep("storm5", 455),
                          rep("storm6", 176))

MOOS_storms$datetimeAK <- as.POSIXct(MOOS_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2015.storms.1<- left_join(MOOS_storms, CPCRW, by = "datetimeAK")
MOOS.2015.storms.1<- left_join(MOOS.2015.storms.1, airtempmean, by = "datetimeAK")

names(MOOS.2015.storms.1)[names(MOOS.2015.storms.1) == ''] <- 'x'

MOOS.2015.per.storm.1 <- MOOS.2015.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- MOOS.2015.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

MOOS.2015.per.storm.1$temp <- temp$temp

#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2015-05-01", tz = "America/Anchorage"),
          as.POSIXct("2015-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
MOOS.2015 <- left_join(ts, MOOS.2015, by = "datetimeAK")
MOOS.2015 <- left_join(MOOS.2015, airtempmean, by = "datetimeAK")


MOOS.2015$week <- rollapplyr(MOOS.2015$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2015$month <- rollapplyr(MOOS.2015$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2015$ThreeMonth <- rollapplyr(MOOS.2015$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2015$temp.week <- rollapplyr(MOOS.2015$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms
MOOS.2015.storms.1 <- MOOS.2015.storms.1[,-c(1,3:9,11:20)]

# joining with storms 
MOOS.2015.1 <- left_join(MOOS.2015.storms.1, MOOS.2015, by = c("datetimeAK")) # week month and 3 month precip totals 

MOOS.2015.per.storm.2 <- MOOS.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2015.per.storm.3 <- MOOS.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2015.per.storm.4 <- MOOS.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# MOOS.2022.per.storm.5 <- MOOS.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2015" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2015" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2015" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2015" & site.ID == "MOOS" & response == "turb")
HI.mean.precip.moos.abs <- subset(HI.mean.precip.response, year == "2015" & site.ID == "MOOS" & response == "abs")

# NO3
HI.moos.no3.2015 <- left_join(HI.mean.precip.moos.NO3, MOOS.2015.per.storm.1, by = "storm.num")
HI.moos.no3.2015 <- left_join(HI.moos.no3.2015, MOOS.2015.per.storm.2, by = "storm.num")
HI.moos.no3.2015 <- left_join(HI.moos.no3.2015, MOOS.2015.per.storm.3, by = "storm.num")
HI.moos.no3.2015 <- left_join(HI.moos.no3.2015, MOOS.2015.per.storm.4, by = "storm.num")
#HI.moos.no3.2015 <- left_join(HI.moos.no3.2015, MOOS.2015.per.storm.5, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2015$HI ~ HI.moos.no3.2015$precip) # model one with just total precip
moos.lm.no3.2 <- lm(HI.moos.no3.2015$HI ~ HI.moos.no3.2015$precip.week) # model one with just total precip
moos.lm.no3.3 <- lm(HI.moos.no3.2015$HI ~ HI.moos.no3.2015$precip.month) # model one with just total precip
moos.lm.no3.4 <- lm(HI.moos.no3.2015$HI ~ HI.moos.no3.2015$ThreeMonth) # model one with just total precip

# fDOM #
HI.moos.fDOM.2015 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2015.per.storm.1, by = "storm.num")
HI.moos.fDOM.2015 <- left_join(HI.moos.fDOM.2015, MOOS.2015.per.storm.2, by = "storm.num")
HI.moos.fDOM.2015 <- left_join(HI.moos.fDOM.2015, MOOS.2015.per.storm.3, by = "storm.num")
HI.moos.fDOM.2015 <- left_join(HI.moos.fDOM.2015, MOOS.2015.per.storm.4, by = "storm.num")
#HI.moos.fDOM.2015 <- left_join(HI.moos.fDOM.2015, MOOS.2015.per.storm.5, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2015$HI ~ HI.moos.fDOM.2015$precip) # model one with just total precip
moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2015$HI ~ HI.moos.fDOM.2015$precip.week) # model one with just total precip
moos.lm.fDOM.3 <- lm(HI.moos.fDOM.2015$HI ~ HI.moos.fDOM.2015$precip.month) # model one with just total precip
moos.lm.fDOM.4 <- lm(HI.moos.fDOM.2015$HI ~ HI.moos.fDOM.2015$ThreeMonth) # model one with just total precip
#moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2015$HI ~ HI.moos.fDOM.2015$temp.week) # model one with just total precip

# SPC #
HI.moos.SPC.2015 <- left_join(HI.mean.precip.moos.SPC, MOOS.2015.per.storm.1, by = "storm.num")
HI.moos.SPC.2015 <- left_join(HI.moos.SPC.2015, MOOS.2015.per.storm.2, by = "storm.num")
HI.moos.SPC.2015 <- left_join(HI.moos.SPC.2015, MOOS.2015.per.storm.3, by = "storm.num")
HI.moos.SPC.2015 <- left_join(HI.moos.SPC.2015, MOOS.2015.per.storm.4, by = "storm.num")
#HI.moos.SPC.2015 <- left_join(HI.moos.SPC.2015, MOOS.2015.per.storm.5, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2015$HI ~ HI.moos.SPC.2015$precip) # model one with just total precip
moos.lm.SPC.2 <- lm(HI.moos.SPC.2015$HI ~ HI.moos.SPC.2015$precip.week) # model one with just total precip
moos.lm.SPC.3 <- lm(HI.moos.SPC.2015$HI ~ HI.moos.SPC.2015$precip.month) # model one with just total precip
moos.lm.SPC.4 <- lm(HI.moos.SPC.2015$HI ~ HI.moos.SPC.2015$ThreeMonth) # model one with just total precip
#moos.lm.SPC.5 <- lm(HI.moos.SPC.2015$HI ~ HI.moos.SPC.2015$temp.week) # model one with just total precip


# turb #
HI.moos.turb.2015 <- left_join(HI.mean.precip.moos.turb, MOOS.2015.per.storm.1, by = "storm.num")
HI.moos.turb.2015 <- left_join(HI.moos.turb.2015, MOOS.2015.per.storm.2, by = "storm.num")
HI.moos.turb.2015 <- left_join(HI.moos.turb.2015, MOOS.2015.per.storm.3, by = "storm.num")
HI.moos.turb.2015 <- left_join(HI.moos.turb.2015, MOOS.2015.per.storm.4, by = "storm.num")
#HI.moos.turb.2015 <- left_join(HI.moos.turb.2015, MOOS.2015.per.storm.5, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2015$HI ~ HI.moos.turb.2015$precip) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2015$HI ~ HI.moos.turb.2015$precip.week) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2015$HI ~ HI.moos.turb.2015$precip.month) # model one with just total precip
moos.lm.turb.4 <- lm(HI.moos.turb.2015$HI ~ HI.moos.turb.2015$ThreeMonth) # model one with just total precip
#moos.lm.turb.5 <- lm(HI.moos.turb.2015$HI ~ HI.moos.turb.2015$temp.week) # model one with just total precip


# abs #
HI.moos.abs.2015 <- left_join(HI.mean.precip.moos.abs, MOOS.2015.per.storm.1, by = "storm.num")
HI.moos.abs.2015 <- left_join(HI.moos.abs.2015, MOOS.2015.per.storm.2, by = "storm.num")
HI.moos.abs.2015 <- left_join(HI.moos.abs.2015, MOOS.2015.per.storm.3, by = "storm.num")
HI.moos.abs.2015 <- left_join(HI.moos.abs.2015, MOOS.2015.per.storm.4, by = "storm.num")
#HI.moos.abs.2015 <- left_join(HI.moos.abs.2015, MOOS.2015.per.storm.5, by = "storm.num")

moos.lm.abs <- lm(HI.moos.abs.2015$HI ~ HI.moos.abs.2015$precip) # model one with just total precip
moos.lm.abs.2 <- lm(HI.moos.abs.2015$HI ~ HI.moos.abs.2015$precip.week) # model one with just total precip
moos.lm.abs.3 <- lm(HI.moos.abs.2015$HI ~ HI.moos.abs.2015$precip.month) # model one with just total precip
moos.lm.abs.4 <- lm(HI.moos.abs.2015$HI ~ HI.moos.abs.2015$ThreeMonth) # model one with just total precip
#moos.lm.abs.5 <- lm(HI.moos.abs.2015$HI ~ HI.moos.abs.2015$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
MOOS.2015.1$day <- julian(MOOS.2015.1$datetimeAK, origin = as.POSIXct('2015-01-01', tz = 'America/Anchorage')) # making a fractional day column 
MOOS.2015.1$day <- as.numeric(MOOS.2015.1$day)

MOOS.2015.per.storm.5 <- MOOS.2015.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.moos.fDOM.2.2015 <- left_join(HI.moos.fDOM.2015, MOOS.2015.per.storm.5, by = "storm.num")

moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2015$HI ~ HI.moos.fDOM.2.2015$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.moos.SPC.2.2015 <- left_join(HI.moos.SPC.2015, MOOS.2015.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2015$HI ~ HI.moos.SPC.2.2015$doy)

HI.moos.turb.2.2015 <- left_join(HI.moos.turb.2015, MOOS.2015.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2015$HI ~ HI.moos.turb.2.2015$doy)


HI.moos.no3.2.2015 <- left_join(HI.moos.no3.2015, MOOS.2015.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2015$HI ~ HI.moos.no3.2.2015$doy)

HI.moos.abs.2.2015 <- left_join(HI.moos.abs.2015, MOOS.2015.per.storm.5, by = "storm.num")
moos.lm.abs.5 <- lm(HI.moos.turb.2.2015$HI ~ HI.moos.turb.2.2015$doy)



HI.moos.2015 <- rbind(HI.moos.no3.2.2015, HI.moos.fDOM.2.2015, HI.moos.SPC.2.2015, HI.moos.turb.2.2015, HI.moos.abs.2.2015) # merging all responses together


HI.moos.2015$date <- as.Date(HI.moos.2015$doy, origin = "2015-01-01")
origin_date <- as.Date("2015-05-12")
HI.moos.2015$TimeSinceChena <- julian(HI.moos.2015$date, origin_date)

write.csv(HI.moos.2015, here("Output_from_analysis", "04_Antecedent_Conditions", "2015", "HI.moos.2015.csv"))


# all sites 
HI.2015 <- rbind(HI.frch.2015, HI.moos.2015) # bind all 2015 together

write.csv(HI.2015, here("Output_from_analysis", "04_Antecedent_Conditions", "2015", "HI.2015.csv"))


######################################## 2018 #####################################################################
setwd("Storm_Events/2018")
## Step 1) Read in list of all sites storms and filter by site
## Step 2) Assign storm number to each individual storm
## Step 3) read in Rain gauge data and summarize storm characteristics (Total precip/Intensity) and 
# antecedent conditions (week/month/3month/doy/time since peak SWE)
## Step 4) Separate by constituent 

### FRCH ####
FRCHstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)


# FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2018/All_Sites/", 
#                                   recursive=F, 
#                                   pattern="FRCH", 
#                                   full.names=TRUE) # reading in individual storms by site 

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 142),
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
                          
                          rep("storm9", 99)) # naming each storm by the number of storm 


# Read in precip data 
POKE_RainGauge_2018 <- read.csv(here("Climate", "Precip", "POKE.RainGauge.2018.csv"))
# POKE_RainGauge_2018 <- read_csv("~/Documents/DoD_2018_Jake/RainGauge/POKE.RainGauge.2018.csv") # Reading in rain gauge data in 
POKE_RainGauge_2018$DateTime <- ymd_hms(POKE_RainGauge_2018$DateTime)

attributes(POKE_RainGauge_2018$DateTime)$tzone <- 'America/Anchorage' # converting to AK time 
airtempmean <- read.csv(here("Climate", "airtempmean.csv"))
airtempmean$date_timeAK <- ymd_hms(airtempmean$date_timeAK)
# airtempmean <- read_csv("~/Documents/Storms_clean_repo/Climate/airtempmean.csv")
attributes(airtempmean$date_timeAK)$tzone <- 'America/Anchorage'
names(airtempmean)[2] <- "DateTime"
FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") #making datetime column 
# Merging in climate conditions 
FRCH.2018.storms.1<- left_join(FRCH_storms, POKE_RainGauge_2018, by = "DateTime") # joining 
FRCH.2018.storms.1<- left_join(FRCH.2018.storms.1, airtempmean, by = "DateTime") # joining 

names(FRCH.2018.storms.1)[names(FRCH.2018.storms.1) == ''] <- 'x'

FRCH.2018.per.storm.1 <- FRCH.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE) # finding the total precip for each delineated storm

temp <- FRCH.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 
FRCH.2018.per.storm.1$temp <- temp$temp

# Reading in chem data to join with the antecedent moisture condition data 
DOD.2018 <- read.csv(here("processed_sensor_data", "2018", "SUNA.EXO.int.corr.lab_2018.csv")) 
DOD.2018 <- DOD.2018[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]

names(DOD.2018) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")

DOD.2018$datetimeAK <- ymd_hms(DOD.2018$datetimeAK) # converting character to datetime
DOD.2018$datetimeAK <- force_tz(DOD.2018$datetimeAK, "America/Anchorage") # converting character to datetime


FRCH.2018 <-  subset(DOD.2018, site.ID == "FRCH")

# summing up week/month/threemonth antecedent precip
FRCH.2018$DateTime <- as.POSIXct(FRCH.2018$datetimeAK)
FRCH.2018 <- left_join(FRCH.2018, POKE_RainGauge_2018, by = "DateTime")
FRCH.2018 <- left_join(FRCH.2018, airtempmean, by = "DateTime")
FRCH.2018$week <- rollapplyr(FRCH.2018$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2018$month <- rollapplyr(FRCH.2018$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2018$ThreeMonth <- rollapplyr(FRCH.2018$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2018$temp.week <- rollapplyr(FRCH.2018$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms 
FRCH.2018.1 <- left_join(FRCH.2018.storms.1, FRCH.2018, by = "DateTime") # week month and 3 month precip totals 

FRCH.2018.per.storm.2 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2018.per.storm.3 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2018.per.storm.4 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
FRCH.2018.per.storm.5 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "turb")
HI.mean.precip.frch.abs <- subset(HI.mean.precip.response, year == "2018" & site.ID == "FRCH" & response == "abs")

HI.frch.no3.2018 <- left_join(HI.mean.precip.frch.NO3, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.2, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.3, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.4, by = "storm.num")
HI.frch.no3.2018 <- left_join(HI.frch.no3.2018, FRCH.2018.per.storm.5, by = "storm.num")

HI.frch.no3.2018 <- HI.frch.no3.2018[,-c(6:7)]
names(HI.frch.no3.2018) <- c("site.ID", "year", "storm.num","response", "HI", "precip", "temp", "precip.week", 
                             "precip.month", "ThreeMonth", "temp.week")

frch.lm.no3 <- lm(HI.frch.no3.2018$HI ~ HI.frch.no3.2018$precip) # model one with just total precip

frch.formula <- y ~ x

ba <- HI.frch.no3.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bb <- HI.frch.no3.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bc <- HI.frch.no3.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bd <- HI.frch.no3.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bd2 <- HI.frch.no3.2018 %>%
  ggplot(aes(x=temp.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("temp.week") +
  ylab("HI-Solute Storage") # plot model 
bd2

HI.frch.fDOM.2018 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.fDOM.2018 <- left_join(HI.frch.fDOM.2018, FRCH.2018.per.storm.2, by = "storm.num")
HI.frch.fDOM.2018 <- left_join(HI.frch.fDOM.2018, FRCH.2018.per.storm.3, by = "storm.num")
HI.frch.fDOM.2018 <- left_join(HI.frch.fDOM.2018, FRCH.2018.per.storm.4, by = "storm.num")
HI.frch.fDOM.2018 <- left_join(HI.frch.fDOM.2018, FRCH.2018.per.storm.5, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2018$HI ~ HI.frch.fDOM.2018$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2018$HI ~ HI.frch.fDOM.2018$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2018$HI ~ HI.frch.fDOM.2018$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2018$HI ~ HI.frch.fDOM.2018$ThreeMonth) # model one with just total precip
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2018$HI ~ HI.frch.fDOM.2018$temp.week) # model one with just total precip

frch.formula <- y ~ x

be <- HI.frch.fDOM.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bf <- HI.frch.fDOM.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bg <- HI.frch.fDOM.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bh <- HI.frch.fDOM.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bh2 <- HI.frch.fDOM.2018 %>%
  ggplot(aes(x=temp.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("temp.week") +
  ylab("HI-Solute Storage") # plot model 
bh2

HI.frch.SPC.2018 <- left_join(HI.mean.precip.frch.SPC, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.SPC.2018 <- left_join(HI.frch.SPC.2018, FRCH.2018.per.storm.2, by = "storm.num")
HI.frch.SPC.2018 <- left_join(HI.frch.SPC.2018, FRCH.2018.per.storm.3, by = "storm.num")
HI.frch.SPC.2018 <- left_join(HI.frch.SPC.2018, FRCH.2018.per.storm.4, by = "storm.num")
HI.frch.SPC.2018 <- left_join(HI.frch.SPC.2018, FRCH.2018.per.storm.5, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2018$HI ~ HI.frch.SPC.2018$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2018$HI ~ HI.frch.SPC.2018$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2018$HI ~ HI.frch.SPC.2018$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2018$HI ~ HI.frch.SPC.2018$ThreeMonth) # model one with just total precip


bi <- HI.frch.SPC.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bj <- HI.frch.SPC.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bk <- HI.frch.SPC.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bl <- HI.frch.SPC.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bl2 <- HI.frch.SPC.2018 %>%
  ggplot(aes(x=temp.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("temp.week") +
  ylab("HI-Solute Storage") # plot model

bl2

HI.frch.turb.2018 <- left_join(HI.mean.precip.frch.turb, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.turb.2018 <- left_join(HI.frch.turb.2018, FRCH.2018.per.storm.2, by = "storm.num")
HI.frch.turb.2018 <- left_join(HI.frch.turb.2018, FRCH.2018.per.storm.3, by = "storm.num")
HI.frch.turb.2018 <- left_join(HI.frch.turb.2018, FRCH.2018.per.storm.4, by = "storm.num")
HI.frch.turb.2018 <- left_join(HI.frch.turb.2018, FRCH.2018.per.storm.5, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2018$HI ~ HI.frch.turb.2018$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2018$HI ~ HI.frch.turb.2018$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2018$HI ~ HI.frch.turb.2018$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2018$HI ~ HI.frch.turb.2018$ThreeMonth) # model one with just total precip

bm <- HI.frch.turb.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bn <- HI.frch.turb.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bo <- HI.frch.turb.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bp <- HI.frch.turb.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- FRCH.2018.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event

HI.frch.no3.2.2018 <- left_join(HI.frch.no3.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.no3.2.2018$TOTAL.TIME <- as.numeric(HI.frch.no3.2.2018$TOTAL.TIME)
HI.frch.no3.2.2018$Intensity <- HI.frch.no3.2.2018$precip/HI.frch.no3.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.no3.2 <- lm(HI.frch.no3.2.2018$HI ~ HI.frch.no3.2.2018$precip + HI.frch.no3.2.2018$Intensity) # model one with total precip and intensity 

bq <- HI.frch.no3.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 

br <- HI.frch.fDOM.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 

bs <- HI.frch.SPC.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

bt <- HI.frch.turb.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
FRCH.2018.1$day <- julian(FRCH.2018.1$DateTime, origin = as.POSIXct('2018-01-01', tz = 'America/Anchorage')) # making a fractional day column 
FRCH.2018.1$day <- as.numeric(FRCH.2018.1$day)

FRCH.2018.per.storm.5 <- FRCH.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.frch.no3.2.2018 <- left_join(HI.frch.no3.2.2018, FRCH.2018.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2018$HI ~ HI.frch.no3.2.2018$doy)

bu <- HI.frch.no3.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2.2018, FRCH.2018.per.storm.5, by = "storm.num")
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$doy)

bv <- HI.frch.fDOM.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2.2018, FRCH.2018.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$doy)

bw <- HI.frch.SPC.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2.2018, FRCH.2018.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$doy)

bx <- HI.frch.turb.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# abs #
HI.frch.abs.2018 <- left_join(HI.mean.precip.frch.abs, FRCH.2018.per.storm.1, by = "storm.num")
HI.frch.abs.2018 <- left_join(HI.frch.abs.2018, FRCH.2018.per.storm.2, by = "storm.num")
HI.frch.abs.2018 <- left_join(HI.frch.abs.2018, FRCH.2018.per.storm.3, by = "storm.num")
HI.frch.abs.2018 <- left_join(HI.frch.abs.2018, FRCH.2018.per.storm.4, by = "storm.num")
HI.frch.abs.2018 <- left_join(HI.frch.abs.2018, FRCH.2018.per.storm.5, by = "storm.num")

frch.lm.abs <- lm(HI.frch.abs.2018$HI ~ HI.frch.abs.2018$precip) # model one with just total precip
frch.lm.abs.2 <- lm(HI.frch.abs.2018$HI ~ HI.frch.abs.2018$precip.week) # model one with just total precip
frch.lm.abs.3 <- lm(HI.frch.abs.2018$HI ~ HI.frch.abs.2018$precip.month) # model one with just total precip
frch.lm.abs.4 <- lm(HI.frch.abs.2018$HI ~ HI.frch.abs.2018$ThreeMonth) # model one with just total precip
frch.lm.abs.5 <- lm(HI.frch.turb.2018$HI ~ HI.frch.turb.2018$temp.week) # model one with just total precip

HI.frch.abs.2.2018 <- left_join(HI.frch.abs.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.abs.2.2018$TOTAL.TIME <- as.numeric(HI.frch.abs.2.2018$TOTAL.TIME)
HI.frch.abs.2.2018$Intensity <- HI.frch.abs.2.2018$precip/HI.frch.abs.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.frch.2018 <- rbind(HI.frch.no3.2.2018, HI.frch.fDOM.2.2018, 
                      HI.frch.SPC.2.2018, HI.frch.turb.2.2018,
                      HI.frch.abs.2018) # merging all responses together 
HI.frch.2018$burn <- "unburned" # adding a burn column
HI.frch.2018$pf <- "medium" # adding a pf column
HI.frch.2018$site.ID <- "FRCH"

write.csv(HI.frch.2018, here("Output_from_analysis", "04_Antecedent_Conditions", "2018", "HI.frch.2018.csv"))

# MOOS ####
MOOSstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(
                          rep("storm10", 432),
                          rep("storm11a", 420),
                          
                          rep("storm2a", 412),
                          
                          rep("storm3", 198),
                          
                          rep("storm5", 282),
                          rep("storm6", 333),
                          rep("storm7", 176),
                          rep("storm8a", 178),
                          
                          rep("storm9", 106))

MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2018.storms.1<- left_join(MOOS_storms, POKE_RainGauge_2018, by = "DateTime")
MOOS.2018.storms.1<- left_join(MOOS.2018.storms.1, airtempmean, by = "DateTime")

names(MOOS.2018.storms.1)[names(MOOS.2018.storms.1) == ''] <- 'x'

MOOS.2018.per.storm.1 <- MOOS.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- MOOS.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

MOOS.2018.per.storm.1$temp <- temp$temp


MOOS.2018 <-  subset(DOD.2018, site.ID == "MOOS")

MOOS.2018$DateTime <- as.POSIXct(MOOS.2018$datetimeAK)
MOOS.2018 <- left_join(MOOS.2018, POKE_RainGauge_2018, by = "DateTime")
MOOS.2018 <- left_join(MOOS.2018, airtempmean, by = "DateTime")
MOOS.2018$week <- rollapplyr(MOOS.2018$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2018$month <- rollapplyr(MOOS.2018$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2018$ThreeMonth <- rollapplyr(MOOS.2018$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2018$temp.week <- rollapplyr(MOOS.2018$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2018.1 <- left_join(MOOS.2018.storms.1, MOOS.2018, by = "DateTime") # week month and 3 month precip totals 

MOOS.2018.per.storm.2 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2018.per.storm.3 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2018.per.storm.4 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
MOOS.2018.per.storm.5 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "turb")
HI.mean.precip.moos.abs <- subset(HI.mean.precip.response, year == "2018" & site.ID == "MOOS" & response == "abs")

HI.moos.no3.2018 <- left_join(HI.mean.precip.moos.NO3, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.4, by = "storm.num")
HI.moos.no3.2018 <- left_join(HI.moos.no3.2018, MOOS.2018.per.storm.5, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$precip) # model one with just total precip
moos.lm.no3.2 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$precip.week) # model one with just total precip
moos.lm.no3.3 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$precip.month) # model one with just total precip
moos.lm.no3.4 <- lm(HI.moos.no3.2018$HI ~ HI.moos.no3.2018$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

aa <- HI.moos.no3.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ab <- HI.moos.no3.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ac <- HI.moos.no3.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ad <- HI.moos.no3.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ad2 <- HI.moos.no3.2018 %>%
  ggplot(aes(x=temp.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("temp.week") +
  ylab("HI-Solute Storage") # plot model 
ad2

HI.moos.fDOM.2018 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.4, by = "storm.num")
HI.moos.fDOM.2018 <- left_join(HI.moos.fDOM.2018, MOOS.2018.per.storm.5, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$precip) # model one with just total precip
moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$precip.week) # model one with just total precip
moos.lm.fDOM.3 <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$precip.month) # model one with just total precip
moos.lm.fDOM.4 <- lm(HI.moos.fDOM.2018$HI ~ HI.moos.fDOM.2018$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

ae <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

af <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ag <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ah <- HI.moos.fDOM.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.SPC.2018 <- left_join(HI.mean.precip.moos.SPC, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.4, by = "storm.num")
HI.moos.SPC.2018 <- left_join(HI.moos.SPC.2018, MOOS.2018.per.storm.5, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$precip) # model one with just total precip
moos.lm.SPC.2 <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$precip.week) # model one with just total precip
moos.lm.SPC.3 <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$precip.month) # model one with just total precip
moos.lm.SPC.4 <- lm(HI.moos.SPC.2018$HI ~ HI.moos.SPC.2018$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

ai <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

aj <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ak <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

al <- HI.moos.SPC.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.turb.2018 <- left_join(HI.mean.precip.moos.turb, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.4, by = "storm.num")
HI.moos.turb.2018 <- left_join(HI.moos.turb.2018, MOOS.2018.per.storm.5, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$precip) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$precip.week) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$precip.month) # model one with just total precip
moos.lm.turb.4 <- lm(HI.moos.turb.2018$HI ~ HI.moos.turb.2018$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

am <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

an <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ao <- HI.moos.turb.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ap <- HI.moos.turb.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- MOOS.2018.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.moos.no3.2.2018 <- left_join(HI.moos.no3.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.no3.2.2018$TOTAL.TIME <- as.numeric(HI.moos.no3.2.2018$TOTAL.TIME)
HI.moos.no3.2.2018$Intensity <- HI.moos.no3.2.2018$precip/HI.moos.no3.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.no3.2 <- lm(HI.moos.no3.2.2018$HI ~ HI.moos.no3.2.2018$precip + HI.moos.no3.2.2018$Intensity) # model one with total precip and intensity 

aq <- HI.moos.no3.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2018 <- left_join(HI.moos.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2018$TOTAL.TIME)
HI.moos.fDOM.2.2018$Intensity <- HI.moos.fDOM.2.2018$precip/HI.moos.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2018$HI ~ HI.moos.fDOM.2.2018$precip + HI.moos.fDOM.2.2018$Intensity) # model one with total precip and intensity 

ar <- HI.moos.fDOM.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2018 <- left_join(HI.moos.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2018$TOTAL.TIME)
HI.moos.SPC.2.2018$Intensity <- HI.moos.SPC.2.2018$precip/HI.moos.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2018$HI ~ HI.moos.SPC.2.2018$precip + HI.moos.SPC.2.2018$Intensity) # model one with total precip and intensity 

as <- HI.moos.SPC.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2018 <- left_join(HI.moos.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2018$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2018$TOTAL.TIME)
HI.moos.turb.2.2018$Intensity <- HI.moos.turb.2.2018$precip/HI.moos.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2018$HI ~ HI.moos.turb.2.2018$precip + HI.moos.turb.2.2018$Intensity) # model one with total precip and intensity 

at <- HI.moos.turb.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
MOOS.2018.1$day <- julian(MOOS.2018.1$DateTime, origin = as.POSIXct('2018-01-01', tz = 'America/Anchorage')) # making a fractional day column 
MOOS.2018.1$day <- as.numeric(MOOS.2018.1$day)

MOOS.2018.per.storm.5 <- MOOS.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.moos.no3.2.2018 <- left_join(HI.moos.no3.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2018$HI ~ HI.moos.no3.2.2018$doy)

au <- HI.moos.no3.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2018 <- left_join(HI.moos.fDOM.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2018$HI ~ HI.moos.fDOM.2.2018$doy)

av <- HI.moos.fDOM.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2018 <- left_join(HI.moos.SPC.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2018$HI ~ HI.moos.SPC.2.2018$doy)

aw <- HI.moos.SPC.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2018 <- left_join(HI.moos.turb.2.2018, MOOS.2018.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2018$HI ~ HI.moos.turb.2.2018$doy)

ax <- HI.moos.turb.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# abs #
HI.moos.abs.2018 <- left_join(HI.mean.precip.moos.abs, MOOS.2018.per.storm.1, by = "storm.num")
HI.moos.abs.2018 <- left_join(HI.moos.abs.2018, MOOS.2018.per.storm.2, by = "storm.num")
HI.moos.abs.2018 <- left_join(HI.moos.abs.2018, MOOS.2018.per.storm.3, by = "storm.num")
HI.moos.abs.2018 <- left_join(HI.moos.abs.2018, MOOS.2018.per.storm.4, by = "storm.num")
HI.moos.abs.2018 <- left_join(HI.moos.abs.2018, MOOS.2018.per.storm.5, by = "storm.num")

moos.lm.abs <- lm(HI.moos.abs.2018$HI ~ HI.moos.abs.2018$precip) # model one with just total precip
moos.lm.abs.2 <- lm(HI.moos.abs.2018$HI ~ HI.moos.abs.2018$precip.week) # model one with just total precip
moos.lm.abs.3 <- lm(HI.moos.abs.2018$HI ~ HI.moos.abs.2018$precip.month) # model one with just total precip
moos.lm.abs.4 <- lm(HI.moos.abs.2018$HI ~ HI.moos.abs.2018$ThreeMonth) # model one with just total precip
moos.lm.abs.5 <- lm(HI.moos.abs.2018$HI ~ HI.moos.abs.2018$temp) # model one with just total precip

HI.moos.abs.2.2018 <- left_join(HI.moos.abs.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.abs.2.2018$TOTAL.TIME <- as.numeric(HI.moos.abs.2.2018$TOTAL.TIME)
HI.moos.abs.2.2018$Intensity <- HI.moos.abs.2.2018$precip/HI.moos.abs.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.moos.2018 <- rbind(HI.moos.no3.2.2018, HI.moos.fDOM.2.2018, HI.moos.SPC.2.2018, 
                      HI.moos.turb.2.2018, HI.moos.abs.2.2018) # merging all responses together 
HI.moos.2018$burn <- "burned" # adding a burn column
HI.moos.2018$pf <- "medium" # adding a pf column

HI.moos.2018$site.ID <- "MOOS"

write.csv(HI.moos.2018, here("Output_from_analysis", "04_Antecedent_Conditions", "2018", "HI.moos.2018.csv"))


# CARI #### 
CARIstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)


CARI_storms<-do.call("rbind", lapply(CARIstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms$storm.num = c(rep("storm1", 317),
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

CARI_storms$DateTime <- as.POSIXct(CARI_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI.2018.storms.1<- left_join(CARI_storms, POKE_RainGauge_2018, by = "DateTime")
CARI.2018.storms.1<- left_join(CARI.2018.storms.1, airtempmean, by = "DateTime")

names(CARI.2018.storms.1)[names(CARI.2018.storms.1) == ''] <- 'x'

CARI.2018.per.storm.1 <- CARI.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- CARI.2018.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

CARI.2018.per.storm.1$temp <- temp$temp

CARI.2018 <- CARI_storms
CARI.2018 <- CARI.2018[,-c(1,10)]
CARI.2018$DateTime <- as.POSIXct(CARI.2018$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
CARI.2018 <- left_join(CARI.2018, POKE_RainGauge_2018, by = "DateTime")
CARI.2018 <- left_join(CARI.2018, airtempmean, by = "DateTime")
CARI.2018$week <- rollapplyr(CARI.2018$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2018$month <- rollapplyr(CARI.2018$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2018$ThreeMonth <- rollapplyr(CARI.2018$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2018$temp.week <- rollapplyr(CARI.2018$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

CARI.2018.1 <- left_join(CARI.2018.storms.1, CARI.2018, by = "DateTime") # week month and 3 month precip totals 

CARI.2018.per.storm.2 <- CARI.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
CARI.2018.per.storm.3 <- CARI.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
CARI.2018.per.storm.4 <- CARI.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
CARI.2018.per.storm.5 <- CARI.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.cari.NO3 <- subset(HI.mean.precip.response, year == "2018" & site.ID == "CARI" & response == "NO3")
HI.mean.precip.cari.fDOM <- subset(HI.mean.precip.response, year == "2018" & site.ID == "CARI" & response == "fDOM")
HI.mean.precip.cari.SPC <- subset(HI.mean.precip.response, year == "2018" & site.ID == "CARI" & response == "SPC")
HI.mean.precip.cari.turb <- subset(HI.mean.precip.response, year == "2018" & site.ID == "CARI" & response == "turb")

HI.cari.no3.2018 <- left_join(HI.mean.precip.cari.NO3, CARI.2018.per.storm.1, by = "storm.num")
HI.cari.no3.2018 <- left_join(HI.cari.no3.2018, CARI.2018.per.storm.2, by = "storm.num")
HI.cari.no3.2018 <- left_join(HI.cari.no3.2018, CARI.2018.per.storm.3, by = "storm.num")
HI.cari.no3.2018 <- left_join(HI.cari.no3.2018, CARI.2018.per.storm.4, by = "storm.num")
HI.cari.no3.2018 <- left_join(HI.cari.no3.2018, CARI.2018.per.storm.5, by = "storm.num")

cari.lm.no3 <- lm(HI.cari.no3.2018$HI ~ HI.cari.no3.2018$precip) # model one with just total precip
cari.lm.no3.2 <- lm(HI.cari.no3.2018$HI ~ HI.cari.no3.2018$precip.week) # model one with just total precip
cari.lm.no3.3 <- lm(HI.cari.no3.2018$HI ~ HI.cari.no3.2018$precip.month) # model one with just total precip
cari.lm.no3.4 <- lm(HI.cari.no3.2018$HI ~ HI.cari.no3.2018$ThreeMonth) # model one with just total precip

cari.formula <- y ~ x

sa <- HI.cari.no3.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sb <- HI.cari.no3.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sc <- HI.cari.no3.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sd <- HI.cari.no3.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.cari.fDOM.2018 <- left_join(HI.mean.precip.cari.fDOM, CARI.2018.per.storm.1, by = "storm.num")
HI.cari.fDOM.2018 <- left_join(HI.cari.fDOM.2018, CARI.2018.per.storm.2, by = "storm.num")
HI.cari.fDOM.2018 <- left_join(HI.cari.fDOM.2018, CARI.2018.per.storm.3, by = "storm.num")
HI.cari.fDOM.2018 <- left_join(HI.cari.fDOM.2018, CARI.2018.per.storm.4, by = "storm.num")
HI.cari.fDOM.2018 <- left_join(HI.cari.fDOM.2018, CARI.2018.per.storm.5, by = "storm.num")

cari.lm.fDOM <- lm(HI.cari.fDOM.2018$HI ~ HI.cari.fDOM.2018$precip) # model one with just total precip
cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2018$HI ~ HI.cari.fDOM.2018$precip.week) # model one with just total precip
cari.lm.fDOM.3 <- lm(HI.cari.fDOM.2018$HI ~ HI.cari.fDOM.2018$precip.month) # model one with just total precip
cari.lm.fDOM.4 <- lm(HI.cari.fDOM.2018$HI ~ HI.cari.fDOM.2018$ThreeMonth) # model one with just total precip

se <- HI.cari.fDOM.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sf <- HI.cari.fDOM.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model

sg <- HI.cari.fDOM.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

sh <- HI.cari.fDOM.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.cari.SPC.2018 <- left_join(HI.mean.precip.cari.SPC, CARI.2018.per.storm.1, by = "storm.num")
HI.cari.SPC.2018 <- left_join(HI.cari.SPC.2018, CARI.2018.per.storm.2, by = "storm.num")
HI.cari.SPC.2018 <- left_join(HI.cari.SPC.2018, CARI.2018.per.storm.3, by = "storm.num")
HI.cari.SPC.2018 <- left_join(HI.cari.SPC.2018, CARI.2018.per.storm.4, by = "storm.num")
HI.cari.SPC.2018 <- left_join(HI.cari.SPC.2018, CARI.2018.per.storm.5, by = "storm.num")

cari.lm.SPC <- lm(HI.cari.SPC.2018$HI ~ HI.cari.SPC.2018$precip) # model one with just total precip
cari.lm.SPC.2 <- lm(HI.cari.SPC.2018$HI ~ HI.cari.SPC.2018$precip.week) # model one with just total precip
cari.lm.SPC.3 <- lm(HI.cari.SPC.2018$HI ~ HI.cari.SPC.2018$precip.month) # model one with just total precip
cari.lm.SPC.4 <- lm(HI.cari.SPC.2018$HI ~ HI.cari.SPC.2018$ThreeMonth) # model one with just total precip

si <- HI.cari.SPC.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sj <- HI.cari.SPC.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sk <- HI.cari.SPC.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sl <- HI.cari.SPC.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.cari.turb.2018 <- left_join(HI.mean.precip.cari.turb, CARI.2018.per.storm.1, by = "storm.num")
HI.cari.turb.2018 <- left_join(HI.cari.turb.2018, CARI.2018.per.storm.2, by = "storm.num")
HI.cari.turb.2018 <- left_join(HI.cari.turb.2018, CARI.2018.per.storm.3, by = "storm.num")
HI.cari.turb.2018 <- left_join(HI.cari.turb.2018, CARI.2018.per.storm.4, by = "storm.num")
HI.cari.turb.2018 <- left_join(HI.cari.turb.2018, CARI.2018.per.storm.5, by = "storm.num")

cari.lm.turb <- lm(HI.cari.turb.2018$HI ~ HI.cari.turb.2018$precip) # model one with just total precip
cari.lm.turb.2 <- lm(HI.cari.turb.2018$HI ~ HI.cari.turb.2018$precip.week) # model one with just total precip
cari.lm.turb.3 <- lm(HI.cari.turb.2018$HI ~ HI.cari.turb.2018$precip.month) # model one with just total precip
cari.lm.turb.4 <- lm(HI.cari.turb.2018$HI ~ HI.cari.turb.2018$ThreeMonth) # model one with just total precip

sm <- HI.cari.turb.2018 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sn <- HI.cari.turb.2018 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

so <- HI.cari.turb.2018 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sp <- HI.cari.turb.2018 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

#CARI.2019.storms.1 <- na.omit(CARI.2019.storms.1)

sum.time <- CARI.2018.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.cari.no3.2.2018 <- left_join(sum.time, HI.cari.no3.2018, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.no3.2.2018$TOTAL.TIME <- as.numeric(HI.cari.no3.2.2018$TOTAL.TIME)
HI.cari.no3.2.2018$Intensity <- HI.cari.no3.2.2018$precip/HI.cari.no3.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.no3.2 <- lm(HI.cari.no3.2.2018$HI ~ HI.cari.no3.2.2018$precip + HI.cari.no3.2.2018$Intensity) # model one with total precip and intensity 

sq <- HI.cari.no3.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2018 <- left_join(HI.cari.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.cari.fDOM.2.2018$TOTAL.TIME)
HI.cari.fDOM.2.2018$Intensity <- HI.cari.fDOM.2.2018$precip/HI.cari.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2.2018$HI ~ HI.cari.fDOM.2.2018$precip + HI.cari.fDOM.2.2018$Intensity) # model one with total precip and intensity 

sr <- HI.cari.fDOM.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2018 <- left_join(HI.cari.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.cari.SPC.2.2018$TOTAL.TIME)
HI.cari.SPC.2.2018$Intensity <- HI.cari.SPC.2.2018$precip/HI.cari.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.SPC.2 <- lm(HI.cari.SPC.2.2018$HI ~ HI.cari.SPC.2.2018$precip + HI.cari.SPC.2.2018$Intensity) # model one with total precip and intensity 

ss <- HI.cari.SPC.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2018 <- left_join(HI.cari.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.turb.2.2018$TOTAL.TIME <- as.numeric(HI.cari.turb.2.2018$TOTAL.TIME)
HI.cari.turb.2.2018$Intensity <- HI.cari.turb.2.2018$precip/HI.cari.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.turb.2 <- lm(HI.cari.turb.2.2018$HI ~ HI.cari.turb.2.2018$precip + HI.cari.turb.2.2018$Intensity) # model one with total precip and intensity 

st <- HI.cari.turb.2.2018 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
CARI.2018.1$day <- julian(CARI.2018.1$DateTime, origin = as.POSIXct('2018-01-01', tz = 'America/Anchorage')) # making a fractional day column 
CARI.2018.1$day <- as.numeric(CARI.2018.1$day)

CARI.2018.per.storm.5 <- CARI.2018.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.cari.no3.2.2018 <- left_join(HI.cari.no3.2.2018, CARI.2018.per.storm.5, by = "storm.num")
cari.lm.no3.5 <- lm(HI.cari.no3.2.2018$HI ~ HI.cari.no3.2.2018$doy)

su <- HI.cari.no3.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2018 <- left_join(HI.cari.fDOM.2.2018, CARI.2018.per.storm.5, by = "storm.num")
cari.lm.fDOM.5 <- lm(HI.cari.fDOM.2.2018$HI ~ HI.cari.fDOM.2.2018$doy)

sv <- HI.cari.fDOM.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2018 <- left_join(HI.cari.SPC.2.2018, CARI.2018.per.storm.5, by = "storm.num")
cari.lm.SPC.5 <- lm(HI.cari.SPC.2.2018$HI ~ HI.cari.SPC.2.2018$doy)

sw <- HI.cari.SPC.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2018 <- left_join(HI.cari.turb.2.2018, CARI.2018.per.storm.5, by = "storm.num")
cari.lm.turb.5 <- lm(HI.cari.turb.2.2018$HI ~ HI.cari.turb.2.2018$doy)

sx <- HI.cari.turb.2.2018 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,
#          ncol = 4)

HI.cari.2018 <- rbind(HI.cari.no3.2.2018, HI.cari.fDOM.2.2018, HI.cari.SPC.2.2018, HI.cari.turb.2.2018) # merging all responses together 
HI.cari.2018$burn <- "burned" # adding a burn column
HI.cari.2018$pf <- "medium" # adding a pf column

HI.cari.2018$site.ID <- "CARI"

write.csv(HI.cari.2018, here("Output_from_analysis", "04_Antecedent_Conditions", "2018", "HI.cari.2018.csv"))

# MERGE and add time since peak  Q in chena #
HI.2018 <- rbind(HI.moos.2018, HI.frch.2018, HI.cari.2018) # bind all 2018 together
HI.2018$date <- as.Date(HI.2018$doy, origin = "2018-01-01")
origin_date <- as.Date("2018-05-22")
HI.2018$TimeSinceChena <- julian(HI.2018$date, origin_date)


write.csv(HI.2018, here("Output_from_analysis", "04_Antecedent_Conditions", "2018", "HI.2018.csv"))


######################################## 2019 ############################################
setwd("Storm_Events/2019")
# import rain gauge data #
FRCH_RainGauge_2019 <- read.csv(here("Climate", "Precip", "FRCH.RainGauge.2019.csv"))
POKE_RainGauge_2019 <- read.csv(here("Climate", "Precip", "POKE.RainGauge.2019.csv"))
VAUL_RainGauge_2019 <- read.csv(here("Climate", "Precip", "VAUL.RainGauge.2019.csv"))
airtempmean <- read.csv(here("Climate", "airtempmean.csv"))

# FRCH_RainGauge_2019 <- read_csv("~/Documents/DoD_2019/RainGauge/FRCH.RainGauge.2019.csv")
# POKE_RainGauge_2019 <- read_csv("~/Documents/DoD_2019/RainGauge/POKE.RainGauge.2019.csv")
# VAUL_RainGauge_2019 <- read_csv("~/Documents/DoD_2019/RainGauge/VAUL.RainGauge.2019.csv")
# airtempmean <- read_csv("~/Documents/Storms_clean_repo/Climate/airtempmean.csv")

# convert to datetime 
FRCH_RainGauge_2019$Datetime <- ymd_hms(FRCH_RainGauge_2019$Datetime)
POKE_RainGauge_2019$DateTime <- ymd_hms(POKE_RainGauge_2019$DateTime)
VAUL_RainGauge_2019$DateTime <- ymd_hms(VAUL_RainGauge_2019$DateTime)
airtempmean$date_timeAK <- ymd_hms(airtempmean$date_timeAK)

# force to AK time 
FRCH_RainGauge_2019$Datetime <- force_tz(FRCH_RainGauge_2019$Datetime, "America/Anchorage") # converting character to datetime
POKE_RainGauge_2019$DateTime <- force_tz(POKE_RainGauge_2019$DateTime, "America/Anchorage") # converting character to datetime
VAUL_RainGauge_2019$DateTime <- force_tz(VAUL_RainGauge_2019$DateTime, "America/Anchorage") # converting character to datetime
airtempmean$date_timeAK <- force_tz(airtempmean$date_timeAK, "America/Anchorage") # converting character to datetime
names(airtempmean)[2] <- "DateTime"

# round to nearest 15 min 
FRCH_RainGauge_2019$DateTime <- lubridate::floor_date(FRCH_RainGauge_2019$Datetime, "15 minutes")
POKE_RainGauge_2019$DateTime <- lubridate::floor_date(POKE_RainGauge_2019$DateTime, "15 minutes")
VAUL_RainGauge_2019$DateTime <- lubridate::floor_date(VAUL_RainGauge_2019$DateTime, "15 minutes")

# MOOS ####
MOOSstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 702),
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

MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2019.storms.1<- left_join(MOOS_storms, FRCH_RainGauge_2019, by = "DateTime")
MOOS.2019.storms.1<- left_join(MOOS.2019.storms.1, airtempmean, by = "DateTime")

names(MOOS.2019.storms.1)[names(MOOS.2019.storms.1) == ''] <- 'x'

MOOS.2019.per.storm.1 <- MOOS.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- MOOS.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

MOOS.2019.per.storm.1$temp <- temp$temp

# Reading in chem data to join with the antecedent moisture condition data 
# Reading in chem data to join with the antecedent moisture condition data 
DOD.2019 <- read.csv(here("processed_sensor_data", "2019", "SUNA.EXO.int.corr.lab_2019.csv")) 
DOD.2019 <- DOD.2019[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                       "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]

names(DOD.2019) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")

DOD.2019$datetimeAK <- ymd_hms(DOD.2019$datetimeAK) # converting character to datetime
DOD.2019$datetimeAK <- force_tz(DOD.2019$datetimeAK, "America/Anchorage") # converting character to datetime


MOOS.2019 <-  subset(DOD.2019, site.ID == "MOOS")

MOOS.2019$DateTime <- as.POSIXct(MOOS.2019$datetimeAK)
MOOS.2019 <- left_join(MOOS.2019, FRCH_RainGauge_2019, by = "DateTime")
MOOS.2019 <- left_join(MOOS.2019, airtempmean, by = "DateTime")
MOOS.2019$week <- rollapplyr(MOOS.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2019$month <- rollapplyr(MOOS.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2019$ThreeMonth <- rollapplyr(MOOS.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2019$temp.week <- rollapplyr(MOOS.2019$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2019.1 <- left_join(MOOS.2019.storms.1, MOOS.2019, by = "DateTime") # week month and 3 month precip totals 

MOOS.2019.per.storm.2 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2019.per.storm.3 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2019.per.storm.4 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
MOOS.2019.per.storm.5 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "turb")
HI.mean.precip.moos.abs <- subset(HI.mean.precip.response, year == "2019" & site.ID == "MOOS" & response == "abs")

HI.moos.no3.2019 <- left_join(HI.mean.precip.moos.NO3, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.4, by = "storm.num")
HI.moos.no3.2019 <- left_join(HI.moos.no3.2019, MOOS.2019.per.storm.5, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$precip) # model one with just total precip
moos.lm.no3.2 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$precip.week) # model one with just total precip
moos.lm.no3.3 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$precip.month) # model one with just total precip
moos.lm.no3.4 <- lm(HI.moos.no3.2019$HI ~ HI.moos.no3.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

aa <- HI.moos.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ab <- HI.moos.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ac <- HI.moos.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ad <- HI.moos.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.fDOM.2019 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.4, by = "storm.num")
HI.moos.fDOM.2019 <- left_join(HI.moos.fDOM.2019, MOOS.2019.per.storm.5, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$precip) # model one with just total precip
moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$precip.week) # model one with just total precip
moos.lm.fDOM.3 <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$precip.month) # model one with just total precip
moos.lm.fDOM.4 <- lm(HI.moos.fDOM.2019$HI ~ HI.moos.fDOM.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

ae <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

af <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ag <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ah <- HI.moos.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.SPC.2019 <- left_join(HI.mean.precip.moos.SPC, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.4, by = "storm.num")
HI.moos.SPC.2019 <- left_join(HI.moos.SPC.2019, MOOS.2019.per.storm.5, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$precip) # model one with just total precip
moos.lm.SPC.2 <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$precip.week) # model one with just total precip
moos.lm.SPC.3 <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$precip.month) # model one with just total precip
moos.lm.SPC.4 <- lm(HI.moos.SPC.2019$HI ~ HI.moos.SPC.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

ai <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

aj <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ak <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

al <- HI.moos.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.turb.2019 <- left_join(HI.mean.precip.moos.turb, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.4, by = "storm.num")
HI.moos.turb.2019 <- left_join(HI.moos.turb.2019, MOOS.2019.per.storm.5, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$precip) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$precip.week) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$precip.month) # model one with just total precip
moos.lm.turb.4 <- lm(HI.moos.turb.2019$HI ~ HI.moos.turb.2019$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

am <- HI.moos.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

an <- HI.moos.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ao <- HI.moos.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ap <- HI.moos.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- MOOS.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.moos.no3.2.2019 <- left_join(HI.moos.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.no3.2.2019$TOTAL.TIME <- as.numeric(HI.moos.no3.2.2019$TOTAL.TIME)
HI.moos.no3.2.2019$Intensity <- HI.moos.no3.2.2019$precip/HI.moos.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.no3.2 <- lm(HI.moos.no3.2.2019$HI ~ HI.moos.no3.2.2019$precip + HI.moos.no3.2.2019$Intensity) # model one with total precip and intensity 

aq <- HI.moos.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2019 <- left_join(HI.moos.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2019$TOTAL.TIME)
HI.moos.fDOM.2.2019$Intensity <- HI.moos.fDOM.2.2019$precip/HI.moos.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2019$HI ~ HI.moos.fDOM.2.2019$precip + HI.moos.fDOM.2.2019$Intensity) # model one with total precip and intensity 

ar <- HI.moos.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2019 <- left_join(HI.moos.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2019$TOTAL.TIME)
HI.moos.SPC.2.2019$Intensity <- HI.moos.SPC.2.2019$precip/HI.moos.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2019$HI ~ HI.moos.SPC.2.2019$precip + HI.moos.SPC.2.2019$Intensity) # model one with total precip and intensity 

as <- HI.moos.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2019 <- left_join(HI.moos.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2019$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2019$TOTAL.TIME)
HI.moos.turb.2.2019$Intensity <- HI.moos.turb.2.2019$precip/HI.moos.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2019$HI ~ HI.moos.turb.2.2019$precip + HI.moos.turb.2.2019$Intensity) # model one with total precip and intensity 

at <- HI.moos.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
MOOS.2019.1$day <- julian(MOOS.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
MOOS.2019.1$day <- as.numeric(MOOS.2019.1$day)

MOOS.2019.per.storm.5 <- MOOS.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.moos.no3.2.2019 <- left_join(HI.moos.no3.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2019$HI ~ HI.moos.no3.2.2019$doy)

au <- HI.moos.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2019 <- left_join(HI.moos.fDOM.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2019$HI ~ HI.moos.fDOM.2.2019$doy)

av <- HI.moos.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2019 <- left_join(HI.moos.SPC.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2019$HI ~ HI.moos.SPC.2.2019$doy)

aw <- HI.moos.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2019 <- left_join(HI.moos.turb.2.2019, MOOS.2019.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2019$HI ~ HI.moos.turb.2.2019$doy)

ax <- HI.moos.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# abs #
HI.moos.abs.2019 <- left_join(HI.mean.precip.moos.abs, MOOS.2019.per.storm.1, by = "storm.num")
HI.moos.abs.2019 <- left_join(HI.moos.abs.2019, MOOS.2019.per.storm.2, by = "storm.num")
HI.moos.abs.2019 <- left_join(HI.moos.abs.2019, MOOS.2019.per.storm.3, by = "storm.num")
HI.moos.abs.2019 <- left_join(HI.moos.abs.2019, MOOS.2019.per.storm.4, by = "storm.num")
HI.moos.abs.2019 <- left_join(HI.moos.abs.2019, MOOS.2019.per.storm.5, by = "storm.num")

moos.lm.abs <- lm(HI.moos.abs.2019$HI ~ HI.moos.abs.2019$precip) # model one with just total precip
moos.lm.abs.2 <- lm(HI.moos.abs.2019$HI ~ HI.moos.abs.2019$precip.week) # model one with just total precip
moos.lm.abs.3 <- lm(HI.moos.abs.2019$HI ~ HI.moos.abs.2019$precip.month) # model one with just total precip
moos.lm.abs.4 <- lm(HI.moos.abs.2019$HI ~ HI.moos.abs.2019$ThreeMonth) # model one with just total precip
moos.lm.abs.5 <- lm(HI.moos.abs.2019$HI ~ HI.moos.abs.2019$temp) # model one with just total precip

HI.moos.abs.2.2019 <- left_join(HI.moos.abs.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.abs.2.2019$TOTAL.TIME <- as.numeric(HI.moos.abs.2.2019$TOTAL.TIME)
HI.moos.abs.2.2019$Intensity <- HI.moos.abs.2.2019$precip/HI.moos.abs.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.moos.2019 <- rbind(HI.moos.no3.2.2019, HI.moos.fDOM.2.2019,
                      HI.moos.SPC.2.2019, HI.moos.turb.2.2019,
                      HI.moos.abs.2.2019) # merging all responses together 
HI.moos.2019$burn <- "burned" # adding a burn column
HI.moos.2019$pf <- "medium" # adding a pf column


write.csv(HI.moos.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.moos.2019.csv"))


# FRCH ####
FRCHstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 993),
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
                          

FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime) 
FRCH.2019.storms.1<- left_join(FRCH_storms, FRCH_RainGauge_2019, by = "DateTime")
FRCH.2019.storms.1<- left_join(FRCH.2019.storms.1, airtempmean, by = "DateTime")

names(FRCH.2019.storms.1)[names(FRCH.2019.storms.1) == ''] <- 'x'

FRCH.2019.per.storm.1 <- FRCH.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- FRCH.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

FRCH.2019.per.storm.1$temp <- temp$temp

FRCH.2019 <-  subset(DOD.2019, site.ID == "FRCH")
FRCH.2019$DateTime <- as.POSIXct(FRCH.2019$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
FRCH.2019 <- left_join(FRCH.2019, FRCH_RainGauge_2019, by = "DateTime")
FRCH.2019 <- left_join(FRCH.2019, airtempmean, by = "DateTime")
FRCH.2019$week <- rollapplyr(FRCH.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2019$month <- rollapplyr(FRCH.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2019$ThreeMonth <- rollapplyr(FRCH.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2019$temp.week <- rollapplyr(FRCH.2019$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

FRCH.2019.1 <- left_join(FRCH.2019.storms.1, FRCH.2019, by = "DateTime") # week month and 3 month precip totals 

FRCH.2019.per.storm.2 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2019.per.storm.3 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2019.per.storm.4 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
FRCH.2019.per.storm.5 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "turb")
HI.mean.precip.frch.abs <- subset(HI.mean.precip.response, year == "2019" & site.ID == "FRCH" & response == "abs")

HI.frch.no3.2019 <- left_join(HI.mean.precip.frch.NO3, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.4, by = "storm.num")
HI.frch.no3.2019 <- left_join(HI.frch.no3.2019, FRCH.2019.per.storm.5, by = "storm.num")

HI.frch.no3.2019 <- HI.frch.no3.2019[,-c(6:7)]
names(HI.frch.no3.2019) <- c("site.ID", "year", "storm.num","response", "HI", "precip", "temp", "precip.week", 
                             "precip.month", "ThreeMonth", "temp.week")
frch.lm.no3 <- lm(HI.frch.no3.2019$HI ~ HI.frch.no3.2019$precip) # model one with just total precip

frch.formula <- y ~ x

ba <- HI.frch.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bb <- HI.frch.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bc <- HI.frch.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bd <- HI.frch.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.fDOM.2019 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.4, by = "storm.num")
HI.frch.fDOM.2019 <- left_join(HI.frch.fDOM.2019, FRCH.2019.per.storm.5, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2019$HI ~ HI.frch.fDOM.2019$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

be <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bf <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bg <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bh <- HI.frch.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.SPC.2019 <- left_join(HI.mean.precip.frch.SPC, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.4, by = "storm.num")
HI.frch.SPC.2019 <- left_join(HI.frch.SPC.2019, FRCH.2019.per.storm.5, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2019$HI ~ HI.frch.SPC.2019$ThreeMonth) # model one with just total precip


bi <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bj <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bk <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bl <- HI.frch.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.turb.2019 <- left_join(HI.mean.precip.frch.turb, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.4, by = "storm.num")
HI.frch.turb.2019 <- left_join(HI.frch.turb.2019, FRCH.2019.per.storm.5, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$ThreeMonth) # model one with just total precip

bm <- HI.frch.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bn <- HI.frch.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bo <- HI.frch.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bp <- HI.frch.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- FRCH.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.frch.no3.2.2019 <- left_join(HI.frch.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.no3.2.2019$TOTAL.TIME <- as.numeric(HI.frch.no3.2.2019$TOTAL.TIME)
HI.frch.no3.2.2019$Intensity <- HI.frch.no3.2.2019$precip/HI.frch.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.no3.2 <- lm(HI.frch.no3.2.2019$HI ~ HI.frch.no3.2.2019$precip + HI.frch.no3.2.2019$Intensity) # model one with total precip and intensity 

bq <- HI.frch.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2019 <- left_join(HI.frch.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2019$TOTAL.TIME)
HI.frch.fDOM.2.2019$Intensity <- HI.frch.fDOM.2.2019$precip/HI.frch.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2019$HI ~ HI.frch.fDOM.2.2019$precip + HI.frch.fDOM.2.2019$Intensity) # model one with total precip and intensity 

br <- HI.frch.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2019 <- left_join(HI.frch.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2019$TOTAL.TIME)
HI.frch.SPC.2.2019$Intensity <- HI.frch.SPC.2.2019$precip/HI.frch.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2019$HI ~ HI.frch.SPC.2.2019$precip + HI.frch.SPC.2.2019$Intensity) # model one with total precip and intensity 

bs <- HI.frch.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2019 <- left_join(HI.frch.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.turb.2.2019$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2019$TOTAL.TIME)
HI.frch.turb.2.2019$Intensity <- HI.frch.turb.2.2019$precip/HI.frch.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.turb.2 <- lm(HI.frch.turb.2.2019$HI ~ HI.frch.turb.2.2019$precip + HI.frch.turb.2.2019$Intensity) # model one with total precip and intensity 

bt <- HI.frch.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
FRCH.2019.1$day <- julian(FRCH.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
FRCH.2019.1$day <- as.numeric(FRCH.2019.1$day)

FRCH.2019.per.storm.5 <- FRCH.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.frch.no3.2.2019 <- left_join(HI.frch.no3.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2019$HI ~ HI.frch.no3.2.2019$doy)

bu <- HI.frch.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2019 <- left_join(HI.frch.fDOM.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2019$HI ~ HI.frch.fDOM.2.2019$doy)

bv <- HI.frch.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2019 <- left_join(HI.frch.SPC.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2019$HI ~ HI.frch.SPC.2.2019$doy)

bw <- HI.frch.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2019 <- left_join(HI.frch.turb.2.2019, FRCH.2019.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2019$HI ~ HI.frch.turb.2.2019$doy)

bx <- HI.frch.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# abs #
HI.frch.abs.2019 <- left_join(HI.mean.precip.frch.abs, FRCH.2019.per.storm.1, by = "storm.num")
HI.frch.abs.2019 <- left_join(HI.frch.abs.2019, FRCH.2019.per.storm.2, by = "storm.num")
HI.frch.abs.2019 <- left_join(HI.frch.abs.2019, FRCH.2019.per.storm.3, by = "storm.num")
HI.frch.abs.2019 <- left_join(HI.frch.abs.2019, FRCH.2019.per.storm.4, by = "storm.num")
HI.frch.abs.2019 <- left_join(HI.frch.abs.2019, FRCH.2019.per.storm.5, by = "storm.num")

frch.lm.abs <- lm(HI.frch.abs.2019$HI ~ HI.frch.abs.2019$precip) # model one with just total precip
frch.lm.abs.2 <- lm(HI.frch.abs.2019$HI ~ HI.frch.abs.2019$precip.week) # model one with just total precip
frch.lm.abs.3 <- lm(HI.frch.abs.2019$HI ~ HI.frch.abs.2019$precip.month) # model one with just total precip
frch.lm.abs.4 <- lm(HI.frch.abs.2019$HI ~ HI.frch.abs.2019$ThreeMonth) # model one with just total precip
frch.lm.abs.5 <- lm(HI.frch.turb.2019$HI ~ HI.frch.turb.2019$temp.week) # model one with just total precip

HI.frch.abs.2.2019 <- left_join(HI.frch.abs.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.abs.2.2019$TOTAL.TIME <- as.numeric(HI.frch.abs.2.2019$TOTAL.TIME)
HI.frch.abs.2.2019$Intensity <- HI.frch.abs.2.2019$precip/HI.frch.abs.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.frch.2019 <- rbind(HI.frch.no3.2.2019, HI.frch.fDOM.2.2019, 
                      HI.frch.SPC.2.2019, HI.frch.turb.2.2019,
                      HI.frch.abs.2.2019) # merging all responses together 
HI.frch.2019$burn <- "unburned" # adding a burn column
HI.frch.2019$pf <- "medium" # adding a pf column

write.csv(HI.frch.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.frch.2019.csv"))


# POKE ####
POKEstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 103),
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

POKE_storms$DateTime <- as.POSIXct(POKE_storms$DateTime) 
POKE.2019.storms.1<- left_join(POKE_storms, POKE_RainGauge_2019, by = "DateTime")
POKE.2019.storms.1<- left_join(POKE.2019.storms.1, airtempmean, by = "DateTime")

names(POKE.2019.storms.1)[names(POKE.2019.storms.1) == ''] <- 'x'

POKE.2019.per.storm.1 <- POKE.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- POKE.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

POKE.2019.per.storm.1$temp <- temp$temp

POKE.2019 <-  subset(DOD.2019, site.ID == "POKE")
POKE.2019$DateTime <- as.POSIXct(POKE.2019$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
POKE.2019 <- left_join(POKE.2019, POKE_RainGauge_2019, by = "DateTime")
POKE.2019 <- left_join(POKE.2019, airtempmean, by = "DateTime")
POKE.2019$week <- rollapplyr(POKE.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2019$month <- rollapplyr(POKE.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2019$ThreeMonth <- rollapplyr(POKE.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2019$temp.week <- rollapplyr(POKE.2019$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

POKE.2019.1 <- left_join(POKE.2019.storms.1, POKE.2019, by = "DateTime") # week month and 3 month precip totals 

POKE.2019.per.storm.2 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
POKE.2019.per.storm.3 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
POKE.2019.per.storm.4 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
POKE.2019.per.storm.5 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.poke.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "NO3")
HI.mean.precip.poke.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "fDOM")
HI.mean.precip.poke.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "SPC")
HI.mean.precip.poke.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "turb")
HI.mean.precip.poke.abs <- subset(HI.mean.precip.response, year == "2019" & site.ID == "POKE" & response == "abs")

HI.poke.no3.2019 <- left_join(HI.mean.precip.poke.NO3, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.4, by = "storm.num")
HI.poke.no3.2019 <- left_join(HI.poke.no3.2019, POKE.2019.per.storm.5, by = "storm.num")

poke.lm.no3 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$precip) # model one with just total precip
poke.lm.no3.2 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$precip.week) # model one with just total precip
poke.lm.no3.3 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$precip.month) # model one with just total precip
poke.lm.no3.4 <- lm(HI.poke.no3.2019$HI ~ HI.poke.no3.2019$ThreeMonth) # model one with just total precip

poke.formula <- y ~ x

pa <- HI.poke.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pb <- HI.poke.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-weeek Precip") +
  ylab("HI-Solute Storage") # plot model 

pc <- HI.poke.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pd <- HI.poke.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.fDOM.2019 <- left_join(HI.mean.precip.poke.fDOM, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.4, by = "storm.num")
HI.poke.fDOM.2019 <- left_join(HI.poke.fDOM.2019, POKE.2019.per.storm.5, by = "storm.num")

poke.lm.fDOM <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$precip) # model one with just total precip
poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$precip.week) # model one with just total precip
poke.lm.fDOM.3 <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$precip.month) # model one with just total precip
poke.lm.fDOM.4 <- lm(HI.poke.fDOM.2019$HI ~ HI.poke.fDOM.2019$ThreeMonth) # model one with just total precip

pe <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pf <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

pg <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ph <- HI.poke.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.SPC.2019 <- left_join(HI.mean.precip.poke.SPC, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.4, by = "storm.num")
HI.poke.SPC.2019 <- left_join(HI.poke.SPC.2019, POKE.2019.per.storm.5, by = "storm.num")

poke.lm.SPC <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$precip) # model one with just total precip
poke.lm.SPC.2 <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$precip.week) # model one with just total precip
poke.lm.SPC.3 <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$precip.month) # model one with just total precip
poke.lm.SPC.4 <- lm(HI.poke.SPC.2019$HI ~ HI.poke.SPC.2019$ThreeMonth) # model one with just total precip

pi <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pj <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

pk <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

pl <- HI.poke.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.poke.turb.2019 <- left_join(HI.mean.precip.poke.turb, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.4, by = "storm.num")
HI.poke.turb.2019 <- left_join(HI.poke.turb.2019, POKE.2019.per.storm.5, by = "storm.num")

poke.lm.turb <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$precip) # model one with just total precip
poke.lm.turb.2 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$precip.week) # model one with just total precip
poke.lm.turb.3 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$precip.month) # model one with just total precip
poke.lm.turb.4 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$ThreeMonth) # model one with just total precip

pm <- HI.poke.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

pn <- HI.poke.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

po <- HI.poke.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pp <- HI.poke.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- POKE.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.poke.no3.2.2019 <- left_join(HI.poke.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.no3.2.2019$TOTAL.TIME <- as.numeric(HI.poke.no3.2.2019$TOTAL.TIME)
HI.poke.no3.2.2019$Intensity <- HI.poke.no3.2.2019$precip/HI.poke.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.no3.2 <- lm(HI.poke.no3.2.2019$HI ~ HI.poke.no3.2.2019$precip + HI.poke.no3.2.2019$Intensity) # model one with total precip and intensity 

pq <- HI.poke.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2019 <- left_join(HI.poke.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.poke.fDOM.2.2019$TOTAL.TIME)
HI.poke.fDOM.2.2019$Intensity <- HI.poke.fDOM.2.2019$precip/HI.poke.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2.2019$HI ~ HI.poke.fDOM.2.2019$precip + HI.poke.fDOM.2.2019$Intensity) # model one with total precip and intensity 

pr <- HI.poke.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2019 <- left_join(HI.poke.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.poke.SPC.2.2019$TOTAL.TIME)
HI.poke.SPC.2.2019$Intensity <- HI.poke.SPC.2.2019$precip/HI.poke.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.SPC.2.2019 <- lm(HI.poke.SPC.2.2019$HI ~ HI.poke.SPC.2.2019$precip + HI.poke.SPC.2.2019$Intensity) # model one with total precip and intensity 

ps <- HI.poke.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2019 <- left_join(HI.poke.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.turb.2.2019$TOTAL.TIME <- as.numeric(HI.poke.turb.2.2019$TOTAL.TIME)
HI.poke.turb.2.2019$Intensity <- HI.poke.turb.2.2019$precip/HI.poke.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.turb.2 <- lm(HI.poke.turb.2.2019$HI ~ HI.poke.turb.2.2019$precip + HI.poke.turb.2.2019$Intensity) # model one with total precip and intensity 

pt <- HI.poke.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
POKE.2019.1$day <- julian(POKE.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
POKE.2019.1$day <- as.numeric(POKE.2019.1$day)

POKE.2019.per.storm.5 <- POKE.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.poke.no3.2.2019 <- left_join(HI.poke.no3.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.no3.5 <- lm(HI.poke.no3.2.2019$HI ~ HI.poke.no3.2.2019$doy)

pu <- HI.poke.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2019 <- left_join(HI.poke.fDOM.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2.2019$HI ~ HI.poke.fDOM.2.2019$doy)

pv <- HI.poke.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2019 <- left_join(HI.poke.SPC.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.SPC.5 <- lm(HI.poke.SPC.2.2019$HI ~ HI.poke.SPC.2.2019$doy)

pw <- HI.poke.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2019 <- left_join(HI.poke.turb.2.2019, POKE.2019.per.storm.5, by = "storm.num")
poke.lm.turb.5 <- lm(HI.poke.turb.2.2019$HI ~ HI.poke.turb.2.2019$doy)

px <- HI.poke.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pp,pq,pr,ps,pt,pu,pv,pw,px,
#  ncol = 4)

# abs #
HI.poke.abs.2019 <- left_join(HI.mean.precip.poke.abs, POKE.2019.per.storm.1, by = "storm.num")
HI.poke.abs.2019 <- left_join(HI.poke.abs.2019, POKE.2019.per.storm.2, by = "storm.num")
HI.poke.abs.2019 <- left_join(HI.poke.abs.2019, POKE.2019.per.storm.3, by = "storm.num")
HI.poke.abs.2019 <- left_join(HI.poke.abs.2019, POKE.2019.per.storm.4, by = "storm.num")
HI.poke.abs.2019 <- left_join(HI.poke.abs.2019, POKE.2019.per.storm.5, by = "storm.num")

poke.lm.abs <- lm(HI.poke.abs.2019$HI ~ HI.poke.abs.2019$precip) # model one with just total precip
poke.lm.abs.2 <- lm(HI.poke.abs.2019$HI ~ HI.poke.abs.2019$precip.week) # model one with just total precip
poke.lm.abs.3 <- lm(HI.poke.abs.2019$HI ~ HI.poke.abs.2019$precip.month) # model one with just total precip
poke.lm.abs.4 <- lm(HI.poke.abs.2019$HI ~ HI.poke.abs.2019$ThreeMonth) # model one with just total precip
poke.lm.abs.5 <- lm(HI.poke.turb.2019$HI ~ HI.poke.turb.2019$temp.week) # model one with just total precip

HI.poke.abs.2.2019 <- left_join(HI.poke.abs.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.abs.2.2019$TOTAL.TIME <- as.numeric(HI.poke.abs.2.2019$TOTAL.TIME)
HI.poke.abs.2.2019$Intensity <- HI.poke.abs.2.2019$precip/HI.poke.abs.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.poke.2019 <- rbind(HI.poke.no3.2.2019, HI.poke.fDOM.2.2019, 
                      HI.poke.SPC.2.2019, HI.poke.turb.2.2019,
                      HI.poke.abs.2.2019) # merging all responses together 
HI.poke.2019$burn <- "burned" # adding a burn column
HI.poke.2019$pf <- "medium" # adding a pf column

write.csv(HI.poke.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.poke.2019.csv"))


# VAUL ####
VAULstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm1", 191),
                          rep("storm2", 207),
                          rep("storm3", 191),
                          rep("storm4a", 307),
                          
                          rep("storm4c", 227),
                          rep("storm5", 275),
                          rep("storm6", 263),
                          rep("storm7", 107),
                          rep("storm8a", 455),
                         
                          rep("storm8c", 191))

VAUL_storms$DateTime <- as.POSIXct(VAUL_storms$DateTime) 
VAUL.2019.storms.1<- left_join(VAUL_storms, VAUL_RainGauge_2019, by = "DateTime")
VAUL.2019.storms.1<- left_join(VAUL.2019.storms.1, airtempmean, by = "DateTime")

names(VAUL.2019.storms.1)[names(VAUL.2019.storms.1) == ''] <- 'x'

VAUL.2019.per.storm.1 <- VAUL.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- VAUL.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

VAUL.2019.per.storm.1$temp <- temp$temp

VAUL.2019 <-  subset(DOD.2019, site.ID == "VAUL")
VAUL.2019$DateTime <- as.POSIXct(VAUL.2019$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
VAUL.2019 <- left_join(VAUL.2019, VAUL_RainGauge_2019, by = "DateTime")
VAUL.2019 <- left_join(VAUL.2019, airtempmean, by = "DateTime")
VAUL.2019$week <- rollapplyr(VAUL.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2019$month <- rollapplyr(VAUL.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2019$ThreeMonth <- rollapplyr(VAUL.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2019$temp.week <- rollapplyr(VAUL.2019$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

VAUL.2019.1 <- left_join(VAUL.2019.storms.1, VAUL.2019, by = "DateTime") # week month and 3 month precip totals 

VAUL.2019.per.storm.2 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
VAUL.2019.per.storm.3 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
VAUL.2019.per.storm.4 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
VAUL.2019.per.storm.5 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.vaul.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "NO3")
HI.mean.precip.vaul.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "fDOM")
HI.mean.precip.vaul.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "SPC")
HI.mean.precip.vaul.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "turb")
HI.mean.precip.vaul.abs <- subset(HI.mean.precip.response, year == "2019" & site.ID == "VAUL" & response == "abs")

HI.vaul.no3.2019 <- left_join(HI.mean.precip.vaul.NO3, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.4, by = "storm.num")
HI.vaul.no3.2019 <- left_join(HI.vaul.no3.2019, VAUL.2019.per.storm.5, by = "storm.num")

vaul.lm.no3 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$precip) # model one with just total precip
vaul.lm.no3.2 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$precip.week) # model one with just total precip
vaul.lm.no3.3 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$precip.month) # model one with just total precip
vaul.lm.no3.4 <- lm(HI.vaul.no3.2019$HI ~ HI.vaul.no3.2019$ThreeMonth) # model one with just total precip

vaul.formula <- y ~ x

va <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vb <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vc <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vd <- HI.vaul.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.fDOM.2019 <- left_join(HI.mean.precip.vaul.fDOM, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.4, by = "storm.num")
HI.vaul.fDOM.2019 <- left_join(HI.vaul.fDOM.2019, VAUL.2019.per.storm.5, by = "storm.num")

vaul.lm.fDOM <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$precip) # model one with just total precip
vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$precip.week) # model one with just total precip
vaul.lm.fDOM.3 <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$precip.month) # model one with just total precip
vaul.lm.fDOM.4 <- lm(HI.vaul.fDOM.2019$HI ~ HI.vaul.fDOM.2019$ThreeMonth) # model one with just total precip

ve <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vf <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vg <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vh <- HI.vaul.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.SPC.2019 <- left_join(HI.mean.precip.vaul.SPC, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.4, by = "storm.num")
HI.vaul.SPC.2019 <- left_join(HI.vaul.SPC.2019, VAUL.2019.per.storm.5, by = "storm.num")

vaul.lm.SPC <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$precip) # model one with just total precip
vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$precip.week) # model one with just total precip
vaul.lm.SPC.3 <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$precip.month) # model one with just total precip
vaul.lm.SPC.4 <- lm(HI.vaul.SPC.2019$HI ~ HI.vaul.SPC.2019$ThreeMonth) # model one with just total precip

vi <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vj <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vk <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vl <- HI.vaul.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.turb.2019 <- left_join(HI.mean.precip.vaul.turb, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.4, by = "storm.num")
HI.vaul.turb.2019 <- left_join(HI.vaul.turb.2019, VAUL.2019.per.storm.5, by = "storm.num")

vaul.lm.turb <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$precip) # model one with just total precip
vaul.lm.turb.2 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$precip.week) # model one with just total precip
vaul.lm.turb.3 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$precip.month) # model one with just total precip
vaul.lm.turb.4 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$ThreeMonth) # model one with just total precip

vm <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

vn <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

vo <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

vp <- HI.vaul.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- VAUL.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.vaul.no3.2.2019 <- left_join(HI.vaul.no3.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.no3.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.no3.2.2019$TOTAL.TIME)
HI.vaul.no3.2.2019$Intensity <- HI.vaul.no3.2.2019$precip/HI.vaul.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.no3.2 <- lm(HI.vaul.no3.2.2019$HI ~ HI.vaul.no3.2.2019$precip + HI.vaul.no3.2.2019$Intensity) # model one with total precip and intensity 

vq <- HI.vaul.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2019 <- left_join(HI.vaul.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.fDOM.2.2019$TOTAL.TIME)
HI.vaul.fDOM.2.2019$Intensity <- HI.vaul.fDOM.2.2019$precip/HI.vaul.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2.2019$HI ~ HI.vaul.fDOM.2.2019$precip + HI.vaul.fDOM.2.2019$Intensity) # model one with total precip and intensity 

vr <- HI.vaul.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2019 <- left_join(HI.vaul.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.SPC.2.2019$TOTAL.TIME)
HI.vaul.SPC.2.2019$Intensity <- HI.vaul.SPC.2.2019$precip/HI.vaul.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2.2019$HI ~ HI.vaul.SPC.2.2019$precip + HI.vaul.SPC.2.2019$Intensity) # model one with total precip and intensity 

vs <- HI.vaul.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2019 <- left_join(HI.vaul.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.turb.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.turb.2.2019$TOTAL.TIME)
HI.vaul.turb.2.2019$Intensity <- HI.vaul.turb.2.2019$precip/HI.vaul.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.turb.2 <- lm(HI.vaul.turb.2.2019$HI ~ HI.vaul.turb.2.2019$precip + HI.vaul.turb.2.2019$Intensity) # model one with total precip and intensity 

vt <- HI.vaul.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
VAUL.2019.1$day <- julian(VAUL.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
VAUL.2019.1$day <- as.numeric(VAUL.2019.1$day)

VAUL.2019.per.storm.5 <- VAUL.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.vaul.no3.2.2019 <- left_join(HI.vaul.no3.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.no3.5 <- lm(HI.vaul.no3.2.2019$HI ~ HI.vaul.no3.2.2019$doy)

vu <- HI.vaul.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2019 <- left_join(HI.vaul.fDOM.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2.2019$HI ~ HI.vaul.fDOM.2.2019$doy)

vv <- HI.vaul.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2019 <- left_join(HI.vaul.SPC.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2.2019$HI ~ HI.vaul.SPC.2.2019$doy)

vw <- HI.vaul.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2019 <- left_join(HI.vaul.turb.2.2019, VAUL.2019.per.storm.5, by = "storm.num")
vaul.lm.turb.5 <- lm(HI.vaul.turb.2.2019$HI ~ HI.vaul.turb.2.2019$doy)

vx <- HI.vaul.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(va,vb,vc,vd,ve,vf,vg,vh,vi,vj,vk,vl,vm,vn,vo,vp,vq,vr,vs,vt,vu,vv,vw,vx,
#        ncol = 4)

# abs #
HI.vaul.abs.2019 <- left_join(HI.mean.precip.vaul.abs, VAUL.2019.per.storm.1, by = "storm.num")
HI.vaul.abs.2019 <- left_join(HI.vaul.abs.2019, VAUL.2019.per.storm.2, by = "storm.num")
HI.vaul.abs.2019 <- left_join(HI.vaul.abs.2019, VAUL.2019.per.storm.3, by = "storm.num")
HI.vaul.abs.2019 <- left_join(HI.vaul.abs.2019, VAUL.2019.per.storm.4, by = "storm.num")
HI.vaul.abs.2019 <- left_join(HI.vaul.abs.2019, VAUL.2019.per.storm.5, by = "storm.num")

vaul.lm.abs <- lm(HI.vaul.abs.2019$HI ~ HI.vaul.abs.2019$precip) # model one with just total precip
vaul.lm.abs.2 <- lm(HI.vaul.abs.2019$HI ~ HI.vaul.abs.2019$precip.week) # model one with just total precip
vaul.lm.abs.3 <- lm(HI.vaul.abs.2019$HI ~ HI.vaul.abs.2019$precip.month) # model one with just total precip
vaul.lm.abs.4 <- lm(HI.vaul.abs.2019$HI ~ HI.vaul.abs.2019$ThreeMonth) # model one with just total precip
vaul.lm.abs.5 <- lm(HI.vaul.turb.2019$HI ~ HI.vaul.turb.2019$temp.week) # model one with just total precip

HI.vaul.abs.2.2019 <- left_join(HI.vaul.abs.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.abs.2.2019$TOTAL.TIME <- as.numeric(HI.vaul.abs.2.2019$TOTAL.TIME)
HI.vaul.abs.2.2019$Intensity <- HI.vaul.abs.2.2019$precip/HI.vaul.abs.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.vaul.2019 <- rbind(HI.vaul.no3.2.2019, HI.vaul.fDOM.2.2019, 
                      HI.vaul.SPC.2.2019, HI.vaul.turb.2.2019,
                      HI.vaul.abs.2.2019) # merging all responses together 
HI.vaul.2019$burn <- "unburned" # adding a burn column
HI.vaul.2019$pf <- "high" # adding a pf column

write.csv(HI.vaul.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.vaul.2019.csv"))


# STRT ####
STRTstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm1", 638),
                          rep("storm2", 274),
                          rep("storm3a", 1035),
                          rep("storm3b", 286),
                          rep("storm3c", 174),
                          rep("storm4", 466),
                          rep("storm5", 98),
                          rep("storm6", 246),
                          rep("storm7a", 246),
                          rep("storm7b", 266),
                          rep("storm7c", 258))

STRT_storms$DateTime <- as.POSIXct(STRT_storms$DateTime) 
STRT.2019.storms.1<- left_join(STRT_storms, FRCH_RainGauge_2019, by = "DateTime")
STRT.2019.storms.1<- left_join(STRT.2019.storms.1, airtempmean, by = "DateTime")

names(STRT.2019.storms.1)[names(STRT.2019.storms.1) == ''] <- 'x'

STRT.2019.per.storm.1 <- STRT.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- STRT.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

STRT.2019.per.storm.1$temp <- temp$temp

STRT.2019 <-  subset(DOD.2019, site.ID == "STRT")
STRT.2019$DateTime <- as.POSIXct(STRT.2019$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")

STRT.2019 <- left_join(STRT.2019, FRCH_RainGauge_2019, by = "DateTime")
STRT.2019 <- STRT.2019[,-12] # removing a datetime column that isnt "DateTime
STRT.2019 <- left_join(STRT.2019, airtempmean, by = "DateTime")
STRT.2019$week <- rollapplyr(STRT.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2019$month <- rollapplyr(STRT.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2019$ThreeMonth <- rollapplyr(STRT.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2019$temp.week <- rollapplyr(STRT.2019$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

STRT.2019.1 <- left_join(STRT.2019.storms.1, STRT.2019, by = "DateTime") # week month and 3 month precip totals 
VAUL.2019.1 <- left_join(VAUL.2019.storms.1, VAUL.2019, by = "DateTime") # week month and 3 month precip totals 

STRT.2019.per.storm.2 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
STRT.2019.per.storm.3 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
STRT.2019.per.storm.4 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
STRT.2019.per.storm.5 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.strt.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "NO3")
HI.mean.precip.strt.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "fDOM")
HI.mean.precip.strt.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "SPC")
HI.mean.precip.strt.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "turb")
HI.mean.precip.strt.abs <- subset(HI.mean.precip.response, year == "2019" & site.ID == "STRT" & response == "abs")

HI.strt.no3.2019 <- left_join(HI.mean.precip.strt.NO3, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.4, by = "storm.num")
HI.strt.no3.2019 <- left_join(HI.strt.no3.2019, STRT.2019.per.storm.5, by = "storm.num")

strt.lm.no3 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$precip) # model one with just total precip
strt.lm.no3.2 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$precip.week) # model one with just total precip
strt.lm.no3.3 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$precip.month) # model one with just total precip
strt.lm.no3.4 <- lm(HI.strt.no3.2019$HI ~ HI.strt.no3.2019$ThreeMonth) # model one with just total precip

strt.formula <- y ~ x

sa <- HI.strt.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sb <- HI.strt.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sc <- HI.strt.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sd <- HI.strt.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.strt.fDOM.2019 <- left_join(HI.mean.precip.strt.fDOM, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.4, by = "storm.num")
HI.strt.fDOM.2019 <- left_join(HI.strt.fDOM.2019, STRT.2019.per.storm.5, by = "storm.num")

strt.lm.fDOM <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$precip) # model one with just total precip
strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$precip.week) # model one with just total precip
strt.lm.fDOM.3 <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$precip.month) # model one with just total precip
strt.lm.fDOM.4 <- lm(HI.strt.fDOM.2019$HI ~ HI.strt.fDOM.2019$ThreeMonth) # model one with just total precip

se <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sf <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model

sg <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

sh <- HI.strt.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.strt.SPC.2019 <- left_join(HI.mean.precip.strt.SPC, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.4, by = "storm.num")
HI.strt.SPC.2019 <- left_join(HI.strt.SPC.2019, STRT.2019.per.storm.5, by = "storm.num")

strt.lm.SPC <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$precip) # model one with just total precip
strt.lm.SPC.2 <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$precip.week) # model one with just total precip
strt.lm.SPC.3 <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$precip.month) # model one with just total precip
strt.lm.SPC.4 <- lm(HI.strt.SPC.2019$HI ~ HI.strt.SPC.2019$ThreeMonth) # model one with just total precip

si <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sj <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sk <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sl <- HI.strt.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.turb.2019 <- left_join(HI.mean.precip.strt.turb, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.4, by = "storm.num")
HI.strt.turb.2019 <- left_join(HI.strt.turb.2019, STRT.2019.per.storm.5, by = "storm.num")

strt.lm.turb <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$precip) # model one with just total precip
strt.lm.turb.2 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$precip.week) # model one with just total precip
strt.lm.turb.3 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$precip.month) # model one with just total precip
strt.lm.turb.4 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$ThreeMonth) # model one with just total precip

sm <- HI.strt.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sn <- HI.strt.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

so <- HI.strt.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sp <- HI.strt.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

#STRT.2019.storms.1 <- na.omit(STRT.2019.storms.1)

sum.time <- STRT.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.strt.no3.2.2019 <- left_join(sum.time, HI.strt.no3.2019, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.no3.2.2019$TOTAL.TIME <- as.numeric(HI.strt.no3.2.2019$TOTAL.TIME)
HI.strt.no3.2.2019$Intensity <- HI.strt.no3.2.2019$precip/HI.strt.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.no3.2 <- lm(HI.strt.no3.2.2019$HI ~ HI.strt.no3.2.2019$precip + HI.strt.no3.2.2019$Intensity) # model one with total precip and intensity 

sq <- HI.strt.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2019 <- left_join(HI.strt.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.strt.fDOM.2.2019$TOTAL.TIME)
HI.strt.fDOM.2.2019$Intensity <- HI.strt.fDOM.2.2019$precip/HI.strt.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2.2019$HI ~ HI.strt.fDOM.2.2019$precip + HI.strt.fDOM.2.2019$Intensity) # model one with total precip and intensity 

sr <- HI.strt.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2019 <- left_join(HI.strt.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.strt.SPC.2.2019$TOTAL.TIME)
HI.strt.SPC.2.2019$Intensity <- HI.strt.SPC.2.2019$precip/HI.strt.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.SPC.2 <- lm(HI.strt.SPC.2.2019$HI ~ HI.strt.SPC.2.2019$precip + HI.strt.SPC.2.2019$Intensity) # model one with total precip and intensity 

ss <- HI.strt.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2019 <- left_join(HI.strt.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.turb.2.2019$TOTAL.TIME <- as.numeric(HI.strt.turb.2.2019$TOTAL.TIME)
HI.strt.turb.2.2019$Intensity <- HI.strt.turb.2.2019$precip/HI.strt.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.turb.2 <- lm(HI.strt.turb.2.2019$HI ~ HI.strt.turb.2.2019$precip + HI.strt.turb.2.2019$Intensity) # model one with total precip and intensity 

st <- HI.strt.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
STRT.2019.1$day <- julian(STRT.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
STRT.2019.1$day <- as.numeric(STRT.2019.1$day)

STRT.2019.per.storm.5 <- STRT.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.strt.no3.2.2019 <- left_join(HI.strt.no3.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.no3.5 <- lm(HI.strt.no3.2.2019$HI ~ HI.strt.no3.2.2019$doy)

su <- HI.strt.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2019 <- left_join(HI.strt.fDOM.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.fDOM.5 <- lm(HI.strt.fDOM.2.2019$HI ~ HI.strt.fDOM.2.2019$doy)

sv <- HI.strt.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2019 <- left_join(HI.strt.SPC.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.SPC.5 <- lm(HI.strt.SPC.2.2019$HI ~ HI.strt.SPC.2.2019$doy)

sw <- HI.strt.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2019 <- left_join(HI.strt.turb.2.2019, STRT.2019.per.storm.5, by = "storm.num")
strt.lm.turb.5 <- lm(HI.strt.turb.2.2019$HI ~ HI.strt.turb.2.2019$doy)

sx <- HI.strt.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,
#          ncol = 4)

# abs #
HI.strt.abs.2019 <- left_join(HI.mean.precip.strt.abs, STRT.2019.per.storm.1, by = "storm.num")
HI.strt.abs.2019 <- left_join(HI.strt.abs.2019, STRT.2019.per.storm.2, by = "storm.num")
HI.strt.abs.2019 <- left_join(HI.strt.abs.2019, STRT.2019.per.storm.3, by = "storm.num")
HI.strt.abs.2019 <- left_join(HI.strt.abs.2019, STRT.2019.per.storm.4, by = "storm.num")
HI.strt.abs.2019 <- left_join(HI.strt.abs.2019, STRT.2019.per.storm.5, by = "storm.num")

strt.lm.abs <- lm(HI.strt.abs.2019$HI ~ HI.strt.abs.2019$precip) # model one with just total precip
strt.lm.abs.2 <- lm(HI.strt.abs.2019$HI ~ HI.strt.abs.2019$precip.week) # model one with just total precip
strt.lm.abs.3 <- lm(HI.strt.abs.2019$HI ~ HI.strt.abs.2019$precip.month) # model one with just total precip
strt.lm.abs.4 <- lm(HI.strt.abs.2019$HI ~ HI.strt.abs.2019$ThreeMonth) # model one with just total precip
strt.lm.abs.5 <- lm(HI.strt.turb.2019$HI ~ HI.strt.turb.2019$temp.week) # model one with just total precip

HI.strt.abs.2.2019 <- left_join(HI.strt.abs.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.abs.2.2019$TOTAL.TIME <- as.numeric(HI.strt.abs.2.2019$TOTAL.TIME)
HI.strt.abs.2.2019$Intensity <- HI.strt.abs.2.2019$precip/HI.strt.abs.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

HI.strt.no3.2.2019 <- HI.strt.no3.2.2019[c("site.ID", "year", "storm.num", "response",
                                           "HI", "precip", "temp", "precip.week",
                                           "precip.month", "ThreeMonth", "temp.week",
                                           "TOTAL.TIME", "Intensity", "doy")] # reordering

HI.strt.abs.2.2019$temp.week <- NA
HI.strt.abs.2.2019 <- HI.strt.abs.2.2019[c("site.ID", "year", "storm.num", "response",
                                           "HI", "precip", "temp", "precip.week",
                                           "precip.month", "ThreeMonth", "temp.week",
                                           "TOTAL.TIME", "Intensity", "doy")] # reordering

HI.strt.2019 <- rbind(HI.strt.no3.2.2019, HI.strt.fDOM.2.2019,
                      HI.strt.SPC.2.2019, HI.strt.turb.2.2019,
                      HI.strt.abs.2.2019)
                      
HI.strt.2019$burn <- "burned" # adding a burn column
HI.strt.2019$pf <- "high" # adding a pf column

write.csv(HI.strt.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.strt.2019.csv"))


# CARI ####
CARIstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

CARI_storms<-do.call("rbind", lapply(CARIstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms$storm.num = c(rep("storm1", 371),
                          rep("storm2", 143),
                          rep("storm3", 83),
                          rep("storm4", 147),
                          rep("storm5", 135),
                          rep("storm6a", 319),
                         
                          rep("storm6c", 465),
                          rep("storm6d", 121),
                          rep("storm7a", 271),
                          
                          rep("storm8", 267))

CARI_storms$DateTime <- as.POSIXct(CARI_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI.2019.storms.1<- left_join(CARI_storms, POKE_RainGauge_2019, by = "DateTime")
CARI.2019.storms.1<- left_join(CARI.2019.storms.1, airtempmean, by = "DateTime")

names(CARI.2019.storms.1)[names(CARI.2019.storms.1) == ''] <- 'x'

CARI.2019.per.storm.1 <- CARI.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- CARI.2019.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

CARI.2019.per.storm.1$temp <- temp$temp


CARI.2019 <- CARI_storms
CARI.2019 <- CARI.2019[,-c(1,3)]
CARI.2019$DateTime <- as.POSIXct(CARI.2019$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
CARI.2019 <- left_join(CARI.2019, POKE_RainGauge_2019, by = "DateTime")
CARI.2019 <- left_join(CARI.2019, airtempmean, by = "DateTime")
CARI.2019$week <- rollapplyr(CARI.2019$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2019$month <- rollapplyr(CARI.2019$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2019$ThreeMonth <- rollapplyr(CARI.2019$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2019$temp.week <- rollapplyr(CARI.2019$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

CARI.2019.1 <- CARI.2019
# CARI.2019.1 <- left_join(CARI.2019.storms.1, CARI.2019, by = "DateTime") # week month and 3 month precip totals 

CARI.2019.per.storm.2 <- CARI.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
CARI.2019.per.storm.3 <- CARI.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
CARI.2019.per.storm.4 <- CARI.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
CARI.2019.per.storm.5 <- CARI.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.cari.NO3 <- subset(HI.mean.precip.response, year == "2019" & site.ID == "CARI" & response == "NO3")
HI.mean.precip.cari.fDOM <- subset(HI.mean.precip.response, year == "2019" & site.ID == "CARI" & response == "fDOM")
HI.mean.precip.cari.SPC <- subset(HI.mean.precip.response, year == "2019" & site.ID == "CARI" & response == "SPC")
HI.mean.precip.cari.turb <- subset(HI.mean.precip.response, year == "2019" & site.ID == "CARI" & response == "turb")

HI.cari.no3.2019 <- left_join(HI.mean.precip.cari.NO3, CARI.2019.per.storm.1, by = "storm.num")
HI.cari.no3.2019 <- left_join(HI.cari.no3.2019, CARI.2019.per.storm.2, by = "storm.num")
HI.cari.no3.2019 <- left_join(HI.cari.no3.2019, CARI.2019.per.storm.3, by = "storm.num")
HI.cari.no3.2019 <- left_join(HI.cari.no3.2019, CARI.2019.per.storm.4, by = "storm.num")
HI.cari.no3.2019 <- left_join(HI.cari.no3.2019, CARI.2019.per.storm.5, by = "storm.num")

cari.lm.no3 <- lm(HI.cari.no3.2019$HI ~ HI.cari.no3.2019$precip) # model one with just total precip
cari.lm.no3.2 <- lm(HI.cari.no3.2019$HI ~ HI.cari.no3.2019$precip.week) # model one with just total precip
cari.lm.no3.3 <- lm(HI.cari.no3.2019$HI ~ HI.cari.no3.2019$precip.month) # model one with just total precip
cari.lm.no3.4 <- lm(HI.cari.no3.2019$HI ~ HI.cari.no3.2019$ThreeMonth) # model one with just total precip

cari.formula <- y ~ x

sa <- HI.cari.no3.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sb <- HI.cari.no3.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sc <- HI.cari.no3.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sd <- HI.cari.no3.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.cari.fDOM.2019 <- left_join(HI.mean.precip.cari.fDOM, CARI.2019.per.storm.1, by = "storm.num")
HI.cari.fDOM.2019 <- left_join(HI.cari.fDOM.2019, CARI.2019.per.storm.2, by = "storm.num")
HI.cari.fDOM.2019 <- left_join(HI.cari.fDOM.2019, CARI.2019.per.storm.3, by = "storm.num")
HI.cari.fDOM.2019 <- left_join(HI.cari.fDOM.2019, CARI.2019.per.storm.4, by = "storm.num")
HI.cari.fDOM.2019 <- left_join(HI.cari.fDOM.2019, CARI.2019.per.storm.5, by = "storm.num")

cari.lm.fDOM <- lm(HI.cari.fDOM.2019$HI ~ HI.cari.fDOM.2019$precip) # model one with just total precip
cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2019$HI ~ HI.cari.fDOM.2019$precip.week) # model one with just total precip
cari.lm.fDOM.3 <- lm(HI.cari.fDOM.2019$HI ~ HI.cari.fDOM.2019$precip.month) # model one with just total precip
cari.lm.fDOM.4 <- lm(HI.cari.fDOM.2019$HI ~ HI.cari.fDOM.2019$ThreeMonth) # model one with just total precip

se <- HI.cari.fDOM.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sf <- HI.cari.fDOM.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model

sg <- HI.cari.fDOM.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

sh <- HI.cari.fDOM.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.cari.SPC.2019 <- left_join(HI.mean.precip.cari.SPC, CARI.2019.per.storm.1, by = "storm.num")
HI.cari.SPC.2019 <- left_join(HI.cari.SPC.2019, CARI.2019.per.storm.2, by = "storm.num")
HI.cari.SPC.2019 <- left_join(HI.cari.SPC.2019, CARI.2019.per.storm.3, by = "storm.num")
HI.cari.SPC.2019 <- left_join(HI.cari.SPC.2019, CARI.2019.per.storm.4, by = "storm.num")
HI.cari.SPC.2019 <- left_join(HI.cari.SPC.2019, CARI.2019.per.storm.5, by = "storm.num")

cari.lm.SPC <- lm(HI.cari.SPC.2019$HI ~ HI.cari.SPC.2019$precip) # model one with just total precip
cari.lm.SPC.2 <- lm(HI.cari.SPC.2019$HI ~ HI.cari.SPC.2019$precip.week) # model one with just total precip
cari.lm.SPC.3 <- lm(HI.cari.SPC.2019$HI ~ HI.cari.SPC.2019$precip.month) # model one with just total precip
cari.lm.SPC.4 <- lm(HI.cari.SPC.2019$HI ~ HI.cari.SPC.2019$ThreeMonth) # model one with just total precip

si <- HI.cari.SPC.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sj <- HI.cari.SPC.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sk <- HI.cari.SPC.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sl <- HI.cari.SPC.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.cari.turb.2019 <- left_join(HI.mean.precip.cari.turb, CARI.2019.per.storm.1, by = "storm.num")
HI.cari.turb.2019 <- left_join(HI.cari.turb.2019, CARI.2019.per.storm.2, by = "storm.num")
HI.cari.turb.2019 <- left_join(HI.cari.turb.2019, CARI.2019.per.storm.3, by = "storm.num")
HI.cari.turb.2019 <- left_join(HI.cari.turb.2019, CARI.2019.per.storm.4, by = "storm.num")
HI.cari.turb.2019 <- left_join(HI.cari.turb.2019, CARI.2019.per.storm.5, by = "storm.num")

cari.lm.turb <- lm(HI.cari.turb.2019$HI ~ HI.cari.turb.2019$precip) # model one with just total precip
cari.lm.turb.2 <- lm(HI.cari.turb.2019$HI ~ HI.cari.turb.2019$precip.week) # model one with just total precip
cari.lm.turb.3 <- lm(HI.cari.turb.2019$HI ~ HI.cari.turb.2019$precip.month) # model one with just total precip
cari.lm.turb.4 <- lm(HI.cari.turb.2019$HI ~ HI.cari.turb.2019$ThreeMonth) # model one with just total precip

sm <- HI.cari.turb.2019 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sn <- HI.cari.turb.2019 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

so <- HI.cari.turb.2019 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sp <- HI.cari.turb.2019 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

#CARI.2019.storms.1 <- na.omit(CARI.2019.storms.1)

sum.time <- CARI.2019.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.cari.no3.2.2019 <- left_join(sum.time, HI.cari.no3.2019, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.no3.2.2019$TOTAL.TIME <- as.numeric(HI.cari.no3.2.2019$TOTAL.TIME)
HI.cari.no3.2.2019$Intensity <- HI.cari.no3.2.2019$precip/HI.cari.no3.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.no3.2 <- lm(HI.cari.no3.2.2019$HI ~ HI.cari.no3.2.2019$precip + HI.cari.no3.2.2019$Intensity) # model one with total precip and intensity 

sq <- HI.cari.no3.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2019 <- left_join(HI.cari.fDOM.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.fDOM.2.2019$TOTAL.TIME <- as.numeric(HI.cari.fDOM.2.2019$TOTAL.TIME)
HI.cari.fDOM.2.2019$Intensity <- HI.cari.fDOM.2.2019$precip/HI.cari.fDOM.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2.2019$HI ~ HI.cari.fDOM.2.2019$precip + HI.cari.fDOM.2.2019$Intensity) # model one with total precip and intensity 

sr <- HI.cari.fDOM.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2019 <- left_join(HI.cari.SPC.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.SPC.2.2019$TOTAL.TIME <- as.numeric(HI.cari.SPC.2.2019$TOTAL.TIME)
HI.cari.SPC.2.2019$Intensity <- HI.cari.SPC.2.2019$precip/HI.cari.SPC.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.SPC.2 <- lm(HI.cari.SPC.2.2019$HI ~ HI.cari.SPC.2.2019$precip + HI.cari.SPC.2.2019$Intensity) # model one with total precip and intensity 

ss <- HI.cari.SPC.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2019 <- left_join(HI.cari.turb.2019, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.turb.2.2019$TOTAL.TIME <- as.numeric(HI.cari.turb.2.2019$TOTAL.TIME)
HI.cari.turb.2.2019$Intensity <- HI.cari.turb.2.2019$precip/HI.cari.turb.2.2019$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.turb.2 <- lm(HI.cari.turb.2.2019$HI ~ HI.cari.turb.2.2019$precip + HI.cari.turb.2.2019$Intensity) # model one with total precip and intensity 

st <- HI.cari.turb.2.2019 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
CARI.2019.1$day <- julian(CARI.2019.1$DateTime, origin = as.POSIXct('2019-01-01', tz = 'America/Anchorage')) # making a fractional day column 
CARI.2019.1$day <- as.numeric(CARI.2019.1$day)

CARI.2019.per.storm.5 <- CARI.2019.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.cari.no3.2.2019 <- left_join(HI.cari.no3.2.2019, CARI.2019.per.storm.5, by = "storm.num")
cari.lm.no3.5 <- lm(HI.cari.no3.2.2019$HI ~ HI.cari.no3.2.2019$doy)

su <- HI.cari.no3.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2019 <- left_join(HI.cari.fDOM.2.2019, CARI.2019.per.storm.5, by = "storm.num")
cari.lm.fDOM.5 <- lm(HI.cari.fDOM.2.2019$HI ~ HI.cari.fDOM.2.2019$doy)

sv <- HI.cari.fDOM.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2019 <- left_join(HI.cari.SPC.2.2019, CARI.2019.per.storm.5, by = "storm.num")
cari.lm.SPC.5 <- lm(HI.cari.SPC.2.2019$HI ~ HI.cari.SPC.2.2019$doy)

sw <- HI.cari.SPC.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2019 <- left_join(HI.cari.turb.2.2019, CARI.2019.per.storm.5, by = "storm.num")
cari.lm.turb.5 <- lm(HI.cari.turb.2.2019$HI ~ HI.cari.turb.2.2019$doy)

sx <- HI.cari.turb.2.2019 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,
#          ncol = 4)

HI.cari.2019 <- rbind(HI.cari.no3.2.2019, HI.cari.fDOM.2.2019, HI.cari.SPC.2.2019, HI.cari.turb.2.2019) # merging all responses together 
HI.cari.2019$burn <- "burned" # adding a burn column
HI.cari.2019$pf <- "medium" # adding a pf column


write.csv(HI.cari.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.cari.2019.csv"))


HI.2019 <- rbind(HI.moos.2019, HI.frch.2019, HI.poke.2019, 
                 HI.vaul.2019, HI.strt.2019, HI.cari.2019) # bind all 2019 together

# add time since peak  Q in chena #
HI.2019$date <- as.Date(HI.2019$doy, origin = "2019-01-01")
origin_date <- as.Date("2019-05-12")
HI.2019$TimeSinceChena <- julian(HI.2019$date, origin_date)

write.csv(HI.2019, here("Output_from_analysis", "04_Antecedent_Conditions", "2019", "HI.2019.csv"))




######################################## 2020 ####
setwd("Storm_Events/2020")
# import rain gauge data #
FRCH_RainGauge_2020 <- read.csv(here("Climate", "Precip", "FRCH.RainGauge.2020.csv"))
POKE_RainGauge_2020 <- read.csv(here("Climate", "Precip", "POKE.RainGauge.2020.csv"))
VAUL_RainGauge_2020 <- read.csv(here("Climate", "Precip", "VAUL.RainGauge.2020.csv"))
STRT_RainGauge_2020 <- read.csv(here("Climate", "Precip", "STRT.RainGauge.2020.csv"))

airtempmean <- read.csv(here("Climate", "airtempmean.csv"))

names(airtempmean)[2] <- "DateTime"

# convert to date time 
FRCH_RainGauge_2020$DateTime <- ymd_hms(FRCH_RainGauge_2020$DateTime) # converting character to datetime
POKE_RainGauge_2020$DateTime <- ymd_hms(POKE_RainGauge_2020$DateTime) # converting character to datetime
VAUL_RainGauge_2020$DateTime <- ymd_hms(VAUL_RainGauge_2020$DateTime) # converting character to datetime
STRT_RainGauge_2020$DateTime <- ymd_hms(STRT_RainGauge_2020$DateTime) # converting character to datetime
airtempmean$DateTime <- ymd_hms(airtempmean$DateTime ) # converting character to datetime

# Force to AK time 
FRCH_RainGauge_2020$DateTime <- force_tz(FRCH_RainGauge_2020$DateTime, "America/Anchorage") # converting character to datetime
POKE_RainGauge_2020$DateTime <- force_tz(POKE_RainGauge_2020$DateTime, "America/Anchorage") # converting character to datetime
VAUL_RainGauge_2020$DateTime <- force_tz(VAUL_RainGauge_2020$DateTime, "America/Anchorage") # converting character to datetime
STRT_RainGauge_2020$DateTime <- force_tz(STRT_RainGauge_2020$DateTime, "America/Anchorage") # converting character to datetime
airtempmean$DateTime <- force_tz(airtempmean$DateTime, "America/Anchorage") # converting character to datetime

# round to nearest 15 min 
FRCH_RainGauge_2020$DateTime <- lubridate::floor_date(FRCH_RainGauge_2020$DateTime, "15 minutes")
POKE_RainGauge_2020$DateTime <- lubridate::floor_date(POKE_RainGauge_2020$DateTime, "15 minutes")
VAUL_RainGauge_2020$DateTime <- lubridate::floor_date(VAUL_RainGauge_2020$DateTime, "15 minutes")
STRT_RainGauge_2020$DateTime <- lubridate::floor_date(STRT_RainGauge_2020$DateTime, "15 minutes")

# MOOS ####
MOOSstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)


MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 723),
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

MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime) 
MOOS.2020.storms.1<- left_join(MOOS_storms, FRCH_RainGauge_2020, by = "DateTime")
MOOS.2020.storms.1<- left_join(MOOS.2020.storms.1, airtempmean, by = "DateTime")


names(MOOS.2020.storms.1)[names(MOOS.2020.storms.1) == ''] <- 'x'

MOOS.2020.per.storm.1 <- MOOS.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- MOOS.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

MOOS.2020.per.storm.1$temp <- temp$temp


chem.2020 <- read.csv(here("processed_sensor_data", "2020", "SUNA.EXO.int.corr.lab_2020.csv"))

chem.2020 <- chem.2020[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]

chem.2020$datetimeAK <- ymd_hms(chem.2020$datetimeAK) # converting character to datetime
chem.2020$datetimeAK <- force_tz(chem.2020$datetimeAK, "America/Anchorage") # converting character to datetime

names(chem.2020) <- c("datetimeAK", "site.ID", "fDOM.QSU", "SpCond.µS.cm", "Turbidity.FNU", "nitrateuM", "ABS_254")

MOOS.2020 <-  subset(chem.2020, site.ID == "MOOS")


MOOS.2020$DateTime <- as.POSIXct(MOOS.2020$datetimeAK)
MOOS.2020 <- left_join(MOOS.2020, FRCH_RainGauge_2020, by = "DateTime")
MOOS.2020 <- left_join(MOOS.2020, airtempmean, by = "DateTime")
MOOS.2020$week <- rollapplyr(MOOS.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2020$month <- rollapplyr(MOOS.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2020$ThreeMonth <- rollapplyr(MOOS.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2020$temp.week <- rollapplyr(MOOS.2020$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2020.1 <- left_join(MOOS.2020.storms.1, MOOS.2020, by = "DateTime") # week month and 3 month precip totals 

MOOS.2020.per.storm.2 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2020.per.storm.3 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2020.per.storm.4 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
MOOS.2020.per.storm.5 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "turb")
HI.mean.precip.moos.abs <- subset(HI.mean.precip.response, year == "2020" & site.ID == "MOOS" & response == "abs")

HI.moos.no3.2020 <- left_join(HI.mean.precip.moos.NO3, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.4, by = "storm.num")
HI.moos.no3.2020 <- left_join(HI.moos.no3.2020, MOOS.2020.per.storm.5, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$precip) # model one with just total precip
moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$precip.week) # model one with just total precip
moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$precip.month) # model one with just total precip
moos.lm.no3 <- lm(HI.moos.no3.2020$HI ~ HI.moos.no3.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

aaa <- HI.moos.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bbb <- HI.moos.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ccc <- HI.moos.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ddd <- HI.moos.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.fDOM.2020 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.4, by = "storm.num")
HI.moos.fDOM.2020 <- left_join(HI.moos.fDOM.2020, MOOS.2020.per.storm.5, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$precip) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$precip.week) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$precip.month) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2020$HI ~ HI.moos.fDOM.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

eee <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

fff <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ggg <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

hhh <- HI.moos.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.SPC.2020 <- left_join(HI.mean.precip.moos.SPC, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.4, by = "storm.num")
HI.moos.SPC.2020 <- left_join(HI.moos.SPC.2020, MOOS.2020.per.storm.5, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$precip) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$precip.week) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$precip.month) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2020$HI ~ HI.moos.SPC.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

iii <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

jjj <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

kkk <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

lll <- HI.moos.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.turb.2020 <- left_join(HI.mean.precip.moos.turb, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.4, by = "storm.num")
HI.moos.turb.2020 <- left_join(HI.moos.turb.2020, MOOS.2020.per.storm.5, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$precip) # model one with just total precip
moos.lm.turb.1 <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$precip.week) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$precip.month) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2020$HI ~ HI.moos.turb.2020$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

mmm <- HI.moos.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

nnn <- HI.moos.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ooo <- HI.moos.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppp <- HI.moos.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- MOOS.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.moos.no3.2.2020 <- left_join(HI.moos.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.no3.2.2020$TOTAL.TIME <- as.numeric(HI.moos.no3.2.2020$TOTAL.TIME)
HI.moos.no3.2.2020$Intensity <- HI.moos.no3.2.2020$precip/HI.moos.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.no3.2 <- lm(HI.moos.no3.2.2020$HI ~ HI.moos.no3.2.2020$precip + HI.moos.no3.2.2020$Intensity) # model one with total precip and intensity 

qqq <- HI.moos.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2020 <- left_join(HI.moos.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2020$TOTAL.TIME)
HI.moos.fDOM.2.2020$Intensity <- HI.moos.fDOM.2.2020$precip/HI.moos.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2020$HI ~ HI.moos.fDOM.2.2020$precip + HI.moos.fDOM.2.2020$Intensity) # model one with total precip and intensity 

rrr <- HI.moos.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2020 <- left_join(HI.moos.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2020$TOTAL.TIME)
HI.moos.SPC.2.2020$Intensity <- HI.moos.SPC.2.2020$precip/HI.moos.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2020$HI ~ HI.moos.SPC.2.2020$precip + HI.moos.SPC.2.2020$Intensity) # model one with total precip and intensity 

sss <- HI.moos.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2020 <- left_join(HI.moos.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2020$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2020$TOTAL.TIME)
HI.moos.turb.2.2020$Intensity <- HI.moos.turb.2.2020$precip/HI.moos.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2020$HI ~ HI.moos.turb.2.2020$precip + HI.moos.turb.2.2020$Intensity) # model one with total precip and intensity 

ttt <- HI.moos.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
MOOS.2020.1$day <- julian(MOOS.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
MOOS.2020.1$day <- as.numeric(MOOS.2020.1$day)
MOOS.2020.per.storm.5 <- MOOS.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.moos.no3.2.2020 <- left_join(HI.moos.no3.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2020$HI ~ HI.moos.no3.2.2020$doy)

uuu <- HI.moos.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.fDOM.2.2020 <- left_join(HI.moos.fDOM.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2020$HI ~ HI.moos.fDOM.2.2020$doy)

utb <- HI.moos.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2020 <- left_join(HI.moos.SPC.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2020$HI ~ HI.moos.SPC.2.2020$doy)

vvv <- HI.moos.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2020 <- left_join(HI.moos.turb.2.2020, MOOS.2020.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2020$HI ~ HI.moos.turb.2.2020$doy)

www <- HI.moos.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(aaa,bbb,ccc,ddd,eee,fff,ggg,hhh,iii,jjj,kkk,lll,mmm,nnn,ooo,ppp,qqq,rrr,sss,ttt,uuu,utb,vvv,www,
#          ncol = 4)

# abs #
HI.moos.abs.2020 <- left_join(HI.mean.precip.moos.abs, MOOS.2020.per.storm.1, by = "storm.num")
HI.moos.abs.2020 <- left_join(HI.moos.abs.2020, MOOS.2020.per.storm.2, by = "storm.num")
HI.moos.abs.2020 <- left_join(HI.moos.abs.2020, MOOS.2020.per.storm.3, by = "storm.num")
HI.moos.abs.2020 <- left_join(HI.moos.abs.2020, MOOS.2020.per.storm.4, by = "storm.num")
HI.moos.abs.2020 <- left_join(HI.moos.abs.2020, MOOS.2020.per.storm.5, by = "storm.num")

moos.lm.abs <- lm(HI.moos.abs.2020$HI ~ HI.moos.abs.2020$precip) # model one with just total precip
moos.lm.abs.2 <- lm(HI.moos.abs.2020$HI ~ HI.moos.abs.2020$precip.week) # model one with just total precip
moos.lm.abs.3 <- lm(HI.moos.abs.2020$HI ~ HI.moos.abs.2020$precip.month) # model one with just total precip
moos.lm.abs.4 <- lm(HI.moos.abs.2020$HI ~ HI.moos.abs.2020$ThreeMonth) # model one with just total precip
moos.lm.abs.5 <- lm(HI.moos.abs.2020$HI ~ HI.moos.abs.2020$temp) # model one with just total precip

HI.moos.abs.2.2020 <- left_join(HI.moos.abs.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.abs.2.2020$TOTAL.TIME <- as.numeric(HI.moos.abs.2.2020$TOTAL.TIME)
HI.moos.abs.2.2020$Intensity <- HI.moos.abs.2.2020$precip/HI.moos.abs.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr



HI.moos.2020 <- rbind(HI.moos.no3.2.2020, HI.moos.fDOM.2.2020, 
                      HI.moos.SPC.2.2020, HI.moos.turb.2.2020,
                      HI.moos.abs.2.2020) # merging all responses together 
HI.moos.2020$burn <- "burned" # adding a burn column
HI.moos.2020$pf <- "medium" # adding a pf column

write.csv(HI.moos.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.moos.2020.csv"))


# FRCH ####
FRCHstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 487),
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

FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime) 
FRCH.2020.storms.1<- left_join(FRCH_storms, FRCH_RainGauge_2020, by = "DateTime")
FRCH.2020.storms.1<- left_join(FRCH.2020.storms.1, airtempmean, by = "DateTime")

names(FRCH.2020.storms.1)[names(FRCH.2020.storms.1) == ''] <- 'x'

FRCH.2020.per.storm.1 <- FRCH.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- FRCH.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

FRCH.2020.per.storm.1$temp <- temp$temp

FRCH.2020 <-  subset(chem.2020, site.ID == "FRCH")


FRCH.2020$DateTime <- as.POSIXct(FRCH.2020$datetimeAK)
FRCH.2020 <- left_join(FRCH.2020, FRCH_RainGauge_2020, by = "DateTime")
FRCH.2020 <- left_join(FRCH.2020, airtempmean, by = "DateTime")
FRCH.2020$week <- rollapplyr(FRCH.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2020$month <- rollapplyr(FRCH.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2020$ThreeMonth <- rollapplyr(FRCH.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2020$temp.week <- rollapplyr(FRCH.2020$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

FRCH.2020.1 <- left_join(FRCH.2020.storms.1, FRCH.2020, by = "DateTime") # week month and 3 month precip totals 

FRCH.2020.per.storm.2 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2020.per.storm.3 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2020.per.storm.4 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
FRCH.2020.per.storm.5 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "turb")
HI.mean.precip.frch.abs <- subset(HI.mean.precip.response, year == "2020" & site.ID == "FRCH" & response == "abs")

HI.frch.no3.2020 <- left_join(HI.mean.precip.frch.NO3, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.4, by = "storm.num")
HI.frch.no3.2020 <- left_join(HI.frch.no3.2020, FRCH.2020.per.storm.5, by = "storm.num")

frch.lm.no3 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$precip) # model one with just total precip
frch.lm.no3.2 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$precip.week) # model one with just total precip
frch.lm.no3.3 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$precip.month) # model one with just total precip
frch.lm.no3.4 <- lm(HI.frch.no3.2020$HI ~ HI.frch.no3.2020$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

baa <- HI.frch.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bab <- HI.frch.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bcc <- HI.frch.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bdd <- HI.frch.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.fDOM.2020 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.4, by = "storm.num")
HI.frch.fDOM.2020 <- left_join(HI.frch.fDOM.2020, FRCH.2020.per.storm.5, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2020$HI ~ HI.frch.fDOM.2020$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

bee <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bff <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bgg <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bhh <- HI.frch.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.SPC.2020 <- left_join(HI.mean.precip.frch.SPC, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.4, by = "storm.num")
HI.frch.SPC.2020 <- left_join(HI.frch.SPC.2020, FRCH.2020.per.storm.5, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2020$HI ~ HI.frch.SPC.2020$ThreeMonth) # model one with just total precip


bii <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bjj <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bkk <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bll <- HI.frch.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model


HI.frch.turb.2020 <- left_join(HI.mean.precip.frch.turb, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.4, by = "storm.num")
HI.frch.turb.2020 <- left_join(HI.frch.turb.2020, FRCH.2020.per.storm.5, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2020$HI ~ HI.frch.turb.2020$ThreeMonth) # model one with just total precip

bmm <- HI.frch.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bnn <- HI.frch.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

boo <- HI.frch.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bpp <- HI.frch.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- FRCH.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.frch.no3.2.2020 <- left_join(HI.frch.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.no3.2.2020$TOTAL.TIME <- as.numeric(HI.frch.no3.2.2020$TOTAL.TIME)
HI.frch.no3.2.2020$Intensity <- HI.frch.no3.2.2020$precip/HI.frch.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.no3.2 <- lm(HI.frch.no3.2.2020$HI ~ HI.frch.no3.2.2020$precip + HI.frch.no3.2.2020$Intensity) # model one with total precip and intensity 

bqq <- HI.frch.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2020 <- left_join(HI.frch.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2020$TOTAL.TIME)
HI.frch.fDOM.2.2020$Intensity <- HI.frch.fDOM.2.2020$precip/HI.frch.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2020$HI ~ HI.frch.fDOM.2.2020$precip + HI.frch.fDOM.2.2020$Intensity) # model one with total precip and intensity 

brr <- HI.frch.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2020 <- left_join(HI.frch.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2020$TOTAL.TIME)
HI.frch.SPC.2.2020$Intensity <- HI.frch.SPC.2.2020$precip/HI.frch.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2020$HI ~ HI.frch.SPC.2.2020$precip + HI.frch.SPC.2.2020$Intensity) # model one with total precip and intensity 

bss <- HI.frch.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2020 <- left_join(HI.frch.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.turb.2.2020$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2020$TOTAL.TIME)
HI.frch.turb.2.2020$Intensity <- HI.frch.turb.2.2020$precip/HI.frch.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.turb.2 <- lm(HI.frch.turb.2.2020$HI ~ HI.frch.turb.2.2020$precip + HI.frch.turb.2.2020$Intensity) # model one with total precip and intensity 

btt <- HI.frch.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
FRCH.2020.1$day <- julian(FRCH.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
FRCH.2020.1$day <- as.numeric(FRCH.2020.1$day)
FRCH.2020.per.storm.5 <- FRCH.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.frch.no3.2.2020 <- left_join(HI.frch.no3.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2020$HI ~ HI.frch.no3.2.2020$doy)

buu <- HI.frch.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2020 <- left_join(HI.frch.fDOM.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2020$HI ~ HI.frch.fDOM.2.2020$doy)

btb <- HI.frch.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2020 <- left_join(HI.frch.SPC.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2020$HI ~ HI.frch.SPC.2.2020$doy)

bvv <- HI.frch.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2020 <- left_join(HI.frch.turb.2.2020, FRCH.2020.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2020$HI ~ HI.frch.turb.2.2020$doy)

bww <- HI.frch.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(baa,bab,bcc,bdd,bee,bff,bgg,bhh,bii,bjj,bkk,bll,bmm,bnn,boo,bpp,bqq,brr,bss,bt,buu,btb,bvv,bww,
#          ncol = 4)

# abs #
HI.frch.abs.2020 <- left_join(HI.mean.precip.frch.abs, FRCH.2020.per.storm.1, by = "storm.num")
HI.frch.abs.2020 <- left_join(HI.frch.abs.2020, FRCH.2020.per.storm.2, by = "storm.num")
HI.frch.abs.2020 <- left_join(HI.frch.abs.2020, FRCH.2020.per.storm.3, by = "storm.num")
HI.frch.abs.2020 <- left_join(HI.frch.abs.2020, FRCH.2020.per.storm.4, by = "storm.num")
HI.frch.abs.2020 <- left_join(HI.frch.abs.2020, FRCH.2020.per.storm.5, by = "storm.num")

frch.lm.abs <- lm(HI.frch.abs.2020$HI ~ HI.frch.abs.2020$precip) # model one with just total precip
frch.lm.abs.2 <- lm(HI.frch.abs.2020$HI ~ HI.frch.abs.2020$precip.week) # model one with just total precip
frch.lm.abs.3 <- lm(HI.frch.abs.2020$HI ~ HI.frch.abs.2020$precip.month) # model one with just total precip
frch.lm.abs.4 <- lm(HI.frch.abs.2020$HI ~ HI.frch.abs.2020$ThreeMonth) # model one with just total precip
frch.lm.abs.5 <- lm(HI.frch.abs.2020$HI ~ HI.frch.abs.2020$temp) # model one with just total precip

HI.frch.abs.2.2020 <- left_join(HI.frch.abs.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.abs.2.2020$TOTAL.TIME <- as.numeric(HI.frch.abs.2.2020$TOTAL.TIME)
HI.frch.abs.2.2020$Intensity <- HI.frch.abs.2.2020$precip/HI.frch.abs.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr



HI.frch.2020 <- rbind(HI.frch.no3.2.2020, HI.frch.fDOM.2.2020,
                      HI.frch.SPC.2.2020, HI.frch.turb.2.2020,
                      HI.frch.abs.2.2020) # merging all responses together 
HI.frch.2020$burn <- "unburned" # adding a burn column
HI.frch.2020$pf <- "medium" # adding a pf column

write.csv(HI.frch.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.frch.2020.csv"))

# POKE ####
POKEstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(
                          
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

POKE_storms$DateTime <- as.POSIXct(POKE_storms$DateTime) 
POKE.2020.storms.1<- left_join(POKE_storms, POKE_RainGauge_2020, by = "DateTime")
POKE.2020.storms.1<- left_join(POKE.2020.storms.1, airtempmean, by = "DateTime")

names(POKE.2020.storms.1)[names(POKE.2020.storms.1) == ''] <- 'x'

POKE.2020.per.storm.1 <- POKE.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- POKE.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

POKE.2020.per.storm.1$temp <- temp$temp

POKE.2020 <-  subset(chem.2020, site.ID == "POKE")


POKE.2020$DateTime <- as.POSIXct(POKE.2020$datetimeAK)
POKE.2020 <- left_join(POKE.2020, POKE_RainGauge_2020, by = "DateTime")
POKE.2020 <- left_join(POKE.2020, airtempmean, by = "DateTime")
POKE.2020$week <- rollapplyr(POKE.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2020$month <- rollapplyr(POKE.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2020$ThreeMonth <- rollapplyr(POKE.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2020$temp.week <- rollapplyr(POKE.2020$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

POKE.2020.1 <- left_join(POKE.2020.storms.1, POKE.2020, by = "DateTime") # week month and 3 month precip totals 

POKE.2020.per.storm.2 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
POKE.2020.per.storm.3 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
POKE.2020.per.storm.4 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
POKE.2020.per.storm.5 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.poke.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "NO3")
HI.mean.precip.poke.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "fDOM")
HI.mean.precip.poke.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "SPC")
HI.mean.precip.poke.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "turb")
HI.mean.precip.poke.abs <- subset(HI.mean.precip.response, year == "2020" & site.ID == "POKE" & response == "abs")

HI.poke.no3.2020 <- left_join(HI.mean.precip.poke.NO3, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.4, by = "storm.num")
HI.poke.no3.2020 <- left_join(HI.poke.no3.2020, POKE.2020.per.storm.5, by = "storm.num")

poke.lm.no3 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$precip) # model one with just total precip
poke.lm.no3.2 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$precip.week) # model one with just total precip
poke.lm.no3.3 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$precip.month) # model one with just total precip
poke.lm.no3.4 <- lm(HI.poke.no3.2020$HI ~ HI.poke.no3.2020$ThreeMonth) # model one with just total precip

poke.formula <- y ~ x

ppa <- HI.poke.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppb <- HI.poke.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppc <- HI.poke.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppd <- HI.poke.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.fDOM.2020 <- left_join(HI.mean.precip.poke.fDOM, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.4, by = "storm.num")
HI.poke.fDOM.2020 <- left_join(HI.poke.fDOM.2020, POKE.2020.per.storm.5, by = "storm.num")

poke.lm.fDOM <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$precip) # model one with just total precip
poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$precip.week) # model one with just total precip
poke.lm.fDOM.3 <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$precip.month) # model one with just total precip
poke.lm.fDOM.4 <- lm(HI.poke.fDOM.2020$HI ~ HI.poke.fDOM.2020$ThreeMonth) # model one with just total precip

ppe <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppf <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppg <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pph <- HI.poke.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Three-week Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.SPC.2020 <- left_join(HI.mean.precip.poke.SPC, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.4, by = "storm.num")
HI.poke.SPC.2020 <- left_join(HI.poke.SPC.2020, POKE.2020.per.storm.5, by = "storm.num")

poke.lm.SPC <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$precip) # model one with just total precip
poke.lm.SPC.2 <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$precip.week) # model one with just total precip
poke.lm.SPC.3 <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$precip.month) # model one with just total precip
poke.lm.SPC.4 <- lm(HI.poke.SPC.2020$HI ~ HI.poke.SPC.2020$ThreeMonth) # model one with just total precip

ppi <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppj <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppk <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppl <- HI.poke.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.turb.2020 <- left_join(HI.mean.precip.poke.turb, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.4, by = "storm.num")
HI.poke.turb.2020 <- left_join(HI.poke.turb.2020, POKE.2020.per.storm.5, by = "storm.num")

poke.lm.turb <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$precip) # model one with just total precip
poke.lm.turb.2 <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$precip.week) # model one with just total precip
poke.lm.turb.3 <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$precip.month) # model one with just total precip
poke.lm.turb.4 <- lm(HI.poke.turb.2020$HI ~ HI.poke.turb.2020$ThreeMonth) # model one with just total precip

ppm <- HI.poke.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppn <- HI.poke.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppo <- HI.poke.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppp <- HI.poke.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- POKE.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.poke.no3.2.2020 <- left_join(HI.poke.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.no3.2.2020$TOTAL.TIME <- as.numeric(HI.poke.no3.2.2020$TOTAL.TIME)
HI.poke.no3.2.2020$Intensity <- HI.poke.no3.2.2020$precip/HI.poke.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.no3.2 <- lm(HI.poke.no3.2.2020$HI ~ HI.poke.no3.2.2020$precip + HI.poke.no3.2.2020$Intensity) # model one with total precip and intensity 

ppq <- HI.poke.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2020 <- left_join(HI.poke.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.poke.fDOM.2.2020$TOTAL.TIME)
HI.poke.fDOM.2.2020$Intensity <- HI.poke.fDOM.2.2020$precip/HI.poke.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2.2020$HI ~ HI.poke.fDOM.2.2020$precip + HI.poke.fDOM.2.2020$Intensity) # model one with total precip and intensity 

ppr <- HI.poke.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2020 <- left_join(HI.poke.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.poke.SPC.2.2020$TOTAL.TIME)
HI.poke.SPC.2.2020$Intensity <- HI.poke.SPC.2.2020$precip/HI.poke.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.SPC.2 <- lm(HI.poke.SPC.2.2020$HI ~ HI.poke.SPC.2.2020$precip + HI.poke.SPC.2.2020$Intensity) # model one with total precip and intensity 

pps <- HI.poke.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2020 <- left_join(HI.poke.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.turb.2.2020$TOTAL.TIME <- as.numeric(HI.poke.turb.2.2020$TOTAL.TIME)
HI.poke.turb.2.2020$Intensity <- HI.poke.turb.2.2020$precip/HI.poke.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.turb.2 <- lm(HI.poke.turb.2.2020$HI ~ HI.poke.turb.2.2020$precip + HI.poke.turb.2.2020$Intensity) # model one with total precip and intensity 

ppt <- HI.poke.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
POKE.2020.1$day <- julian(POKE.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
POKE.2020.1$day <- as.numeric(POKE.2020.1$day)
POKE.2020.per.storm.5 <- POKE.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.poke.no3.2.2020 <- left_join(HI.poke.no3.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.no3.5 <- lm(HI.poke.no3.2.2020$HI ~ HI.poke.no3.2.2020$doy)

ppu <- HI.poke.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2020 <- left_join(HI.poke.fDOM.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2.2020$HI ~ HI.poke.fDOM.2.2020$doy)

ppv <- HI.poke.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2020 <- left_join(HI.poke.SPC.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.SPC.5 <- lm(HI.poke.SPC.2.2020$HI ~ HI.poke.SPC.2.2020$doy)

ppw <- HI.poke.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2020 <- left_join(HI.poke.turb.2.2020, POKE.2020.per.storm.5, by = "storm.num")
poke.lm.turb.5 <- lm(HI.poke.turb.2.2020$HI ~ HI.poke.turb.2.2020$doy)

ppx <- HI.poke.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(ppa,ppb,ppc,ppd,ppe,ppf,ppg,pph,ppq,ppr,ppu,ppv,
#        ncol = 4)

# abs #
HI.poke.abs.2020 <- left_join(HI.mean.precip.poke.abs, POKE.2020.per.storm.1, by = "storm.num")
HI.poke.abs.2020 <- left_join(HI.poke.abs.2020, POKE.2020.per.storm.2, by = "storm.num")
HI.poke.abs.2020 <- left_join(HI.poke.abs.2020, POKE.2020.per.storm.3, by = "storm.num")
HI.poke.abs.2020 <- left_join(HI.poke.abs.2020, POKE.2020.per.storm.4, by = "storm.num")
HI.poke.abs.2020 <- left_join(HI.poke.abs.2020, POKE.2020.per.storm.5, by = "storm.num")

poke.lm.abs <- lm(HI.poke.abs.2020$HI ~ HI.poke.abs.2020$precip) # model one with just total precip
poke.lm.abs.2 <- lm(HI.poke.abs.2020$HI ~ HI.poke.abs.2020$precip.week) # model one with just total precip
poke.lm.abs.3 <- lm(HI.poke.abs.2020$HI ~ HI.poke.abs.2020$precip.month) # model one with just total precip
poke.lm.abs.4 <- lm(HI.poke.abs.2020$HI ~ HI.poke.abs.2020$ThreeMonth) # model one with just total precip
poke.lm.abs.5 <- lm(HI.poke.abs.2020$HI ~ HI.poke.abs.2020$temp) # model one with just total precip

HI.poke.abs.2.2020 <- left_join(HI.poke.abs.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.abs.2.2020$TOTAL.TIME <- as.numeric(HI.poke.abs.2.2020$TOTAL.TIME)
HI.poke.abs.2.2020$Intensity <- HI.poke.abs.2.2020$precip/HI.poke.abs.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.poke.2020 <- rbind(HI.poke.no3.2.2020, HI.poke.fDOM.2.2020, 
                      HI.poke.SPC.2.2020, HI.poke.turb.2.2020,
                      HI.poke.abs.2.2020) # merging all responses together 
HI.poke.2020$burn <- "burned" # adding a burn column
HI.poke.2020$pf <- "medium" # adding a pf column

write.csv(HI.poke.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.poke.2020.csv"))

# VAUL ####
VAULstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm10", 195),
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
                          

VAUL_storms$DateTime <- as.POSIXct(VAUL_storms$DateTime) 
VAUL.2020.storms.1<- left_join(VAUL_storms, VAUL_RainGauge_2020, by = "DateTime")
VAUL.2020.storms.1<- left_join(VAUL.2020.storms.1, airtempmean, by = "DateTime")

names(VAUL.2020.storms.1)[names(VAUL.2020.storms.1) == ''] <- 'x'

VAUL.2020.per.storm.1 <- VAUL.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- VAUL.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

VAUL.2020.per.storm.1$temp <- temp$temp

VAUL.2020 <-  subset(chem.2020, site.ID == "VAUL")


VAUL.2020$DateTime <- as.POSIXct(VAUL.2020$datetimeAK)
VAUL.2020 <- left_join(VAUL.2020, VAUL_RainGauge_2020, by = "DateTime")
VAUL.2020 <- left_join(VAUL.2020, airtempmean, by = "DateTime")
VAUL.2020$week <- rollapplyr(VAUL.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2020$month <- rollapplyr(VAUL.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2020$ThreeMonth <- rollapplyr(VAUL.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2020$temp.week <- rollapplyr(VAUL.2020$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

VAUL.2020.1 <- left_join(VAUL.2020.storms.1, VAUL.2020, by = "DateTime") # week month and 3 month precip totals 

VAUL.2020.per.storm.2 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
VAUL.2020.per.storm.3 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
VAUL.2020.per.storm.4 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
VAUL.2020.per.storm.5 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.vaul.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "NO3")
HI.mean.precip.vaul.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "fDOM")
HI.mean.precip.vaul.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "SPC")
HI.mean.precip.vaul.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "turb")
HI.mean.precip.vaul.abs <- subset(HI.mean.precip.response, year == "2020" & site.ID == "VAUL" & response == "abs")

HI.vaul.no3.2020 <- left_join(HI.mean.precip.vaul.NO3, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.4, by = "storm.num")
HI.vaul.no3.2020 <- left_join(HI.vaul.no3.2020, VAUL.2020.per.storm.5, by = "storm.num")

vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$precip) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$precip.week) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$precip.month) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2020$HI ~ HI.vaul.no3.2020$ThreeMonth) # model one with just total precip

vaul.formula <- y ~ x

daa <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dbb <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dcc <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

dcd <- HI.vaul.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.vaul.fDOM.2020 <- left_join(HI.mean.precip.vaul.fDOM, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.4, by = "storm.num")
HI.vaul.fDOM.2020 <- left_join(HI.vaul.fDOM.2020, VAUL.2020.per.storm.5, by = "storm.num")

vaul.lm.fDOM <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$precip) # model one with just total precip
vaul.lm.fDOM.1 <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$precip.week) # model one with just total precip
vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$precip.month) # model one with just total precip
vaul.lm.fDOM.3 <- lm(HI.vaul.fDOM.2020$HI ~ HI.vaul.fDOM.2020$ThreeMonth) # model one with just total precip

dee <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dff <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dgg <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

dhh <- HI.vaul.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.vaul.SPC.2020 <- left_join(HI.mean.precip.vaul.SPC, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.4, by = "storm.num")
HI.vaul.SPC.2020 <- left_join(HI.vaul.SPC.2020, VAUL.2020.per.storm.5, by = "storm.num")

vaul.lm.SPC <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$precip) # model one with just total precip
vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$precip.week) # model one with just total precip
vaul.lm.SPC.3 <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$precip.month) # model one with just total precip
vaul.lm.SPC.4 <- lm(HI.vaul.SPC.2020$HI ~ HI.vaul.SPC.2020$ThreeMonth) # model one with just total precip

dii <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

djj <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dkk <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

dll <- HI.vaul.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.turb.2020 <- left_join(HI.mean.precip.vaul.turb, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.4, by = "storm.num")
HI.vaul.turb.2020 <- left_join(HI.vaul.turb.2020, VAUL.2020.per.storm.5, by = "storm.num")

vaul.lm.turb <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$precip) # model one with just total precip
vaul.lm.turb.2 <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$precip.week) # model one with just total precip
vaul.lm.turb.3 <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$precip.month) # model one with just total precip
vaul.lm.turb.4 <- lm(HI.vaul.turb.2020$HI ~ HI.vaul.turb.2020$ThreeMonth) # model one with just total precip

dmm <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dnn <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

doo <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

dpp <- HI.vaul.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- VAUL.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.vaul.no3.2.2020 <- left_join(HI.vaul.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.no3.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.no3.2.2020$TOTAL.TIME)
HI.vaul.no3.2.2020$Intensity <- HI.vaul.no3.2.2020$precip/HI.vaul.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.no3.2 <- lm(HI.vaul.no3.2.2020$HI ~ HI.vaul.no3.2.2020$precip + HI.vaul.no3.2.2020$Intensity) # model one with total precip and intensity 

dqq <- HI.vaul.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2020 <- left_join(HI.vaul.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.fDOM.2.2020$TOTAL.TIME)
HI.vaul.fDOM.2.2020$Intensity <- HI.vaul.fDOM.2.2020$precip/HI.vaul.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2.2020$HI ~ HI.vaul.fDOM.2.2020$precip + HI.vaul.fDOM.2.2020$Intensity) # model one with total precip and intensity 

drr <- HI.vaul.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2020 <- left_join(HI.vaul.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.SPC.2.2020$TOTAL.TIME)
HI.vaul.SPC.2.2020$Intensity <- HI.vaul.SPC.2.2020$precip/HI.vaul.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2.2020$HI ~ HI.vaul.SPC.2.2020$precip + HI.vaul.SPC.2.2020$Intensity) # model one with total precip and intensity 

dss <- HI.vaul.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2020 <- left_join(HI.vaul.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.turb.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.turb.2.2020$TOTAL.TIME)
HI.vaul.turb.2.2020$Intensity <- HI.vaul.turb.2.2020$precip/HI.vaul.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.turb.2 <- lm(HI.vaul.turb.2.2020$HI ~ HI.vaul.turb.2.2020$precip + HI.vaul.turb.2.2020$Intensity) # model one with total precip and intensity 

dtt <- HI.vaul.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
VAUL.2020.1$day <- julian(VAUL.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
VAUL.2020.1$day <- as.numeric(VAUL.2020.1$day)
VAUL.2020.per.storm.5 <- VAUL.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.vaul.no3.2.2020 <- left_join(HI.vaul.no3.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.no3.5 <- lm(HI.vaul.no3.2.2020$HI ~ HI.vaul.no3.2.2020$doy)

duu <- HI.vaul.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2020 <- left_join(HI.vaul.fDOM.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2.2020$HI ~ HI.vaul.fDOM.2.2020$doy)

dtb <- HI.vaul.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2020 <- left_join(HI.vaul.SPC.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2.2020$HI ~ HI.vaul.SPC.2.2020$doy)

dvv <- HI.vaul.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2020 <- left_join(HI.vaul.turb.2.2020, VAUL.2020.per.storm.5, by = "storm.num")
vaul.lm.turb.5 <- lm(HI.vaul.turb.2.2020$HI ~ HI.vaul.turb.2.2020$doy)

dww <- HI.vaul.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(daa,dbb,dcc,dcd,dee,dff,dgg,dhh,dii,djj,dkk,dll,dmm,dnn,doo,dpp,dqq,drr,dss,dtt, duu,dtb,dvv,dww,
#          ncol = 4)

# abs #
HI.vaul.abs.2020 <- left_join(HI.mean.precip.vaul.abs, VAUL.2020.per.storm.1, by = "storm.num")
HI.vaul.abs.2020 <- left_join(HI.vaul.abs.2020, VAUL.2020.per.storm.2, by = "storm.num")
HI.vaul.abs.2020 <- left_join(HI.vaul.abs.2020, VAUL.2020.per.storm.3, by = "storm.num")
HI.vaul.abs.2020 <- left_join(HI.vaul.abs.2020, VAUL.2020.per.storm.4, by = "storm.num")
HI.vaul.abs.2020 <- left_join(HI.vaul.abs.2020, VAUL.2020.per.storm.5, by = "storm.num")

vaul.lm.abs <- lm(HI.vaul.abs.2020$HI ~ HI.vaul.abs.2020$precip) # model one with just total precip
vaul.lm.abs.2 <- lm(HI.vaul.abs.2020$HI ~ HI.vaul.abs.2020$precip.week) # model one with just total precip
vaul.lm.abs.3 <- lm(HI.vaul.abs.2020$HI ~ HI.vaul.abs.2020$precip.month) # model one with just total precip
vaul.lm.abs.4 <- lm(HI.vaul.abs.2020$HI ~ HI.vaul.abs.2020$ThreeMonth) # model one with just total precip
vaul.lm.abs.5 <- lm(HI.vaul.abs.2020$HI ~ HI.vaul.abs.2020$temp) # model one with just total precip

HI.vaul.abs.2.2020 <- left_join(HI.vaul.abs.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.abs.2.2020$TOTAL.TIME <- as.numeric(HI.vaul.abs.2.2020$TOTAL.TIME)
HI.vaul.abs.2.2020$Intensity <- HI.vaul.abs.2.2020$precip/HI.vaul.abs.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.vaul.2020 <- rbind(HI.vaul.no3.2.2020, HI.vaul.fDOM.2.2020,
                      HI.vaul.SPC.2.2020, HI.vaul.turb.2.2020,
                      HI.vaul.abs.2.2020) # merging all responses together 
HI.vaul.2020$burn <- "unburned" # adding a burn column
HI.vaul.2020$pf <- "high" # adding a pf column

write.csv(HI.vaul.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.vaul.2020.csv"))

# STRT ####
STRTstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm10", 246),
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

STRT_storms$DateTime <- as.POSIXct(STRT_storms$DateTime) 
STRT.2020.storms.1<- left_join(STRT_storms, STRT_RainGauge_2020, by = "DateTime")
STRT.2020.storms.1<- left_join(STRT.2020.storms.1, airtempmean, by = "DateTime")

names(STRT.2020.storms.1)[names(STRT.2020.storms.1) == ''] <- 'x'

STRT.2020.per.storm.1 <- STRT.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- STRT.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

STRT.2020.per.storm.1$temp <- temp$temp


STRT.2020 <-  subset(chem.2020, site.ID == "STRT")


STRT.2020$DateTime <- as.POSIXct(STRT.2020$datetimeAK)
STRT.2020 <- left_join(STRT.2020, STRT_RainGauge_2020, by = "DateTime")
STRT.2020 <- left_join(STRT.2020, airtempmean, by = "DateTime")
STRT.2020$week <- rollapplyr(STRT.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2020$month <- rollapplyr(STRT.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2020$ThreeMonth <- rollapplyr(STRT.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2020$temp.week <- rollapplyr(STRT.2020$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

STRT.2020.1 <- left_join(STRT.2020.storms.1, STRT.2020, by = "DateTime") # week month and 3 month precip totals 

STRT.2020.per.storm.2 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
STRT.2020.per.storm.3 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
STRT.2020.per.storm.4 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
STRT.2020.per.storm.5 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.strt.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "NO3")
HI.mean.precip.strt.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "fDOM")
HI.mean.precip.strt.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "SPC")
HI.mean.precip.strt.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "turb")
HI.mean.precip.strt.abs <- subset(HI.mean.precip.response, year == "2020" & site.ID == "STRT" & response == "abs")

HI.strt.no3.2020 <- left_join(HI.mean.precip.strt.NO3, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.4, by = "storm.num")
HI.strt.no3.2020 <- left_join(HI.strt.no3.2020, STRT.2020.per.storm.5, by = "storm.num")

strt.lm.no3 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$precip) # model one with just total precip
strt.lm.no3.2 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$precip.week) # model one with just total precip
strt.lm.no3.3 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$precip.month) # model one with just total precip
strt.lm.no3.4 <- lm(HI.strt.no3.2020$HI ~ HI.strt.no3.2020$ThreeMonth) # model one with just total precip

strt.formula <- y ~ x

eaa <- HI.strt.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ebb <- HI.strt.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ecc <- HI.strt.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

edd <- HI.strt.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Three Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.fDOM.2020 <- left_join(HI.mean.precip.strt.fDOM, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.4, by = "storm.num")
HI.strt.fDOM.2020 <- left_join(HI.strt.fDOM.2020, STRT.2020.per.storm.5, by = "storm.num")

strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$precip) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$precip.week) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$precip.month) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2020$HI ~ HI.strt.fDOM.2020$ThreeMonth) # model one with just total precip

ede <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

eff <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

egg <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ehh <- HI.strt.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.SPC.2020 <- left_join(HI.mean.precip.strt.SPC, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.4, by = "storm.num")
HI.strt.SPC.2020 <- left_join(HI.strt.SPC.2020, STRT.2020.per.storm.5, by = "storm.num")

strt.lm.SPC <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$precip) # model one with just total precip
strt.lm.SPC.2 <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$precip.week) # model one with just total precip
strt.lm.SPC.3 <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$precip.month) # model one with just total precip
strt.lm.SPC.4 <- lm(HI.strt.SPC.2020$HI ~ HI.strt.SPC.2020$ThreeMonth) # model one with just total precip

eii <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ejj <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ekk <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ell <- HI.strt.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.turb.2020 <- left_join(HI.mean.precip.strt.turb, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.4, by = "storm.num")
HI.strt.turb.2020 <- left_join(HI.strt.turb.2020, STRT.2020.per.storm.5, by = "storm.num")

strt.lm.turb <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$precip) # model one with just total precip
strt.lm.turb.2 <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$precip.week) # model one with just total precip
strt.lm.turb.3 <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$precip.month) # model one with just total precip
strt.lm.turb.4 <- lm(HI.strt.turb.2020$HI ~ HI.strt.turb.2020$ThreeMonth) # model one with just total precip

emm <- HI.strt.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

enn <- HI.strt.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

eoo <- HI.strt.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

epp <- HI.strt.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

STRT.2020.storms.1 <- na.omit(STRT.2020.storms.1)

sum.time <- STRT.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

# this sumtime works for some of the storms but not all so I am manually creating the dataframe below and merging that 

sum.time <- data.frame(
  storm.num = c("storm10", "storm1a", "storm1b", "storm1c", "storm1e", "storm2",
                "storm3", "storm4a", "storm4b", "storm5", "storm6", "storm7a",
                "storm7b", "storm9a", "storm9b", "storm9c", "storm1d", "storm8"),
  TOTAL.TIME = c(61.25, 26.5, 40,26,110.75,41.25,
                 96.25,34.75,80.25,68.25,30.25,24.25,
                 23.5,73.25,21.25,120.25,21.25,20.25))

HI.strt.no3.2.2020 <- left_join(HI.strt.no3.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.no3.2.2020$TOTAL.TIME <- as.numeric(HI.strt.no3.2.2020$TOTAL.TIME)
HI.strt.no3.2.2020$Intensity <- HI.strt.no3.2.2020$precip/HI.strt.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.no3.2 <- lm(HI.strt.no3.2.2020$HI ~ HI.strt.no3.2.2020$precip + HI.strt.no3.2.2020$Intensity) # model one with total precip and intensity 

eqq <- HI.strt.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2020 <- left_join(HI.strt.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.strt.fDOM.2.2020$TOTAL.TIME)
HI.strt.fDOM.2.2020$Intensity <- HI.strt.fDOM.2.2020$precip/HI.strt.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2.2020$HI ~ HI.strt.fDOM.2.2020$precip + HI.strt.fDOM.2.2020$Intensity) # model one with total precip and intensity 

err <- HI.strt.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2020 <- left_join(HI.strt.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.strt.SPC.2.2020$TOTAL.TIME)
HI.strt.SPC.2.2020$Intensity <- HI.strt.SPC.2.2020$precip/HI.strt.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.SPC.2 <- lm(HI.strt.SPC.2.2020$HI ~ HI.strt.SPC.2.2020$precip + HI.strt.SPC.2.2020$Intensity) # model one with total precip and intensity 

ess <- HI.strt.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2020 <- left_join(HI.strt.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.turb.2.2020$TOTAL.TIME <- as.numeric(HI.strt.turb.2.2020$TOTAL.TIME)
HI.strt.turb.2.2020$Intensity <- HI.strt.turb.2.2020$precip/HI.strt.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.turb.2 <- lm(HI.strt.turb.2.2020$HI ~ HI.strt.turb.2.2020$precip + HI.strt.turb.2.2020$Intensity) # model one with total precip and intensity 

ett <- HI.strt.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
STRT.2020.1$day <- julian(STRT.2020.1$DateTime, origin = "2020-01-01", tz = 'America/Anchorage')
STRT.2020.1$day <- as.numeric(STRT.2020.1$day)
STRT.2020.per.storm.5 <- STRT.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.strt.no3.2.2020 <- left_join(HI.strt.no3.2.2020, STRT.2020.per.storm.5, by = "storm.num")
strt.lm.no3.5 <- lm(HI.strt.no3.2.2020$HI ~ HI.strt.no3.2.2020$doy)

euu <- HI.strt.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2020 <- left_join(HI.strt.fDOM.2.2020, STRT.2020.per.storm.5, by = "storm.num")
strt.lm.fDOM.5 <- lm(HI.strt.fDOM.2.2020$HI ~ HI.strt.fDOM.2.2020$doy)

etb <- HI.strt.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2020 <- left_join(HI.strt.SPC.2.2020, STRT.2020.per.storm.5, by = "storm.num")
sttrt.lm.SPC.5 <- lm(HI.strt.SPC.2.2020$HI ~ HI.strt.SPC.2.2020$doy)

evv <- HI.strt.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2020 <- left_join(HI.strt.turb.2.2020, STRT.2020.per.storm.5, by = "storm.num")
strt.lm.turb.5 <- lm(HI.strt.turb.2.2020$HI ~ HI.strt.turb.2.2020$doy)

eww <- HI.strt.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(eaa,ebb,ecc,edd,ede,eff,egg,ehh,eii,ejj,ekk,ell,emm,enn,eoo,epp,eqq,err,ess,ett,euu,etb,evv,eww,
#          ncol = 4)

# abs #
HI.strt.abs.2020 <- left_join(HI.mean.precip.strt.abs, STRT.2020.per.storm.1, by = "storm.num")
HI.strt.abs.2020 <- left_join(HI.strt.abs.2020, STRT.2020.per.storm.2, by = "storm.num")
HI.strt.abs.2020 <- left_join(HI.strt.abs.2020, STRT.2020.per.storm.3, by = "storm.num")
HI.strt.abs.2020 <- left_join(HI.strt.abs.2020, STRT.2020.per.storm.4, by = "storm.num")
HI.strt.abs.2020 <- left_join(HI.strt.abs.2020, STRT.2020.per.storm.5, by = "storm.num")

strt.lm.abs <- lm(HI.strt.abs.2020$HI ~ HI.strt.abs.2020$precip) # model one with just total precip
strt.lm.abs.2 <- lm(HI.strt.abs.2020$HI ~ HI.strt.abs.2020$precip.week) # model one with just total precip
strt.lm.abs.3 <- lm(HI.strt.abs.2020$HI ~ HI.strt.abs.2020$precip.month) # model one with just total precip
strt.lm.abs.4 <- lm(HI.strt.abs.2020$HI ~ HI.strt.abs.2020$ThreeMonth) # model one with just total precip
strt.lm.abs.5 <- lm(HI.strt.abs.2020$HI ~ HI.strt.abs.2020$temp) # model one with just total precip

HI.strt.abs.2.2020 <- left_join(HI.strt.abs.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.abs.2.2020$TOTAL.TIME <- as.numeric(HI.strt.abs.2.2020$TOTAL.TIME)
HI.strt.abs.2.2020$Intensity <- HI.strt.abs.2.2020$precip/HI.strt.abs.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr



HI.strt.2020 <- rbind(HI.strt.no3.2.2020, HI.strt.fDOM.2.2020,
                      HI.strt.SPC.2.2020, HI.strt.turb.2.2020,
                      HI.strt.abs.2.2020) # merging all responses together 
HI.strt.2020$burn <- "burned" # adding a burn column
HI.strt.2020$pf <- "high" # adding a pf column

write.csv(HI.strt.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.strt.2020.csv"))


# CARI ####
CARIstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

CARI_storms<-do.call("rbind", lapply(CARIstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms <- CARI_storms[order(CARI_storms$datetimeAK),]

CARI_storms$storm.num = c(rep("storm1", 203),
                          rep("storm2a", 103),
                          rep("storm2b", 251),
                          
                          rep("storm3", 283),
                          rep("storm4", 155),
                          rep("storm5", 219),
                          rep("storm6", 183),
                          rep("storm7", 307),
                          rep("storm8a", 111),
                          rep("storm8b", 481),
                          rep("storm9", 99))

CARI_storms$DateTime <- as.POSIXct(CARI_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI.2020.storms.1<- left_join(CARI_storms, POKE_RainGauge_2020, by = "DateTime")
CARI.2020.storms.1<- left_join(CARI.2020.storms.1, airtempmean, by = "DateTime")

names(CARI.2020.storms.1)[names(CARI.2020.storms.1) == ''] <- 'x'

CARI.2020.per.storm.1 <- CARI.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- CARI.2020.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

CARI.2020.per.storm.1$temp <- temp$temp


CARI.2020 <- CARI_storms
CARI.2020 <- CARI.2020[,-c(1)]

CARI.2020$DateTime <- as.POSIXct(CARI.2020$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
CARI.2020 <- left_join(CARI.2020, POKE_RainGauge_2020, by = "DateTime")
CARI.2020 <- left_join(CARI.2020, airtempmean, by = "DateTime")
CARI.2020$week <- rollapplyr(CARI.2020$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2020$month <- rollapplyr(CARI.2020$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2020$ThreeMonth <- rollapplyr(CARI.2020$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2020$temp.week <- rollapplyr(CARI.2020$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

CARI.2020.1 <- left_join(CARI.2020.storms.1, CARI.2020, by = "DateTime") # week month and 3 month precip totals 
names(CARI.2020.1)[names(CARI.2020.1) == 'storm.num.x'] <- 'storm.num'


CARI.2020.per.storm.2 <- CARI.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
CARI.2020.per.storm.3 <- CARI.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
CARI.2020.per.storm.4 <- CARI.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
CARI.2020.per.storm.5 <- CARI.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.cari.NO3 <- subset(HI.mean.precip.response, year == "2020" & site.ID == "CARI" & response == "NO3")
HI.mean.precip.cari.fDOM <- subset(HI.mean.precip.response, year == "2020" & site.ID == "CARI" & response == "fDOM")
HI.mean.precip.cari.SPC <- subset(HI.mean.precip.response, year == "2020" & site.ID == "CARI" & response == "SPC")
HI.mean.precip.cari.turb <- subset(HI.mean.precip.response, year == "2020" & site.ID == "CARI" & response == "turb")

HI.cari.no3.2020 <- left_join(HI.mean.precip.cari.NO3, CARI.2020.per.storm.1, by = "storm.num")
HI.cari.no3.2020 <- left_join(HI.cari.no3.2020, CARI.2020.per.storm.2, by = "storm.num")
HI.cari.no3.2020 <- left_join(HI.cari.no3.2020, CARI.2020.per.storm.3, by = "storm.num")
HI.cari.no3.2020 <- left_join(HI.cari.no3.2020, CARI.2020.per.storm.4, by = "storm.num")
HI.cari.no3.2020 <- left_join(HI.cari.no3.2020, CARI.2020.per.storm.5, by = "storm.num")

cari.lm.no3 <- lm(HI.cari.no3.2020$HI ~ HI.cari.no3.2020$precip) # model one with just total precip
cari.lm.no3.2 <- lm(HI.cari.no3.2020$HI ~ HI.cari.no3.2020$precip.week) # model one with just total precip
cari.lm.no3.3 <- lm(HI.cari.no3.2020$HI ~ HI.cari.no3.2020$precip.month) # model one with just total precip
cari.lm.no3.4 <- lm(HI.cari.no3.2020$HI ~ HI.cari.no3.2020$ThreeMonth) # model one with just total precip

cari.formula <- y ~ x

sa <- HI.cari.no3.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sb <- HI.cari.no3.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sc <- HI.cari.no3.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sd <- HI.cari.no3.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.cari.fDOM.2020 <- left_join(HI.mean.precip.cari.fDOM, CARI.2020.per.storm.1, by = "storm.num")
HI.cari.fDOM.2020 <- left_join(HI.cari.fDOM.2020, CARI.2020.per.storm.2, by = "storm.num")
HI.cari.fDOM.2020 <- left_join(HI.cari.fDOM.2020, CARI.2020.per.storm.3, by = "storm.num")
HI.cari.fDOM.2020 <- left_join(HI.cari.fDOM.2020, CARI.2020.per.storm.4, by = "storm.num")
HI.cari.fDOM.2020 <- left_join(HI.cari.fDOM.2020, CARI.2020.per.storm.5, by = "storm.num")

cari.lm.fDOM <- lm(HI.cari.fDOM.2020$HI ~ HI.cari.fDOM.2020$precip) # model one with just total precip
cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2020$HI ~ HI.cari.fDOM.2020$precip.week) # model one with just total precip
cari.lm.fDOM.3 <- lm(HI.cari.fDOM.2020$HI ~ HI.cari.fDOM.2020$precip.month) # model one with just total precip
cari.lm.fDOM.4 <- lm(HI.cari.fDOM.2020$HI ~ HI.cari.fDOM.2020$ThreeMonth) # model one with just total precip

se <- HI.cari.fDOM.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sf <- HI.cari.fDOM.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model

sg <- HI.cari.fDOM.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

sh <- HI.cari.fDOM.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.cari.SPC.2020 <- left_join(HI.mean.precip.cari.SPC, CARI.2020.per.storm.1, by = "storm.num")
HI.cari.SPC.2020 <- left_join(HI.cari.SPC.2020, CARI.2020.per.storm.2, by = "storm.num")
HI.cari.SPC.2020 <- left_join(HI.cari.SPC.2020, CARI.2020.per.storm.3, by = "storm.num")
HI.cari.SPC.2020 <- left_join(HI.cari.SPC.2020, CARI.2020.per.storm.4, by = "storm.num")
HI.cari.SPC.2020 <- left_join(HI.cari.SPC.2020, CARI.2020.per.storm.5, by = "storm.num")

cari.lm.SPC <- lm(HI.cari.SPC.2020$HI ~ HI.cari.SPC.2020$precip) # model one with just total precip
cari.lm.SPC.2 <- lm(HI.cari.SPC.2020$HI ~ HI.cari.SPC.2020$precip.week) # model one with just total precip
cari.lm.SPC.3 <- lm(HI.cari.SPC.2020$HI ~ HI.cari.SPC.2020$precip.month) # model one with just total precip
cari.lm.SPC.4 <- lm(HI.cari.SPC.2020$HI ~ HI.cari.SPC.2020$ThreeMonth) # model one with just total precip

si <- HI.cari.SPC.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sj <- HI.cari.SPC.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sk <- HI.cari.SPC.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sl <- HI.cari.SPC.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.cari.turb.2020 <- left_join(HI.mean.precip.cari.turb, CARI.2020.per.storm.1, by = "storm.num")
HI.cari.turb.2020 <- left_join(HI.cari.turb.2020, CARI.2020.per.storm.2, by = "storm.num")
HI.cari.turb.2020 <- left_join(HI.cari.turb.2020, CARI.2020.per.storm.3, by = "storm.num")
HI.cari.turb.2020 <- left_join(HI.cari.turb.2020, CARI.2020.per.storm.4, by = "storm.num")
HI.cari.turb.2020 <- left_join(HI.cari.turb.2020, CARI.2020.per.storm.5, by = "storm.num")

cari.lm.turb <- lm(HI.cari.turb.2020$HI ~ HI.cari.turb.2020$precip) # model one with just total precip
cari.lm.turb.2 <- lm(HI.cari.turb.2020$HI ~ HI.cari.turb.2020$precip.week) # model one with just total precip
cari.lm.turb.3 <- lm(HI.cari.turb.2020$HI ~ HI.cari.turb.2020$precip.month) # model one with just total precip
cari.lm.turb.4 <- lm(HI.cari.turb.2020$HI ~ HI.cari.turb.2020$ThreeMonth) # model one with just total precip

sm <- HI.cari.turb.2020 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sn <- HI.cari.turb.2020 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

so <- HI.cari.turb.2020 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sp <- HI.cari.turb.2020 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

#CARI.2019.storms.1 <- na.omit(CARI.2019.storms.1)

sum.time <- CARI.2020.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.cari.no3.2.2020 <- left_join(sum.time, HI.cari.no3.2020, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.no3.2.2020$TOTAL.TIME <- as.numeric(HI.cari.no3.2.2020$TOTAL.TIME)
HI.cari.no3.2.2020$Intensity <- HI.cari.no3.2.2020$precip/HI.cari.no3.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.no3.2 <- lm(HI.cari.no3.2.2020$HI ~ HI.cari.no3.2.2020$precip + HI.cari.no3.2.2020$Intensity) # model one with total precip and intensity 

sq <- HI.cari.no3.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2020 <- left_join(HI.cari.fDOM.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.fDOM.2.2020$TOTAL.TIME <- as.numeric(HI.cari.fDOM.2.2020$TOTAL.TIME)
HI.cari.fDOM.2.2020$Intensity <- HI.cari.fDOM.2.2020$precip/HI.cari.fDOM.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2.2020$HI ~ HI.cari.fDOM.2.2020$precip + HI.cari.fDOM.2.2020$Intensity) # model one with total precip and intensity 

sr <- HI.cari.fDOM.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2020 <- left_join(HI.cari.SPC.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.SPC.2.2020$TOTAL.TIME <- as.numeric(HI.cari.SPC.2.2020$TOTAL.TIME)
HI.cari.SPC.2.2020$Intensity <- HI.cari.SPC.2.2020$precip/HI.cari.SPC.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.SPC.2 <- lm(HI.cari.SPC.2.2020$HI ~ HI.cari.SPC.2.2020$precip + HI.cari.SPC.2.2020$Intensity) # model one with total precip and intensity 

ss <- HI.cari.SPC.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2020 <- left_join(HI.cari.turb.2020, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.turb.2.2020$TOTAL.TIME <- as.numeric(HI.cari.turb.2.2020$TOTAL.TIME)
HI.cari.turb.2.2020$Intensity <- HI.cari.turb.2.2020$precip/HI.cari.turb.2.2020$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.turb.2 <- lm(HI.cari.turb.2.2020$HI ~ HI.cari.turb.2.2020$precip + HI.cari.turb.2.2020$Intensity) # model one with total precip and intensity 

st <- HI.cari.turb.2.2020 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
CARI.2020.1$day <- julian(CARI.2020.1$DateTime, origin = as.POSIXct('2020-01-01', tz = 'America/Anchorage')) # making a fractional day column 
CARI.2020.1$day <- as.numeric(CARI.2020.1$day)

CARI.2020.per.storm.5 <- CARI.2020.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.cari.no3.2.2020 <- left_join(HI.cari.no3.2.2020, CARI.2020.per.storm.5, by = "storm.num")
cari.lm.no3.5 <- lm(HI.cari.no3.2.2020$HI ~ HI.cari.no3.2.2020$doy)

su <- HI.cari.no3.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2020 <- left_join(HI.cari.fDOM.2.2020, CARI.2020.per.storm.5, by = "storm.num")
cari.lm.fDOM.5 <- lm(HI.cari.fDOM.2.2020$HI ~ HI.cari.fDOM.2.2020$doy)

sv <- HI.cari.fDOM.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2020 <- left_join(HI.cari.SPC.2.2020, CARI.2020.per.storm.5, by = "storm.num")
cari.lm.SPC.5 <- lm(HI.cari.SPC.2.2020$HI ~ HI.cari.SPC.2.2020$doy)

sw <- HI.cari.SPC.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2020 <- left_join(HI.cari.turb.2.2020, CARI.2020.per.storm.5, by = "storm.num")
cari.lm.turb.5 <- lm(HI.cari.turb.2.2020$HI ~ HI.cari.turb.2.2020$doy)

sx <- HI.cari.turb.2.2020 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,
#          ncol = 4)

HI.cari.2020 <- rbind(HI.cari.no3.2.2020, HI.cari.fDOM.2.2020, HI.cari.SPC.2.2020, HI.cari.turb.2.2020) # merging all responses together 
HI.cari.2020$burn <- "burned" # adding a burn column
HI.cari.2020$pf <- "medium" # adding a pf column

write.csv(HI.cari.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.cari.2020.csv"))


HI.2020 <- rbind(HI.moos.2020, HI.frch.2020, HI.poke.2020, HI.vaul.2020, 
                 HI.strt.2020, HI.cari.2020) # bind all 2020 together

# add time since peak  Q in chena #
HI.2020$date <- as.Date(HI.2020$doy, origin = "2020-01-01")
origin_date <- as.Date("2020-05-13")
HI.2020$TimeSinceChena <- julian(HI.2020$date, origin_date)

write.csv(HI.2020, here("Output_from_analysis", "04_Antecedent_Conditions", "2020", "HI.2020.csv"))


####################################### 2021 ############################################################
setwd("Storm_Events/2021")
# import rain gauge data #
FRCH_RainGauge_2020 <- read.csv(here("Climate", "Precip", "FRCH.RainGauge.2020.csv"))
POKE_RainGauge_2020 <- read.csv(here("Climate", "Precip", "POKE.RainGauge.2020.csv"))
VAUL_RainGauge_2020 <- read.csv(here("Climate", "Precip", "VAUL.RainGauge.2020.csv"))
STRT_RainGauge_2020 <- read.csv(here("Climate", "Precip", "STRT.RainGauge.2020.csv"))

POKE_RainGauge_2021 <- read_csv(here("Climate", "Precip", "POKE.RainGauge.2021.csv"))
VAUL_RainGauge_2021 <- read_csv(here("Climate", "Precip", "VAUL.RainGauge.2021.csv"))
STRT_RainGauge_2021 <- read_csv(here("Climate", "Precip", "STRT.RainGauge.2021.csv"))

airtempmean <- read.csv(here("Climate", "airtempmean.csv"))



# convert to date time 
POKE_RainGauge_2021$DateTime <- ymd_hms(POKE_RainGauge_2021$DateTime) # converting character to datetime
VAUL_RainGauge_2021$DateTime <- ymd_hms(VAUL_RainGauge_2021$DateTime) # converting character to datetime
STRT_RainGauge_2021$DateTime <- ymd_hms(STRT_RainGauge_2021$DateTime) # converting character to datetime
airtempmean$date_timeAK <- ymd_hms(airtempmean$date_timeAK ) # converting character to datetime
names(airtempmean)[2] <- "DateTime"

# Force to AK time 
POKE_RainGauge_2021$DateTime <- force_tz(POKE_RainGauge_2021$DateTime, "America/Anchorage") # converting character to datetime
VAUL_RainGauge_2021$DateTime <- force_tz(VAUL_RainGauge_2021$DateTime, "America/Anchorage") # converting character to datetime
STRT_RainGauge_2021$DateTime <- force_tz(STRT_RainGauge_2021$DateTime, "America/Anchorage") # converting character to datetime
airtempmean$DateTime <- force_tz(airtempmean$DateTime, "America/Anchorage") # converting character to datetime

# round to nearest 15 min 
POKE_RainGauge_2021$DateTime <- lubridate::floor_date(POKE_RainGauge_2021$DateTime, "15 minutes")
VAUL_RainGauge_2021$DateTime <- lubridate::floor_date(VAUL_RainGauge_2021$DateTime, "15 minutes")
STRT_RainGauge_2021$DateTime <- lubridate::floor_date(STRT_RainGauge_2021$DateTime, "15 minutes")

# MOOS #### 
MOOSstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 191),
                          rep("storm2", 251),
                          rep("storm3a", 115),
                          rep("storm3b", 359),
                          rep("storm4a", 415),
                          
                          rep("storm5a", 315),
                          
                          rep("storm6", 127),
                          rep("storm7", 259))


MOOS_storms$DateTime <- as.POSIXct(MOOS_storms$DateTime) 
MOOS.2021.storms.1<- left_join(MOOS_storms, STRT_RainGauge_2021, by = "DateTime")
MOOS.2021.storms.1<- left_join(MOOS.2021.storms.1, airtempmean, by = "DateTime")

names(MOOS.2021.storms.1)[names(MOOS.2021.storms.1) == ''] <- 'x'

MOOS.2021.per.storm.1 <- MOOS.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- MOOS.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

MOOS.2021.per.storm.1$temp <- temp$temp

chem.2021 <- read.csv(here("processed_sensor_data", "2021", "SUNA.EXO.int.corr.lab_2021.csv"))

chem.2021 <- chem.2021[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]

chem.2021$datetimeAK <- ymd_hms(chem.2021$datetimeAK) # converting character to datetime
chem.2021$datetimeAK <- force_tz(chem.2021$datetimeAK, "America/Anchorage") # converting character to datetime

names(chem.2021) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")


MOOS.2021 <-  subset(chem.2021, site.ID == "MOOS")
MOOS.2021$DateTime <- MOOS.2021$datetimeAK
MOOS.2021$DateTime <- as.POSIXct(MOOS.2021$DateTime, tz = "America/Anchorage")
MOOS.2021 <- left_join(MOOS.2021, STRT_RainGauge_2021, by = "DateTime")
MOOS.2021 <- left_join(MOOS.2021, airtempmean, by = "DateTime")
MOOS.2021$week <- rollapplyr(MOOS.2021$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2021$month <- rollapplyr(MOOS.2021$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2021$ThreeMonth <- rollapplyr(MOOS.2021$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2021$temp.week <- rollapplyr(MOOS.2021$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

MOOS.2021.1 <- left_join(MOOS.2021.storms.1, MOOS.2021, by = "DateTime") # week month and 3 month precip totals 

MOOS.2021.per.storm.2 <- MOOS.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2021.per.storm.3 <- MOOS.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2021.per.storm.4 <- MOOS.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
MOOS.2021.per.storm.5 <- MOOS.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2021" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2021" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2021" & site.ID == "MOOS" & response == "turb")
HI.mean.precip.moos.abs <- subset(HI.mean.precip.response, year == "2021" & site.ID == "MOOS" & response == "abs")

HI.moos.fDOM.2021 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2021.per.storm.1, by = "storm.num")
HI.moos.fDOM.2021 <- left_join(HI.moos.fDOM.2021, MOOS.2021.per.storm.2, by = "storm.num")
HI.moos.fDOM.2021 <- left_join(HI.moos.fDOM.2021, MOOS.2021.per.storm.3, by = "storm.num")
HI.moos.fDOM.2021 <- left_join(HI.moos.fDOM.2021, MOOS.2021.per.storm.4, by = "storm.num")
HI.moos.fDOM.2021 <- left_join(HI.moos.fDOM.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2021$HI ~ HI.moos.fDOM.2021$precip) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2021$HI ~ HI.moos.fDOM.2021$precip.week) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2021$HI ~ HI.moos.fDOM.2021$precip.month) # model one with just total precip
moos.lm.fDOM <- lm(HI.moos.fDOM.2021$HI ~ HI.moos.fDOM.2021$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

eee <- HI.moos.fDOM.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

fff <- HI.moos.fDOM.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ggg <- HI.moos.fDOM.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

hhh <- HI.moos.fDOM.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.SPC.2021 <- left_join(HI.mean.precip.moos.SPC, MOOS.2021.per.storm.1, by = "storm.num")
HI.moos.SPC.2021 <- left_join(HI.moos.SPC.2021, MOOS.2021.per.storm.2, by = "storm.num")
HI.moos.SPC.2021 <- left_join(HI.moos.SPC.2021, MOOS.2021.per.storm.3, by = "storm.num")
HI.moos.SPC.2021 <- left_join(HI.moos.SPC.2021, MOOS.2021.per.storm.4, by = "storm.num")
HI.moos.SPC.2021 <- left_join(HI.moos.SPC.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2021$HI ~ HI.moos.SPC.2021$precip) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2021$HI ~ HI.moos.SPC.2021$precip.week) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2021$HI ~ HI.moos.SPC.2021$precip.month) # model one with just total precip
moos.lm.SPC <- lm(HI.moos.SPC.2021$HI ~ HI.moos.SPC.2021$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

iii <- HI.moos.SPC.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

jjj <- HI.moos.SPC.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

kkk <- HI.moos.SPC.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

lll <- HI.moos.SPC.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.moos.turb.2021 <- left_join(HI.mean.precip.moos.turb, MOOS.2021.per.storm.1, by = "storm.num")
HI.moos.turb.2021 <- left_join(HI.moos.turb.2021, MOOS.2021.per.storm.2, by = "storm.num")
HI.moos.turb.2021 <- left_join(HI.moos.turb.2021, MOOS.2021.per.storm.3, by = "storm.num")
HI.moos.turb.2021 <- left_join(HI.moos.turb.2021, MOOS.2021.per.storm.4, by = "storm.num")
HI.moos.turb.2021 <- left_join(HI.moos.turb.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2021$HI ~ HI.moos.turb.2021$precip) # model one with just total precip
moos.lm.turb.1 <- lm(HI.moos.turb.2021$HI ~ HI.moos.turb.2021$precip.week) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2021$HI ~ HI.moos.turb.2021$precip.month) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2021$HI ~ HI.moos.turb.2021$ThreeMonth) # model one with just total precip

moos.formula <- y ~ x

mmm <- HI.moos.turb.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

nnn <- HI.moos.turb.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ooo <- HI.moos.turb.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppp <- HI.moos.turb.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sum.time <- MOOS.2021.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.moos.fDOM.2.2021 <- left_join(HI.moos.fDOM.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.fDOM.2.2021$TOTAL.TIME <- as.numeric(HI.moos.fDOM.2.2021$TOTAL.TIME)
HI.moos.fDOM.2.2021$Intensity <- HI.moos.fDOM.2.2021$precip/HI.moos.fDOM.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2.2021$HI ~ HI.moos.fDOM.2.2021$precip + HI.moos.fDOM.2.2021$Intensity) # model one with total precip and intensity 

rrr <- HI.moos.fDOM.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2021 <- left_join(HI.moos.SPC.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.SPC.2.2021$TOTAL.TIME <- as.numeric(HI.moos.SPC.2.2021$TOTAL.TIME)
HI.moos.SPC.2.2021$Intensity <- HI.moos.SPC.2.2021$precip/HI.moos.SPC.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.SPC.2 <- lm(HI.moos.SPC.2.2021$HI ~ HI.moos.SPC.2.2021$precip + HI.moos.SPC.2.2021$Intensity) # model one with total precip and intensity 

sss <- HI.moos.SPC.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2021 <- left_join(HI.moos.turb.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.moos.turb.2.2021$TOTAL.TIME <- as.numeric(HI.moos.turb.2.2021$TOTAL.TIME)
HI.moos.turb.2.2021$Intensity <- HI.moos.turb.2.2021$precip/HI.moos.turb.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

moos.lm.turb.2 <- lm(HI.moos.turb.2.2021$HI ~ HI.moos.turb.2.2021$precip + HI.moos.turb.2.2021$Intensity) # model one with total precip and intensity 

ttt <- HI.moos.turb.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
MOOS.2021.1$day <- julian(MOOS.2021.1$DateTime, origin = "2021-01-01", tz = 'America/Anchorage')
MOOS.2021.1$day <- as.numeric(MOOS.2021.1$day)
MOOS.2021.per.storm.5 <- MOOS.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 


HI.moos.fDOM.2.2021 <- left_join(HI.moos.fDOM.2.2021, MOOS.2021.per.storm.5, by = "storm.num")
moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2021$HI ~ HI.moos.fDOM.2.2021$doy)

utb <- HI.moos.fDOM.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.SPC.2.2021 <- left_join(HI.moos.SPC.2.2021, MOOS.2021.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2021$HI ~ HI.moos.SPC.2.2021$doy)

vvv <- HI.moos.SPC.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.moos.turb.2.2021 <- left_join(HI.moos.turb.2.2021, MOOS.2021.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2021$HI ~ HI.moos.turb.2.2021$doy)

www <- HI.moos.turb.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = moos.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("MOOS turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(aaa,bbb,ccc,ddd,eee,fff,ggg,hhh,iii,jjj,kkk,lll,mmm,nnn,ooo,ppp,qqq,rrr,sss,ttt,uuu,utb,vvv,www,
#          ncol = 4)


HI.moos.2021 <- rbind(HI.moos.fDOM.2.2021, HI.moos.SPC.2.2021, HI.moos.turb.2.2021) # merging all responses together 
HI.moos.2021$burn <- "unburned" # adding a burn column
HI.moos.2021$pf <- "medium" # adding a pf column

write.csv(HI.moos.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.moos.2021.csv"))

# FRCH ####
FRCHstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)


FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(
                          rep("storm2", 304),
                          rep("storm3", 208),
                          rep("storm4", 224),
                          rep("storm5a", 444),
                         
                          rep("storm6a", 424),
                          
                          rep("storm7", 140),
                          rep("storm8", 468))

FRCH_storms$DateTime <- as.POSIXct(FRCH_storms$DateTime) 
FRCH.2021.storms.1<- left_join(FRCH_storms, STRT_RainGauge_2021, by = "DateTime")
FRCH.2021.storms.1<- left_join(FRCH.2021.storms.1, airtempmean, by = "DateTime")

names(FRCH.2021.storms.1)[names(FRCH.2021.storms.1) == ''] <- 'x'

FRCH.2021.per.storm.1 <- FRCH.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- FRCH.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

FRCH.2021.per.storm.1$temp <- temp$temp

FRCH.2021 <- subset(chem.2021, site.ID == "FRCH")
FRCH.2021$DateTime <- FRCH.2021$datetimeAK
FRCH.2021$DateTime <- ymd_hms(FRCH.2021$DateTime)
FRCH.2021 <- left_join(FRCH.2021, STRT_RainGauge_2021, by = "DateTime")
FRCH.2021 <- left_join(FRCH.2021, airtempmean, by = "DateTime")
FRCH.2021$week <- rollapplyr(FRCH.2021$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2021$month <- rollapplyr(FRCH.2021$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2021$ThreeMonth <- rollapplyr(FRCH.2021$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2021$temp.week <- rollapplyr(FRCH.2021$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

FRCH.2021.1 <- left_join(FRCH.2021.storms.1, FRCH.2021, by = "DateTime") # week month and 3 month precip totals 

FRCH.2021.per.storm.2 <- FRCH.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2021.per.storm.3 <- FRCH.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2021.per.storm.4 <- FRCH.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
FRCH.2021.per.storm.5 <- FRCH.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2021" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2021" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2021" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2021" & site.ID == "FRCH" & response == "turb")
HI.mean.precip.frch.abs <- subset(HI.mean.precip.response, year == "2021" & site.ID == "FRCH" & response == "abs")

HI.frch.no3.2021 <- left_join(HI.mean.precip.frch.NO3, FRCH.2021.per.storm.1, by = "storm.num")
HI.frch.no3.2021 <- left_join(HI.frch.no3.2021, FRCH.2021.per.storm.2, by = "storm.num")
HI.frch.no3.2021 <- left_join(HI.frch.no3.2021, FRCH.2021.per.storm.3, by = "storm.num")
HI.frch.no3.2021 <- left_join(HI.frch.no3.2021, FRCH.2021.per.storm.4, by = "storm.num")
HI.frch.no3.2021 <- left_join(HI.frch.no3.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.no3 <- lm(HI.frch.no3.2021$HI ~ HI.frch.no3.2021$precip) # model one with just total precip
frch.lm.no3.2 <- lm(HI.frch.no3.2021$HI ~ HI.frch.no3.2021$precip.week) # model one with just total precip
frch.lm.no3.3 <- lm(HI.frch.no3.2021$HI ~ HI.frch.no3.2021$precip.month) # model one with just total precip
frch.lm.no3.4 <- lm(HI.frch.no3.2021$HI ~ HI.frch.no3.2021$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

baa <- HI.frch.no3.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bab <- HI.frch.no3.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bcc <- HI.frch.no3.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bdd <- HI.frch.no3.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.fDOM.2021 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2021.per.storm.1, by = "storm.num")
HI.frch.fDOM.2021 <- left_join(HI.frch.fDOM.2021, FRCH.2021.per.storm.2, by = "storm.num")
HI.frch.fDOM.2021 <- left_join(HI.frch.fDOM.2021, FRCH.2021.per.storm.3, by = "storm.num")
HI.frch.fDOM.2021 <- left_join(HI.frch.fDOM.2021, FRCH.2021.per.storm.4, by = "storm.num")
HI.frch.fDOM.2021 <- left_join(HI.frch.fDOM.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2021$HI ~ HI.frch.fDOM.2021$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2021$HI ~ HI.frch.fDOM.2021$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2021$HI ~ HI.frch.fDOM.2021$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2021$HI ~ HI.frch.fDOM.2021$ThreeMonth) # model one with just total precip

frch.formula <- y ~ x

bee <- HI.frch.fDOM.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bff <- HI.frch.fDOM.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bgg <- HI.frch.fDOM.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bhh <- HI.frch.fDOM.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.frch.SPC.2021 <- left_join(HI.mean.precip.frch.SPC, FRCH.2021.per.storm.1, by = "storm.num")
HI.frch.SPC.2021 <- left_join(HI.frch.SPC.2021, FRCH.2021.per.storm.2, by = "storm.num")
HI.frch.SPC.2021 <- left_join(HI.frch.SPC.2021, FRCH.2021.per.storm.3, by = "storm.num")
HI.frch.SPC.2021 <- left_join(HI.frch.SPC.2021, FRCH.2021.per.storm.4, by = "storm.num")
HI.frch.SPC.2021 <- left_join(HI.frch.SPC.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2021$HI ~ HI.frch.SPC.2021$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2021$HI ~ HI.frch.SPC.2021$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2021$HI ~ HI.frch.SPC.2021$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2021$HI ~ HI.frch.SPC.2021$ThreeMonth) # model one with just total precip


bii <- HI.frch.SPC.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bjj <- HI.frch.SPC.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

bkk <- HI.frch.SPC.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bll <- HI.frch.SPC.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model


HI.frch.turb.2021 <- left_join(HI.mean.precip.frch.turb, FRCH.2021.per.storm.1, by = "storm.num")
HI.frch.turb.2021 <- left_join(HI.frch.turb.2021, FRCH.2021.per.storm.2, by = "storm.num")
HI.frch.turb.2021 <- left_join(HI.frch.turb.2021, FRCH.2021.per.storm.3, by = "storm.num")
HI.frch.turb.2021 <- left_join(HI.frch.turb.2021, FRCH.2021.per.storm.4, by = "storm.num")
HI.frch.turb.2021 <- left_join(HI.frch.turb.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2021$HI ~ HI.frch.turb.2021$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2021$HI ~ HI.frch.turb.2021$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2021$HI ~ HI.frch.turb.2021$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2021$HI ~ HI.frch.turb.2021$ThreeMonth) # model one with just total precip

bmm <- HI.frch.turb.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

bnn <- HI.frch.turb.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

boo <- HI.frch.turb.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

bpp <- HI.frch.turb.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

FRCH.2021.storms.1 <- na.omit(FRCH.2021.storms.1)

sum.time <- FRCH.2021.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.frch.no3.2.2021 <- left_join(HI.frch.no3.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.no3.2.2021$TOTAL.TIME <- as.numeric(HI.frch.no3.2.2021$TOTAL.TIME)
HI.frch.no3.2.2021$Intensity <- HI.frch.no3.2.2021$precip/HI.frch.no3.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.no3.2 <- lm(HI.frch.no3.2.2021$HI ~ HI.frch.no3.2.2021$precip + HI.frch.no3.2.2021$Intensity) # model one with total precip and intensity 

bqq <- HI.frch.no3.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2021 <- left_join(HI.frch.fDOM.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.fDOM.2.2021$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2021$TOTAL.TIME)
HI.frch.fDOM.2.2021$Intensity <- HI.frch.fDOM.2.2021$precip/HI.frch.fDOM.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2021$HI ~ HI.frch.fDOM.2.2021$precip + HI.frch.fDOM.2.2021$Intensity) # model one with total precip and intensity 

brr <- HI.frch.fDOM.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2021 <- left_join(HI.frch.SPC.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.SPC.2.2021$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2021$TOTAL.TIME)
HI.frch.SPC.2.2021$Intensity <- HI.frch.SPC.2.2021$precip/HI.frch.SPC.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2021$HI ~ HI.frch.SPC.2.2021$precip + HI.frch.SPC.2.2021$Intensity) # model one with total precip and intensity 

bss <- HI.frch.SPC.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2021 <- left_join(HI.frch.turb.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.turb.2.2021$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2021$TOTAL.TIME)
HI.frch.turb.2.2021$Intensity <- HI.frch.turb.2.2021$precip/HI.frch.turb.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

frch.lm.turb.2 <- lm(HI.frch.turb.2.2021$HI ~ HI.frch.turb.2.2021$precip + HI.frch.turb.2.2021$Intensity) # model one with total precip and intensity 

btt <- HI.frch.turb.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
FRCH.2021.1$day <- julian(FRCH.2021.1$DateTime, origin = "2021-01-01", tz = 'America/Anchorage')
FRCH.2021.1$day <- as.numeric(FRCH.2021.1$day)
FRCH.2021.per.storm.5 <- FRCH.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.frch.no3.2.2021 <- left_join(HI.frch.no3.2.2021, FRCH.2021.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2021$HI ~ HI.frch.no3.2.2021$doy)

buu <- HI.frch.no3.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.fDOM.2.2021 <- left_join(HI.frch.fDOM.2.2021, FRCH.2021.per.storm.5, by = "storm.num")
frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2021$HI ~ HI.frch.fDOM.2.2021$doy)

btb <- HI.frch.fDOM.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.SPC.2.2021 <- left_join(HI.frch.SPC.2.2021, FRCH.2021.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2021$HI ~ HI.frch.SPC.2.2021$doy)

bvv <- HI.frch.SPC.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.frch.turb.2.2021 <- left_join(HI.frch.turb.2.2021, FRCH.2021.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2021$HI ~ HI.frch.turb.2.2021$doy)

bww <- HI.frch.turb.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = frch.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("FRCH turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(baa,bab,bcc,bdd,bee,bff,bgg,bhh,bii,bjj,bkk,bll,bmm,bnn,boo,bpp,bqq,brr,bss,bt,buu,btb,bvv,bww,
#          ncol = 4)

# abs #
HI.frch.abs.2021 <- left_join(HI.mean.precip.frch.abs, FRCH.2021.per.storm.1, by = "storm.num")
HI.frch.abs.2021 <- left_join(HI.frch.abs.2021, FRCH.2021.per.storm.2, by = "storm.num")
HI.frch.abs.2021 <- left_join(HI.frch.abs.2021, FRCH.2021.per.storm.3, by = "storm.num")
HI.frch.abs.2021 <- left_join(HI.frch.abs.2021, FRCH.2021.per.storm.4, by = "storm.num")
HI.frch.abs.2021 <- left_join(HI.frch.abs.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.abs <- lm(HI.frch.abs.2021$HI ~ HI.frch.abs.2021$precip) # model one with just total precip
frch.lm.abs.2 <- lm(HI.frch.abs.2021$HI ~ HI.frch.abs.2021$precip.week) # model one with just total precip
frch.lm.abs.3 <- lm(HI.frch.abs.2021$HI ~ HI.frch.abs.2021$precip.month) # model one with just total precip
frch.lm.abs.4 <- lm(HI.frch.abs.2021$HI ~ HI.frch.abs.2021$ThreeMonth) # model one with just total precip
frch.lm.abs.5 <- lm(HI.frch.abs.2021$HI ~ HI.frch.abs.2021$temp) # model one with just total precip

HI.frch.abs.2.2021 <- left_join(HI.frch.abs.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.frch.abs.2.2021$TOTAL.TIME <- as.numeric(HI.frch.abs.2.2021$TOTAL.TIME)
HI.frch.abs.2.2021$Intensity <- HI.frch.abs.2.2021$precip/HI.frch.abs.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.frch.2021 <- rbind(HI.frch.no3.2.2021, HI.frch.fDOM.2.2021, 
                      HI.frch.SPC.2.2021, HI.frch.turb.2.2021,
                      HI.frch.abs.2.2021) # merging all responses together 
HI.frch.2021$burn <- "unburned" # adding a burn column
HI.frch.2021$pf <- "medium" # adding a pf column

write.csv(HI.frch.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.frch.2021.csv"))

# POKE #### 
POKEstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 235),
                          rep("storm2", 191),
                          rep("storm3", 167),
                          rep("storm4", 191),
                          rep("storm5", 367),
                          rep("storm6", 159),
                          rep("storm7a", 451),
                          rep("storm7b", 263),
                         
                          rep("storm7d", 147))


POKE_storms$DateTime <- as.POSIXct(POKE_storms$DateTime) 
POKE.2021.storms.1<- left_join(POKE_storms, POKE_RainGauge_2021, by = "DateTime")
POKE.2021.storms.1<- left_join(POKE.2021.storms.1, airtempmean, by = "DateTime")

names(POKE.2021.storms.1)[names(POKE.2021.storms.1) == ''] <- 'x'

POKE.2021.per.storm.1 <- POKE.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- POKE.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

POKE.2021.per.storm.1$temp <- temp$temp

POKE.2021 <- subset(chem.2021, site.ID == "POKE")
POKE.2021$DateTime <- POKE.2021$datetimeAK
POKE.2021$DateTime <- ymd_hms(POKE.2021$DateTime)
POKE.2021 <- left_join(POKE.2021, POKE_RainGauge_2021, by = "DateTime")
POKE.2021 <- left_join(POKE.2021, airtempmean, by = "DateTime")
POKE.2021$week <- rollapplyr(POKE.2021$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2021$month <- rollapplyr(POKE.2021$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2021$ThreeMonth <- rollapplyr(POKE.2021$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2021$temp.week <- rollapplyr(POKE.2021$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

POKE.2021.1 <- left_join(POKE.2021.storms.1, POKE.2021, by = "DateTime") # week month and 3 month precip totals 

POKE.2021.per.storm.2 <- POKE.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
POKE.2021.per.storm.3 <- POKE.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
POKE.2021.per.storm.4 <- POKE.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
POKE.2021.per.storm.5 <- POKE.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.poke.NO3 <- subset(HI.mean.precip.response, year == "2021" & site.ID == "POKE" & response == "NO3")
HI.mean.precip.poke.fDOM <- subset(HI.mean.precip.response, year == "2021" & site.ID == "POKE" & response == "fDOM")
HI.mean.precip.poke.SPC <- subset(HI.mean.precip.response, year == "2021" & site.ID == "POKE" & response == "SPC")
HI.mean.precip.poke.turb <- subset(HI.mean.precip.response, year == "2021" & site.ID == "POKE" & response == "turb")
HI.mean.precip.poke.abs <- subset(HI.mean.precip.response, year == "2021" & site.ID == "POKE" & response == "abs")

HI.poke.no3.2021 <- left_join(HI.mean.precip.poke.NO3, POKE.2021.per.storm.1, by = "storm.num")
HI.poke.no3.2021 <- left_join(HI.poke.no3.2021, POKE.2021.per.storm.2, by = "storm.num")
HI.poke.no3.2021 <- left_join(HI.poke.no3.2021, POKE.2021.per.storm.3, by = "storm.num")
HI.poke.no3.2021 <- left_join(HI.poke.no3.2021, POKE.2021.per.storm.4, by = "storm.num")
HI.poke.no3.2021 <- left_join(HI.poke.no3.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.no3 <- lm(HI.poke.no3.2021$HI ~ HI.poke.no3.2021$precip) # model one with just total precip
poke.lm.no3.2 <- lm(HI.poke.no3.2021$HI ~ HI.poke.no3.2021$precip.week) # model one with just total precip
poke.lm.no3.3 <- lm(HI.poke.no3.2021$HI ~ HI.poke.no3.2021$precip.month) # model one with just total precip
poke.lm.no3.4 <- lm(HI.poke.no3.2021$HI ~ HI.poke.no3.2021$ThreeMonth) # model one with just total precip

poke.formula <- y ~ x

ppa <- HI.poke.no3.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppb <- HI.poke.no3.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppc <- HI.poke.no3.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppd <- HI.poke.no3.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.fDOM.2021 <- left_join(HI.mean.precip.poke.fDOM, POKE.2021.per.storm.1, by = "storm.num")
HI.poke.fDOM.2021 <- left_join(HI.poke.fDOM.2021, POKE.2021.per.storm.2, by = "storm.num")
HI.poke.fDOM.2021 <- left_join(HI.poke.fDOM.2021, POKE.2021.per.storm.3, by = "storm.num")
HI.poke.fDOM.2021 <- left_join(HI.poke.fDOM.2021, POKE.2021.per.storm.4, by = "storm.num")
HI.poke.fDOM.2021 <- left_join(HI.poke.fDOM.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.fDOM <- lm(HI.poke.fDOM.2021$HI ~ HI.poke.fDOM.2021$precip) # model one with just total precip
poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2021$HI ~ HI.poke.fDOM.2021$precip.week) # model one with just total precip
poke.lm.fDOM.3 <- lm(HI.poke.fDOM.2021$HI ~ HI.poke.fDOM.2021$precip.month) # model one with just total precip
poke.lm.fDOM.4 <- lm(HI.poke.fDOM.2021$HI ~ HI.poke.fDOM.2021$ThreeMonth) # model one with just total precip

ppe <- HI.poke.fDOM.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppf <- HI.poke.fDOM.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppg <- HI.poke.fDOM.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

pph <- HI.poke.fDOM.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Three-week Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.SPC.2021 <- left_join(HI.mean.precip.poke.SPC, POKE.2021.per.storm.1, by = "storm.num")
HI.poke.SPC.2021 <- left_join(HI.poke.SPC.2021, POKE.2021.per.storm.2, by = "storm.num")
HI.poke.SPC.2021 <- left_join(HI.poke.SPC.2021, POKE.2021.per.storm.3, by = "storm.num")
HI.poke.SPC.2021 <- left_join(HI.poke.SPC.2021, POKE.2021.per.storm.4, by = "storm.num")
HI.poke.SPC.2021 <- left_join(HI.poke.SPC.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.SPC <- lm(HI.poke.SPC.2021$HI ~ HI.poke.SPC.2021$precip) # model one with just total precip
poke.lm.SPC.2 <- lm(HI.poke.SPC.2021$HI ~ HI.poke.SPC.2021$precip.week) # model one with just total precip
poke.lm.SPC.3 <- lm(HI.poke.SPC.2021$HI ~ HI.poke.SPC.2021$precip.month) # model one with just total precip
poke.lm.SPC.4 <- lm(HI.poke.SPC.2021$HI ~ HI.poke.SPC.2021$ThreeMonth) # model one with just total precip

ppi <- HI.poke.SPC.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppj <- HI.poke.SPC.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppk <- HI.poke.SPC.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppl <- HI.poke.SPC.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.poke.turb.2021 <- left_join(HI.mean.precip.poke.turb, POKE.2021.per.storm.1, by = "storm.num")
HI.poke.turb.2021 <- left_join(HI.poke.turb.2021, POKE.2021.per.storm.2, by = "storm.num")
HI.poke.turb.2021 <- left_join(HI.poke.turb.2021, POKE.2021.per.storm.3, by = "storm.num")
HI.poke.turb.2021 <- left_join(HI.poke.turb.2021, POKE.2021.per.storm.4, by = "storm.num")
HI.poke.turb.2021 <- left_join(HI.poke.turb.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.turb <- lm(HI.poke.turb.2021$HI ~ HI.poke.turb.2021$precip) # model one with just total precip
poke.lm.turb.2 <- lm(HI.poke.turb.2021$HI ~ HI.poke.turb.2021$precip.week) # model one with just total precip
poke.lm.turb.3 <- lm(HI.poke.turb.2021$HI ~ HI.poke.turb.2021$precip.month) # model one with just total precip
poke.lm.turb.4 <- lm(HI.poke.turb.2021$HI ~ HI.poke.turb.2021$ThreeMonth) # model one with just total precip

ppm <- HI.poke.turb.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ppn <- HI.poke.turb.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ppo <- HI.poke.turb.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ppp <- HI.poke.turb.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

POKE.2021.storms.1 <- na.omit(POKE.2021.storms.1)

sum.time <- POKE.2021.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.poke.no3.2.2021 <- left_join(HI.poke.no3.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.no3.2.2021$TOTAL.TIME <- as.numeric(HI.poke.no3.2.2021$TOTAL.TIME)
HI.poke.no3.2.2021$Intensity <- HI.poke.no3.2.2021$precip/HI.poke.no3.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.no3.2 <- lm(HI.poke.no3.2.2021$HI ~ HI.poke.no3.2.2021$precip + HI.poke.no3.2.2021$Intensity) # model one with total precip and intensity 

ppq <- HI.poke.no3.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2021 <- left_join(HI.poke.fDOM.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.fDOM.2.2021$TOTAL.TIME <- as.numeric(HI.poke.fDOM.2.2021$TOTAL.TIME)
HI.poke.fDOM.2.2021$Intensity <- HI.poke.fDOM.2.2021$precip/HI.poke.fDOM.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2.2021$HI ~ HI.poke.fDOM.2.2021$precip + HI.poke.fDOM.2.2021$Intensity) # model one with total precip and intensity 

ppr <- HI.poke.fDOM.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2021 <- left_join(HI.poke.SPC.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.SPC.2.2021$TOTAL.TIME <- as.numeric(HI.poke.SPC.2.2021$TOTAL.TIME)
HI.poke.SPC.2.2021$Intensity <- HI.poke.SPC.2.2021$precip/HI.poke.SPC.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.SPC.2 <- lm(HI.poke.SPC.2.2021$HI ~ HI.poke.SPC.2.2021$precip + HI.poke.SPC.2.2021$Intensity) # model one with total precip and intensity 

pps <- HI.poke.SPC.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2021 <- left_join(HI.poke.turb.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.turb.2.2021$TOTAL.TIME <- as.numeric(HI.poke.turb.2.2021$TOTAL.TIME)
HI.poke.turb.2.2021$Intensity <- HI.poke.turb.2.2021$precip/HI.poke.turb.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

poke.lm.turb.2 <- lm(HI.poke.turb.2.2021$HI ~ HI.poke.turb.2.2021$precip + HI.poke.turb.2.2021$Intensity) # model one with total precip and intensity 

ppt <- HI.poke.turb.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
POKE.2021.1$day <- julian(POKE.2021.1$DateTime, origin = "2021-01-01", tz = 'America/Anchorage')
POKE.2021.1$day <- as.numeric(POKE.2021.1$day)
POKE.2021.per.storm.5 <- POKE.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.poke.no3.2.2021 <- left_join(HI.poke.no3.2.2021, POKE.2021.per.storm.5, by = "storm.num")
poke.lm.no3.5 <- lm(HI.poke.no3.2.2021$HI ~ HI.poke.no3.2.2021$doy)

ppu <- HI.poke.no3.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.fDOM.2.2021 <- left_join(HI.poke.fDOM.2.2021, POKE.2021.per.storm.5, by = "storm.num")
poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2.2021$HI ~ HI.poke.fDOM.2.2021$doy)

ppv <- HI.poke.fDOM.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.SPC.2.2021 <- left_join(HI.poke.SPC.2.2021, POKE.2021.per.storm.5, by = "storm.num")
poke.lm.SPC.5 <- lm(HI.poke.SPC.2.2021$HI ~ HI.poke.SPC.2.2021$doy)

ppw <- HI.poke.SPC.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.poke.turb.2.2021 <- left_join(HI.poke.turb.2.2021, POKE.2021.per.storm.5, by = "storm.num")
poke.lm.turb.5 <- lm(HI.poke.turb.2.2021$HI ~ HI.poke.turb.2.2021$doy)

ppx <- HI.poke.turb.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = poke.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("POKE turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(ppa,ppb,ppc,ppd,ppe,ppf,ppg,pph,ppq,ppr,ppu,ppv,
#        ncol = 4)

# abs #
HI.poke.abs.2021 <- left_join(HI.mean.precip.poke.abs, POKE.2021.per.storm.1, by = "storm.num")
HI.poke.abs.2021 <- left_join(HI.poke.abs.2021, POKE.2021.per.storm.2, by = "storm.num")
HI.poke.abs.2021 <- left_join(HI.poke.abs.2021, POKE.2021.per.storm.3, by = "storm.num")
HI.poke.abs.2021 <- left_join(HI.poke.abs.2021, POKE.2021.per.storm.4, by = "storm.num")
HI.poke.abs.2021 <- left_join(HI.poke.abs.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.abs <- lm(HI.poke.abs.2021$HI ~ HI.poke.abs.2021$precip) # model one with just total precip
poke.lm.abs.2 <- lm(HI.poke.abs.2021$HI ~ HI.poke.abs.2021$precip.week) # model one with just total precip
poke.lm.abs.3 <- lm(HI.poke.abs.2021$HI ~ HI.poke.abs.2021$precip.month) # model one with just total precip
poke.lm.abs.4 <- lm(HI.poke.abs.2021$HI ~ HI.poke.abs.2021$ThreeMonth) # model one with just total precip
poke.lm.abs.5 <- lm(HI.poke.abs.2021$HI ~ HI.poke.abs.2021$temp) # model one with just total precip

HI.poke.abs.2.2021 <- left_join(HI.poke.abs.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.poke.abs.2.2021$TOTAL.TIME <- as.numeric(HI.poke.abs.2.2021$TOTAL.TIME)
HI.poke.abs.2.2021$Intensity <- HI.poke.abs.2.2021$precip/HI.poke.abs.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.poke.2021 <- rbind(HI.poke.no3.2.2021, HI.poke.fDOM.2.2021,
                      HI.poke.SPC.2.2021, HI.poke.turb.2.2021,
                      HI.poke.abs.2.2021) # merging all responses together 
HI.poke.2021$burn <- "burned" # adding a burn column
HI.poke.2021$pf <- "medium" # adding a pf column

write.csv(HI.poke.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.poke.2021.csv"))

# VAUL #### 
VAULstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm1a", 375),
                          rep("storm1b", 267),
                          
                          rep("storm3", 667),
                          rep("storm4a", 427),
                          rep("storm4b", 319),
                          rep("storm5a", 715))
                         


VAUL_storms$DateTime <- as.POSIXct(VAUL_storms$DateTime) 
VAUL.2021.storms.1<- left_join(VAUL_storms, VAUL_RainGauge_2021, by = "DateTime")
VAUL.2021.storms.1<- left_join(VAUL.2021.storms.1, airtempmean, by = "DateTime")

names(VAUL.2021.storms.1)[names(VAUL.2021.storms.1) == ''] <- 'x'

VAUL.2021.per.storm.1 <- VAUL.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- VAUL.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

VAUL.2021.per.storm.1$temp <- temp$temp

VAUL.2021 <- subset(chem.2021, site.ID == "VAUL")
VAUL.2021$DateTime <- VAUL.2021$datetimeAK
VAUL.2021$DateTime <- ymd_hms(VAUL.2021$DateTime)
VAUL.2021 <- left_join(VAUL.2021, VAUL_RainGauge_2021, by = "DateTime")
VAUL.2021 <- left_join(VAUL.2021, airtempmean, by = "DateTime")
VAUL.2021$week <- rollapplyr(VAUL.2021$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2021$month <- rollapplyr(VAUL.2021$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2021$ThreeMonth <- rollapplyr(VAUL.2021$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2021$temp.week <- rollapplyr(VAUL.2021$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

VAUL.2021.1 <- left_join(VAUL.2021.storms.1, VAUL.2021, by = "DateTime") # week month and 3 month precip totals 

VAUL.2021.per.storm.2 <- VAUL.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
VAUL.2021.per.storm.3 <- VAUL.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
VAUL.2021.per.storm.4 <- VAUL.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
VAUL.2021.per.storm.5 <- VAUL.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.vaul.NO3 <- subset(HI.mean.precip.response, year == "2021" & site.ID == "VAUL" & response == "NO3")
HI.mean.precip.vaul.fDOM <- subset(HI.mean.precip.response, year == "2021" & site.ID == "VAUL" & response == "fDOM")
HI.mean.precip.vaul.SPC <- subset(HI.mean.precip.response, year == "2021" & site.ID == "VAUL" & response == "SPC")
HI.mean.precip.vaul.turb <- subset(HI.mean.precip.response, year == "2021" & site.ID == "VAUL" & response == "turb")
HI.mean.precip.vaul.abs <- subset(HI.mean.precip.response, year == "2021" & site.ID == "VAUL" & response == "abs")

HI.vaul.no3.2021 <- left_join(HI.mean.precip.vaul.NO3, VAUL.2021.per.storm.1, by = "storm.num")
HI.vaul.no3.2021 <- left_join(HI.vaul.no3.2021, VAUL.2021.per.storm.2, by = "storm.num")
HI.vaul.no3.2021 <- left_join(HI.vaul.no3.2021, VAUL.2021.per.storm.3, by = "storm.num")
HI.vaul.no3.2021 <- left_join(HI.vaul.no3.2021, VAUL.2021.per.storm.4, by = "storm.num")
HI.vaul.no3.2021 <- left_join(HI.vaul.no3.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.no3 <- lm(HI.vaul.no3.2021$HI ~ HI.vaul.no3.2021$precip) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2021$HI ~ HI.vaul.no3.2021$precip.week) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2021$HI ~ HI.vaul.no3.2021$precip.month) # model one with just total precip
vaul.lm.no3 <- lm(HI.vaul.no3.2021$HI ~ HI.vaul.no3.2021$ThreeMonth) # model one with just total precip

vaul.formula <- y ~ x

daa <- HI.vaul.no3.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dbb <- HI.vaul.no3.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dcc <- HI.vaul.no3.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

dcd <- HI.vaul.no3.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.vaul.fDOM.2021 <- left_join(HI.mean.precip.vaul.fDOM, VAUL.2021.per.storm.1, by = "storm.num")
HI.vaul.fDOM.2021 <- left_join(HI.vaul.fDOM.2021, VAUL.2021.per.storm.2, by = "storm.num")
HI.vaul.fDOM.2021 <- left_join(HI.vaul.fDOM.2021, VAUL.2021.per.storm.3, by = "storm.num")
HI.vaul.fDOM.2021 <- left_join(HI.vaul.fDOM.2021, VAUL.2021.per.storm.4, by = "storm.num")
HI.vaul.fDOM.2021 <- left_join(HI.vaul.fDOM.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.fDOM <- lm(HI.vaul.fDOM.2021$HI ~ HI.vaul.fDOM.2021$precip) # model one with just total precip
vaul.lm.fDOM.1 <- lm(HI.vaul.fDOM.2021$HI ~ HI.vaul.fDOM.2021$precip.week) # model one with just total precip
vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2021$HI ~ HI.vaul.fDOM.2021$precip.month) # model one with just total precip
vaul.lm.fDOM.3 <- lm(HI.vaul.fDOM.2021$HI ~ HI.vaul.fDOM.2021$ThreeMonth) # model one with just total precip

dee <- HI.vaul.fDOM.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dff <- HI.vaul.fDOM.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dgg <- HI.vaul.fDOM.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model

dhh <- HI.vaul.fDOM.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.vaul.SPC.2021 <- left_join(HI.mean.precip.vaul.SPC, VAUL.2021.per.storm.1, by = "storm.num")
HI.vaul.SPC.2021 <- left_join(HI.vaul.SPC.2021, VAUL.2021.per.storm.2, by = "storm.num")
HI.vaul.SPC.2021 <- left_join(HI.vaul.SPC.2021, VAUL.2021.per.storm.3, by = "storm.num")
HI.vaul.SPC.2021 <- left_join(HI.vaul.SPC.2021, VAUL.2021.per.storm.4, by = "storm.num")
HI.vaul.SPC.2021 <- left_join(HI.vaul.SPC.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.SPC <- lm(HI.vaul.SPC.2021$HI ~ HI.vaul.SPC.2021$precip) # model one with just total precip
vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2021$HI ~ HI.vaul.SPC.2021$precip.week) # model one with just total precip
vaul.lm.SPC.3 <- lm(HI.vaul.SPC.2021$HI ~ HI.vaul.SPC.2021$precip.month) # model one with just total precip
vaul.lm.SPC.4 <- lm(HI.vaul.SPC.2021$HI ~ HI.vaul.SPC.2021$ThreeMonth) # model one with just total precip

dii <- HI.vaul.SPC.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

djj <- HI.vaul.SPC.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

dkk <- HI.vaul.SPC.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

dll <- HI.vaul.SPC.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.vaul.turb.2021 <- left_join(HI.mean.precip.vaul.turb, VAUL.2021.per.storm.1, by = "storm.num")
HI.vaul.turb.2021 <- left_join(HI.vaul.turb.2021, VAUL.2021.per.storm.2, by = "storm.num")
HI.vaul.turb.2021 <- left_join(HI.vaul.turb.2021, VAUL.2021.per.storm.3, by = "storm.num")
HI.vaul.turb.2021 <- left_join(HI.vaul.turb.2021, VAUL.2021.per.storm.4, by = "storm.num")
HI.vaul.turb.2021 <- left_join(HI.vaul.turb.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.turb <- lm(HI.vaul.turb.2021$HI ~ HI.vaul.turb.2021$precip) # model one with just total precip
vaul.lm.turb.2 <- lm(HI.vaul.turb.2021$HI ~ HI.vaul.turb.2021$precip.week) # model one with just total precip
vaul.lm.turb.3 <- lm(HI.vaul.turb.2021$HI ~ HI.vaul.turb.2021$precip.month) # model one with just total precip
vaul.lm.turb.4 <- lm(HI.vaul.turb.2021$HI ~ HI.vaul.turb.2021$ThreeMonth) # model one with just total precip

dmm <- HI.vaul.turb.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

dnn <- HI.vaul.turb.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

doo <- HI.vaul.turb.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

dpp <- HI.vaul.turb.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


sum.time <- VAUL.2021.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column

HI.vaul.no3.2.2021 <- left_join(HI.vaul.no3.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.no3.2.2021$TOTAL.TIME <- as.numeric(HI.vaul.no3.2.2021$TOTAL.TIME)
HI.vaul.no3.2.2021$Intensity <- HI.vaul.no3.2.2021$precip/HI.vaul.no3.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.no3.2 <- lm(HI.vaul.no3.2.2021$HI ~ HI.vaul.no3.2.2021$precip + HI.vaul.no3.2.2021$Intensity) # model one with total precip and intensity 

dqq <- HI.vaul.no3.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2021 <- left_join(HI.vaul.fDOM.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.fDOM.2.2021$TOTAL.TIME <- as.numeric(HI.vaul.fDOM.2.2021$TOTAL.TIME)
HI.vaul.fDOM.2.2021$Intensity <- HI.vaul.fDOM.2.2021$precip/HI.vaul.fDOM.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2.2021$HI ~ HI.vaul.fDOM.2.2021$precip + HI.vaul.fDOM.2.2021$Intensity) # model one with total precip and intensity 

drr <- HI.vaul.fDOM.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2021 <- left_join(HI.vaul.SPC.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.SPC.2.2021$TOTAL.TIME <- as.numeric(HI.vaul.SPC.2.2021$TOTAL.TIME)
HI.vaul.SPC.2.2021$Intensity <- HI.vaul.SPC.2.2021$precip/HI.vaul.SPC.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2.2021$HI ~ HI.vaul.SPC.2.2021$precip + HI.vaul.SPC.2.2021$Intensity) # model one with total precip and intensity 

dss <- HI.vaul.SPC.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2021 <- left_join(HI.vaul.turb.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.turb.2.2021$TOTAL.TIME <- as.numeric(HI.vaul.turb.2.2021$TOTAL.TIME)
HI.vaul.turb.2.2021$Intensity <- HI.vaul.turb.2.2021$precip/HI.vaul.turb.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

vaul.lm.turb.2 <- lm(HI.vaul.turb.2.2021$HI ~ HI.vaul.turb.2.2021$precip + HI.vaul.turb.2.2021$Intensity) # model one with total precip and intensity 

dtt <- HI.vaul.turb.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
VAUL.2021.1$day <- julian(VAUL.2021.1$DateTime, origin = "2021-01-01", tz = 'America/Anchorage')
VAUL.2021.1$day <- as.numeric(VAUL.2021.1$day)
VAUL.2021.per.storm.5 <- VAUL.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.vaul.no3.2.2021 <- left_join(HI.vaul.no3.2.2021, VAUL.2021.per.storm.5, by = "storm.num")
vaul.lm.no3.5 <- lm(HI.vaul.no3.2.2021$HI ~ HI.vaul.no3.2.2021$doy)

duu <- HI.vaul.no3.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.fDOM.2.2021 <- left_join(HI.vaul.fDOM.2.2021, VAUL.2021.per.storm.5, by = "storm.num")
vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2.2021$HI ~ HI.vaul.fDOM.2.2021$doy)

dtb <- HI.vaul.fDOM.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.SPC.2.2021 <- left_join(HI.vaul.SPC.2.2021, VAUL.2021.per.storm.5, by = "storm.num")
vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2.2021$HI ~ HI.vaul.SPC.2.2021$doy)

dvv <- HI.vaul.SPC.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.vaul.turb.2.2021 <- left_join(HI.vaul.turb.2.2021, VAUL.2021.per.storm.5, by = "storm.num")
vaul.lm.turb.5 <- lm(HI.vaul.turb.2.2021$HI ~ HI.vaul.turb.2.2021$doy)

dww <- HI.vaul.turb.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = vaul.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("VAUL turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(daa,dbb,dcc,dcd,dee,dff,dgg,dhh,dii,djj,dkk,dll,dmm,dnn,doo,dpp,dqq,drr,dss,dtt, duu,dtb,dvv,dww,
#          ncol = 4)

# abs #
HI.vaul.abs.2021 <- left_join(HI.mean.precip.vaul.abs, VAUL.2021.per.storm.1, by = "storm.num")
HI.vaul.abs.2021 <- left_join(HI.vaul.abs.2021, VAUL.2021.per.storm.2, by = "storm.num")
HI.vaul.abs.2021 <- left_join(HI.vaul.abs.2021, VAUL.2021.per.storm.3, by = "storm.num")
HI.vaul.abs.2021 <- left_join(HI.vaul.abs.2021, VAUL.2021.per.storm.4, by = "storm.num")
HI.vaul.abs.2021 <- left_join(HI.vaul.abs.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.abs <- lm(HI.vaul.abs.2021$HI ~ HI.vaul.abs.2021$precip) # model one with just total precip
vaul.lm.abs.2 <- lm(HI.vaul.abs.2021$HI ~ HI.vaul.abs.2021$precip.week) # model one with just total precip
vaul.lm.abs.3 <- lm(HI.vaul.abs.2021$HI ~ HI.vaul.abs.2021$precip.month) # model one with just total precip
vaul.lm.abs.4 <- lm(HI.vaul.abs.2021$HI ~ HI.vaul.abs.2021$ThreeMonth) # model one with just total precip
vaul.lm.abs.5 <- lm(HI.vaul.abs.2021$HI ~ HI.vaul.abs.2021$temp) # model one with just total precip

HI.vaul.abs.2.2021 <- left_join(HI.vaul.abs.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.vaul.abs.2.2021$TOTAL.TIME <- as.numeric(HI.vaul.abs.2.2021$TOTAL.TIME)
HI.vaul.abs.2.2021$Intensity <- HI.vaul.abs.2.2021$precip/HI.vaul.abs.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.vaul.2021 <- rbind(HI.vaul.no3.2.2021, HI.vaul.fDOM.2.2021,
                      HI.vaul.SPC.2.2021, HI.vaul.turb.2.2021,
                      HI.vaul.abs.2.2021) # merging all responses together 
HI.vaul.2021$burn <- "unburned" # adding a burn column
HI.vaul.2021$pf <- "high" # adding a pf column

write.csv(HI.vaul.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.vaul.2021.csv"))

# STRT #### 
STRTstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm1a", 447),
                          
                          rep("storm2a", 307),
                          
                          rep("storm3", 127))


STRT_storms$DateTime <- as.POSIXct(STRT_storms$DateTime) 
STRT.2021.storms.1<- left_join(STRT_storms, STRT_RainGauge_2021, by = "DateTime")
STRT.2021.storms.1<- left_join(STRT.2021.storms.1, airtempmean, by = "DateTime")

names(STRT.2021.storms.1)[names(STRT.2021.storms.1) == ''] <- 'x'

STRT.2021.per.storm.1 <- STRT.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- STRT.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

STRT.2021.per.storm.1$temp <- temp$temp


STRT.2021 <- subset(chem.2021, site.ID == "STRT")
STRT.2021$DateTime <- STRT.2021$datetimeAK
STRT.2021$DateTime <- ymd_hms(STRT.2021$DateTime)
STRT.2021 <- left_join(STRT.2021, STRT_RainGauge_2021, by = "DateTime")
STRT.2021 <- left_join(STRT.2021, airtempmean, by = "DateTime")
STRT.2021$week <- rollapplyr(STRT.2021$inst_rainfall_mm, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2021$month <- rollapplyr(STRT.2021$inst_rainfall_mm, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2021$ThreeMonth <- rollapplyr(STRT.2021$inst_rainfall_mm, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2021$temp.week <- rollapplyr(STRT.2021$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

STRT.2021.1 <- left_join(STRT.2021.storms.1, STRT.2021, by = "DateTime") # week month and 3 month precip totals 

STRT.2021.per.storm.2 <- STRT.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
STRT.2021.per.storm.3 <- STRT.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
STRT.2021.per.storm.4 <- STRT.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
STRT.2021.per.storm.5 <- STRT.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.strt.NO3 <- subset(HI.mean.precip.response, year == "2021" & site.ID == "STRT" & response == "NO3")
HI.mean.precip.strt.fDOM <- subset(HI.mean.precip.response, year == "2021" & site.ID == "STRT" & response == "fDOM")
HI.mean.precip.strt.SPC <- subset(HI.mean.precip.response, year == "2021" & site.ID == "STRT" & response == "SPC")
HI.mean.precip.strt.turb <- subset(HI.mean.precip.response, year == "2021" & site.ID == "STRT" & response == "turb")
HI.mean.precip.strt.abs <- subset(HI.mean.precip.response, year == "2021" & site.ID == "STRT" & response == "abs")

HI.strt.no3.2021 <- left_join(HI.mean.precip.strt.NO3, STRT.2021.per.storm.1, by = "storm.num")
HI.strt.no3.2021 <- left_join(HI.strt.no3.2021, STRT.2021.per.storm.2, by = "storm.num")
HI.strt.no3.2021 <- left_join(HI.strt.no3.2021, STRT.2021.per.storm.3, by = "storm.num")
HI.strt.no3.2021 <- left_join(HI.strt.no3.2021, STRT.2021.per.storm.4, by = "storm.num")
HI.strt.no3.2021 <- left_join(HI.strt.no3.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.no3 <- lm(HI.strt.no3.2021$HI ~ HI.strt.no3.2021$precip) # model one with just total precip
strt.lm.no3.2 <- lm(HI.strt.no3.2021$HI ~ HI.strt.no3.2021$precip.week) # model one with just total precip
strt.lm.no3.3 <- lm(HI.strt.no3.2021$HI ~ HI.strt.no3.2021$precip.month) # model one with just total precip
strt.lm.no3.4 <- lm(HI.strt.no3.2021$HI ~ HI.strt.no3.2021$ThreeMonth) # model one with just total precip

strt.formula <- y ~ x

eaa <- HI.strt.no3.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ebb <- HI.strt.no3.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ecc <- HI.strt.no3.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

edd <- HI.strt.no3.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Three Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.fDOM.2021 <- left_join(HI.mean.precip.strt.fDOM, STRT.2021.per.storm.1, by = "storm.num")
HI.strt.fDOM.2021 <- left_join(HI.strt.fDOM.2021, STRT.2021.per.storm.2, by = "storm.num")
HI.strt.fDOM.2021 <- left_join(HI.strt.fDOM.2021, STRT.2021.per.storm.3, by = "storm.num")
HI.strt.fDOM.2021 <- left_join(HI.strt.fDOM.2021, STRT.2021.per.storm.4, by = "storm.num")
HI.strt.fDOM.2021 <- left_join(HI.strt.fDOM.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.fDOM <- lm(HI.strt.fDOM.2021$HI ~ HI.strt.fDOM.2021$precip) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2021$HI ~ HI.strt.fDOM.2021$precip.week) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2021$HI ~ HI.strt.fDOM.2021$precip.month) # model one with just total precip
strt.lm.fDOM <- lm(HI.strt.fDOM.2021$HI ~ HI.strt.fDOM.2021$ThreeMonth) # model one with just total precip

ede <- HI.strt.fDOM.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

eff <- HI.strt.fDOM.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

egg <- HI.strt.fDOM.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ehh <- HI.strt.fDOM.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.SPC.2021 <- left_join(HI.mean.precip.strt.SPC, STRT.2021.per.storm.1, by = "storm.num")
HI.strt.SPC.2021 <- left_join(HI.strt.SPC.2021, STRT.2021.per.storm.2, by = "storm.num")
HI.strt.SPC.2021 <- left_join(HI.strt.SPC.2021, STRT.2021.per.storm.3, by = "storm.num")
HI.strt.SPC.2021 <- left_join(HI.strt.SPC.2021, STRT.2021.per.storm.4, by = "storm.num")
HI.strt.SPC.2021 <- left_join(HI.strt.SPC.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.SPC <- lm(HI.strt.SPC.2021$HI ~ HI.strt.SPC.2021$precip) # model one with just total precip
strt.lm.SPC.2 <- lm(HI.strt.SPC.2021$HI ~ HI.strt.SPC.2021$precip.week) # model one with just total precip
strt.lm.SPC.3 <- lm(HI.strt.SPC.2021$HI ~ HI.strt.SPC.2021$precip.month) # model one with just total precip
strt.lm.SPC.4 <- lm(HI.strt.SPC.2021$HI ~ HI.strt.SPC.2021$ThreeMonth) # model one with just total precip

eii <- HI.strt.SPC.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

ejj <- HI.strt.SPC.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

ekk <- HI.strt.SPC.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

ell <- HI.strt.SPC.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.strt.turb.2021 <- left_join(HI.mean.precip.strt.turb, STRT.2021.per.storm.1, by = "storm.num")
HI.strt.turb.2021 <- left_join(HI.strt.turb.2021, STRT.2021.per.storm.2, by = "storm.num")
HI.strt.turb.2021 <- left_join(HI.strt.turb.2021, STRT.2021.per.storm.3, by = "storm.num")
HI.strt.turb.2021 <- left_join(HI.strt.turb.2021, STRT.2021.per.storm.4, by = "storm.num")
HI.strt.turb.2021 <- left_join(HI.strt.turb.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.turb <- lm(HI.strt.turb.2021$HI ~ HI.strt.turb.2021$precip) # model one with just total precip
strt.lm.turb.2 <- lm(HI.strt.turb.2021$HI ~ HI.strt.turb.2021$precip.week) # model one with just total precip
strt.lm.turb.3 <- lm(HI.strt.turb.2021$HI ~ HI.strt.turb.2021$precip.month) # model one with just total precip
strt.lm.turb.4 <- lm(HI.strt.turb.2021$HI ~ HI.strt.turb.2021$ThreeMonth) # model one with just total precip

emm <- HI.strt.turb.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

enn <- HI.strt.turb.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("one-week Precip") +
  ylab("HI-Solute Storage") # plot model 

eoo <- HI.strt.turb.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("one-month Precip") +
  ylab("HI-Solute Storage") # plot model 

epp <- HI.strt.turb.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

STRT.2021.storms.1 <- na.omit(STRT.2021.storms.1)

sum.time <- STRT.2021.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.strt.no3.2.2021 <- left_join(HI.strt.no3.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.no3.2.2021$TOTAL.TIME <- as.numeric(HI.strt.no3.2.2021$TOTAL.TIME)
HI.strt.no3.2.2021$Intensity <- HI.strt.no3.2.2021$precip/HI.strt.no3.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.no3.2 <- lm(HI.strt.no3.2.2021$HI ~ HI.strt.no3.2.2021$precip + HI.strt.no3.2.2021$Intensity) # model one with total precip and intensity 

eqq <- HI.strt.no3.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2021 <- left_join(HI.strt.fDOM.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.fDOM.2.2021$TOTAL.TIME <- as.numeric(HI.strt.fDOM.2.2021$TOTAL.TIME)
HI.strt.fDOM.2.2021$Intensity <- HI.strt.fDOM.2.2021$precip/HI.strt.fDOM.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2.2021$HI ~ HI.strt.fDOM.2.2021$precip + HI.strt.fDOM.2.2021$Intensity) # model one with total precip and intensity 

err <- HI.strt.fDOM.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2021 <- left_join(HI.strt.SPC.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.SPC.2.2021$TOTAL.TIME <- as.numeric(HI.strt.SPC.2.2021$TOTAL.TIME)
HI.strt.SPC.2.2021$Intensity <- HI.strt.SPC.2.2021$precip/HI.strt.SPC.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.SPC.2 <- lm(HI.strt.SPC.2.2021$HI ~ HI.strt.SPC.2.2021$precip + HI.strt.SPC.2.2021$Intensity) # model one with total precip and intensity 

ess <- HI.strt.SPC.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2021 <- left_join(HI.strt.turb.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.turb.2.2021$TOTAL.TIME <- as.numeric(HI.strt.turb.2.2021$TOTAL.TIME)
HI.strt.turb.2.2021$Intensity <- HI.strt.turb.2.2021$precip/HI.strt.turb.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

strt.lm.turb.2 <- lm(HI.strt.turb.2.2021$HI ~ HI.strt.turb.2.2021$precip + HI.strt.turb.2.2021$Intensity) # model one with total precip and intensity 

ett <- HI.strt.turb.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
STRT.2021.1$day <- julian(STRT.2021.1$DateTime, origin = "2021-01-01", tz = 'America/Anchorage')
STRT.2021.1$day <- as.numeric(STRT.2021.1$day)
STRT.2021.per.storm.5 <- STRT.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.strt.no3.2.2021 <- left_join(HI.strt.no3.2.2021, STRT.2021.per.storm.5, by = "storm.num")
strt.lm.no3.5 <- lm(HI.strt.no3.2.2021$HI ~ HI.strt.no3.2.2021$doy)

euu <- HI.strt.no3.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.fDOM.2.2021 <- left_join(HI.strt.fDOM.2.2021, STRT.2021.per.storm.5, by = "storm.num")
strt.lm.fDOM.5 <- lm(HI.strt.fDOM.2.2021$HI ~ HI.strt.fDOM.2.2021$doy)

etb <- HI.strt.fDOM.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.SPC.2.2021 <- left_join(HI.strt.SPC.2.2021, STRT.2021.per.storm.5, by = "storm.num")
sttrt.lm.SPC.5 <- lm(HI.strt.SPC.2.2021$HI ~ HI.strt.SPC.2.2021$doy)

evv <- HI.strt.SPC.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.strt.turb.2.2021 <- left_join(HI.strt.turb.2.2021, STRT.2021.per.storm.5, by = "storm.num")
strt.lm.turb.5 <- lm(HI.strt.turb.2.2021$HI ~ HI.strt.turb.2.2021$doy)

eww <- HI.strt.turb.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = strt.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("STRT turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(eaa,ebb,ecc,edd,ede,eff,egg,ehh,eii,ejj,ekk,ell,emm,enn,eoo,epp,eqq,err,ess,ett,euu,etb,evv,eww,
#          ncol = 4)

# abs #
HI.strt.abs.2021 <- left_join(HI.mean.precip.strt.abs, STRT.2021.per.storm.1, by = "storm.num")
HI.strt.abs.2021 <- left_join(HI.strt.abs.2021, STRT.2021.per.storm.2, by = "storm.num")
HI.strt.abs.2021 <- left_join(HI.strt.abs.2021, STRT.2021.per.storm.3, by = "storm.num")
HI.strt.abs.2021 <- left_join(HI.strt.abs.2021, STRT.2021.per.storm.4, by = "storm.num")
HI.strt.abs.2021 <- left_join(HI.strt.abs.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.abs <- lm(HI.strt.abs.2021$HI ~ HI.strt.abs.2021$precip) # model one with just total precip
strt.lm.abs.2 <- lm(HI.strt.abs.2021$HI ~ HI.strt.abs.2021$precip.week) # model one with just total precip
strt.lm.abs.3 <- lm(HI.strt.abs.2021$HI ~ HI.strt.abs.2021$precip.month) # model one with just total precip
strt.lm.abs.4 <- lm(HI.strt.abs.2021$HI ~ HI.strt.abs.2021$ThreeMonth) # model one with just total precip
strt.lm.abs.5 <- lm(HI.strt.abs.2021$HI ~ HI.strt.abs.2021$temp) # model one with just total precip

HI.strt.abs.2.2021 <- left_join(HI.strt.abs.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.strt.abs.2.2021$TOTAL.TIME <- as.numeric(HI.strt.abs.2.2021$TOTAL.TIME)
HI.strt.abs.2.2021$Intensity <- HI.strt.abs.2.2021$precip/HI.strt.abs.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr


HI.strt.2021 <- rbind(HI.strt.no3.2.2021, HI.strt.fDOM.2.2021, 
                      HI.strt.SPC.2.2021, HI.strt.turb.2.2021,
                      HI.strt.abs.2.2021) # merging all responses together 
HI.strt.2021$burn <- "burned" # adding a burn column
HI.strt.2021$pf <- "high" # adding a pf column

write.csv(HI.strt.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.strt.2021.csv"))

# CARI ####
CARIstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

CARI_storms<-do.call("rbind", lapply(CARIstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms$storm.num = c(rep("storm1", 167),
                          rep("storm2", 139),
                          rep("storm3", 159),
                          rep("storm4", 127),
                          rep("storm5", 395),
                          rep("storm6", 395),
                          rep("storm7", 447),
                          rep("storm8", 323),
                          rep("storm9", 107),
                          rep("storm10", 243))


CARI_storms$DateTime <- as.POSIXct(CARI_storms$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI.2021.storms.1<- left_join(CARI_storms, POKE_RainGauge_2021, by = "DateTime")
CARI.2021.storms.1<- left_join(CARI.2021.storms.1, airtempmean, by = "DateTime")

names(CARI.2021.storms.1)[names(CARI.2021.storms.1) == ''] <- 'x'

CARI.2021.per.storm.1 <- CARI.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(inst_rainfall_mm), list(precip = sum), na.rm = TRUE)

temp <- CARI.2021.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

CARI.2021.per.storm.1$temp <- temp$temp

CARI.2021 <- CARI_storms
CARI.2021 <- CARI.2021[,-c(1,3)]
CARI.2021$DateTime <- as.POSIXct(CARI.2021$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M")
CARI.2021 <- left_join(CARI.2021, POKE_RainGauge_2021, by = "DateTime")
CARI.2021 <- left_join(CARI.2021, airtempmean, by = "DateTime")
CARI.2021$week <- rollapplyr(CARI.2021$inst_rainfall_mm, 10080, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2021$month <- rollapplyr(CARI.2021$inst_rainfall_mm, 40320, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2021$ThreeMonth <- rollapplyr(CARI.2021$inst_rainfall_mm, 120960, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2021$temp.week <- rollapplyr(CARI.2021$airtemp_100.1000cm_mean, 10080, mean, na.rm = TRUE, fill = NA, partial = TRUE)

CARI.2021.1 <- CARI.2021
# CARI.2021.1 <- left_join(CARI.2021.storms.1, CARI.2021, by = "DateTime") # week month and 3 month precip totals 

CARI.2021.per.storm.2 <- CARI.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
CARI.2021.per.storm.3 <- CARI.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
CARI.2021.per.storm.4 <- CARI.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
CARI.2021.per.storm.5 <- CARI.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.mean.precip.cari.NO3 <- subset(HI.mean.precip.response, year == "2021" & site.ID == "CARI" & response == "NO3")
HI.mean.precip.cari.fDOM <- subset(HI.mean.precip.response, year == "2021" & site.ID == "CARI" & response == "fDOM")
HI.mean.precip.cari.SPC <- subset(HI.mean.precip.response, year == "2021" & site.ID == "CARI" & response == "SPC")
HI.mean.precip.cari.turb <- subset(HI.mean.precip.response, year == "2021" & site.ID == "CARI" & response == "turb")

HI.cari.no3.2021 <- left_join(HI.mean.precip.cari.NO3, CARI.2021.per.storm.1, by = "storm.num")
HI.cari.no3.2021 <- left_join(HI.cari.no3.2021, CARI.2021.per.storm.2, by = "storm.num")
HI.cari.no3.2021 <- left_join(HI.cari.no3.2021, CARI.2021.per.storm.3, by = "storm.num")
HI.cari.no3.2021 <- left_join(HI.cari.no3.2021, CARI.2021.per.storm.4, by = "storm.num")
HI.cari.no3.2021 <- left_join(HI.cari.no3.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.no3 <- lm(HI.cari.no3.2021$HI ~ HI.cari.no3.2021$precip) # model one with just total precip
cari.lm.no3.2 <- lm(HI.cari.no3.2021$HI ~ HI.cari.no3.2021$precip.week) # model one with just total precip
cari.lm.no3.3 <- lm(HI.cari.no3.2021$HI ~ HI.cari.no3.2021$precip.month) # model one with just total precip
cari.lm.no3.4 <- lm(HI.cari.no3.2021$HI ~ HI.cari.no3.2021$ThreeMonth) # model one with just total precip

cari.formula <- y ~ x

sa <- HI.cari.no3.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sb <- HI.cari.no3.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sc <- HI.cari.no3.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sd <- HI.cari.no3.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 


HI.cari.fDOM.2021 <- left_join(HI.mean.precip.cari.fDOM, CARI.2021.per.storm.1, by = "storm.num")
HI.cari.fDOM.2021 <- left_join(HI.cari.fDOM.2021, CARI.2021.per.storm.2, by = "storm.num")
HI.cari.fDOM.2021 <- left_join(HI.cari.fDOM.2021, CARI.2021.per.storm.3, by = "storm.num")
HI.cari.fDOM.2021 <- left_join(HI.cari.fDOM.2021, CARI.2021.per.storm.4, by = "storm.num")
HI.cari.fDOM.2021 <- left_join(HI.cari.fDOM.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.fDOM <- lm(HI.cari.fDOM.2021$HI ~ HI.cari.fDOM.2021$precip) # model one with just total precip
cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2021$HI ~ HI.cari.fDOM.2021$precip.week) # model one with just total precip
cari.lm.fDOM.3 <- lm(HI.cari.fDOM.2021$HI ~ HI.cari.fDOM.2021$precip.month) # model one with just total precip
cari.lm.fDOM.4 <- lm(HI.cari.fDOM.2021$HI ~ HI.cari.fDOM.2021$ThreeMonth) # model one with just total precip

se <- HI.cari.fDOM.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sf <- HI.cari.fDOM.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model

sg <- HI.cari.fDOM.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model

sh <- HI.cari.fDOM.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model

HI.cari.SPC.2021 <- left_join(HI.mean.precip.cari.SPC, CARI.2021.per.storm.1, by = "storm.num")
HI.cari.SPC.2021 <- left_join(HI.cari.SPC.2021, CARI.2021.per.storm.2, by = "storm.num")
HI.cari.SPC.2021 <- left_join(HI.cari.SPC.2021, CARI.2021.per.storm.3, by = "storm.num")
HI.cari.SPC.2021 <- left_join(HI.cari.SPC.2021, CARI.2021.per.storm.4, by = "storm.num")
HI.cari.SPC.2021 <- left_join(HI.cari.SPC.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.SPC <- lm(HI.cari.SPC.2021$HI ~ HI.cari.SPC.2021$precip) # model one with just total precip
cari.lm.SPC.2 <- lm(HI.cari.SPC.2021$HI ~ HI.cari.SPC.2021$precip.week) # model one with just total precip
cari.lm.SPC.3 <- lm(HI.cari.SPC.2021$HI ~ HI.cari.SPC.2021$precip.month) # model one with just total precip
cari.lm.SPC.4 <- lm(HI.cari.SPC.2021$HI ~ HI.cari.SPC.2021$ThreeMonth) # model one with just total precip

si <- HI.cari.SPC.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sj <- HI.cari.SPC.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

sk <- HI.cari.SPC.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sl <- HI.cari.SPC.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

HI.cari.turb.2021 <- left_join(HI.mean.precip.cari.turb, CARI.2021.per.storm.1, by = "storm.num")
HI.cari.turb.2021 <- left_join(HI.cari.turb.2021, CARI.2021.per.storm.2, by = "storm.num")
HI.cari.turb.2021 <- left_join(HI.cari.turb.2021, CARI.2021.per.storm.3, by = "storm.num")
HI.cari.turb.2021 <- left_join(HI.cari.turb.2021, CARI.2021.per.storm.4, by = "storm.num")
HI.cari.turb.2021 <- left_join(HI.cari.turb.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.turb <- lm(HI.cari.turb.2021$HI ~ HI.cari.turb.2021$precip) # model one with just total precip
cari.lm.turb.2 <- lm(HI.cari.turb.2021$HI ~ HI.cari.turb.2021$precip.week) # model one with just total precip
cari.lm.turb.3 <- lm(HI.cari.turb.2021$HI ~ HI.cari.turb.2021$precip.month) # model one with just total precip
cari.lm.turb.4 <- lm(HI.cari.turb.2021$HI ~ HI.cari.turb.2021$ThreeMonth) # model one with just total precip

sm <- HI.cari.turb.2021 %>%
  ggplot(aes(x=precip, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Precip") +
  ylab("HI-Solute Storage") # plot model 

sn <- HI.cari.turb.2021 %>%
  ggplot(aes(x=precip.week, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-week Precip") +
  ylab("HI-Solute Storage") # plot model 

so <- HI.cari.turb.2021 %>%
  ggplot(aes(x=precip.month, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("One-month Precip") +
  ylab("HI-Solute Storage") # plot model 

sp <- HI.cari.turb.2021 %>%
  ggplot(aes(x=ThreeMonth, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Three-month Precip") +
  ylab("HI-Solute Storage") # plot model 

#CARI.2019.storms.1 <- na.omit(CARI.2019.storms.1)

sum.time <- CARI.2021.storms.1 %>%
  mutate(grp=data.table::rleid(storm.num))%>%
  group_by(grp) %>%
  summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
                                                         min(DateTime),units="hour"))%>%
  group_by(storm.num) %>%
  summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column


HI.cari.no3.2.2021 <- left_join(sum.time, HI.cari.no3.2021, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.no3.2.2021$TOTAL.TIME <- as.numeric(HI.cari.no3.2.2021$TOTAL.TIME)
HI.cari.no3.2.2021$Intensity <- HI.cari.no3.2.2021$precip/HI.cari.no3.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.no3.2 <- lm(HI.cari.no3.2.2021$HI ~ HI.cari.no3.2.2021$precip + HI.cari.no3.2.2021$Intensity) # model one with total precip and intensity 

sq <- HI.cari.no3.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2021 <- left_join(HI.cari.fDOM.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.fDOM.2.2021$TOTAL.TIME <- as.numeric(HI.cari.fDOM.2.2021$TOTAL.TIME)
HI.cari.fDOM.2.2021$Intensity <- HI.cari.fDOM.2.2021$precip/HI.cari.fDOM.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2.2021$HI ~ HI.cari.fDOM.2.2021$precip + HI.cari.fDOM.2.2021$Intensity) # model one with total precip and intensity 

sr <- HI.cari.fDOM.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2021 <- left_join(HI.cari.SPC.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.SPC.2.2021$TOTAL.TIME <- as.numeric(HI.cari.SPC.2.2021$TOTAL.TIME)
HI.cari.SPC.2.2021$Intensity <- HI.cari.SPC.2.2021$precip/HI.cari.SPC.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.SPC.2 <- lm(HI.cari.SPC.2.2021$HI ~ HI.cari.SPC.2.2021$precip + HI.cari.SPC.2.2021$Intensity) # model one with total precip and intensity 

ss <- HI.cari.SPC.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2021 <- left_join(HI.cari.turb.2021, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
HI.cari.turb.2.2021$TOTAL.TIME <- as.numeric(HI.cari.turb.2.2021$TOTAL.TIME)
HI.cari.turb.2.2021$Intensity <- HI.cari.turb.2.2021$precip/HI.cari.turb.2.2021$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr

cari.lm.turb.2 <- lm(HI.cari.turb.2.2021$HI ~ HI.cari.turb.2.2021$precip + HI.cari.turb.2.2021$Intensity) # model one with total precip and intensity 

st <- HI.cari.turb.2.2021 %>%
  ggplot(aes(x=Intensity, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Intensity (mm/hr)") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

# day of year #
CARI.2021.1$day <- julian(CARI.2021.1$DateTime, origin = as.POSIXct('2021-01-01', tz = 'America/Anchorage')) # making a fractional day column 
CARI.2021.1$day <- as.numeric(CARI.2021.1$day)

CARI.2021.per.storm.5 <- CARI.2021.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
HI.cari.no3.2.2021 <- left_join(HI.cari.no3.2.2021, CARI.2021.per.storm.5, by = "storm.num")
cari.lm.no3.5 <- lm(HI.cari.no3.2.2021$HI ~ HI.cari.no3.2.2021$doy)

su <- HI.cari.no3.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI NO3") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.fDOM.2.2021 <- left_join(HI.cari.fDOM.2.2021, CARI.2021.per.storm.5, by = "storm.num")
cari.lm.fDOM.5 <- lm(HI.cari.fDOM.2.2021$HI ~ HI.cari.fDOM.2.2021$doy)

sv <- HI.cari.fDOM.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI fDOM") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.SPC.2.2021 <- left_join(HI.cari.SPC.2.2021, CARI.2021.per.storm.5, by = "storm.num")
cari.lm.SPC.5 <- lm(HI.cari.SPC.2.2021$HI ~ HI.cari.SPC.2.2021$doy)

sw <- HI.cari.SPC.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI SPC") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

HI.cari.turb.2.2021 <- left_join(HI.cari.turb.2.2021, CARI.2021.per.storm.5, by = "storm.num")
cari.lm.turb.5 <- lm(HI.cari.turb.2.2021$HI ~ HI.cari.turb.2.2021$doy)

sx <- HI.cari.turb.2.2021 %>%
  ggplot(aes(x=doy, 
             y=HI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = cari.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ggtitle("CARI turb") +
  xlab("Day of year") +
  ylab("HI-Solute Storage") +
  theme_classic() # plot model 

#plot_grid(sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so,sp,sq,sr,ss,st,su,sv,sw,sx,
#          ncol = 4)

HI.cari.2021 <- rbind(HI.cari.no3.2.2021, HI.cari.fDOM.2.2021, HI.cari.SPC.2.2021, HI.cari.turb.2.2021) # merging all responses together 
HI.cari.2021$burn <- "burned" # adding a burn column
HI.cari.2021$pf <- "medium" # adding a pf column

write.csv(HI.cari.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.cari.2021.csv"))

HI.cari.2021 <- HI.cari.2021[c("site.ID", "year", "storm.num", "response",
                               "HI", "precip", "temp", "precip.week", "precip.month",
                               "ThreeMonth", "temp.week", "TOTAL.TIME", 
                               "Intensity", "doy", "burn", "pf")]

HI.2021 <- rbind(HI.moos.2021, HI.frch.2021,
                 HI.vaul.2021, HI.strt.2021,
                 HI.poke.2021, HI.cari.2021)
                  # bind all 2021 together

# add time since peak  Q in chena #
HI.2021$date <- as.Date(HI.2021$doy, origin = "2021-01-01")
origin_date <- as.Date("2021-05-12")
HI.2021$TimeSinceChena <- julian(HI.2021$date, origin_date)

write.csv(HI.2021, here("Output_from_analysis", "04_Antecedent_Conditions", "2021", "HI.2021.csv"))


####################################### 2022 ############################################################
setwd("Storm_Events/2022")
### Import climate ###
CPCRW <- read_csv(here("Climate", "Precip", "CPCRW.RainGauge.2022.final.csv"))
CPCRW$datetimeAK <- force_tz(CPCRW$datetimeAK, "America/Anchorage") # it already is in AK time so I want to make it recognize it without changing the actually time value 

# this needs to be updated to include 2022 data
airtempmean <- read.csv(here("Climate", "airtempmean.csv"))
airtempmean$datetimeAK <- airtempmean$date_timeAK
airtempmean$datetimeAK <- as.POSIXct(airtempmean$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 

### FRCH ####
FRCHstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)


FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 219),
                          rep("storm2", 235),
                          rep("storm3", 223),
                          rep("storm4", 167))

FRCH_storms$datetimeAK <- as.POSIXct(FRCH_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH.2022.storms.1<- left_join(FRCH_storms, CPCRW, by = "datetimeAK")
FRCH.2022.storms.1<- left_join(FRCH.2022.storms.1, airtempmean, by = "datetimeAK")

names(FRCH.2022.storms.1)[names(FRCH.2022.storms.1) == ''] <- 'x'

FRCH.2022.per.storm.1 <- FRCH.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- FRCH.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

FRCH.2022.per.storm.1$temp <- temp$temp


# Reading in chem data to join with the antecedent moisture condition data 
DOD.2022 <- read.csv(here("processed_sensor_data", "2022", "SUNA.EXO.int.corr.lab_2022.csv")) 
DOD.2022 <- DOD.2022[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
names(DOD.2022) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")


# summing up week/month/threemonth antecedent precip
DOD.2022$datetimeAK <- ymd_hms(DOD.2022$datetimeAK) # converting character to datetime
DOD.2022$datetimeAK <- force_tz(DOD.2022$datetimeAK, "America/Anchorage") # converting character to datetime

DOD.2022 <- left_join(DOD.2022, CPCRW, by = "datetimeAK")

FRCH.2022 <- subset(DOD.2022, site.ID == "FRCH")
MOOS.2022 <- subset(DOD.2022, site.ID == "MOOS")
POKE.2022 <- subset(DOD.2022, site.ID == "POKE")
VAUL.2022 <- subset(DOD.2022, site.ID == "VAUL")
STRT.2022 <- subset(DOD.2022, site.ID == "STRT")


#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2022-05-01", tz = "America/Anchorage"),
          as.POSIXct("2022-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
FRCH.2022 <- left_join(ts, FRCH.2022, by = "datetimeAK")
FRCH.2022 <- left_join(FRCH.2022, airtempmean, by = "datetimeAK")


FRCH.2022$week <- rollapplyr(FRCH.2022$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2022$month <- rollapplyr(FRCH.2022$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2022$ThreeMonth <- rollapplyr(FRCH.2022$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
FRCH.2022$temp.week <- rollapplyr(FRCH.2022$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms 
FRCH.2022.1 <- left_join(FRCH.2022.storms.1, FRCH.2022, by = c("datetimeAK", "NO3", "fDOM", "SPC", "Turb", "ABS_254", "site.ID",
                                                                     "CRREL", "CARI")) # week month and 3 month precip totals 
# renaming antecedent month
names(FRCH.2022.1)[names(FRCH.2022.1) == 'month.y'] <- 'month'
names(FRCH.2022.1)[names(FRCH.2022.1) == 'airtemp_100.1000cm_mean.y'] <- 'airtemp_100.1000cm_mean'

FRCH.2022.per.storm.2 <- FRCH.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
FRCH.2022.per.storm.3 <- FRCH.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
FRCH.2022.per.storm.4 <- FRCH.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# FRCH.2022.per.storm.5 <- FRCH.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.frch.NO3 <- subset(HI.mean.precip.response, year == "2022" & site.ID == "FRCH" & response == "NO3")
HI.mean.precip.frch.fDOM <- subset(HI.mean.precip.response, year == "2022" & site.ID == "FRCH" & response == "fDOM")
HI.mean.precip.frch.SPC <- subset(HI.mean.precip.response, year == "2022" & site.ID == "FRCH" & response == "SPC")
HI.mean.precip.frch.turb <- subset(HI.mean.precip.response, year == "2022" & site.ID == "FRCH" & response == "turb")
HI.mean.precip.frch.abs <- subset(HI.mean.precip.response, year == "2022" & site.ID == "FRCH" & response == "abs")

# NO3
HI.frch.no3.2022 <- left_join(HI.mean.precip.frch.NO3, FRCH.2022.per.storm.1, by = "storm.num")
HI.frch.no3.2022 <- left_join(HI.frch.no3.2022, FRCH.2022.per.storm.2, by = "storm.num")
HI.frch.no3.2022 <- left_join(HI.frch.no3.2022, FRCH.2022.per.storm.3, by = "storm.num")
HI.frch.no3.2022 <- left_join(HI.frch.no3.2022, FRCH.2022.per.storm.4, by = "storm.num")
#HI.frch.no3.2022 <- left_join(HI.frch.no3.2022, FRCH.2022.per.storm.5, by = "storm.num")

frch.lm.no3 <- lm(HI.frch.no3.2022$HI ~ HI.frch.no3.2022$precip) # model one with just total precip
frch.lm.no3.2 <- lm(HI.frch.no3.2022$HI ~ HI.frch.no3.2022$precip.week) # model one with just total precip
frch.lm.no3.3 <- lm(HI.frch.no3.2022$HI ~ HI.frch.no3.2022$precip.month) # model one with just total precip
frch.lm.no3.4 <- lm(HI.frch.no3.2022$HI ~ HI.frch.no3.2022$ThreeMonth) # model one with just total precip

# fDOM #
HI.frch.fDOM.2022 <- left_join(HI.mean.precip.frch.fDOM, FRCH.2022.per.storm.1, by = "storm.num")
HI.frch.fDOM.2022 <- left_join(HI.frch.fDOM.2022, FRCH.2022.per.storm.2, by = "storm.num")
HI.frch.fDOM.2022 <- left_join(HI.frch.fDOM.2022, FRCH.2022.per.storm.3, by = "storm.num")
HI.frch.fDOM.2022 <- left_join(HI.frch.fDOM.2022, FRCH.2022.per.storm.4, by = "storm.num")
#HI.frch.fDOM.2021 <- left_join(HI.frch.fDOM.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.fDOM <- lm(HI.frch.fDOM.2022$HI ~ HI.frch.fDOM.2022$precip) # model one with just total precip
frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2022$HI ~ HI.frch.fDOM.2022$precip.week) # model one with just total precip
frch.lm.fDOM.3 <- lm(HI.frch.fDOM.2022$HI ~ HI.frch.fDOM.2022$precip.month) # model one with just total precip
frch.lm.fDOM.4 <- lm(HI.frch.fDOM.2022$HI ~ HI.frch.fDOM.2022$ThreeMonth) # model one with just total precip
#frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2022$HI ~ HI.frch.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.frch.SPC.2022 <- left_join(HI.mean.precip.frch.SPC, FRCH.2022.per.storm.1, by = "storm.num")
HI.frch.SPC.2022 <- left_join(HI.frch.SPC.2022, FRCH.2022.per.storm.2, by = "storm.num")
HI.frch.SPC.2022 <- left_join(HI.frch.SPC.2022, FRCH.2022.per.storm.3, by = "storm.num")
HI.frch.SPC.2022 <- left_join(HI.frch.SPC.2022, FRCH.2022.per.storm.4, by = "storm.num")
#HI.frch.SPC.2021 <- left_join(HI.frch.SPC.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.SPC <- lm(HI.frch.SPC.2022$HI ~ HI.frch.SPC.2022$precip) # model one with just total precip
frch.lm.SPC.2 <- lm(HI.frch.SPC.2022$HI ~ HI.frch.SPC.2022$precip.week) # model one with just total precip
frch.lm.SPC.3 <- lm(HI.frch.SPC.2022$HI ~ HI.frch.SPC.2022$precip.month) # model one with just total precip
frch.lm.SPC.4 <- lm(HI.frch.SPC.2022$HI ~ HI.frch.SPC.2022$ThreeMonth) # model one with just total precip
#frch.lm.SPC.5 <- lm(HI.frch.SPC.2022$HI ~ HI.frch.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.frch.turb.2022 <- left_join(HI.mean.precip.frch.turb, FRCH.2022.per.storm.1, by = "storm.num")
HI.frch.turb.2022 <- left_join(HI.frch.turb.2022, FRCH.2022.per.storm.2, by = "storm.num")
HI.frch.turb.2022 <- left_join(HI.frch.turb.2022, FRCH.2022.per.storm.3, by = "storm.num")
HI.frch.turb.2022 <- left_join(HI.frch.turb.2022, FRCH.2022.per.storm.4, by = "storm.num")
#HI.frch.turb.2021 <- left_join(HI.frch.turb.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.turb <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$precip) # model one with just total precip
frch.lm.turb.2 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$precip.week) # model one with just total precip
frch.lm.turb.3 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$precip.month) # model one with just total precip
frch.lm.turb.4 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$ThreeMonth) # model one with just total precip
#frch.lm.turb.5 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$temp.week) # model one with just total precip


# abs #
HI.frch.abs.2022 <- left_join(HI.mean.precip.frch.abs, FRCH.2022.per.storm.1, by = "storm.num")
HI.frch.abs.2022 <- left_join(HI.frch.abs.2022, FRCH.2022.per.storm.2, by = "storm.num")
HI.frch.abs.2022 <- left_join(HI.frch.abs.2022, FRCH.2022.per.storm.3, by = "storm.num")
HI.frch.abs.2022 <- left_join(HI.frch.abs.2022, FRCH.2022.per.storm.4, by = "storm.num")
#HI.frch.abs.2021 <- left_join(HI.frch.abs.2021, FRCH.2021.per.storm.5, by = "storm.num")

frch.lm.abs <- lm(HI.frch.abs.2022$HI ~ HI.frch.abs.2022$precip) # model one with just total precip
frch.lm.abs.2 <- lm(HI.frch.abs.2022$HI ~ HI.frch.abs.2022$precip.week) # model one with just total precip
frch.lm.abs.3 <- lm(HI.frch.abs.2022$HI ~ HI.frch.abs.2022$precip.month) # model one with just total precip
frch.lm.abs.4 <- lm(HI.frch.abs.2022$HI ~ HI.frch.abs.2022$ThreeMonth) # model one with just total precip
#frch.lm.abs.5 <- lm(HI.frch.turb.2022$HI ~ HI.frch.turb.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
FRCH.2022.1$day <- julian(FRCH.2022.1$datetimeAK, origin = as.POSIXct('2022-01-01', tz = 'America/Anchorage')) # making a fractional day column 
FRCH.2022.1$day <- as.numeric(FRCH.2022.1$day)

FRCH.2022.per.storm.5 <- FRCH.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.frch.fDOM.2.2022 <- left_join(HI.frch.fDOM.2022, FRCH.2022.per.storm.5, by = "storm.num")

frch.lm.fDOM.5 <- lm(HI.frch.fDOM.2.2022$HI ~ HI.frch.fDOM.2.2022$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.frch.SPC.2.2022 <- left_join(HI.frch.SPC.2022, FRCH.2022.per.storm.5, by = "storm.num")
frch.lm.SPC.5 <- lm(HI.frch.SPC.2.2022$HI ~ HI.frch.SPC.2.2022$doy)

HI.frch.turb.2.2022 <- left_join(HI.frch.turb.2022, FRCH.2022.per.storm.5, by = "storm.num")
frch.lm.turb.5 <- lm(HI.frch.turb.2.2022$HI ~ HI.frch.turb.2.2022$doy)


HI.frch.no3.2.2022 <- left_join(HI.frch.no3.2022, FRCH.2022.per.storm.5, by = "storm.num")
frch.lm.no3.5 <- lm(HI.frch.no3.2.2022$HI ~ HI.frch.no3.2.2022$doy)

HI.frch.abs.2.2022 <- left_join(HI.frch.abs.2022, FRCH.2022.per.storm.5, by = "storm.num")
frch.lm.abs.5 <- lm(HI.frch.turb.2.2022$HI ~ HI.frch.turb.2.2022$doy)



HI.frch.2022 <- rbind(HI.frch.no3.2.2022, HI.frch.fDOM.2.2022, HI.frch.SPC.2.2022, HI.frch.turb.2.2022, HI.frch.abs.2.2022) # merging all responses together


HI.frch.2022$date <- as.Date(HI.frch.2022$doy, origin = "2022-01-01")
origin_date <- as.Date("2022-05-13")
HI.frch.2022$TimeSinceChena <- julian(HI.frch.2022$date, origin_date)

write.csv(HI.frch.2022, here("Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.frch.2022.csv"))


### MOOS ####
MOOSstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

# MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
#                                   recursive=F, 
#                                   pattern="MOOS", 
#                                   full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 199),
                          rep("storm2a", 71),
                          rep("storm2b", 151),
                          rep("storm3", 99),
                          rep("storm4", 215))

MOOS_storms$datetimeAK <- as.POSIXct(MOOS_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS.2022.storms.1<- left_join(MOOS_storms, CPCRW, by = "datetimeAK")
MOOS.2022.storms.1<- left_join(MOOS.2022.storms.1, airtempmean, by = "datetimeAK")

names(MOOS.2022.storms.1)[names(MOOS.2022.storms.1) == ''] <- 'x'

MOOS.2022.per.storm.1 <- MOOS.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- MOOS.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

MOOS.2022.per.storm.1$temp <- temp$temp

#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2022-05-01", tz = "America/Anchorage"),
          as.POSIXct("2022-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
MOOS.2022 <- left_join(ts, MOOS.2022, by = "datetimeAK")
MOOS.2022 <- left_join(MOOS.2022, airtempmean, by = "datetimeAK")


MOOS.2022$week <- rollapplyr(MOOS.2022$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2022$month <- rollapplyr(MOOS.2022$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2022$ThreeMonth <- rollapplyr(MOOS.2022$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
MOOS.2022$temp.week <- rollapplyr(MOOS.2022$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms 
MOOS.2022.1 <- left_join(MOOS.2022.storms.1, MOOS.2022, by = c("datetimeAK", "NO3", "fDOM", "SPC", "Turb", "ABS_254", "site.ID",
                                                               "CRREL", "CARI")) # week month and 3 month precip totals 
# renaming antecedent month
names(MOOS.2022.1)[names(MOOS.2022.1) == 'month.y'] <- 'month'
names(MOOS.2022.1)[names(MOOS.2022.1) == 'airtemp_100.1000cm_mean.y'] <- 'airtemp_100.1000cm_mean'

MOOS.2022.per.storm.2 <- MOOS.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
MOOS.2022.per.storm.3 <- MOOS.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
MOOS.2022.per.storm.4 <- MOOS.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# MOOS.2022.per.storm.5 <- MOOS.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.moos.NO3 <- subset(HI.mean.precip.response, year == "2022" & site.ID == "MOOS" & response == "NO3")
HI.mean.precip.moos.fDOM <- subset(HI.mean.precip.response, year == "2022" & site.ID == "MOOS" & response == "fDOM")
HI.mean.precip.moos.SPC <- subset(HI.mean.precip.response, year == "2022" & site.ID == "MOOS" & response == "SPC")
HI.mean.precip.moos.turb <- subset(HI.mean.precip.response, year == "2022" & site.ID == "MOOS" & response == "turb")
HI.mean.precip.moos.abs <- subset(HI.mean.precip.response, year == "2022" & site.ID == "MOOS" & response == "abs")

# NO3
HI.moos.no3.2022 <- left_join(HI.mean.precip.moos.NO3, MOOS.2022.per.storm.1, by = "storm.num")
HI.moos.no3.2022 <- left_join(HI.moos.no3.2022, MOOS.2022.per.storm.2, by = "storm.num")
HI.moos.no3.2022 <- left_join(HI.moos.no3.2022, MOOS.2022.per.storm.3, by = "storm.num")
HI.moos.no3.2022 <- left_join(HI.moos.no3.2022, MOOS.2022.per.storm.4, by = "storm.num")
#HI.moos.no3.2022 <- left_join(HI.moos.no3.2022, MOOS.2022.per.storm.5, by = "storm.num")

moos.lm.no3 <- lm(HI.moos.no3.2022$HI ~ HI.moos.no3.2022$precip) # model one with just total precip
moos.lm.no3.2 <- lm(HI.moos.no3.2022$HI ~ HI.moos.no3.2022$precip.week) # model one with just total precip
moos.lm.no3.3 <- lm(HI.moos.no3.2022$HI ~ HI.moos.no3.2022$precip.month) # model one with just total precip
moos.lm.no3.4 <- lm(HI.moos.no3.2022$HI ~ HI.moos.no3.2022$ThreeMonth) # model one with just total precip

# fDOM #
HI.moos.fDOM.2022 <- left_join(HI.mean.precip.moos.fDOM, MOOS.2022.per.storm.1, by = "storm.num")
HI.moos.fDOM.2022 <- left_join(HI.moos.fDOM.2022, MOOS.2022.per.storm.2, by = "storm.num")
HI.moos.fDOM.2022 <- left_join(HI.moos.fDOM.2022, MOOS.2022.per.storm.3, by = "storm.num")
HI.moos.fDOM.2022 <- left_join(HI.moos.fDOM.2022, MOOS.2022.per.storm.4, by = "storm.num")
#HI.moos.fDOM.2021 <- left_join(HI.moos.fDOM.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.fDOM <- lm(HI.moos.fDOM.2022$HI ~ HI.moos.fDOM.2022$precip) # model one with just total precip
moos.lm.fDOM.2 <- lm(HI.moos.fDOM.2022$HI ~ HI.moos.fDOM.2022$precip.week) # model one with just total precip
moos.lm.fDOM.3 <- lm(HI.moos.fDOM.2022$HI ~ HI.moos.fDOM.2022$precip.month) # model one with just total precip
moos.lm.fDOM.4 <- lm(HI.moos.fDOM.2022$HI ~ HI.moos.fDOM.2022$ThreeMonth) # model one with just total precip
#moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2022$HI ~ HI.moos.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.moos.SPC.2022 <- left_join(HI.mean.precip.moos.SPC, MOOS.2022.per.storm.1, by = "storm.num")
HI.moos.SPC.2022 <- left_join(HI.moos.SPC.2022, MOOS.2022.per.storm.2, by = "storm.num")
HI.moos.SPC.2022 <- left_join(HI.moos.SPC.2022, MOOS.2022.per.storm.3, by = "storm.num")
HI.moos.SPC.2022 <- left_join(HI.moos.SPC.2022, MOOS.2022.per.storm.4, by = "storm.num")
#HI.moos.SPC.2021 <- left_join(HI.moos.SPC.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.SPC <- lm(HI.moos.SPC.2022$HI ~ HI.moos.SPC.2022$precip) # model one with just total precip
moos.lm.SPC.2 <- lm(HI.moos.SPC.2022$HI ~ HI.moos.SPC.2022$precip.week) # model one with just total precip
moos.lm.SPC.3 <- lm(HI.moos.SPC.2022$HI ~ HI.moos.SPC.2022$precip.month) # model one with just total precip
moos.lm.SPC.4 <- lm(HI.moos.SPC.2022$HI ~ HI.moos.SPC.2022$ThreeMonth) # model one with just total precip
#moos.lm.SPC.5 <- lm(HI.moos.SPC.2022$HI ~ HI.moos.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.moos.turb.2022 <- left_join(HI.mean.precip.moos.turb, MOOS.2022.per.storm.1, by = "storm.num")
HI.moos.turb.2022 <- left_join(HI.moos.turb.2022, MOOS.2022.per.storm.2, by = "storm.num")
HI.moos.turb.2022 <- left_join(HI.moos.turb.2022, MOOS.2022.per.storm.3, by = "storm.num")
HI.moos.turb.2022 <- left_join(HI.moos.turb.2022, MOOS.2022.per.storm.4, by = "storm.num")
#HI.moos.turb.2021 <- left_join(HI.moos.turb.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.turb <- lm(HI.moos.turb.2022$HI ~ HI.moos.turb.2022$precip) # model one with just total precip
moos.lm.turb.2 <- lm(HI.moos.turb.2022$HI ~ HI.moos.turb.2022$precip.week) # model one with just total precip
moos.lm.turb.3 <- lm(HI.moos.turb.2022$HI ~ HI.moos.turb.2022$precip.month) # model one with just total precip
moos.lm.turb.4 <- lm(HI.moos.turb.2022$HI ~ HI.moos.turb.2022$ThreeMonth) # model one with just total precip
#moos.lm.turb.5 <- lm(HI.moos.turb.2022$HI ~ HI.moos.turb.2022$temp.week) # model one with just total precip


# abs #
HI.moos.abs.2022 <- left_join(HI.mean.precip.moos.abs, MOOS.2022.per.storm.1, by = "storm.num")
HI.moos.abs.2022 <- left_join(HI.moos.abs.2022, MOOS.2022.per.storm.2, by = "storm.num")
HI.moos.abs.2022 <- left_join(HI.moos.abs.2022, MOOS.2022.per.storm.3, by = "storm.num")
HI.moos.abs.2022 <- left_join(HI.moos.abs.2022, MOOS.2022.per.storm.4, by = "storm.num")
#HI.moos.abs.2021 <- left_join(HI.moos.abs.2021, MOOS.2021.per.storm.5, by = "storm.num")

moos.lm.abs <- lm(HI.moos.abs.2022$HI ~ HI.moos.abs.2022$precip) # model one with just total precip
moos.lm.abs.2 <- lm(HI.moos.abs.2022$HI ~ HI.moos.abs.2022$precip.week) # model one with just total precip
moos.lm.abs.3 <- lm(HI.moos.abs.2022$HI ~ HI.moos.abs.2022$precip.month) # model one with just total precip
moos.lm.abs.4 <- lm(HI.moos.abs.2022$HI ~ HI.moos.abs.2022$ThreeMonth) # model one with just total precip
#moos.lm.abs.5 <- lm(HI.moos.abs.2022$HI ~ HI.moos.abs.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
MOOS.2022.1$day <- julian(MOOS.2022.1$datetimeAK, origin = as.POSIXct('2022-01-01', tz = 'America/Anchorage')) # making a fractional day column 
MOOS.2022.1$day <- as.numeric(MOOS.2022.1$day)

MOOS.2022.per.storm.5 <- MOOS.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.moos.fDOM.2.2022 <- left_join(HI.moos.fDOM.2022, MOOS.2022.per.storm.5, by = "storm.num")

moos.lm.fDOM.5 <- lm(HI.moos.fDOM.2.2022$HI ~ HI.moos.fDOM.2.2022$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.moos.SPC.2.2022 <- left_join(HI.moos.SPC.2022, MOOS.2022.per.storm.5, by = "storm.num")
moos.lm.SPC.5 <- lm(HI.moos.SPC.2.2022$HI ~ HI.moos.SPC.2.2022$doy)

HI.moos.turb.2.2022 <- left_join(HI.moos.turb.2022, MOOS.2022.per.storm.5, by = "storm.num")
moos.lm.turb.5 <- lm(HI.moos.turb.2.2022$HI ~ HI.moos.turb.2.2022$doy)


HI.moos.no3.2.2022 <- left_join(HI.moos.no3.2022, MOOS.2022.per.storm.5, by = "storm.num")
moos.lm.no3.5 <- lm(HI.moos.no3.2.2022$HI ~ HI.moos.no3.2.2022$doy)

HI.moos.abs.2.2022 <- left_join(HI.moos.abs.2022, MOOS.2022.per.storm.5, by = "storm.num")
moos.lm.abs.5 <- lm(HI.moos.turb.2.2022$HI ~ HI.moos.turb.2.2022$doy)



HI.moos.2022 <- rbind(HI.moos.no3.2.2022, HI.moos.fDOM.2.2022, HI.moos.SPC.2.2022, HI.moos.turb.2.2022, HI.moos.abs.2.2022) # merging all responses together


HI.moos.2022$date <- as.Date(HI.moos.2022$doy, origin = "2022-01-01")
origin_date <- as.Date("2022-05-13")
HI.moos.2022$TimeSinceChena <- julian(HI.moos.2022$date, origin_date)

write.csv(here("Storms_clean_repo", "Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.moos.2022.csv"))

# write.csv(HI.moos.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2022/HI.moos.2022.csv")

### POKE ####
POKEstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

# POKEstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
#                                   recursive=F, 
#                                   pattern="POKE", 
#                                   full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 139),
                          rep("storm2", 119),
                          rep("storm3", 95),
                          rep("storm4", 187))

POKE_storms$datetimeAK <- as.POSIXct(POKE_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
POKE.2022.storms.1<- left_join(POKE_storms, CPCRW, by = "datetimeAK")
POKE.2022.storms.1<- left_join(POKE.2022.storms.1, airtempmean, by = "datetimeAK")

names(POKE.2022.storms.1)[names(POKE.2022.storms.1) == ''] <- 'x'

POKE.2022.per.storm.1 <- POKE.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- POKE.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

POKE.2022.per.storm.1$temp <- temp$temp

#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2022-05-01", tz = "America/Anchorage"),
          as.POSIXct("2022-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
POKE.2022 <- left_join(ts, POKE.2022, by = "datetimeAK")
POKE.2022 <- left_join(POKE.2022, airtempmean, by = "datetimeAK")


POKE.2022$week <- rollapplyr(POKE.2022$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2022$month <- rollapplyr(POKE.2022$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2022$ThreeMonth <- rollapplyr(POKE.2022$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.2022$temp.week <- rollapplyr(POKE.2022$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms 
POKE.2022.1 <- left_join(POKE.2022.storms.1, POKE.2022, by = c("datetimeAK", "NO3", "fDOM", "SPC", "Turb", "ABS_254", "site.ID",
                                                               "CRREL", "CARI")) # week month and 3 month precip totals 
# renaming antecedent month
names(POKE.2022.1)[names(POKE.2022.1) == 'month.y'] <- 'month'
names(POKE.2022.1)[names(POKE.2022.1) == 'airtemp_100.1000cm_mean.y'] <- 'airtemp_100.1000cm_mean'

POKE.2022.per.storm.2 <- POKE.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
POKE.2022.per.storm.3 <- POKE.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
POKE.2022.per.storm.4 <- POKE.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# POKE.2022.per.storm.5 <- POKE.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.poke.NO3 <- subset(HI.mean.precip.response, year == "2022" & site.ID == "POKE" & response == "NO3")
HI.mean.precip.poke.fDOM <- subset(HI.mean.precip.response, year == "2022" & site.ID == "POKE" & response == "fDOM")
HI.mean.precip.poke.SPC <- subset(HI.mean.precip.response, year == "2022" & site.ID == "POKE" & response == "SPC")
HI.mean.precip.poke.turb <- subset(HI.mean.precip.response, year == "2022" & site.ID == "POKE" & response == "turb")
HI.mean.precip.poke.abs <- subset(HI.mean.precip.response, year == "2022" & site.ID == "POKE" & response == "abs")

# NO3
HI.poke.no3.2022 <- left_join(HI.mean.precip.poke.NO3, POKE.2022.per.storm.1, by = "storm.num")
HI.poke.no3.2022 <- left_join(HI.poke.no3.2022, POKE.2022.per.storm.2, by = "storm.num")
HI.poke.no3.2022 <- left_join(HI.poke.no3.2022, POKE.2022.per.storm.3, by = "storm.num")
HI.poke.no3.2022 <- left_join(HI.poke.no3.2022, POKE.2022.per.storm.4, by = "storm.num")
#HI.poke.no3.2022 <- left_join(HI.poke.no3.2022, MOOS.2022.per.storm.5, by = "storm.num")

poke.lm.no3 <- lm(HI.poke.no3.2022$HI ~ HI.poke.no3.2022$precip) # model one with just total precip
poke.lm.no3.2 <- lm(HI.poke.no3.2022$HI ~ HI.poke.no3.2022$precip.week) # model one with just total precip
poke.lm.no3.3 <- lm(HI.poke.no3.2022$HI ~ HI.poke.no3.2022$precip.month) # model one with just total precip
poke.lm.no3.4 <- lm(HI.poke.no3.2022$HI ~ HI.poke.no3.2022$ThreeMonth) # model one with just total precip

# fDOM #
HI.poke.fDOM.2022 <- left_join(HI.mean.precip.poke.fDOM, POKE.2022.per.storm.1, by = "storm.num")
HI.poke.fDOM.2022 <- left_join(HI.poke.fDOM.2022, POKE.2022.per.storm.2, by = "storm.num")
HI.poke.fDOM.2022 <- left_join(HI.poke.fDOM.2022, POKE.2022.per.storm.3, by = "storm.num")
HI.poke.fDOM.2022 <- left_join(HI.poke.fDOM.2022, POKE.2022.per.storm.4, by = "storm.num")
#HI.poke.fDOM.2021 <- left_join(HI.poke.fDOM.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.fDOM <- lm(HI.poke.fDOM.2022$HI ~ HI.poke.fDOM.2022$precip) # model one with just total precip
poke.lm.fDOM.2 <- lm(HI.poke.fDOM.2022$HI ~ HI.poke.fDOM.2022$precip.week) # model one with just total precip
poke.lm.fDOM.3 <- lm(HI.poke.fDOM.2022$HI ~ HI.poke.fDOM.2022$precip.month) # model one with just total precip
poke.lm.fDOM.4 <- lm(HI.poke.fDOM.2022$HI ~ HI.poke.fDOM.2022$ThreeMonth) # model one with just total precip
#poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2022$HI ~ HI.poke.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.poke.SPC.2022 <- left_join(HI.mean.precip.poke.SPC, POKE.2022.per.storm.1, by = "storm.num")
HI.poke.SPC.2022 <- left_join(HI.poke.SPC.2022, POKE.2022.per.storm.2, by = "storm.num")
HI.poke.SPC.2022 <- left_join(HI.poke.SPC.2022, POKE.2022.per.storm.3, by = "storm.num")
HI.poke.SPC.2022 <- left_join(HI.poke.SPC.2022, POKE.2022.per.storm.4, by = "storm.num")
#HI.poke.SPC.2021 <- left_join(HI.poke.SPC.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.SPC <- lm(HI.poke.SPC.2022$HI ~ HI.poke.SPC.2022$precip) # model one with just total precip
poke.lm.SPC.2 <- lm(HI.poke.SPC.2022$HI ~ HI.poke.SPC.2022$precip.week) # model one with just total precip
poke.lm.SPC.3 <- lm(HI.poke.SPC.2022$HI ~ HI.poke.SPC.2022$precip.month) # model one with just total precip
poke.lm.SPC.4 <- lm(HI.poke.SPC.2022$HI ~ HI.poke.SPC.2022$ThreeMonth) # model one with just total precip
#poke.lm.SPC.5 <- lm(HI.poke.SPC.2022$HI ~ HI.poke.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.poke.turb.2022 <- left_join(HI.mean.precip.poke.turb, POKE.2022.per.storm.1, by = "storm.num")
HI.poke.turb.2022 <- left_join(HI.poke.turb.2022, POKE.2022.per.storm.2, by = "storm.num")
HI.poke.turb.2022 <- left_join(HI.poke.turb.2022, POKE.2022.per.storm.3, by = "storm.num")
HI.poke.turb.2022 <- left_join(HI.poke.turb.2022, POKE.2022.per.storm.4, by = "storm.num")
#HI.poke.turb.2021 <- left_join(HI.poke.turb.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.turb <- lm(HI.poke.turb.2022$HI ~ HI.poke.turb.2022$precip) # model one with just total precip
poke.lm.turb.2 <- lm(HI.poke.turb.2022$HI ~ HI.poke.turb.2022$precip.week) # model one with just total precip
poke.lm.turb.3 <- lm(HI.poke.turb.2022$HI ~ HI.poke.turb.2022$precip.month) # model one with just total precip
poke.lm.turb.4 <- lm(HI.poke.turb.2022$HI ~ HI.poke.turb.2022$ThreeMonth) # model one with just total precip
#poke.lm.turb.5 <- lm(HI.poke.turb.2022$HI ~ HI.poke.turb.2022$temp.week) # model one with just total precip


# abs #
HI.poke.abs.2022 <- left_join(HI.mean.precip.poke.abs, POKE.2022.per.storm.1, by = "storm.num")
HI.poke.abs.2022 <- left_join(HI.poke.abs.2022, POKE.2022.per.storm.2, by = "storm.num")
HI.poke.abs.2022 <- left_join(HI.poke.abs.2022, POKE.2022.per.storm.3, by = "storm.num")
HI.poke.abs.2022 <- left_join(HI.poke.abs.2022, POKE.2022.per.storm.4, by = "storm.num")
#HI.poke.abs.2021 <- left_join(HI.poke.abs.2021, POKE.2021.per.storm.5, by = "storm.num")

poke.lm.abs <- lm(HI.poke.abs.2022$HI ~ HI.poke.abs.2022$precip) # model one with just total precip
poke.lm.abs.2 <- lm(HI.poke.abs.2022$HI ~ HI.poke.abs.2022$precip.week) # model one with just total precip
poke.lm.abs.3 <- lm(HI.poke.abs.2022$HI ~ HI.poke.abs.2022$precip.month) # model one with just total precip
poke.lm.abs.4 <- lm(HI.poke.abs.2022$HI ~ HI.poke.abs.2022$ThreeMonth) # model one with just total precip
#poke.lm.abs.5 <- lm(HI.poke.abs.2022$HI ~ HI.poke.abs.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
POKE.2022.1$day <- julian(POKE.2022.1$datetimeAK, origin = as.POSIXct('2022-01-01', tz = 'America/Anchorage')) # making a fractional day column 
POKE.2022.1$day <- as.numeric(POKE.2022.1$day)

POKE.2022.per.storm.5 <- POKE.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.poke.fDOM.2.2022 <- left_join(HI.poke.fDOM.2022, POKE.2022.per.storm.5, by = "storm.num")

poke.lm.fDOM.5 <- lm(HI.poke.fDOM.2.2022$HI ~ HI.poke.fDOM.2.2022$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.poke.SPC.2.2022 <- left_join(HI.poke.SPC.2022, POKE.2022.per.storm.5, by = "storm.num")
poke.lm.SPC.5 <- lm(HI.poke.SPC.2.2022$HI ~ HI.poke.SPC.2.2022$doy)

HI.poke.turb.2.2022 <- left_join(HI.poke.turb.2022, POKE.2022.per.storm.5, by = "storm.num")
poke.lm.turb.5 <- lm(HI.poke.turb.2.2022$HI ~ HI.poke.turb.2.2022$doy)


HI.poke.no3.2.2022 <- left_join(HI.poke.no3.2022, POKE.2022.per.storm.5, by = "storm.num")
poke.lm.no3.5 <- lm(HI.poke.no3.2.2022$HI ~ HI.poke.no3.2.2022$doy)

HI.poke.abs.2.2022 <- left_join(HI.poke.abs.2022, POKE.2022.per.storm.5, by = "storm.num")
poke.lm.abs.5 <- lm(HI.poke.turb.2.2022$HI ~ HI.poke.turb.2.2022$doy)



HI.poke.2022 <- rbind(HI.poke.no3.2.2022, HI.poke.fDOM.2.2022, HI.poke.SPC.2.2022, HI.poke.turb.2.2022, HI.poke.abs.2.2022) # merging all responses together


HI.poke.2022$date <- as.Date(HI.poke.2022$doy, origin = "2022-01-01")
origin_date <- as.Date("2022-05-13")
HI.poke.2022$TimeSinceChena <- julian(HI.poke.2022$date, origin_date)


write.csv(here("Storms_clean_repo", "Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.poke.2022.csv"))

# write.csv(HI.poke.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2022/HI.poke.2022.csv")


### STRT ####
STRTstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

# STRTstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
#                                   recursive=F, 
#                                   pattern="STRT", 
#                                   full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm1", 103),
                          rep("storm2", 191),
                          rep("storm3", 107))
                          

STRT_storms$datetimeAK <- as.POSIXct(STRT_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
STRT.2022.storms.1<- left_join(STRT_storms, CPCRW, by = "datetimeAK")
STRT.2022.storms.1<- left_join(STRT.2022.storms.1, airtempmean, by = "datetimeAK")

names(STRT.2022.storms.1)[names(STRT.2022.storms.1) == ''] <- 'x'

STRT.2022.per.storm.1 <- STRT.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- STRT.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

STRT.2022.per.storm.1$temp <- temp$temp

#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2022-05-01", tz = "America/Anchorage"),
          as.POSIXct("2022-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
STRT.2022 <- left_join(ts, STRT.2022, by = "datetimeAK")
STRT.2022 <- left_join(STRT.2022, airtempmean, by = "datetimeAK")


STRT.2022$week <- rollapplyr(STRT.2022$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2022$month <- rollapplyr(STRT.2022$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2022$ThreeMonth <- rollapplyr(STRT.2022$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
STRT.2022$temp.week <- rollapplyr(STRT.2022$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms 
STRT.2022.1 <- left_join(STRT.2022.storms.1, STRT.2022, by = c("datetimeAK", "NO3", "fDOM", "SPC", "Turb", "ABS_254", "site.ID",
                                                               "CRREL", "CARI")) # week month and 3 month precip totals 
# renaming antecedent month
names(STRT.2022.1)[names(STRT.2022.1) == 'month.y'] <- 'month'
names(STRT.2022.1)[names(STRT.2022.1) == 'airtemp_100.1000cm_mean.y'] <- 'airtemp_100.1000cm_mean'

STRT.2022.per.storm.2 <- STRT.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
STRT.2022.per.storm.3 <- STRT.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
STRT.2022.per.storm.4 <- STRT.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# STRT.2022.per.storm.5 <- STRT.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.strt.NO3 <- subset(HI.mean.precip.response, year == "2022" & site.ID == "STRT" & response == "NO3")
HI.mean.precip.strt.fDOM <- subset(HI.mean.precip.response, year == "2022" & site.ID == "STRT" & response == "fDOM")
HI.mean.precip.strt.SPC <- subset(HI.mean.precip.response, year == "2022" & site.ID == "STRT" & response == "SPC")
HI.mean.precip.strt.turb <- subset(HI.mean.precip.response, year == "2022" & site.ID == "STRT" & response == "turb")
HI.mean.precip.strt.abs <- subset(HI.mean.precip.response, year == "2022" & site.ID == "STRT" & response == "abs")

# NO3
HI.strt.no3.2022 <- left_join(HI.mean.precip.strt.NO3, STRT.2022.per.storm.1, by = "storm.num")
HI.strt.no3.2022 <- left_join(HI.strt.no3.2022, STRT.2022.per.storm.2, by = "storm.num")
HI.strt.no3.2022 <- left_join(HI.strt.no3.2022, STRT.2022.per.storm.3, by = "storm.num")
HI.strt.no3.2022 <- left_join(HI.strt.no3.2022, STRT.2022.per.storm.4, by = "storm.num")
#HI.strt.no3.2022 <- left_join(HI.strt.no3.2022, STRT.2022.per.storm.5, by = "storm.num")

strt.lm.no3 <- lm(HI.strt.no3.2022$HI ~ HI.strt.no3.2022$precip) # model one with just total precip
strt.lm.no3.2 <- lm(HI.strt.no3.2022$HI ~ HI.strt.no3.2022$precip.week) # model one with just total precip
strt.lm.no3.3 <- lm(HI.strt.no3.2022$HI ~ HI.strt.no3.2022$precip.month) # model one with just total precip
strt.lm.no3.4 <- lm(HI.strt.no3.2022$HI ~ HI.strt.no3.2022$ThreeMonth) # model one with just total precip

# fDOM #
HI.strt.fDOM.2022 <- left_join(HI.mean.precip.strt.fDOM, STRT.2022.per.storm.1, by = "storm.num")
HI.strt.fDOM.2022 <- left_join(HI.strt.fDOM.2022, STRT.2022.per.storm.2, by = "storm.num")
HI.strt.fDOM.2022 <- left_join(HI.strt.fDOM.2022, STRT.2022.per.storm.3, by = "storm.num")
HI.strt.fDOM.2022 <- left_join(HI.strt.fDOM.2022, STRT.2022.per.storm.4, by = "storm.num")
#HI.strt.fDOM.2021 <- left_join(HI.strt.fDOM.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.fDOM <- lm(HI.strt.fDOM.2022$HI ~ HI.strt.fDOM.2022$precip) # model one with just total precip
strt.lm.fDOM.2 <- lm(HI.strt.fDOM.2022$HI ~ HI.strt.fDOM.2022$precip.week) # model one with just total precip
strt.lm.fDOM.3 <- lm(HI.strt.fDOM.2022$HI ~ HI.strt.fDOM.2022$precip.month) # model one with just total precip
strt.lm.fDOM.4 <- lm(HI.strt.fDOM.2022$HI ~ HI.strt.fDOM.2022$ThreeMonth) # model one with just total precip
#strt.lm.fDOM.5 <- lm(HI.poke.fDOM.2022$HI ~ HI.strt.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.strt.SPC.2022 <- left_join(HI.mean.precip.strt.SPC, STRT.2022.per.storm.1, by = "storm.num")
HI.strt.SPC.2022 <- left_join(HI.strt.SPC.2022, STRT.2022.per.storm.2, by = "storm.num")
HI.strt.SPC.2022 <- left_join(HI.strt.SPC.2022, STRT.2022.per.storm.3, by = "storm.num")
HI.strt.SPC.2022 <- left_join(HI.strt.SPC.2022, STRT.2022.per.storm.4, by = "storm.num")
#HI.strt.SPC.2021 <- left_join(HI.strt.SPC.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.SPC <- lm(HI.strt.SPC.2022$HI ~ HI.strt.SPC.2022$precip) # model one with just total precip
strt.lm.SPC.2 <- lm(HI.strt.SPC.2022$HI ~ HI.strt.SPC.2022$precip.week) # model one with just total precip
strt.lm.SPC.3 <- lm(HI.strt.SPC.2022$HI ~ HI.strt.SPC.2022$precip.month) # model one with just total precip
strt.lm.SPC.4 <- lm(HI.strt.SPC.2022$HI ~ HI.strt.SPC.2022$ThreeMonth) # model one with just total precip
#strt.lm.SPC.5 <- lm(HI.strt.SPC.2022$HI ~ HI.strt.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.strt.turb.2022 <- left_join(HI.mean.precip.strt.turb, STRT.2022.per.storm.1, by = "storm.num")
HI.strt.turb.2022 <- left_join(HI.strt.turb.2022, STRT.2022.per.storm.2, by = "storm.num")
HI.strt.turb.2022 <- left_join(HI.strt.turb.2022, STRT.2022.per.storm.3, by = "storm.num")
HI.strt.turb.2022 <- left_join(HI.strt.turb.2022, STRT.2022.per.storm.4, by = "storm.num")
#HI.strt.turb.2021 <- left_join(HI.strt.turb.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.turb <- lm(HI.strt.turb.2022$HI ~ HI.strt.turb.2022$precip) # model one with just total precip
strt.lm.turb.2 <- lm(HI.strt.turb.2022$HI ~ HI.strt.turb.2022$precip.week) # model one with just total precip
strt.lm.turb.3 <- lm(HI.strt.turb.2022$HI ~ HI.strt.turb.2022$precip.month) # model one with just total precip
strt.lm.turb.4 <- lm(HI.strt.turb.2022$HI ~ HI.strt.turb.2022$ThreeMonth) # model one with just total precip
#strt.lm.turb.5 <- lm(HI.strt.turb.2022$HI ~ HI.strt.turb.2022$temp.week) # model one with just total precip


# abs #
HI.strt.abs.2022 <- left_join(HI.mean.precip.strt.abs, STRT.2022.per.storm.1, by = "storm.num")
HI.strt.abs.2022 <- left_join(HI.strt.abs.2022, STRT.2022.per.storm.2, by = "storm.num")
HI.strt.abs.2022 <- left_join(HI.strt.abs.2022, STRT.2022.per.storm.3, by = "storm.num")
HI.strt.abs.2022 <- left_join(HI.strt.abs.2022, STRT.2022.per.storm.4, by = "storm.num")
#HI.strt.abs.2021 <- left_join(HI.strt.abs.2021, STRT.2021.per.storm.5, by = "storm.num")

strt.lm.abs <- lm(HI.strt.abs.2022$HI ~ HI.strt.abs.2022$precip) # model one with just total precip
strt.lm.abs.2 <- lm(HI.strt.abs.2022$HI ~ HI.strt.abs.2022$precip.week) # model one with just total precip
strt.lm.abs.3 <- lm(HI.strt.abs.2022$HI ~ HI.strt.abs.2022$precip.month) # model one with just total precip
strt.lm.abs.4 <- lm(HI.strt.abs.2022$HI ~ HI.strt.abs.2022$ThreeMonth) # model one with just total precip
#strt.lm.abs.5 <- lm(HI.strt.abs.2022$HI ~ HI.strt.abs.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
STRT.2022.1$day <- julian(STRT.2022.1$datetimeAK, origin = as.POSIXct('2022-01-01', tz = 'America/Anchorage')) # making a fractional day column 
STRT.2022.1$day <- as.numeric(STRT.2022.1$day)

STRT.2022.per.storm.5 <- STRT.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.strt.fDOM.2.2022 <- left_join(HI.strt.fDOM.2022, STRT.2022.per.storm.5, by = "storm.num")

strt.lm.fDOM.5 <- lm(HI.strt.fDOM.2.2022$HI ~ HI.strt.fDOM.2.2022$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.strt.SPC.2.2022 <- left_join(HI.strt.SPC.2022, STRT.2022.per.storm.5, by = "storm.num")
strt.lm.SPC.5 <- lm(HI.strt.SPC.2.2022$HI ~ HI.strt.SPC.2.2022$doy)

HI.strt.turb.2.2022 <- left_join(HI.strt.turb.2022, STRT.2022.per.storm.5, by = "storm.num")
strt.lm.turb.5 <- lm(HI.strt.turb.2.2022$HI ~ HI.strt.turb.2.2022$doy)


HI.strt.no3.2.2022 <- left_join(HI.strt.no3.2022, STRT.2022.per.storm.5, by = "storm.num")
strt.lm.no3.5 <- lm(HI.strt.no3.2.2022$HI ~ HI.strt.no3.2.2022$doy)

HI.strt.abs.2.2022 <- left_join(HI.strt.abs.2022, STRT.2022.per.storm.5, by = "storm.num")
strt.lm.abs.5 <- lm(HI.strt.turb.2.2022$HI ~ HI.strt.turb.2.2022$doy)



HI.strt.2022 <- rbind(HI.strt.no3.2.2022, HI.strt.fDOM.2.2022, HI.strt.SPC.2.2022, HI.strt.turb.2.2022, HI.strt.abs.2.2022) # merging all responses together


HI.strt.2022$date <- as.Date(HI.strt.2022$doy, origin = "2022-01-01")
origin_date <- as.Date("2022-05-13")
HI.strt.2022$TimeSinceChena <- julian(HI.strt.2022$date, origin_date)


write.csv(here("Storms_clean_repo", "Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.strt.2022.csv"))

# write.csv(HI.strt.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2022/HI.strt.2022.csv")


### VAUL ####
VAULstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

# VAULstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
#                                   recursive=F, 
#                                   pattern="VAUL", 
#                                   full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm1", 127),
                          rep("storm2", 763))
                          


VAUL_storms$datetimeAK <- as.POSIXct(VAUL_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
VAUL.2022.storms.1<- left_join(VAUL_storms, CPCRW, by = "datetimeAK")
VAUL.2022.storms.1<- left_join(VAUL.2022.storms.1, airtempmean, by = "datetimeAK")

names(VAUL.2022.storms.1)[names(VAUL.2022.storms.1) == ''] <- 'x'

VAUL.2022.per.storm.1 <- VAUL.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- VAUL.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

VAUL.2022.per.storm.1$temp <- temp$temp

#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2022-05-01", tz = "America/Anchorage"),
          as.POSIXct("2022-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
VAUL.2022 <- left_join(ts, VAUL.2022, by = "datetimeAK")
VAUL.2022 <- left_join(VAUL.2022, airtempmean, by = "datetimeAK")


VAUL.2022$week <- rollapplyr(VAUL.2022$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2022$month <- rollapplyr(VAUL.2022$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2022$ThreeMonth <- rollapplyr(VAUL.2022$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
VAUL.2022$temp.week <- rollapplyr(VAUL.2022$airtemp_100.1000cm_mean.x, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)

# joining with storms 
VAUL.2022.1 <- left_join(VAUL.2022.storms.1, VAUL.2022, by = c("datetimeAK", "NO3", "fDOM", "SPC", "Turb", "ABS_254", "site.ID",
                                                               "CRREL", "CARI")) # week month and 3 month precip totals 
# renaming antecedent month
names(VAUL.2022.1)[names(VAUL.2022.1) == 'month.y'] <- 'month'
names(VAUL.2022.1)[names(VAUL.2022.1) == 'airtemp_100.1000cm_mean.y'] <- 'airtemp_100.1000cm_mean'

VAUL.2022.per.storm.2 <- VAUL.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
VAUL.2022.per.storm.3 <- VAUL.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
VAUL.2022.per.storm.4 <- VAUL.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# VAUL2022.per.storm.5 <- VAUL.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.vaul.NO3 <- subset(HI.mean.precip.response, year == "2022" & site.ID == "VAUL" & response == "NO3")
HI.mean.precip.vaul.fDOM <- subset(HI.mean.precip.response, year == "2022" & site.ID == "VAUL" & response == "fDOM")
HI.mean.precip.vaul.SPC <- subset(HI.mean.precip.response, year == "2022" & site.ID == "VAUL" & response == "SPC")
HI.mean.precip.vaul.turb <- subset(HI.mean.precip.response, year == "2022" & site.ID == "VAUL" & response == "turb")
HI.mean.precip.vaul.abs <- subset(HI.mean.precip.response, year == "2022" & site.ID == "VAUL" & response == "abs")

# NO3
HI.vaul.no3.2022 <- left_join(HI.mean.precip.vaul.NO3, VAUL.2022.per.storm.1, by = "storm.num")
HI.vaul.no3.2022 <- left_join(HI.vaul.no3.2022, VAUL.2022.per.storm.2, by = "storm.num")
HI.vaul.no3.2022 <- left_join(HI.vaul.no3.2022, VAUL.2022.per.storm.3, by = "storm.num")
HI.vaul.no3.2022 <- left_join(HI.vaul.no3.2022, VAUL.2022.per.storm.4, by = "storm.num")
#HI.vaul.no3.2022 <- left_join(HI.vaul.no3.2022, VAUL.2022.per.storm.5, by = "storm.num")

vaul.lm.no3 <- lm(HI.vaul.no3.2022$HI ~ HI.vaul.no3.2022$precip) # model one with just total precip
vaul.lm.no3.2 <- lm(HI.vaul.no3.2022$HI ~ HI.vaul.no3.2022$precip.week) # model one with just total precip
vaul.lm.no3.3 <- lm(HI.vaul.no3.2022$HI ~ HI.vaul.no3.2022$precip.month) # model one with just total precip
vaul.lm.no3.4 <- lm(HI.vaul.no3.2022$HI ~ HI.vaul.no3.2022$ThreeMonth) # model one with just total precip

# fDOM #
HI.vaul.fDOM.2022 <- left_join(HI.mean.precip.vaul.fDOM, VAUL.2022.per.storm.1, by = "storm.num")
HI.vaul.fDOM.2022 <- left_join(HI.vaul.fDOM.2022, VAUL.2022.per.storm.2, by = "storm.num")
HI.vaul.fDOM.2022 <- left_join(HI.vaul.fDOM.2022, VAUL.2022.per.storm.3, by = "storm.num")
HI.vaul.fDOM.2022 <- left_join(HI.vaul.fDOM.2022, VAUL.2022.per.storm.4, by = "storm.num")
#HI.vaul.fDOM.2021 <- left_join(HI.vaul.fDOM.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.fDOM <- lm(HI.vaul.fDOM.2022$HI ~ HI.vaul.fDOM.2022$precip) # model one with just total precip
vaul.lm.fDOM.2 <- lm(HI.vaul.fDOM.2022$HI ~ HI.vaul.fDOM.2022$precip.week) # model one with just total precip
vaul.lm.fDOM.3 <- lm(HI.vaul.fDOM.2022$HI ~ HI.vaul.fDOM.2022$precip.month) # model one with just total precip
vaul.lm.fDOM.4 <- lm(HI.vaul.fDOM.2022$HI ~ HI.vaul.fDOM.2022$ThreeMonth) # model one with just total precip
#vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2022$HI ~ HI.vaul.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.vaul.SPC.2022 <- left_join(HI.mean.precip.vaul.SPC, VAUL.2022.per.storm.1, by = "storm.num")
HI.vaul.SPC.2022 <- left_join(HI.vaul.SPC.2022, VAUL.2022.per.storm.2, by = "storm.num")
HI.vaul.SPC.2022 <- left_join(HI.vaul.SPC.2022, VAUL.2022.per.storm.3, by = "storm.num")
HI.vaul.SPC.2022 <- left_join(HI.vaul.SPC.2022, VAUL.2022.per.storm.4, by = "storm.num")
#HI.vaul.SPC.2021 <- left_join(HI.vaul.SPC.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.SPC <- lm(HI.vaul.SPC.2022$HI ~ HI.vaul.SPC.2022$precip) # model one with just total precip
vaul.lm.SPC.2 <- lm(HI.vaul.SPC.2022$HI ~ HI.vaul.SPC.2022$precip.week) # model one with just total precip
vaul.lm.SPC.3 <- lm(HI.vaul.SPC.2022$HI ~ HI.vaul.SPC.2022$precip.month) # model one with just total precip
vaul.lm.SPC.4 <- lm(HI.vaul.SPC.2022$HI ~ HI.vaul.SPC.2022$ThreeMonth) # model one with just total precip
#vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2022$HI ~ HI.vaul.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.vaul.turb.2022 <- left_join(HI.mean.precip.vaul.turb, VAUL.2022.per.storm.1, by = "storm.num")
HI.vaul.turb.2022 <- left_join(HI.vaul.turb.2022, VAUL.2022.per.storm.2, by = "storm.num")
HI.vaul.turb.2022 <- left_join(HI.vaul.turb.2022, VAUL.2022.per.storm.3, by = "storm.num")
HI.vaul.turb.2022 <- left_join(HI.vaul.turb.2022, VAUL.2022.per.storm.4, by = "storm.num")
#HI.vaul.turb.2021 <- left_join(HI.vaul.turb.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.turb <- lm(HI.vaul.turb.2022$HI ~ HI.vaul.turb.2022$precip) # model one with just total precip
vaul.lm.turb.2 <- lm(HI.vaul.turb.2022$HI ~ HI.vaul.turb.2022$precip.week) # model one with just total precip
vaul.lm.turb.3 <- lm(HI.vaul.turb.2022$HI ~ HI.vaul.turb.2022$precip.month) # model one with just total precip
vaul.lm.turb.4 <- lm(HI.vaul.turb.2022$HI ~ HI.vaul.turb.2022$ThreeMonth) # model one with just total precip
#vaul.lm.turb.5 <- lm(HI.vaul.turb.2022$HI ~ HI.vaul.turb.2022$temp.week) # model one with just total precip


# abs #
HI.vaul.abs.2022 <- left_join(HI.mean.precip.vaul.abs, VAUL.2022.per.storm.1, by = "storm.num")
HI.vaul.abs.2022 <- left_join(HI.vaul.abs.2022, VAUL.2022.per.storm.2, by = "storm.num")
HI.vaul.abs.2022 <- left_join(HI.vaul.abs.2022, VAUL.2022.per.storm.3, by = "storm.num")
HI.vaul.abs.2022 <- left_join(HI.vaul.abs.2022, VAUL.2022.per.storm.4, by = "storm.num")
#HI.vaul.abs.2021 <- left_join(HI.vaul.abs.2021, VAUL.2021.per.storm.5, by = "storm.num")

vaul.lm.abs <- lm(HI.vaul.abs.2022$HI ~ HI.vaul.abs.2022$precip) # model one with just total precip
vaul.lm.abs.2 <- lm(HI.vaul.abs.2022$HI ~ HI.vaul.abs.2022$precip.week) # model one with just total precip
vaul.lm.abs.3 <- lm(HI.vaul.abs.2022$HI ~ HI.vaul.abs.2022$precip.month) # model one with just total precip
vaul.lm.abs.4 <- lm(HI.vaul.abs.2022$HI ~ HI.vaul.abs.2022$ThreeMonth) # model one with just total precip
#vaul.lm.abs.5 <- lm(HI.vaul.abs.2022$HI ~ HI.vaul.abs.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
VAUL.2022.1$day <- julian(VAUL.2022.1$datetimeAK, origin = as.POSIXct('2022-01-01', tz = 'America/Anchorage')) # making a fractional day column 
VAUL.2022.1$day <- as.numeric(VAUL.2022.1$day)

VAUL.2022.per.storm.5 <- VAUL.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.vaul.fDOM.2.2022 <- left_join(HI.vaul.fDOM.2022, VAUL.2022.per.storm.5, by = "storm.num")

vaul.lm.fDOM.5 <- lm(HI.vaul.fDOM.2.2022$HI ~ HI.vaul.fDOM.2.2022$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.vaul.SPC.2.2022 <- left_join(HI.vaul.SPC.2022, VAUL.2022.per.storm.5, by = "storm.num")
vaul.lm.SPC.5 <- lm(HI.vaul.SPC.2.2022$HI ~ HI.vaul.SPC.2.2022$doy)

HI.vaul.turb.2.2022 <- left_join(HI.vaul.turb.2022, VAUL.2022.per.storm.5, by = "storm.num")
vaul.lm.turb.5 <- lm(HI.vaul.turb.2.2022$HI ~ HI.vaul.turb.2.2022$doy)


HI.vaul.no3.2.2022 <- left_join(HI.vaul.no3.2022, VAUL.2022.per.storm.5, by = "storm.num")
vaul.lm.no3.5 <- lm(HI.vaul.no3.2.2022$HI ~ HI.vaul.no3.2.2022$doy)

HI.vaul.abs.2.2022 <- left_join(HI.vaul.abs.2022, VAUL.2022.per.storm.5, by = "storm.num")
vaul.lm.abs.5 <- lm(HI.vaul.turb.2.2022$HI ~ HI.vaul.turb.2.2022$doy)



HI.vaul.2022 <- rbind(HI.vaul.no3.2.2022, HI.vaul.fDOM.2.2022, HI.vaul.SPC.2.2022, HI.vaul.turb.2.2022, HI.vaul.abs.2.2022) # merging all responses together


HI.vaul.2022$date <- as.Date(HI.vaul.2022$doy, origin = "2022-01-01")
origin_date <- as.Date("2022-05-13")
HI.vaul.2022$TimeSinceChena <- julian(HI.vaul.2022$date, origin_date)


write.csv(here("Storms_clean_repo", "Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.vaul.2022.csv"))

# write.csv(HI.vaul.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2022/HI.vaul.2022.csv")


### CARI ####
CARIstorm_file_list <- list.files(path = "All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

# CARIstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
#                                   recursive=F, 
#                                   pattern="CARI", 
#                                   full.names=TRUE)

CARI_storms<-do.call("rbind", lapply(CARIstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms$storm.num = c(rep("storm1", 231),
                          rep("storm2", 190),
                          rep("storm3", 204),
                          rep("storm4a", 119),
                          rep("storm4b", 167),
                          rep("storm5", 379),
                          rep("storm6", 91),
                          rep("storm7", 191),
                          rep("storm8", 103))

CARI_storms$datetimeAK <- CARI_storms$DateTimeAK

CARI_storms$datetimeAK <- as.POSIXct(CARI_storms$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI.2022.storms.1<- left_join(CARI_storms, CPCRW, by = "datetimeAK")
CARI.2022.storms.1<- left_join(CARI.2022.storms.1, airtempmean, by = "datetimeAK")

names(CARI.2022.storms.1)[names(CARI.2022.storms.1) == ''] <- 'x'

CARI.2022.per.storm.1 <- CARI.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(mean), list(precip = sum), na.rm = TRUE)

temp <- CARI.2022.storms.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(airtemp_100.1000cm_mean), list(temp = mean), na.rm = TRUE) # finding the mean temperature for each storm event 

CARI.2022.per.storm.1$temp <- temp$temp

# reading in CARI DATA # 
CARI.2022 <- read.csv(here("processed_sensor_data", "2022", "NEON_Q_WaterQuality2022.csv"))

# time zones are the bane of my existence
# when I import the file that had AKDT time from the last repo it loads in as UTC 
# but when I try and change it all over it gets all wonky so here is my horrible 
# way to solve the issue
# 
CARI.2022 <- CARI.2022[c("site.ID.x", "DateTimeAK", "Discharge", "NO3", "fDOM", "SPC", "Turb")] # picking the columns I want 
names(CARI.2022) <- c("site.ID", "datetimeAK", "Discharge", "NO3", "fDOM", "SPC", "Turb") # renaming columns 
CARI.2022$site.ID <- "CARI"

CARI.2022$datetimeAK <- ymd_hms(CARI.2022$datetimeAK) # changing from character to datetime 
CARI.2022$datetimeAK <- force_tz(CARI.2022$datetimeAK, "America/Anchorage") # it already is in AK time so I want to make it recognize it without changing the actually time value 

CARI.2022 <- CARI.2022[c("site.ID", "datetimeAK", "NO3", "fDOM", "SPC", "Turb")] # clipping out just chem 
CARI.2022 <- left_join(CARI.2022, CPCRW, by = "datetimeAK")


#making a uniform time series with 15 minute intervals and then I can sum precip by 24/48hour windows 
ts <- seq(as.POSIXct("2022-05-01", tz = "America/Anchorage"),
          as.POSIXct("2022-10-31", tz = "America/Anchorage"),
          by = "15 min")
head(ts)
ts <- as.data.frame(ts)
names(ts) <- c("datetimeAK")
CARI.2022 <- left_join(ts, CARI.2022, by = "datetimeAK")
CARI.2022 <- left_join(CARI.2022, airtempmean, by = "datetimeAK")

CARI.2022 <- CARI.2022[,-c(7,10,12,13)]

CARI.2022$week <- rollapplyr(CARI.2022$mean, 672, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2022$month <- rollapplyr(CARI.2022$mean, 2688, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2022$ThreeMonth <- rollapplyr(CARI.2022$mean, 8064, sum, na.rm = TRUE, fill = NA, partial = TRUE)
CARI.2022$temp.week <- rollapplyr(CARI.2022$airtemp_100.1000cm_mean, 672, mean, na.rm = TRUE, fill = NA, partial = TRUE)


CARI.2022.storms.1 <- CARI.2022.storms.1[,-c(1:9,12:21)]

# joining with storms 
CARI.2022.1 <- left_join(CARI.2022.storms.1, CARI.2022, by = c("datetimeAK")) # week month and 3 month precip totals 
# renaming antecedent month
# names(CARI.2022.1)[names(CARI.2022.1) == 'month.y'] <- 'month'
# names(CARI.2022.1)[names(CARI.2022.1) == 'airtemp_100.1000cm_mean.y'] <- 'airtemp_100.1000cm_mean'

CARI.2022.per.storm.2 <- CARI.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(week), list(precip.week = first), na.rm = TRUE) # grouping weekly precip leading up to storm event
CARI.2022.per.storm.3 <- CARI.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(month), list(precip.month = first), na.rm = TRUE) # groouping monthly precip leading up to a storm 
CARI.2022.per.storm.4 <- CARI.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(ThreeMonth), list(ThreeMonth = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 
# CARI.2022.per.storm.5 <- CARI.2022.1 %>% group_by(storm.num) %>% 
#   summarise_at(vars(temp.week), list(temp.week = first), na.rm = TRUE) # grouping one week mean temperature leading up to a storm 

HI.mean.precip.cari.NO3 <- subset(HI.mean.precip.response, year == "2022" & site.ID == "CARI" & response == "NO3")
HI.mean.precip.cari.fDOM <- subset(HI.mean.precip.response, year == "2022" & site.ID == "CARI" & response == "fDOM")
HI.mean.precip.cari.SPC <- subset(HI.mean.precip.response, year == "2022" & site.ID == "CARI" & response == "SPC")
HI.mean.precip.cari.turb <- subset(HI.mean.precip.response, year == "2022" & site.ID == "CARI" & response == "turb")

# NO3
HI.cari.no3.2022 <- left_join(HI.mean.precip.cari.NO3, CARI.2022.per.storm.1, by = "storm.num")
HI.cari.no3.2022 <- left_join(HI.cari.no3.2022, CARI.2022.per.storm.2, by = "storm.num")
HI.cari.no3.2022 <- left_join(HI.cari.no3.2022, CARI.2022.per.storm.3, by = "storm.num")
HI.cari.no3.2022 <- left_join(HI.cari.no3.2022, CARI.2022.per.storm.4, by = "storm.num")
#HI.cari.no3.2022 <- left_join(HI.cari.no3.2022, CARI.2022.per.storm.5, by = "storm.num")

cari.lm.no3 <- lm(HI.cari.no3.2022$HI ~ HI.cari.no3.2022$precip) # model one with just total precip
cari.lm.no3.2 <- lm(HI.cari.no3.2022$HI ~ HI.cari.no3.2022$precip.week) # model one with just total precip
cari.lm.no3.3 <- lm(HI.cari.no3.2022$HI ~ HI.cari.no3.2022$precip.month) # model one with just total precip
cari.lm.no3.4 <- lm(HI.cari.no3.2022$HI ~ HI.cari.no3.2022$ThreeMonth) # model one with just total precip

# fDOM #
HI.cari.fDOM.2022 <- left_join(HI.mean.precip.cari.fDOM, CARI.2022.per.storm.1, by = "storm.num")
HI.cari.fDOM.2022 <- left_join(HI.cari.fDOM.2022, CARI.2022.per.storm.2, by = "storm.num")
HI.cari.fDOM.2022 <- left_join(HI.cari.fDOM.2022, CARI.2022.per.storm.3, by = "storm.num")
HI.cari.fDOM.2022 <- left_join(HI.cari.fDOM.2022, CARI.2022.per.storm.4, by = "storm.num")
#HI.cari.fDOM.2021 <- left_join(HI.cari.fDOM.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.fDOM <- lm(HI.cari.fDOM.2022$HI ~ HI.cari.fDOM.2022$precip) # model one with just total precip
cari.lm.fDOM.2 <- lm(HI.cari.fDOM.2022$HI ~ HI.cari.fDOM.2022$precip.week) # model one with just total precip
cari.lm.fDOM.3 <- lm(HI.cari.fDOM.2022$HI ~ HI.cari.fDOM.2022$precip.month) # model one with just total precip
cari.lm.fDOM.4 <- lm(HI.cari.fDOM.2022$HI ~ HI.cari.fDOM.2022$ThreeMonth) # model one with just total precip
#cari.lm.fDOM.5 <- lm(HI.cari.fDOM.2022$HI ~ HI.cari.fDOM.2022$temp.week) # model one with just total precip

# SPC #
HI.cari.SPC.2022 <- left_join(HI.mean.precip.cari.SPC, CARI.2022.per.storm.1, by = "storm.num")
HI.cari.SPC.2022 <- left_join(HI.cari.SPC.2022, CARI.2022.per.storm.2, by = "storm.num")
HI.cari.SPC.2022 <- left_join(HI.cari.SPC.2022, CARI.2022.per.storm.3, by = "storm.num")
HI.cari.SPC.2022 <- left_join(HI.cari.SPC.2022, CARI.2022.per.storm.4, by = "storm.num")
#HI.cari.SPC.2021 <- left_join(HI.cari.SPC.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.SPC <- lm(HI.cari.SPC.2022$HI ~ HI.cari.SPC.2022$precip) # model one with just total precip
cari.lm.SPC.2 <- lm(HI.cari.SPC.2022$HI ~ HI.cari.SPC.2022$precip.week) # model one with just total precip
cari.lm.SPC.3 <- lm(HI.cari.SPC.2022$HI ~ HI.cari.SPC.2022$precip.month) # model one with just total precip
cari.lm.SPC.4 <- lm(HI.cari.SPC.2022$HI ~ HI.cari.SPC.2022$ThreeMonth) # model one with just total precip
#cari.lm.SPC.5 <- lm(HI.cari.SPC.2022$HI ~ HI.cari.SPC.2022$temp.week) # model one with just total precip


# turb #
HI.cari.turb.2022 <- left_join(HI.mean.precip.cari.turb, CARI.2022.per.storm.1, by = "storm.num")
HI.cari.turb.2022 <- left_join(HI.cari.turb.2022, CARI.2022.per.storm.2, by = "storm.num")
HI.cari.turb.2022 <- left_join(HI.cari.turb.2022, CARI.2022.per.storm.3, by = "storm.num")
HI.cari.turb.2022 <- left_join(HI.cari.turb.2022, CARI.2022.per.storm.4, by = "storm.num")
#HI.cari.turb.2021 <- left_join(HI.cari.turb.2021, CARI.2021.per.storm.5, by = "storm.num")

cari.lm.turb <- lm(HI.cari.turb.2022$HI ~ HI.cari.turb.2022$precip) # model one with just total precip
cari.lm.turb.2 <- lm(HI.cari.turb.2022$HI ~ HI.cari.turb.2022$precip.week) # model one with just total precip
cari.lm.turb.3 <- lm(HI.cari.turb.2022$HI ~ HI.cari.turb.2022$precip.month) # model one with just total precip
cari.lm.turb.4 <- lm(HI.cari.turb.2022$HI ~ HI.cari.turb.2022$ThreeMonth) # model one with just total precip
#cari.lm.turb.5 <- lm(HI.cari.turb.2022$HI ~ HI.cari.turb.2022$temp.week) # model one with just total precip

# this would be for intensity but we are not doing this right now if just comparing to the DOD stuff
# sum.time <- FRCH.2018.storms.1 %>%
#   mutate(grp=data.table::rleid(storm.num))%>%
#   group_by(grp) %>%
#   summarise(storm.num=max(storm.num),TOTAL.TIME=difftime(max(DateTime),
#                                                          min(DateTime),units="hour"))%>%
#   group_by(storm.num) %>%
#   summarise(TOTAL.TIME=sum(TOTAL.TIME)) # creating a total time column for each individual storm and then I can generate an intensity metric which would be TotalPrecip/duration of event
# 
# HI.frch.fDOM.2.2018 <- left_join(HI.frch.fDOM.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.fDOM.2.2018$TOTAL.TIME <- as.numeric(HI.frch.fDOM.2.2018$TOTAL.TIME)
# HI.frch.fDOM.2.2018$Intensity <- HI.frch.fDOM.2.2018$precip/HI.frch.fDOM.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.fDOM.2 <- lm(HI.frch.fDOM.2.2018$HI ~ HI.frch.fDOM.2.2018$precip + HI.frch.fDOM.2.2018$Intensity) # model one with total precip and intensity 
# 
# br <- HI.frch.fDOM.2.2018 %>%
#   ggplot(aes(x=Intensity, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = frch.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("FRCH fDOM") +
#   xlab("Intensity (mm/hr)") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 
# 
# HI.frch.SPC.2.2018 <- left_join(HI.frch.SPC.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.SPC.2.2018$TOTAL.TIME <- as.numeric(HI.frch.SPC.2.2018$TOTAL.TIME)
# HI.frch.SPC.2.2018$Intensity <- HI.frch.SPC.2.2018$precip/HI.frch.SPC.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.SPC.2 <- lm(HI.frch.SPC.2.2018$HI ~ HI.frch.SPC.2.2018$precip + HI.frch.SPC.2.2018$Intensity) # model one with total precip and intensity 
# 
# HI.frch.turb.2.2018 <- left_join(HI.frch.turb.2018, sum.time, by = "storm.num") # merging total time per storm event and the HI per storm 
# HI.frch.turb.2.2018$TOTAL.TIME <- as.numeric(HI.frch.turb.2.2018$TOTAL.TIME)
# HI.frch.turb.2.2018$Intensity <- HI.frch.turb.2.2018$precip/HI.frch.turb.2.2018$TOTAL.TIME # Intensity is total precip for individual storm divided by total time so we get mm/hr
# 
# frch.lm.turb.2 <- lm(HI.frch.turb.2.2018$HI ~ HI.frch.turb.2.2018$precip + HI.frch.turb.2.2018$Intensity) # model one with total precip and intensity 

# day of year # SEASONALITY
CARI.2022.1$day <- julian(CARI.2022.1$datetimeAK, origin = as.POSIXct('2022-01-01', tz = 'America/Anchorage')) # making a fractional day column 
CARI.2022.1$day <- as.numeric(CARI.2022.1$day)

CARI.2022.per.storm.5 <- CARI.2022.1 %>% group_by(storm.num) %>% 
  summarise_at(vars(day), list(doy = first), na.rm = TRUE) # grouping 3 month precip leading up to a storm 

HI.cari.fDOM.2.2022 <- left_join(HI.cari.fDOM.2022, CARI.2022.per.storm.5, by = "storm.num")

cari.lm.fDOM.5 <- lm(HI.cari.fDOM.2.2022$HI ~ HI.cari.fDOM.2.2022$doy)

# HI.salcha.fDOM.2.2021 %>%
#   ggplot(aes(x=doy, 
#              y=HI)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   stat_poly_eq(formula = salcha.formula, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE) +
#   ggtitle("SALCHA fDOM") +
#   xlab("Day of year") +
#   ylab("HI-Solute Storage") +
#   theme_classic() # plot model 

HI.cari.SPC.2.2022 <- left_join(HI.cari.SPC.2022, CARI.2022.per.storm.5, by = "storm.num")
cari.lm.SPC.5 <- lm(HI.cari.SPC.2.2022$HI ~ HI.cari.SPC.2.2022$doy)

HI.cari.turb.2.2022 <- left_join(HI.cari.turb.2022, CARI.2022.per.storm.5, by = "storm.num")
cari.lm.turb.5 <- lm(HI.cari.turb.2.2022$HI ~ HI.cari.turb.2.2022$doy)


HI.cari.no3.2.2022 <- left_join(HI.cari.no3.2022, CARI.2022.per.storm.5, by = "storm.num")
cari.lm.no3.5 <- lm(HI.cari.no3.2.2022$HI ~ HI.cari.no3.2.2022$doy)



HI.cari.2022 <- rbind(HI.cari.no3.2.2022, HI.cari.fDOM.2.2022, HI.cari.SPC.2.2022, HI.cari.turb.2.2022) # merging all responses together


HI.cari.2022$date <- as.Date(HI.cari.2022$doy, origin = "2022-01-01")
origin_date <- as.Date("2022-05-13")
HI.cari.2022$TimeSinceChena <- julian(HI.cari.2022$date, origin_date)


write.csv(here("Storms_clean_repo", "Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.cari.2022.csv"))

# write.csv(HI.cari.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2022/HI.cari.2022.csv")



# all sites 
HI.2022 <- rbind(HI.frch.2022, HI.moos.2022, 
                 HI.poke.2022, HI.vaul.2022, 
                 HI.strt.2022, HI.cari.2022) # bind all 2022 together

write.csv(here("Storms_clean_repo", "Output_from_analysis", "04_Antecedent_Conditions", "2022", "HI.2022.csv"))

# write.csv(HI.2022, "~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2022/HI.2022.csv")




