#### READ ME ####
##The purpose of this script is to plot hysteresis loops from DoD sites data prior to hysteresis analysis and, specifically, the hysteresisMetrics function.##
# Read in storm events and run them through the HI function 

#### libraries ####
options(tz="America/Anchorage")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(imputeTS)
library(TSA)
library(bbmle)
library(zoo)
library(xts)
library(forecast)
library(stats)
library(lattice)
library(nlme)
library(geosphere)
library(car)
library(EcoHydRology)
library(dplyr)

# plot on normalized scale # 
# load data #
# any storm that is commented out is an empty dataframe due to gaps in the data
setwd("~/Documents/Storms_clean_repo")
#FRCH_storm1_06_21 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm1_06_21.csv")
FRCH_storm1_06_21_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_Q.csv")
FRCH_storm1_06_21_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_NO3.csv")
FRCH_storm1_06_21_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_fDOM.csv")
FRCH_storm1_06_21_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_SPC.csv")
FRCH_storm1_06_21_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_Turb.csv")

#FRCH_storm2a_06_29 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm2a_06_29.csv")
FRCH_storm2a_06_29_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2a_06_29_Q.csv")
FRCH_storm2a_06_29_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2a_06_29_NO3.csv")
FRCH_storm2a_06_29_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2a_06_29_fDOM.csv",
                                 col_types = cols(datavalue = col_double()))
FRCH_storm2a_06_29_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2a_06_29_SPC.csv",
                             col_types = cols(datavalue = col_double()))
FRCH_storm2a_06_29_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2a_06_29_Turb.csv",
                                   col_types = cols(datavalue = col_double()))

#FRCH_storm2b_07_04 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm2b_07_04.csv")
FRCH_storm2b_07_04_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2b_07_04_Q.csv")
FRCH_storm2b_07_04_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2b_07_04_NO3.csv")
FRCH_storm2b_07_04_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2b_07_04_fDOM.csv")
FRCH_storm2b_07_04_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2b_07_04_SPC.csv")
FRCH_storm2b_07_04_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm2b_07_04_Turb.csv")

#FRCH_storm3_07_10 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm3_07_10.csv")
FRCH_storm3_07_10_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm3_07_10_Q.csv")
FRCH_storm3_07_10_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm3_07_10_NO3.csv")
FRCH_storm3_07_10_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm3_07_10_fDOM.csv",
                                   col_types = cols(datavalue = col_double()))
FRCH_storm3_07_10_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm3_07_10_SPC.csv")
FRCH_storm3_07_10_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm3_07_10_Turb.csv")


#FRCH_storm4a_07_15 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm4a_07_15.csv")
FRCH_storm4a_07_15_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4a_07_15_Q.csv")
FRCH_storm4a_07_15_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4a_07_15_NO3.csv")
FRCH_storm4a_07_15_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4a_07_15_fDOM.csv")
FRCH_storm4a_07_15_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4a_07_15_SPC.csv")
FRCH_storm4a_07_15_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4a_07_15_Turb.csv")

#FRCH_storm4b_07_16 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm4b_07_16.csv")
FRCH_storm4b_07_16_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4b_07_16_Q.csv")
FRCH_storm4b_07_16_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4b_07_16_NO3.csv")
FRCH_storm4b_07_16_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4b_07_16_fDOM.csv")
FRCH_storm4b_07_16_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4b_07_16_SPC.csv")
FRCH_storm4b_07_16_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm4b_07_16_Turb.csv")

#FRCH_storm5_08_04 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm5_08_04.csv")
FRCH_storm5_08_04_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm5_08_04_Q.csv")
FRCH_storm5_08_04_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm5_08_04_NO3.csv")
FRCH_storm5_08_04_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm5_08_04_fDOM.csv")
FRCH_storm5_08_04_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm5_08_04_SPC.csv")
FRCH_storm5_08_04_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm5_08_04_Turb.csv")

#FRCH_storm6_08_13 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm6_08_13.csv")
FRCH_storm6_08_13_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm6_08_13_Q.csv")
#FRCH_storm6_08_13_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm6_08_13_NO3.csv")
FRCH_storm6_08_13_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm6_08_13_fDOM.csv")
FRCH_storm6_08_13_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm6_08_13_SPC.csv")
FRCH_storm6_08_13_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm6_08_13_Turb.csv")

#FRCH_storm7_08_23 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm7_08_23.csv")
FRCH_storm7_08_23_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm7_08_23_Q.csv")
#FRCH_storm7_08_23_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm7_08_23_NO3.csv")
FRCH_storm7_08_23_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm7_08_23_fDOM.csv")
FRCH_storm7_08_23_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm7_08_23_SPC.csv")
FRCH_storm7_08_23_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm7_08_23_Turb.csv")


#FRCH_storm8a_08_26 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm8a_08_26.csv")
FRCH_storm8a_08_26_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8a_08_26_Q.csv")
#FRCH_storm8a_08_26_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm8a_08_26_NO3.csv")
FRCH_storm8a_08_26_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8a_08_26_fDOM.csv")
FRCH_storm8a_08_26_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8a_08_26_SPC.csv")
FRCH_storm8a_08_26_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8a_08_26_Turb.csv")

#FRCH_storm8b_08_27 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm8b_08_27.csv")
FRCH_storm8b_08_28_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8b_08_28_Q.csv")
#FRCH_storm8b_08_27_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm8b_08_27_NO3.csv")
FRCH_storm8b_08_28_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8b_08_28_fDOM.csv")
FRCH_storm8b_08_28_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8b_08_28_SPC.csv")
FRCH_storm8b_08_28_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm8b_08_28_Turb.csv")

#FRCH_storm9_08_29 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm9_08_29.csv")
FRCH_storm9_08_30_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm9_08_30_Q.csv")
#FRCH_storm9_08_29_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm9_08_29_NO3.csv")
FRCH_storm9_08_30_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm9_08_30_fDOM.csv")
FRCH_storm9_08_30_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm9_08_30_SPC.csv")
FRCH_storm9_08_30_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm9_08_30_Turb.csv")

#FRCH_storm10_09_01 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm10_09_01.csv")
FRCH_storm10_09_01_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm10_09_01_Q.csv")
#FRCH_storm10_09_01_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm10_09_01_NO3.csv")
FRCH_storm10_09_01_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm10_09_01_fDOM.csv")
FRCH_storm10_09_01_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm10_09_01_SPC.csv")
FRCH_storm10_09_01_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm10_09_01_Turb.csv")

#FRCH_storm11a_09_22 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm11a_09_22.csv")
FRCH_storm11a_09_22_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11a_09_22_Q.csv")
#FRCH_storm11a_09_22_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm11a_09_22_NO3.csv")
FRCH_storm11a_09_22_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11a_09_22_fDOM.csv")
FRCH_storm11a_09_22_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11a_09_22_SPC.csv")
FRCH_storm11a_09_22_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11a_09_22_Turb.csv")

#FRCH_storm11b_09_24 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FFRCH_storm11b_09_24.csv")
FRCH_storm11b_09_24_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11b_09_24_Q.csv")
#FRCH_storm11b_09_24_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm11b_09_24_NO3.csv")
FRCH_storm11b_09_24_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11b_09_24_fDOM.csv")
FRCH_storm11b_09_24_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11b_09_24_SPC.csv")
FRCH_storm11b_09_24_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm11b_09_24_Turb.csv")

# MOOS #
#MOOS_storm1_06_21 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm1_06_21.csv")
MOOS_storm1_06_21_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm1_06_21_Q.csv")
MOOS_storm1_06_21_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm1_06_21_NO3.csv")
MOOS_storm1_06_21_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm1_06_21_fDOM.csv")
MOOS_storm1_06_21_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm1_06_21_SPC.csv")
MOOS_storm1_06_21_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm1_06_21_Turb.csv")

#MOOS_storm2a_06_29 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm2a_06_29.csv")
MOOS_storm2a_06_30_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2a_06_30_Q.csv")
#MOOS_storm2a_06_30_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2a_06_30_NO3.csv")
MOOS_storm2a_06_30_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2a_06_30_fDOM.csv")
MOOS_storm2a_06_30_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2a_06_30_SPC.csv")
MOOS_storm2a_06_30_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2a_06_30_Turb.csv")

#MOOS_storm2b_07_01 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm2b_07_01.csv")
MOOS_storm2b_07_01_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2b_07_01_Q.csv")
#MOOS_storm2b_07_01_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm2b_07_01_NO3.csv")
MOOS_storm2b_07_01_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2b_07_01_fDOM.csv")
MOOS_storm2b_07_01_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2b_07_01_SPC.csv")
MOOS_storm2b_07_01_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2b_07_01_Turb.csv")


#MOOS_storm2c_07_04 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm2c_07_04.csv")
MOOS_storm2c_07_04_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2c_07_04_Q.csv")
#MOOS_storm2c_07_04_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm2c_07_04_NO3.csv")
MOOS_storm2c_07_04_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2c_07_04_fDOM.csv")
MOOS_storm2c_07_04_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2c_07_04_SPC.csv")
MOOS_storm2c_07_04_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm2c_07_04_Turb.csv")


#MOOS_storm3_07_09 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm3_07_09.csv")
MOOS_storm3_07_09_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm3_07_09_Q.csv")
MOOS_storm3_07_09_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm3_07_09_NO3.csv")
MOOS_storm3_07_09_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm3_07_09_fDOM.csv")
MOOS_storm3_07_09_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm3_07_09_SPC.csv")
MOOS_storm3_07_09_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm3_07_09_Turb.csv")

#MOOS_storm4_07_15 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm4_07_15.csv")
# MOOS_storm4_07_15_Q <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm4_07_15_Q.csv")
# MOOS_storm4_07_15_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm4_07_15_NO3.csv")
# MOOS_storm4_07_15_fDOM <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm4_07_15_fDOM.csv")
# MOOS_storm4_07_15_SPC <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm4_07_15_SPC.csv")
# MOOS_storm4_07_15_turb <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm4_07_15_Turb.csv")

#MOOS_storm5_08_04 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm5_08_04.csv")
MOOS_storm5_08_04_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm5_08_04_Q.csv")
MOOS_storm5_08_04_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm5_08_04_NO3.csv")
MOOS_storm5_08_04_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm5_08_04_fDOM.csv")
MOOS_storm5_08_04_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm5_08_04_SPC.csv")
MOOS_storm5_08_04_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm5_08_04_Turb.csv")

#MOOS_storm6_08_13 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm6_08_13.csv")
MOOS_storm6_08_13_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm6_08_13_Q.csv")
MOOS_storm6_08_13_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm6_08_13_NO3.csv")
MOOS_storm6_08_13_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm6_08_13_fDOM.csv")
MOOS_storm6_08_13_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm6_08_13_SPC.csv")
MOOS_storm6_08_13_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm6_08_13_Turb.csv")

#MOOS_storm7_08_23 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm7_08_23.csv")
MOOS_storm7_08_23_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm7_08_23_Q.csv")
MOOS_storm7_08_23_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm7_08_23_NO3.csv")
MOOS_storm7_08_23_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm7_08_23_fDOM.csv")
MOOS_storm7_08_23_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm7_08_23_SPC.csv")
MOOS_storm7_08_23_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm7_08_23_Turb.csv")

#MOOS_storm8a_08_26 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm8a_08_26.csv")
MOOS_storm8a_08_26_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8a_08_26_Q.csv")
MOOS_storm8a_08_26_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8a_08_26_NO3.csv")
MOOS_storm8a_08_26_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8a_08_26_fDOM.csv")
MOOS_storm8a_08_26_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8a_08_26_SPC.csv")
MOOS_storm8a_08_26_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8a_08_26_Turb.csv")

#MOOS_storm8b_08_27 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm8b_08_27.csv")
MOOS_storm8b_08_28_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8b_08_28_Q.csv")
MOOS_storm8b_08_28_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8b_08_28_NO3.csv")
MOOS_storm8b_08_28_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8b_08_28_fDOM.csv")
MOOS_storm8b_08_28_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8b_08_28_SPC.csv")
MOOS_storm8b_08_28_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm8b_08_28_Turb.csv")

#MOOS_storm9_08_29 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm9_08_29.csv")
MOOS_storm9_08_30_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm9_08_30_Q.csv")
MOOS_storm9_08_30_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm9_08_30_NO3.csv")
MOOS_storm9_08_30_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm9_08_30_fDOM.csv")
MOOS_storm9_08_30_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm9_08_30_SPC.csv")
MOOS_storm9_08_30_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm9_08_30_Turb.csv")

#MOOS_storm10_09_01 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm10_09_01.csv")
MOOS_storm10_09_01_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm10_09_01_Q.csv")
MOOS_storm10_09_01_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm10_09_01_NO3.csv")
MOOS_storm10_09_01_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm10_09_01_fDOM.csv")
MOOS_storm10_09_01_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm10_09_01_SPC.csv")
MOOS_storm10_09_01_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm10_09_01_Turb.csv")

#MOOS_storm11a_09_22 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm11a_09_22.csv")
MOOS_storm11a_09_22_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11a_09_22_Q.csv")
MOOS_storm11a_09_22_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11a_09_22_NO3.csv")
MOOS_storm11a_09_22_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11a_09_22_fDOM.csv")
MOOS_storm11a_09_22_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11a_09_22_SPC.csv")
MOOS_storm11a_09_22_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11a_09_22_Turb.csv")

#MOOS_storm11b_09_23 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm11b_09_23.csv")
MOOS_storm11b_09_24_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11b_09_24_Q.csv")
MOOS_storm11b_09_24_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11b_09_24_NO3.csv")
MOOS_storm11b_09_24_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11b_09_24_fDOM.csv")
MOOS_storm11b_09_24_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11b_09_24_SPC.csv")
MOOS_storm11b_09_24_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm11b_09_24_Turb.csv")

#MOOS_storm12_09_24 <- read_csv("~/Documents/Storms/Storm_Events/2018/MOOS/MOOS_storm12_09_24.csv")
MOOS_storm12_09_24_Q <- read_csv("Storm_Events/2018/MOOS/MOOS_storm12_09_24_Q.csv")
MOOS_storm12_09_24_NO3 <- read_csv("Storm_Events/2018/MOOS/MOOS_storm12_09_24_NO3.csv")
MOOS_storm12_09_24_fDOM <- read_csv("Storm_Events/2018/MOOS/MOOS_storm12_09_24_fDOM.csv")
MOOS_storm12_09_24_SPC <- read_csv("Storm_Events/2018/MOOS/MOOS_storm12_09_24_SPC.csv")
MOOS_storm12_09_24_turb <- read_csv("Storm_Events/2018/MOOS/MOOS_storm12_09_24_Turb.csv")

# CARI #
#CARI_storm1_06_10 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm1_06_10.csv") # EMPTY
#CARI_storm1_06_10_Q <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm1_06_10_Q.csv")
#CARI_storm1_06_10_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm1_06_10_NO3.csv", 
#                                  col_types = cols(datavalue = col_double()))
#CARI_storm1_06_10_fDOM <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm1_06_10_fDOM.csv", 
#col_types = cols(datavalue = col_double()))
#CARI_storm1_06_10_SPC <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm1_06_10_SPC.csv")
#CARI_storm1_06_10_turb <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm1_06_10_Turb.csv")

#CARI_storm2_06_21 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm2_06_21.csv")
CARI_storm2_06_21_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm2_06_21_Q.csv")
CARI_storm2_06_21_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm2_06_21_NO3.csv")
CARI_storm2_06_21_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm2_06_21_fDOM.csv", 
                                   col_types = cols(datavalue = col_double()))
CARI_storm2_06_21_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm2_06_21_SPC.csv")
CARI_storm2_06_21_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm2_06_21_Turb.csv")

#CARI_storm3_06_29 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm2_06_21.csv")
CARI_storm3_06_29_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm3_06_29_Q.csv")
CARI_storm3_06_29_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm3_06_29_NO3.csv")
CARI_storm3_06_29_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm3_06_29_fDOM.csv", 
                                   col_types = cols(datavalue = col_double()))
CARI_storm3_06_29_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm3_06_29_SPC.csv")
CARI_storm3_06_29_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm3_06_29_Turb.csv")

#CARI_storm4a_06_30 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4a_06_30.csv") # EMPTY
#CARI_storm4a_06_30_Q <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4a_06_30_Q.csv")
#CARI_storm4a_06_30_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4a_06_30_NO3.csv", 
#                                   col_types = cols(datavalue = col_double()))
#CARI_storm4a_06_30_fDOM <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4a_06_30_fDOM.csv")
#CARI_storm4a_06_30_SPC <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4a_06_30_SPC.csv")
#CARI_storm4a_06_30_turb <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4a_06_30_Turb.csv")

#CARI_storm4b_07_01 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4b_07_01.csv")
#CARI_storm4b_07_01_Q <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4b_07_01_Q.csv")
#CARI_storm4b_07_01_NO3 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4b_07_01_NO3.csv", 
#                                   col_types = cols(datavalue = col_double()))
#CARI_storm4b_07_01_fDOM <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4b_07_01_fDOM.csv", 
#    col_types = cols(datavalue = col_double()))
#CARI_storm4b_07_01_SPC <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4b_07_01_SPC.csv")
#CARI_storm4b_07_01_turb <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm4b_07_01_Turb.csv")

#CARI_storm5a_08_04 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm5a_08_04.csv")
CARI_storm5a_08_04_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm5a_08_04_Q.csv")
CARI_storm5a_08_04_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm5a_08_04_NO3.csv")
CARI_storm5a_08_04_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm5a_08_04_fDOM.csv", 
                                    col_types = cols(datavalue = col_double()))
CARI_storm5a_08_04_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm5a_08_04_SPC.csv")
CARI_storm5a_08_04_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm5a_08_04_Turb.csv")

#CARI_storm5b_08_05 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm5b_08_05.csv")
CARI_storm5b_08_05_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm5b_08_05_Q.csv")
CARI_storm5b_08_05_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm5b_08_05_NO3.csv")
CARI_storm5b_08_05_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm5b_08_05_fDOM.csv", 
                                    col_types = cols(datavalue = col_double()))
CARI_storm5b_08_05_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm5b_08_05_SPC.csv")
CARI_storm5b_08_05_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm5b_08_05_Turb.csv")

#CARI_storm5c_08_06 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm5c_08_06.csv")
CARI_storm5c_08_06_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm5c_08_06_Q.csv")
CARI_storm5c_08_06_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm5c_08_06_NO3.csv")
CARI_storm5c_08_06_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm5c_08_06_fDOM.csv", 
                                    col_types = cols(datavalue = col_double()))
CARI_storm5c_08_06_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm5c_08_06_SPC.csv")
CARI_storm5c_08_06_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm5c_08_06_Turb.csv")

#CARI_storm6_08_13 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm6_08_13.csv")
CARI_storm6_08_13_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm6_08_13_Q.csv")
CARI_storm6_08_13_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm6_08_13_NO3.csv")
CARI_storm6_08_13_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm6_08_13_fDOM.csv", 
                                   col_types = cols(datavalue = col_double()))
CARI_storm6_08_13_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm6_08_13_SPC.csv")
CARI_storm6_08_13_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm6_08_13_Turb.csv")

#CARI_storm7_08_21 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm7_08_21.csv")
CARI_storm7_08_21_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm7_08_21_Q.csv")
CARI_storm7_08_21_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm7_08_21_NO3.csv")
CARI_storm7_08_21_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm7_08_21_fDOM.csv", 
                                   col_types = cols(datavalue = col_double()))
CARI_storm7_08_21_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm7_08_21_SPC.csv")
CARI_storm7_08_21_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm7_08_21_Turb.csv")

#CARI_storm8_08_24 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm8_08_24.csv")
CARI_storm8_08_24_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm8_08_24_Q.csv")
CARI_storm8_08_24_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm8_08_24_NO3.csv")
CARI_storm8_08_24_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm8_08_24_fDOM.csv", 
                                   col_types = cols(datavalue = col_double()))
CARI_storm8_08_24_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm8_08_24_SPC.csv")
CARI_storm8_08_24_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm8_08_24_Turb.csv")

#CARI_storm9_08_26 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm9_08_26.csv")
CARI_storm9_08_26_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm9_08_26_Q.csv")
CARI_storm9_08_26_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm9_08_26_NO3.csv")
CARI_storm9_08_26_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm9_08_26_fDOM.csv", 
                                   col_types = cols(datavalue = col_double()))
CARI_storm9_08_26_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm9_08_26_SPC.csv")
CARI_storm9_08_26_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm9_08_26_Turb.csv")

#CARI_storm10_08_30 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm10_08_30.csv")
CARI_storm10_08_30_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm10_08_30_Q.csv")
CARI_storm10_08_30_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm10_08_30_NO3.csv")
CARI_storm10_08_30_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm10_08_30_fDOM.csv", 
                                    col_types = cols(datavalue = col_double()))
CARI_storm10_08_30_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm10_08_30_SPC.csv")
CARI_storm10_08_30_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm10_08_30_Turb.csv")

#CARI_storm11_09_02 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm11_09_02.csv")
CARI_storm11_09_02_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm11_09_02_Q.csv")
CARI_storm11_09_02_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm11_09_02_NO3.csv")
CARI_storm11_09_02_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm11_09_02_fDOM.csv", 
                                    col_types = cols(datavalue = col_double()))
CARI_storm11_09_02_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm11_09_02_SPC.csv")
CARI_storm11_09_02_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm11_09_02_Turb.csv")

#CARI_storm12a_09_20 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm12a_09_20.csv")
CARI_storm12a_09_20_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm12a_09_20_Q.csv")
CARI_storm12a_09_20_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm12a_09_20_NO3.csv")
CARI_storm12a_09_20_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm12a_09_20_fDOM.csv", 
                                     col_types = cols(datavalue = col_double()))
CARI_storm12a_09_20_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm12a_09_20_SPC.csv")
CARI_storm12a_09_20_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm12a_09_20_Turb.csv")

#CARI_storm12b_09_25 <- read_csv("~/Documents/Storms/Storm_Events/2018/CARI/CARI_storm12b_09_25.csv")
CARI_storm12b_09_25_Q <- read_csv("Storm_Events/2018/CARI/CARI_storm12b_09_25_Q.csv")
CARI_storm12b_09_25_NO3 <- read_csv("Storm_Events/2018/CARI/CARI_storm12b_09_25_NO3.csv")
CARI_storm12b_09_25_fDOM <- read_csv("Storm_Events/2018/CARI/CARI_storm12b_09_25_fDOM.csv", 
                                     col_types = cols(datavalue = col_double()))
CARI_storm12b_09_25_SPC <- read_csv("Storm_Events/2018/CARI/CARI_storm12b_09_25_SPC.csv")
CARI_storm12b_09_25_turb <- read_csv("Storm_Events/2018/CARI/CARI_storm12b_09_25_Turb.csv")


# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

# FRCH #
#fxn: plot hysteresis loop #
hyst_plot = function(dat_Q, dat_response, site, response_var, storm_num) {
  dat.p = ggplot(data = dat_Q, 
                 aes(x=(dat_Q$datavalue), 
                     y=(dat_response$datavalue), 
                     color = as.numeric(dat_Q$valuedatetime))) +
    geom_point() +
    scale_colour_gradientn(colors = rainbow(3)) +
    theme_bw() +
    theme(legend.position="none") + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    ylab(paste(site, response_var))+
    xlab("Normalized Discharge") +
    ggtitle(paste("Storm", storm_num))
  return(dat.p)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# plot FRCH loops #
FRCH_storm1_06_21_NO3.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_NO3, "FRCH", "NO3", "0621")
FRCH_storm2a_06_29_NO3.p = hyst_plot(FRCH_storm2a_06_29_Q, FRCH_storm2a_06_29_NO3, "FRCH", "NO3", "0629")
FRCH_storm2b_07_04_NO3.p = hyst_plot(FRCH_storm2b_07_04_Q, FRCH_storm2b_07_04_NO3, "FRCH", "NO3", "0704")
FRCH_storm3_07_10_NO3.p = hyst_plot(FRCH_storm3_07_10_Q, FRCH_storm3_07_10_NO3, "FRCH", "NO3", "0710")
FRCH_storm4a_07_15_NO3.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_NO3, "FRCH", "NO3", "0715a")
FRCH_storm4b_07_16_NO3.p = hyst_plot(FRCH_storm4b_07_16_Q, FRCH_storm4b_07_16_NO3, "FRCH", "NO3", "0716b")
FRCH_storm5_08_04_NO3.p = hyst_plot(FRCH_storm5_08_04_Q, FRCH_storm5_08_04_NO3, "FRCH", "NO3", "0804")
#FRCH_storm6_08_13_NO3.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_NO3, "FRCH", "NO3", "0813")
#FRCH_storm7_08_23_NO3.p = hyst_plot(FRCH_storm7_08_23_Q, FRCH_storm7_08_23_NO3, "FRCH", "NO3", "0823")
#FRCH_storm8a_08_26_NO3.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_NO3, "FRCH", "NO3", "0826a")
#FRCH_storm8b_08_27_NO3.p = hyst_plot(FRCH_storm8b_08_27_Q, FRCH_storm8b_08_27_NO3, "FRCH", "NO3", "0827b")
#FRCH_storm9_08_29_NO3.p = hyst_plot(FRCH_storm9_08_29_Q, FRCH_storm9_08_29_NO3, "FRCH", "NO3", "0829")
#FRCH_storm10_09_01_NO3.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_NO3, "FRCH", "NO3", "0901")
#FRCH_storm11a_09_22_NO3.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_NO3, "FRCH", "NO3", "0922a")
#FRCH_storm11b_09_24_NO3.p = hyst_plot(FRCH_storm11b_09_24_Q, FRCH_storm11b_09_24_NO3, "FRCH", "NO3", "0924b")

# NO3
multiplot(FRCH_storm1_06_21_NO3.p)
multiplot(FRCH_storm2a_06_29_NO3.p)
multiplot(FRCH_storm2b_07_04_NO3.p)
multiplot(FRCH_storm3_07_10_NO3.p)
multiplot(FRCH_storm4a_07_15_NO3.p)
multiplot(FRCH_storm4b_07_16_NO3.p)
multiplot(FRCH_storm5_08_04_NO3.p)

# fDOM #
FRCH_storm1_06_21_fDOM.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_fDOM, "FRCH", "fDOM", "0621")
FRCH_storm2a_06_29_fDOM.p = hyst_plot(FRCH_storm2a_06_29_Q, FRCH_storm2a_06_29_fDOM, "FRCH", "fDOM", "0629")
FRCH_storm2b_07_04_fDOM.p = hyst_plot(FRCH_storm2b_07_04_Q, FRCH_storm2b_07_04_fDOM, "FRCH", "fDOM", "0704")
FRCH_storm3_07_10_fDOM.p = hyst_plot(FRCH_storm3_07_10_Q, FRCH_storm3_07_10_fDOM, "FRCH", "fDOM", "0710")
FRCH_storm4a_07_15_fDOM.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_fDOM, "FRCH", "fDOM", "0715a")
FRCH_storm4b_07_16_fDOM.p = hyst_plot(FRCH_storm4b_07_16_Q, FRCH_storm4b_07_16_fDOM, "FRCH", "fDOM", "0716b")
FRCH_storm5_08_04_fDOM.p = hyst_plot(FRCH_storm5_08_04_Q, FRCH_storm5_08_04_fDOM, "FRCH", "fDOM", "0804")
FRCH_storm6_08_13_fDOM.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_fDOM, "FRCH", "fDOM", "0813")
FRCH_storm7_08_23_fDOM.p = hyst_plot(FRCH_storm7_08_23_Q, FRCH_storm7_08_23_fDOM, "FRCH", "fDOM", "0823")
FRCH_storm8a_08_26_fDOM.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_fDOM, "FRCH", "fDOM", "0826a")
FRCH_storm8b_08_28_fDOM.p = hyst_plot(FRCH_storm8b_08_28_Q, FRCH_storm8b_08_28_fDOM, "FRCH", "fDOM", "0828b")
FRCH_storm9_08_30_fDOM.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_fDOM, "FRCH", "fDOM", "0830")
FRCH_storm10_09_01_fDOM.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_fDOM, "FRCH", "fDOM", "0901a")
FRCH_storm11a_09_22_fDOM.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_fDOM, "FRCH", "fDOM", "0922a")
FRCH_storm11b_09_24_fDOM.p = hyst_plot(FRCH_storm11b_09_24_Q, FRCH_storm11b_09_24_fDOM, "FRCH", "fDOM", "0924b")

multiplot(FRCH_storm1_06_21_fDOM.p)
multiplot(FRCH_storm2a_06_29_fDOM.p)
multiplot(FRCH_storm2b_07_04_fDOM.p)
multiplot(FRCH_storm3_07_10_fDOM.p)
multiplot(FRCH_storm4a_07_15_fDOM.p)
multiplot(FRCH_storm4b_07_16_fDOM.p)
multiplot(FRCH_storm5_08_04_fDOM.p)
multiplot(FRCH_storm6_08_13_fDOM.p)
multiplot(FRCH_storm7_08_23_fDOM.p)
multiplot(FRCH_storm8a_08_26_fDOM.p)
multiplot(FRCH_storm8b_08_28_fDOM.p)
multiplot(FRCH_storm9_08_30_fDOM.p)
multiplot(FRCH_storm10_09_01_fDOM.p)
multiplot(FRCH_storm11a_09_22_fDOM.p)
multiplot(FRCH_storm11b_09_24_fDOM.p)


# SPC #
FRCH_storm1_06_21_SPC.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_SPC, "FRCH", "SPC", "0621")
FRCH_storm2a_06_29_SPC.p = hyst_plot(FRCH_storm2a_06_29_Q, FRCH_storm2a_06_29_SPC, "FRCH", "SPC", "0629")
FRCH_storm2b_07_04_SPC.p = hyst_plot(FRCH_storm2b_07_04_Q, FRCH_storm2b_07_04_SPC, "FRCH", "SPC", "0704")
FRCH_storm3_07_10_SPC.p = hyst_plot(FRCH_storm3_07_10_Q, FRCH_storm3_07_10_SPC, "FRCH", "SPC", "0710")
FRCH_storm4a_07_15_SPC.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_SPC, "FRCH", "SPC", "0715a")
FRCH_storm4b_07_16_SPC.p = hyst_plot(FRCH_storm4b_07_16_Q, FRCH_storm4b_07_16_SPC, "FRCH", "SPC", "0716b")
FRCH_storm5_08_04_SPC.p = hyst_plot(FRCH_storm5_08_04_Q, FRCH_storm5_08_04_SPC, "FRCH", "SPC", "0804")
FRCH_storm6_08_13_SPC.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_SPC, "FRCH", "SPC", "0813")
FRCH_storm7_08_23_SPC.p = hyst_plot(FRCH_storm7_08_23_Q, FRCH_storm7_08_23_SPC, "FRCH", "SPC", "0823")
FRCH_storm8a_08_26_SPC.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_SPC, "FRCH", "SPC", "0826a")
FRCH_storm8b_08_28_SPC.p = hyst_plot(FRCH_storm8b_08_28_Q, FRCH_storm8b_08_28_SPC, "FRCH", "SPC", "0828b")
FRCH_storm9_08_30_SPC.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_SPC, "FRCH", "SPC", "0830")
FRCH_storm10_09_01_SPC.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_SPC, "FRCH", "SPC", "0901a")
FRCH_storm11a_09_22_SPC.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_SPC, "FRCH", "SPC", "0922a")
FRCH_storm11b_09_24_SPC.p = hyst_plot(FRCH_storm11b_09_24_Q, FRCH_storm11b_09_24_SPC, "FRCH", "SPC", "0924b")

multiplot(FRCH_storm1_06_21_SPC.p)
multiplot(FRCH_storm2a_06_29_SPC.p)
multiplot(FRCH_storm2b_07_04_SPC.p)
multiplot(FRCH_storm3_07_10_SPC.p)
multiplot(FRCH_storm4a_07_15_SPC.p)
multiplot(FRCH_storm4b_07_16_SPC.p)
multiplot(FRCH_storm5_08_04_SPC.p)
multiplot(FRCH_storm6_08_13_SPC.p)
multiplot(FRCH_storm7_08_23_SPC.p)
multiplot(FRCH_storm8a_08_26_SPC.p)
multiplot(FRCH_storm8b_08_28_SPC.p)
multiplot(FRCH_storm9_08_30_SPC.p)
multiplot(FRCH_storm10_09_01_SPC.p)
multiplot(FRCH_storm11a_09_22_SPC.p)
multiplot(FRCH_storm11b_09_24_SPC.p)


# Turb
FRCH_storm1_06_21_turb.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_turb, "FRCH", "turb", "0621")
FRCH_storm2a_06_29_turb.p = hyst_plot(FRCH_storm2a_06_29_Q, FRCH_storm2a_06_29_turb, "FRCH", "turb", "0629")
FRCH_storm2b_07_04_turb.p = hyst_plot(FRCH_storm2b_07_04_Q, FRCH_storm2b_07_04_turb, "FRCH", "turb", "0704")
FRCH_storm3_07_10_turb.p = hyst_plot(FRCH_storm3_07_10_Q, FRCH_storm3_07_10_turb, "FRCH", "turb", "0710")
FRCH_storm4a_07_15_turb.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_turb, "FRCH", "turb", "0715a")
FRCH_storm4b_07_16_turb.p = hyst_plot(FRCH_storm4b_07_16_Q, FRCH_storm4b_07_16_turb, "FRCH", "turb", "0716b")
FRCH_storm5_08_04_turb.p = hyst_plot(FRCH_storm5_08_04_Q, FRCH_storm5_08_04_turb, "FRCH", "turb", "0804")
FRCH_storm6_08_13_turb.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_turb, "FRCH", "turb", "0813")
FRCH_storm7_08_23_turb.p = hyst_plot(FRCH_storm7_08_23_Q, FRCH_storm7_08_23_turb, "FRCH", "turb", "0823")
FRCH_storm8a_08_26_turb.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_fDOM, "FRCH", "turb", "0826a")
FRCH_storm8b_08_28_turb.p = hyst_plot(FRCH_storm8b_08_28_Q, FRCH_storm8b_08_28_turb, "FRCH", "turb", "0828b")
FRCH_storm9_08_30_turb.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_turb, "FRCH", "turb", "0830")
FRCH_storm10_09_01_turb.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_turh, "FRCH", "turb", "0901a")
FRCH_storm11a_09_22_turb.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_turb, "FRCH", "turb", "0922a")
FRCH_storm11b_09_24_turb.p = hyst_plot(FRCH_storm11b_09_24_Q, FRCH_storm11b_09_24_turb, "FRCH", "turb", "0924b")

multiplot(FRCH_storm1_06_21_turb.p)
multiplot(FRCH_storm2a_06_29_turb.p)
multiplot(FRCH_storm2b_07_04_turb.p)
multiplot(FRCH_storm3_07_10_turb.p)
multiplot(FRCH_storm4a_07_15_turb.p)
multiplot(FRCH_storm4b_07_16_turb.p)
multiplot(FRCH_storm5_08_04_turb.p)
multiplot(FRCH_storm6_08_13_turb.p)
multiplot(FRCH_storm7_08_23_turb.p)
multiplot(FRCH_storm8a_08_26_turb.p)
multiplot(FRCH_storm8b_08_28_turb.p)
multiplot(FRCH_storm9_08_30_turb.p)
#multiplot(FRCH_storm10_09_01_turb.p)
multiplot(FRCH_storm11a_09_22_turb.p)
multiplot(FRCH_storm11b_09_24_turb.p)


# Multiplots of FRCH storms #

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

FRCH_HI_Loops <- multiplot(FRCH_storm1_06_21_NO3.p, FRCH_storm1_06_21_fDOM.p, FRCH_storm1_06_21_SPC.p, FRCH_storm1_06_21_turb.p,
          FRCH_storm2a_06_29_NO3.p, FRCH_storm2a_06_29_fDOM.p, FRCH_storm2a_06_29_SPC.p, FRCH_storm2a_06_29_turb.p,
          FRCH_storm2b_07_04_NO3.p, FRCH_storm2b_07_04_fDOM.p, FRCH_storm2b_07_04_SPC.p, FRCH_storm2b_07_04_turb.p,
          FRCH_storm3_07_10_NO3.p, FRCH_storm3_07_10_fDOM.p, FRCH_storm3_07_10_SPC.p, FRCH_storm3_07_10_turb.p,
          FRCH_storm4a_07_15_NO3.p, FRCH_storm4a_07_15_fDOM.p, FRCH_storm4a_07_15_SPC.p, FRCH_storm4a_07_15_turb.p,
          FRCH_storm4b_07_16_NO3.p, FRCH_storm4b_07_16_fDOM.p, FRCH_storm4b_07_16_SPC.p, FRCH_storm4b_07_16_turb.p,
          FRCH_storm5_08_04_NO3.p, FRCH_storm5_08_04_fDOM.p, FRCH_storm5_08_04_SPC.p, FRCH_storm5_08_04_turb.p,
          FRCH_storm6_08_13_fDOM.p, FRCH_storm6_08_13_SPC.p, FRCH_storm6_08_13_turb.p,
          FRCH_storm7_08_23_fDOM.p, FRCH_storm7_08_23_SPC.p, FRCH_storm7_08_23_turb.p,
          FRCH_storm8a_08_26_fDOM.p, FRCH_storm8a_08_26_SPC.p, FRCH_storm8a_08_26_turb.p,
          FRCH_storm8b_08_28_fDOM.p, FRCH_storm8b_08_28_SPC.p, FRCH_storm8b_08_28_turb.p,
          FRCH_storm9_08_30_fDOM.p, FRCH_storm9_08_30_SPC.p, FRCH_storm9_08_30_turb.p,
          FRCH_storm10_09_01_fDOM.p, FRCH_storm10_09_01_SPC.p,
          FRCH_storm11a_09_22_fDOM.p, FRCH_storm11a_09_22_SPC.p, FRCH_storm11a_09_22_turb.p,
          FRCH_storm11b_09_24_fDOM.p, FRCH_storm11b_09_24_SPC.p, FRCH_storm11b_09_24_turb.p,
          cols = 7)

# set working directory to save it to 
setwd("~/Documents/Storms_clean_repo")
dir.create(file.path("plots"))
dir.create(file.path("plots", "HI_plots"))
dir.create(file.path("plots", "HI_plots", "2018"))
dir.create(file.path("plots", "HI_plots", "2018", "FRCH"))
dir.create(file.path("plots", "HI_plots", "2018", "MOOS"))
dir.create(file.path("plots", "HI_plots", "2018", "CARI"))

# export pdf 20 x 30
setwd("~/Documents/Storms_clean_repo/plots/HI_plots/2018")
# ggsave(FRCH_HI_Loops, 
#        file = "FRCH_HI_Loops_2018.pdf", 
#        path = "FRCH/", 
#        width = 20, height = 30, units = "in")

# MOOS #
# fxn: plot hysteresis loop #
hyst_plot = function(dat_Q, dat_response, site, response_var, storm_num) {
  dat.p = ggplot(data = dat_Q, 
                 aes(x=(dat_Q$datavalue), 
                     y=(dat_response$datavalue), 
                     color = as.numeric(dat_Q$valuedatetime))) +
    geom_point() +
    scale_colour_gradientn(colors = rainbow(3)) +
    theme_bw() +
    theme(legend.position="none") + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    ylab(paste(site, response_var))+
    xlab("Normalized Discharge") +
    ggtitle(paste("Storm", storm_num))
  return(dat.p)
}

# plot MOOS loops #
# NO3


MOOS_storm1_06_21_NO3.p = hyst_plot(MOOS_storm1_06_21_Q, MOOS_storm1_06_21_NO3, "MOOS", "NO3", "0621")
#MOOS_storm2a_06_29_NO3.p = hyst_plot(MOOS_storm2a_06_29_Q, MOOS_storm2a_06_29_NO3, "MOOS", "NO3", "0629a")
#MOOS_storm2b_07_01_NO3.p = hyst_plot(MOOS_storm2b_07_01_Q, MOOS_storm2b_07_01_NO3, "MOOS", "NO3", "0701b")
#MOOS_storm2c_07_04_NO3.p = hyst_plot(MOOS_storm2c_07_04_Q, MOOS_storm2c_07_04_NO3, "MOOS", "NO3", "0704c")
MOOS_storm3_07_09_NO3.p = hyst_plot(MOOS_storm3_07_09_Q, MOOS_storm3_07_09_NO3, "MOOS", "NO3", "0709")
#MOOS_storm4_07_15_NO3.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_NO3, "MOOS", "NO3", "0715")
MOOS_storm5_08_04_NO3.p = hyst_plot(MOOS_storm5_08_04_Q, MOOS_storm5_08_04_NO3, "MOOS", "NO3", "0804")
MOOS_storm6_08_13_NO3.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_NO3, "MOOS", "NO3", "0813")
MOOS_storm7_08_23_NO3.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_NO3, "MOOS", "NO3", "0823")
MOOS_storm8a_08_26_NO3.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_NO3, "MOOS", "NO3", "0826")
MOOS_storm8b_08_28_NO3.p = hyst_plot(MOOS_storm8b_08_28_Q, MOOS_storm8b_08_28_NO3, "MOOS", "NO3", "0828")
MOOS_storm9_08_30_NO3.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_NO3, "MOOS", "NO3", "0830")
MOOS_storm10_09_01_NO3.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_NO3, "MOOS", "NO3", "0901")
MOOS_storm11a_09_22_NO3.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_NO3, "MOOS", "NO3", "0922")
MOOS_storm11b_09_24_NO3.p = hyst_plot(MOOS_storm11b_09_24_Q, MOOS_storm11b_09_24_NO3, "MOOS", "NO3", "0924")
MOOS_storm12_09_24_NO3.p = hyst_plot(MOOS_storm12_09_24_Q, MOOS_storm12_09_24_NO3, "MOOS", "NO3", "0924")


multiplot(MOOS_storm1_06_21_NO3.p)
multiplot(MOOS_storm3_07_09_NO3.p)
multiplot(MOOS_storm5_08_04_NO3.p)
multiplot(MOOS_storm6_08_13_NO3.p)
multiplot(MOOS_storm7_08_23_NO3.p)
multiplot(MOOS_storm8a_08_26_NO3.p)
multiplot(MOOS_storm8b_08_28_NO3.p)
multiplot(MOOS_storm9_08_30_NO3.p)
multiplot(MOOS_storm10_09_01_NO3.p)
multiplot(MOOS_storm11a_09_22_NO3.p)
multiplot(MOOS_storm11b_09_24_NO3.p)
multiplot(MOOS_storm12_09_24_NO3.p)


# fDOM #
MOOS_storm1_06_21_fDOM.p = hyst_plot(MOOS_storm1_06_21_Q, MOOS_storm1_06_21_fDOM, "MOOS", "fDOM", "0621")
MOOS_storm2a_06_30_fDOM.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_fDOM, "MOOS", "fDOM", "0630a")
MOOS_storm2b_07_01_fDOM.p = hyst_plot(MOOS_storm2b_07_01_Q, MOOS_storm2b_07_01_fDOM, "MOOS", "fDOM", "0701b")
MOOS_storm2c_07_04_fDOM.p = hyst_plot(MOOS_storm2c_07_04_Q, MOOS_storm2c_07_04_fDOM, "MOOS", "fDOM", "0704c")
MOOS_storm3_07_09_fDOM.p = hyst_plot(MOOS_storm3_07_09_Q, MOOS_storm3_07_09_fDOM, "MOOS", "fDOM", "0709")
#MOOS_storm4_07_15_fDOM.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_fDOM, "MOOS", "fDOM", "0715")
MOOS_storm5_08_04_fDOM.p = hyst_plot(MOOS_storm5_08_04_Q, MOOS_storm5_08_04_fDOM, "MOOS", "fDOM", "0804")
MOOS_storm6_08_13_fDOM.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_fDOM, "MOOS", "fDOM", "0813")
MOOS_storm7_08_23_fDOM.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_fDOM, "MOOS", "fDOM", "0823")
MOOS_storm8a_08_26_fDOM.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_fDOM, "MOOS", "fDOM", "0826a")
MOOS_storm8b_08_28_fDOM.p = hyst_plot(MOOS_storm8b_08_28_Q, MOOS_storm8b_08_28_fDOM, "MOOS", "fDOM", "0828b")
MOOS_storm9_08_30_fDOM.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_fDOM, "MOOS", "fDOM", "0830")
MOOS_storm10_09_01_fDOM.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_fDOM, "MOOS", "fDOM", "0901")
MOOS_storm11a_09_22_fDOM.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_fDOM, "MOOS", "fDOM", "0922")
MOOS_storm11b_09_24_fDOM.p = hyst_plot(MOOS_storm11b_09_24_Q, MOOS_storm11b_09_24_fDOM, "MOOS", "fDOM", "0924")
MOOS_storm12_09_24_fDOM.p = hyst_plot(MOOS_storm12_09_24_Q, MOOS_storm12_09_24_fDOM, "MOOS", "fDOM", "0924")

multiplot(MOOS_storm1_06_21_fDOM.p)
multiplot(MOOS_storm2a_06_30_fDOM.p)
multiplot(MOOS_storm2b_07_01_fDOM.p)
multiplot(MOOS_storm2c_07_04_fDOM.p)
multiplot(MOOS_storm3_07_09_fDOM.p)
multiplot(MOOS_storm5_08_04_fDOM.p)
multiplot(MOOS_storm6_08_13_fDOM.p)
multiplot(MOOS_storm7_08_23_fDOM.p)
multiplot(MOOS_storm8a_08_26_fDOM.p)
multiplot(MOOS_storm8b_08_28_fDOM.p)
multiplot(MOOS_storm9_08_30_fDOM.p)
multiplot(MOOS_storm10_09_01_fDOM.p)
multiplot(MOOS_storm11a_09_22_fDOM.p)
multiplot(MOOS_storm11b_09_24_fDOM.p)
multiplot(MOOS_storm12_09_24_fDOM.p)


# SPC #

MOOS_storm1_06_21_SPC.p = hyst_plot(MOOS_storm1_06_21_Q, MOOS_storm1_06_21_SPC, "MOOS", "SPC", "0621")
MOOS_storm2a_06_30_SPC.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_SPC, "MOOS", "SPC", "0630a")
MOOS_storm2b_07_01_SPC.p = hyst_plot(MOOS_storm2b_07_01_Q, MOOS_storm2b_07_01_SPC, "MOOS", "SPC", "0701b")
MOOS_storm2c_07_04_SPC.p = hyst_plot(MOOS_storm2c_07_04_Q, MOOS_storm2c_07_04_SPC, "MOOS", "SPC", "0704c")
MOOS_storm3_07_09_SPC.p = hyst_plot(MOOS_storm3_07_09_Q, MOOS_storm3_07_09_SPC, "MOOS", "SPC", "0709")
#MOOS_storm4_07_15_SPC.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_SPC, "MOOS", "SPC", "0715")
MOOS_storm5_08_04_SPC.p = hyst_plot(MOOS_storm5_08_04_Q, MOOS_storm5_08_04_SPC, "MOOS", "SPC", "0804")
MOOS_storm6_08_13_SPC.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_SPC, "MOOS", "SPC", "0813")
MOOS_storm7_08_23_SPC.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_SPC, "MOOS", "SPC", "0823")
MOOS_storm8a_08_26_SPC.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_SPC, "MOOS", "SPC", "0826")
MOOS_storm8b_08_28_SPC.p = hyst_plot(MOOS_storm8b_08_28_Q, MOOS_storm8b_08_28_SPC, "MOOS", "SPC", "0828")
MOOS_storm9_08_30_SPC.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_SPC, "MOOS", "SPC", "0830")
MOOS_storm10_09_01_SPC.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_SPC, "MOOS", "SPC", "0901")
MOOS_storm11a_09_22_SPC.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_SPC, "MOOS", "SPC", "0922")
MOOS_storm11b_09_24_SPC.p = hyst_plot(MOOS_storm11b_09_24_Q, MOOS_storm11b_09_24_SPC, "MOOS", "SPC", "0924")
MOOS_storm12_09_24_SPC.p = hyst_plot(MOOS_storm12_09_24_Q, MOOS_storm12_09_24_SPC, "MOOS", "SPC", "0924")

multiplot(MOOS_storm1_06_21_SPC.p)
multiplot(MOOS_storm2a_06_30_SPC.p)
multiplot(MOOS_storm2b_07_01_SPC.p)
multiplot(MOOS_storm2c_07_04_SPC.p)
multiplot(MOOS_storm3_07_09_SPC.p)
multiplot(MOOS_storm5_08_04_SPC.p)
multiplot(MOOS_storm6_08_13_SPC.p)
multiplot(MOOS_storm7_08_23_SPC.p)
multiplot(MOOS_storm8a_08_26_SPC.p)
multiplot(MOOS_storm8b_08_28_SPC.p)
multiplot(MOOS_storm9_08_30_SPC.p)
multiplot(MOOS_storm10_09_01_SPC.p)
multiplot(MOOS_storm11a_09_22_SPC.p)
multiplot(MOOS_storm11b_09_24_SPC.p)
multiplot(MOOS_storm12_09_24_SPC.p)



# turb
MOOS_storm1_06_21_turb.p = hyst_plot(MOOS_storm1_06_21_Q, MOOS_storm1_06_21_turb, "MOOS", "turb", "0621")
MOOS_storm2a_06_30_turb.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_turb, "MOOS", "turb", "0630a")
MOOS_storm2b_07_01_turb.p = hyst_plot(MOOS_storm2b_07_01_Q, MOOS_storm2b_07_01_turb, "MOOS", "turb", "0701b")
MOOS_storm2c_07_04_turb.p = hyst_plot(MOOS_storm2c_07_04_Q, MOOS_storm2c_07_04_turb, "MOOS", "turb", "0704c")
MOOS_storm3_07_09_turb.p = hyst_plot(MOOS_storm3_07_09_Q, MOOS_storm3_07_09_turb, "MOOS", "turb", "0709")
#MOOS_storm4_07_15_turb.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_turb, "MOOS", "turb", "0715")
MOOS_storm5_08_04_turb.p = hyst_plot(MOOS_storm5_08_04_Q, MOOS_storm5_08_04_turb, "MOOS", "turb", "0804")
MOOS_storm6_08_13_turb.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_turb, "MOOS", "turb", "0813")
MOOS_storm7_08_23_turb.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_turb, "MOOS", "turb", "0823")
MOOS_storm8a_08_26_turb.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_turb, "MOOS", "turb", "0826")
MOOS_storm8b_08_28_turb.p = hyst_plot(MOOS_storm8b_08_28_Q, MOOS_storm8b_08_28_turb, "MOOS", "turb", "0828")
MOOS_storm9_08_30_turb.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_turb, "MOOS", "turb", "0830")
MOOS_storm10_09_01_turb.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_turb, "MOOS", "turb", "0901")
MOOS_storm11a_09_22_turb.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_turb, "MOOS", "turb", "0922")
MOOS_storm11b_09_24_turb.p = hyst_plot(MOOS_storm11b_09_24_Q, MOOS_storm11b_09_24_turb, "MOOS", "turb", "0924")
MOOS_storm12_09_24_turb.p = hyst_plot(MOOS_storm12_09_24_Q, MOOS_storm12_09_24_turb, "MOOS", "turb", "0924")


multiplot(MOOS_storm1_06_21_turb.p)
multiplot(MOOS_storm2a_06_30_turb.p)
multiplot(MOOS_storm2b_07_01_turb.p)
multiplot(MOOS_storm2c_07_04_turb.p)
multiplot(MOOS_storm3_07_09_turb.p)
multiplot(MOOS_storm5_08_04_turb.p)
multiplot(MOOS_storm6_08_13_turb.p)
multiplot(MOOS_storm7_08_23_turb.p)
multiplot(MOOS_storm8a_08_26_turb.p)
multiplot(MOOS_storm8b_08_28_turb.p)
multiplot(MOOS_storm9_08_30_turb.p)
multiplot(MOOS_storm10_09_01_turb.p)
multiplot(MOOS_storm11a_09_22_turb.p)
multiplot(MOOS_storm11b_09_24_turb.p)
multiplot(MOOS_storm12_09_24_turb.p)

# Multiplots of MOOS storms #

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(MOOS_storm1_06_21_NO3.p, MOOS_storm1_06_21_fDOM.p, MOOS_storm1_06_21_SPC.p, MOOS_storm1_06_21_turb.p,
          MOOS_storm2a_06_30_fDOM.p, MOOS_storm2a_06_30_SPC.p, MOOS_storm2a_06_30_turb.p,
          MOOS_storm2b_07_01_fDOM.p, MOOS_storm2b_07_01_SPC.p, MOOS_storm2b_07_01_turb.p,
          MOOS_storm2c_07_04_fDOM.p, MOOS_storm2c_07_04_SPC.p, MOOS_storm2c_07_04_turb.p,
          MOOS_storm3_07_09_NO3.p, MOOS_storm3_07_09_fDOM.p, MOOS_storm3_07_09_SPC.p, MOOS_storm3_07_09_turb.p,
          
          MOOS_storm5_08_04_NO3.p, MOOS_storm5_08_04_fDOM.p, MOOS_storm5_08_04_SPC.p, MOOS_storm5_08_04_turb.p,
          MOOS_storm6_08_13_NO3.p, MOOS_storm6_08_13_fDOM.p, MOOS_storm6_08_13_SPC.p, MOOS_storm6_08_13_turb.p,
          MOOS_storm7_08_23_NO3.p, MOOS_storm7_08_23_fDOM.p, MOOS_storm7_08_23_SPC.p, MOOS_storm7_08_23_turb.p,
          MOOS_storm8a_08_26_NO3.p, MOOS_storm8a_08_26_fDOM.p, MOOS_storm8a_08_26_SPC.p, MOOS_storm8a_08_26_turb.p,
          MOOS_storm8b_08_28_NO3.p, MOOS_storm8b_08_28_fDOM.p, MOOS_storm8b_08_28_SPC.p, MOOS_storm8b_08_28_turb.p,
          MOOS_storm9_08_30_NO3.p, MOOS_storm9_08_30_fDOM.p, MOOS_storm9_08_30_SPC.p, MOOS_storm9_08_30_turb.p,
          MOOS_storm10_09_01_NO3.p, MOOS_storm10_09_01_fDOM.p, MOOS_storm10_09_01_SPC.p, MOOS_storm10_09_01_turb.p,
          MOOS_storm11a_09_22_NO3.p, MOOS_storm11a_09_22_fDOM.p, MOOS_storm11a_09_22_SPC.p, MOOS_storm11a_09_22_turb.p,
          MOOS_storm11b_09_24_NO3.p, MOOS_storm11b_09_24_fDOM.p, MOOS_storm11b_09_24_SPC.p, MOOS_storm11b_09_24_turb.p,
          MOOS_storm12_09_24_NO3.p, MOOS_storm12_09_24_fDOM.p, MOOS_storm12_09_24_SPC.p, MOOS_storm12_09_24_turb.p,
          cols = 7
)

# export pdf 20 x 30
setwd("~/Documents/Storms_clean_repo/plots/HI_plots/2018")
# ggsave(MOOS_HI_Loops, 
#        file = "MOOS_HI_Loops_2018.pdf", 
#        path = "MOOS/", 
#        width = 20, height = 30, units = "in")

### CARI ###
#NO3
#CARI_storm1_06_10_NO3.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_NO3, "CARI", "NO3", "0610")
CARI_storm2_06_21_NO3.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_NO3, "CARI", "NO3", "0621")
CARI_storm3_06_29_NO3.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_NO3, "CARI", "NO3", "0629")
#CARI_storm4a_06_30_NO3.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_NO3, "CARI", "NO3", "0630a")
#CARI_storm4b_07_01_NO3.p = hyst_plot(CARI_storm4b_07_01_Q, CARI_storm4b_07_01_NO3, "CARI", "NO3", "0701b")
CARI_storm5a_08_04_NO3.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_NO3, "CARI", "NO3", "0804a")
CARI_storm5b_08_05_NO3.p = hyst_plot(CARI_storm5b_08_05_Q, CARI_storm5b_08_05_NO3, "CARI", "NO3", "0805b")
CARI_storm5c_08_06_NO3.p = hyst_plot(CARI_storm5c_08_06_Q, CARI_storm5c_08_06_NO3, "CARI", "NO3", "0806c")
CARI_storm6_08_13_NO3.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_NO3, "CARI", "NO3", "0813")
CARI_storm7_08_21_NO3.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_NO3, "CARI", "NO3", "0821")
CARI_storm8_08_24_NO3.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_NO3, "CARI", "NO3", "0824")
CARI_storm9_08_26_NO3.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_NO3, "CARI", "NO3", "0826")
CARI_storm10_08_30_NO3.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_NO3, "CARI", "NO3", "0830")
CARI_storm11_09_02_NO3.p = hyst_plot(CARI_storm11_09_02_Q, CARI_storm11_09_02_NO3, "CARI", "NO3", "0902")
CARI_storm12a_09_20_NO3.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_NO3, "CARI", "NO3", "0920a")
CARI_storm12b_09_25_NO3.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_NO3, "CARI", "NO3", "0925b")

multiplot(CARI_storm2_06_21_NO3.p)
multiplot(CARI_storm3_06_29_NO3.p)

multiplot(CARI_storm5a_08_04_NO3.p)
multiplot(CARI_storm5b_08_05_NO3.p)
multiplot(CARI_storm5c_08_06_NO3.p)
multiplot(CARI_storm6_08_13_NO3.p)
multiplot(CARI_storm7_08_21_NO3.p)
multiplot(CARI_storm8_08_24_NO3.p)
multiplot(CARI_storm9_08_26_NO3.p)
multiplot(CARI_storm10_08_30_NO3.p)
multiplot(CARI_storm11_09_02_NO3.p)
multiplot(CARI_storm12a_09_20_NO3.p)
multiplot(CARI_storm12b_09_25_NO3.p)


#fDOM
#CARI_storm1_06_10_fDOM.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_fDOM, "CARI", "fDOM", "0610")
CARI_storm2_06_21_fDOM.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_fDOM, "CARI", "fDOM", "0621")
CARI_storm3_06_29_fDOM.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_fDOM, "CARI", "fDOM", "0629")
#CARI_storm4a_06_30_fDOM.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_fDOM, "CARI", "fDOM", "0630a")
#CARI_storm4b_07_01_fDOM.p = hyst_plot(CARI_storm4b_07_01_Q, CARI_storm4b_07_01_fDOM, "CARI", "fDOM", "0701b")
CARI_storm5a_08_04_fDOM.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_fDOM, "CARI", "fDOM", "0804a")
CARI_storm5b_08_05_fDOM.p = hyst_plot(CARI_storm5b_08_05_Q, CARI_storm5b_08_05_fDOM, "CARI", "fDOM", "0805b")
CARI_storm5c_08_06_fDOM.p = hyst_plot(CARI_storm5c_08_06_Q, CARI_storm5c_08_06_fDOM, "CARI", "fDOM", "0806c")
CARI_storm6_08_13_fDOM.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_fDOM, "CARI", "fDOM", "0813")
CARI_storm7_08_21_fDOM.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_fDOM, "CARI", "fDOM", "0821")
CARI_storm8_08_24_fDOM.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_fDOM, "CARI", "fDOM", "0824")
CARI_storm9_08_26_fDOM.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_fDOM, "CARI", "fDOM", "0826")
CARI_storm10_08_30_fDOM.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_fDOM, "CARI", "fDOM", "0830")
CARI_storm11_09_02_fDOM.p = hyst_plot(CARI_storm11_09_02_Q, CARI_storm11_09_02_fDOM, "CARI", "fDOM", "0902")
CARI_storm12a_09_20_fDOM.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_fDOM, "CARI", "fDOM", "0920a")
CARI_storm12b_09_25_fDOM.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_fDOM, "CARI", "v", "0925b")

multiplot(CARI_storm2_06_21_fDOM.p)
multiplot(CARI_storm3_06_29_fDOM.p)

multiplot(CARI_storm5a_08_04_fDOM.p)
multiplot(CARI_storm5b_08_05_fDOM.p)
multiplot(CARI_storm5c_08_06_fDOM.p)
multiplot(CARI_storm6_08_13_fDOM.p)
multiplot(CARI_storm7_08_21_fDOM.p)
multiplot(CARI_storm8_08_24_fDOM.p)
multiplot(CARI_storm9_08_26_fDOM.p)
multiplot(CARI_storm10_08_30_fDOM.p)
multiplot(CARI_storm11_09_02_fDOM.p)
multiplot(CARI_storm12a_09_20_fDOM.p)
multiplot(CARI_storm12b_09_25_fDOM.p)


#SPC
#CARI_storm1_06_10_SPC.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_SPC, "CARI", "SPC", "0610")
CARI_storm2_06_21_SPC.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_SPC, "CARI", "SPC", "0621")
CARI_storm3_06_29_SPC.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_SPC, "CARI", "SPC", "0629")
#CARI_storm4a_06_30_SPC.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_SPC, "CARI", "SPC", "0630a")
#CARI_storm4b_07_01_SPC.p = hyst_plot(CARI_storm4b_07_01_Q, CARI_storm4b_07_01_SPC, "CARI", "SPC", "0701b")
CARI_storm5a_08_04_SPC.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_SPC, "CARI", "SPC", "0804a")
CARI_storm5b_08_05_SPC.p = hyst_plot(CARI_storm5b_08_05_Q, CARI_storm5b_08_05_SPC, "CARI", "SPC", "0805b")
CARI_storm5c_08_06_SPC.p = hyst_plot(CARI_storm5c_08_06_Q, CARI_storm5c_08_06_SPC, "CARI", "SPC", "0806c")
CARI_storm6_08_13_SPC.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_SPC, "CARI", "SPC", "0813")
CARI_storm7_08_21_SPC.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_SPC, "CARI", "SPC", "0821")
CARI_storm8_08_24_SPC.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_SPC, "CARI", "SPC", "0824")
CARI_storm9_08_26_SPC.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_SPC, "CARI", "SPC", "0826")
CARI_storm10_08_30_SPC.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_SPC, "CARI", "SPC", "0830")
CARI_storm11_09_02_SPC.p = hyst_plot(CARI_storm11_09_02_Q, CARI_storm11_09_02_SPC, "CARI", "SPC", "0902")
CARI_storm12a_09_20_SPC.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_SPC, "CARI", "SPC", "0920a")
CARI_storm12b_09_25_SPC.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_SPC, "CARI", "SPC", "0925b")

#multiplot(CARI_storm1_06_10_SPC.p)
multiplot(CARI_storm2_06_21_SPC.p)
multiplot(CARI_storm3_06_29_SPC.p)
#multiplot(CARI_storm4a_06_30_SPC.p)
#multiplot(CARI_storm4b_07_01_SPC.p)
multiplot(CARI_storm5a_08_04_SPC.p)
multiplot(CARI_storm5b_08_05_SPC.p)
multiplot(CARI_storm5c_08_06_SPC.p)
multiplot(CARI_storm6_08_13_SPC.p)
multiplot(CARI_storm7_08_21_SPC.p)
multiplot(CARI_storm8_08_24_SPC.p)
multiplot(CARI_storm9_08_26_SPC.p)
multiplot(CARI_storm10_08_30_SPC.p)
multiplot(CARI_storm11_09_02_SPC.p)
multiplot(CARI_storm12a_09_20_SPC.p)
multiplot(CARI_storm12b_09_25_SPC.p)

#Turb
#CARI_storm1_06_10_turb.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_turb, "CARI", "turb", "0610")
CARI_storm2_06_21_turb.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_turb, "CARI", "turb", "0621")
CARI_storm3_06_29_turb.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_turb, "CARI", "turb", "0629")
#CARI_storm4a_06_30_turb.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_turb, "CARI", "turb", "0630a")
#CARI_storm4b_07_01_turb.p = hyst_plot(CARI_storm4b_07_01_Q, CARI_storm4b_07_01_turb, "CARI", "turb", "0701b")
CARI_storm5a_08_04_turb.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_turb, "CARI", "turb", "0804a")
CARI_storm5b_08_05_turb.p = hyst_plot(CARI_storm5b_08_05_Q, CARI_storm5b_08_05_turb, "CARI", "turb", "0805b")
CARI_storm5c_08_06_turb.p = hyst_plot(CARI_storm5c_08_06_Q, CARI_storm5c_08_06_turb, "CARI", "turb", "0806c")
CARI_storm6_08_13_turb.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_turb, "CARI", "turb", "0813")
CARI_storm7_08_21_turb.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_turb, "CARI", "turb", "0821")
CARI_storm8_08_24_turb.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_turb, "CARI", "turb", "0824")
CARI_storm9_08_26_turb.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_turb, "CARI", "turb", "0826")
CARI_storm10_08_30_turb.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_turb, "CARI", "turb", "0830")
CARI_storm11_09_02_turb.p = hyst_plot(CARI_storm11_09_02_Q, CARI_storm11_09_02_turb, "CARI", "turb", "0902")
CARI_storm12a_09_20_turb.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_turb, "CARI", "turb", "0920a")
CARI_storm12b_09_25_turb.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_turb, "CARI", "turb", "0925b")

#multiplot(CARI_storm1_06_10_turb.p)
multiplot(CARI_storm2_06_21_turb.p)
multiplot(CARI_storm3_06_29_turb.p)
#multiplot(CARI_storm4a_06_30_turb.p)
#multiplot(CARI_storm4b_07_01_turb.p)
multiplot(CARI_storm5a_08_04_turb.p)
multiplot(CARI_storm5b_08_05_turb.p)
multiplot(CARI_storm5c_08_06_turb.p)
multiplot(CARI_storm6_08_13_turb.p)
multiplot(CARI_storm7_08_21_turb.p)
multiplot(CARI_storm8_08_24_turb.p)
multiplot(CARI_storm9_08_26_turb.p)
multiplot(CARI_storm10_08_30_turb.p)
multiplot(CARI_storm11_09_02_turb.p)
multiplot(CARI_storm12a_09_20_turb.p)
multiplot(CARI_storm12b_09_25_turb.p)

multiplot(
  CARI_storm2_06_21_NO3.p,CARI_storm2_06_21_fDOM.p,CARI_storm2_06_21_SPC.p,CARI_storm2_06_21_turb.p,
  CARI_storm3_06_29_NO3.p,CARI_storm3_06_29_fDOM.p,CARI_storm3_06_29_SPC.p,CARI_storm3_06_29_turb.p,
  
  CARI_storm5a_08_04_NO3.p, CARI_storm5a_08_04_fDOM.p,CARI_storm5a_08_04_SPC.p,CARI_storm5a_08_04_turb.p,
  CARI_storm5b_08_05_NO3.p, CARI_storm5b_08_05_fDOM.p,CARI_storm5b_08_05_SPC.p,CARI_storm5b_08_05_turb.p,
  CARI_storm5c_08_06_NO3.p,CARI_storm5c_08_06_fDOM.p,CARI_storm5c_08_06_SPC.p,CARI_storm5c_08_06_turb.p,
  CARI_storm6_08_13_NO3.p, CARI_storm6_08_13_fDOM.p,CARI_storm6_08_13_SPC.p,CARI_storm6_08_13_turb.p,
  CARI_storm7_08_21_NO3.p, CARI_storm7_08_21_fDOM.p,CARI_storm7_08_21_SPC.p,CARI_storm7_08_21_turb.p,
  CARI_storm8_08_24_NO3.p, CARI_storm8_08_24_fDOM.p,CARI_storm8_08_24_SPC.p,CARI_storm8_08_24_turb.p,
  CARI_storm9_08_26_NO3.p, CARI_storm9_08_26_fDOM.p,CARI_storm9_08_26_SPC.p,CARI_storm9_08_26_turb.p,
  CARI_storm10_08_30_NO3.p, CARI_storm10_08_30_fDOM.p,CARI_storm10_08_30_SPC.p,CARI_storm10_08_30_turb.p,
  CARI_storm11_09_02_NO3.p, CARI_storm11_09_02_fDOM.p,CARI_storm11_09_02_SPC.p,CARI_storm11_09_02_turb.p,
  CARI_storm12a_09_20_NO3.p, CARI_storm12a_09_20_fDOM.p,CARI_storm12a_09_20_SPC.p,CARI_storm12a_09_20_turb.p,
  CARI_storm12b_09_25_NO3.p, CARI_storm12b_09_25_fDOM.p,CARI_storm12b_09_25_SPC.p,CARI_storm12b_09_25_turb.p,
  cols = 7
)


# export pdf 20 x 30
setwd("~/Documents/Storms_clean_repo/plots/HI_plots/2018")
# ggsave(CARI_HI_Loops, 
#        file = "CARI_HI_Loops_2018.pdf", 
#        path = "CARI/", 
#        width = 20, height = 30, units = "in")




