
#### GDD ####

#### libraries ####
library(stats)
library(MARSS)
library(forecast)
library(datasets)
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(forecast)
library(zoo)
library(xts)
library(imputeTS)
library(Hmisc)
library(data.table)
library(grid)
library(gridExtra)
library(beepr)

#### data wrangling - load data ####

FRCH_chem_2018 <- read_csv("~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2018.csv", 
                           col_types = cols(fDOM.QSU = col_double(), 
                                            nitrateuM = col_double(), SpCond.uScm = col_double(), 
                                            Turbidity.FNU = col_double()))

FRCH_chem_2019 <- read_csv("~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2019.csv", 
                           col_types = cols(fDOM.QSU.mn = col_double(), 
                                            nitrateuM = col_double(), SpCond.uScm.mn = col_double(), 
                                            Turbidity.FNU.mn = col_double()))

FRCH_chem_2020 <- read_csv("~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2020.csv", 
                           col_types = cols(fDOM.QSU = col_double(), 
                                            nitrateuM = col_double(), SpCond.µS.cm = col_double(), 
                                            Turbidity.FNU = col_double()))

FRCH_chem_2021 <- read_csv("~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2021.csv", 
                           col_types = cols(MeanDischarge = col_double())) 


colnames("")

CPCRW.2017 = read.csv("Stitched_data/CPCRW.2017_may22.00.00.00_sept01.00.00.00.csv", row.names = 1)
CPCRW.2017$date_timeAK = as.POSIXct(CPCRW.2017$date_timeAK, "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
class(CPCRW.2017$date_timeAK)
tz(CPCRW.2017$date_timeAK)

### remove Jones lab grab sample chemistry ###
CPCRW.2017$grab_Ammonium_uM = NULL
CPCRW.2017$grab_Bromide_mg_L = NULL
CPCRW.2017$grab_Chloride_mg_L[CPCRW.2017$grab_lab == "jones"] = NA
CPCRW.2017$grab_DOC_uM[CPCRW.2017$grab_lab == "jones"] = NA
CPCRW.2017$grab_Nitrate_uM[CPCRW.2017$grab_lab == "jones"] = NA
CPCRW.2017$grab_Sulfate_mg_L[CPCRW.2017$grab_lab == "jones"] = NA
CPCRW.2017$grab_TN_mg_L[CPCRW.2017$grab_lab == "jones"] = NA
CPCRW.2017$grab_lab[CPCRW.2017$grab_lab == "jones"] = NA
CPCRW.2017$grab_lab = NULL

### add in air temp ###
### from CPCRW CRREL Main Met Station ###
airtemp = read.csv("~/Documents/Storms/Ancilliary_data/CRREL_airtemp.csv", skip = 5)
airtemp$date_timeAK = as.POSIXct(airtemp$Time, "%m/%d/%y %H:%M", tz="America/Anchorage")
class(airtemp$date_timeAK)
tz(airtemp$date_timeAK)
airtemp$airtemp_100.1200cm_mean = (airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_100cm.CRREL..C.+ airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_300cm.CRREL..C.+ airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1000cm.CRREL..C.+ airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1200cm.CRREL..C.)/4
airtemp = subset(airtemp, select=c("date_timeAK", "airtemp_100.1200cm_mean"))

#CPCRW.2017 = left_join(CPCRW.2017, airtemp, by="date_timeAK")

#### data wrangling - calculate daily local GDD/GDUs ####

### 15-min air temp needed to get daily min and max ###
## from CPCRW CRREL Main Met Station ##
airtemp = read.csv("~/Documents/Storms/Ancilliary_data/CRREL_airtemp.csv", skip = 5)
airtemp$date_timeAK = as.POSIXct(airtemp$Time, "%m/%d/%y %H:%M", tz="America/Anchorage")
class(airtemp$date_timeAK)
tz(airtemp$date_timeAK)
# min temp at 1200 is missing, so i'm going to exclude this height in mean and max temp avgs 
airtemp$airtemp_100.1200cm_mean = (
  airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_100cm.CRREL..C.+ 
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_300cm.CRREL..C.+ 
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1000cm.CRREL..C. +
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1200cm.CRREL..C.)/4

airtemp$airtemp_100.1200cm_max = (
  airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_100cm_MAX.CRREL..C.+ 
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_300cm_MAX.CRREL..C.+ 
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1000cm_MAX.CRREL..C. +
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1200cm_MAX.CRREL..C.)/4

airtemp$airtemp_100.1200cm_min = (
  airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_100cm_MIN.CRREL..C.+ 
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_300cm_MIN.CRREL..C.+ 
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1000cm_MIN.CRREL..C.+
    airtemp$CPCRW.CRREL.Main.Met.Station..AirTemp_1200cm_MIN.CRREL..C.)/4

plot(airtemp$airtemp_100.1200cm_mean ~ airtemp$date_timeAK, type="l")
lines(airtemp$airtemp_100.1200cm_max ~ airtemp$date_timeAK, type="l", col="red")
lines(airtemp$airtemp_100.1200cm_min ~ airtemp$date_timeAK, type="l", col="blue")
airtemp = subset(airtemp, select=c("date_timeAK", "airtemp_100.1200cm_mean", "airtemp_100.1200cm_max", "airtemp_100.1200cm_min"))
range(airtemp$date_timeAK)

### CPCRW ###
airtemp2 = read.csv("~/Documents/Storms/Ancilliary_data/Caribou_airtemp.csv", skip=5)
airtemp2$date_timeAK = as.POSIXct(airtemp2$Time, "%m/%d/%y %H:%M", tz="America/Anchorage")
class(airtemp2$date_timeAK)
tz(airtemp2$date_timeAK)

airtemp2$airtemp2_100.1000cm_mean = (
  airtemp2$CPCRW.Caribou.Peak..AirTemp_100cm.CPEAK..C.+ 
    airtemp2$CPCRW.Caribou.Peak..AirTemp_200cm.CPEAK..C.+ 
    airtemp2$CPCRW.Caribou.Peak..AirTemp_1000cm.CPEAK..C.)/3

airtemp2$airtemp2_100.1000cm_max = (
  airtemp2$CPCRW.Caribou.Peak..AirTemp_100cm_MAX.CPEAK..C.+ 
    airtemp2$CPCRW.Caribou.Peak..AirTemp_200cm_MAX.CPEAK..C.+ 
    airtemp2$CPCRW.Caribou.Peak..AirTemp_1000cm_MAX.CPEAK..C.)/3

airtemp2$airtemp2_100.1000cm_min = (
  airtemp2$CPCRW.Caribou.Peak..AirTemp_100cm_MIN.CPEAK..C.+ 
    airtemp2$CPCRW.Caribou.Peak..AirTemp_200cm_MIN.CPEAK..C.+ 
    airtemp2$CPCRW.Caribou.Peak..AirTemp_1000cm_MIN.CPEAK..C.)/3

plot(airtemp2$airtemp2_100.1000cm_mean ~ airtemp2$date_timeAK, type="l")
lines(airtemp2$airtemp2_100.1000cm_max ~ airtemp2$date_timeAK, type="l", col="red")
lines(airtemp2$airtemp2_100.1000cm_min ~ airtemp2$date_timeAK, type="l", col="blue")
airtemp2 = subset(airtemp2, select=c("date_timeAK", "airtemp2_100.1000cm_mean", "airtemp2_100.1000cm_max", "airtemp2_100.1000cm_min"))
range(airtemp2$date_timeAK)

# join data from two met staions
time <- data.frame(
  date_timeAK = seq.POSIXt(
    from = ISOdatetime(2018,05,01,0,0,0, tz = "America/Anchorage"),
    to = ISOdatetime(2021,10,31,23,0,0, tz= "America/Anchorage"),
    by = "1 hours" ))
airtempall = left_join(time, airtemp, by="date_timeAK")
airtempall = left_join(airtempall, airtemp2, by="date_timeAK")

# average temps from two met stations
airtempmean = data.frame(date_timeAK = airtempall$date_timeAK,
                         airtemp_100.1000cm_mean = rowMeans(
                           airtempall[c("airtemp_100.1200cm_mean", 
                                        "airtemp2_100.1000cm_mean")]),
                         airtemp_100.1000cm_max = rowMeans(
                           airtempall[c("airtemp_100.1200cm_max", 
                                        "airtemp2_100.1000cm_max")]),
                         airtemp_100.1000cm_min = rowMeans(
                           airtempall[c("airtemp_100.1200cm_min", 
                                        "airtemp2_100.1000cm_min")]))

write.csv(airtempmean, "~/Documents/Storms/Ancilliary_data/airtempmean.csv")

# get daily
airtempmean$day = format(as.POSIXct(airtempmean$date_timeAK,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
airtempmean$day = as.POSIXct(airtempmean$day, "%Y-%m-%d", tz="America/Anchorage")
airtempmean.daily = 
  airtempmean %>%
  select(day, airtemp_100.1000cm_mean, airtemp_100.1000cm_max, airtemp_100.1000cm_min) %>%
  group_by(day) %>%
  summarize_all(funs(mean, max, min), na.rm = TRUE)

plot(airtempmean.daily$airtemp_100.1000cm_mean_mean ~ airtempmean.daily$day, type="o", ylim=c(-20,40))
lines(airtempmean.daily$airtemp_100.1000cm_max_max ~ airtempmean.daily$day, type="o", col="red")
lines(airtempmean.daily$airtemp_100.1000cm_min_min ~ airtempmean.daily$day, type="o", col="blue")

### calc GDD ###
#install.packages("remotes")
#remotes::install_github("karawoo/cbccy")
#install.packages("cbccy")
library(cbccy)



GDD = calcGDD(temp.hi = airtempmean.daily$airtemp_100.1000cm_max_max, 
              temp.low = airtempmean.daily$airtemp_100.1000cm_min_min, 
              temp.base = 5, 
              temp.max = 30,
              temp.min = 5)
GDD = data.frame(day = airtempmean.daily$day,
                 GDD.local = GDD)


names(GDD)[1] <- 'date'
write.csv(GDD, "~/Documents/Storms/Ancilliary_data/GDD.csv")








