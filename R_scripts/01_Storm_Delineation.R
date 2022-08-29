#### READ ME ####
# The purpose of this script is to prepare DoD data for hysteresis analysis and, specifically, the hysteresisMetrics function.
# Step 1: Load in processed SUNA and EXO data from the previous year 
# scripts within DoD 2018/2019/2020/2021
#DoD->AK Sensors->2020-> SUNA->Processed & DoD->AK Sensors->2020-> EXO->Processed 
# Step 2: fill gaps in nitrate, fDOM, SpCond, and turbidity data
# Step 3: Define baseflow in each catchment.
# Step 4: Set criteria for storm delineation for each catchment based on some percentage over baseflow.
# Step 5: Delineate storms in each catchment.
# Step 6: IN PYTHON: convert R discharge df to pandas df containing a datetime column named 'valuedatetime', and discharge values in a column 'datavalue'
# Step 7: IN PYTHON: convert R response df(s) to pandas df(s) containing a datetime column named 'valuedatetime', and response values in a column 'datavalue'

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
library(readr)
library(googledrive)
library(purrr)
library(here)
# Load from local machine #
#### Load field dates and times ####
#These are stored in field_datetime.csv on Drive (DoD project/2020 AK sensors)

setwd(here("Storms_clean_repo"))

# Make sub-directories where data will be stored
dir.create(file.path("R_scripts"))
dir.create(file.path("processed_sensor_data"))
dir.create(file.path("processed_sensor_data", "2018"))
dir.create(file.path("Q"))
dir.create(file.path("Q", "2018"))
dir.create(file.path("Q", "2018", "FRCH"))
dir.create(file.path("Q", "2018", "MOOS"))
dir.create(file.path("Q", "Q_chem"))

setwd(here("Q", "2018"))
Q.daily.2018 <- read.csv("Q.daily.2018.csv") # this is from my DoD Discharge script that finalizes Q and is QA/QCd data
Q.2018 <- read.csv("Q_2018.csv")
names(Q.2018) <- c("datetimeAK", "site.ID", "Q", "day") # renaming the column headers to match that of the chem file 

Q.2018$datetimeAK <- ymd_hms(Q.2018$datetimeAK) # converting character to datetime


setwd(here("processed_sensor_data", "2018"))
chem.2018 <- read.csv("SUNA.EXO.int.corr.csv")

chem.2018 <- chem.2018[, -c(2:15,19:24,26:52,54:56)] # removing unnecessary columns which consists of a lot of the SUNA diagnostic columns and channels that are needed
# I just want the constituents and datetimeAk

chem.2018 <- chem.2018[, c(1,6,4,2,3,5)] # reorganizing column headers

chem.2018$datetimeAK <- ymd_hms(chem.2018$datetimeAK) # converting character to datetime

names(chem.2018) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3")

### when merging the previous file in another script the dates got all screwed up ###
# I am reading in the file and separating by site and then ordering the dates in ascending order and then remerging 
FRCH.2018 <-  subset(chem.2018, site.ID == "FRCH")
FRCH.2018 <- FRCH.2018[order(FRCH.2018$datetimeAK),]

MOOS.2018 <-  subset(chem.2018, site.ID == "MOOS")
MOOS.2018 <- MOOS.2018[order(MOOS.2018$datetimeAK),]

chem.2018 <- rbind(FRCH.2018, MOOS.2018)



DOD.2018 <- full_join(chem.2018, Q.2018) # merging chem and discharge data 

#write_csv(DOD.2018, "~/Documents/Storms_clean_repo/Q/Q_chem/DOD.2018.csv")
### READ in CARI Data ###
# this is data directly from the NEON data base website that I downloaded 

CARI.2018 <- read_csv("NEON_WaterQuality2018.csv",
                      col_types = cols(Discharge = col_double(), 
                                       fDOM = col_double(), NO3 = col_double(), 
                                       SpCond = col_double(), Turb = col_double()))
names(CARI.2018)[names(CARI.2018) == 'DateTime'] <- 'datetimeAK'
names(CARI.2018)[names(CARI.2018) == 'Site'] <- 'site.ID'

#plot(CARI.2018$datetimeAK, CARI.2018$Discharge)
attributes(CARI.2018$datetimeAK)$tzone <- "America/Anchorage" # making sure it is in AK timezone

CARI.2018$day <-  format(as.POSIXct(CARI.2018$datetimeAK,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
CARI.2018$day <-  as.POSIXct(CARI.2018$day, "%Y-%m-%d", tz="America/Anchorage")

# make a daily Q record for CARI
cari.final.discharge.2018 <- CARI.2018[,-c(4:7)]
CARI.daily.2018 <-  with(CARI.2018, tapply(Discharge, list(day, site.ID), mean))
CARI.daily.2018 <-  as.data.frame(CARI.daily.2018)

CARI.Q.2018 <-  as.data.frame(CARI.daily.2018$CARI)
CARI.Q.2018$day <-  as.Date(rownames(CARI.daily.2018))
names(CARI.Q.2018) <-  c("Discharge_Lsec", "day")

# subset data by site #
setwd("~/Documents/Storms_clean_repo")
FRCH.2018 <-  subset(DOD.2018, site.ID == "FRCH")

MOOS.2018 = subset(DOD.2018, site.ID == "MOOS")

# Making a final discharge file for each site
# FRCH
frch.final.discharge.2018 <- subset(Q.2018, site.ID == "FRCH")
# head(frch.final.discharge.2018$datetimeAK)
# attributes(frch.final.discharge.2018$DateTime)$tzone <-  'America/Anchorage'

# MOOS 
moos.final.discharge.2018 <- subset(Q.2018, site.ID == "MOOS")
# attributes(moos.final.discharge.2018$datetimeAK)$tzone <-  'America/Anchorage'
# head(moos.final.discharge.2018$datetimeAK)

Q.2018$day <-  format(as.POSIXct(Q.2018$datetimeAK,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d")
Q.2018$day <-  as.POSIXct(Q.2018$day, "%Y-%m-%d", tz="America/Anchorage")

# Making a daily Q record for each site 
Q.daily.2018 <-  with(Q.2018, tapply(Q, list(day, site.ID), mean))
Q.daily.2018 <-  as.data.frame(Q.daily.2018)

FRCH.Q.2018 <-  as.data.frame(Q.daily.2018$FRCH)
FRCH.Q.2018$day <-  as.Date(rownames(Q.daily.2018))
names(FRCH.Q.2018) <-  c("Discharge_Lsec", "day")

MOOS.Q.2018 <-  as.data.frame(Q.daily.2018$MOOS)
MOOS.Q.2018$day <-  as.Date(rownames(Q.daily.2018))
names(MOOS.Q.2018) <-  c("Discharge_Lsec", "day")

# data wrangling - fill gaps #
# fxn #
fillgaps15 = function(df, dat, datquotes, largegap.num){
  ## Document gaps >= largegap.num (1 largegap.num = 15 min) ##
  # (note - the criteria of what constitutes a "large" gap should be reevaluated depending on the trend being characterized)
  is.na.rle <- rle(is.na(dat))
  is.na.rle$values <- is.na.rle$values & is.na.rle$lengths >= (largegap.num)
  biggaps = df[inverse.rle(is.na.rle), ]
  tz(biggaps$datetimeAK) = "America/Anchorage"
  biggaps = subset(biggaps, select = "datetimeAK")
  # Make univariate time series, covert to zoo, then to ts #
  ts.xts = subset(df, select = c("datetimeAK",datquotes))
  ts.xts<-read.zoo(ts.xts, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
  ts.xts<-as.xts(ts.xts)
  # remove leading and trailing NAs #
  ts.xts = na.trim(ts.xts, is.na="any")
  # Apply auto.arima and kalman filter to impute missing values #
  fit2 = auto.arima(ts.xts) 
  kal = KalmanSmooth(ts.xts, fit2$model)
  id.na<-which(is.na(ts.xts))
  for(i in id.na) 
    ts.xts[i]<-fit2$model$Z %*% kal$smooth[i,]
  # revert to dataframe #
  ts.df = as.data.frame((ts.xts))
  ts.df$date_timeAK = as.POSIXct(row.names(ts.df), tz="America/Anchorage")
  names(ts.df) = c("dat_filled", "datetimeAK")
  # remove large gaps # 
  ts.df$dat_filled[ts.df$date_timeAK %in% as.POSIXct(biggaps$datetimeAK)] = NA
  # Replace large gaps with linear interpolation #
  ts.df$dat_filled = na.interpolation(ts.df$dat_filled)
  ts.df = subset(ts.df, select = c("dat_filled", "datetimeAK"))
  return(ts.df)
}

# FRCH #
# join Q and chem data 
FRCH = full_join(frch.final.discharge.2018, FRCH.2018)

# MOOS #
MOOS = full_join(moos.final.discharge.2018, MOOS.2018)

# # Write CSV #
# write_csv(FRCH, "~/Documents/Storms/Q_Chem/FRCH/FRCH_chem_2018.csv")
# write_csv(MOOS, "~/Documents/Storms/Q_Chem/MOOS/MOOS_chem_2018.csv")

# Baseflow Separation #
# this wont work with any NAs so I have to remove NAs
any(is.na(FRCH.Q.2018$day))
any(is.na(FRCH.Q.2018$Discharge_Lsec)) 
FRCH.Q.2018 <- na.omit(FRCH.Q.2018) # Removed 17 rows - (151 to 134)

any(is.na(MOOS.Q.2018$day))
any(is.na(MOOS.Q.2018$Discharge_Lsec)) # no NAs in MOOS 

any(is.na(CARI.Q.2018$day))
any(is.na(CARI.Q.2018$Discharge_Lsec)) 
CARI.Q.2018 <- na.omit(CARI.Q.2018) # Removed 4 rows - (81 to 77)


### examine the recursive digital filter at .9, .925, .95 levels ###
plot(FRCH$Q ~ FRCH$datetimeAK, type = "l", xlab = "", ylab = "Q (L/sec)",
     xlim =  as.POSIXct(c("2018-05-01 00:00:00","2018-10-31 00:00:00"), tz="America/Anchorage"),
     ylim = c(0, 1500), col="blue")

plot(MOOS$Q ~ MOOS$datetimeAK, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-31 00:00:00"), tz="America/Anchorage"),
     ylim = c(0,5000), col="blue")

ggplot(aes(x = datetimeAK, y = Discharge), data = CARI.2018) +
  geom_line(color="#A6CEE3", size=1.25) +
  theme_classic() +
  ggtitle("CARI measured Q") +
  xlab("Date") +
  ylab("Discharge (L/s)") +
  xlab("")

### Hydrograph Separation ###

# FRCH
frch.final.discharge.2018 <- na.omit(frch.final.discharge.2018) # 6521-6472

FRCH_Q_bf = BaseflowSeparation(frch.final.discharge.2018$Q)
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge.2018$Q, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(FRCH.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 

# MOOS
moos.final.discharge.2018 <- na.omit(moos.final.discharge.2018) # none!

MOOS_Q_bf = BaseflowSeparation(moos.final.discharge.2018$Q, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(MOOS.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=MOOS_Q_bf$bt) 

# CARI
any(is.na(cari.final.discharge.2018$Discharge))
cari.final.discharge.2018 <- na.omit(cari.final.discharge.2018) # removed 432006-381662
CARI_Q_bf = BaseflowSeparation(cari.final.discharge.2018$Discharge, filter_parameter = 0.90, passes = 3)
hydrograph(input=subset(CARI.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=CARI_Q_bf$bt) 


###.925 ###
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge.2018$Q)
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge.2018$Q, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(FRCH.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 

# MOOS
MOOS_Q_bf = BaseflowSeparation(moos.final.discharge.2018$Q, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(MOOS.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=MOOS_Q_bf$bt) 

#CARI
CARI_Q_bf = BaseflowSeparation(cari.final.discharge.2018$Discharge, filter_parameter = 0.925, passes = 3)
hydrograph(input=subset(CARI.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=CARI_Q_bf$bt) 

### .95 ###
FRCH_Q_bf = BaseflowSeparation(frch.final.discharge.2018$Q, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(FRCH.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=FRCH_Q_bf$bt) 

# 
MOOS_Q_bf = BaseflowSeparation(moos.final.discharge.2018$Q, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(MOOS.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=MOOS_Q_bf$bt) 

#
CARI_Q_bf = BaseflowSeparation(cari.final.discharge.2018$Discharge, filter_parameter = 0.95, passes = 3)
hydrograph(input=subset(CARI.Q.2018, select = c(day, Discharge_Lsec)), streamflow2=CARI_Q_bf$bt) 

# Deliniate storms in FRCH #

# ID storms: Any events where Q and chem responded after an alarm from our precipitation metric
# Pick starting points: manually select inflection pt when Q began to rise
# Pick ending points: manually select pt when Q reached pre-storm baseflow OR when another event occurred

FRCH_bfQ_mn = mean(FRCH_Q_bf$bt)
FRCH_bfQ_mn
FRCH_bfQ_mn*2

MOOS_bfQ_mn = mean(MOOS_Q_bf$bt)
MOOS_bfQ_mn
MOOS_bfQ_mn*2

CARI_bfQ_mn = mean(CARI_Q_bf$bt)
CARI_bfQ_mn
CARI_bfQ_mn*2

# Merge Discharge and Precip #

POKE.st <- read_csv("~/Documents/DoD_2018_Jake/RainGauge/POKE.RainGauge.2018.csv")
attributes(POKE.st$DateTime)$tzone <- "America/Anchorage"
### Sum daily discharge ###
POKE.st$twentyfour <- rollapplyr(POKE.st$inst_rainfall_mm, 96, sum, na.rm = TRUE, fill = NA, partial = TRUE)
POKE.st$fourtyeight <- rollapplyr(POKE.st$inst_rainfall_mm, 192, sum, na.rm = TRUE, fill = NA, partial = TRUE)

# Greater than 5 #
poke.five.twenty.four <- POKE.st[which(POKE.st$twentyfour >= 5),] # twenty four hour period where the precip is 5
poke.five.fourty.eight <- POKE.st[which(POKE.st$fourtyeight >= 5),] # fourty eight hour period where the precip is greater than 10

# Greater than 10 #
poke.ten.twenty.four <- POKE.st[which(POKE.st$twentyfour >= 10),] # twenty four hour period where the precip is 10
poke.ten.fourty.eight <- POKE.st[which(POKE.st$fourtyeight >= 10),] # fourty eight hour period where the precip is greater than 10


### Precip Discharge Chem ###
#FRCH#
# rename the columns to what they were to save me a bunch of time going back and changing every call to a column
names(FRCH) <- c("DateTime", "Site", "MeanDischarge", "day",
                 "fDOM.QSU", "SpCond.uScm", "Turbidity.FNU",
                 "nitrateuM")
#plots
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-01 0:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'POKE precip. (mm)')
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col="yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col = "green", lwd = 0.1)
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-01 0:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
par(new = T)
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
lines(FRCH$nitrateuM * 25 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="blue",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))

# NO chem until June 1st 
# Storm 1 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
lines(FRCH$nitrateuM * 25 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-21 01:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-06-24 01:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm1_06_21 = FRCH[FRCH$DateTime > as.POSIXct("2018-06-21 01:30:00", tz="America/Anchorage") &
                           FRCH$DateTime < as.POSIXct("2018-06-24 01:00:00", tz="America/Anchorage"),]
plot(FRCH_storm1_06_21$MeanDischarge ~ as.POSIXct(FRCH_storm1_06_21$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,600), col="blue", main="FRCH 180621 storm 1",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 25 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 6 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2a # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-29 18:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-04 23:45:00", ), col="purple")

FRCH_storm2a_06_29 = FRCH[FRCH$DateTime > as.POSIXct("2018-06-29 18:30:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-07-04 23:45:00", tz="America/Anchorage"),]
plot(FRCH_storm2a_06_29$MeanDischarge ~ as.POSIXct(FRCH_storm2a_06_29$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(300,1200), col="blue", main="FRCH 180629 storm 2a",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 25 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2b # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
abline(v= as.POSIXct("2018-07-04 23:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-08 14:00:00", tz="America/Anchorage"), col="purple")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-04 14:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-08 14:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm2b_07_04 = FRCH[FRCH$DateTime > as.POSIXct("2018-07-04 23:45:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-07-08 14:00:00", tz="America/Anchorage"),]
plot(FRCH_storm2b_07_04$MeanDischarge ~ as.POSIXct(FRCH_storm2b_07_04$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,600), col="blue", main="FRCH 180704 storm 2b",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 3 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-10 20:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-14 23:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm3_07_10 = FRCH[FRCH$DateTime > as.POSIXct("2018-07-10 20:30:00", tz="America/Anchorage") &
                           FRCH$DateTime < as.POSIXct("2018-07-14 23:00:00", tz="America/Anchorage"),]
plot(FRCH_storm3_07_10$MeanDischarge ~ as.POSIXct(FRCH_storm3_07_10$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(300,600), col="blue", main="FRCH 180710 storm 3",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-28 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 4a # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-15 02:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-16 23:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm4a_07_15 = FRCH[FRCH$DateTime > as.POSIXct("2018-07-15 02:30:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-07-16 23:00:00", tz="America/Anchorage"),]
plot(FRCH_storm4a_07_15$MeanDischarge ~ as.POSIXct(FRCH_storm4a_07_15$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(350,700), col="blue", main="FRCH 180715 storm 4a",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))

par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 4b # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-16 23:15:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-24 15:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm4b_07_16 = FRCH[FRCH$DateTime > as.POSIXct("2018-07-16 23:15:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-07-24 15:00:00", tz="America/Anchorage"),]
plot(FRCH_storm4b_07_16$MeanDischarge ~ as.POSIXct(FRCH_storm4b_07_16$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(300,600), col="blue", main="FRCH 180716 storm 4b",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 5 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-01 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-01 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-04 17:15:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-11 15:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm5_08_04 = FRCH[FRCH$DateTime > as.POSIXct("2018-08-04 17:15:00", tz="America/Anchorage") &
                           FRCH$DateTime < as.POSIXct("2018-08-11 15:00:00", tz="America/Anchorage"),]
plot(FRCH_storm5_08_04$MeanDischarge ~ as.POSIXct(FRCH_storm5_08_04$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(250,1000), col="blue", main="FRCH 180804 storm 5",
     xlim = as.POSIXct(c("2018-08-01 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00")))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-01 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 6 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-13 07:15:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-19 15:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm6_08_13 = FRCH[FRCH$DateTime > as.POSIXct("2018-08-13 07:15:00", tz="America/Anchorage") &
                           FRCH$DateTime < as.POSIXct("2018-08-19 15:00:00", tz="America/Anchorage"),]
plot(FRCH_storm6_08_13$MeanDischarge ~ as.POSIXct(FRCH_storm6_08_13$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(300,800), col="blue", main="FRCH 180813 storm 6",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 7 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-23 20:15:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-26 02:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm7_08_23 = FRCH[FRCH$DateTime > as.POSIXct("2018-08-23 20:15:00", tz="America/Anchorage") &
                           FRCH$DateTime < as.POSIXct("2018-08-26 02:00:00", tz="America/Anchorage"),]
plot(FRCH_storm7_08_23$MeanDischarge ~ as.POSIXct(FRCH_storm7_08_23$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(300,700), col="blue", main="FRCH 180823 storm 7",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 4 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 100 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 8a # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-26 12:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-28 04:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm8a_08_26 = FRCH[FRCH$DateTime > as.POSIXct("2018-08-26 12:00:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-08-28 04:00:00", tz="America/Anchorage"),]
plot(FRCH_storm8a_08_26$MeanDischarge ~ as.POSIXct(FRCH_storm8a_08_26$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,700), col="blue", main="FRCH 180826 storm 8a",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 6 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 8b # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-28 04:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-30 04:00:00", tz="America/Anchorage"), col="purple")

FRCH_storm8b_08_28 = FRCH[FRCH$DateTime > as.POSIXct("2018-08-28 04:00:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-08-30 04:00:00", tz="America/Anchorage"),]
plot(FRCH_storm8b_08_28$MeanDischarge ~ as.POSIXct(FRCH_storm8b_08_28$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,800), col="blue", main="FRCH 180828 storm 8b",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 9 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-30 04:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-0113:45:00", tz="America/Anchorage"), col="purple")

FRCH_storm9_08_30 = FRCH[FRCH$DateTime > as.POSIXct("2018-08-30 04:00:00", tz="America/Anchorage") &
                           FRCH$DateTime < as.POSIXct("2018-09-01 05:45:00", tz="America/Anchorage"),]
plot(FRCH_storm9_08_30$MeanDischarge ~ as.POSIXct(FRCH_storm9_08_30$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,800), col="blue", main="FRCH 180830 storm 9",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 7 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 10 # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-01 12:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-15 20:45:00", tz="America/Anchorage"), col="purple")

FRCH_storm10_09_01 = FRCH[FRCH$DateTime > as.POSIXct("2018-09-01 12:00:00", tz="America/Anchorage") &
                            FRCH$DateTime < as.POSIXct("2018-09-15 20:45:00", tz="America/Anchorage"),]
plot(FRCH_storm10_09_01$MeanDischarge ~ as.POSIXct(FRCH_storm10_09_01$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,1400), col="blue", main="FRCH 180901 storm 10a",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 5 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 7 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)


# Storm 11a # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-22 12:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-24 03:45:00", tz="America/Anchorage"), col="purple")

FRCH_storm11a_09_22 = FRCH[FRCH$DateTime > as.POSIXct("2018-09-22 12:00:00", tz="America/Anchorage") &
                             FRCH$DateTime < as.POSIXct("2018-09-24 03:45:00", tz="America/Anchorage"),]
plot(FRCH_storm11a_09_22$MeanDischarge ~ as.POSIXct(FRCH_storm11a_09_22$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(600,1000), col="blue", main="FRCH 180922 storm 11a",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))

par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 11b# 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-24 11:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-29 23:45:00", tz="America/Anchorage"), col="purple")

FRCH_storm11b_09_24 = FRCH[FRCH$DateTime > as.POSIXct("2018-09-24 11:45:00", tz="America/Anchorage") &
                             FRCH$DateTime < as.POSIXct("2018-09-29 15:45:00", tz="America/Anchorage"),]
plot(FRCH_storm11b_09_24$MeanDischarge ~ as.POSIXct(FRCH_storm11b_09_24$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(600,1000), col="blue", main="FRCH 180924 storm 11b",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 10 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# No storm for the remainder of the season # 
plot(FRCH$MeanDischarge ~ FRCH$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-10-01 00:00:00","2018-10-31 23:45:00"), tz="America/Anchorage"))
abline(h=FRCH_bfQ_mn*2, col="red", lty=2)
abline(h=FRCH_bfQ_mn, col="red")
lines(FRCH$nitrateuM * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$fDOM.QSU * 15 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$SpCond.uScm * 7 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(FRCH$Turbidity.FNU * 20 ~ FRCH$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-10-01 00:00:00","2018-10-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)


# modify format and save FRCH storms # 
# nitrateuM, fDOM.QSU 
FRCH_storm1_06_21_Q = subset(FRCH_storm1_06_21, select = c("DateTime","MeanDischarge"))
names(FRCH_storm1_06_21_Q) = c("valuedatetime","datavalue")
FRCH_storm1_06_21_NO3 = subset(FRCH_storm1_06_21, select = c("DateTime","nitrateuM"))
names(FRCH_storm1_06_21_NO3) = c("valuedatetime","datavalue")
FRCH_storm1_06_21_fDOM = subset(FRCH_storm1_06_21, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm1_06_21_fDOM) = c("valuedatetime","datavalue")
FRCH_storm1_06_21_SPC = subset(FRCH_storm1_06_21, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm1_06_21_SPC) = c("valuedatetime","datavalue")
FRCH_storm1_06_21_turb = subset(FRCH_storm1_06_21, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm1_06_21_turb) = c("valuedatetime","datavalue")

FRCH_storm2a_06_29_Q = subset(FRCH_storm2a_06_29, select = c("DateTime","MeanDischarge"))
names(FRCH_storm2a_06_29_Q) = c("valuedatetime","datavalue")
FRCH_storm2a_06_29_NO3 = subset(FRCH_storm2a_06_29, select = c("DateTime","nitrateuM"))
names(FRCH_storm2a_06_29_NO3) = c("valuedatetime","datavalue")
FRCH_storm2a_06_29_fDOM = subset(FRCH_storm2a_06_29, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm2a_06_29_fDOM) = c("valuedatetime","datavalue")
FRCH_storm2a_06_29_SPC = subset(FRCH_storm2a_06_29, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm2a_06_29_SPC) = c("valuedatetime","datavalue")
FRCH_storm2a_06_29_turb = subset(FRCH_storm2a_06_29, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm2a_06_29_turb) = c("valuedatetime","datavalue")

FRCH_storm2b_07_04_Q = subset(FRCH_storm2b_07_04, select = c("DateTime","MeanDischarge"))
names(FRCH_storm2b_07_04_Q) = c("valuedatetime","datavalue")
FRCH_storm2b_07_04_NO3 = subset(FRCH_storm2b_07_04, select = c("DateTime","nitrateuM"))
names(FRCH_storm2b_07_04_NO3) = c("valuedatetime","datavalue")
FRCH_storm2b_07_04_fDOM = subset(FRCH_storm2b_07_04, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm2b_07_04_fDOM) = c("valuedatetime","datavalue")
FRCH_storm2b_07_04_SPC = subset(FRCH_storm2b_07_04, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm2b_07_04_SPC) = c("valuedatetime","datavalue")
FRCH_storm2b_07_04_turb = subset(FRCH_storm2b_07_04, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm2b_07_04_turb) = c("valuedatetime","datavalue")

FRCH_storm3_07_10_Q = subset(FRCH_storm3_07_10, select = c("DateTime","MeanDischarge"))
names(FRCH_storm3_07_10_Q) = c("valuedatetime","datavalue")
FRCH_storm3_07_10_NO3 = subset(FRCH_storm3_07_10, select = c("DateTime","nitrateuM"))
names(FRCH_storm3_07_10_NO3) = c("valuedatetime","datavalue")
FRCH_storm3_07_10_fDOM = subset(FRCH_storm3_07_10, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm3_07_10_fDOM) = c("valuedatetime","datavalue")
FRCH_storm3_07_10_SPC = subset(FRCH_storm3_07_10, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm3_07_10_SPC) = c("valuedatetime","datavalue")
FRCH_storm3_07_10_turb = subset(FRCH_storm3_07_10, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm3_07_10_turb) = c("valuedatetime","datavalue")

FRCH_storm4a_07_15_Q = subset(FRCH_storm4a_07_15, select = c("DateTime","MeanDischarge"))
names(FRCH_storm4a_07_15_Q) = c("valuedatetime","datavalue")
FRCH_storm4a_07_15_NO3 = subset(FRCH_storm4a_07_15, select = c("DateTime","nitrateuM"))
names(FRCH_storm4a_07_15_NO3) = c("valuedatetime","datavalue")
FRCH_storm4a_07_15_fDOM = subset(FRCH_storm4a_07_15, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm4a_07_15_fDOM) = c("valuedatetime","datavalue")
FRCH_storm4a_07_15_SPC = subset(FRCH_storm4a_07_15, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm4a_07_15_SPC) = c("valuedatetime","datavalue")
FRCH_storm4a_07_15_turb = subset(FRCH_storm4a_07_15, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm4a_07_15_turb) = c("valuedatetime","datavalue")

FRCH_storm4b_07_16_Q = subset(FRCH_storm4b_07_16, select = c("DateTime","MeanDischarge"))
names(FRCH_storm4b_07_16_Q) = c("valuedatetime","datavalue")
FRCH_storm4b_07_16_NO3 = subset(FRCH_storm4b_07_16, select = c("DateTime","nitrateuM"))
names(FRCH_storm4b_07_16_NO3) = c("valuedatetime","datavalue")
FRCH_storm4b_07_16_fDOM = subset(FRCH_storm4b_07_16, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm4b_07_16_fDOM) = c("valuedatetime","datavalue")
FRCH_storm4b_07_16_SPC = subset(FRCH_storm4b_07_16, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm4b_07_16_SPC) = c("valuedatetime","datavalue")
FRCH_storm4b_07_16_turb = subset(FRCH_storm4b_07_16, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm4b_07_16_turb) = c("valuedatetime","datavalue")

FRCH_storm5_08_04_Q = subset(FRCH_storm5_08_04, select = c("DateTime","MeanDischarge"))
names(FRCH_storm5_08_04_Q) = c("valuedatetime","datavalue")
FRCH_storm5_08_04_NO3 = subset(FRCH_storm5_08_04, select = c("DateTime","nitrateuM"))
names(FRCH_storm5_08_04_NO3) = c("valuedatetime","datavalue")
FRCH_storm5_08_04_fDOM = subset(FRCH_storm5_08_04, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm5_08_04_fDOM) = c("valuedatetime","datavalue")
FRCH_storm5_08_04_SPC = subset(FRCH_storm5_08_04, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm5_08_04_SPC) = c("valuedatetime","datavalue")
FRCH_storm5_08_04_turb = subset(FRCH_storm5_08_04, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm5_08_04_turb) = c("valuedatetime","datavalue")

FRCH_storm6_08_13_Q = subset(FRCH_storm6_08_13, select = c("DateTime","MeanDischarge"))
names(FRCH_storm6_08_13_Q) = c("valuedatetime","datavalue")
FRCH_storm6_08_13_NO3 = subset(FRCH_storm6_08_13, select = c("DateTime","nitrateuM"))
names(FRCH_storm6_08_13_NO3) = c("valuedatetime","datavalue")
FRCH_storm6_08_13_fDOM = subset(FRCH_storm6_08_13, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm6_08_13_fDOM) = c("valuedatetime","datavalue")
FRCH_storm6_08_13_SPC = subset(FRCH_storm6_08_13, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm6_08_13_SPC) = c("valuedatetime","datavalue")
FRCH_storm6_08_13_turb = subset(FRCH_storm6_08_13, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm6_08_13_turb) = c("valuedatetime","datavalue")

FRCH_storm7_08_23_Q = subset(FRCH_storm7_08_23, select = c("DateTime","MeanDischarge"))
names(FRCH_storm7_08_23_Q) = c("valuedatetime","datavalue")
FRCH_storm7_08_23_NO3 = subset(FRCH_storm7_08_23, select = c("DateTime","nitrateuM"))
names(FRCH_storm7_08_23_NO3) = c("valuedatetime","datavalue")
FRCH_storm7_08_23_fDOM = subset(FRCH_storm7_08_23, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm7_08_23_fDOM) = c("valuedatetime","datavalue")
FRCH_storm7_08_23_SPC = subset(FRCH_storm7_08_23, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm7_08_23_SPC) = c("valuedatetime","datavalue")
FRCH_storm7_08_23_turb = subset(FRCH_storm7_08_23, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm7_08_23_turb) = c("valuedatetime","datavalue")

FRCH_storm8a_08_26_Q = subset(FRCH_storm8a_08_26, select = c("DateTime","MeanDischarge"))
names(FRCH_storm8a_08_26_Q) = c("valuedatetime","datavalue")
FRCH_storm8a_08_26_NO3 = subset(FRCH_storm8a_08_26, select = c("DateTime","nitrateuM"))
names(FRCH_storm8a_08_26_NO3) = c("valuedatetime","datavalue")
FRCH_storm8a_08_26_fDOM = subset(FRCH_storm8a_08_26, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm8a_08_26_fDOM) = c("valuedatetime","datavalue")
FRCH_storm8a_08_26_SPC = subset(FRCH_storm8a_08_26, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm8a_08_26_SPC) = c("valuedatetime","datavalue")
FRCH_storm8a_08_26_turb = subset(FRCH_storm8a_08_26, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm8a_08_26_turb) = c("valuedatetime","datavalue")

FRCH_storm8b_08_28_Q = subset(FRCH_storm8b_08_28, select = c("DateTime","MeanDischarge"))
names(FRCH_storm8b_08_28_Q) = c("valuedatetime","datavalue")
FRCH_storm8b_08_28_NO3 = subset(FRCH_storm8b_08_28, select = c("DateTime","nitrateuM"))
names(FRCH_storm8b_08_28_NO3) = c("valuedatetime","datavalue")
FRCH_storm8b_08_28_fDOM = subset(FRCH_storm8b_08_28, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm8b_08_28_fDOM) = c("valuedatetime","datavalue")
FRCH_storm8b_08_28_SPC = subset(FRCH_storm8b_08_28, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm8b_08_28_SPC) = c("valuedatetime","datavalue")
FRCH_storm8b_08_28_turb = subset(FRCH_storm8b_08_28, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm8b_08_28_turb) = c("valuedatetime","datavalue")

FRCH_storm9_08_30_Q = subset(FRCH_storm9_08_30, select = c("DateTime","MeanDischarge"))
names(FRCH_storm9_08_30_Q) = c("valuedatetime","datavalue")
FRCH_storm9_08_30_NO3 = subset(FRCH_storm9_08_30, select = c("DateTime","nitrateuM"))
names(FRCH_storm9_08_30_NO3) = c("valuedatetime","datavalue")
FRCH_storm9_08_30_fDOM = subset(FRCH_storm9_08_30, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm9_08_30_fDOM) = c("valuedatetime","datavalue")
FRCH_storm9_08_30_SPC = subset(FRCH_storm9_08_30, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm9_08_30_SPC) = c("valuedatetime","datavalue")
FRCH_storm9_08_30_turb = subset(FRCH_storm9_08_30, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm9_08_30_turb) = c("valuedatetime","datavalue")

FRCH_storm10_09_01_Q = subset(FRCH_storm10_09_01, select = c("DateTime","MeanDischarge"))
names(FRCH_storm10_09_01_Q) = c("valuedatetime","datavalue")
FRCH_storm10_09_01_NO3 = subset(FRCH_storm10_09_01, select = c("DateTime","nitrateuM"))
names(FRCH_storm10_09_01_NO3) = c("valuedatetime","datavalue")
FRCH_storm10_09_01_fDOM = subset(FRCH_storm10_09_01, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm10_09_01_fDOM) = c("valuedatetime","datavalue")
FRCH_storm10_09_01_SPC = subset(FRCH_storm10_09_01, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm10_09_01_SPC) = c("valuedatetime","datavalue")
FRCH_storm10_09_01_turb = subset(FRCH_storm10_09_01, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm10_09_01_turb) = c("valuedatetime","datavalue")

FRCH_storm11a_09_22_Q = subset(FRCH_storm11a_09_22, select = c("DateTime","MeanDischarge"))
names(FRCH_storm11a_09_22_Q) = c("valuedatetime","datavalue")
FRCH_storm11a_09_22_NO3 = subset(FRCH_storm11a_09_22, select = c("DateTime","nitrateuM"))
names(FRCH_storm11a_09_22_NO3) = c("valuedatetime","datavalue")
FRCH_storm11a_09_22_fDOM = subset(FRCH_storm11a_09_22, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm11a_09_22_fDOM) = c("valuedatetime","datavalue")
FRCH_storm11a_09_22_SPC = subset(FRCH_storm11a_09_22, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm11a_09_22_SPC) = c("valuedatetime","datavalue")
FRCH_storm11a_09_22_turb = subset(FRCH_storm11a_09_22, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm11a_09_22_turb) = c("valuedatetime","datavalue")

FRCH_storm11b_09_24_Q = subset(FRCH_storm11b_09_24, select = c("DateTime","MeanDischarge"))
names(FRCH_storm11b_09_24_Q) = c("valuedatetime","datavalue")
FRCH_storm11b_09_24_NO3 = subset(FRCH_storm11b_09_24, select = c("DateTime","nitrateuM"))
names(FRCH_storm11b_09_24_NO3) = c("valuedatetime","datavalue")
FRCH_storm11b_09_24_fDOM = subset(FRCH_storm11b_09_24, select = c("DateTime","fDOM.QSU"))
names(FRCH_storm11b_09_24_fDOM) = c("valuedatetime","datavalue")
FRCH_storm11b_09_24_SPC = subset(FRCH_storm11b_09_24, select = c("DateTime","SpCond.uScm"))
names(FRCH_storm11b_09_24_SPC) = c("valuedatetime","datavalue")
FRCH_storm11b_09_24_turb = subset(FRCH_storm11b_09_24, select = c("DateTime","Turbidity.FNU"))
names(FRCH_storm11b_09_24_turb) = c("valuedatetime","datavalue")

### Write csv ###
dir.create(file.path("Storm_Events"))
dir.create(file.path("Storm_Events", "2018"))
dir.create(file.path("Storm_Events", "2018", "FRCH"))
dir.create(file.path("Storm_Events", "2018", "MOOS"))
setwd(here("Storm_Events", "2018", "FRCH"))

write.csv(FRCH_storm1_06_21, "FRCH_storm1_06_21.csv")
write.csv(FRCH_storm1_06_21_Q, "FRCH_storm1_06_21_Q.csv")
write.csv(FRCH_storm1_06_21_NO3, "FRCH_storm1_06_21_NO3.csv")
write.csv(FRCH_storm1_06_21_fDOM, "FRCH_storm1_06_21_fDOM.csv")
write.csv(FRCH_storm1_06_21_SPC, "FRCH_storm1_06_21_SPC.csv")
write.csv(FRCH_storm1_06_21_turb, "FRCH_storm1_06_21_Turb.csv")

write.csv(FRCH_storm2a_06_29, "FRCH_storm2a_06_29.csv")
write.csv(FRCH_storm2a_06_29_Q, "FRCH_storm2a_06_29_Q.csv")
write.csv(FRCH_storm2a_06_29_NO3, "FRCH_storm2a_06_29_NO3.csv")
write.csv(FRCH_storm2a_06_29_fDOM, "FRCH_storm2a_06_29_fDOM.csv")
write.csv(FRCH_storm2a_06_29_SPC, "FRCH_storm2a_06_29_SPC.csv")
write.csv(FRCH_storm2a_06_29_turb, "FRCH_storm2a_06_29_Turb.csv")

write.csv(FRCH_storm2b_07_04, "FRCH_storm2b_07_04.csv")
write.csv(FRCH_storm2b_07_04_Q, "FRCH_storm2b_07_04_Q.csv")
write.csv(FRCH_storm2b_07_04_NO3, "FRCH_storm2b_07_04_NO3.csv")
write.csv(FRCH_storm2b_07_04_fDOM, "FRCH_storm2b_07_04_fDOM.csv")
write.csv(FRCH_storm2b_07_04_SPC, "FRCH_storm2b_07_04_SPC.csv")
write.csv(FRCH_storm2b_07_04_turb, "FRCH_storm2b_07_04_Turb.csv")

write.csv(FRCH_storm3_07_10, "FRCH_storm3_07_10.csv")
write.csv(FRCH_storm3_07_10_Q, "FRCH_storm3_07_10_Q.csv")
write.csv(FRCH_storm3_07_10_NO3, "FRCH_storm3_07_10_NO3.csv")
write.csv(FRCH_storm3_07_10_fDOM, "FRCH_storm3_07_10_fDOM.csv")
write.csv(FRCH_storm3_07_10_SPC, "FRCH_storm3_07_10_SPC.csv")
write.csv(FRCH_storm3_07_10_turb, "FRCH_storm3_07_10_Turb.csv")

write.csv(FRCH_storm4a_07_15, "FRCH_storm4a_07_15.csv")
write.csv(FRCH_storm4a_07_15_Q, "FRCH_storm4a_07_15_Q.csv")
write.csv(FRCH_storm4a_07_15_NO3, "FRCH_storm4a_07_15_NO3.csv")
write.csv(FRCH_storm4a_07_15_fDOM, "FRCH_storm4a_07_15_fDOM.csv")
write.csv(FRCH_storm4a_07_15_SPC, "FRCH_storm4a_07_15_SPC.csv")
write.csv(FRCH_storm4a_07_15_turb, "FRCH_storm4a_07_15_Turb.csv")

write.csv(FRCH_storm4b_07_16, "FRCH_storm4b_07_16.csv")
write.csv(FRCH_storm4b_07_16_Q, "FRCH_storm4b_07_16_Q.csv")
write.csv(FRCH_storm4b_07_16_NO3, "FRCH_storm4b_07_16_NO3.csv")
write.csv(FRCH_storm4b_07_16_fDOM, "FRCH_storm4b_07_16_fDOM.csv")
write.csv(FRCH_storm4b_07_16_SPC, "FRCH_storm4b_07_16_SPC.csv")
write.csv(FRCH_storm4b_07_16_turb, "FRCH_storm4b_07_16_Turb.csv")

write.csv(FRCH_storm5_08_04, "FRCH_storm5_08_04.csv")
write.csv(FRCH_storm5_08_04_Q, "FRCH_storm5_08_04_Q.csv")
write.csv(FRCH_storm5_08_04_NO3, "FRCH_storm5_08_04_NO3.csv")
write.csv(FRCH_storm5_08_04_fDOM, "FRCH_storm5_08_04_fDOM.csv")
write.csv(FRCH_storm5_08_04_SPC, "FRCH_storm5_08_04_SPC.csv")
write.csv(FRCH_storm5_08_04_turb, "FRCH_storm5_08_04_Turb.csv")

write.csv(FRCH_storm6_08_13, "FRCH_storm6_08_13.csv")
write.csv(FRCH_storm6_08_13_Q, "FRCH_storm6_08_13_Q.csv")
write.csv(FRCH_storm6_08_13_NO3, "FRCH_storm6_08_13_NO3.csv")
write.csv(FRCH_storm6_08_13_fDOM, "FRCH_storm6_08_13_fDOM.csv")
write.csv(FRCH_storm6_08_13_SPC, "FRCH_storm6_08_13_SPC.csv")
write.csv(FRCH_storm6_08_13_turb, "FRCH_storm6_08_13_Turb.csv")

write.csv(FRCH_storm7_08_23, "FRCH_storm7_08_23.csv")
write.csv(FRCH_storm7_08_23_Q, "FRCH_storm7_08_23_Q.csv")
write.csv(FRCH_storm7_08_23_NO3, "FRCH_storm7_08_23_NO3.csv")
write.csv(FRCH_storm7_08_23_fDOM, "FRCH_storm7_08_23_fDOM.csv")
write.csv(FRCH_storm7_08_23_SPC, "FRCH_storm7_08_23_SPC.csv")
write.csv(FRCH_storm7_08_23_turb, "FRCH_storm7_08_23_Turb.csv")

write.csv(FRCH_storm8a_08_26, "FRCH_storm8a_08_26.csv")
write.csv(FRCH_storm8a_08_26_Q, "FRCH_storm8a_08_26_Q.csv")
write.csv(FRCH_storm8a_08_26_NO3, "FRCH_storm8a_08_26_NO3.csv")
write.csv(FRCH_storm8a_08_26_fDOM, "FRCH_storm8a_08_26_fDOM.csv")
write.csv(FRCH_storm8a_08_26_SPC, "FRCH_storm8a_08_26_SPC.csv")
write.csv(FRCH_storm8a_08_26_turb, "FRCH_storm8a_08_26_Turb.csv")

write.csv(FRCH_storm8b_08_28, "FRCH_storm8b_08_28.csv")
write.csv(FRCH_storm8b_08_28_Q, "FRCH_storm8b_08_28_Q.csv")
write.csv(FRCH_storm8b_08_28_NO3, "FRCH_storm8b_08_28_NO3.csv")
write.csv(FRCH_storm8b_08_28_fDOM, "FRCH_storm8b_08_28_fDOM.csv")
write.csv(FRCH_storm8b_08_28_SPC, "FRCH_storm8b_08_28_SPC.csv")
write.csv(FRCH_storm8b_08_28_turb, "FRCH_storm8b_08_28_Turb.csv")

write.csv(FRCH_storm9_08_30, "FRCH_storm9_08_30.csv")
write.csv(FRCH_storm9_08_30_Q, "FRCH_storm9_08_30_Q.csv")
write.csv(FRCH_storm9_08_30_NO3, "FRCH_storm9_08_30_NO3.csv")
write.csv(FRCH_storm9_08_30_fDOM, "FRCH_storm9_08_30_fDOM.csv")
write.csv(FRCH_storm9_08_30_SPC, "FRCH_storm9_08_30_SPC.csv")
write.csv(FRCH_storm9_08_30_turb, "FRCH_storm9_08_30_Turb.csv")

write.csv(FRCH_storm10_09_01, "FRCH_storm10_09_01.csv")
write.csv(FRCH_storm10_09_01_Q, "FRCH_storm10_09_01_Q.csv")
write.csv(FRCH_storm10_09_01_NO3, "FRCH_storm10_09_01_NO3.csv")
write.csv(FRCH_storm10_09_01_fDOM, "FRCH_storm10_09_01_fDOM.csv")
write.csv(FRCH_storm10_09_01_SPC, "FRCH_storm10_09_01_SPC.csv")
write.csv(FRCH_storm10_09_01_turb, "FRCH_storm10_09_01_Turb.csv")

write.csv(FRCH_storm11a_09_22, "FRCH_storm11a_09_22.csv")
write.csv(FRCH_storm11a_09_22_Q, "FRCH_storm11a_09_22_Q.csv")
write.csv(FRCH_storm11a_09_22_NO3, "FRCH_storm11a_09_22_NO3.csv")
write.csv(FRCH_storm11a_09_22_fDOM, "FRCH_storm11a_09_22_fDOM.csv")
write.csv(FRCH_storm11a_09_22_SPC, "FRCH_storm11a_09_22_SPC.csv")
write.csv(FRCH_storm11a_09_22_turb, "FRCH_storm11a_09_22_Turb.csv")

write.csv(FRCH_storm11b_09_24, "FRCH_storm11b_09_24.csv")
write.csv(FRCH_storm11b_09_24_Q, "FRCH_storm11b_09_24_Q.csv")
write.csv(FRCH_storm11b_09_24_NO3, "FRCH_storm11b_09_24_NO3.csv")
write.csv(FRCH_storm11b_09_24_fDOM, "FRCH_storm11b_09_24_fDOM.csv")
write.csv(FRCH_storm11b_09_24_SPC, "FRCH_storm11b_09_24_SPC.csv")
write.csv(FRCH_storm11b_09_24_turb, "FRCH_storm11b_09_24_Turb.csv")


### Precip Discharge Chem ###
#MOOS#

# rename the columns to what they were to save me a bunch of time going back and changing every call to a column
names(MOOS) <- c("DateTime", "Site", "MeanDischarge", "day",
                 "fDOM.QSU", "SpCond.uScm", "Turbidity.FNU",
                 "nitrateuM")
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-01 0:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'POKE precip. (mm)')
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col="yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col = "green", lwd = 0.1)
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-01 0:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
par(new = T)
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 100 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="blue",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))

# Storm 1 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-21 20:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-06-23 01:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm1_06_21 = MOOS[MOOS$DateTime > as.POSIXct("2018-06-21 20:30:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-06-23 01:45:00", tz="America/Anchorage"),]
plot(MOOS_storm1_06_21$MeanDischarge ~ as.POSIXct(MOOS_storm1_06_21$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,1000), col="blue", main="MOOS 180621 storm 1",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 30 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2a # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-30 04:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-01 17:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm2a_06_30 = MOOS[MOOS$DateTime > as.POSIXct("2018-06-30 04:30:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-07-01 17:45:00", tz="America/Anchorage"),]
plot(MOOS_storm2a_06_30$MeanDischarge ~ as.POSIXct(MOOS_storm2a_06_30$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,3000), col="blue", main="MOOS 180629 storm 2a",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 30 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2b # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-01 17:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-04 18:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm2b_07_01 = MOOS[MOOS$DateTime > as.POSIXct("2018-07-01 17:45:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-07-04 18:45:00", tz="America/Anchorage"),]
plot(MOOS_storm2b_07_01$MeanDischarge ~ as.POSIXct(MOOS_storm2b_07_01$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,3000), col="blue", main="MOOS 180701 storm 2b",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 30 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2c # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-04 23:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-08 18:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm2c_07_04 = MOOS[MOOS$DateTime > as.POSIXct("2018-07-04 23:45:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-07-08 18:45:00", tz="America/Anchorage"),]
plot(MOOS_storm2c_07_04$MeanDischarge ~ as.POSIXct(MOOS_storm2c_07_04$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,3000), col="blue", main="MOOS 180704 storm 2c",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 30 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 3 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-09 23:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-14 18:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm3_07_09 = MOOS[MOOS$DateTime > as.POSIXct("2018-07-09 23:45:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-07-14 18:45:00", tz="America/Anchorage"),]
plot(MOOS_storm3_07_09$MeanDischarge ~ as.POSIXct(MOOS_storm3_07_09$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,2000), col="blue", main="MOOS 180709 storm 3",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-20 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 4 #  Not a storm # 
# I wont save this one in the csv later in the script
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-30 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-15 10:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-20 12:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm4_07_15 = MOOS[MOOS$DateTime > as.POSIXct("2018-07-15 10:45:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-07-20 12:45:00", tz="America/Anchorage"),]
plot(MOOS_storm4_07_15$MeanDischarge ~ as.POSIXct(MOOS_storm4_07_15$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,2000), col="blue", main="MOOS 180715 storm 4",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-30 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))

par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-14 00:00:00","2018-07-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 5 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-30 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-30 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-04 23:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-10 20:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm5_08_04 = MOOS[MOOS$DateTime > as.POSIXct("2018-08-04 23:45:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-08-10 20:45:00", tz="America/Anchorage"),]
plot(MOOS_storm5_08_04$MeanDischarge ~ as.POSIXct(MOOS_storm5_08_04$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(600,2000), col="blue", main="MOOS 180804 storm 5",
     xlim = as.POSIXct(c("2018-07-30 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-30 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 6 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-13 14:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-20 12:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm6_08_13 = MOOS[MOOS$DateTime > as.POSIXct("2018-08-13 14:45:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-08-20 12:45:00", tz="America/Anchorage"),]
plot(MOOS_storm6_08_13$MeanDischarge ~ as.POSIXct(MOOS_storm6_08_13$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,2500), col="blue", main="MOOS 180813 storm 6",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 7 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-23 23:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-26 15:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm7_08_23 = MOOS[MOOS$DateTime > as.POSIXct("2018-08-23 23:45:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-08-26 15:45:00", tz="America/Anchorage"),]
plot(MOOS_storm7_08_23$MeanDischarge ~ as.POSIXct(MOOS_storm7_08_23$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,2500), col="blue", main="MOOS 180823 storm 7",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 8a # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-26 15:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-28 06:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm8a_08_26 = MOOS[MOOS$DateTime > as.POSIXct("2018-08-26 15:45:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-08-28 06:45:00", tz="America/Anchorage"),]
plot(MOOS_storm8a_08_26$MeanDischarge ~ as.POSIXct(MOOS_storm8a_08_26$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,2500), col="blue", main="MOOS 180826 storm 8a",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 8b # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-28 06:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-30 07:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm8b_08_28 = MOOS[MOOS$DateTime > as.POSIXct("2018-08-28 06:45:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-08-30 07:45:00", tz="America/Anchorage"),]
plot(MOOS_storm8b_08_28$MeanDischarge ~ as.POSIXct(MOOS_storm8b_08_28$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(800,2500), col="blue", main="MOOS 180828 storm 8b",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 35 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 9 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-30 07:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-01 12:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm9_08_30 = MOOS[MOOS$DateTime > as.POSIXct("2018-08-30 07:45:00", tz="America/Anchorage") &
                           MOOS$DateTime < as.POSIXct("2018-09-01 12:45:00", tz="America/Anchorage"),]
plot(MOOS_storm9_08_30$MeanDischarge ~ as.POSIXct(MOOS_storm9_08_30$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(1500,2500), col="blue", main="MOOS 180829 storm 9",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 100 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 10 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 8 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-10 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 10 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-01 12:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-10 12:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm10_09_01 = MOOS[MOOS$DateTime > as.POSIXct("2018-09-01 12:45:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-09-10 12:45:00", tz="America/Anchorage"),]
plot(MOOS_storm10_09_01$MeanDischarge ~ as.POSIXct(MOOS_storm10_09_01$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(1500,4500), col="blue", main="MOOS 180901 storm 10",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 50 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 25 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 11a# 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-22 05:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-24 02:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm11a_09_22 = MOOS[MOOS$DateTime > as.POSIXct("2018-09-22 05:45:00", tz="America/Anchorage") &
                             MOOS$DateTime < as.POSIXct("2018-09-24 02:45:00", tz="America/Anchorage"),]
plot(MOOS_storm11a_09_22$MeanDischarge ~ as.POSIXct(MOOS_storm11a_09_22$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(1500,3000), col="blue", main="MOOS 180922 storm 11a",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 50 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 20 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 11b# 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-24 02:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-24 20:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm11b_09_24 = MOOS[MOOS$DateTime > as.POSIXct("2018-09-24 02:45:00", tz="America/Anchorage") &
                             MOOS$DateTime < as.POSIXct("2018-09-24 20:45:00", tz="America/Anchorage"),]
plot(MOOS_storm11b_09_24$MeanDischarge ~ as.POSIXct(MOOS_storm11b_09_24$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(1500,3000), col="blue", main="MOOS 180924 storm 11b",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(MOOS$nitrateuM * 50 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$fDOM.QSU * 15 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$SpCond.uScm * 20 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
lines(MOOS$Turbidity.FNU * 150 ~ MOOS$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2019-05-01 00:00:00","2019-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 12 # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-24 20:45:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-30 23:45:00", tz="America/Anchorage"), col="purple")

MOOS_storm12_09_24 = MOOS[MOOS$DateTime > as.POSIXct("2018-09-24 20:45:00", tz="America/Anchorage") &
                            MOOS$DateTime < as.POSIXct("2018-09-30 23:45:00", tz="America/Anchorage"),]
plot(MOOS_storm12_09_24$MeanDischarge ~ as.POSIXct(MOOS_storm12_09_24$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(1500,3000), col="blue", main="MOOS 180924 storm 12",
     xlim = as.POSIXct(c("2018-09-01 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))

# Alarms but no response in discharge and chem so no storm # 
plot(MOOS$MeanDischarge ~ MOOS$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-30 00:00:00","2018-10-31 23:45:00"), tz="America/Anchorage"))
abline(h=MOOS_bfQ_mn*2, col="red", lty=2)
abline(h=MOOS_bfQ_mn, col="red")

par(new = T)

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-30 00:00:00","2018-10-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)

# modify format and save MOOS storms # 
# nitrateuM, fDOM.QSU 
MOOS_storm1_06_21_Q = subset(MOOS_storm1_06_21, select = c("DateTime","MeanDischarge"))
names(MOOS_storm1_06_21_Q) = c("valuedatetime","datavalue")
MOOS_storm1_06_21_NO3 = subset(MOOS_storm1_06_21, select = c("DateTime","nitrateuM"))
names(MOOS_storm1_06_21_NO3) = c("valuedatetime","datavalue")
MOOS_storm1_06_21_fDOM = subset(MOOS_storm1_06_21, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm1_06_21_fDOM) = c("valuedatetime","datavalue")
MOOS_storm1_06_21_SPC = subset(MOOS_storm1_06_21, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm1_06_21_SPC) = c("valuedatetime","datavalue")
MOOS_storm1_06_21_turb = subset(MOOS_storm1_06_21, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm1_06_21_turb) = c("valuedatetime","datavalue")

MOOS_storm2a_06_30_Q = subset(MOOS_storm2a_06_30, select = c("DateTime","MeanDischarge"))
names(MOOS_storm2a_06_30_Q) = c("valuedatetime","datavalue")
MOOS_storm2a_06_30_NO3 = subset(MOOS_storm2a_06_30, select = c("DateTime","nitrateuM"))
names(MOOS_storm2a_06_30_NO3) = c("valuedatetime","datavalue")
MOOS_storm2a_06_30_fDOM = subset(MOOS_storm2a_06_30, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm2a_06_30_fDOM) = c("valuedatetime","datavalue")
MOOS_storm2a_06_30_SPC = subset(MOOS_storm2a_06_30, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm2a_06_30_SPC) = c("valuedatetime","datavalue")
MOOS_storm2a_06_30_turb = subset(MOOS_storm2a_06_30, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm2a_06_30_turb) = c("valuedatetime","datavalue")

MOOS_storm2b_07_01_Q = subset(MOOS_storm2b_07_01, select = c("DateTime","MeanDischarge"))
names(MOOS_storm2b_07_01_Q) = c("valuedatetime","datavalue")
MOOS_storm2b_07_01_NO3 = subset(MOOS_storm2b_07_01, select = c("DateTime","nitrateuM"))
names(MOOS_storm2b_07_01_NO3) = c("valuedatetime","datavalue")
MOOS_storm2b_07_01_fDOM = subset(MOOS_storm2b_07_01, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm2b_07_01_fDOM) = c("valuedatetime","datavalue")
MOOS_storm2b_07_01_SPC = subset(MOOS_storm2b_07_01, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm2b_07_01_SPC) = c("valuedatetime","datavalue")
MOOS_storm2b_07_01_turb = subset(MOOS_storm2b_07_01, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm2b_07_01_turb) = c("valuedatetime","datavalue")

MOOS_storm2c_07_04_Q = subset(MOOS_storm2c_07_04, select = c("DateTime","MeanDischarge"))
names(MOOS_storm2c_07_04_Q) = c("valuedatetime","datavalue")
MOOS_storm2c_07_04_NO3 = subset(MOOS_storm2c_07_04, select = c("DateTime","nitrateuM"))
names(MOOS_storm2c_07_04_NO3) = c("valuedatetime","datavalue")
MOOS_storm2c_07_04_fDOM = subset(MOOS_storm2c_07_04, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm2c_07_04_fDOM) = c("valuedatetime","datavalue")
MOOS_storm2c_07_04_SPC = subset(MOOS_storm2c_07_04, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm2c_07_04_SPC) = c("valuedatetime","datavalue")
MOOS_storm2c_07_04_turb = subset(MOOS_storm2c_07_04, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm2c_07_04_turb) = c("valuedatetime","datavalue")

MOOS_storm3_07_09_Q = subset(MOOS_storm3_07_09, select = c("DateTime","MeanDischarge"))
names(MOOS_storm3_07_09_Q) = c("valuedatetime","datavalue")
MOOS_storm3_07_09_NO3 = subset(MOOS_storm3_07_09, select = c("DateTime","nitrateuM"))
names(MOOS_storm3_07_09_NO3) = c("valuedatetime","datavalue")
MOOS_storm3_07_09_fDOM = subset(MOOS_storm3_07_09, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm3_07_09_fDOM) = c("valuedatetime","datavalue")
MOOS_storm3_07_09_SPC = subset(MOOS_storm3_07_09, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm3_07_09_SPC) = c("valuedatetime","datavalue")
MOOS_storm3_07_09_turb = subset(MOOS_storm3_07_09, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm3_07_09_turb) = c("valuedatetime","datavalue")

MOOS_storm4_07_15_Q = subset(MOOS_storm4_07_15, select = c("DateTime","MeanDischarge"))
names(MOOS_storm4_07_15_Q) = c("valuedatetime","datavalue")
MOOS_storm4_07_15_NO3 = subset(MOOS_storm4_07_15, select = c("DateTime","nitrateuM"))
names(MOOS_storm4_07_15_NO3) = c("valuedatetime","datavalue")
MOOS_storm4_07_15_fDOM = subset(MOOS_storm4_07_15, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm4_07_15_fDOM) = c("valuedatetime","datavalue")
MOOS_storm4_07_15_SPC = subset(MOOS_storm4_07_15, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm4_07_15_SPC) = c("valuedatetime","datavalue")
MOOS_storm4_07_15_turb = subset(MOOS_storm4_07_15, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm4_07_15_turb) = c("valuedatetime","datavalue")

MOOS_storm5_08_04_Q = subset(MOOS_storm5_08_04, select = c("DateTime","MeanDischarge"))
names(MOOS_storm5_08_04_Q) = c("valuedatetime","datavalue")
MOOS_storm5_08_04_NO3 = subset(MOOS_storm5_08_04, select = c("DateTime","nitrateuM"))
names(MOOS_storm5_08_04_NO3) = c("valuedatetime","datavalue")
MOOS_storm5_08_04_fDOM = subset(MOOS_storm5_08_04, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm5_08_04_fDOM) = c("valuedatetime","datavalue")
MOOS_storm5_08_04_SPC = subset(MOOS_storm5_08_04, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm5_08_04_SPC) = c("valuedatetime","datavalue")
MOOS_storm5_08_04_turb = subset(MOOS_storm5_08_04, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm5_08_04_turb) = c("valuedatetime","datavalue")

MOOS_storm6_08_13_Q = subset(MOOS_storm6_08_13, select = c("DateTime","MeanDischarge"))
names(MOOS_storm6_08_13_Q) = c("valuedatetime","datavalue")
MOOS_storm6_08_13_NO3 = subset(MOOS_storm6_08_13, select = c("DateTime","nitrateuM"))
names(MOOS_storm6_08_13_NO3) = c("valuedatetime","datavalue")
MOOS_storm6_08_13_fDOM = subset(MOOS_storm6_08_13, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm6_08_13_fDOM) = c("valuedatetime","datavalue")
MOOS_storm6_08_13_SPC = subset(MOOS_storm6_08_13, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm6_08_13_SPC) = c("valuedatetime","datavalue")
MOOS_storm6_08_13_turb = subset(MOOS_storm6_08_13, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm6_08_13_turb) = c("valuedatetime","datavalue")

MOOS_storm7_08_23_Q = subset(MOOS_storm7_08_23, select = c("DateTime","MeanDischarge"))
names(MOOS_storm7_08_23_Q) = c("valuedatetime","datavalue")
MOOS_storm7_08_23_NO3 = subset(MOOS_storm7_08_23, select = c("DateTime","nitrateuM"))
names(MOOS_storm7_08_23_NO3) = c("valuedatetime","datavalue")
MOOS_storm7_08_23_fDOM = subset(MOOS_storm7_08_23, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm7_08_23_fDOM) = c("valuedatetime","datavalue")
MOOS_storm7_08_23_SPC = subset(MOOS_storm7_08_23, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm7_08_23_SPC) = c("valuedatetime","datavalue")
MOOS_storm7_08_23_turb = subset(MOOS_storm7_08_23, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm7_08_23_turb) = c("valuedatetime","datavalue")

MOOS_storm8a_08_26_Q = subset(MOOS_storm8a_08_26, select = c("DateTime","MeanDischarge"))
names(MOOS_storm8a_08_26_Q) = c("valuedatetime","datavalue")
MOOS_storm8a_08_26_NO3 = subset(MOOS_storm8a_08_26, select = c("DateTime","nitrateuM"))
names(MOOS_storm8a_08_26_NO3) = c("valuedatetime","datavalue")
MOOS_storm8a_08_26_fDOM = subset(MOOS_storm8a_08_26, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm8a_08_26_fDOM) = c("valuedatetime","datavalue")
MOOS_storm8a_08_26_SPC = subset(MOOS_storm8a_08_26, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm8a_08_26_SPC) = c("valuedatetime","datavalue")
MOOS_storm8a_08_26_turb = subset(MOOS_storm8a_08_26, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm8a_08_26_turb) = c("valuedatetime","datavalue")

MOOS_storm8b_08_28_Q = subset(MOOS_storm8b_08_28, select = c("DateTime","MeanDischarge"))
names(MOOS_storm8b_08_28_Q) = c("valuedatetime","datavalue")
MOOS_storm8b_08_28_NO3 = subset(MOOS_storm8b_08_28, select = c("DateTime","nitrateuM"))
names(MOOS_storm8b_08_28_NO3) = c("valuedatetime","datavalue")
MOOS_storm8b_08_28_fDOM = subset(MOOS_storm8b_08_28, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm8b_08_28_fDOM) = c("valuedatetime","datavalue")
MOOS_storm8b_08_28_SPC = subset(MOOS_storm8b_08_28, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm8b_08_28_SPC) = c("valuedatetime","datavalue")
MOOS_storm8b_08_28_turb = subset(MOOS_storm8b_08_28, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm8b_08_28_turb) = c("valuedatetime","datavalue")

MOOS_storm9_08_30_Q = subset(MOOS_storm9_08_30, select = c("DateTime","MeanDischarge"))
names(MOOS_storm9_08_30_Q) = c("valuedatetime","datavalue")
MOOS_storm9_08_30_NO3 = subset(MOOS_storm9_08_30, select = c("DateTime","nitrateuM"))
names(MOOS_storm9_08_30_NO3) = c("valuedatetime","datavalue")
MOOS_storm9_08_30_fDOM = subset(MOOS_storm9_08_30, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm9_08_30_fDOM) = c("valuedatetime","datavalue")
MOOS_storm9_08_30_SPC = subset(MOOS_storm9_08_30, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm9_08_30_SPC) = c("valuedatetime","datavalue")
MOOS_storm9_08_30_turb = subset(MOOS_storm9_08_30, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm9_08_30_turb) = c("valuedatetime","datavalue")

MOOS_storm10_09_01_Q = subset(MOOS_storm10_09_01, select = c("DateTime","MeanDischarge"))
names(MOOS_storm10_09_01_Q) = c("valuedatetime","datavalue")
MOOS_storm10_09_01_NO3 = subset(MOOS_storm10_09_01, select = c("DateTime","nitrateuM"))
names(MOOS_storm10_09_01_NO3) = c("valuedatetime","datavalue")
MOOS_storm10_09_01_fDOM = subset(MOOS_storm10_09_01, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm10_09_01_fDOM) = c("valuedatetime","datavalue")
MOOS_storm10_09_01_SPC = subset(MOOS_storm10_09_01, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm10_09_01_SPC) = c("valuedatetime","datavalue")
MOOS_storm10_09_01_turb = subset(MOOS_storm10_09_01, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm10_09_01_turb) = c("valuedatetime","datavalue")

MOOS_storm11a_09_22_Q = subset(MOOS_storm11a_09_22, select = c("DateTime","MeanDischarge"))
names(MOOS_storm11a_09_22_Q) = c("valuedatetime","datavalue")
MOOS_storm11a_09_22_NO3 = subset(MOOS_storm11a_09_22, select = c("DateTime","nitrateuM"))
names(MOOS_storm11a_09_22_NO3) = c("valuedatetime","datavalue")
MOOS_storm11a_09_22_fDOM = subset(MOOS_storm11a_09_22, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm11a_09_22_fDOM) = c("valuedatetime","datavalue")
MOOS_storm11a_09_22_SPC = subset(MOOS_storm11a_09_22, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm11a_09_22_SPC) = c("valuedatetime","datavalue")
MOOS_storm11a_09_22_turb = subset(MOOS_storm11a_09_22, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm11a_09_22_turb) = c("valuedatetime","datavalue")

MOOS_storm11b_09_24_Q = subset(MOOS_storm11b_09_24, select = c("DateTime","MeanDischarge"))
names(MOOS_storm11b_09_24_Q) = c("valuedatetime","datavalue")
MOOS_storm11b_09_24_NO3 = subset(MOOS_storm11b_09_24, select = c("DateTime","nitrateuM"))
names(MOOS_storm11b_09_24_NO3) = c("valuedatetime","datavalue")
MOOS_storm11b_09_24_fDOM = subset(MOOS_storm11b_09_24, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm11b_09_24_fDOM) = c("valuedatetime","datavalue")
MOOS_storm11b_09_24_SPC = subset(MOOS_storm11b_09_24, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm11b_09_24_SPC) = c("valuedatetime","datavalue")
MOOS_storm11b_09_24_turb = subset(MOOS_storm11b_09_24, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm11b_09_24_turb) = c("valuedatetime","datavalue")

MOOS_storm12_09_24_Q = subset(MOOS_storm12_09_24, select = c("DateTime","MeanDischarge"))
names(MOOS_storm12_09_24_Q) = c("valuedatetime","datavalue")
MOOS_storm12_09_24_NO3 = subset(MOOS_storm12_09_24, select = c("DateTime","nitrateuM"))
names(MOOS_storm12_09_24_NO3) = c("valuedatetime","datavalue")
MOOS_storm12_09_24_fDOM = subset(MOOS_storm12_09_24, select = c("DateTime","fDOM.QSU"))
names(MOOS_storm12_09_24_fDOM) = c("valuedatetime","datavalue")
MOOS_storm12_09_24_SPC = subset(MOOS_storm12_09_24, select = c("DateTime","SpCond.uScm"))
names(MOOS_storm12_09_24_SPC) = c("valuedatetime","datavalue")
MOOS_storm12_09_24_turb = subset(MOOS_storm12_09_24, select = c("DateTime","Turbidity.FNU"))
names(MOOS_storm12_09_24_turb) = c("valuedatetime","datavalue")

### Write csv ###
setwd(here("Storm_Events", "2018", "MOOS"))
write.csv(MOOS_storm1_06_21, "MOOS_storm1_06_21.csv")
write.csv(MOOS_storm1_06_21_Q, "MOOS_storm1_06_21_Q.csv")
write.csv(MOOS_storm1_06_21_NO3, "MOOS_storm1_06_21_NO3.csv")
write.csv(MOOS_storm1_06_21_fDOM, "MOOS_storm1_06_21_fDOM.csv")
write.csv(MOOS_storm1_06_21_SPC, "MOOS_storm1_06_21_SPC.csv")
write.csv(MOOS_storm1_06_21_turb, "MOOS_storm1_06_21_Turb.csv")

write.csv(MOOS_storm2a_06_30, "MOOS_storm2a_06_30.csv")
write.csv(MOOS_storm2a_06_30_Q, "MOOS_storm2a_06_30_Q.csv")
write.csv(MOOS_storm2a_06_30_NO3, "MOOS_storm2a_06_30_NO3.csv")
write.csv(MOOS_storm2a_06_30_fDOM, "MOOS_storm2a_06_30_fDOM.csv")
write.csv(MOOS_storm2a_06_30_SPC, "MOOS_storm2a_06_30_SPC.csv")
write.csv(MOOS_storm2a_06_30_turb, "MOOS_storm2a_06_30_Turb.csv")

write.csv(MOOS_storm2b_07_01, "MOOS_storm2b_07_01.csv")
write.csv(MOOS_storm2b_07_01_Q, "MOOS_storm2b_07_01_Q.csv")
write.csv(MOOS_storm2b_07_01_NO3, "MOOS_storm2b_07_01_NO3.csv")
write.csv(MOOS_storm2b_07_01_fDOM, "MOOS_storm2b_07_01_fDOM.csv")
write.csv(MOOS_storm2b_07_01_SPC, "MOOS_storm2b_07_01_SPC.csv")
write.csv(MOOS_storm2b_07_01_turb, "MOOS_storm2b_07_01_Turb.csv")

write.csv(MOOS_storm2c_07_04, "MOOS_storm2c_07_04.csv")
write.csv(MOOS_storm2c_07_04_Q, "MOOS_storm2c_07_04_Q.csv")
write.csv(MOOS_storm2c_07_04_NO3, "MOOS_storm2c_07_04_NO3.csv")
write.csv(MOOS_storm2c_07_04_fDOM, "MOOS_storm2c_07_04_fDOM.csv")
write.csv(MOOS_storm2c_07_04_SPC, "MOOS_storm2c_07_04_SPC.csv")
write.csv(MOOS_storm2c_07_04_turb, "MOOS_storm2c_07_04_Turb.csv")

write.csv(MOOS_storm3_07_09, "MOOS_storm3_07_09.csv")
write.csv(MOOS_storm3_07_09_Q, "MOOS_storm3_07_09_Q.csv")
write.csv(MOOS_storm3_07_09_NO3, "MOOS_storm3_07_09_NO3.csv")
write.csv(MOOS_storm3_07_09_fDOM, "MOOS_storm3_07_09_fDOM.csv")
write.csv(MOOS_storm3_07_09_SPC, "MOOS_storm3_07_09_SPC.csv")
write.csv(MOOS_storm3_07_09_turb, "MOOS_storm3_07_09_Turb.csv")

write.csv(MOOS_storm4_07_15, "MOOS_storm4_07_15.csv")
write.csv(MOOS_storm4_07_15_Q, "MOOS_storm4_07_15_Q.csv")
write.csv(MOOS_storm4_07_15_NO3, "MOOS_storm4_07_15_NO3.csv")
write.csv(MOOS_storm4_07_15_fDOM, "MOOS_storm4_07_15_fDOM.csv")
write.csv(MOOS_storm4_07_15_SPC, "MOOS_storm4_07_15_SPC.csv")
write.csv(MOOS_storm4_07_15_turb, "MOOS_storm4_07_15_Turb.csv")

write.csv(MOOS_storm5_08_04, "MOOS_storm5_08_04.csv")
write.csv(MOOS_storm5_08_04_Q, "MOOS_storm5_08_04_Q.csv")
write.csv(MOOS_storm5_08_04_NO3, "MOOS_storm5_08_04_NO3.csv")
write.csv(MOOS_storm5_08_04_fDOM, "MOOS_storm5_08_04_fDOM.csv")
write.csv(MOOS_storm5_08_04_SPC, "MOOS_storm5_08_04_SPC.csv")
write.csv(MOOS_storm5_08_04_turb, "MOOS_storm5_08_04_Turb.csv")

write.csv(MOOS_storm6_08_13, "MOOS_storm6_08_13.csv")
write.csv(MOOS_storm6_08_13_Q, "MOOS_storm6_08_13_Q.csv")
write.csv(MOOS_storm6_08_13_NO3, "MOOS_storm6_08_13_NO3.csv")
write.csv(MOOS_storm6_08_13_fDOM, "MOOS_storm6_08_13_fDOM.csv")
write.csv(MOOS_storm6_08_13_SPC, "MOOS_storm6_08_13_SPC.csv")
write.csv(MOOS_storm6_08_13_turb, "MOOS_storm6_08_13_Turb.csv")

write.csv(MOOS_storm7_08_23, "MOOS_storm7_08_23.csv")
write.csv(MOOS_storm7_08_23_Q, "MOOS_storm7_08_23_Q.csv")
write.csv(MOOS_storm7_08_23_NO3, "MOOS_storm7_08_23_NO3.csv")
write.csv(MOOS_storm7_08_23_fDOM, "MOOS_storm7_08_23_fDOM.csv")
write.csv(MOOS_storm7_08_23_SPC, "MOOS_storm7_08_23_SPC.csv")
write.csv(MOOS_storm7_08_23_turb, "MOOS_storm7_08_23_Turb.csv")

write.csv(MOOS_storm8a_08_26, "MOOS_storm8a_08_26.csv")
write.csv(MOOS_storm8a_08_26_Q, "MOOS_storm8a_08_26_Q.csv")
write.csv(MOOS_storm8a_08_26_NO3, "MOOS_storm8a_08_26_NO3.csv")
write.csv(MOOS_storm8a_08_26_fDOM, "MOOS_storm8a_08_26_fDOM.csv")
write.csv(MOOS_storm8a_08_26_SPC, "MOOS_storm8a_08_26_SPC.csv")
write.csv(MOOS_storm8a_08_26_turb, "MOOS_storm8a_08_26_Turb.csv")

write.csv(MOOS_storm8b_08_28, "MOOS_storm8b_08_28.csv")
write.csv(MOOS_storm8b_08_28_Q, "MOOS_storm8b_08_28_Q.csv")
write.csv(MOOS_storm8b_08_28_NO3, "MOOS_storm8b_08_28_NO3.csv")
write.csv(MOOS_storm8b_08_28_fDOM, "MOOS_storm8b_08_28_fDOM.csv")
write.csv(MOOS_storm8b_08_28_SPC, "MOOS_storm8b_08_28_SPC.csv")
write.csv(MOOS_storm8b_08_28_turb, "MOOS_storm8b_08_28_Turb.csv")

write.csv(MOOS_storm9_08_30, "MOOS_storm9_08_30.csv")
write.csv(MOOS_storm9_08_30_Q, "MOOS_storm9_08_30_Q.csv")
write.csv(MOOS_storm9_08_30_NO3, "MOOS_storm9_08_30_NO3.csv")
write.csv(MOOS_storm9_08_30_fDOM, "MOOS_storm9_08_30_fDOM.csv")
write.csv(MOOS_storm9_08_30_SPC, "MOOS_storm9_08_30_SPC.csv")
write.csv(MOOS_storm9_08_30_turb, "MOOS_storm9_08_30_Turb.csv")

write.csv(MOOS_storm10_09_01, "MOOS_storm10_09_01.csv")
write.csv(MOOS_storm10_09_01_Q, "MOOS_storm10_09_01_Q.csv")
write.csv(MOOS_storm10_09_01_NO3, "MOOS_storm10_09_01_NO3.csv")
write.csv(MOOS_storm10_09_01_fDOM, "MOOS_storm10_09_01_fDOM.csv")
write.csv(MOOS_storm10_09_01_SPC, "MOOS_storm10_09_01_SPC.csv")
write.csv(MOOS_storm10_09_01_turb, "MOOS_storm10_09_01_Turb.csv")

write.csv(MOOS_storm11a_09_22, "MOOS_storm11a_09_22.csv")
write.csv(MOOS_storm11a_09_22_Q, "MOOS_storm11a_09_22_Q.csv")
write.csv(MOOS_storm11a_09_22_NO3, "MOOS_storm11a_09_22_NO3.csv")
write.csv(MOOS_storm11a_09_22_fDOM, "MOOS_storm11a_09_22_fDOM.csv")
write.csv(MOOS_storm11a_09_22_SPC, "MOOS_storm11a_09_22_SPC.csv")
write.csv(MOOS_storm11a_09_22_turb, "MOOS_storm11a_09_22_Turb.csv")

write.csv(MOOS_storm11b_09_24, "MOOS_storm11b_09_24.csv")
write.csv(MOOS_storm11b_09_24_Q, "MOOS_storm11b_09_24_Q.csv")
write.csv(MOOS_storm11b_09_24_NO3, "MOOS_storm11b_09_24_NO3.csv")
write.csv(MOOS_storm11b_09_24_fDOM, "MOOS_storm11b_09_24_fDOM.csv")
write.csv(MOOS_storm11b_09_24_SPC, "MOOS_storm11b_09_24_SPC.csv")
write.csv(MOOS_storm11b_09_24_turb, "MOOS_storm11b_09_24_Turb.csv")

write.csv(MOOS_storm12_09_24, "MOOS_storm12_09_24.csv")
write.csv(MOOS_storm12_09_24_Q, "MOOS_storm12_09_24_Q.csv")
write.csv(MOOS_storm12_09_24_NO3, "MOOS_storm12_09_24_NO3.csv")
write.csv(MOOS_storm12_09_24_fDOM, "MOOS_storm12_09_24_fDOM.csv")
write.csv(MOOS_storm12_09_24_SPC, "MOOS_storm12_09_24_SPC.csv")
write.csv(MOOS_storm12_09_24_turb, "MOOS_storm12_09_24_Turb.csv")


### Precip Discharge Chem ###
#CARI#
setwd("~/Documents/Storms_clean_repo")
CARI_2018 <- CARI.2018 # renaming this to save me lots of time from old script
names(CARI_2018) <- c("DateTime", "Site", "Discharge", "NO3",
                      "fDOM", "SpCond", "Turb", "day")

plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-01 0:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'POKE precip. (mm)')
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col="yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col = "green", lwd = 0.1)
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-01 0:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
par(new = T)
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 00:00:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 10 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 0.5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="blue",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))

# Storm 1 # 
# no chem
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-05-21 01:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-05-22 18:00:00", tz="America/Anchorage"), col="purple")

CARI_storm1_05_21 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-05-21 01:30:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-05-22 18:00:00", tz="America/Anchorage"),]
plot(CARI_storm1_05_21$Discharge ~ as.POSIXct(CARI_storm1_05_21$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(500,1500), col="blue", main="CARI 180521 storm 1",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 10 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 0.5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="blue",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2 # 
# no chem
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-05-23 18:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-05-25 18:00:00", tz="America/Anchorage"), col="purple")

CARI_storm2_05_23 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-05-23 18:30:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-05-25 18:00:00", tz="America/Anchorage"),]
plot(CARI_storm2_05_23$Discharge ~ as.POSIXct(CARI_storm2_05_23$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(500,1500), col="blue", main="CARI 180523 storm 2",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-15 00:00:00","2018-05-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 1 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-05-31 00:00:00","2018-06-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-31 00:00:00","2018-06-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-10 10:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-06-13 18:00:00", tz="America/Anchorage"), col="purple")

CARI_storm1_06_10 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-06-10 10:30:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-06-13 18:00:00", tz="America/Anchorage"),]
plot(CARI_storm1_06_10$Discharge ~ as.POSIXct(CARI_storm1_06_10$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(300,600), col="blue", main="CARI 180610 storm 1",
     xlim = as.POSIXct(c("2018-05-31 00:00:00","2018-06-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-05-31 00:00:00","2018-06-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 2 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-21 20:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-06-23 18:00:00", tz="America/Anchorage"), col="purple")

CARI_storm2_06_21 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-06-21 20:30:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-06-23 18:00:00", tz="America/Anchorage"),]
plot(CARI_storm2_06_21$Discharge ~ as.POSIXct(CARI_storm2_06_21$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,600), col="blue", main="CARI 180621 storm 2",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 3 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-29 06:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-06-30 15:00:00", tz="America/Anchorage"), col="purple")

CARI_storm3_06_29 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-06-29 06:30:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-06-30 15:00:00", tz="America/Anchorage"),]
plot(CARI_storm3_06_29$Discharge ~ as.POSIXct(CARI_storm3_06_29$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,800), col="blue", main="CARI 180629 storm 3",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-15 00:00:00","2018-06-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 4a # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-06-30 15:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-01 12:00:00", tz="America/Anchorage"), col="purple")

CARI_storm4a_06_30 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-06-30 15:30:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-07-01 12:00:00", tz="America/Anchorage"),]
plot(CARI_storm4a_06_30$Discharge ~ as.POSIXct(CARI_storm4a_06_30$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,800), col="blue", main="CARI 180630 storm 4a",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 4b # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-07-01 14:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-07-03 12:00:00", tz="America/Anchorage"), col="purple")

CARI_storm4b_07_01 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-07-01 14:30:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-07-03 12:00:00", tz="America/Anchorage"),]
plot(CARI_storm4b_07_01$Discharge ~ as.POSIXct(CARI_storm4b_07_01$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,1000), col="blue", main="CARI 180701 storm 4b",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-06-30 00:00:00","2018-07-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 5a # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-04 12:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-05 08:00:00", tz="America/Anchorage"), col="purple")

CARI_storm5a_08_04 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-04 12:30:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-08-05 08:00:00", tz="America/Anchorage"),]
plot(CARI_storm5a_08_04$Discharge ~ as.POSIXct(CARI_storm5a_08_04$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,600), col="blue", main="CARI 180804 storm 5a",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 5b # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-05 08:30:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-06 15:00:00", tz="America/Anchorage"), col="purple")

CARI_storm5b_08_05 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-05 08:30:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-08-06 15:00:00", tz="America/Anchorage"),]
plot(CARI_storm5b_08_05$Discharge ~ as.POSIXct(CARI_storm5b_08_05$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,1000), col="blue", main="CARI 180805 storm 5b",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 30 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 5c # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-06 15:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-12 15:00:00", tz="America/Anchorage"), col="purple")

CARI_storm5c_08_06 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-06 15:00:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-08-12 15:00:00", tz="America/Anchorage"),]
plot(CARI_storm5c_08_06$Discharge ~ as.POSIXct(CARI_storm5c_08_06$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,1000), col="blue", main="CARI 180806 storm 5c",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-07-31 00:00:00","2018-08-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 6 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-13 17:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-20 15:00:00", tz="America/Anchorage"), col="purple")

CARI_storm6_08_13 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-13 17:00:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-08-20 15:00:00", tz="America/Anchorage"),]
plot(CARI_storm6_08_13$Discharge ~ as.POSIXct(CARI_storm6_08_13$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,1200), col="blue", main="CARI 180813 storm 6",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 7 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-21 05:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-22 23:00:00", tz="America/Anchorage"), col="purple")

CARI_storm7_08_21 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-21 05:00:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-08-22 23:00:00", tz="America/Anchorage"),]
plot(CARI_storm7_08_21$Discharge ~ as.POSIXct(CARI_storm7_08_21$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,700), col="blue", main="CARI 180821 storm 7",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 8 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-24 12:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-26 12:00:00", tz="America/Anchorage"), col="purple")

CARI_storm8_08_24 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-24 12:00:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-08-26 12:00:00", tz="America/Anchorage"),]
plot(CARI_storm8_08_24$Discharge ~ as.POSIXct(CARI_storm8_08_24$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,700), col="blue", main="CARI 180824 storm 8",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 9 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-26 12:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-08-30 06:00:00", tz="America/Anchorage"), col="purple")

CARI_storm9_08_26 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-26 12:00:00", tz="America/Anchorage") &
                                CARI_2018$DateTime < as.POSIXct("2018-08-30 06:00:00", tz="America/Anchorage"),]
plot(CARI_storm9_08_26$Discharge ~ as.POSIXct(CARI_storm9_08_26$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(200,700), col="blue", main="CARI 180826 storm 9",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-12 00:00:00","2018-08-31 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 10 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-08-30 10:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-02 03:00:00", tz="America/Anchorage"), col="purple")

CARI_storm10_08_30 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-08-30 10:00:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-09-02 03:00:00", tz="America/Anchorage"),]
plot(CARI_storm10_08_30$Discharge ~ as.POSIXct(CARI_storm10_08_30$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,1500), col="blue", main="CARI 180830 storm 10",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 11 # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-02 03:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-04 03:00:00", tz="America/Anchorage"), col="purple")

CARI_storm11_09_02 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-09-02 03:00:00", tz="America/Anchorage") &
                                 CARI_2018$DateTime < as.POSIXct("2018-09-04 03:00:00", tz="America/Anchorage"),]
plot(CARI_storm11_09_02$Discharge ~ as.POSIXct(CARI_storm11_09_02$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,1000), col="blue", main="CARI 180902 storm 11",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-08-30 00:00:00","2018-09-15 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 12a # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-20 23:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-25 08:00:00", tz="America/Anchorage"), col="purple")

CARI_storm12a_09_20 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-09-20 23:00:00", tz="America/Anchorage") &
                                  CARI_2018$DateTime < as.POSIXct("2018-09-25 08:00:00", tz="America/Anchorage"),]
plot(CARI_storm12a_09_20$Discharge ~ as.POSIXct(CARI_storm12a_09_20$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,1500), col="blue", main="CARI 180920 storm 12a",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# Storm 12b # 
plot(CARI_2018$Discharge ~ CARI_2018$DateTime, type="l", xlab="", ylab="Q (L/sec)",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
abline(h=CARI_bfQ_mn*2, col="red", lty=2)
abline(h=CARI_bfQ_mn, col="red")
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)
mtext(side = 4, line = 3, 'CRREL Met Station precip. (mm)') 
abline(v = as.POSIXct(poke.five.fourty.eight$DateTime), col = "yellow", lwd = 0.1)
abline(v = as.POSIXct(poke.five.twenty.four$DateTime), col="green", lwd = 0.1)
abline(v= as.POSIXct("2018-09-25 09:00:00", tz="America/Anchorage"), col="purple")
abline(v= as.POSIXct("2018-09-30 20:00:00", tz="America/Anchorage"), col="purple")

CARI_storm12b_09_25 = CARI_2018[CARI_2018$DateTime > as.POSIXct("2018-09-25 09:00:00", tz="America/Anchorage") &
                                  CARI_2018$DateTime < as.POSIXct("2018-09-30 20:00:00", tz="America/Anchorage"),]
plot(CARI_storm12b_09_25$Discharge ~ as.POSIXct(CARI_storm12b_09_25$DateTime, tz="America/Anchorage"), type="l", xlab="", ylab="Q (L/sec)",ylim = c(400,1000), col="blue", main="CARI 180925 storm 12b",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"))
lines(CARI_2018$NO3 * 20 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="purple",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$fDOM * 7 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="brown",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$SpCond * 5 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="red",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
lines(CARI_2018$Turb * 300 ~ CARI_2018$DateTime, type="l", xlab="", ylab="", col="black",
      xlim = as.POSIXct(c("2018-05-01 00:00:00","2018-10-15 01:00:00"), tz="America/Anchorage"))
par(new = T)
plot(POKE.st$inst_rainfall_mm ~ POKE.st$DateTime, type="h",
     xlim = as.POSIXct(c("2018-09-15 00:00:00","2018-09-30 23:45:00"), tz="America/Anchorage"),
     ylim = c(10,0), 
     axes=F, xlab="", ylab="")
axis(side = 4)

# fDOM NO3 SPC Turb # 
CARI_storm1_06_10_Q = subset(CARI_storm1_06_10, select = c("DateTime","Discharge"))
names(CARI_storm1_06_10_Q) = c("valuedatetime","datavalue")
CARI_storm1_06_10_NO3 = subset(CARI_storm1_06_10, select = c("DateTime","NO3"))
names(CARI_storm1_06_10_NO3) = c("valuedatetime","datavalue")
CARI_storm1_06_10_fDOM = subset(CARI_storm1_06_10, select = c("DateTime","fDOM"))
names(CARI_storm1_06_10_fDOM) = c("valuedatetime","datavalue")
CARI_storm1_06_10_SPC = subset(CARI_storm1_06_10, select = c("DateTime","SpCond"))
names(CARI_storm1_06_10_SPC) = c("valuedatetime","datavalue")
CARI_storm1_06_10_turb = subset(CARI_storm1_06_10, select = c("DateTime","Turb"))
names(CARI_storm1_06_10_turb) = c("valuedatetime","datavalue")


CARI_storm2_06_21_Q = subset(CARI_storm2_06_21, select = c("DateTime","Discharge"))
names(CARI_storm2_06_21_Q) = c("valuedatetime","datavalue")
CARI_storm2_06_21_NO3 = subset(CARI_storm2_06_21, select = c("DateTime","NO3"))
names(CARI_storm2_06_21_NO3) = c("valuedatetime","datavalue")
CARI_storm2_06_21_fDOM = subset(CARI_storm2_06_21, select = c("DateTime","fDOM"))
names(CARI_storm2_06_21_fDOM) = c("valuedatetime","datavalue")
CARI_storm2_06_21_SPC = subset(CARI_storm2_06_21, select = c("DateTime","SpCond"))
names(CARI_storm2_06_21_SPC) = c("valuedatetime","datavalue")
CARI_storm2_06_21_turb = subset(CARI_storm2_06_21, select = c("DateTime","Turb"))
names(CARI_storm2_06_21_turb) = c("valuedatetime","datavalue")

CARI_storm3_06_29_Q = subset(CARI_storm3_06_29, select = c("DateTime","Discharge"))
names(CARI_storm3_06_29_Q) = c("valuedatetime","datavalue")
CARI_storm3_06_29_NO3 = subset(CARI_storm3_06_29, select = c("DateTime","NO3"))
names(CARI_storm3_06_29_NO3) = c("valuedatetime","datavalue")
CARI_storm3_06_29_fDOM = subset(CARI_storm3_06_29, select = c("DateTime","fDOM"))
names(CARI_storm3_06_29_fDOM) = c("valuedatetime","datavalue")
CARI_storm3_06_29_SPC = subset(CARI_storm3_06_29, select = c("DateTime","SpCond"))
names(CARI_storm3_06_29_SPC) = c("valuedatetime","datavalue")
CARI_storm3_06_29_turb = subset(CARI_storm3_06_29, select = c("DateTime","Turb"))
names(CARI_storm3_06_29_turb) = c("valuedatetime","datavalue")

CARI_storm4a_06_30_Q = subset(CARI_storm4a_06_30, select = c("DateTime","Discharge"))
names(CARI_storm4a_06_30_Q) = c("valuedatetime","datavalue")
CARI_storm4a_06_30_NO3 = subset(CARI_storm4a_06_30, select = c("DateTime","NO3"))
names(CARI_storm4a_06_30_NO3) = c("valuedatetime","datavalue")
CARI_storm4a_06_30_fDOM = subset(CARI_storm4a_06_30, select = c("DateTime","fDOM"))
names(CARI_storm4a_06_30_fDOM) = c("valuedatetime","datavalue")
CARI_storm4a_06_30_SPC = subset(CARI_storm4a_06_30, select = c("DateTime","SpCond"))
names(CARI_storm4a_06_30_SPC) = c("valuedatetime","datavalue")
CARI_storm4a_06_30_turb = subset(CARI_storm4a_06_30, select = c("DateTime","Turb"))
names(CARI_storm4a_06_30_turb) = c("valuedatetime","datavalue")

CARI_storm4b_07_01_Q = subset(CARI_storm4b_07_01, select = c("DateTime","Discharge"))
names(CARI_storm4b_07_01_Q) = c("valuedatetime","datavalue")
CARI_storm4b_07_01_NO3 = subset(CARI_storm4b_07_01, select = c("DateTime","NO3"))
names(CARI_storm4b_07_01_NO3) = c("valuedatetime","datavalue")
CARI_storm4b_07_01_fDOM = subset(CARI_storm4b_07_01, select = c("DateTime","fDOM"))
names(CARI_storm4b_07_01_fDOM) = c("valuedatetime","datavalue")
CARI_storm4b_07_01_SPC = subset(CARI_storm4b_07_01, select = c("DateTime","SpCond"))
names(CARI_storm4b_07_01_SPC) = c("valuedatetime","datavalue")
CARI_storm4b_07_01_turb = subset(CARI_storm4b_07_01, select = c("DateTime","Turb"))
names(CARI_storm4b_07_01_turb) = c("valuedatetime","datavalue")

CARI_storm5a_08_04_Q = subset(CARI_storm5a_08_04, select = c("DateTime","Discharge"))
names(CARI_storm5a_08_04_Q) = c("valuedatetime","datavalue")
CARI_storm5a_08_04_NO3 = subset(CARI_storm5a_08_04, select = c("DateTime","NO3"))
names(CARI_storm5a_08_04_NO3) = c("valuedatetime","datavalue")
CARI_storm5a_08_04_fDOM = subset(CARI_storm5a_08_04, select = c("DateTime","fDOM"))
names(CARI_storm5a_08_04_fDOM) = c("valuedatetime","datavalue")
CARI_storm5a_08_04_SPC = subset(CARI_storm5a_08_04, select = c("DateTime","SpCond"))
names(CARI_storm5a_08_04_SPC) = c("valuedatetime","datavalue")
CARI_storm5a_08_04_turb = subset(CARI_storm5a_08_04, select = c("DateTime","Turb"))
names(CARI_storm5a_08_04_turb) = c("valuedatetime","datavalue")

CARI_storm5b_08_05_Q = subset(CARI_storm5b_08_05, select = c("DateTime","Discharge"))
names(CARI_storm5b_08_05_Q) = c("valuedatetime","datavalue")
CARI_storm5b_08_05_NO3 = subset(CARI_storm5b_08_05, select = c("DateTime","NO3"))
names(CARI_storm5b_08_05_NO3) = c("valuedatetime","datavalue")
CARI_storm5b_08_05_fDOM = subset(CARI_storm5b_08_05, select = c("DateTime","fDOM"))
names(CARI_storm5b_08_05_fDOM) = c("valuedatetime","datavalue")
CARI_storm5b_08_05_SPC = subset(CARI_storm5b_08_05, select = c("DateTime","SpCond"))
names(CARI_storm5b_08_05_SPC) = c("valuedatetime","datavalue")
CARI_storm5b_08_05_turb = subset(CARI_storm5b_08_05, select = c("DateTime","Turb"))
names(CARI_storm5b_08_05_turb) = c("valuedatetime","datavalue")

CARI_storm5c_08_06_Q = subset(CARI_storm5c_08_06, select = c("DateTime","Discharge"))
names(CARI_storm5c_08_06_Q) = c("valuedatetime","datavalue")
CARI_storm5c_08_06_NO3 = subset(CARI_storm5c_08_06, select = c("DateTime","NO3"))
names(CARI_storm5c_08_06_NO3) = c("valuedatetime","datavalue")
CARI_storm5c_08_06_fDOM = subset(CARI_storm5c_08_06, select = c("DateTime","fDOM"))
names(CARI_storm5c_08_06_fDOM) = c("valuedatetime","datavalue")
CARI_storm5c_08_06_SPC = subset(CARI_storm5c_08_06, select = c("DateTime","SpCond"))
names(CARI_storm5c_08_06_SPC) = c("valuedatetime","datavalue")
CARI_storm5c_08_06_turb = subset(CARI_storm5c_08_06, select = c("DateTime","Turb"))
names(CARI_storm5c_08_06_turb) = c("valuedatetime","datavalue")

CARI_storm6_08_13_Q = subset(CARI_storm6_08_13, select = c("DateTime","Discharge"))
names(CARI_storm6_08_13_Q) = c("valuedatetime","datavalue")
CARI_storm6_08_13_NO3 = subset(CARI_storm6_08_13, select = c("DateTime","NO3"))
names(CARI_storm6_08_13_NO3) = c("valuedatetime","datavalue")
CARI_storm6_08_13_fDOM = subset(CARI_storm6_08_13, select = c("DateTime","fDOM"))
names(CARI_storm6_08_13_fDOM) = c("valuedatetime","datavalue")
CARI_storm6_08_13_SPC = subset(CARI_storm6_08_13, select = c("DateTime","SpCond"))
names(CARI_storm6_08_13_SPC) = c("valuedatetime","datavalue")
CARI_storm6_08_13_turb = subset(CARI_storm6_08_13, select = c("DateTime","Turb"))
names(CARI_storm6_08_13_turb) = c("valuedatetime","datavalue")

CARI_storm7_08_21_Q = subset(CARI_storm7_08_21, select = c("DateTime","Discharge"))
names(CARI_storm7_08_21_Q) = c("valuedatetime","datavalue")
CARI_storm7_08_21_NO3 = subset(CARI_storm7_08_21, select = c("DateTime","NO3"))
names(CARI_storm7_08_21_NO3) = c("valuedatetime","datavalue")
CARI_storm7_08_21_fDOM = subset(CARI_storm7_08_21, select = c("DateTime","fDOM"))
names(CARI_storm7_08_21_fDOM) = c("valuedatetime","datavalue")
CARI_storm7_08_21_SPC = subset(CARI_storm7_08_21, select = c("DateTime","SpCond"))
names(CARI_storm7_08_21_SPC) = c("valuedatetime","datavalue")
CARI_storm7_08_21_turb = subset(CARI_storm7_08_21, select = c("DateTime","Turb"))
names(CARI_storm7_08_21_turb) = c("valuedatetime","datavalue")

CARI_storm8_08_24_Q = subset(CARI_storm8_08_24, select = c("DateTime","Discharge"))
names(CARI_storm8_08_24_Q) = c("valuedatetime","datavalue")
CARI_storm8_08_24_NO3 = subset(CARI_storm8_08_24, select = c("DateTime","NO3"))
names(CARI_storm8_08_24_NO3) = c("valuedatetime","datavalue")
CARI_storm8_08_24_fDOM = subset(CARI_storm8_08_24, select = c("DateTime","fDOM"))
names(CARI_storm8_08_24_fDOM) = c("valuedatetime","datavalue")
CARI_storm8_08_24_SPC = subset(CARI_storm8_08_24, select = c("DateTime","SpCond"))
names(CARI_storm8_08_24_SPC) = c("valuedatetime","datavalue")
CARI_storm8_08_24_turb = subset(CARI_storm8_08_24, select = c("DateTime","Turb"))
names(CARI_storm8_08_24_turb) = c("valuedatetime","datavalue")

CARI_storm9_08_26_Q = subset(CARI_storm9_08_26, select = c("DateTime","Discharge"))
names(CARI_storm9_08_26_Q) = c("valuedatetime","datavalue")
CARI_storm9_08_26_NO3 = subset(CARI_storm9_08_26, select = c("DateTime","NO3"))
names(CARI_storm9_08_26_NO3) = c("valuedatetime","datavalue")
CARI_storm9_08_26_fDOM = subset(CARI_storm9_08_26, select = c("DateTime","fDOM"))
names(CARI_storm9_08_26_fDOM) = c("valuedatetime","datavalue")
CARI_storm9_08_26_SPC = subset(CARI_storm9_08_26, select = c("DateTime","SpCond"))
names(CARI_storm9_08_26_SPC) = c("valuedatetime","datavalue")
CARI_storm9_08_26_turb = subset(CARI_storm9_08_26, select = c("DateTime","Turb"))
names(CARI_storm9_08_26_turb) = c("valuedatetime","datavalue")

CARI_storm10_08_30_Q = subset(CARI_storm10_08_30, select = c("DateTime","Discharge"))
names(CARI_storm10_08_30_Q) = c("valuedatetime","datavalue")
CARI_storm10_08_30_NO3 = subset(CARI_storm10_08_30, select = c("DateTime","NO3"))
names(CARI_storm10_08_30_NO3) = c("valuedatetime","datavalue")
CARI_storm10_08_30_fDOM = subset(CARI_storm10_08_30, select = c("DateTime","fDOM"))
names(CARI_storm10_08_30_fDOM) = c("valuedatetime","datavalue")
CARI_storm10_08_30_SPC = subset(CARI_storm10_08_30, select = c("DateTime","SpCond"))
names(CARI_storm10_08_30_SPC) = c("valuedatetime","datavalue")
CARI_storm10_08_30_turb = subset(CARI_storm10_08_30, select = c("DateTime","Turb"))
names(CARI_storm10_08_30_turb) = c("valuedatetime","datavalue")

CARI_storm11_09_02_Q = subset(CARI_storm11_09_02, select = c("DateTime","Discharge"))
names(CARI_storm11_09_02_Q) = c("valuedatetime","datavalue")
CARI_storm11_09_02_NO3 = subset(CARI_storm11_09_02, select = c("DateTime","NO3"))
names(CARI_storm11_09_02_NO3) = c("valuedatetime","datavalue")
CARI_storm11_09_02_fDOM = subset(CARI_storm11_09_02, select = c("DateTime","fDOM"))
names(CARI_storm11_09_02_fDOM) = c("valuedatetime","datavalue")
CARI_storm11_09_02_SPC = subset(CARI_storm11_09_02, select = c("DateTime","SpCond"))
names(CARI_storm11_09_02_SPC) = c("valuedatetime","datavalue")
CARI_storm11_09_02_turb = subset(CARI_storm11_09_02, select = c("DateTime","Turb"))
names(CARI_storm11_09_02_turb) = c("valuedatetime","datavalue")

CARI_storm12a_09_20_Q = subset(CARI_storm12a_09_20, select = c("DateTime","Discharge"))
names(CARI_storm12a_09_20_Q) = c("valuedatetime","datavalue")
CARI_storm12a_09_20_NO3 = subset(CARI_storm12a_09_20, select = c("DateTime","NO3"))
names(CARI_storm12a_09_20_NO3) = c("valuedatetime","datavalue")
CARI_storm12a_09_20_fDOM = subset(CARI_storm12a_09_20, select = c("DateTime","fDOM"))
names(CARI_storm12a_09_20_fDOM) = c("valuedatetime","datavalue")
CARI_storm12a_09_20_SPC = subset(CARI_storm12a_09_20, select = c("DateTime","SpCond"))
names(CARI_storm12a_09_20_SPC) = c("valuedatetime","datavalue")
CARI_storm12a_09_20_turb = subset(CARI_storm12a_09_20, select = c("DateTime","Turb"))
names(CARI_storm12a_09_20_turb) = c("valuedatetime","datavalue")

CARI_storm12b_09_25_Q = subset(CARI_storm12b_09_25, select = c("DateTime","Discharge"))
names(CARI_storm12b_09_25_Q) = c("valuedatetime","datavalue")
CARI_storm12b_09_25_NO3 = subset(CARI_storm12b_09_25, select = c("DateTime","NO3"))
names(CARI_storm12b_09_25_NO3) = c("valuedatetime","datavalue")
CARI_storm12b_09_25_fDOM = subset(CARI_storm12b_09_25, select = c("DateTime","fDOM"))
names(CARI_storm12b_09_25_fDOM) = c("valuedatetime","datavalue")
CARI_storm12b_09_25_SPC = subset(CARI_storm12b_09_25, select = c("DateTime","SpCond"))
names(CARI_storm12b_09_25_SPC) = c("valuedatetime","datavalue")
CARI_storm12b_09_25_turb = subset(CARI_storm12b_09_25, select = c("DateTime","Turb"))
names(CARI_storm12b_09_25_turb) = c("valuedatetime","datavalue")

#Write csv #
dir.create(file.path("Storm_Events", "2018", "CARI"))
setwd(here("Storm_Events", "2018", "CARI"))

write.csv(CARI_storm1_06_10, "CARI_storm1_06_10.csv")
write.csv(CARI_storm1_06_10_Q, "CARI_storm1_06_10_Q.csv")
write.csv(CARI_storm1_06_10_NO3, "CARI_storm1_06_10_NO3.csv")
write.csv(CARI_storm1_06_10_fDOM, "CARI_storm1_06_10_fDOM.csv")
write.csv(CARI_storm1_06_10_SPC, "CARI_storm1_06_10_SPC.csv")
write.csv(CARI_storm1_06_10_turb, "CARI_storm1_06_10_Turb.csv")

write.csv(CARI_storm2_06_21, "CARI_storm2_06_21.csv")
write.csv(CARI_storm2_06_21_Q, "CARI_storm2_06_21_Q.csv")
write.csv(CARI_storm2_06_21_NO3, "CARI_storm2_06_21_NO3.csv")
write.csv(CARI_storm2_06_21_fDOM, "CARI_storm2_06_21_fDOM.csv")
write.csv(CARI_storm2_06_21_SPC, "CARI_storm2_06_21_SPC.csv")
write.csv(CARI_storm2_06_21_turb, "CARI_storm2_06_21_Turb.csv")

write.csv(CARI_storm3_06_29, "CARI_storm3_06_29.csv")
write.csv(CARI_storm3_06_29_Q, "CARI_storm3_06_29_Q.csv")
write.csv(CARI_storm3_06_29_NO3, "CARI_storm3_06_29_NO3.csv")
write.csv(CARI_storm3_06_29_fDOM, "CARI_storm3_06_29_fDOM.csv")
write.csv(CARI_storm3_06_29_SPC, "CARI_storm3_06_29_SPC.csv")
write.csv(CARI_storm3_06_29_turb, "CARI_storm3_06_29_Turb.csv")

write.csv(CARI_storm4a_06_30, "CARI_storm4a_06_30.csv")
write.csv(CARI_storm4a_06_30_Q, "CARI_storm4a_06_30_Q.csv")
write.csv(CARI_storm4a_06_30_NO3, "CARI_storm4a_06_30_NO3.csv")
write.csv(CARI_storm4a_06_30_fDOM, "CARI_storm4a_06_30_fDOM.csv")
write.csv(CARI_storm4a_06_30_SPC, "CARI_storm4a_06_30_SPC.csv")
write.csv(CARI_storm4a_06_30_turb, "CARI_storm4a_06_30_Turb.csv")

write.csv(CARI_storm4b_07_01, "CARI_storm4b_07_01.csv")
write.csv(CARI_storm4b_07_01_Q, "CARI_storm4b_07_01_Q.csv")
write.csv(CARI_storm4b_07_01_NO3, "CARI_storm4b_07_01_NO3.csv")
write.csv(CARI_storm4b_07_01_fDOM, "CARI_storm4b_07_01_fDOM.csv")
write.csv(CARI_storm4b_07_01_SPC, "CARI_storm4b_07_01_SPC.csv")
write.csv(CARI_storm4b_07_01_turb, "CARI_storm4b_07_01_Turb.csv")

write.csv(CARI_storm5a_08_04, "CARI_storm5a_08_04.csv")
write.csv(CARI_storm5a_08_04_Q, "CARI_storm5a_08_04_Q.csv")
write.csv(CARI_storm5a_08_04_NO3, "CARI_storm5a_08_04_NO3.csv")
write.csv(CARI_storm5a_08_04_fDOM, "CARI_storm5a_08_04_fDOM.csv")
write.csv(CARI_storm5a_08_04_SPC, "CARI_storm5a_08_04_SPC.csv")
write.csv(CARI_storm5a_08_04_turb, "CARI_storm5a_08_04_Turb.csv")

write.csv(CARI_storm5b_08_05, "CARI_storm5b_08_05.csv")
write.csv(CARI_storm5b_08_05_Q, "CARI_storm5b_08_05_Q.csv")
write.csv(CARI_storm5b_08_05_NO3, "CARI_storm5b_08_05_NO3.csv")
write.csv(CARI_storm5b_08_05_fDOM, "CARI_storm5b_08_05_fDOM.csv")
write.csv(CARI_storm5b_08_05_SPC, "CARI_storm5b_08_05_SPC.csv")
write.csv(CARI_storm5b_08_05_turb, "CARI_storm5b_08_05_Turb.csv")

write.csv(CARI_storm5c_08_06, "CARI_storm5c_08_06.csv")
write.csv(CARI_storm5c_08_06_Q, "CARI_storm5c_08_06_Q.csv")
write.csv(CARI_storm5c_08_06_NO3, "CARI_storm5c_08_06_NO3.csv")
write.csv(CARI_storm5c_08_06_fDOM, "CARI_storm5c_08_06_fDOM.csv")
write.csv(CARI_storm5c_08_06_SPC, "CARI_storm5c_08_06_SPC.csv")
write.csv(CARI_storm5c_08_06_turb, "CARI_storm5c_08_06_Turb.csv")

write.csv(CARI_storm6_08_13, "CARI_storm6_08_13.csv")
write.csv(CARI_storm6_08_13_Q, "CARI_storm6_08_13_Q.csv")
write.csv(CARI_storm6_08_13_NO3, "CARI_storm6_08_13_NO3.csv")
write.csv(CARI_storm6_08_13_fDOM, "CARI_storm6_08_13_fDOM.csv")
write.csv(CARI_storm6_08_13_SPC, "CARI_storm6_08_13_SPC.csv")
write.csv(CARI_storm6_08_13_turb, "CARI_storm6_08_13_Turb.csv")

write.csv(CARI_storm7_08_21, "CARI_storm7_08_21.csv")
write.csv(CARI_storm7_08_21_Q, "CARI_storm7_08_21_Q.csv")
write.csv(CARI_storm7_08_21_NO3, "CARI_storm7_08_21_NO3.csv")
write.csv(CARI_storm7_08_21_fDOM, "CARI_storm7_08_21_fDOM.csv")
write.csv(CARI_storm7_08_21_SPC, "CARI_storm7_08_21_SPC.csv")
write.csv(CARI_storm7_08_21_turb, "CARI_storm7_08_21_Turb.csv")

write.csv(CARI_storm8_08_24, "CARI_storm8_08_24.csv")
write.csv(CARI_storm8_08_24_Q, "CARI_storm8_08_24_Q.csv")
write.csv(CARI_storm8_08_24_NO3, "CARI_storm8_08_24_NO3.csv")
write.csv(CARI_storm8_08_24_fDOM, "CARI_storm8_08_24_fDOM.csv")
write.csv(CARI_storm8_08_24_SPC, "CARI_storm8_08_24_SPC.csv")
write.csv(CARI_storm8_08_24_turb, "CARI_storm8_08_24_Turb.csv")

write.csv(CARI_storm9_08_26, "CARI_storm9_08_26.csv")
write.csv(CARI_storm9_08_26_Q, "CARI_storm9_08_26_Q.csv")
write.csv(CARI_storm9_08_26_NO3, "CARI_storm9_08_26_NO3.csv")
write.csv(CARI_storm9_08_26_fDOM, "CARI_storm9_08_26_fDOM.csv")
write.csv(CARI_storm9_08_26_SPC, "CARI_storm9_08_26_SPC.csv")
write.csv(CARI_storm9_08_26_turb, "CARI_storm9_08_26_Turb.csv")

write.csv(CARI_storm10_08_30, "CARI_storm10_08_30.csv")
write.csv(CARI_storm10_08_30_Q, "CARI_storm10_08_30_Q.csv")
write.csv(CARI_storm10_08_30_NO3, "CARI_storm10_08_30_NO3.csv")
write.csv(CARI_storm10_08_30_fDOM, "CARI_storm10_08_30_fDOM.csv")
write.csv(CARI_storm10_08_30_SPC, "CARI_storm10_08_30_SPC.csv")
write.csv(CARI_storm10_08_30_turb, "CARI_storm10_08_30_Turb.csv")

write.csv(CARI_storm11_09_02, "CARI_storm11_09_02.csv")
write.csv(CARI_storm11_09_02_Q, "CARI_storm11_09_02_Q.csv")
write.csv(CARI_storm11_09_02_NO3, "CARI_storm11_09_02_NO3.csv")
write.csv(CARI_storm11_09_02_fDOM, "CARI_storm11_09_02_fDOM.csv")
write.csv(CARI_storm11_09_02_SPC, "CARI_storm11_09_02_SPC.csv")
write.csv(CARI_storm11_09_02_turb, "CARI_storm11_09_02_Turb.csv")

write.csv(CARI_storm12a_09_20, "CARI_storm12a_09_20.csv")
write.csv(CARI_storm12a_09_20_Q, "CARI_storm12a_09_20_Q.csv")
write.csv(CARI_storm12a_09_20_NO3, "CARI_storm12a_09_20_NO3.csv")
write.csv(CARI_storm12a_09_20_fDOM, "CARI_storm12a_09_20_fDOM.csv")
write.csv(CARI_storm12a_09_20_SPC, "CARI_storm12a_09_20_SPC.csv")
write.csv(CARI_storm12a_09_20_turb, "CARI_storm12a_09_20_Turb.csv")

write.csv(CARI_storm12b_09_25, "CARI_storm12b_09_25.csv")
write.csv(CARI_storm12b_09_25_Q, "CARI_storm12b_09_25_Q.csv")
write.csv(CARI_storm12b_09_25_NO3, "CARI_storm12b_09_25_NO3.csv")
write.csv(CARI_storm12b_09_25_fDOM, "CARI_storm12b_09_25_fDOM.csv")
write.csv(CARI_storm12b_09_25_SPC, "CARI_storm12b_09_25_SPC.csv")
write.csv(CARI_storm12b_09_25_turb, "CARI_storm12b_09_25_Turb.csv")

# add burst data to storm data #

# load storm data #
setwd("~/Documents/Storms_clean_repo")
storm_file_list <- list.files(path="Storm_Events/2018/FRCH_MOOS_CARI/", 
                              recursive=F, 
                              pattern=".csv", 
                              full.names=TRUE)

storm_list<-do.call("list", lapply(storm_file_list, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                   header=T, row.names=1))

storm_file_list = sub("~/Documents/Storms_clean_repo/Storm_Events/2018/FRCH_MOOS_CARI//", storm_file_list, replacement = "")
storm_file_list = sub(".csv", storm_file_list, replacement = "")
names(storm_list) = storm_file_list

for(i in 1:length(storm_list)){
  storm_list[[i]][["valuedatetime"]] = as.POSIXct(storm_list[[i]][["valuedatetime"]],
                                                  "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 

#  organize storm data by site and solute #
CARI_storm_list = storm_list[c(1:80)] # 80
FRCH_storm_list = storm_list[c(81:155)] # 75
MOOS_storm_list = storm_list[c(156:235)] # 80

CARI_NO3_storm_list = CARI_storm_list[c(grep("NO3", names(CARI_storm_list)))]
CARI_fDOM_storm_list = CARI_storm_list[c(grep("fDOM", names(CARI_storm_list)))]
CARI_SpCond_storm_list = CARI_storm_list[c(grep("SPC", names(CARI_storm_list)))]
CARI_turb_storm_list = CARI_storm_list[c(grep("Turb", names(CARI_storm_list)))]

FRCH_NO3_storm_list = FRCH_storm_list[c(grep("NO3", names(FRCH_storm_list)))]
FRCH_fDOM_storm_list = FRCH_storm_list[c(grep("fDOM", names(FRCH_storm_list)))]
FRCH_SpCond_storm_list = FRCH_storm_list[c(grep("SPC", names(FRCH_storm_list)))]
FRCH_turb_storm_list = FRCH_storm_list[c(grep("Turb", names(FRCH_storm_list)))]

MOOS_NO3_storm_list = MOOS_storm_list[c(grep("NO3", names(MOOS_storm_list)))]
MOOS_fDOM_storm_list = MOOS_storm_list[c(grep("fDOM", names(MOOS_storm_list)))]
MOOS_SpCond_storm_list = MOOS_storm_list[c(grep("SPC", names(MOOS_storm_list)))]
MOOS_turb_storm_list = MOOS_storm_list[c(grep("Turb", names(MOOS_storm_list)))]

# load burst SUNA data #

FRCHfile_list <- list.files(path="~/Documents/DoD_2018_Jake/SUNA_data/from_internal_harddrive/raw/FRCH/", 
                            recursive=F, 
                            pattern=".CSV", 
                            full.names=TRUE)

MOOSfile_list <- list.files(path="~/Documents/DoD_2018_Jake/SUNA_data/from_internal_harddrive/raw/MOOS/", 
                            recursive=F, 
                            pattern=".CSV", 
                            full.names=TRUE)

# Merge all data files for sensor #
#correct header=14

SUNA.FRCH<-do.call("rbind", lapply(FRCHfile_list, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                   skip=14, header=FALSE))

SUNA.MOOS<-do.call("rbind", lapply(MOOSfile_list, 
                                   read.csv, 
                                   stringsAsFactors=FALSE, 
                                   skip=14, header=FALSE))

# Variable names for SUNA output file #
pre<-"ch"
suff<-seq(12:267)
ch<-paste(pre, suff)
SUNAnames<-c("ID", "date_yearday", "time_fhoursUTC", "nitrateuM", "nitratemgL", "abs254", 
             "abs350", "brtrace", "specave", "darkvaluefit", "inttimefac", ch, "int_TC", 
             "spec_TC", "lamp_TC", "lamptimecum", "relhum", "mainV", "lampV", "intV", 
             "mainmA", "fit1", "fit2", "fitbase1", "fitbase2", "fitRMSE", "CTDtime", 
             "CTDsal", "CTDT", "CTDdBar", "checksum")
names(SUNA.FRCH)<-SUNAnames
names(SUNA.MOOS)<-SUNAnames


# Remove unneeded columns #

#remove raw channel data
SUNA.FRCHr<-SUNA.FRCH[,c(1:11,268:286)]
SUNA.MOOSr<-SUNA.MOOS[,c(1:11,268:286)]

#remove dark frames. 
### <<Note: Change date below>> ###
sum(SUNA.FRCHr$ID == 'DF')
SUNA.FRCHr %>% group_by(ID) %>% tally()
SUNA.FRCHrlf<-SUNA.FRCHr[!grepl("DF", SUNA.FRCHr$ID),]


SUNA.MOOSr %>% group_by(ID) %>% tally()
SUNA.MOOSrlf<-SUNA.MOOSr[!grepl("DF", SUNA.MOOSr$ID),]


### Date and time reformatting ###
## FRCH ##
# create separate year and day columns
year_day <- t(sapply(SUNA.FRCHrlf$date_yearday, function(x) substring(x, first=c(1,5), last=c(4,7))))
year_day<-as.data.frame(year_day)
names(year_day)<-c("year", "day")
year_day$day<-as.numeric(year_day$day)
year_day$year<-as.numeric(year_day$year)
SUNA.FRCHrlf<-cbind(SUNA.FRCHrlf, year_day)
# combine hours and julian day into fractional days
SUNA.FRCHrlf$day_timeUTC<-SUNA.FRCHrlf$day+(SUNA.FRCHrlf$time_fhoursUTC/24)
# assign year to 2018 data
origin18 <- as.POSIXct("2017-12-31 00:00:00", tz="GMT")
SUNA.FRCHrlf$date_timeUTC<-origin18 + SUNA.FRCHrlf$day_timeUTC * 3600 * 24
# convert from UTC to AKDT
SUNA.FRCHrlf$date_timeAK<-as.POSIXct(format(SUNA.FRCHrlf$date_timeUTC, tz="America/Anchorage", usetz=TRUE))
summary(SUNA.FRCHrlf$date_timeAK)
class(SUNA.FRCHrlf$date_timeAK)
tz(SUNA.FRCHrlf$date_timeAK) = "America/Anchorage"

## MOOS ##
# create separate year and day columns
year_day <- t(sapply(SUNA.MOOSrlf$date_yearday, function(x) substring(x, first=c(1,5), last=c(4,7))))
year_day<-as.data.frame(year_day)
names(year_day)<-c("year", "day")
year_day$day<-as.numeric(year_day$day)
year_day$year<-as.numeric(year_day$year)
SUNA.MOOSrlf<-cbind(SUNA.MOOSrlf, year_day)
# combine hours and julian day into fractional days
SUNA.MOOSrlf$day_timeUTC<-SUNA.MOOSrlf$day+(SUNA.MOOSrlf$time_fhoursUTC/24)
# assign year to 2018 data
origin18 <- as.POSIXct("2017-12-31 00:00:00", tz="GMT")
SUNA.MOOSrlf$date_timeUTC<-origin18 + SUNA.MOOSrlf$day_timeUTC * 3600 * 24
# convert from UTC to AKDT
SUNA.MOOSrlf$date_timeAK<-as.POSIXct(format(SUNA.MOOSrlf$date_timeUTC, tz="America/Anchorage", usetz=TRUE))
summary(SUNA.MOOSrlf$date_timeAK)
class(SUNA.MOOSrlf$date_timeAK)
tz(SUNA.MOOSrlf$date_timeAK) = "America/Anchorage"

### reduce columns ###
SUNA.FRCH.burst = subset(SUNA.FRCHrlf, select = c("date_timeAK", "nitrateuM"))
SUNA.MOOS.burst = subset(SUNA.MOOSrlf, select = c("date_timeAK", "nitrateuM"))

### round to nearest 15 min ###

SUNA.FRCH.burst$date_timeAK = lubridate::round_date(SUNA.FRCH.burst$date_timeAK, "15 minutes") 
SUNA.MOOS.burst$date_timeAK = lubridate::round_date(SUNA.MOOS.burst$date_timeAK, "15 minutes") 

# load burst EXO data #

### load and stitch EXO data ###

# FRCH #
FRCHfile_list <- list.files(path="~/Documents/DoD_2018_Jake/EXO_data/from_internal_harddrive/raw/FRCH/", 
                            recursive=F, 
                            pattern=".csv", 
                            full.names=TRUE)

EXO.FRCH<-do.call("rbind", lapply(FRCHfile_list, 
                                  read.csv, 
                                  stringsAsFactors=FALSE, 
                                  skip = 20,
                                  header=T, blank.lines.skip = TRUE, fill = TRUE))

names(EXO.FRCH) <- c("Date..MM.DD.YYYY.", "Time..HH.mm.ss.","Time..Fract..Sec.", "Site.Name",  "Fault.Code",
                     "Battery.V", "Cable.Pwr.V", "fDOM.RFU","fDOM.QSU", "Temp..C", "Cond.µS.cm", "SpCond.µS.cm",
                     "Sal.psu","nLF.Cond.µS.cm", "TDS.mg.L", "Turbidity.FNU", "TSS.mg.L")
EXO.FRCH <- EXO.FRCH[-1,]



# MOOS #
MOOSfile_list <- list.files(path="~/Documents/DoD_2018_Jake/EXO_data/from_internal_harddrive/raw/MOOS/", 
                            recursive=F, 
                            pattern=".csv", 
                            full.names=TRUE)

EXO.MOOS<-do.call("rbind", lapply(MOOSfile_list, 
                                  read.csv, 
                                  stringsAsFactors=FALSE, 
                                  skip = 20,
                                  header=T))

names(EXO.MOOS) <- c("Date..MM.DD.YYYY.", "Time..HH.mm.ss.","Time..Fract..Sec.", "Site.Name",  "Fault.Code",
                     "Battery.V", "Cable.Pwr.V", "fDOM.RFU","fDOM.QSU", "Temp..C", "Cond.µS.cm", "SpCond.µS.cm",
                     "Sal.psu","nLF.Cond.µS.cm", "TDS.mg.L", "Turbidity.FNU", "TSS.mg.L")
EXO.MOOS <- EXO.MOOS[-1,]

### format dates ###

## FRCH ##
# put date and time in same column
EXO.FRCH$date_time = paste(EXO.FRCH$Date..MM.DD.YYYY., EXO.FRCH$Time..HH.mm.ss., sep = " ")
# convert to POIXct and set timezone
EXO.FRCH$date_timeET<-as.POSIXct(EXO.FRCH$date_time, "%m/%d/%y %H:%M:%S", tz="America/New_York")
# convert to Alaska Time
EXO.FRCH$date_timeAK <- mdy_hms(EXO.FRCH$date_time)
attributes(EXO.FRCH$date_timeAK)$tzone <- "America/Anchorage"
head(EXO.FRCH)
class(EXO.FRCH$date_timeAK)
tz(EXO.FRCH$date_timeAK)

## MOOS ##
# put date and time in same column
EXO.MOOS$date_time = paste(EXO.MOOS$Date..MM.DD.YYYY., EXO.MOOS$Time..HH.mm.ss., sep = " ")
# convert to POIXct and set timezone
EXO.MOOS$date_timeET<-as.POSIXct(EXO.MOOS$date_time, "%m/%d/%y %H:%M:%S", tz="America/New_York")

# convert to Alaska Time
EXO.MOOS$date_timeAK <- mdy_hms(EXO.MOOS$date_time)
attributes(EXO.MOOS$date_timeAK)$tzone <- "America/Anchorage"
head(EXO.MOOS)
class(EXO.MOOS$date_timeAK)
tz(EXO.MOOS$date_timeAK)

### Variable names ###
names(EXO.FRCH)
names(EXO.FRCH)<-(c("Date", "Time", "Time_s", "Site", "FaultCode", 
                    "BatteryV", "CablePwrV", "fDOMRFU", "fDOMQSU","TempC","ConduScm", 
                    "SpConduScm","Salpsu","nLFConduScm","TDSmgL", "TurbidityFNU",
                    "TSSmgL", "date_time", "date_timeET", "date_timeAK"))


names(EXO.MOOS)
names(EXO.MOOS)<-(c("Date", "Time", "Time_s", "Site", "FaultCode", 
                    "BatteryV", "CablePwrV", "fDOMRFU", "fDOMQSU","TempC","ConduScm", 
                    "SpConduScm","Salpsu","nLFConduScm","TDSmgL", "TurbidityFNU",
                    "TSSmgL", "date_time", "date_timeET", "date_timeAK"))

### reduce columns ###
EXO.FRCH.burst = subset(EXO.FRCH, select=c("date_timeAK", "fDOMQSU", "SpConduScm", "TurbidityFNU"))
EXO.MOOS.burst = subset(EXO.MOOS, select=c("date_timeAK", "fDOMQSU", "SpConduScm", "TurbidityFNU"))

### round to nearest 15 min ###
EXO.FRCH.burst$date_timeAK = lubridate::round_date(EXO.FRCH.burst$date_timeAK, "15 minutes") 
EXO.MOOS.burst$date_timeAK = lubridate::round_date(EXO.MOOS.burst$date_timeAK, "15 minutes") 

### fDOM outlier removal and baseline corrections ########
# plot #
library(ggplot2)
# ggplot(EXO.FRCH.burst) +
#   geom_point(aes(x = EXO.FRCH.burst$date_timeAK, y = EXO.FRCH.burst$fDOMQSU))
# 
# plot(EXO.FRCH.burst$fDOMQSU~ EXO.FRCH.burst$date_timeAK)
# plot(EXO.MOOS.burst$fDOMQSU~ EXO.MOOS.burst$date_timeAK)

# FRCH #
EXO.FRCH.burst$fDOMQSU <- as.numeric(EXO.FRCH.burst$fDOMQSU)
EXO.FRCH.burst$SpConduScm <- as.numeric(EXO.FRCH.burst$SpConduScm)
EXO.FRCH.burst$TurbidityFNU <- as.numeric(EXO.FRCH.burst$TurbidityFNU)

EXO.FRCH.burst <- EXO.FRCH.burst[-21222, ]
EXO.FRCH.burst.1 <- filter(EXO.FRCH.burst, EXO.FRCH.burst$fDOMQSU > 25)
EXO.FRCH.burst <- filter(EXO.FRCH.burst, EXO.FRCH.burst$fDOMQSU > 25)
EXO.FRCH.burst$fDOMQSU[EXO.FRCH.burst$fDOMQSU<25] <- NA
# EXO.C2.burst$fDOMQSU[EXO.C2.burst$fDOMQSU < 20 &
#                        EXO.C2.burst$date_timeAK > as.POSIXct("2017-06-14") &
#                        EXO.C2.burst$date_timeAK < as.POSIXct("2017-06-16")] <- NA 
# plot(EXO.FRCH.burst.1$fDOMQSU ~ EXO.FRCH.burst.1$date_timeAK)


# # match NAs in DOD.2018 to bursts #####
# setwd("~/Documents/Storms_clean_repo")
# DOD_2018 <- read_csv("Q/Q_chem/DOD.2018.csv", 
#                      col_types = cols(NO3 = col_double()))
# 
# DOD_2018$date_timeAK <- as.POSIXct(DOD_2018$datetimeAK, "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
# names(DOD_2018) <- c("datetimeAK", "site.ID", "fDOM.QSU",
#                      "SpCond.uScm", "Turbidity_FNU", "nitrateuM",
#                      "Q", "day", "date_timeAK")
# 
# FRCH <- subset(DOD_2018, site.ID == "FRCH")
# 
# 
## NO3 ##
# temp <-  inner_join(SUNA.FRCH.burst, subset(FRCH, select = c("date_timeAK", "nitrateuM")), by = "date_timeAK")
# temp$nitrateuM <-  ifelse(is.na(temp$nitrate_uM), NA, temp$nitrateuM)
# SUNA.FRCH.burst$nitrateuM = temp$nitrateuM
# 
# ## fDOM.QSU ##
# temp= inner_join(subset(EXO.FRCH.burst, select=c("date_timeAK", "fDOM.QSU")), 
#                  subset(FRCH, select=c("date_timeAK", "fDOM.QSU")), 
#                  by= "date_timeAK")
# temp$fDOM.QSU.x = ifelse(is.na(temp$fDOM.QSU.x), NA, temp$fDOM.QSU.x)
# EXO.FRCH.burst$fDOMQSU = temp$fDOMQSU
# 
# ## SpConduScm ##
# temp= inner_join(subset(EXO.FRCH.burst, select=c("date_timeAK", "SpCond.µS.cm")), 
#                  subset(FRCH, select=c("date_timeAK", "SpCond.uScm")), 
#                  by= "date_timeAK")
# temp$SpCond.uScm = ifelse(is.na(temp$SpCond.uScm), NA, temp$SpCond.uScm)
# EXO.FRCH.burst$SpCond.µS.cm = temp$SpCond.uScm
# 
# ## TurbidityFNU ##
# temp= inner_join(subset(EXO.FRCH.burst, select=c("date_timeAK", "TurbidityFNU")), 
#                  subset(FRCH, select=c("date_timeAK", "Turbidity_FNU")), 
#                  by= "date_timeAK")
# temp$TurbidityFNU = ifelse(is.na(temp$Turbidity_FNU), NA, temp$TurbidityFNU)
# EXO.C2.burst$TurbidityFNU = temp$TurbidityFNU
# 

#### save clean-ish burst data ####
setwd("~/Documents/Storms_clean_repo")

EXO.FRCH.burst$site.ID <- "FRCH"
EXO.MOOS.burst$site.ID <- "MOOS"
SUNA.FRCH.burst$site.ID <- "FRCH"
SUNA.MOOS.burst$site.ID <- "MOOS"


dir.create(file.path("Storm_Events", "2018", "Bursts"))

write.csv(SUNA.FRCH.burst, "~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/SUNA.FRCH.burst.csv")
write.csv(SUNA.MOOS.burst, "~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/SUNA.MOOS.burst.csv")


write.csv(EXO.FRCH.burst, "~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/EXO.FRCH.burst.csv")
write.csv(EXO.MOOS.burst, "~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/EXO.MOOS.burst.csv")

#### join burst and storm data ####

### NO3 ###

for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]] = inner_join(FRCH_NO3_storm_list[[i]], SUNA.FRCH.burst, by=c("valuedatetime" = "date_timeAK"))
}

for(i in 1:length(MOOS_NO3_storm_list)){
  MOOS_NO3_storm_list[[i]] = inner_join(MOOS_NO3_storm_list[[i]], SUNA.MOOS.burst, by=c("valuedatetime" = "date_timeAK"))
}

### fDOM ###

for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]] = inner_join(FRCH_fDOM_storm_list[[i]], 
                                         subset(EXO.FRCH.burst, select=c("date_timeAK", "fDOMQSU")), 
                                         by=c("valuedatetime" = "date_timeAK"))
}

for(i in 1:length(MOOS_fDOM_storm_list)){
  MOOS_fDOM_storm_list[[i]] = inner_join(MOOS_fDOM_storm_list[[i]], 
                                         subset(EXO.MOOS.burst, select=c("date_timeAK", "fDOMQSU")), 
                                         by=c("valuedatetime" = "date_timeAK"))
}

### SpCond ###

for(i in 1:length(FRCH_SpCond_storm_list)){
  FRCH_SpCond_storm_list[[i]] = inner_join(FRCH_SpCond_storm_list[[i]], 
                                           subset(EXO.FRCH.burst, select=c("date_timeAK", "SpConduScm")), 
                                           by=c("valuedatetime" = "date_timeAK"))
}

for(i in 1:length(MOOS_SpCond_storm_list)){
  MOOS_SpCond_storm_list[[i]] = inner_join(MOOS_SpCond_storm_list[[i]], 
                                           subset(EXO.MOOS.burst, select=c("date_timeAK", "SpConduScm")), 
                                           by=c("valuedatetime" = "date_timeAK"))
}

### turb ###

for(i in 1:length(FRCH_turb_storm_list)){
  FRCH_turb_storm_list[[i]] = inner_join(FRCH_turb_storm_list[[i]], 
                                         subset(EXO.FRCH.burst, select=c("date_timeAK", "TurbidityFNU")), 
                                         by=c("valuedatetime" = "date_timeAK"))
}

for(i in 1:length(MOOS_turb_storm_list)){
  MOOS_turb_storm_list[[i]] = inner_join(MOOS_turb_storm_list[[i]], 
                                         subset(EXO.MOOS.burst, select=c("date_timeAK", "TurbidityFNU")), 
                                         by=c("valuedatetime" = "date_timeAK"))
}




# save storm with burst data #

saveRDS(FRCH_NO3_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/FRCH_NO3_storm_list.RData")
saveRDS(FRCH_fDOM_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/FRCH_fDOM_storm_list.RData")
saveRDS(FRCH_SpCond_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/FRCH_SpCond_storm_list.RData")
saveRDS(FRCH_turb_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/FRCH_turb_storm_list.RData")

saveRDS(MOOS_NO3_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/MOOS_NO3_storm_list.RData")
saveRDS(MOOS_fDOM_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/MOOS_fDOM_storm_list.RData")
saveRDS(MOOS_SpCond_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/MOOS_SpCond_storm_list.RData")
saveRDS(MOOS_turb_storm_list, file="~/Documents/Storms_clean_repo/Storm_Events/2018/Bursts/MOOS_turb_storm_list.RData")


