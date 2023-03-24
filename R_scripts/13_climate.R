# press Command+Option+O to collapse all sections and get an overview of the workflow! #

#### read me ####
# The purpose of this script is to summarize CRREL and CPEAK air temp and precip records for 30 years (or as far back as is available) through 2017 (or through the year of the study) for manuscript site descriptions. This file was created for the CPCRW-2017 manuscript, so fork and edit as needed for your purposes.

# note that I have commented out the plotting code in here so that I could test the code quickly. The plots take a while to load because these records are long, but you should take the time to examine the plots when running the code the first time or if you bring in updated data.

# 1) Get that data. 
## The below website has BNZ meterological data up to present, so is a better source than the BNZ data catalog. Note that this data is uncorrected pre 2010, so examine it critically!
## Website: http://bnznet.iab.uaf.edu/vdv/index.html
## User name: data_user
## Password: borealBNZ
## Use the downward arrow icon on the left panel menu to select the range of data that you need and download to a .txt file. Yes, you have to just click through the months until you get back to 1986 D:
## See the screenshot in the repo to see what the data download should look like
## This data needs to be updated for newer studies!

# 2) Get that snow data.
## The best snow water equivelent data (SWE) for wintertime precip. is from the CARSNOW site within the Caribou Poker Creeks Research Watershed for 2007-2016. I can't find this data in the uncorrected BNZ catalog, so I got this from the regular BNZ data catalog here: http://www.lter.uaf.edu/data/data-detail/id/386
## Snow data from more current sources similarly only goes back to 2006, and has no documentation of what the conversion to SWE is. Due to the lack of congruent data with the rain data from any source, I'm going to use the CARSNOW data to get an annual average, and just add this to the rain data for a climate description.
## This data needs to be updated for newer studies!

# 3) Of the met. data downloaded, determine the longest records. 
## See above for snow decisions. 
## For air, the longest record is from CPCRW.CRREL.Main.Met.Station..AirTemp from the 100 and 300 cm heights. (1992) I will use the avg. of these
## For rain, the longest record is CPCRW.Caribou.Peak..TippingBucket_TOT.mm.CPEAK.mm. (1993). I will use this.

# 4) Examine and clean data. Most records have obviously anomalous highs and/or lows. Id and remove.

# 5) calculate annual and monthly means for study period and long term records, and compare. See "final report" for how this should all turned into prose for a manuscript. 


#### libraries ####
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(zoo)
library(xts)
library(forecast)
library(here)
library(RColorBrewer)
library(dplyr)
library(here)
library(stats)
library(readr)
library(plotly)
library(GGally)
library(ggpmisc)
library(ggpubr)
library(ggExtra)
library(nlme)
library(MuMIn)
library(multcomp)
library(sjPlot)
library(AICcmodavg)




#### load data ####

dat = read.table(file = "vdv_1987-01-01 00_00_00_2020-02-27 00_00_00_20200227045530.csv", 
                 header = T, sep = ",", dec = ".", skip=4)
dat$dateAK = as.POSIXct(dat$Time, "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")

snow = read.table(file = "386_SNOWPILLOW_CARSNOW_2007-2021.txt", header = T, sep = ",", dec = ".")
#snow$dateAK = as.POSIXct(paste(snow$date, snow$hour), "%Y-%m-%d %H%M", tz="America/Anchorage")
snow$dateAK = as.POSIXct(snow$date, "%Y-%m-%d", tz="America/Anchorage")

#### determine longest records #

min(dat$dateAK[!is.na(dat$CPCRW.Caribou.Peak..AirTemp_1000cm.CPEAK..C.)], na.rm=T) #1998
min(dat$dateAK[!is.na(dat$CPCRW.Caribou.Peak..AirTemp_200cm.CPEAK..C.)], na.rm=T) #1998
min(dat$dateAK[!is.na(dat$CPCRW.Caribou.Peak..AirTemp_100cm.CPEAK..C.)], na.rm=T) #1993
min(dat$dateAK[!is.na(dat$CPCRW.CRREL.Main.Met.Station..AirTemp_1000cm.CRREL..C.)], na.rm=T) #2000
min(dat$dateAK[!is.na(dat$CPCRW.CRREL.Main.Met.Station..AirTemp_300cm.CRREL..C.)], na.rm=T) #1992
min(dat$dateAK[!is.na(dat$CPCRW.CRREL.Main.Met.Station..AirTemp_1200cm.CRREL..C.)], na.rm=T) #2000
min(dat$dateAK[!is.na(dat$CPCRW.CRREL.Main.Met.Station..AirTemp_100cm.CRREL..C.)], na.rm=T) #1992
min(dat$dateAK[!is.na(dat$CPCRW.Caribou.Peak..SnowBucket_mm.CPEAK.mm.)], na.rm=T) #2008
min(dat$dateAK[!is.na(dat$CPCRW.Caribou.Peak..TippingBucket_TOT.mm.CPEAK.mm.)], na.rm=T) #1993
min(dat$dateAK[!is.na(dat$CPCRW.CRREL.Main.Met.Station..SnowDepth.m..CRREL.m.)], na.rm=T) #2006
min(dat$dateAK[!is.na(dat$CPCRW.CRREL.Main.Met.Station..TippingBucket_TOT.mm..CRREL.mm.)], na.rm=T) #2007

min(snow$dateAK) #2007

# longest temp records is CPCRW.CRREL.Main.Met.Station..AirTemp_300cm.CRREL..C.so I will use this for temp

# longest rain record is CPCRW.Caribou.Peak..TippingBucket_TOT.mm.CPEAK.mm., so I will use this for rain

# longest snow 

#### select data for summaries #

temp = data.frame(
  date_timeAK = dat$dateAK,
  AirTemp = dat$CPCRW.CRREL.Main.Met.Station..AirTemp_300cm.CRREL..C.
)
temp = na.trim(temp, is.na = "any") # trim nas from ends
# check for nas
any(is.na(temp))
#plot(temp$AirTemp ~ temp$date_timeAK)

rain = data.frame(
  date_timeAK = dat$dateAK,
  Rain = dat$CPCRW.Caribou.Peak..TippingBucket_TOT.mm.CPEAK.mm.
)
rain = na.trim(rain, is.na = "any") # trim nas from ends
# check for nas
any(is.na(rain))
#plot(rain$Rain ~ rain$date_timeAK)

#### clean data #

temp.c = temp[temp$AirTemp < 40,]
#plot(temp.c$AirTemp ~ temp.c$date_timeAK)

rain.c = rain[rain$Rain > -1 & rain$Rain < 70,]
#plot(rain.c$Rain ~ rain.c$date_timeAK)
## numbers before 1997 look fishy (too high), so need to remove
rain.c.r = rain.c[rain.c$date_timeAK > "1998-01-01 00:00:00 AKST",]
rain.c.r = na.trim(rain.c.r, is.na="all")
#plot(rain.c.r$Rain ~ rain.c.r$date_timeAK)

#plot(snow$value ~ snow$dateAK)
snow.c = snow[snow$value > 0,]
#plot(snow.c$value ~ snow.c$dateAK)

#### set date limits for study period #
yr.limit = 2023 # this should be the year AFTER your study
yr.of = 2017 # this should be the year OF your study
#### air temp #

temp.c$day = day(temp.c$date_timeAK) # add a column for day
temp.c$mo = month(temp.c$date_timeAK) # add a column for month
temp.c$yr = year(temp.c$date_timeAK) # add a column for year

## convert hourly data to daily averages ##
temp.daily = 
  temp.c %>%
  select(day, mo, yr, AirTemp)%>%
  group_by(day, mo, yr) %>%
  summarize_all(funs(mean), na.rm = T)

## convert mean daily to mean monthly ##
temp.monthly = 
  temp.daily %>%
  group_by(mo, yr) %>%
  summarize_all(funs(mean), na.rm = T)

## convert mean monthly to mean annual ##
temp.annual = 
  temp.monthly %>%
  group_by(yr) %>%
  summarize_all(funs(mean), na.rm = TRUE)
plot(temp.annual$AirTemp ~ temp.annual$yr)

# calculate annual mean for study period - put the year after your last study year in here
mean(temp.annual$AirTemp[temp.annual$yr < yr.limit], na.rm = T)
# get year range for mean
range(temp.annual$yr[temp.annual$yr < yr.limit], na.rm = T)
# annual mean = -3.560643 C
# for: 1992 - 2017

# compare study year to long term avg. 
now = temp.annual$AirTemp[temp.annual$yr == yr.of][1]
then = mean(temp.annual$AirTemp[temp.annual$yr < yr.limit], na.rm = T)
now-then
# study is 1.02 deg higher than long term avg... I think that qualifies as higher

# calculate mean monthly lowest temperature from January means
mean(temp.monthly$AirTemp[temp.monthly$mo == 1 & temp.monthly$yr < yr.limit], na.rm = T)
# -23.20802 C

# calculate mean monthly highest temperature from July means
mean(temp.monthly$AirTemp[temp.monthly$mo == 7 & temp.monthly$yr < yr.limit], na.rm = T)
# 15.06846 C

# compare study period to the long term avg for those months
# THIS study period = May-Sept 2017
# I'm going to designate a difference of |1 deg| as different enough to report as non-similar

# temp in May of this study:
now.may = temp.monthly$AirTemp[temp.monthly$mo == 5 & temp.monthly$yr == yr.of][1] 
# long term Mar temp avg:
then.may = mean(temp.monthly$AirTemp[temp.monthly$mo == 5 & temp.monthly$yr < yr.limit], na.rm = T)
now.may - then.may
# study is 0.132 deg higher than long term avg = "similar"

# temp in june of this study:
now.june = temp.monthly$AirTemp[temp.monthly$mo == 6 & temp.monthly$yr == yr.of][1] 
# long term Mar temp avg:
then.june = mean(temp.monthly$AirTemp[temp.monthly$mo == 6 & temp.monthly$yr < yr.limit], na.rm = T)
now.june - then.june
# study is 0.718 deg higher than long term avg = "similar"

# temp in july of this study:
now.july = temp.monthly$AirTemp[temp.monthly$mo == 7 & temp.monthly$yr == yr.of][1] 
# long term Mar temp avg:
then.july = mean(temp.monthly$AirTemp[temp.monthly$mo == 7 & temp.monthly$yr < yr.limit], na.rm = T)
now.july - then.july
# study is 1.962 deg higher than long term avg... I think that qualifies as higher

# temp in aug of this study:
now.aug = temp.monthly$AirTemp[temp.monthly$mo == 8 & temp.monthly$yr == yr.of][1] 
# long term Mar temp avg:
then.aug = mean(temp.monthly$AirTemp[temp.monthly$mo == 8 & temp.monthly$yr < yr.limit], na.rm = T)
now.aug - then.aug
# study is 0.589 deg higher than long term avg = "similar"

#
#### precip RAIN #
rain.c.r$day = day(rain.c.r$date_timeAK) # add a column for day
rain.c.r$mo = month(rain.c.r$date_timeAK) # add a column for month
rain.c.r$yr = year(rain.c.r$date_timeAK) # add a column for year

## convert hourly data to daily sum ##
rain.daily = 
  rain.c.r %>%
  select(day, mo, yr, Rain)%>%
  group_by(day, mo, yr) %>%
  summarize_all(funs(sum), na.rm = TRUE)

## convert daily sum to monthly sum ##
rain.monthly = 
  rain.daily %>%
  group_by(mo, yr) %>%
  summarize_all(funs(sum), na.rm = TRUE)

## convert daily sum to annual sum ##
rain.annual = 
  rain.daily %>%
  group_by(yr) %>%
  summarize_all(funs(sum), na.rm = TRUE)


# compare study period to the long term avg for those months
# THIS study period = May-Sept 2017
# I'm going to designate a difference of |10 mm| as different enough to report as non-similar

# rain in May of this study:
now.may = rain.monthly$Rain[rain.monthly$mo == 5 & rain.monthly$yr == yr.of][1] 
# long term Mar rain avg:
then.may = mean(rain.monthly$Rain[rain.monthly$mo == 5 & rain.monthly$yr < yr.limit], na.rm = T)
now.may - then.may
# study is 14.89705 mm higher than long term avg = "higher"

# rain in june of this study:
now.june = rain.monthly$Rain[rain.monthly$mo == 6 & rain.monthly$yr == yr.of][1] 
# long term Mar rain avg:
then.june = mean(rain.monthly$Rain[rain.monthly$mo == 6 & rain.monthly$yr < yr.limit], na.rm = T)
now.june - then.june
# study is -23.1236 mm lower than long term avg = "lower"

# rain in july of this study:
now.july = rain.monthly$Rain[rain.monthly$mo == 7 & rain.monthly$yr == yr.of][1] 
# long term Mar rain avg:
then.july = mean(rain.monthly$Rain[rain.monthly$mo == 7 & rain.monthly$yr < yr.limit], na.rm = T)
now.july - then.july
# study is 7.33 mm higher than long term avg = "similar"

# rain in aug of this study:
now.aug = rain.monthly$Rain[rain.monthly$mo == 8 & rain.monthly$yr == yr.of][1] 
# long term Mar rain avg:
then.aug = mean(rain.monthly$Rain[rain.monthly$mo == 8 & rain.monthly$yr < yr.limit], na.rm = T)
now.aug - then.aug
# study is 1.1 mm higher than long term avg = "similar"

#### precip SNOW #
# 	The snow pillow records the hourly water content of the snowpack (snow water equivalent) at the CARSNOW site within the Caribou Poker Creeks Research Watershed during the winter months. It consists of two 1m square aluminium "pillows" filled with a propylene glycol/water solution attached via piping to a druck pressure transducer. The pressure on the pillow is converted to **cm of water**. A manometer tube is also attached for manaul readings and calibration

snow.c$day = day(snow.c$dateAK) # add a column for day
snow.c$mo = month(snow.c$dateAK) # add a column for month
snow.c$yr = year(snow.c$dateAK) # add a column for year

snow.c$value <- as.numeric(snow.c$value)
## convert hourly data to daily mean ##
snow.daily = 
  snow.c %>%
  select(day, mo, yr, value)%>%
  group_by(day, mo, yr) %>%
  summarize_all(funs(mean), na.rm = TRUE)

## convert daily sum to annual mean ##
snow.annual = 
  snow.daily %>%
  group_by(yr) %>%
  summarize_all(funs(mean), na.rm = TRUE)




## calculate mean annual snowfall, converting cm to mm
# numbers in 2009 are negative, hence the requirement for values to be > 0
mean(snow.annual$value[snow.annual$value>0 & snow.annual$yr < yr.limit])*10 

#### mean annual precip = rainfall + mean annual SWE in mm, == 
mean(rain.annual$Rain[rain.annual$yr<yr.limit], na.rm=T) + 
  mean(snow.annual$value[snow.annual$value>0 & snow.annual$yr < yr.limit])*10 

#### what percentage falls as rain?
((mean(rain.annual$Rain[rain.annual$yr<yr.limit], na.rm=T) ) / 
    ((mean(rain.annual$Rain[rain.annual$yr<yr.limit], na.rm=T) )+ 
       mean(snow.annual$value[snow.annual$value>0 & snow.annual$yr < yr.limit])*10 )
)*100


snow.annual$value <- snow.annual$value*10 # converting cm to mm to add to the rain figure
#### final report #

# this is for a study that occurred May-Sept. 2017
# Mean annual air temperature is -3.6°C, with lowest mean monthly temperature typically in January (-23.2°C) and highest in July (15.1°C). During the study period (May-Sept. 2017), mean monthly temperatures were similar to long-term averages (since 1992), with the exception of mean July temperatures ~2°C warmer than average. The CPCRW receives 351.1 mm precipitation annually on average, with 294.2 mm falling as rain. Mean monthly precipitation during the study period was higher in May (39.4 mm), lower in June (34.8 mm), and similar in July (104.1 mm) and Aug. (83.1 mm) compared to long term monthly averages. 



######## PLOTS #
brewer.pal(n = 8, name = "Dark2")

rain.monthly.summer <- subset(rain.monthly, rain.monthly$mo > 4 & rain.monthly$mo < 10)

ggplot(rain.monthly.summer, aes(fill=as.factor(mo), y= Rain, x= yr)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  theme_classic() +
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FB9A99", "#FF7F00", "#FDBF6F"))

# snow.winter.1 <- subset(snow.monthly, snow.monthly$mo == 1)
# snow.winter.2 <- subset(snow.monthly, snow.monthly$mo == 2)
# snow.winter.3 <- subset(snow.monthly, snow.monthly$mo == 3)
# snow.winter.4 <- subset(snow.monthly, snow.monthly$mo == 4)
# snow.winter.10 <- subset(snow.monthly, snow.monthly$mo == 10)
# snow.winter.11 <- subset(snow.monthly, snow.monthly$mo == 11)
# snow.winter.12<- subset(snow.monthly, snow.monthly$mo == 12)
# 
# snow.winter <- rbind(snow.winter.1, snow.winter.2, snow.winter.3, snow.winter.4, snow.winter.10,
#                      snow.winter.11, snow.winter.12)


ggplot(snow.winter, aes(fill=as.factor(mo), y= value, x= yr)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  theme_classic() +
  scale_fill_manual(values = c("#A6CEE3", "#B2DF8A", "#FB9A99", "#FF7F00", "#FDBF6F", "blue", "red"))


## convert daily sum to annual mean ##
names(snow.annual) <- c("yr", "day", "mo", "precip")
snow.annual$method <- "Snow"

rain.monthly.summer$method <- "Rain"
names(rain.monthly.summer) <- c("mo", "yr", "day", "precip", "method")

snow.rain <- rbind(rain.monthly.summer, snow.annual)


snow.rain[c(1:21), 5] <- "May"
snow.rain[c(22:42), 5] <- "June"
snow.rain[c(43:63), 5] <- "July"
snow.rain[c(64:85), 5] <- "August"
snow.rain[c(86:107), 5] <- "September"
snow.rain[c(108:121), 5] <- "Snow"


ggplot(snow.rain, aes(fill=method, y= precip, x= yr)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  theme_classic() +
  scale_fill_manual(values = c("#B2182B", "#B2DF8A", "#FB9A99", "#FDBF6F", "#FF7F00", "#92C5DE"))

snow.rain.new <- snow.rain

dfRemain <- df[-c(2, 3),]

snow.rain.new <- snow.rain.new[-c(121:122), ] # getting rid of 2020/2021 data for snow due to not having that updated rain record yet

snow.rain.new$method <- factor(snow.rain.new$method,                 # Relevel group factor
                               levels = c("May", "June", "July", "August", "September", "Snow"))
ggplot(snow.rain.new, aes(fill=method, y= precip, x= yr)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#E6AB02", "#D95F02", "#7570B3", "#666666", "#A6761D", "#92C5DE")) 

ggsave("~/Documents/Storms/Harms_general/Climate.pdf", width = 6, height = 6, device = "pdf")

#### THESIS plot test #
# ggplot(rain.sum, aes(x = year, y = rain, fill = MONTH)) + 
#   geom_bar(position="stack", stat="identity", color = "black") +
#   xlab("Year") +
#   ylab("Precipitation (mm)") +
#   theme_classic() +
#   theme(legend.title = element_blank()) +
#   scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) 
# 
# # Snow 
# snow.pillow <- read.csv(here("Climate", "177_SNOWPILLOW_LTER1_1989-2021.txt"))
# snow.pillow$value <- as.numeric(snow.pillow$value)
# snow.pillow <- snow.pillow %>% 
#   mutate(across(c(value),
#                 ~ifelse(unit == "cm", value*10, .)))
# 
# 
# snow.pillow <- snow.pillow[c("date", "value")]
# names(snow.pillow)[names(snow.pillow) == 'date'] <- 'Date'
# names(snow.pillow)[names(snow.pillow) == 'value'] <- 'snow'
# 
# snow.pillow$Date <- ymd(snow.pillow$Date)
# snow.pillow$snow <- as.numeric(snow.pillow$snow)
# snow.pillow$month <- month(snow.pillow$Date)
# snow.pillow$year <- year(snow.pillow$Date)
# 
# snow.sum <- snow.pillow %>% 
#   group_by(year) %>% 
#   dplyr::summarise(snow = max(snow, na.rm = TRUE)) # totaling by year and snow/rain 
# 
# 
# 
# climate <- left_join(snow.sum, rain.sum)
# 
# climate.long <- climate %>%
#      pivot_longer(
#        cols = c(snow,rain),
#        names_to = "response_var",
#        values_to = "precip",
#        values_drop_na = TRUE
#      ) # converting to a long format so each response_var is within a single column
#    
# climate.sum <- climate.long %>% 
#   group_by(year,month, response_var) %>% 
#   dplyr::summarise(precip = mean(precip, na.rm = TRUE)) # totaling by year and snow/rain 
# 
# climate.sum <- climate.sum[!duplicated(climate.sum$precip), ]
# 
# climate.sum <- climate.sum[-c(1:3),]
# 
# climate.sum$MONTH <- NA
# 
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "1", "Snow Water Equivalent", .)))
# 
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "5", "May", .)))
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "6", "June", .)))
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "7", "July", .)))
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "8", "August", .)))
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "9", "September", .)))
# climate.sum <- climate.sum %>% 
#   mutate(across(c(MONTH),
#                 ~ifelse(month == "10", "October", .)))
# 
# climate.sum$MONTH <- factor(climate.sum$MONTH,                 # Relevel group factor
#                          levels = c("May", "June", "July", "August", "September", "October", "Snow Water Equivalent"))
# 
# cbPalette <- c("#CC79A7", "#E69F00", "#D55E00", "#009E73", "#F0E442", "#0072B2", "#56B4E9")
# 
# ggplot(climate.sum, aes(x = year, y = precip, fill = MONTH)) + 
#   geom_bar(position="stack", stat="identity", color = "black") +
#   xlab("Year") +
#   ylab("Precipitation (mm)") +
#   theme_classic() +
#   theme(legend.title = element_blank()) +
#   scale_fill_manual(values=cbPalette)
# 
# ggsave("total_precip.pdf",
#        path = here("Climate"),
#        width = 10, height = 10)
# 

#### SNOTEL DATA # This is what I am using in my thesis as of 10/7/22 ####
precip_file_list <- list.files(path="~/Documents/Storms_clean_repo/Climate/SWE_Precip/",
                                  recursive=F,
                                  full.names=TRUE) # reading in individual storms by site 

precip <-do.call("rbind", lapply(precip_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

# rain 
rain <- read.csv("~/Documents/Storms_clean_repo/Climate/Precip/Fairbanks_Precip_Snow.csv", skip = 5, header = T)
rain <- rain[c("Date", "Precipitation..in.")]
rain$Date <- mdy(rain$Date) # converting the date column from character to date format

rain$month <- month(rain$Date) # extracting which month in order to sum by month
rain$year <- year(rain$Date) # extracting year to sum by year as well

rain$rain.mm <- rain$Precipitation..in.*25.4 # converting from inches to mm 


rain.sum <- rain %>% 
  group_by(year, month) %>% 
  dplyr::summarise(rain = sum(rain.mm, na.rm = TRUE)) # totaling by year and snow/rain 

rain.sum <- rain.sum %>%
  dplyr::mutate(across(c(rain),
                ~ifelse(month == "1"| month == "2" | month == "3" |
                          month == "4" | month == "11"| month == "12", NA, .))) # removing any rain precip that would occur during the winter months

rain.sum$MONTH <- NA

rain.sum <- rain.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "5", "May", .)))
rain.sum <- rain.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "6", "June", .)))
rain.sum <- rain.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "7", "July", .)))
rain.sum <- rain.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "8", "August", .)))
rain.sum <- rain.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "9", "September", .)))
rain.sum <- rain.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "10", "October", .)))

rain.sum$MONTH <- factor(rain.sum$MONTH,                 # Relevel group factor
                         levels = c("May", "June", "July", "August", "September", "October"))

rain.sum[33,3] <- 15.493
rain.sum[78,3] <- 33.781
rain.sum[89,3] <- 7.873
rain.sum[106,3] <- 23.113
rain.sum[137,3] <- 6.857
rain.sum[138,3] <- 15.495
rain.sum[150,3] <- 7.875
rain.sum[151,3] <- 28.703
rain.sum[153,3] <- 34.035
rain.sum[178,3] <- 14.223
rain.sum[189,3] <- 40.131
rain.sum[221,3] <- 6.095
rain.sum[225,3] <- 30.225
rain.sum[226,3] <- 11.431
rain.sum[235,3] <- 43.181
rain.sum[237,3] <- 16.511
rain.sum[247,3] <- 43.689
rain.sum[250,3] <- 35.051
rain.sum[262,3] <- 14.985
rain.sum[282,3] <- 26.163
rain.sum[293,3] <- 19.813
rain.sum[296,3] <- 39.625
rain.sum[297,3] <- 52.831
rain.sum[298,3] <- 0.507
rain.sum[308,3] <- 54.611
rain.sum[319,3] <- 25.657
rain.sum[365,3] <- 27.6861
rain.sum[367,3] <- 12.9541
rain.sum[368,3] <- 35.3061
rain.sum[369,3] <- 36.8301



# snow 
snow.pillow <- precip[c("Date", "WTEQ.I-1 (in)")]
snow.pillow$SWEmm <- snow.pillow$`WTEQ.I-1 (in)`*25.4 # converting to mm 

names(snow.pillow)[names(snow.pillow) == 'SWEmm'] <- 'snow'

snow.pillow$Date <- mdy(snow.pillow$Date)
snow.pillow$month <- month(snow.pillow$Date)
snow.pillow$year <- year(snow.pillow$Date)

snow.sum <- snow.pillow %>% 
  group_by(year) %>% 
  dplyr::summarise(snow = max(snow, na.rm = TRUE)) # totaling by year and snow/rain 

snow.sum[6,2] <- 88.91
snow.sum[9,2] <- 53.35
snow.sum[14,2] <- 104.15
snow.sum[16,2] <- 91.45
snow.sum[25,2] <- 88.92
snow.sum[26,2] <- 88.93
snow.sum[30,2] <- 142.25


climate <- left_join(snow.sum, rain.sum)

climate.long <- climate %>%
  pivot_longer(
    cols = c(snow,rain),
    names_to = "response_var",
    values_to = "precip",
    values_drop_na = TRUE
  ) # converting to a long format so each response_var is within a single column

climate.sum <- climate.long %>% 
  group_by(year,month, response_var) %>% 
  dplyr::summarise(precip = mean(precip, na.rm = TRUE)) # totaling by year and snow/rain 


climate.sum <- climate.sum[!duplicated(climate.sum$precip), ]


climate.sum$MONTH <- NA

climate.sum <- climate.sum[-1,]

climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "1", "Snow Water Equivalent", .)))

climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "5", "May", .)))
climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "6", "June", .)))
climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "7", "July", .)))
climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "8", "August", .)))
climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "9", "September", .)))
climate.sum <- climate.sum %>% 
  dplyr::mutate(across(c(MONTH),
                ~ifelse(month == "10", "October", .)))

climate.sum$MONTH <- factor(climate.sum$MONTH,                 # Relevel group factor
                            levels = c("May", "June", "July", "August", "September", "October", "Snow Water Equivalent"))

cbPalette <- c("#CC79A7", "#E69F00", "#D55E00", "#009E73", "#F0E442", "#0072B2", "#56B4E9")

ggplot(climate.sum, aes(x = year, y = precip, fill = MONTH)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm')) +
  scale_fill_manual(values=cbPalette) +
  theme(axis.text.x=element_text(size=15), 
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))



ggsave("total_precip_snotel.pdf",
       path = here("Climate"),
       width = 7, height = 7)

#### summary stats ####
# mean annual
climate.sum.year <- climate.sum %>% 
  group_by(year) %>% 
  dplyr::summarise(precip = sum(precip, na.rm = TRUE)) # totaling by year and snow/rain 
mean(climate.sum.year$precip) # 327.7288

# summer rain 
summer.rain <- rain.sum %>% 
  subset(rain.sum$month == 6 | rain.sum$month == 7 | rain.sum$month == 8)
  

summer.sum <- summer.rain %>% 
  group_by(year) %>% 
  dplyr::summarise(rain = sum(rain, na.rm = TRUE)) # totaling by year and snow/rain 
mean(summer.sum$rain) # 149.0737

# winter snow  
winter.snow <- climate.sum %>% 
  subset(climate.sum$response_var == "snow")
mean(winter.snow$precip) # 112.2548

112.2548/327.7288 # 34% How much of precip is rain 


#
which(summer.sum$rain > 159)
# 2019 - 202.6920
# 2020 - 191.0080
# 2021 - 175.5140



