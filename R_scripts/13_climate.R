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
library(here)

#### February 7th, 2026 - AIRTEMP ####
# This code is from Bonanza Creek LTER 

# Package ID: knb-lter-bnz.164.27 Cataloging System:https://pasta.edirepository.org.
# Data set title: Bonanza Creek LTER:  Hourly Air Temperature Measurements (mean, min, max) at Various Heights from 1995 to Present in the Caribou-Poker Creeks Research Watershed near Fairbanks, Alaska.
# Data set creator:  F Chapin -  
# Data set creator:  Roger Ruess -  
# Data set creator:  Michelle Mack -  
# Data set creator:    - Bonanza Creek LTER 
# Metadata Provider:    - Bonanza Creek LTER 
# Contact:    - Data Manager Bonanza Creek LTER  - uaf-bnz-im-team@alaska.edu
# Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      

options(HTTPUserAgent="EDI_CodeGen")

# Url2 is CRREL-MET station #
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-bnz/164/27/04d07f76b88162a3a3af44253a602774" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "site",     
                 "date",     
                 "hour....",     
                 "airtemp",     
                 "mean_flag",     
                 "airtemp_max",     
                 "max_flag",     
                 "airtemp_min",     
                 "min_flag",     
                 "height"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$date != "",]) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt2$hour....)=="factor") dt2$hour.... <-as.numeric(levels(dt2$hour....))[as.integer(dt2$hour....) ]               
if (class(dt2$hour....)=="character") dt2$hour.... <-as.numeric(dt2$hour....)
if (class(dt2$airtemp)=="factor") dt2$airtemp <-as.numeric(levels(dt2$airtemp))[as.integer(dt2$airtemp) ]               
if (class(dt2$airtemp)=="character") dt2$airtemp <-as.numeric(dt2$airtemp)
if (class(dt2$mean_flag)!="factor") dt2$mean_flag<- as.factor(dt2$mean_flag)
if (class(dt2$airtemp_max)=="factor") dt2$airtemp_max <-as.numeric(levels(dt2$airtemp_max))[as.integer(dt2$airtemp_max) ]               
if (class(dt2$airtemp_max)=="character") dt2$airtemp_max <-as.numeric(dt2$airtemp_max)
if (class(dt2$max_flag)!="factor") dt2$max_flag<- as.factor(dt2$max_flag)
if (class(dt2$airtemp_min)=="factor") dt2$airtemp_min <-as.numeric(levels(dt2$airtemp_min))[as.integer(dt2$airtemp_min) ]               
if (class(dt2$airtemp_min)=="character") dt2$airtemp_min <-as.numeric(dt2$airtemp_min)
if (class(dt2$min_flag)!="factor") dt2$min_flag<- as.factor(dt2$min_flag)
if (class(dt2$height)=="factor") dt2$height <-as.numeric(levels(dt2$height))[as.integer(dt2$height) ]               
if (class(dt2$height)=="character") dt2$height <-as.numeric(dt2$height)

# Convert Missing Values to NA for non-dates

dt2$airtemp <- ifelse((trimws(as.character(dt2$airtemp))==trimws("NULL")),NA,dt2$airtemp)               
suppressWarnings(dt2$airtemp <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt2$airtemp))==as.character(as.numeric("NULL"))),NA,dt2$airtemp))
dt2$airtemp_max <- ifelse((trimws(as.character(dt2$airtemp_max))==trimws("NULL")),NA,dt2$airtemp_max)               
suppressWarnings(dt2$airtemp_max <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt2$airtemp_max))==as.character(as.numeric("NULL"))),NA,dt2$airtemp_max))
dt2$airtemp_min <- ifelse((trimws(as.character(dt2$airtemp_min))==trimws("NULL")),NA,dt2$airtemp_min)               
suppressWarnings(dt2$airtemp_min <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt2$airtemp_min))==as.character(as.numeric("NULL"))),NA,dt2$airtemp_min))

### Make datetime column ###
airtemp <- dt2 %>%
  mutate(
    # Convert hour.... (e.g., 1400) to "14:00:00"
    time_str = str_pad(hour...., width = 4, pad = "0") %>%
      str_replace("(\\d{2})(\\d{2})", "\\1:\\2:00"),
    
    # Combine date and time into datetime
    datetime = ymd_hms(paste(date, time_str), tz = "America/Los_Angeles")
  ) %>%
  select(-time_str) %>% 
  select(site, datetime, airtemp, mean_flag, airtemp_max, max_flag, airtemp_min, min_flag, height) %>% 
  filter(height == 300)

# Plot to look at the data
# It looks like there are some values that are greater than 50 and that seems erroneous. Lets filter those out. 
ggplot(airtemp, aes(x = datetime, y = airtemp)) +
  geom_point() +
  theme_bw()

# Filter out negative values and 2022 data because there is only 1 value and that will mess up the averages 
airtemp <- airtemp %>% 
  filter(airtemp < 50) %>% 
  filter(datetime < "2022-01-01 00:00:00")

# Plot again
# This looks good. Lets do summary stats on this 
ggplot(airtemp, aes(x = datetime, y = airtemp)) +
  geom_point() +
  theme_bw()

### set date limits for study period ###
yr.limit = 2023 # this should be the year AFTER your study
yr.of = 2015 # this should be the year OF your study

# Add a column for day, month and year 
airtemp <- airtemp %>% 
  mutate(day = day(datetime),
         month = month(datetime),
         year = year(datetime))

# Convert hourly data to daily averages
temp_daily <- airtemp %>% 
  select(day, month, year, airtemp) %>% 
  group_by(day, month, year) %>%
  summarise(
    airtemp_avg = mean(airtemp, na.rm = TRUE),
    .groups = "drop"
  )

# Convert hourly data to daily averages
temp_monthly <- temp_daily %>%
  group_by(year, month) %>%
  summarise(
    airtemp_avg = mean(airtemp_avg, na.rm = TRUE),
    .groups = "drop"
  )

# Convert mean monthly to mean annual #
temp_annual <- temp_monthly %>%
  group_by(year) %>%
  summarise(
    airtemp_avg = mean(airtemp_avg, na.rm = TRUE),
    .groups = "drop"
  )

# Plot annual temperature
ggplot(temp_annual, aes(year, airtemp_avg)) +
  geom_point() +
  theme_bw()

# calculate annual mean for historical data 
historical_stats <- temp_annual %>%
  filter(year < yr.limit) %>%
  reframe(
    mean_airtemp  = mean(airtemp_avg, na.rm = TRUE),
    min_airtemp   = min(airtemp_avg, na.rm = TRUE),
    max_airtemp   = max(airtemp_avg, na.rm = TRUE))
# For 1992-2021:
  # mean: -3.47       
  # range: [-7.12, -1.14]

# compare study years (2015-2022) to long term avg. 
study_mean <- temp_annual %>%
  filter(year >= 2015, year <= 2022) %>%
  reframe(
    mean_airtemp  = mean(airtemp_avg, na.rm = TRUE),
    min_airtemp   = min(airtemp_avg, na.rm = TRUE),
    max_airtemp   = max(airtemp_avg, na.rm = TRUE))
# For 2015-2022:
  # mean: -2.53
  # range: [-4.23, -1.14]

study_mean$mean_airtemp - historical_stats$mean_airtemp
# study is 0.9367856 deg higher than long term avg... I think that qualifies as higher

# Calculate monthly temperature for January and July for the historical record
monthly_min <- temp_monthly %>%
  filter(month %in% c(1, 7)) %>%
  group_by(month) %>%
  summarise(mean_min_temp = mean(airtemp_avg, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_name = if_else(month == 1, "January", "July"))
# January: -23.10786 C
# July: 15.13378 C

# compare study period to the long term avg for those months
# THIS study period = May-October 2015-2022
# I'm going to designate a difference of |1 deg| as different enough to report as non-similar
# Parameters
study_years <- 2015:2022
historical_cutoff <- 2015
months_to_compare <- 5:10  # May–October

comparison <- temp_monthly %>%
  filter(month %in% months_to_compare) %>%
  group_by(month) %>%
  summarise(
    study_avg = mean(airtemp_avg[year %in% study_years], na.rm = TRUE),
    hist_avg  = mean(airtemp_avg[year < historical_cutoff], na.rm = TRUE),
    difference = study_avg - hist_avg,
    .groups = "drop"
  ) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, study_avg, hist_avg, difference)
# May: study is 1.16 deg higher than long term avg = "different"
# June: study is 0.216 deg lower than long term avg = "similar"
# July: study is 0.718 deg higher than long term avg = "similar"
# August: study is 0.0257 deg higher than long term avg = "similar"
# September: study is 0.940 deg higher than long term avg = "similar"
# October: study is 3.05 deg higher than long term avg = "different"

# Parameters
study_years <- 2015:2022
historical_cutoff <- 2015
months_to_compare <- 5:10  # May–October

comparison_temp <- temp_monthly %>%
  filter(month %in% months_to_compare) %>%
  group_by(month) %>%
  summarise(
    study_avg = mean(airtemp_avg[year %in% study_years], na.rm = TRUE),
    hist_avg  = mean(airtemp_avg[year < historical_cutoff], na.rm = TRUE),
    difference = study_avg - hist_avg,
    .groups = "drop"
  ) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, study_avg, hist_avg, difference)
# May: study is 1.16 deg higher than long term avg = "different"
# June: study is 0.216 deg lower than long term avg = "similar"
# July: study is 0.718 deg higher than long term avg = "similar"
# August: study is 0.0257 deg higher than long term avg = "similar"
# September: study is 0.940 deg higher than long term avg = "similar"
# October: study is 3.05 deg higher than long term avg = "different"

# By season:
# Parameters
study_years <- 2015:2022
historical_cutoff <- 2015

# Define seasons
summer_months <- c(6, 7, 8)       # June–August
winter_months <- c(11, 12, 1, 2)  # November–February

# Add season and season_year (Winter spans two years)
seasonal <- temp_monthly %>%
  mutate(
    season = case_when(
      month %in% summer_months ~ "Summer",
      month %in% winter_months ~ "Winter",
      TRUE ~ NA_character_
    ),
    season_year = if_else(season == "Winter" & month %in% c(11, 12),
                          year + 1,  # Nov–Dec belong to next year's winter
                          year)
  ) %>%
  filter(!is.na(season))

# Historical averages for each season
hist_means <- seasonal %>%
  filter(season_year < historical_cutoff) %>%
  group_by(season) %>%
  summarise(hist_avg = mean(airtemp_avg, na.rm = TRUE), .groups = "drop")

# Study period averages by season and year
study_by_year <- seasonal %>%
  filter(season_year %in% study_years) %>%
  group_by(season, season_year) %>%
  summarise(study_avg = mean(airtemp_avg, na.rm = TRUE), .groups = "drop")

# Combine and calculate difference
comparison_season <- study_by_year %>%
  left_join(hist_means, by = "season") %>%
  mutate(difference = study_avg - hist_avg) %>%
  arrange(season, season_year)

# Summer 2017 was 1.17 deg higher than historical average = "different"
# Winter 2015, 2016 was ~5 deg higher than historical average = "different"
# Winter 2018, 2019 was ~ 4 deg higher than historical average = "different"
  



#### February 7th, 2026 - SNOW ####
# Package ID: knb-lter-bnz.386.21 Cataloging System:https://pasta.edirepository.org.
# Data set title: Bonanza Creek LTER: Hourly Snow Pillow Measurements from 2007 to Present in the Caribou-Poker Creeks Research Watershed near Fairbanks, Alaska.
# Data set creator:  F Chapin -  
# Data set creator:  Roger Ruess -  
# Data set creator:  Michelle Mack -  
# Data set creator:    - Bonanza Creek LTER 
# Metadata Provider:    - Bonanza Creek LTER 
# Contact:    - Data Manager Bonanza Creek LTER  - uaf-bnz-im-team@alaska.edu
# Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-bnz/386/21/267487e89fcf2514be7cb4ea6b165b12" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "site",     
                 "date",     
                 "hour....",     
                 "measurment",     
                 "value",     
                 "unit",     
                 "flag"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$hour....)=="factor") dt1$hour.... <-as.numeric(levels(dt1$hour....))[as.integer(dt1$hour....) ]               
if (class(dt1$hour....)=="character") dt1$hour.... <-as.numeric(dt1$hour....)
if (class(dt1$measurment)!="factor") dt1$measurment<- as.factor(dt1$measurment)
if (class(dt1$value)=="factor") dt1$value <-as.numeric(levels(dt1$value))[as.integer(dt1$value) ]               
if (class(dt1$value)=="character") dt1$value <-as.numeric(dt1$value)
if (class(dt1$unit)!="factor") dt1$unit<- as.factor(dt1$unit)
if (class(dt1$flag)!="factor") dt1$flag<- as.factor(dt1$flag)

# Convert Missing Values to NA for non-dates

dt1$value <- ifelse((trimws(as.character(dt1$value))==trimws("NULL")),NA,dt1$value)               
suppressWarnings(dt1$value <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt1$value))==as.character(as.numeric("NULL"))),NA,dt1$value))

### Make datetime column ###
snow <- dt1 %>%
  mutate(
    # Convert hour.... (e.g., 1400) to "14:00:00"
    time_str = str_pad(hour...., width = 4, pad = "0") %>%
      str_replace("(\\d{2})(\\d{2})", "\\1:\\2:00"),
    
    # Combine date and time into datetime
    datetime = ymd_hms(paste(date, time_str), tz = "America/Los_Angeles")
  ) %>%
  select(-time_str) %>% 
  select(site, datetime, measurment, value, unit, flag)

# Plot to look at the data
# It looks like there are some negative values. Let me filter those out that are not real data
ggplot(snow, aes(x = datetime, y = value)) +
  geom_point() +
  theme_bw()

# Filter out negative values
snow <- snow %>% 
  filter(value > 0)

# Plot again
# This looks good. Lets do summary stats on this 
ggplot(snow, aes(x = datetime, y = value)) +
  geom_point() +
  theme_bw()

# Compare historical averages and study period

### set date limits for study period ###
yr.limit = 2023 # this should be the year AFTER your study
yr.of = 2015 # this should be the year OF your study

# Add a column for day, month and year 
snow <- snow %>% 
  mutate(day = day(datetime),
         month = month(datetime),
         year = year(datetime))

# Convert hourly data to daily averages
snow_daily <- snow %>% 
  select(day, month, year, value) %>% 
  group_by(day, month, year) %>%
  summarise(
    snow_avg = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# Convert hourly data to daily averages
snow_monthly <- snow_daily %>%
  group_by(year, month) %>%
  summarise(
    snow_avg = mean(snow_avg, na.rm = TRUE),
    .groups = "drop"
  )

# Convert mean monthly to mean annual #
snow_annual <- snow_monthly %>%
  group_by(year) %>%
  summarise(
    snow_avg = mean(snow_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  na.omit() %>% 
  filter(year < 2022)

# Plot annual temperature
ggplot(snow_annual, aes(year, snow_avg)) +
  geom_point() +
  theme_bw()

# compare study period to the long term avg for those months
# THIS study period = May-October 2015-2022
# I'm going to designate a difference of |10 cm| as different enough to report as non-similar
study_years <- 2015:2022
historical_cutoff <- 2015
months_to_compare <- c(9,10,11,12,1,2,3,4)  # September–April

comparison_snow <- snow_monthly %>%
  filter(month %in% months_to_compare) %>%
  group_by(month) %>%
  summarise(
    study_avg = mean(snow_avg[year %in% study_years], na.rm = TRUE),
    hist_avg  = mean(snow_avg[year < historical_cutoff], na.rm = TRUE),
    difference = study_avg - hist_avg,
    .groups = "drop"
  ) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, study_avg, hist_avg, difference)
# May: study is 1.13mm lower than long term avg = "similar"
# June: study is 6.80mm higher than long term avg = "similar"
# July: study is 18.9mm higher than long term avg = "different"
# August: study is 52.8mm higher than long term avg = "different"
# September: study is 24.5mm higher than long term avg = "different"
# October: study is 8.67mm higher than long term avg = "similar "



#### February 7th, 2026 - RAIN ####
# Package ID: knb-lter-bnz.167.25 Cataloging System:https://pasta.edirepository.org.
# Data set title: Bonanza Creek LTER:  Hourly Precipitation Measurements from 1993 to Present in the Caribou-Poker Creeks Research Watershed near Fairbanks, Alaska.
# Data set creator:  F Chapin -  
# Data set creator:  Roger Ruess -  
# Data set creator:  Michelle Mack -  
# Data set creator:    - Bonanza Creek LTER 
# Metadata Provider:    - Bonanza Creek LTER 
# Contact:    - Data Manager Bonanza Creek LTER  - uaf-bnz-im-team@alaska.edu
# Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-bnz/167/25/09c9c7110a9d5ce1263268802fec2367" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "site",     
                 "date",     
                 "hour....",     
                 "measurment",     
                 "value",     
                 "unit",     
                 "flag"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$site)!="factor") dt4$site<- as.factor(dt4$site)                                   
# attempting to convert dt4$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp4date<-as.Date(dt4$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt4[dt4$date != "",]) == length(tmp4date[!is.na(tmp4date)])){dt4$date <- tmp4date } else {print("Date conversion failed for dt4$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt4$hour....)=="factor") dt4$hour.... <-as.numeric(levels(dt4$hour....))[as.integer(dt4$hour....) ]               
if (class(dt4$hour....)=="character") dt4$hour.... <-as.numeric(dt4$hour....)
if (class(dt4$measurment)!="factor") dt4$measurment<- as.factor(dt4$measurment)
if (class(dt4$value)=="factor") dt4$value <-as.numeric(levels(dt4$value))[as.integer(dt4$value) ]               
if (class(dt4$value)=="character") dt4$value <-as.numeric(dt4$value)
if (class(dt4$unit)!="factor") dt4$unit<- as.factor(dt4$unit)
if (class(dt4$flag)!="factor") dt4$flag<- as.factor(dt4$flag)

# Convert Missing Values to NA for non-dates

dt4$value <- ifelse((trimws(as.character(dt4$value))==trimws("NULL")),NA,dt4$value)               
suppressWarnings(dt4$value <- ifelse(!is.na(as.numeric("NULL")) & (trimws(as.character(dt4$value))==as.character(as.numeric("NULL"))),NA,dt4$value))

### Make datetime column ###
rain <- dt4 %>%
  mutate(
    # Convert hour.... (e.g., 1400) to "14:00:00"
    time_str = str_pad(hour...., width = 4, pad = "0") %>%
      str_replace("(\\d{2})(\\d{2})", "\\1:\\2:00"),
    
    # Combine date and time into datetime
    datetime = ymd_hms(paste(date, time_str), tz = "America/Los_Angeles")
  ) %>%
  select(-time_str) %>% 
  select(site, datetime, measurment, value, unit, flag)

# Plot to look at the data
# It looks like there are some negative values. Let me filter those out that are not real data
ggplot(rain, aes(x = datetime, y = value)) +
  geom_point() +
  theme_bw()

# Filter out negative values
rain <- rain %>% 
  filter(value > 0)

# plot again 
# There are a few points that are way too high (>900) I will filter that out as well
ggplot(rain, aes(x = datetime, y = value)) +
  geom_point() +
  theme_bw()

# Filter out negative values
rain <- rain %>% 
  filter(value < 75)

# plot again 
# This looks good. Lets do stats based on this file 
ggplot(rain, aes(x = datetime, y = value)) +
  geom_point() +
  theme_bw()

# Compare historical averages and study period

### set date limits for study period ###
yr.limit = 2023 # this should be the year AFTER your study
yr.of = 2015 # this should be the year OF your study

# Add a column for day, month and year 
rain <- rain %>% 
  mutate(day = day(datetime),
         month = month(datetime),
         year = year(datetime)) %>% 
  filter(year > 1997)

# Convert hourly data to daily averages
rain_daily <- rain %>% 
  select(day, month, year, value) %>% 
  group_by(day, month, year) %>%
  summarise(
    rain_avg = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# Convert hourly data to daily averages
rain_monthly <- rain_daily %>%
  group_by(year, month) %>%
  summarise(
    rain_avg = sum(rain_avg, na.rm = TRUE),
    .groups = "drop"
  )

# Convert mean monthly to mean annual #
rain_annual <- rain_monthly %>%
  group_by(year) %>%
  summarise(
    rain_avg = sum(rain_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  na.omit()

# Plot annual temperature
ggplot(rain_annual, aes(year, rain_avg)) +
  geom_point() +
  theme_bw()

# compare study period to the long term avg for those months
# THIS study period = May-October 2015-2022
# I'm going to designate a difference of |50cm| as different enough to report as non-similar
study_years <- 2015:2022
historical_cutoff <- 2015
months_to_compare <- 5:10  # May–October

comparison_rain <- rain_monthly %>%
  filter(month %in% months_to_compare) %>%
  group_by(month) %>%
  summarise(
    study_avg = mean(rain_avg[year %in% study_years], na.rm = TRUE),
    hist_avg  = mean(rain_avg[year < historical_cutoff], na.rm = TRUE),
    difference = study_avg - hist_avg,
    .groups = "drop"
  ) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, study_avg, hist_avg, difference)
# January: study is 15.5cm higher than long term avg = "similar"
# February: study is 47.5cm higher than long term avg = "similar"
# March: study is 71.7cm higher than long term avg = "different"
# April: study is 88.8cm higher than long term avg = "different"
# September: study is 17.3cm higher than long term avg = "similar"
# October: study is 51.1cm higher than long term avg = "different"
# November: study is 68.6cm higher than long term avg = "different"
# December: study is 82.6cm higher than long term avg = "different"

####February 7th, 2026 CLIMATE PLOT ####
precip_file_list <- list.files(path="~/GitHub/Storms_clean_repo/Climate/SWE_Precip/",
                                  recursive=F,
                                  full.names=TRUE) # reading in individual storms by site 

precip <-do.call("rbind", lapply(precip_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

# rain 
rain <- read.csv("~/GitHub/Storms_clean_repo/Climate/Precip/Fairbanks_Precip_Snow.csv", skip = 5, header = T)
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

climate <- ggplot(climate.sum, aes(x = year, y = precip, fill = MONTH)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm')) +
  scale_fill_manual(values=cbPalette) +
  scale_x_continuous(breaks = unique(climate.sum$year), 
                     labels = function(x) { 
                       ifelse(x %% 5 == 0, x, "")  
                     }) + # Add this line to customize labeling
  theme(axis.text.x=element_text(size=20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 18))

ggsave(climate, path = here("plots", "Publication"),
       file = paste0("climate_figure_", Sys.Date(), ".jpg"),
       width = 12, height = 10, units = "in")


ggsave("total_precip_snotel.pdf",
       path = "~/GitHub/Storms_clean_repo/plots/Publication/",
       width = 10, height = 10)

#### summary stats ####
# mean annual
climate.sum.year <- climate.sum %>% 
  group_by(year) %>% 
  dplyr::summarise(precip = sum(precip, na.rm = TRUE)) # totaling by year and snow/rain 
mean(climate.sum.year$precip) # 328.6301

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
which(summer.sum$rain > 149)
# 1998 - 199.6430
# 2002 - 163.3220
# 2003 - 214.8850
# 2007 - 179.5780
# 2008 - 225.0440
# 2010 - 150.6220
# 2014 - 295.4020
# 2015 - 162.3070
# 2016 - 249.4290

# 2019 - 202.6920
# 2020 - 191.0080
# 2021 - 175.5140























#### ARCHIVE CODE #### 

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



######## PLOTS ###
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




