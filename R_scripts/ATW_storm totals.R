### Script to update storm total precip and other metrics for Jake's CQ dynamics
## Created by ATW 9/11/25

## calculate:
## 1) Storm total ppt
## 2) Storm intensity (total/duration)
## 3) cumulative ppt 1 week before storm
## 4) cumulative ppt 1 month before storm
## 5) time since peak flow on Chena (Jake already has?)

library(here)
library(tidyverse)
library(lubridate)
library(slider)

## Anna has already created .csv with total ppt for each year, read those in here
ppt15 <- read.csv(here("Climate", "Precip", "2015_totalppt.csv"))
ppt15<- ppt15 %>% mutate(site.ID = ifelse(Site =="French (33% permafrost)", "FRCH",
                                                ifelse(Site == "Moose (38% permafrost)","MOOS",
                                                       ifelse(Site == "Poker (25% permafrost)","POKE",
                                                                     # ifelse(Site == "STRT", "Stuart (31% permafrost)",
                                                                     "VAUL"))))
# ppt17 <- read.csv(here("Climate", "Precip", "2017_totalppt.csv"))
# ppt17<- ppt17 %>% mutate(site.ID = ifelse(Site =="French (33% permafrost)", "FRCH",
#                                           ifelse(Site == "Moose (38% permafrost)","MOOS",
#                                                  ifelse(Site == "Poker (25% permafrost)","POKE",
#                                                         # ifelse(Site == "STRT", "Stuart (31% permafrost)",
#                                                         "VAUL"))))
ppt18 <- read.csv(here("Climate", "Precip", "2018_totalppt.csv"))

ppt19 <- read.csv(here("Climate", "Precip", "2019_totalppt.csv"))
ppt19$site.ID=ppt19$Site

ppt20 <- read.csv(here("Climate", "Precip", "2020_totalppt.csv"))
ppt20$site.ID=ppt20$Site

ppt21 <- read.csv(here("Climate", "Precip", "2021_totalppt.csv"))
ppt21$site.ID=ppt21$Site

ppt22 <- read.csv(here("Climate", "Precip", "2022_totalppt.csv"))

#### For each ppt year, compute rolling sums including "today", then subtract today's ppt so the window is
# strictly BEFORE the storm start day -- previous cum 7 day and 30 day total 
### If there hasn't been 30 days of data before storm, use running total up to that point as 30 day total 
## filter out NA dates
ppt15<-ppt15%>%filter(., !is.na(Date))
ppt15$Date<-as.Date(ppt15$Date)
## set any NAs in ppt to 0
ppt15 <- ppt15 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))

ppt15 <- ppt15 %>%
  group_by(site.ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    # Includes today; .complete = TRUE ensures NA until enough history exists
    roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0),
    roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0),
    prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
    prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
  ) %>%
  select(site.ID, Date, ppt_tot, prev7, prev30) %>%
  ungroup()

# ppt17<-ppt17%>%filter(., !is.na(Date))
# ppt17$Date<-as.Date(ppt17$Date)
# ## set any NAs in ppt to 0
# ppt17 <- ppt17 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))
# 
# ppt17 <- ppt17 %>%
#   group_by(site.ID) %>%
#   arrange(Date, .by_group = TRUE) %>%
#   mutate(
#     # Includes today; .complete = TRUE ensures NA until enough history exists
#     roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0, .complete = TRUE),
#     roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0, .complete = TRUE),
#     prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
#     prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
#   ) %>%
#   select(site.ID, Date, ppt_tot, prev7, prev30) %>%
#   ungroup()

ppt18<-ppt18%>%filter(., !is.na(Date))
ppt18$Date<-as.Date(ppt18$Date)
## set any NAs in ppt to 0
ppt18 <- ppt18 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))

ppt18 <- ppt18 %>%
  # group_by(site.ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    # Includes today; .complete = TRUE ensures NA until enough history exists
    roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0),
    roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0),
    prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
    prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
  ) %>%
  select(Date, ppt_tot, prev7, prev30) %>%
  ungroup()

ppt19<-ppt19%>%filter(., !is.na(Date))
ppt19$Date<-as.Date(ppt19$Date)
## set any NAs in ppt to 0
ppt19 <- ppt19 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))

ppt19 <- ppt19 %>%
  group_by(site.ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    # Includes today; .complete = TRUE ensures NA until enough history exists
    roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0),
    roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0),
    prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
    prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
  ) %>%
  select(site.ID, Date, ppt_tot, prev7, prev30) %>%
  ungroup()

ppt20<-ppt20%>%filter(., !is.na(Date))
ppt20$Date<-as.Date(ppt20$Date)
## set any NAs in ppt to 0
ppt20 <- ppt20 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))

ppt20 <- ppt20 %>%
  group_by(site.ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    # Includes today; .complete = TRUE ensures NA until enough history exists
    roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0),
    roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0),
    prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
    prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
  ) %>%
  select(site.ID, Date, ppt_tot, prev7, prev30) %>%
  ungroup()

ppt21<-ppt21%>%filter(., !is.na(Date))
ppt21$Date<-as.Date(ppt21$Date)
## set any NAs in ppt to 0
ppt21 <- ppt21 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))

ppt21 <- ppt21 %>%
  group_by(site.ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    # Includes today; .complete = TRUE ensures NA until enough history exists
    roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0),
    roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0),
    prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
    prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
  ) %>%
  select(site.ID, Date, ppt_tot, prev7, prev30) %>%
  ungroup()

ppt22<-ppt22%>%filter(., !is.na(Date))
ppt22$Date<-as.Date(ppt22$Date)
## set any NAs in ppt to 0
ppt22 <- ppt22 %>% mutate(ppt_tot = parse_number(as.character(ppt_tot)))

ppt22 <- ppt22 %>%
  # group_by(site.ID) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    # Includes today; .complete = TRUE ensures NA until enough history exists
    roll_8_incl_today  = slide_index_dbl(ppt_tot, Date, sum, .before = 7,  .after = 0),
    roll_31_incl_today = slide_index_dbl(ppt_tot, Date, sum, .before = 30, .after = 0),
    prev7  = roll_8_incl_today  - ppt_tot,   # last 7 days EXCLUDING today
    prev30 = roll_31_incl_today - ppt_tot    # last 30 days EXCLUDING today
  ) %>%
  select(Date, ppt_tot, prev7, prev30) %>%
  ungroup()

## read in Jake's data with storm dates for each year 
###################################### 2015
### MOOS ####
MOOSstorm_file_list15 <- list.files(path = here("Storm_Events/2015/All_sites/"), 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms15<-do.call("rbind", lapply(MOOSstorm_file_list15, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms15$storm.num = c(rep("storm1", 383),
                          rep("storm2", 575),
                          rep("storm3a", 611),
                          
                          rep("storm4", 191),
                          rep("storm5", 455),
                          rep("storm6", 176))

MOOS_storms15$datetimeAK <- as.POSIXct(MOOS_storms15$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS_storms15$Date<-as.Date(MOOS_storms15$datetimeAK)
MOOS_storms15<-MOOS_storms15%>%select(., Date, site.ID, storm.num)
MOOS_storms15<-distinct(MOOS_storms15)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
MOOS15<-MOOS_storms15%>%left_join(ppt15, by=c('site.ID', 'Date'))
MOOS15<-MOOS15%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
MOOS15<-MOOS15%>%
  left_join(ppt15%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
MOOS15<-distinct(MOOS15)

### FRCH ####
FRCHstorm_file_list15 <- list.files(path = here("Storm_Events/2015/All_sites/"), 
                                    recursive=F, 
                                    pattern="FRCH", 
                                    full.names=TRUE)

FRCH_storms15<-do.call("rbind", lapply(FRCHstorm_file_list15, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms15$storm.num = c(rep("storm1", 287),
                          rep("storm2", 331),
                          rep("storm3", 383),
                          rep("storm4", 299),
                          rep("storm5a", 448),
                          
                          rep("storm6a", 1295),
                          
                          rep("storm7", 239))

FRCH_storms15$datetimeAK <- as.POSIXct(FRCH_storms15$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH_storms15$Date<-as.Date(FRCH_storms15$datetimeAK)
FRCH_storms15<-FRCH_storms15%>%select(., Date, site.ID, storm.num)
FRCH_storms15<-distinct(FRCH_storms15)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
FRCH15<-FRCH_storms15%>%left_join(ppt15, by=c('site.ID', 'Date'))
FRCH15<-FRCH15%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
FRCH15<-FRCH15%>%
  left_join(ppt15%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
FRCH15<-distinct(FRCH15)

## write out for each year and each site 
write.csv(MOOS15, file= 'Output_from_analysis/MOOS_2015_storm_totals.csv', row.names=TRUE)
write.csv(FRCH15, file= 'Output_from_analysis/FRCH_2015_storm_totals.csv', row.names=TRUE)

################################# 2018 
### MOOS ####
MOOSstorm_file_list18 <- list.files(path = "Storm_Events/2018/All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms18<-do.call("rbind", lapply(MOOSstorm_file_list18, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms18$storm.num = c(
  rep("storm10", 432),
  rep("storm11a", 420),
  
  rep("storm2a", 412),
  
  rep("storm3", 198),
  
  rep("storm5", 282),
  rep("storm6", 333),
  rep("storm7", 176),
  rep("storm8a", 178),
  
  rep("storm9", 106))

MOOS_storms18$datetimeAK <- as.POSIXct(MOOS_storms18$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS_storms18$Date<-as.Date(MOOS_storms18$datetimeAK)
MOOS_storms18<-MOOS_storms18%>%select(., Date, storm.num)
MOOS_storms18<-distinct(MOOS_storms18)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
MOOS18<-MOOS_storms18%>%left_join(ppt18, by=c('Date'))
MOOS18<-MOOS18%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
MOOS18<-MOOS18%>%
  left_join(ppt18%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
MOOS18<-distinct(MOOS18)

### FRCH ####
FRCHstorm_file_list18 <- list.files(path = "Storm_Events/2018/All_sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

FRCH_storms18<-do.call("rbind", lapply(FRCHstorm_file_list18, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms18$storm.num = c(rep("storm1", 142),
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


FRCH_storms18$datetimeAK <- as.POSIXct(FRCH_storms18$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH_storms18$Date<-as.Date(FRCH_storms18$datetimeAK)
FRCH_storms18<-FRCH_storms18%>%select(., Date, storm.num)
FRCH_storms18<-distinct(FRCH_storms18)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
FRCH18<-FRCH_storms18%>%left_join(ppt18, by=c('Date'))
FRCH18<-FRCH18%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
FRCH18<-FRCH18%>%
  left_join(ppt18%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
FRCH18<-distinct(FRCH18)

###### CARI
# CARI #### 
CARIstorm_file_list18 <- list.files(path = "Storm_Events/2018/All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)


CARI_storms18<-do.call("rbind", lapply(CARIstorm_file_list18, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms18$storm.num = c(rep("storm1", 317),
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

CARI_storms18$DateTime <- as.POSIXct(CARI_storms18$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI_storms18$Date<-as.Date(CARI_storms18$day)
CARI_storms18<-CARI_storms18%>%select(., Date, storm.num)
CARI_storms18<-distinct(CARI_storms18)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
CARI18<-CARI_storms18%>%left_join(ppt18, by=c('Date'))
CARI18<-CARI18%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
CARI18<-CARI18%>%
  left_join(ppt18%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
CARI18<-distinct(CARI18)
## write out for each year and each site 
write.csv(MOOS18, file= 'Output_from_analysis/MOOS_2018_storm_totals.csv', row.names=TRUE)
write.csv(FRCH18, file= 'Output_from_analysis/FRCH_2018_storm_totals.csv', row.names=TRUE)
write.csv(CARI18, file= 'Output_from_analysis/CARI_2018_storm_totals.csv', row.names=TRUE)

########################################## 2019
### MOOS ####
MOOSstorm_file_list19 <- list.files(path = here("Storm_Events/2019/All_sites/"), 
                                    recursive=F, 
                                    pattern="MOOS", 
                                    full.names=TRUE)

MOOS_storms19<-do.call("rbind", lapply(MOOSstorm_file_list19, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms19$storm.num = c(rep("storm1", 702),
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

MOOS_storms19$DateTime <- as.POSIXct(MOOS_storms19$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS_storms19$Date<-as.Date(MOOS_storms19$datetimeAK)
MOOS_storms19<-MOOS_storms19%>%select(., Date, site.ID, storm.num)
MOOS_storms19<-distinct(MOOS_storms19)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
MOOS19<-MOOS_storms19%>%left_join(ppt19, by=c('site.ID', 'Date'))
MOOS19<-MOOS19%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
MOOS19<-MOOS19%>%
  left_join(ppt19%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
MOOS19<-distinct(MOOS19)

### FRCH ####
FRCHstorm_file_list19 <- list.files(path = here("Storm_Events/2019/All_sites/"), 
                                    recursive=F, 
                                    pattern="FRCH", 
                                    full.names=TRUE)

FRCH_storms19<-do.call("rbind", lapply(FRCHstorm_file_list19, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms19$storm.num = c(rep("storm1", 993),
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

FRCH_storms19$datetimeAK <- as.POSIXct(FRCH_storms19$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH_storms19$Date<-as.Date(FRCH_storms19$datetimeAK)
FRCH_storms19<-FRCH_storms19%>%select(., Date, site.ID, storm.num)
FRCH_storms19<-distinct(FRCH_storms19)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
FRCH19<-FRCH_storms19%>%left_join(ppt19, by=c('site.ID', 'Date'))
FRCH19<-FRCH19%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
FRCH19<-FRCH19%>%
  left_join(ppt19%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
FRCH19<-distinct(FRCH19)

# POKE ####
POKEstorm_file_list19 <- list.files(path = here("Storm_Events/2019/All_sites/"), 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms19<-do.call("rbind", lapply(POKEstorm_file_list19, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms19$storm.num = c(rep("storm1", 103),
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

POKE_storms19$DateTime <- as.POSIXct(POKE_storms19$DateTime) 
POKE_storms19$Date<-as.Date(POKE_storms19$datetimeAK)
POKE_storms19<-POKE_storms19%>%select(., Date, site.ID, storm.num)
POKE_storms19<-distinct(POKE_storms19)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
POKE19<-POKE_storms19%>%left_join(ppt19, by=c('site.ID', 'Date'))
POKE19<-POKE19%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
POKE19<-POKE19%>%
  left_join(ppt19%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
POKE19<-distinct(POKE19)

# VAUL ####
VAULstorm_file_list19 <- list.files(path = here("Storm_Events/2019/All_sites/"), 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms19<-do.call("rbind", lapply(VAULstorm_file_list19, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms19$storm.num = c(rep("storm1", 191),
                          rep("storm2", 207),
                          rep("storm3", 191),
                          rep("storm4a", 307),
                          
                          rep("storm4c", 227),
                          rep("storm5", 275),
                          rep("storm6", 263),
                          rep("storm7", 107),
                          rep("storm8a", 455),
                          
                          rep("storm8c", 191))

VAUL_storms19$DateTime <- as.POSIXct(VAUL_storms19$DateTime) 
VAUL_storms19$Date<-as.Date(VAUL_storms19$datetimeAK)
VAUL_storms19<-VAUL_storms19%>%select(., Date, site.ID, storm.num)
VAUL_storms19<-distinct(VAUL_storms19)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
VAUL19<-VAUL_storms19%>%left_join(ppt19, by=c('site.ID', 'Date'))
VAUL19<-VAUL19%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
VAUL19<-VAUL19%>%
  left_join(ppt19%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
VAUL19<-distinct(VAUL19)

# STRT ####
STRTstorm_file_list19 <- list.files(path = here("Storm_Events/2019/All_sites/"),
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms19<-do.call("rbind", lapply(STRTstorm_file_list19, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms19$storm.num = c(rep("storm1", 638),
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

STRT_storms19$DateTime <- as.POSIXct(STRT_storms19$DateTime)
STRT_storms19$Date<-as.Date(STRT_storms19$datetimeAK)
STRT_storms19<-STRT_storms19%>%select(., Date, site.ID, storm.num)
STRT_storms19<-distinct(STRT_storms19)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
STRT19<-STRT_storms19%>%left_join(ppt19, by=c('site.ID', 'Date'))
STRT19<-STRT19%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
STRT19<-STRT19%>%
  left_join(ppt19%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
STRT19<-distinct(STRT19)

# CARI ####
CARIstorm_file_list19 <- list.files(path = here("Storm_Events/2019/All_sites/"),
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

CARI_storms19<-do.call("rbind", lapply(CARIstorm_file_list19, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms19$storm.num = c(rep("storm1", 371),
                          rep("storm2", 143),
                          rep("storm3", 83),
                          rep("storm4", 147),
                          rep("storm5", 135),
                          rep("storm6a", 319),
                          
                          rep("storm6c", 465),
                          rep("storm6d", 121),
                          rep("storm7a", 271),
                          
                          rep("storm8", 267))

CARI_storms19$DateTime <- as.POSIXct(CARI_storms19$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI_storms19$DateTime <- as.POSIXct(CARI_storms19$DateTime)
CARI_storms19$Date<-as.Date(CARI_storms19$datetimeAK)
CARI_storms19<-CARI_storms19%>%select(., Date, site.ID, storm.num)
CARI_storms19<-distinct(CARI_storms19)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
##### USE POKE PRECIP FOR CARI
ppt19$site.ID[ppt19$site.ID=="POKE"]="CARI"
CARI19<-CARI_storms19%>%left_join(ppt19, by=c('site.ID', 'Date'))
CARI19<-CARI19%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
CARI19<-CARI19%>%
  left_join(ppt19%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
CARI19<-distinct(CARI19)

## write out for each year and each site 
write.csv(MOOS19, file= 'Output_from_analysis/MOOS_2019_storm_totals.csv', row.names=TRUE)
write.csv(FRCH19, file= 'Output_from_analysis/FRCH_2019_storm_totals.csv', row.names=TRUE)
write.csv(POKE19, file= 'Output_from_analysis/POKE_2019_storm_totals.csv', row.names=TRUE)
write.csv(VAUL19, file= 'Output_from_analysis/VAUL_2019_storm_totals.csv', row.names=TRUE)
write.csv(STRT19, file= 'Output_from_analysis/STRT_2019_storm_totals.csv', row.names=TRUE)
write.csv(CARI19, file= 'Output_from_analysis/CARI_2019_storm_totals.csv', row.names=TRUE)

########################################## 2020
### MOOS ####
MOOSstorm_file_list20 <- list.files(path = here("Storm_Events/2020/All_sites/"), 
                                    recursive=F, 
                                    pattern="MOOS", 
                                    full.names=TRUE)

MOOS_storms20<-do.call("rbind", lapply(MOOSstorm_file_list20, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms20$storm.num = c(rep("storm1", 723),
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

MOOS_storms20$DateTime <- as.POSIXct(MOOS_storms20$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS_storms20$Date<-as.Date(MOOS_storms20$datetimeAK)
MOOS_storms20<-MOOS_storms20%>%select(., Date, site.ID, storm.num)
MOOS_storms20<-distinct(MOOS_storms20)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
MOOS20<-MOOS_storms20%>%left_join(ppt20, by=c('site.ID', 'Date'))
MOOS20<-MOOS20%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
MOOS20<-MOOS20%>%
  left_join(ppt20%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
MOOS20<-distinct(MOOS20)

### FRCH ####
FRCHstorm_file_list20 <- list.files(path = here("Storm_Events/2020/All_sites/"), 
                                    recursive=F, 
                                    pattern="FRCH", 
                                    full.names=TRUE)

FRCH_storms20<-do.call("rbind", lapply(FRCHstorm_file_list20, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms20$storm.num = c(rep("storm1", 487),
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
FRCH_storms20$datetimeAK <- as.POSIXct(FRCH_storms20$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH_storms20$Date<-as.Date(FRCH_storms20$datetimeAK)
FRCH_storms20<-FRCH_storms20%>%select(., Date, site.ID, storm.num)
FRCH_storms20<-distinct(FRCH_storms20)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
FRCH20<-FRCH_storms20%>%left_join(ppt20, by=c('site.ID', 'Date'))
FRCH20<-FRCH20%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
FRCH20<-FRCH20%>%
  left_join(ppt20%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
FRCH20<-distinct(FRCH20)

# POKE ####
POKEstorm_file_list20 <- list.files(path = here("Storm_Events/2020/All_sites/"), 
                                    recursive=F, 
                                    pattern="POKE", 
                                    full.names=TRUE)

POKE_storms20<-do.call("rbind", lapply(POKEstorm_file_list20, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms20$storm.num = c(
  
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

POKE_storms20$DateTime <- as.POSIXct(POKE_storms20$DateTime) 
POKE_storms20$Date<-as.Date(POKE_storms20$datetimeAK)
POKE_storms20<-POKE_storms20%>%select(., Date, site.ID, storm.num)
POKE_storms20<-distinct(POKE_storms20)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
POKE20<-POKE_storms20%>%left_join(ppt20, by=c('site.ID', 'Date'))
POKE20<-POKE20%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
POKE20<-POKE20%>%
  left_join(ppt20%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
POKE20<-distinct(POKE20)

# VAUL ####
VAULstorm_file_list20 <- list.files(path = here("Storm_Events/2020/All_sites/"), 
                                    recursive=F, 
                                    pattern="VAUL", 
                                    full.names=TRUE)

VAUL_storms20<-do.call("rbind", lapply(VAULstorm_file_list20, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms20$storm.num = c(rep("storm10", 195),
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

VAUL_storms20$DateTime <- as.POSIXct(VAUL_storms20$DateTime) 
VAUL_storms20$Date<-as.Date(VAUL_storms20$datetimeAK)
VAUL_storms20<-VAUL_storms20%>%select(., Date, site.ID, storm.num)
VAUL_storms20<-distinct(VAUL_storms20)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
VAUL20<-VAUL_storms20%>%left_join(ppt20, by=c('site.ID', 'Date'))
VAUL20<-VAUL20%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
VAUL20<-VAUL20%>%
  left_join(ppt20%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
VAUL20<-distinct(VAUL20)

# STRT ####
STRTstorm_file_list20 <- list.files(path = here("Storm_Events/2020/All_sites/"),
                                    recursive=F, 
                                    pattern="STRT", 
                                    full.names=TRUE)

STRT_storms20<-do.call("rbind", lapply(STRTstorm_file_list20, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms20$storm.num = c(rep("storm10", 246),
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

STRT_storms20$DateTime <- as.POSIXct(STRT_storms20$DateTime)
STRT_storms20$Date<-as.Date(STRT_storms20$datetimeAK)
STRT_storms20<-STRT_storms20%>%select(., Date, site.ID, storm.num)
STRT_storms20<-distinct(STRT_storms20)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
STRT20<-STRT_storms20%>%left_join(ppt20, by=c('site.ID', 'Date'))
STRT20<-STRT20%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
STRT20<-STRT20%>%
  left_join(ppt20%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
STRT20<-distinct(STRT20)

# CARI ####
CARIstorm_file_list20 <- list.files(path = here("Storm_Events/2020/All_sites/"),
                                    recursive=F, 
                                    pattern="CARI", 
                                    full.names=TRUE)

CARI_storms20<-do.call("rbind", lapply(CARIstorm_file_list20, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms20$storm.num = c(rep("storm1", 203),
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

CARI_storms20$DateTime <- as.POSIXct(CARI_storms20$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI_storms20$DateTime <- as.POSIXct(CARI_storms20$DateTime)
CARI_storms20$Date<-as.Date(CARI_storms20$datetimeAK)
CARI_storms20<-CARI_storms20%>%select(., Date, site.ID, storm.num)
CARI_storms20<-distinct(CARI_storms20)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
##### USE POKE PRECIP FOR CARI
ppt20$site.ID[ppt20$site.ID=="POKE"]="CARI"
CARI20<-CARI_storms20%>%left_join(ppt20, by=c('site.ID', 'Date'))
CARI20<-CARI20%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
CARI20<-CARI20%>%
  left_join(ppt20%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
CARI20<-distinct(CARI20)

## write out for each year and each site 
write.csv(MOOS20, file= 'Output_from_analysis/MOOS_2020_storm_totals.csv', row.names=TRUE)
write.csv(FRCH20, file= 'Output_from_analysis/FRCH_2020_storm_totals.csv', row.names=TRUE)
write.csv(POKE20, file= 'Output_from_analysis/POKE_2020_storm_totals.csv', row.names=TRUE)
write.csv(VAUL20, file= 'Output_from_analysis/VAUL_2020_storm_totals.csv', row.names=TRUE)
write.csv(STRT20, file= 'Output_from_analysis/STRT_2020_storm_totals.csv', row.names=TRUE)
write.csv(CARI20, file= 'Output_from_analysis/CARI_2020_storm_totals.csv', row.names=TRUE)


########################################## 2021
### MOOS ####
MOOSstorm_file_list21 <- list.files(path = here("Storm_Events/2021/All_sites/"), 
                                    recursive=F, 
                                    pattern="MOOS", 
                                    full.names=TRUE)

MOOS_storms21<-do.call("rbind", lapply(MOOSstorm_file_list21, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms21$storm.num = c(rep("storm1", 191),
                          rep("storm2", 251),
                          rep("storm3a", 115),
                          rep("storm3b", 359),
                          rep("storm4a", 415),
                          
                          rep("storm5a", 315),
                          
                          rep("storm6", 127),
                          rep("storm7", 259))

MOOS_storms21$DateTime <- as.POSIXct(MOOS_storms21$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS_storms21$Date<-as.Date(MOOS_storms21$datetimeAK)
MOOS_storms21<-MOOS_storms21%>%select(., Date, site.ID, storm.num)
MOOS_storms21<-distinct(MOOS_storms21)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
MOOS21<-MOOS_storms21%>%left_join(ppt21, by=c('site.ID', 'Date'))
MOOS21<-MOOS21%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
MOOS21<-MOOS21%>%
  left_join(ppt21%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
MOOS21<-distinct(MOOS21)

### FRCH ####
FRCHstorm_file_list21 <- list.files(path = here("Storm_Events/2021/All_sites/"), 
                                    recursive=F, 
                                    pattern="FRCH", 
                                    full.names=TRUE)

FRCH_storms21<-do.call("rbind", lapply(FRCHstorm_file_list21, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms21$storm.num = c(
  rep("storm2", 304),
  rep("storm3", 208),
  rep("storm4", 224),
  rep("storm5a", 444),
  
  rep("storm6a", 424),
  
  rep("storm7", 140),
  rep("storm8", 468))

FRCH_storms21$datetimeAK <- as.POSIXct(FRCH_storms21$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH_storms21$Date<-as.Date(FRCH_storms21$datetimeAK)
FRCH_storms21<-FRCH_storms21%>%select(., Date, site.ID, storm.num)
FRCH_storms21<-distinct(FRCH_storms21)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
FRCH21<-FRCH_storms21%>%left_join(ppt21, by=c('site.ID', 'Date'))
FRCH21<-FRCH21%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
FRCH21<-FRCH21%>%
  left_join(ppt21%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
FRCH21<-distinct(FRCH21)
FRCH21<-FRCH21%>%filter(., !is.na(site.ID))


# POKE ####
POKEstorm_file_list21 <- list.files(path = here("Storm_Events/2021/All_sites/"), 
                                    recursive=F, 
                                    pattern="POKE", 
                                    full.names=TRUE)

POKE_storms21<-do.call("rbind", lapply(POKEstorm_file_list21, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms21$storm.num = c(rep("storm1", 235),
                          rep("storm2", 191),
                          rep("storm3", 167),
                          rep("storm4", 191),
                          rep("storm5", 367),
                          rep("storm6", 159),
                          rep("storm7a", 451),
                          rep("storm7b", 263),
                          
                          rep("storm7d", 147))

POKE_storms21$DateTime <- as.POSIXct(POKE_storms21$DateTime) 
POKE_storms21$Date<-as.Date(POKE_storms21$datetimeAK)
POKE_storms21<-POKE_storms21%>%select(., Date, site.ID, storm.num)
POKE_storms21<-distinct(POKE_storms21)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
POKE21<-POKE_storms21%>%left_join(ppt21, by=c('site.ID', 'Date'))
POKE21<-POKE21%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
POKE21<-POKE21%>%
  left_join(ppt21%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
POKE21<-distinct(POKE21)

# VAUL ####
VAULstorm_file_list21 <- list.files(path = here("Storm_Events/2021/All_sites/"), 
                                    recursive=F, 
                                    pattern="VAUL", 
                                    full.names=TRUE)

VAUL_storms21<-do.call("rbind", lapply(VAULstorm_file_list21, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms21$storm.num = c(rep("storm1a", 375),
                          rep("storm1b", 267),
                          
                          rep("storm3", 667),
                          rep("storm4a", 427),
                          rep("storm4b", 319),
                          rep("storm5a", 715))

VAUL_storms21$DateTime <- as.POSIXct(VAUL_storms21$DateTime) 
VAUL_storms21$Date<-as.Date(VAUL_storms21$datetimeAK)
VAUL_storms21<-VAUL_storms21%>%select(., Date, site.ID, storm.num)
VAUL_storms21<-distinct(VAUL_storms21)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
VAUL21<-VAUL_storms21%>%left_join(ppt21, by=c('site.ID', 'Date'))
VAUL21<-VAUL21%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
VAUL21<-VAUL21%>%
  left_join(ppt21%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
VAUL21<-distinct(VAUL21)

# STRT ####
STRTstorm_file_list21 <- list.files(path = here("Storm_Events/2021/All_sites/"),
                                    recursive=F, 
                                    pattern="STRT", 
                                    full.names=TRUE)

STRT_storms21<-do.call("rbind", lapply(STRTstorm_file_list21, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms21$storm.num = c(rep("storm1a", 447),
                          
                          rep("storm2a", 307),
                          
                          rep("storm3", 127))

STRT_storms21$DateTime <- as.POSIXct(STRT_storms21$DateTime)
STRT_storms21$Date<-as.Date(STRT_storms21$datetimeAK)
STRT_storms21<-STRT_storms21%>%select(., Date, site.ID, storm.num)
STRT_storms21<-distinct(STRT_storms21)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
STRT21<-STRT_storms21%>%left_join(ppt21, by=c('site.ID', 'Date'))
STRT21<-STRT21%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
STRT21<-STRT21%>%
  left_join(ppt21%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
STRT21<-distinct(STRT21)

# CARI ####
CARIstorm_file_list21 <- list.files(path = here("Storm_Events/2021/All_sites/"),
                                    recursive=F, 
                                    pattern="CARI", 
                                    full.names=TRUE)

CARI_storms21<-do.call("rbind", lapply(CARIstorm_file_list21, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms21$storm.num = c(rep("storm1", 167),
                          rep("storm2", 139),
                          rep("storm3", 159),
                          rep("storm4", 127),
                          rep("storm5", 395),
                          rep("storm6", 395),
                          rep("storm7", 447),
                          rep("storm8", 323),
                          rep("storm9", 107),
                          rep("storm10", 243))

CARI_storms21$DateTime <- as.POSIXct(CARI_storms21$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI_storms21$DateTime <- as.POSIXct(CARI_storms21$DateTime)
CARI_storms21$Date<-as.Date(CARI_storms21$datetimeAK)
CARI_storms21<-CARI_storms21%>%select(., Date, site.ID, storm.num)
CARI_storms21<-distinct(CARI_storms21)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
##### USE POKE PRECIP FOR CARI
ppt21$site.ID[ppt21$site.ID=="POKE"]="CARI"
CARI21<-CARI_storms21%>%left_join(ppt21, by=c('site.ID', 'Date'))
CARI21<-CARI21%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
CARI21<-CARI21%>%
  left_join(ppt21%>%select(., site.ID, Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("site.ID", "storm_start"))%>%
  relocate(site.ID, storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
CARI21<-distinct(CARI21)

## write out for each year and each site 
write.csv(MOOS21, file= 'Output_from_analysis/MOOS_2021_storm_totals.csv', row.names=TRUE)
write.csv(FRCH21, file= 'Output_from_analysis/FRCH_2021_storm_totals.csv', row.names=TRUE)
write.csv(POKE21, file= 'Output_from_analysis/POKE_2021_storm_totals.csv', row.names=TRUE)
write.csv(VAUL21, file= 'Output_from_analysis/VAUL_2021_storm_totals.csv', row.names=TRUE)
write.csv(STRT21, file= 'Output_from_analysis/STRT_2021_storm_totals.csv', row.names=TRUE)
write.csv(CARI21, file= 'Output_from_analysis/CARI_2021_storm_totals.csv', row.names=TRUE)

########################################## 2022
### MOOS ####
MOOSstorm_file_list22 <- list.files(path = here("Storm_Events/2022/All_sites/"), 
                                    recursive=F, 
                                    pattern="MOOS", 
                                    full.names=TRUE)

MOOS_storms22<-do.call("rbind", lapply(MOOSstorm_file_list22, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))
MOOS_storms22$storm.num = c(rep("storm1", 199),
                          rep("storm2a", 223),
                          
                          rep("storm3", 99),
                          rep("storm4", 215))

MOOS_storms22$DateTime <- as.POSIXct(MOOS_storms22$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
MOOS_storms22$Date<-as.Date(MOOS_storms22$datetimeAK)
MOOS_storms22<-MOOS_storms22%>%select(., Date, site.ID, storm.num)
MOOS_storms22<-distinct(MOOS_storms22)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
MOOS22<-MOOS_storms22%>%left_join(ppt22, by=c('Date'))
MOOS22<-MOOS22%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
MOOS22<-MOOS22%>%
  left_join(ppt22%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
MOOS22<-distinct(MOOS22)

### FRCH ####
FRCHstorm_file_list22 <- list.files(path = here("Storm_Events/2022/All_sites/"), 
                                    recursive=F, 
                                    pattern="FRCH", 
                                    full.names=TRUE)

FRCH_storms22<-do.call("rbind", lapply(FRCHstorm_file_list22, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms22$storm.num = c(rep("storm1", 219),
                          rep("storm2", 235),
                          rep("storm3", 223),
                          rep("storm4", 167))

FRCH_storms22$datetimeAK <- as.POSIXct(FRCH_storms22$datetimeAK, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
FRCH_storms22$Date<-as.Date(FRCH_storms22$datetimeAK)
FRCH_storms22<-FRCH_storms22%>%select(., Date, site.ID, storm.num)
FRCH_storms22<-distinct(FRCH_storms22)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
FRCH22<-FRCH_storms22%>%left_join(ppt22, by=c('Date'))
FRCH22<-FRCH22%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
FRCH22<-FRCH22%>%
  left_join(ppt22%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
FRCH22<-distinct(FRCH22)

# POKE ####
POKEstorm_file_list22 <- list.files(path = here("Storm_Events/2022/All_sites/"), 
                                    recursive=F, 
                                    pattern="POKE", 
                                    full.names=TRUE)

POKE_storms22<-do.call("rbind", lapply(POKEstorm_file_list22, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms22$storm.num = c(rep("storm1", 139),
                          rep("storm2", 119),
                          rep("storm3", 95),
                          rep("storm4", 187))

POKE_storms22$DateTime <- as.POSIXct(POKE_storms22$datetimeAK) 
POKE_storms22$Date<-as.Date(POKE_storms22$datetimeAK)
POKE_storms22<-POKE_storms22%>%select(., Date, site.ID, storm.num)
POKE_storms22<-distinct(POKE_storms22)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
POKE22<-POKE_storms22%>%left_join(ppt22, by=c('Date'))
POKE22<-POKE22%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
POKE22<-POKE22%>%
  left_join(ppt22%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
POKE22<-distinct(POKE22)

# VAUL ####
VAULstorm_file_list22 <- list.files(path = here("Storm_Events/2022/All_sites/"), 
                                    recursive=F, 
                                    pattern="VAUL", 
                                    full.names=TRUE)

VAUL_storms22<-do.call("rbind", lapply(VAULstorm_file_list22, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms22$storm.num = c(rep("storm1", 127),
                          rep("storm2", 763))

VAUL_storms22$DateTime <- as.POSIXct(VAUL_storms22$datetimeAK) 
VAUL_storms22$Date<-as.Date(VAUL_storms22$datetimeAK)
VAUL_storms22<-VAUL_storms22%>%select(., Date, site.ID, storm.num)
VAUL_storms22<-distinct(VAUL_storms22)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
VAUL22<-VAUL_storms22%>%left_join(ppt22, by=c('Date'))
VAUL22<-VAUL22%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
VAUL22<-VAUL22%>%
  left_join(ppt22%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
VAUL22<-distinct(VAUL22)

# STRT ####
STRTstorm_file_list22 <- list.files(path = here("Storm_Events/2022/All_sites/"),
                                    recursive=F, 
                                    pattern="STRT", 
                                    full.names=TRUE)

STRT_storms22<-do.call("rbind", lapply(STRTstorm_file_list22, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms22$storm.num = c(rep("storm1", 103),
                          rep("storm2", 191),
                          rep("storm3", 107))

STRT_storms22$DateTime <- as.POSIXct(STRT_storms22$datetimeAK)
STRT_storms22$Date<-as.Date(STRT_storms22$datetimeAK)
STRT_storms22<-STRT_storms22%>%select(., Date, site.ID, storm.num)
STRT_storms22<-distinct(STRT_storms22)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
STRT22<-STRT_storms22%>%left_join(ppt22, by=c('Date'))
STRT22<-STRT22%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
STRT22<-STRT22%>%
  left_join(ppt22%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
STRT22<-distinct(STRT22)

# CARI ####
CARIstorm_file_list22 <- list.files(path = here("Storm_Events/2022/All_sites/"),
                                    recursive=F, 
                                    pattern="CARI", 
                                    full.names=TRUE)

CARI_storms22<-do.call("rbind", lapply(CARIstorm_file_list22, 
                                       read.csv, 
                                       check.names = FALSE,
                                       stringsAsFactors=FALSE, 
                                       header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms22$storm.num = c(rep("storm1", 231),
                          rep("storm2", 190),
                          rep("storm3", 204),
                          rep("storm4a", 119),
                          rep("storm4b", 167),
                          rep("storm5", 379),
                          rep("storm6", 91),
                          rep("storm7", 191),
                          rep("storm8", 103))

CARI_storms22$DateTime <- as.POSIXct(CARI_storms22$DateTime, tz = "America/Anchorage", format = "%Y-%m-%d %H:%M") 
CARI_storms22$DateTime <- as.POSIXct(CARI_storms22$DateTime)
CARI_storms22$Date<-as.Date(CARI_storms22$DateTime)
CARI_storms22<-CARI_storms22%>%select(., Date, site.ID, storm.num)
CARI_storms22<-distinct(CARI_storms22)

#### Join storms with ppt values, calculate total ppt and duration, intensity 
##### USE POKE PRECIP FOR CARI
# ppt22$site.ID[ppt22$site.ID=="POKE"]="CARI"
CARI22<-CARI_storms22%>%left_join(ppt22, by=c('Date'))
CARI22<-CARI22%>%
  group_by(storm.num)%>%
  summarise(storm_start=min(Date, na.rm=TRUE),
            storm_end=max(Date, na.rm=TRUE),
            duration_days=as.integer(storm_end-storm_start)+1L,
            total_storm_ppt=sum(ppt_tot, na.rm=TRUE),
            intensity=total_storm_ppt/duration_days,
            # site.ID=site.ID,
            .groups="drop"
  )
## then join with previous 7 day and 30 day total
CARI22<-CARI22%>%
  left_join(ppt22%>%select(., Date, prev7, prev30)%>%
              rename(storm_start=Date),
            by=c("storm_start"))%>%
  relocate(storm.num, storm_start, storm_end, duration_days, total_storm_ppt, intensity, prev7, prev30)
CARI22<-distinct(CARI22)

## write out for each year and each site 
write.csv(MOOS22, file= 'Output_from_analysis/MOOS_2022_storm_totals.csv', row.names=TRUE)
write.csv(FRCH22, file= 'Output_from_analysis/FRCH_2022_storm_totals.csv', row.names=TRUE)
write.csv(POKE22, file= 'Output_from_analysis/POKE_2022_storm_totals.csv', row.names=TRUE)
write.csv(VAUL22, file= 'Output_from_analysis/VAUL_2022_storm_totals.csv', row.names=TRUE)
write.csv(STRT22, file= 'Output_from_analysis/STRT_2022_storm_totals.csv', row.names=TRUE)
write.csv(CARI22, file= 'Output_from_analysis/CARI_2022_storm_totals.csv', row.names=TRUE)





