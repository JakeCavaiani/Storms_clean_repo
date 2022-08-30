#########################################################################################################
########################################### FI ##########################################################
#########################################################################################################
# Step 1) Load in the storm files from script 01_storm delineation
# Step 2) normalize discharge and concentration for each solute
# Step 3) Run FI function 
# Step 4) Run Q, solute file in FI function and cehck to see which ones run properly
# Step 5) plot against HI

########################################## 2018 ##########################################################

# load storm data to R #

### Q dat ###
storm_file_list_beta <- list.files(path="~/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI/", 
                                   recursive=F, 
                                   pattern=".csv", 
                                   full.names=TRUE)

storm_list_beta<-do.call("list", lapply(storm_file_list_beta, 
                                        read.csv, 
                                        stringsAsFactors=FALSE, 
                                        header=T, row.names=1))

storm_file_list_beta = sub("~/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//", storm_file_list_beta, replacement = "")
storm_file_list_beta = sub(".csv", storm_file_list_beta, replacement = "")
names(storm_list_beta) = storm_file_list_beta

for(i in 1:length(storm_list_beta)){
  storm_list_beta[[i]][["valuedatetime"]] = as.POSIXct(storm_list_beta[[i]][["valuedatetime"]],
                                                       "%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
} # changing character format into datetime 

#  organize storm data by site and solute # 5 for each storm 
CARI_storm_list_beta = storm_list_beta[c(1:65)] #65
FRCH_storm_list_beta = storm_list_beta[c(66:140)] #75
MOOS_storm_list_beta = storm_list_beta[c(141:220)] #80

CARI_Q_storm_list_beta = CARI_storm_list_beta[c(grep("Q", names(CARI_storm_list_beta)))]
FRCH_Q_storm_list_beta = FRCH_storm_list_beta[c(grep("Q", names(FRCH_storm_list_beta)))]
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


### solute data with bursts ###
FRCH_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_NO3_storm_list.RData")
FRCH_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_fDOM_storm_list.RData")
FRCH_SpCond_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_SpCond_storm_list.RData")
FRCH_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/FRCH_turb_storm_list.RData")

MOOS_NO3_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/MOOS_NO3_storm_list.RData")
MOOS_fDOM_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/MOOS_fDOM_storm_list.RData")
MOOS_SpCond_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/MOOS_SpCond_storm_list.RData")
MOOS_turb_storm_list = readRDS("~/Documents/Storms/Storm_Events/WithBurst/2018/MOOS_turb_storm_list.RData")

# normalize solute data #

### remove burst-complied data ###

for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]][["datavalue"]] = FRCH_NO3_storm_list[[i]][["nitrateuM"]]
  FRCH_NO3_storm_list[[i]][["nitrateuM"]] = NULL
  FRCH_NO3_storm_list[[i]][["site.ID"]] = NULL
}

for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue"]] = FRCH_fDOM_storm_list[[i]][["fDOMQSU"]]
  FRCH_fDOM_storm_list[[i]][["fDOMQSU"]] = NULL
}

for(i in 1:length(FRCH_SpCond_storm_list)){
  FRCH_SpCond_storm_list[[i]][["datavalue"]] = FRCH_SpCond_storm_list[[i]][["SpConduScm"]]
  FRCH_SpCond_storm_list[[i]][["SpConduScm"]] = NULL
}

for(i in 1:length(FRCH_turb_storm_list)){
  FRCH_turb_storm_list[[i]][["datavalue"]] = FRCH_turb_storm_list[[i]][["TurbidityFNU"]]
  FRCH_turb_storm_list[[i]][["TurbidityFNU"]] = NULL
}

for(i in 1:length(MOOS_NO3_storm_list)){
  MOOS_NO3_storm_list[[i]][["datavalue"]] = MOOS_NO3_storm_list[[i]][["nitrateuM"]]
  MOOS_NO3_storm_list[[i]][["nitrateuM"]] = NULL
  MOOS_NO3_storm_list[[i]][["site.ID"]] = NULL
}

for(i in 1:length(MOOS_fDOM_storm_list)){
  MOOS_fDOM_storm_list[[i]][["datavalue"]] = MOOS_fDOM_storm_list[[i]][["fDOMQSU"]]
  MOOS_fDOM_storm_list[[i]][["fDOMQSU"]] = NULL
}

for(i in 1:length(MOOS_SpCond_storm_list)){
  MOOS_SpCond_storm_list[[i]][["datavalue"]] = MOOS_SpCond_storm_list[[i]][["SpConduScm"]]
  MOOS_SpCond_storm_list[[i]][["SpConduScm"]] = NULL
}

for(i in 1:length(MOOS_turb_storm_list)){
  MOOS_turb_storm_list[[i]][["datavalue"]] = MOOS_turb_storm_list[[i]][["TurbidityFNU"]]
  MOOS_turb_storm_list[[i]][["TurbidityFNU"]] = NULL
}


### normalize burst data ###

for(i in 1:length(FRCH_NO3_storm_list)){
  FRCH_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_NO3_storm_list[[i]][["datavalue"]]-min(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue"]] = as.numeric(FRCH_fDOM_storm_list[[i]][["datavalue"]])
} # changing character format into numeric 


for(i in 1:length(FRCH_fDOM_storm_list)){
  FRCH_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_fDOM_storm_list[[i]][["datavalue"]]-min(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(FRCH_SpCond_storm_list)){
  FRCH_SpCond_storm_list[[i]][["datavalue"]] = as.numeric(FRCH_SpCond_storm_list[[i]][["datavalue"]])
} # changing character format into numeric 


for(i in 1:length(FRCH_SpCond_storm_list)){
  FRCH_SpCond_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_SpCond_storm_list[[i]][["datavalue"]]-min(FRCH_SpCond_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_SpCond_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_SpCond_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(FRCH_turb_storm_list)){
  FRCH_turb_storm_list[[i]][["datavalue"]] = as.numeric(FRCH_turb_storm_list[[i]][["datavalue"]])
} # changing character format into numeric 

for(i in 1:length(FRCH_turb_storm_list)){
  FRCH_turb_storm_list[[i]][["datavalue.norm"]] = 
    (FRCH_turb_storm_list[[i]][["datavalue"]]-min(FRCH_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(FRCH_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(FRCH_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_NO3_storm_list)){
  MOOS_NO3_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_NO3_storm_list[[i]][["datavalue"]]-min(MOOS_NO3_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_NO3_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_NO3_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_fDOM_storm_list)){
  MOOS_fDOM_storm_list[[i]][["datavalue"]] = as.numeric(MOOS_fDOM_storm_list[[i]][["datavalue"]])
} # changing character format into numeric 


for(i in 1:length(MOOS_fDOM_storm_list)){
  MOOS_fDOM_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_fDOM_storm_list[[i]][["datavalue"]]-min(MOOS_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_fDOM_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_fDOM_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_SpCond_storm_list)){
  MOOS_SpCond_storm_list[[i]][["datavalue"]] = as.numeric(MOOS_SpCond_storm_list[[i]][["datavalue"]])
} # changing character format into numeric 

for(i in 1:length(MOOS_SpCond_storm_list)){
  MOOS_SpCond_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_SpCond_storm_list[[i]][["datavalue"]]-min(MOOS_SpCond_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_SpCond_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_SpCond_storm_list[[i]][["datavalue"]], na.rm=T))
}

for(i in 1:length(MOOS_turb_storm_list)){
  MOOS_turb_storm_list[[i]][["datavalue"]] = as.numeric(MOOS_turb_storm_list[[i]][["datavalue"]])
} 

for(i in 1:length(MOOS_turb_storm_list)){
  MOOS_turb_storm_list[[i]][["datavalue.norm"]] = 
    (MOOS_turb_storm_list[[i]][["datavalue"]]-min(MOOS_turb_storm_list[[i]][["datavalue"]], na.rm=T))/
    (max(MOOS_turb_storm_list[[i]][["datavalue"]], na.rm=T)-min(MOOS_turb_storm_list[[i]][["datavalue"]], na.rm=T))
}

#fxn: calculate FI by difference and bootstrap CIs #

FI_diff = function(dat_Q, dat_response) {
  FI_dat = rbind(dat_response[as.POSIXct(dat_response$valuedatetime) == min(as.POSIXct(dat_response$valuedatetime)),], 
                 dat_response[as.POSIXct(dat_response$valuedatetime) == as.POSIXct(dat_Q$valuedatetime[dat_Q$datavalue.norm == max(dat_Q$datavalue.norm)]),])
  
  FI_dat$valuedatetime = as.character(as.POSIXct(FI_dat$valuedatetime))
  
  dat_Q$valuedatetime = as.character(as.POSIXct(dat_Q$valuedatetime))
  
  FI_dat = left_join(FI_dat, 
                     subset(dat_Q, select=c("valuedatetime", "datavalue.norm")),
                     by="valuedatetime")
  
  names(FI_dat) = c("valuedatetime", "datavalue", "datavalue.norm", "Q")
  
  FI_dat$datavalue.norm = as.numeric(FI_dat$datavalue.norm)
  FI_dat$Q = as.numeric(FI_dat$Q)
  
  FI = mean(FI_dat$datavalue.norm[FI_dat$valuedatetime == max(FI_dat$valuedatetime)]) - mean(FI_dat$datavalue.norm[FI_dat$valuedatetime == min(FI_dat$valuedatetime)])
  
  meanDiff = function(data, indices) { 
    d <- data[indices,] # allows boot to select sample
    m1 = mean(d$datavalue.norm[d$valuedatetime == max(d$valuedatetime)])
    m2 = mean(d$datavalue.norm[d$valuedatetime == min(d$valuedatetime)])
    m = m1 - m2
    return(m)
  }
  
  FI_boot = boot(FI_dat, meanDiff, R = 10000, strata = as.factor(FI_dat[,1]))
  FI_bootCI = boot.ci(FI_boot, type="bca")
  
  FI_bootCI = data.frame(cbind(FI_boot$t0, FI_bootCI[["bca"]][4], FI_bootCI[["bca"]][5]))
  names(FI_bootCI) = c("FI", "lower", "upper")
  
  FI_results = list(FI_dat, FI_bootCI)
  
  return(FI_results)
}

# calculate FI by difference and bootstrap CIs #

# FRCH # 
# NO3
FRCH_storm1_06_21_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_NO3`)
FRCH_storm2a_06_29_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_NO3`)
#FRCH_storm2b_07_04_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2b_07_04_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2b_07_04_NO3`)
FRCH_storm3_07_10_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm3_07_10_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm3_07_10_NO3`)
FRCH_storm4a_07_15_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4a_07_15_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4a_07_15_NO3`)
FRCH_storm4b_07_16_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4b_07_16_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4b_07_16_NO3`)
FRCH_storm5_08_04_NO3_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_Q`, FRCH_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_NO3`)

#fDOM
FRCH_storm1_06_21_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_fDOM`)
#FRCH_storm2a_06_29_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_fDOM`)
#FRCH_storm2b_07_04_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2b_07_04_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2b_07_04_fDOM`)
#FRCH_storm3_07_10_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm3_07_10_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm3_07_10_fDOM`)
#FRCH_storm4a_07_15_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4a_07_15_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4a_07_15_fDOM`)
#FRCH_storm4b_07_16_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4b_07_16_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm4b_07_16_fDOM`)
FRCH_storm5_08_04_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_fDOM`)
FRCH_storm6_08_13_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm6_08_13_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm6_08_13_fDOM`)
FRCH_storm7_08_23_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm7_08_23_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm7_08_23_fDOM`)
#FRCH_storm8a_08_26_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8a_08_26_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8a_08_26_fDOM`)
FRCH_storm8b_08_27_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8b_08_27_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8b_08_27_fDOM`)
FRCH_storm9_08_29_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm9_08_29_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm9_08_29_fDOM`)
FRCH_storm10_09_01_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm10_09_01_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm10_09_01_fDOM`)
FRCH_storm11a_09_22_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11a_09_22_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11a_09_22_fDOM`)
FRCH_storm11b_09_24_fDOM_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11b_09_24_Q`, FRCH_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11b_09_24_fDOM`)

#SPC
FRCH_storm1_06_21_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_SPC`)
#FRCH_storm2a_06_29_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm2a_06_29_SPC`)
#FRCH_storm2b_07_04_SPC_FI = FI_diff()
#FRCH_storm3_07_10_SPC_FI = FI_diff()
#FRCH_storm4a_07_15_SPC_FI = FI_diff()
#FRCH_storm4b_07_16_SPC_FI = FI_diff()
FRCH_storm5_08_04_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_SPC`)
FRCH_storm6_08_13_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm6_08_13_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm6_08_13_SPC`)
FRCH_storm7_08_23_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm7_08_23_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm7_08_23_SPC`)
#FRCH_storm8a_08_26_SPC_FI = FI_diff()
FRCH_storm8b_08_27_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8b_08_27_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8b_08_27_SPC`) # 0.0277777777777762
FRCH_storm9_08_29_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm9_08_29_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm9_08_29_SPC`)
FRCH_storm10_09_01_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm10_09_01_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm10_09_01_SPC`)
#FRCH_storm11a_09_22_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11a_09_22_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11a_09_22_SPC`)
FRCH_storm11b_09_24_SPC_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11b_09_24_Q`, FRCH_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11b_09_24_SPC`)

#turb
FRCH_storm1_06_21_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm1_06_21_Turb`)
#FRCH_storm2a_06_29_turb_FI = FI_diff()
##FRCH_storm2b_07_04_turb_FI = FI_diff()
#FRCH_storm3_07_10_turb_FI = FI_diff()
#FRCH_storm4a_07_15_turb_FI = FI_diff()
#FRCH_storm4b_07_16_turb_FI = FI_diff()
FRCH_storm5_08_04_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm5_08_04_Turb`)
FRCH_storm6_08_13_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm6_08_13_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm6_08_13_Turb`)
FRCH_storm7_08_23_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm7_08_23_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm7_08_23_Turb`)
FRCH_storm8a_08_26_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8a_08_26_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8a_08_26_Turb`)
#FRCH_storm8b_08_27_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8b_08_27_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm8b_08_27_Turb`)
FRCH_storm9_08_29_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm9_08_29_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm9_08_29_Turb`)
FRCH_storm10_09_01_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm10_09_01_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm10_09_01_Turb`)
FRCH_storm11a_09_22_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11a_09_22_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11a_09_22_Turb`)
FRCH_storm11b_09_24_turb_FI = FI_diff(FRCH_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11b_09_24_Q`, FRCH_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//FRCH_storm11b_09_24_Turb`)


# MOOS # 
# NO3
MOOS_storm1_06_21_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_NO3`)
#MOOS_storm2a_06_29_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2a_06_29_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2a_06_29_NO3`)
#MOOS_storm2b_07_01_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2b_07_01_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2b_07_01_NO3`)
#MOOS_storm2c_07_04_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2c_07_04_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2c_07_04_NO3`)
MOOS_storm3_07_09_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm3_07_09_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm3_07_09_NO3`)
MOOS_storm4_07_15_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_NO3`)
MOOS_storm5_08_04_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_NO3`)
MOOS_storm6_08_13_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_NO3`)
MOOS_storm7_08_23_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_NO3`)
MOOS_storm8a_08_26_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_NO3`)
MOOS_storm8b_08_27_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_NO3`)
MOOS_storm9_08_29_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_NO3`)
MOOS_storm10_09_01_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_NO3`)
MOOS_storm11a_09_22_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_NO3`)
MOOS_storm11b_09_23_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_NO3`)
MOOS_storm12_09_24_NO3_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_Q`, MOOS_NO3_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_NO3`)

#fDOM
MOOS_storm1_06_21_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_fDOM`)
#MOOS_storm2a_06_29_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2a_06_29_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2a_06_29_fDOM`)
#MOOS_storm2b_07_01_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2b_07_01_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2b_07_01_fDOM`)
#MOOS_storm2c_07_04_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2c_07_04_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm2c_07_04_fDOM`)
#MOOS_storm3_07_09_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm3_07_09_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm3_07_09_fDOM`)
#MOOS_storm4_07_15_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_fDOM`)
MOOS_storm5_08_04_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_fDOM`) # 0.254818972259281
MOOS_storm6_08_13_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_fDOM`) # 0.139685321981553
MOOS_storm7_08_23_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_fDOM`) # 0.150231221097363
#MOOS_storm8a_08_26_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_fDOM`)
MOOS_storm8b_08_27_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_fDOM`) # 0.206574024585783
#MOOS_storm9_08_29_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_fDOM`)
#MOOS_storm10_09_01_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_fDOM`)
MOOS_stor11a_09_22_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_fDOM`) # 0.569246097641979
MOOS_storm11b_09_23_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_fDOM`) # 0.346520146520147
MOOS_storm12_09_24_fDOM_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_Q`, MOOS_fDOM_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_fDOM`)

#SPC
MOOS_storm1_06_21_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_SPC`)
#MOOS_storm2a_06_29_SPC_FI
#MOOS_storm2b_07_01_SPC_FI 
#MOOS_storm2c_07_04_SPC_FI
#MOOS_storm3_07_09_SPC_FI
MOOS_storm4_07_15_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_SPC`)
MOOS_storm5_08_04_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_SPC`)
MOOS_storm6_08_13_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_SPC`)
MOOS_storm7_08_23_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_SPC`)
MOOS_storm8a_08_26_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_SPC`)
MOOS_storm8b_08_27_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_SPC`)
MOOS_storm9_08_29_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_SPC`) # 0.00893854748603373
MOOS_storm10_09_01_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_SPC`)
MOOS_stor11a_09_22_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_SPC`)
MOOS_storm11b_09_23_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_SPC`)
MOOS_storm12_09_24_SPC_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_Q`, MOOS_SpCond_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_SPC`) # 0.00101214574898778

#turb
MOOS_storm1_06_21_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm1_06_21_Turb`)
MOOS_storm2a_06_29_turb_FI = 
  MOOS_storm2b_07_01_turb_FI 
MOOS_storm2c_07_04_turb_FI
#MOOS_storm3_07_09_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm3_07_09_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm3_07_09_Turb`)
#MOOS_storm4_07_15_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm4_07_15_Turb`)
MOOS_storm5_08_04_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm5_08_04_Turb`) #0.330552981155391
MOOS_storm6_08_13_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm6_08_13_Turb`) # 0.354272517321016
MOOS_storm7_08_23_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm7_08_23_Turb`) # 0.45563465879493
MOOS_storm8a_08_26_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8a_08_26_Turb`) # 0.451545433418545
MOOS_storm8b_08_27_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm8b_08_27_Turb`) # 0.0596426084938501
MOOS_storm9_08_29_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm9_08_29_Turb`) # 0.216361788617886
MOOS_storm10_09_01_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm10_09_01_Turb`) # 0.21969732984708 
MOOS_stor11a_09_22_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11a_09_22_Turb`) # 0.395529380270379
MOOS_storm11b_09_23_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm11b_09_23_Turb`) # 0.0449070631970259
MOOS_storm12_09_24_turb_FI = FI_diff(MOOS_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_Q`, MOOS_turb_storm_list$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//MOOS_storm12_09_24_Turb`) # 0.0935303526097228

# CARI # 
# NO3
#CARI_storm1_06_10_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_NO3`)
#CARI_storm2_06_21_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_NO3`)
#CARI_storm3_06_29_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_NO3`)
#CARI_storm4a_06_30_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_NO3`)
#CARI_storm4b_07_01_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_NO3`)
#CARI_storm5a_08_04_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_NO3`)
#CARI_storm5b_08_05_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_NO3`)
CARI_storm5c_08_06_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_Q`,CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_NO3`) #0.0465116279069774
#CARI_storm6_08_13_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_NO3`)
#CARI_storm7_08_21_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_NO3`)
#CARI_storm8_08_24_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_NO3`)
#CARI_storm9_08_26_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_NO3`)
#CARI_storm10_08_30_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_NO3`)
#CARI_storm11_09_02_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_NO3`)
#CARI_storm12a_09_20_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_NO3`)
#CARI_storm12b_09_25_NO3_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_Q`, CARI_NO3_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_NO3`)

# fDOM
#CARI_storm1_06_10_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_Q`,CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_fDOM`) 
CARI_storm2_06_21_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_fDOM`) # 0.453265044814341
CARI_storm3_06_29_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_fDOM`) #0.654370489174017
#CARI_storm4a_06_30_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_fDOM`) 
#CARI_storm4b_07_01_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_fDOM`) 
CARI_storm5a_08_04_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_fDOM`) #0.161242603550296
CARI_storm5b_08_05_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_fDOM`) #0.572166514737162
CARI_storm5c_08_06_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_Q`,  CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_fDOM`) #0.316719453242207
CARI_storm6_08_13_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_fDOM`) #0.777721416252409
#CARI_storm7_08_21_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_fDOM`) w is infinite
CARI_storm8_08_24_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_fDOM`) #0.608794197642793
CARI_storm9_08_26_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_fDOM`) #0.884684684684685
CARI_storm10_08_30_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_fDOM`) # 0.650042782055983
CARI_storm11_09_02_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_fDOM`) #0.814095449500555
#CARI_storm12a_09_20_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_fDOM`) # missing value where TRUE/FALSE needed
CARI_storm12b_09_25_fDOM_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_Q`, CARI_fDOM_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_fDOM`) #0.0677162175799779


# SPC
#CARI_storm1_06_10_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_SPC`)
CARI_storm2_06_21_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_SPC`) # 0.24927536231884
##CARI_storm3_06_29_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_SPC`) # w is inifinite
#CARI_storm4a_06_30_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_SPC`) #w is inifinite
#CARI_storm4b_07_01_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_SPC`)#w is inifinite
#CARI_storm5a_08_04_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_SPC`)# w is infnite
#CARI_storm5b_08_05_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_SPC`) # w is infinite
#CARI_storm5c_08_06_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_SPC`)#w is inifinite
#CARI_storm6_08_13_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_SPC`)#w is inifinite
#CARI_storm7_08_21_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_SPC`)#w is inifinite
#CARI_storm8_08_24_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_SPC`)#w is inifinite
#CARI_storm9_08_26_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_SPC`)#w is inifinite
#CARI_storm10_08_30_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_SPC`)#w is inifinite
#CARI_storm11_09_02_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_SPC`)#w is inifinite
#CARI_storm12a_09_20_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_SPC`) # missing values 
CARI_storm12b_09_25_SPC_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_Q`, CARI_SpCond_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_SPC`) # 0.058107696988135

# turb
CARI_storm1_06_10_SP_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm1_06_10_Turb`) #0.27755905511811
#CARI_storm2_06_21_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm2_06_21_Turb`)
CARI_storm3_06_29_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm3_06_29_Turb`)#0.778284671532847
CARI_storm4a_06_30_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4a_06_30_Turb`) # 0.0558476295886368
CARI_storm4b_07_01_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm4b_07_01_Turb`) #0.390557939914163
CARI_storm5a_08_04_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5a_08_04_Turb`)#0.689542483660131
CARI_storm5b_08_05_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5b_08_05_Turb`) #0.702678571428571
CARI_storm5c_08_06_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm5c_08_06_Turb`) #0.528497409326425
CARI_storm6_08_13_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm6_08_13_Turb`) #0.315496098104794
CARI_storm7_08_21_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm7_08_21_Turb`)#0.308176100628931
CARI_storm8_08_24_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm8_08_24_Turb`) # 0.725738396624473
CARI_storm9_08_26_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm9_08_26_Turb`) # 0.431818181818182
CARI_storm10_08_30_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm10_08_30_Turb`) #0.0079810604885815
CARI_storm11_09_02_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm11_09_02_Turb`) #0.011472275334608
#CARI_storm12a_09_20_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12a_09_20_Turb`)
CARI_storm12b_09_25_turb_FI = FI_diff(CARI_Q_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_Q`, CARI_turb_storm_list_beta$`/Users/jakecavaiani/Documents/Storms/Storm_Events/2018/FRCH_MOOS_CARI//CARI_storm12b_09_25_Turb`) # 0.0937499999999999


# gather results and save #

FI_results = rbind(
  c("FRCH_storm1_06_21_NO3_FI",FRCH_storm1_06_21_NO3_FI[[2]]),
  c("FRCH_storm2a_06_29_NO3_FI",FRCH_storm2a_06_29_NO3_FI[[2]]),
  c("FRCH_storm2b_07_01_NO3_FI",NA, NA, NA),
  c("FRCH_storm2c_07_04_NO3_FI",NA, NA, NA),
  c("FRCH_storm3_07_10_NO3_FI",FRCH_storm3_07_10_NO3_FI[[2]]),
  c("FRCH_storm4a_07_15_NO3_FI",FRCH_storm4a_07_15_NO3_FI[[2]]),
  c("FRCH_storm4b_07_16_NO3_FI",FRCH_storm4b_07_16_NO3_FI[[2]]),
  c("FRCH_storm5_08_04_NO3_FI",FRCH_storm5_08_04_NO3_FI[[2]]),
  c("FRCH_storm6_08_13_NO3_FI",NA, NA, NA),
  c("FRCH_storm7_08_23_NO3_FI",NA, NA, NA),
  c("FRCH_storm8b_08_27_NO3_FI",NA, NA, NA),
  c("FRCH_storm9_08_29_NO3_FI",NA, NA, NA),
  c("FRCH_storm10_09_01_NO3_FI",NA, NA, NA),
  c("FRCH_storm11a_09_22_NO3_FI",NA, NA, NA),
  c("FRCH_storm11b_09_24_NO3_FI",NA, NA, NA),
  
  c("FRCH_storm1_06_21_fDOM_FI",FRCH_storm1_06_21_fDOM_FI[[2]]),
  c("FRCH_storm2a_06_29_fDOM_FI",NA, NA, NA),
  c("FRCH_storm2b_07_01_fDOM_FI",NA, NA, NA),
  c("FRCH_storm2c_07_04_fDOM_FI",NA, NA, NA),
  c("FRCH_storm3_07_10_fDOM_FI",NA, NA, NA),
  c("FRCH_storm4a_07_15_fDOM_FI",NA, NA, NA),
  c("FRCH_storm4b_07_16_fDOM_FI",NA, NA, NA),
  c("FRCH_storm5_08_04_fDOM_FI",FRCH_storm5_08_04_fDOM_FI[[2]]),
  c("FRCH_storm6_08_13_fDOM_FI",FRCH_storm6_08_13_fDOM_FI[[2]]),
  c("FRCH_storm7_08_23_fDOM_FI",FRCH_storm7_08_23_fDOM_FI[[2]]),
  c("FRCH_storm8a_08_26_fDOM_FI",NA, NA, NA),
  c("FRCH_storm8b_08_27_fDOM_FI",FRCH_storm8b_08_27_fDOM_FI[[2]]),
  c("FRCH_storm9_08_29_fDOM_FI",FRCH_storm9_08_29_fDOM_FI[[2]]),
  c("FRCH_storm10_09_01_fDOM_FI",FRCH_storm10_09_01_fDOM_FI[[2]]),
  c("FRCH_storm11a_09_22_fDOM_FI",FRCH_storm11a_09_22_fDOM_FI[[2]]),
  c("FRCH_storm11b_09_24_fDOM_FI",FRCH_storm11b_09_24_fDOM_FI[[2]]),
  
  c("FRCH_storm1_06_21_SPC_FI",FRCH_storm1_06_21_SPC_FI[[2]]),
  c("FRCH_storm2a_06_29_SPC_FI",NA, NA, NA),
  c("FRCH_storm2b_07_01_SPC_FI",NA, NA, NA),
  c("FRCH_storm2c_07_04_SPC_FI",NA, NA, NA),
  c("FRCH_storm3_07_10_SPC_FI",NA, NA, NA),
  c("FRCH_storm4a_07_15_SPC_FI",NA, NA, NA),
  c("FRCH_storm4b_07_16_SPC_FI",NA, NA, NA),
  c("FRCH_storm5_08_04_SPC_FI",FRCH_storm5_08_04_SPC_FI[[2]]),
  c("FRCH_storm6_08_13_SPC_FI",FRCH_storm6_08_13_SPC_FI[[2]]),
  c("FRCH_storm7_08_23_SPC_FI",FRCH_storm7_08_23_SPC_FI[[2]]),
  c("FRCH_storm8a_08_26_SPC_FI",NA, NA, NA),
  c("FRCH_storm8b_08_27_SPC_FI",0.0277777777777762, NA, NA),
  c("FRCH_storm9_08_29_SPC_FI",FRCH_storm9_08_29_SPC_FI[[2]]),
  c("FRCH_storm10_09_01_SPC_FI",FRCH_storm10_09_01_SPC_FI[[2]]),
  c("FRCH_storm11b_09_24_SPC_FI",FRCH_storm11b_09_24_SPC_FI[[2]]),
  
  c("FRCH_storm1_06_21_turb_FI",FRCH_storm1_06_21_turb_FI[[2]]),
  c("FRCH_storm2a_06_29_turb_FI",NA, NA, NA),
  c("FRCH_storm2b_07_01_turb_FI",NA, NA, NA),
  c("FRCH_storm2c_07_04_turb_FI",NA, NA, NA),
  c("FRCH_storm3_07_10_turb_FI",NA, NA, NA),
  c("FRCH_storm4a_07_15_turb_FI",NA, NA, NA),
  c("FRCH_storm4b_07_16_turb_FI",NA, NA, NA),
  c("FRCH_storm5_08_04_turb_FI",FRCH_storm5_08_04_turb_FI[[2]]),
  c("FRCH_storm6_08_13_turb_FI",FRCH_storm6_08_13_turb_FI[[2]]),
  c("FRCH_storm7_08_23_turb_FI",FRCH_storm7_08_23_turb_FI[[2]]),
  c("FRCH_storm8a_08_26_turb_FI",NA, NA, NA),
  c("FRCH_storm8b_08_27_turb_FI",NA, NA, NA),
  c("FRCH_storm9_08_29_turb_FI",FRCH_storm9_08_29_turb_FI[[2]]),
  c("FRCH_storm10_09_01_turb_FI",FRCH_storm10_09_01_turb_FI[[2]]),
  c("FRCH_storm11a_09_22_turb_FI",FRCH_storm11a_09_22_turb_FI[[2]]),
  c("FRCH_storm11b_09_24_turb_FI",FRCH_storm11b_09_24_turb_FI[[2]]),
  c("MOOS_storm1_06_21_NO3_FI",MOOS_storm1_06_21_NO3_FI[[2]]),
  c("MOOS_storm2a_06_29_NO3_FI",NA, NA, NA),
  c("MOOS_storm2b_07_01_NO3_FI",NA, NA, NA),
  c("MOOS_storm2c_07_04_NO3_FI",NA, NA, NA),
  c("MOOS_storm3_07_09_NO3_FI",MOOS_storm3_07_09_NO3_FI[[2]]),
  c("MOOS_storm4_07_15_NO3_FI",MOOS_storm4_07_15_NO3_FI[[2]]),
  c("MOOS_storm5_08_04_NO3_FI",MOOS_storm5_08_04_NO3_FI[[2]]),
  c("MOOS_storm6_08_13_NO3_FI",MOOS_storm6_08_13_NO3_FI[[2]]),
  c("MOOS_storm7_08_23_NO3_FI",MOOS_storm7_08_23_NO3_FI[[2]]),
  c("MOOS_storm8a_08_26_NO3_FI",MOOS_storm8a_08_26_NO3_FI[[2]]),
  c("MOOS_storm8b_08_27_NO3_FI",MOOS_storm8b_08_27_NO3_FI[[2]]),
  c("MOOS_storm9_08_29_NO3_FI",MOOS_storm9_08_29_NO3_FI[[2]]),
  c("MOOS_storm10_09_01_NO3_FI",MOOS_storm10_09_01_NO3_FI[[2]]),
  c("MOOS_storm11a_09_22_NO3_FI",MOOS_storm11a_09_22_NO3_FI[[2]]),
  c("MOOS_storm11b_09_23_NO3_FI",MOOS_storm11b_09_23_NO3_FI[[2]]),
  c("MOOS_storm12_09_24_NO3_FI",MOOS_storm12_09_24_NO3_FI[[2]]),
  c("MOOS_storm1_06_21_fDOM_FI",MOOS_storm1_06_21_fDOM_FI[[2]]),
  c("MOOS_storm2a_06_29_fDOM_FI",NA, NA, NA),
  c("MOOS_storm2b_07_01_fDOM_FI",NA, NA, NA),
  c("MOOS_storm2c_07_04_fDOM_FI",NA, NA, NA),
  c("MOOS_storm3_07_09_fDOM_FI",NA, NA, NA),
  c("MOOS_storm4_07_15_fDOM_FI",NA, NA, NA),
  c("MOOS_storm5_08_04_fDOM_FI",0.254818972259281, NA,NA),
  c("MOOS_storm6_08_13_fDOM_FI",0.139685321981553, NA, NA),
  c("MOOS_storm7_08_23_fDOM_FI",0.150231221097363, NA, NA),
  c("MOOS_storm8a_08_26_fDOM_FI",NA,NA,NA),
  c("MOOS_storm8b_08_27_fDOM_FI",0.206574024585783, NA, NA),
  c("MOOS_storm9_08_29_fDOM_FI",NA,NA,NA),
  c("MOOS_storm10_09_01_fDOM_FI",NA,NA,NA),
  c("MOOS_storm11a_09_22_fDOM_FI",0.569246097641979, NA, NA),
  c("MOOS_storm11b_09_23_fDOM_FI",0.346520146520147, NA, NA),
  c("MOOS_storm12_09_24_fDOM_FI",NA,NA,NA),
  c("MOOS_storm1_06_21_SPC_FI", MOOS_storm1_06_21_SPC_FI[[2]]),
  c("MOOS_storm2a_06_29_SPC_FI",NA, NA, NA),
  c("MOOS_storm2b_07_01_SPC_FI",NA, NA, NA),
  c("MOOS_storm2c_07_04_SPC_FI",NA, NA, NA),
  c("MOOS_storm3_07_09_SPC_FI",NA, NA, NA),
  c("MOOS_storm4_07_15_SPC_FI",NA, NA, NA),
  c("MOOS_storm5_08_04_SPC_FI",NA, NA, NA),
  c("MOOS_storm6_08_13_SPC_FI",NA, NA, NA),
  c("MOOS_storm7_08_23_SPC_FI",NA, NA, NA),
  c("MOOS_storm8a_08_26_SPC_FI",NA,NA,NA),
  c("MOOS_storm8b_08_27_SPC_FI",NA, NA, NA),
  c("MOOS_storm9_08_29_SPC_FI", 0.00893854748603373, NA, NA),
  c("MOOS_storm10_09_01_SPC_FI",NA, NA, NA),
  c("MOOS_storm11a_09_22_SPC_FI",NA, NA, NA),
  c("MOOS_storm11b_09_23_SPC_FI",NA, NA, NA),
  c("MOOS_storm12_09_24_SPC_FI", 0.00101214574898778, NA, NA),
  c("MOOS_storm1_06_21_turb_FI",MOOS_storm1_06_21_NO3_FI[[2]]),
  c("MOOS_storm2a_06_29_turb_FI",NA, NA, NA),
  c("MOOS_storm2b_07_01_turb_FI",NA, NA, NA),
  c("MOOS_storm2c_07_04_turb_FI",NA, NA, NA),
  c("MOOS_storm3_07_09_turb_FI",NA, NA, NA),
  c("MOOS_storm4_07_15_turb_FI",NA, NA, NA),
  c("MOOS_storm5_08_04_turb_FI",0.330552981155391, NA, NA), 
  c("MOOS_storm6_08_13_turb_FI",0.354272517321016, NA, NA),
  c("MOOS_storm7_08_23_turb_FI",0.45563465879493, NA, NA),
  c("MOOS_storm8a_08_26_turb_FI", 0.451545433418545, NA, NA),
  c("MOOS_storm8b_08_27_turb_FI",0.0596426084938501, NA, NA),
  c("MOOS_storm9_08_29_turb_FI",0.216361788617886, NA, NA),
  c("MOOS_storm10_09_01_turb_FI",0.21969732984708 ,NA, NA),
  c("MOOS_storm11a_09_22_turb_FI",0.395529380270379, NA, NA),
  c("MOOS_storm11b_09_23_turb_FI", 0.0449070631970259, NA, NA),
  c("MOOS_storm12_09_24_turb_FI",0.0935303526097228, NA, NA),
  
  c("CARI_storm1_06_10_NO3_FI",NA, NA, NA),
  c("CARI_storm2_06_21_NO3_FI",NA, NA, NA),
  c("CARI_storm3_06_29_NO3_FI",NA, NA, NA),
  c("CARI_storm4a_06_30_NO3_FI",NA, NA, NA),
  c("CARI_storm4b_07_01_NO3_FI",NA, NA, NA),
  c("CARI_storm5a_08_04_NO3_FI",NA, NA, NA),
  c("CARI_storm5b_08_05_NO3_FI",NA, NA, NA),
  c("CARI_storm5c_08_06_NO3_FI",0.0465116279069774, NA, NA),
  c("CARI_storm6_08_13_NO3_FI",NA, NA, NA),
  c("CARI_storm7_08_21_NO3_FI",NA, NA, NA),
  c("CARI_storm8_08_24_NO3_FI",NA, NA, NA),
  c("CARI_storm9_08_26_NO3_FI",NA, NA, NA),
  c("CARI_storm10_08_30_NO3_FI",NA, NA, NA),
  c("CARI_storm11_09_02_NO3_FI",NA, NA, NA),
  c("CARI_storm12a_09_20_NO3_FI",NA, NA, NA),
  c("CARI_storm12b_09_25_NO3_FI",NA, NA, NA),
  
  c("CARI_storm1_06_10_fDOM_FI",0.247818910634285, NA, NA),
  c("CARI_storm2_06_21_fDOM_FI",0.453265044814341,NA,NA),
  c("CARI_storm3_06_29_fDOM_FI",0.654370489174017,NA,NA),
  c("CARI_storm4a_06_30_fDOM_FI",0.373843342959863,NA,NA),
  c("CARI_storm4b_07_01_fDOM_FI",0.420978398040234,NA,NA),
  c("CARI_storm5a_08_04_fDOM_FI",0.161242603550296,NA,NA),
  c("CARI_storm5b_08_05_fDOM_FI",0.572166514737162,NA,NA),
  c("CARI_storm5c_08_06_fDOM_FI",0.316719453242207,NA,NA),
  c("CARI_storm6_08_13_fDOM_FI",0.777721416252409,NA,NA),
  c("CARI_storm7_08_21_fDOM_FI",NA,NA,NA),
  c("CARI_storm8_08_24_fDOM_FI",0.608794197642793,NA,NA),
  c("CARI_storm9_08_26_fDOM_FI",0.884684684684685,NA,NA),
  c("CARI_storm10_08_30_fDOM_FI",0.650042782055983,NA,NA),
  c("CARI_storm11_09_02_fDOM_FI",0.814095449500555,NA,NA),
  c("CARI_storm12a_09_20_fDOM_FI",NA,NA,NA),
  c("CARI_storm12b_09_25_fDOM_FI",0.0677162175799779,NA,NA),
  
  c("CARI_storm1_06_10_SPC_FI",NA,NA,NA),
  c("CARI_storm2_06_21_SPC_FI",.24927536231884,NA,NA),
  c("CARI_storm3_06_29_SPC_FI",NA,NA,NA),
  c("CARI_storm4a_06_30_SPC_FI",NA,NA,NA),
  c("CARI_storm4b_07_01_SPC_FI",NA,NA,NA),
  c("CARI_storm5a_08_04_SPC_FI",NA,NA,NA),
  c("CARI_storm5b_08_05_SPC_FI",NA,NA,NA),
  c("CARI_storm5c_08_06_SPC_FI",NA,NA,NA),
  c("CARI_storm6_08_13_SPC_FI",NA,NA,NA),
  c("CARI_storm7_08_21_SPC_FI",NA,NA,NA),
  c("CARI_storm8_08_24_SPC_FI",NA,NA,NA),
  c("CARI_storm9_08_26_SPC_FI",NA,NA,NA),
  c("CARI_storm10_08_30_SPC_FI",NA,NA,NA),
  c("CARI_storm11_09_02_SPC_FI",NA,NA,NA),
  c("CARI_storm12a_09_20_SPC_FI",NA,NA,NA),
  c("CARI_storm12b_09_25_SPC_FI",0.0371479928100658,NA,NA),
  
  c("CARI_storm1_06_10_turb_FI",0.27755905511811,NA,NA),
  c("CARI_storm2_06_21_turb_FI",NA,NA,NA),
  c("CARI_storm3_06_29_turb_FI",0.778284671532847,NA,NA),
  c("CARI_storm4a_06_30_turb_FI", 0.0558476295886368,NA,NA),
  c("CARI_storm4b_07_01_turb_FI",0.390557939914163,NA,NA),
  c("CARI_storm5a_08_04_turb_FI",0.689542483660131,NA,NA),
  c("CARI_storm5b_08_05_turb_FI",0.702678571428571,NA,NA),
  c("CARI_storm5c_08_06_turb_FI",0.528497409326425,NA,NA),
  c("CARI_storm6_08_13_turb_FI",0.315496098104794,NA,NA),
  c("CARI_storm7_08_21_turb_FI",0.308176100628931,NA,NA),
  c("CARI_storm8_08_24_turb_FI",0.725738396624473,NA,NA),
  c("CARI_storm9_08_26_turb_FI",0.431818181818182,NA,NA),
  c("CARI_storm10_08_30_turb_FI",0.0079810604885815,NA,NA),
  c("CARI_storm11_09_02_turb_FI",0.011472275334608,NA,NA),
  c("CARI_storm12a_09_20_turb_FI",NA,NA,NA),
  c("CARI_storm12b_09_25_turb_FI",0.0937499999999999,NA,NA))



FI_results = as.data.frame(FI_results)

names(FI_results) = c("ID", "Flushing_index", "percCI_2.5", "percCI_97.5")

FI_results$ID = unlist(FI_results$ID)
FI_results$Flushing_index = round(as.numeric(as.character(FI_results$Flushing_index)), 4)
FI_results$`percCI_2.5` = round(as.numeric(as.character(FI_results$`percCI_2.5`)), 4)
FI_results$`percCI_97.5` = round(as.numeric(as.character(FI_results$`percCI_97.5`)), 4)

write.csv(FI_results, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/all.FI.diff.results_2018.csv")


# calculate 95% bootstrap around median of Hyst. Indicies for each site and storm #

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

# FRCH #
FRCH.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2018/FRCH/FRCH.HI.df.csv")

storm.list = unique(FRCH.HI.df$storm.ID)
FRCH.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(FRCH.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
FRCH.HI.boot$storm.ID = storm.list

# MOOS #
MOOS.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2018/MOOS/MOOS.HI.df.csv")

storm.list = unique(MOOS.HI.df$storm.ID)
MOOS.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(MOOS.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
MOOS.HI.boot$storm.ID = storm.list

# CARI #
CARI.HI.df <- read.csv("~/Documents/Storms/Output_from_analysis/HI_plots/2018/CARI/CARI.HI.df.csv")

storm.list = unique(CARI.HI.df$storm.ID)
CARI.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(CARI.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
CARI.HI.boot$storm.ID = storm.list


# join data #

FRCH.HI.boot$site.ID = "FRCH"
MOOS.HI.boot$site.ID = "MOOS"
CARI.HI.boot$site.ID = "CARI"


HI = rbind(FRCH.HI.boot, MOOS.HI.boot, CARI.HI.boot)

all.FI.diff.results = read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/all.FI.diff.results_2018.csv", header = T, row.names = 1)

FI = subset(all.FI.diff.results, select=c("Flushing_index", "percCI_2.5", "percCI_97.5", "ID"))
FI$ID = as.character(FI$ID)
FI = separate(FI, ID, into=c("site.ID", "storm.ID", "month", "day", "response_var", NA), sep = "_")
names(FI) = c("Flush_index", "FI_ymin", "FI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI$site.ID=NULL
HI = separate(HI, storm.ID, into=c("site.ID", "storm.ID", "month", "day", "response_var"), sep = "_")
names(HI) = c("Hyst_index", "HI_ymin", "HI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI_FI = left_join(HI, FI, by=c("site.ID", "storm.ID", "response_var"))
write.csv(HI_FI, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results.2018.csv")


# plot #

# NO3 #
HI_FI_NO3 = subset(HI_FI, response_var == "NO3")
HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH','MOOS', 'CARI'))

HI_FI_NO3.p = 
  ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("a) NO3-")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_NO3.p

# fDOM #
HI_FI_fDOM = subset(HI_FI, response_var == "fDOM")
HI_FI_fDOM$site.ID <- factor(HI_FI_fDOM$site.ID, levels = c('FRCH','MOOS', 'CARI'))

HI_FI_fDOM.p = 
  ggplot(HI_FI_fDOM, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("b) fDOM")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_fDOM.p

# SPC #
HI_FI_SPC = subset(HI_FI, response_var == "SPC")
HI_FI_SPC$site.ID <- factor(HI_FI_SPC$site.ID, levels = c('FRCH','MOOS', 'CARI'))

HI_FI_SPC.p = 
  ggplot(HI_FI_SPC, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("c) SPC")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_SPC.p

# turb #
HI_FI_turb = subset(HI_FI, response_var == "turb")
HI_FI_turb$site.ID <- factor(HI_FI_turb$site.ID, levels = c('FRCH','MOOS', 'CARI'))

HI_FI_turb.p = 
  ggplot(HI_FI_turb, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID)), size = 4)+
  geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
  scale_color_manual(values = c("orange red", viridis::viridis(4)), "Catchment")+theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("d) Turb")+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
HI_FI_turb.p


grid.arrange(HI_FI_NO3.p,HI_FI_fDOM.p,HI_FI_SPC.p,HI_FI_turb.p)


