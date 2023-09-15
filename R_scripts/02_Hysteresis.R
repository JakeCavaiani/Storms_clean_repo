#### READ ME ####
##The purpose of this script is to plot hysteresis loops from DoD sites data prior to hysteresis analysis and, specifically, the hysteresisMetrics function.##
# Read in storm events and run them through the multiplot function to generate hysteresis plots
# make sure you plot on a normalized scale 

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

library(dplyr)
library(here)

######################################## 2015 ####################################
# load data #
library(dataRetrieval)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(here)
library(tidyverse)
library(zoo)

# FRCH #

# FRCH_storm1_07_01 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01.csv"))
FRCH_storm1_07_01_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01_Q.csv"))
FRCH_storm1_07_01_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01_NO3.csv"))
FRCH_storm1_07_01_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01_fDOM.csv"))
FRCH_storm1_07_01_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01_SPC.csv"))
FRCH_storm1_07_01_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01_turb.csv"))
FRCH_storm1_07_01_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm1_07_01_abs.csv"))

# FRCH_storm2_07_19 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19.csv"))
FRCH_storm2_07_19_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19_Q.csv"))
FRCH_storm2_07_19_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19_NO3.csv"))
FRCH_storm2_07_19_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19_fDOM.csv"))
FRCH_storm2_07_19_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19_SPC.csv"))
FRCH_storm2_07_19_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19_turb.csv"))
FRCH_storm2_07_19_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm2_07_19_abs.csv"))

# FRCH_storm3_07_26 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26.csv"))
FRCH_storm3_07_26_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26_Q.csv"))
FRCH_storm3_07_26_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26_NO3.csv"))
FRCH_storm3_07_26_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26_fDOM.csv"))
FRCH_storm3_07_26_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26_SPC.csv"))
FRCH_storm3_07_26_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26_turb.csv"))
FRCH_storm3_07_26_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm3_07_26_abs.csv"))

# FRCH_storm4_08_12 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12.csv"))
FRCH_storm4_08_12_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12_Q.csv"))
FRCH_storm4_08_12_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12_NO3.csv"))
FRCH_storm4_08_12_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12_fDOM.csv"))
FRCH_storm4_08_12_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12_SPC.csv"))
FRCH_storm4_08_12_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12_turb.csv"))
FRCH_storm4_08_12_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm4_08_12_abs.csv"))

# FRCH_storm5a_08_18 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18.csv"))
FRCH_storm5a_08_18_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18_Q.csv"))
FRCH_storm5a_08_18_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18_NO3.csv"))
FRCH_storm5a_08_18_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18_fDOM.csv"))
FRCH_storm5a_08_18_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18_SPC.csv"))
FRCH_storm5a_08_18_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18_turb.csv"))
FRCH_storm5a_08_18_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm5a_08_18_abs.csv"))

# FRCH_storm6a_08_25 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25.csv"))
FRCH_storm6a_08_25_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25_Q.csv"))
FRCH_storm6a_08_25_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25_NO3.csv"))
FRCH_storm6a_08_25_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25_fDOM.csv"))
FRCH_storm6a_08_25_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25_SPC.csv"))
FRCH_storm6a_08_25_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25_turb.csv"))
FRCH_storm6a_08_25_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm6a_08_25_abs.csv"))

# FRCH_storm7_09_13 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13.csv"))
FRCH_storm7_09_13_Q <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13_Q.csv"))
FRCH_storm7_09_13_NO3 <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13_NO3.csv"))
FRCH_storm7_09_13_fDOM <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13_fDOM.csv"))
FRCH_storm7_09_13_SPC <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13_SPC.csv"))
FRCH_storm7_09_13_Turb <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13_turb.csv"))
FRCH_storm7_09_13_abs <- read_csv(here("Storm_events", "2015", "FRCH", "FRCH_storm7_09_13_abs.csv"))

# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# NO3 #
FRCH_storm1_07_01_NO3.p = hyst_plot(FRCH_storm1_07_01_Q, FRCH_storm1_07_01_NO3, "FRCH", "NO3", "0701")
FRCH_storm2_07_19_NO3.p = hyst_plot(FRCH_storm2_07_19_Q, FRCH_storm2_07_19_NO3, "FRCH", "NO3", "0719")
FRCH_storm3_07_26_NO3.p = hyst_plot(FRCH_storm3_07_26_Q, FRCH_storm3_07_26_NO3, "FRCH", "NO3", "0726")
FRCH_storm4_08_12_NO3.p = hyst_plot(FRCH_storm4_08_12_Q, FRCH_storm4_08_12_NO3, "FRCH", "NO3", "0812")
FRCH_storm5a_08_18_NO3.p = hyst_plot(FRCH_storm5a_08_18_Q, FRCH_storm5a_08_18_NO3, "FRCH", "NO3", "0818a")

FRCH_storm6a_08_25_NO3.p = hyst_plot(FRCH_storm6a_08_25_Q, FRCH_storm6a_08_25_NO3, "FRCH", "NO3", "0825a")

FRCH_storm7_09_13_NO3.p = hyst_plot(FRCH_storm7_09_13_Q, FRCH_storm7_09_13_NO3, "FRCH", "NO3", "0913")

multiplot(FRCH_storm1_07_01_NO3.p) # partial 
multiplot(FRCH_storm2_07_19_NO3.p) # partial 
multiplot(FRCH_storm3_07_26_NO3.p) # partial 
multiplot(FRCH_storm4_08_12_NO3.p)
multiplot(FRCH_storm5a_08_18_NO3.p)

multiplot(FRCH_storm6a_08_25_NO3.p) # empty

multiplot(FRCH_storm7_09_13_NO3.p)

# fDOM #
FRCH_storm1_07_01_fDOM.p = hyst_plot(FRCH_storm1_07_01_Q, FRCH_storm1_07_01_fDOM, "FRCH", "fDOM", "0701")
FRCH_storm2_07_19_fDOM.p = hyst_plot(FRCH_storm2_07_19_Q, FRCH_storm2_07_19_fDOM, "FRCH", "fDOM", "0719")
FRCH_storm3_07_26_fDOM.p = hyst_plot(FRCH_storm3_07_26_Q, FRCH_storm3_07_26_fDOM, "FRCH", "fDOM", "0726")
FRCH_storm4_08_12_fDOM.p = hyst_plot(FRCH_storm4_08_12_Q, FRCH_storm4_08_12_fDOM, "FRCH", "fDOM", "0812")
FRCH_storm5a_08_18_fDOM.p = hyst_plot(FRCH_storm5a_08_18_Q, FRCH_storm5a_08_18_fDOM, "FRCH", "fDOM", "0818a")

FRCH_storm6a_08_25_fDOM.p = hyst_plot(FRCH_storm6a_08_25_Q, FRCH_storm6a_08_25_fDOM, "FRCH", "fDOM", "0825a")

FRCH_storm7_09_13_fDOM.p = hyst_plot(FRCH_storm7_09_13_Q, FRCH_storm7_09_13_fDOM, "FRCH", "fDOM", "0913")

multiplot(FRCH_storm1_07_01_fDOM.p) 
multiplot(FRCH_storm2_07_19_fDOM.p) 
multiplot(FRCH_storm3_07_26_fDOM.p) 
multiplot(FRCH_storm4_08_12_fDOM.p)
multiplot(FRCH_storm5a_08_18_fDOM.p)

multiplot(FRCH_storm6a_08_25_fDOM.p) 

multiplot(FRCH_storm7_09_13_fDOM.p)

# SPC #
FRCH_storm1_07_01_SPC.p = hyst_plot(FRCH_storm1_07_01_Q, FRCH_storm1_07_01_SPC, "FRCH", "SPC", "0701")
FRCH_storm2_07_19_SPC.p = hyst_plot(FRCH_storm2_07_19_Q, FRCH_storm2_07_19_SPC, "FRCH", "SPC", "0719")
FRCH_storm3_07_26_SPC.p = hyst_plot(FRCH_storm3_07_26_Q, FRCH_storm3_07_26_SPC, "FRCH", "SPC", "0726")
FRCH_storm4_08_12_SPC.p = hyst_plot(FRCH_storm4_08_12_Q, FRCH_storm4_08_12_SPC, "FRCH", "SPC", "0812")
FRCH_storm5a_08_18_SPC.p = hyst_plot(FRCH_storm5a_08_18_Q, FRCH_storm5a_08_18_SPC, "FRCH", "SPC", "0818a")

FRCH_storm6a_08_25_SPC.p = hyst_plot(FRCH_storm6a_08_25_Q, FRCH_storm6a_08_25_SPC, "FRCH", "SPC", "0825a")

FRCH_storm7_09_13_SPC.p = hyst_plot(FRCH_storm7_09_13_Q, FRCH_storm7_09_13_SPC, "FRCH", "SPC", "0913")

multiplot(FRCH_storm1_07_01_SPC.p) 
multiplot(FRCH_storm2_07_19_SPC.p) 
multiplot(FRCH_storm3_07_26_SPC.p) 
multiplot(FRCH_storm4_08_12_SPC.p)
multiplot(FRCH_storm5a_08_18_SPC.p)

multiplot(FRCH_storm6a_08_25_SPC.p) 

multiplot(FRCH_storm7_09_13_SPC.p)

# turb #
FRCH_storm1_07_01_turb.p = hyst_plot(FRCH_storm1_07_01_Q, FRCH_storm1_07_01_Turb, "FRCH", "turb", "0701")
FRCH_storm2_07_19_turb.p = hyst_plot(FRCH_storm2_07_19_Q, FRCH_storm2_07_19_Turb, "FRCH", "turb", "0719")
FRCH_storm3_07_26_turb.p = hyst_plot(FRCH_storm3_07_26_Q, FRCH_storm3_07_26_Turb, "FRCH", "turb", "0726")
FRCH_storm4_08_12_turb.p = hyst_plot(FRCH_storm4_08_12_Q, FRCH_storm4_08_12_Turb, "FRCH", "turb", "0812")
FRCH_storm5a_08_18_turb.p = hyst_plot(FRCH_storm5a_08_18_Q, FRCH_storm5a_08_18_Turb, "FRCH", "turb", "0818a")

FRCH_storm6a_08_25_turb.p = hyst_plot(FRCH_storm6a_08_25_Q, FRCH_storm6a_08_25_Turb, "FRCH", "turb", "0825a")

FRCH_storm7_09_13_turb.p = hyst_plot(FRCH_storm7_09_13_Q, FRCH_storm7_09_13_Turb, "FRCH", "turb", "0913")

multiplot(FRCH_storm1_07_01_turb.p) 
multiplot(FRCH_storm2_07_19_turb.p) 
multiplot(FRCH_storm3_07_26_turb.p) 
multiplot(FRCH_storm4_08_12_turb.p)
multiplot(FRCH_storm5a_08_18_turb.p)

multiplot(FRCH_storm6a_08_25_turb.p) 

multiplot(FRCH_storm7_09_13_turb.p)

# abs #
FRCH_storm1_07_01_abs.p = hyst_plot(FRCH_storm1_07_01_Q, FRCH_storm1_07_01_abs, "FRCH", "abs", "0701")
FRCH_storm2_07_19_abs.p = hyst_plot(FRCH_storm2_07_19_Q, FRCH_storm2_07_19_abs, "FRCH", "abs", "0719")
FRCH_storm3_07_26_abs.p = hyst_plot(FRCH_storm3_07_26_Q, FRCH_storm3_07_26_abs, "FRCH", "abs", "0726")
FRCH_storm4_08_12_abs.p = hyst_plot(FRCH_storm4_08_12_Q, FRCH_storm4_08_12_abs, "FRCH", "abs", "0812")
FRCH_storm5a_08_18_abs.p = hyst_plot(FRCH_storm5a_08_18_Q, FRCH_storm5a_08_18_abs, "FRCH", "abs", "0818a")

FRCH_storm6a_08_25_abs.p = hyst_plot(FRCH_storm6a_08_25_Q, FRCH_storm6a_08_25_abs, "FRCH", "abs", "0825a")

FRCH_storm7_09_13_abs.p = hyst_plot(FRCH_storm7_09_13_Q, FRCH_storm7_09_13_abs, "FRCH", "abs", "0913")
# 
multiplot(FRCH_storm1_07_01_abs.p)
multiplot(FRCH_storm2_07_19_abs.p)
multiplot(FRCH_storm3_07_26_abs.p)
multiplot(FRCH_storm4_08_12_abs.p)
multiplot(FRCH_storm5a_08_18_abs.p)

multiplot(FRCH_storm6a_08_25_abs.p) # empty

multiplot(FRCH_storm7_09_13_abs.p)

# Plot all the storms that are correct:

FRCH_HI_Loop <- multiplot(FRCH_storm1_07_01_NO3.p,FRCH_storm1_07_01_fDOM.p, FRCH_storm1_07_01_SPC.p,FRCH_storm1_07_01_turb.p,FRCH_storm1_07_01_abs.p,
                          FRCH_storm2_07_19_NO3.p,FRCH_storm2_07_19_fDOM.p, FRCH_storm2_07_19_SPC.p,FRCH_storm2_07_19_turb.p, FRCH_storm2_07_19_abs.p,
                          FRCH_storm3_07_26_NO3.p,FRCH_storm3_07_26_fDOM.p, FRCH_storm3_07_26_SPC.p,FRCH_storm3_07_26_turb.p,FRCH_storm3_07_26_abs.p,
                          FRCH_storm4_08_12_NO3.p,FRCH_storm4_08_12_fDOM.p, FRCH_storm4_08_12_SPC.p,FRCH_storm4_08_12_turb.p,FRCH_storm4_08_12_abs.p,
                          FRCH_storm5a_08_18_NO3.p,FRCH_storm5a_08_18_fDOM.p, FRCH_storm5a_08_18_SPC.p,FRCH_storm5a_08_18_turb.p,FRCH_storm5a_08_18_abs.p,
                          
                          FRCH_storm6a_08_25_NO3.p,FRCH_storm6a_08_25_fDOM.p, FRCH_storm6a_08_25_SPC.p,FRCH_storm6a_08_25_turb.p,FRCH_storm6a_08_25_abs.p,
                          
                          FRCH_storm7_09_13_NO3.p,FRCH_storm7_09_13_fDOM.p, FRCH_storm7_09_13_SPC.p,FRCH_storm7_09_13_turb.p, FRCH_storm7_09_13_abs.p)

# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2015.pdf",
       path = here("plots", "02_Hysteresis", "2015"),
       width = 20, height = 30, units = "in")


# MOOS #
# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

# MOOS_storm1_07_01 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01.csv"))
MOOS_storm1_07_01_Q <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01_Q.csv"))
MOOS_storm1_07_01_NO3 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01_NO3.csv"))
MOOS_storm1_07_01_fDOM <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01_fDOM.csv"))
MOOS_storm1_07_01_SPC <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01_SPC.csv"))
MOOS_storm1_07_01_Turb <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01_turb.csv"))
MOOS_storm1_07_01_abs <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm1_07_01_abs.csv"))

# MOOS_storm1_07_18 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18.csv"))
MOOS_storm2_07_18_Q <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18_Q.csv"))
MOOS_storm2_07_18_NO3 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18_NO3.csv"))
MOOS_storm2_07_18_fDOM <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18_fDOM.csv"))
MOOS_storm2_07_18_SPC <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18_SPC.csv"))
MOOS_storm2_07_18_Turb <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18_turb.csv"))
MOOS_storm2_07_18_abs <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm2_07_18_abs.csv"))

# MOOS_storm3a_07_27 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27.csv"))
MOOS_storm3a_07_27_Q <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27_Q.csv"))
MOOS_storm3a_07_27_NO3 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27_NO3.csv"))
MOOS_storm3a_07_27_fDOM <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27_fDOM.csv"))
MOOS_storm3a_07_27_SPC <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27_SPC.csv"))
MOOS_storm3a_07_27_Turb <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27_turb.csv"))
MOOS_storm3a_07_27_abs <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm3a_07_27_abs.csv"))


# MOOS_storm4_08_16 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16.csv"))
MOOS_storm4_08_16_Q <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16_Q.csv"))
MOOS_storm4_08_16_NO3 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16_NO3.csv"))
MOOS_storm4_08_16_fDOM <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16_fDOM.csv"))
MOOS_storm4_08_16_SPC <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16_SPC.csv"))
MOOS_storm4_08_16_Turb <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16_turb.csv"))
MOOS_storm4_08_16_abs <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm4_08_16_abs.csv"))

# MOOS_storm5_08_25 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_csv"))
MOOS_storm5_08_25_Q <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_Q.csv"))
MOOS_storm5_08_25_NO3 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_NO3.csv"))
MOOS_storm5_08_25_fDOM <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_fDOM.csv"))
MOOS_storm5_08_25_SPC <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_SPC.csv"))
MOOS_storm5_08_25_Turb <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_turb.csv"))
MOOS_storm5_08_25_abs <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm5_08_25_abs.csv"))

# MOOS_storm6_09_14 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_csv"))
MOOS_storm6_09_14_Q <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_Q.csv"))
MOOS_storm6_09_14_NO3 <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_NO3.csv"))
MOOS_storm6_09_14_fDOM <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_fDOM.csv"))
MOOS_storm6_09_14_SPC <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_SPC.csv"))
MOOS_storm6_09_14_Turb <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_turb.csv"))
MOOS_storm6_09_14_abs <- read_csv(here("Storm_events", "2015", "MOOS", "MOOS_storm6_09_14_abs.csv"))


# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# plot MOOS loops #

# NO3 #
MOOS_storm1_07_01_NO3.p = hyst_plot(MOOS_storm1_07_01_Q, MOOS_storm1_07_01_NO3, "MOOS", "NO3", "0701")
MOOS_storm2_07_18_NO3.p = hyst_plot(MOOS_storm2_07_18_Q, MOOS_storm2_07_18_NO3, "MOOS", "NO3", "0718")
MOOS_storm3a_07_27_NO3.p = hyst_plot(MOOS_storm3a_07_27_Q, MOOS_storm3a_07_27_NO3, "MOOS", "NO3", "0727a")

MOOS_storm4_08_16_NO3.p = hyst_plot(MOOS_storm4_08_16_Q, MOOS_storm4_08_16_NO3, "MOOS", "NO3", "0816")
MOOS_storm5_08_25_NO3.p = hyst_plot(MOOS_storm5_08_25_Q, MOOS_storm5_08_25_NO3, "MOOS", "NO3", "0825")
MOOS_storm6_09_14_NO3.p = hyst_plot(MOOS_storm6_09_14_Q, MOOS_storm6_09_14_NO3, "MOOS", "NO3", "0914")

multiplot(MOOS_storm1_07_01_NO3.p)
multiplot(MOOS_storm2_07_18_NO3.p) 
multiplot(MOOS_storm3a_07_27_NO3.p) 
multiplot(MOOS_storm3b_07_28_NO3.p)
multiplot(MOOS_storm4_08_16_NO3.p)
multiplot(MOOS_storm5_08_25_NO3.p) # empty
multiplot(MOOS_storm6_09_14_NO3.p) 


# fDOM #
MOOS_storm1_07_01_fDOM.p = hyst_plot(MOOS_storm1_07_01_Q, MOOS_storm1_07_01_fDOM, "MOOS", "fDOM", "0701")
MOOS_storm2_07_18_fDOM.p = hyst_plot(MOOS_storm2_07_18_Q, MOOS_storm2_07_18_fDOM, "MOOS", "fDOM", "0718")
MOOS_storm3a_07_27_fDOM.p = hyst_plot(MOOS_storm3a_07_27_Q, MOOS_storm3a_07_27_fDOM, "MOOS", "fDOM", "0727a")

MOOS_storm4_08_16_fDOM.p = hyst_plot(MOOS_storm4_08_16_Q, MOOS_storm4_08_16_fDOM, "MOOS", "fDOM", "0816")
MOOS_storm5_08_25_fDOM.p = hyst_plot(MOOS_storm5_08_25_Q, MOOS_storm5_08_25_fDOM, "MOOS", "fDOM", "0825")
MOOS_storm6_09_14_fDOM.p = hyst_plot(MOOS_storm6_09_14_Q, MOOS_storm6_09_14_fDOM, "MOOS", "fDOM", "0914")

multiplot(MOOS_storm1_07_01_fDOM.p)
multiplot(MOOS_storm2_07_18_fDOM.p) 
multiplot(MOOS_storm3a_07_27_fDOM.p) 
multiplot(MOOS_storm3b_07_28_fDOM.p)
multiplot(MOOS_storm4_08_16_fDOM.p)
multiplot(MOOS_storm5_08_25_fDOM.p) 
multiplot(MOOS_storm6_09_14_fDOM.p) 

# SPC #
MOOS_storm1_07_01_SPC.p = hyst_plot(MOOS_storm1_07_01_Q, MOOS_storm1_07_01_SPC, "MOOS", "SPC", "0701")
MOOS_storm2_07_18_SPC.p = hyst_plot(MOOS_storm2_07_18_Q, MOOS_storm2_07_18_SPC, "MOOS", "SPC", "0718")
MOOS_storm3a_07_27_SPC.p = hyst_plot(MOOS_storm3a_07_27_Q, MOOS_storm3a_07_27_SPC, "MOOS", "SPC", "0727a")

MOOS_storm4_08_16_SPC.p = hyst_plot(MOOS_storm4_08_16_Q, MOOS_storm4_08_16_SPC, "MOOS", "SPC", "0816")
MOOS_storm5_08_25_SPC.p = hyst_plot(MOOS_storm5_08_25_Q, MOOS_storm5_08_25_SPC, "MOOS", "SPC", "0825")
MOOS_storm6_09_14_SPC.p = hyst_plot(MOOS_storm6_09_14_Q, MOOS_storm6_09_14_SPC, "MOOS", "SPC", "0914")

multiplot(MOOS_storm1_07_01_SPC.p)
multiplot(MOOS_storm2_07_18_SPC.p) 
multiplot(MOOS_storm3a_07_27_SPC.p) # empty
multiplot(MOOS_storm3b_07_28_SPC.p) # empty
multiplot(MOOS_storm4_08_16_SPC.p)
multiplot(MOOS_storm5_08_25_SPC.p) 
multiplot(MOOS_storm6_09_14_SPC.p) 

# turb #
MOOS_storm1_07_01_turb.p = hyst_plot(MOOS_storm1_07_01_Q, MOOS_storm1_07_01_Turb, "MOOS", "Turb", "0701")
MOOS_storm2_07_18_turb.p = hyst_plot(MOOS_storm2_07_18_Q, MOOS_storm2_07_18_Turb, "MOOS", "Turb", "0718")
MOOS_storm3a_07_27_turb.p = hyst_plot(MOOS_storm3a_07_27_Q, MOOS_storm3a_07_27_Turb, "MOOS", "Turb", "0727a")

MOOS_storm4_08_16_turb.p = hyst_plot(MOOS_storm4_08_16_Q, MOOS_storm4_08_16_Turb, "MOOS", "Turb", "0816")
MOOS_storm5_08_25_turb.p = hyst_plot(MOOS_storm5_08_25_Q, MOOS_storm5_08_25_Turb, "MOOS", "Turb", "0825")
MOOS_storm6_09_14_turb.p = hyst_plot(MOOS_storm6_09_14_Q, MOOS_storm6_09_14_Turb, "MOOS", "Turb", "0914")

multiplot(MOOS_storm1_07_01_turb.p)
multiplot(MOOS_storm2_07_18_turb.p) 
multiplot(MOOS_storm3a_07_27_turb.p)  # empty

multiplot(MOOS_storm4_08_16_turb.p)
multiplot(MOOS_storm5_08_25_turb.p) 
multiplot(MOOS_storm6_09_14_turb.p) 

# abs #
MOOS_storm1_07_01_abs.p = hyst_plot(MOOS_storm1_07_01_Q, MOOS_storm1_07_01_abs, "MOOS", "abs", "0701")
MOOS_storm2_07_18_abs.p = hyst_plot(MOOS_storm2_07_18_Q, MOOS_storm2_07_18_abs, "MOOS", "abs", "0718")
MOOS_storm3a_07_27_abs.p = hyst_plot(MOOS_storm3a_07_27_Q, MOOS_storm3a_07_27_abs, "MOOS", "abs", "0727a")

MOOS_storm4_08_16_abs.p = hyst_plot(MOOS_storm4_08_16_Q, MOOS_storm4_08_16_abs, "MOOS", "abs", "0816")
MOOS_storm5_08_25_abs.p = hyst_plot(MOOS_storm5_08_25_Q, MOOS_storm5_08_25_abs, "MOOS", "abs", "0825")
MOOS_storm6_09_14_abs.p = hyst_plot(MOOS_storm6_09_14_Q, MOOS_storm6_09_14_abs, "MOOS", "abs", "0914")

multiplot(MOOS_storm1_07_01_abs.p)
multiplot(MOOS_storm2_07_18_abs.p) 
multiplot(MOOS_storm3a_07_27_abs.p) 

multiplot(MOOS_storm4_08_16_abs.p)
multiplot(MOOS_storm5_08_25_abs.p) # empty
multiplot(MOOS_storm6_09_14_abs.p) 

# Plot all the storms that are correct:

MOOS_HI_Loop <- multiplot(MOOS_storm1_07_01_NO3.p,MOOS_storm1_07_01_fDOM.p, MOOS_storm1_07_01_SPC.p,MOOS_storm1_07_01_turb.p,MOOS_storm1_07_01_abs.p,
                          MOOS_storm2_07_18_NO3.p,MOOS_storm2_07_18_fDOM.p, MOOS_storm2_07_18_SPC.p,MOOS_storm2_07_18_turb.p,MOOS_storm2_07_18_abs.p,
                          MOOS_storm3a_07_27_NO3.p,MOOS_storm3a_07_27_fDOM.p, MOOS_storm3a_07_27_SPC.p,MOOS_storm3a_07_27_turb.p, MOOS_storm3a_07_27_abs.p,
                          
                          MOOS_storm4_08_16_NO3.p,MOOS_storm4_08_16_fDOM.p, MOOS_storm4_08_16_SPC.p,MOOS_storm4_08_16_turb.p, MOOS_storm4_08_16_abs.p,
                          MOOS_storm5_08_25_NO3.p,MOOS_storm5_08_25_fDOM.p, MOOS_storm5_08_25_SPC.p,MOOS_storm5_08_25_turb.p, MOOS_storm5_08_25_abs.p,
                          MOOS_storm6_09_14_NO3.p, MOOS_storm6_09_14_fDOM.p,  MOOS_storm6_09_14_SPC.p, MOOS_storm6_09_14_turb.p, MOOS_storm6_09_14_abs.p)
                          
# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2015.pdf",
       path = here("plots", "02_Hysteresis", "2015"),
       width = 20, height = 30, units = "in")




###################################### 2018 #######################################


# plot on normalized scale # 
# load data #
#FRCH_storm1_06_21 <- read_csv("~/Documents/Storms/Storm_Events/2018/FRCH/FRCH_storm1_06_21.csv") # not a storm!!!!
# FRCH_storm1_06_21_Q <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_Q.csv")
# FRCH_storm1_06_21_NO3 <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_NO3.csv")
# FRCH_storm1_06_21_fDOM <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_fDOM.csv")
# FRCH_storm1_06_21_SPC <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_SPC.csv")
# FRCH_storm1_06_21_turb <- read_csv("Storm_Events/2018/FRCH/FRCH_storm1_06_21_Turb.csv")

FRCH_storm2a_06_30_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm2a_06_30_Q.csv"))
FRCH_storm2a_06_30_NO3 <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm2a_06_30_NO3.csv"))
FRCH_storm2a_06_30_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm2a_06_30_fDOM.csv"))
FRCH_storm2a_06_30_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm2a_06_30_SPC.csv"))
FRCH_storm2a_06_30_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm2a_06_30_Turb.csv"))
FRCH_storm2a_06_30_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm2a_06_30_abs.csv"))

FRCH_storm3_07_11_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm3_07_11_Q.csv"))
FRCH_storm3_07_11_NO3 <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm3_07_11_NO3.csv"))
FRCH_storm3_07_11_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm3_07_11_fDOM.csv"))
FRCH_storm3_07_11_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm3_07_11_SPC.csv"))
FRCH_storm3_07_11_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm3_07_11_Turb.csv"))
FRCH_storm3_07_11_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm3_07_11_abs.csv"))

FRCH_storm4a_07_15_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4a_07_15_Q.csv"))
FRCH_storm4a_07_15_NO3 <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4a_07_15_NO3.csv"))
FRCH_storm4a_07_15_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4a_07_15_fDOM.csv"))
FRCH_storm4a_07_15_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4a_07_15_SPC.csv"))
FRCH_storm4a_07_15_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4a_07_15_Turb.csv"))
FRCH_storm4a_07_15_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4a_07_15_abs.csv"))

FRCH_storm4b_07_17_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4b_07_17_Q.csv"))
FRCH_storm4b_07_17_NO3 <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4b_07_17_NO3.csv"))
FRCH_storm4b_07_17_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4b_07_17_fDOM.csv"))
FRCH_storm4b_07_17_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4b_07_17_SPC.csv"))
FRCH_storm4b_07_17_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4b_07_17_Turb.csv"))
FRCH_storm4b_07_17_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm4b_07_17_abs.csv"))

FRCH_storm5_08_05_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm5_08_05_Q.csv"))
FRCH_storm5_08_05_NO3 <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm5_08_05_NO3.csv"))
FRCH_storm5_08_05_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm5_08_05_fDOM.csv"))
FRCH_storm5_08_05_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm5_08_05_SPC.csv"))
FRCH_storm5_08_05_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm5_08_05_Turb.csv"))
FRCH_storm5_08_05_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm5_08_05_abs.csv"))

FRCH_storm6_08_13_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm6_08_13_Q.csv"))
FRCH_storm6_08_13_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm6_08_13_fDOM.csv"))
FRCH_storm6_08_13_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm6_08_13_SPC.csv"))
FRCH_storm6_08_13_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm6_08_13_Turb.csv"))
FRCH_storm6_08_13_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm6_08_13_abs.csv"))

FRCH_storm7_08_24_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm7_08_24_Q.csv"))
FRCH_storm7_08_24_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm7_08_24_fDOM.csv"))
FRCH_storm7_08_24_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm7_08_24_SPC.csv"))
FRCH_storm7_08_24_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm7_08_24_Turb.csv"))
FRCH_storm7_08_24_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm7_08_24_abs.csv"))

FRCH_storm8a_08_26_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm8a_08_26_Q.csv"))
FRCH_storm8a_08_26_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm8a_08_26_fDOM.csv"))
FRCH_storm8a_08_26_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm8a_08_26_SPC.csv"))
FRCH_storm8a_08_26_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm8a_08_26_Turb.csv"))
FRCH_storm8a_08_26_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm8a_08_26_abs.csv"))


FRCH_storm9_08_30_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm9_08_30_Q.csv"))
FRCH_storm9_08_30_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm9_08_30_fDOM.csv"))
FRCH_storm9_08_30_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm9_08_30_SPC.csv"))
FRCH_storm9_08_30_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm9_08_30_turb.csv"))
FRCH_storm9_08_30_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm9_08_30_abs.csv"))

FRCH_storm10_09_01_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm10_09_01_Q.csv"))
FRCH_storm10_09_01_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm10_09_01_fDOM.csv"))
FRCH_storm10_09_01_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm10_09_01_SPC.csv"))
FRCH_storm10_09_01_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm10_09_01_turb.csv"))
FRCH_storm10_09_01_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm10_09_01_abs.csv"))

FRCH_storm11a_09_22_Q <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm11a_09_22_Q.csv"))
FRCH_storm11a_09_22_fDOM <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm11a_09_22_fDOM.csv"))
FRCH_storm11a_09_22_SPC <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm11a_09_22_SPC.csv"))
FRCH_storm11a_09_22_turb <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm11a_09_22_Turb.csv"))
FRCH_storm11a_09_22_abs <- read_csv(here("Storm_Events", "2018", "FRCH", "FRCH_storm11a_09_22_abs.csv"))


# MOOS #
MOOS_storm2a_06_30_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm2a_06_30_Q.csv"))
MOOS_storm2a_06_30_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm2a_06_30_NO3.csv"))
MOOS_storm2a_06_30_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm2a_06_30_fDOM.csv"))
MOOS_storm2a_06_30_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm2a_06_30_SPC.csv"))
MOOS_storm2a_06_30_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm2a_06_30_Turb.csv"))
MOOS_storm2a_06_30_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm2a_06_30_abs.csv"))

MOOS_storm3_07_10_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm3_07_10_Q.csv"))
MOOS_storm3_07_10_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm3_07_10_NO3.csv"))
MOOS_storm3_07_10_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm3_07_10_fDOM.csv"))
MOOS_storm3_07_10_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm3_07_10_SPC.csv"))
MOOS_storm3_07_10_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm3_07_10_Turb.csv"))
MOOS_storm3_07_10_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm3_07_10_abs.csv"))

MOOS_storm5_08_05_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm5_08_05_Q.csv"))
MOOS_storm5_08_05_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm5_08_05_NO3,csv"))
MOOS_storm5_08_05_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm5_08_05_fDOM.csv"))
MOOS_storm5_08_05_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm5_08_05_SPC.csv"))
MOOS_storm5_08_05_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm5_08_05_Turb.csv"))
MOOS_storm5_08_05_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm5_08_05_abs.csv"))

MOOS_storm6_08_13_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm6_08_13_Q.csv"))
MOOS_storm6_08_13_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm6_08_13_NO3.csv"))
MOOS_storm6_08_13_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm6_08_13_fDOM.csv"))
MOOS_storm6_08_13_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm6_08_13_SPC.csv"))
MOOS_storm6_08_13_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm6_08_13_Turb.csv"))
MOOS_storm6_08_13_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm6_08_13_abs.csv"))

MOOS_storm7_08_23_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm7_08_23_Q.csv"))
MOOS_storm7_08_23_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm7_08_23_NO3.csv"))
MOOS_storm7_08_23_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm7_08_23_fDOM.csv"))
MOOS_storm7_08_23_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm7_08_23_SPC.csv"))
MOOS_storm7_08_23_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm7_08_23_Turb.csv"))
MOOS_storm7_08_23_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm7_08_23_abs.csv"))

MOOS_storm8a_08_26_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm8a_08_26_Q.csv"))
MOOS_storm8a_08_26_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm8a_08_26_NO3.csv"))
MOOS_storm8a_08_26_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm8a_08_26_fDOM.csv"))
MOOS_storm8a_08_26_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm8a_08_26_SPC.csv"))
MOOS_storm8a_08_26_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm8a_08_26_Turb.csv"))
MOOS_storm8a_08_26_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm8a_08_26_abs.csv"))

MOOS_storm9_08_30_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm9_08_30_Q.csv"))
MOOS_storm9_08_30_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm9_08_30_NO3.csv"))
MOOS_storm9_08_30_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm9_08_30_fDOM.csv"))
MOOS_storm9_08_30_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm9_08_30_SPC.csv"))
MOOS_storm9_08_30_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm9_08_30_Turb.csv"))
MOOS_storm9_08_30_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm9_08_30_abs.csv"))

MOOS_storm10_09_01_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm10_09_01_Q.csv"))
MOOS_storm10_09_01_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm10_09_01_NO3.csv"))
MOOS_storm10_09_01_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm10_09_01_fDOM.csv"))
MOOS_storm10_09_01_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm10_09_01_SPC.csv"))
MOOS_storm10_09_01_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm10_09_01_Turb.csv"))
MOOS_storm10_09_01_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm10_09_01_abs.csv"))

MOOS_storm11a_09_22_Q <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm11a_09_22_Q.csv"))
MOOS_storm11a_09_22_NO3 <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm11a_09_22_NO3.csv"))
MOOS_storm11a_09_22_fDOM <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm11a_09_22_fDOM.csv"))
MOOS_storm11a_09_22_SPC <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm11a_09_22_SPC.csv"))
MOOS_storm11a_09_22_turb <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm11a_09_22_Turb.csv"))
MOOS_storm11a_09_22_abs <- read_csv(here("Storm_Events", "2018", "MOOS", "MOOS_storm11a_09_22_abs.csv"))

# CARI # 

CARI_storm3_06_29_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm3_06_29_Q.csv"))
CARI_storm3_06_29_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm3_06_29_NO3.csv"))
CARI_storm3_06_29_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm3_06_29_fDOM.csv"))
CARI_storm3_06_29_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm3_06_29_SPC.csv"))
CARI_storm3_06_29_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm3_06_29_turb.csv"))

CARI_storm5a_08_04_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5a_08_04_Q.csv"))
CARI_storm5a_08_04_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5a_08_04_NO3.csv"))
CARI_storm5a_08_04_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5a_08_04_fDOM.csv"))
CARI_storm5a_08_04_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5a_08_04_SPC.csv"))
CARI_storm5a_08_04_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5a_08_04_turb.csv"))

CARI_storm5b_08_05_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5b_08_05_Q.csv"))
CARI_storm5b_08_05_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5b_08_05_NO3.csv"))
CARI_storm5b_08_05_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5b_08_05_fDOM.csv"))
CARI_storm5b_08_05_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5b_08_05_SPC.csv"))
CARI_storm5b_08_05_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5b_08_05_turb.csv"))

CARI_storm5c_08_06_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5c_08_06_Q.csv"))
CARI_storm5c_08_06_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5c_08_06_NO3.csv"))
CARI_storm5c_08_06_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5c_08_06_fDOM.csv"))
CARI_storm5c_08_06_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5c_08_06_SPC.csv"))
CARI_storm5c_08_06_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm5c_08_06_turb.csv"))

CARI_storm6_08_13_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm6_08_13_Q.csv"))
CARI_storm6_08_13_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm6_08_13_NO3.csv"))
CARI_storm6_08_13_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm6_08_13_fDOM.csv"))
CARI_storm6_08_13_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm6_08_13_SPC.csv"))
CARI_storm6_08_13_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm6_08_13_turb.csv"))

CARI_storm8_08_24_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm8_08_24_Q.csv"))
CARI_storm8_08_24_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm8_08_24_NO3.csv"))
CARI_storm8_08_24_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm8_08_24_fDOM.csv"))
CARI_storm8_08_24_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm8_08_24_SPC.csv"))
CARI_storm8_08_24_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm8_08_24_turb.csv"))

CARI_storm9_08_26_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm9_08_26_Q.csv"))
CARI_storm9_08_26_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm9_08_26_NO3.csv"))
CARI_storm9_08_26_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm9_08_26_fDOM.csv"))
CARI_storm9_08_26_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm9_08_26_SPC.csv"))
CARI_storm9_08_26_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm9_08_26_turb.csv"))

CARI_storm10_08_30_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm10_08_30_Q.csv"))
CARI_storm10_08_30_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm10_08_30_NO3.csv"))
CARI_storm10_08_30_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm10_08_30_fDOM.csv"))
CARI_storm10_08_30_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm10_08_30_SPC.csv"))
CARI_storm10_08_30_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm10_08_30_turb.csv"))

CARI_storm11_09_01_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm11_09_01_Q.csv"))
CARI_storm11_09_01_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm11_09_01_NO3.csv"))
CARI_storm11_09_01_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm11_09_01_fDOM.csv"))
CARI_storm11_09_01_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm11_09_01_SPC.csv"))
CARI_storm11_09_01_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm11_09_01_turb.csv"))

CARI_storm12a_09_20_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12a_09_20_Q.csv"))
CARI_storm12a_09_20_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12a_09_20_NO3.csv"))
CARI_storm12a_09_20_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12a_09_20_fDOM.csv"))
CARI_storm12a_09_20_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12a_09_20_SPC.csv"))
CARI_storm12a_09_20_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12a_09_20_turb.csv"))

CARI_storm12b_09_25_Q <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12b_09_25_Q.csv"))
CARI_storm12b_09_25_NO3 <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12b_09_25_NO3.csv"))
CARI_storm12b_09_25_fDOM <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12b_09_25_fDOM.csv"))
CARI_storm12b_09_25_SPC <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12b_09_25_SPC.csv"))
CARI_storm12b_09_25_turb <- read_csv(here("Storm_Events", "2018", "CARI", "CARI_storm12b_09_25_turb.csv"))


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
#FRCH_storm1_06_21_NO3.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_NO3, "FRCH", "NO3", "0621")
FRCH_storm2a_06_30_NO3.p = hyst_plot(FRCH_storm2a_06_30_Q, FRCH_storm2a_06_30_NO3, "FRCH", "NO3", "0630")

FRCH_storm3_07_11_NO3.p = hyst_plot(FRCH_storm3_07_11_Q, FRCH_storm3_07_11_NO3, "FRCH", "NO3", "0711")
FRCH_storm4a_07_15_NO3.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_NO3, "FRCH", "NO3", "0715a")
FRCH_storm4b_07_17_NO3.p = hyst_plot(FRCH_storm4b_07_17_Q, FRCH_storm4b_07_17_NO3, "FRCH", "NO3", "0717b")
FRCH_storm5_08_05_NO3.p = hyst_plot(FRCH_storm5_08_05_Q, FRCH_storm5_08_05_NO3, "FRCH", "NO3", "0805")
#FRCH_storm6_08_13_NO3.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_NO3, "FRCH", "NO3", "0813")
#FRCH_storm7_08_23_NO3.p = hyst_plot(FRCH_storm7_08_23_Q, FRCH_storm7_08_23_NO3, "FRCH", "NO3", "0823")
#FRCH_storm8a_08_26_NO3.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_NO3, "FRCH", "NO3", "0826a")

#FRCH_storm9_08_29_NO3.p = hyst_plot(FRCH_storm9_08_29_Q, FRCH_storm9_08_29_NO3, "FRCH", "NO3", "0829")
#FRCH_storm10_09_01_NO3.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_NO3, "FRCH", "NO3", "0901")
#FRCH_storm11a_09_22_NO3.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_NO3, "FRCH", "NO3", "0922a")


# fDOM #
#FRCH_storm1_06_21_fDOM.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_fDOM, "FRCH", "fDOM", "0621")
FRCH_storm2a_06_30_fDOM.p = hyst_plot(FRCH_storm2a_06_30_Q, FRCH_storm2a_06_30_fDOM, "FRCH", "fDOM", "0630")

FRCH_storm3_07_11_fDOM.p = hyst_plot(FRCH_storm3_07_11_Q, FRCH_storm3_07_11_fDOM, "FRCH", "fDOM", "0711")
FRCH_storm4a_07_15_fDOM.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_fDOM, "FRCH", "fDOM", "0715a")
FRCH_storm4b_07_17_fDOM.p = hyst_plot(FRCH_storm4b_07_17_Q, FRCH_storm4b_07_17_fDOM, "FRCH", "fDOM", "0717b")
FRCH_storm5_08_05_fDOM.p = hyst_plot(FRCH_storm5_08_05_Q, FRCH_storm5_08_05_fDOM, "FRCH", "fDOM", "0805")
FRCH_storm6_08_13_fDOM.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_fDOM, "FRCH", "fDOM", "0813")
FRCH_storm7_08_24_fDOM.p = hyst_plot(FRCH_storm7_08_24_Q, FRCH_storm7_08_24_fDOM, "FRCH", "fDOM", "0824")
FRCH_storm8a_08_26_fDOM.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_fDOM, "FRCH", "fDOM", "0826a")

FRCH_storm9_08_30_fDOM.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_fDOM, "FRCH", "fDOM", "0830")
FRCH_storm10_09_01_fDOM.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_fDOM, "FRCH", "fDOM", "0901a")
FRCH_storm11a_09_22_fDOM.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_fDOM, "FRCH", "fDOM", "0922a")

# SPC #
#FRCH_storm1_06_21_SPC.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_SPC, "FRCH", "SPC", "0621")
FRCH_storm2a_06_30_SPC.p = hyst_plot(FRCH_storm2a_06_30_Q, FRCH_storm2a_06_30_SPC, "FRCH", "SPC", "0630")

FRCH_storm3_07_11_SPC.p = hyst_plot(FRCH_storm3_07_11_Q, FRCH_storm3_07_11_SPC, "FRCH", "SPC", "0711")
FRCH_storm4a_07_15_SPC.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_SPC, "FRCH", "SPC", "0715a")
FRCH_storm4b_07_17_SPC.p = hyst_plot(FRCH_storm4b_07_17_Q, FRCH_storm4b_07_17_SPC, "FRCH", "SPC", "0717b")
FRCH_storm5_08_05_SPC.p = hyst_plot(FRCH_storm5_08_05_Q, FRCH_storm5_08_05_SPC, "FRCH", "SPC", "0805")
FRCH_storm6_08_13_SPC.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_SPC, "FRCH", "SPC", "0813")
FRCH_storm7_08_24_SPC.p = hyst_plot(FRCH_storm7_08_24_Q, FRCH_storm7_08_24_SPC, "FRCH", "SPC", "0824")
FRCH_storm8a_08_26_SPC.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_SPC, "FRCH", "SPC", "0826a")

FRCH_storm9_08_30_SPC.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_SPC, "FRCH", "SPC", "0830")
FRCH_storm10_09_01_SPC.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_SPC, "FRCH", "SPC", "0901a")
FRCH_storm11a_09_22_SPC.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_SPC, "FRCH", "SPC", "0922a")

# Turb
#FRCH_storm1_06_21_turb.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_turb, "FRCH", "turb", "0621")
FRCH_storm2a_06_30_turb.p = hyst_plot(FRCH_storm2a_06_30_Q, FRCH_storm2a_06_30_turb, "FRCH", "turb", "0630")

FRCH_storm3_07_11_turb.p = hyst_plot(FRCH_storm3_07_11_Q, FRCH_storm3_07_11_turb, "FRCH", "turb", "0711")
FRCH_storm4a_07_15_turb.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_turb, "FRCH", "turb", "0715a")
FRCH_storm4b_07_17_turb.p = hyst_plot(FRCH_storm4b_07_17_Q, FRCH_storm4b_07_17_turb, "FRCH", "turb", "0717b")
FRCH_storm5_08_05_turb.p = hyst_plot(FRCH_storm5_08_05_Q, FRCH_storm5_08_05_turb, "FRCH", "turb", "0805")
FRCH_storm6_08_13_turb.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_turb, "FRCH", "turb", "0813")
FRCH_storm7_08_24_turb.p = hyst_plot(FRCH_storm7_08_24_Q, FRCH_storm7_08_24_turb, "FRCH", "turb", "0824")
FRCH_storm8a_08_26_turb.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_fDOM, "FRCH", "turb", "0826a")

FRCH_storm9_08_30_turb.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_turb, "FRCH", "turb", "0830")
FRCH_storm10_09_01_turb.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_turh, "FRCH", "turb", "0901a")
FRCH_storm11a_09_22_turb.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_turb, "FRCH", "turb", "0922a")


# ABS
#FRCH_storm1_06_21_abs.p = hyst_plot(FRCH_storm1_06_21_Q, FRCH_storm1_06_21_abs, "FRCH", "abs", "0621")
FRCH_storm2a_06_30_abs.p = hyst_plot(FRCH_storm2a_06_30_Q, FRCH_storm2a_06_30_abs, "FRCH", "abs", "0630")

FRCH_storm3_07_11_abs.p = hyst_plot(FRCH_storm3_07_11_Q, FRCH_storm3_07_11_abs, "FRCH", "abs", "0711")
FRCH_storm4a_07_15_abs.p = hyst_plot(FRCH_storm4a_07_15_Q, FRCH_storm4a_07_15_abs, "FRCH", "abs", "0715a")
FRCH_storm4b_07_17_abs.p = hyst_plot(FRCH_storm4b_07_17_Q, FRCH_storm4b_07_17_abs, "FRCH", "abs", "0717b")
FRCH_storm5_08_05_abs.p = hyst_plot(FRCH_storm5_08_05_Q, FRCH_storm5_08_05_abs, "FRCH", "abs", "0805")
FRCH_storm6_08_13_abs.p = hyst_plot(FRCH_storm6_08_13_Q, FRCH_storm6_08_13_abs, "FRCH", "abs", "0813")
FRCH_storm7_08_24_abs.p = hyst_plot(FRCH_storm7_08_24_Q, FRCH_storm7_08_24_abs, "FRCH", "abs", "0824")
FRCH_storm8a_08_26_abs.p = hyst_plot(FRCH_storm8a_08_26_Q, FRCH_storm8a_08_26_abs, "FRCH", "abs", "0826a")

FRCH_storm9_08_30_abs.p = hyst_plot(FRCH_storm9_08_30_Q, FRCH_storm9_08_30_abs, "FRCH", "abs", "0830")
FRCH_storm10_09_01_abs.p = hyst_plot(FRCH_storm10_09_01_Q, FRCH_storm10_09_01_abs, "FRCH", "abs", "0901a")
FRCH_storm11a_09_22_abs.p = hyst_plot(FRCH_storm11a_09_22_Q, FRCH_storm11a_09_22_abs, "FRCH", "abs", "0922a")


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

FRCH_HI_Loops <- multiplot(
          FRCH_storm2a_06_30_NO3.p, FRCH_storm2a_06_30_fDOM.p, FRCH_storm2a_06_30_SPC.p, FRCH_storm2a_06_30_turb.p,
          
          FRCH_storm3_07_11_NO3.p, FRCH_storm3_07_11_fDOM.p, FRCH_storm3_07_11_SPC.p, FRCH_storm3_07_11_turb.p,
          FRCH_storm4a_07_15_NO3.p, FRCH_storm4a_07_15_fDOM.p, FRCH_storm4a_07_15_SPC.p, FRCH_storm4a_07_15_turb.p,
          FRCH_storm4b_07_17_NO3.p, FRCH_storm4b_07_17_fDOM.p, FRCH_storm4b_07_17_SPC.p, FRCH_storm4b_07_17_turb.p,
          FRCH_storm5_08_05_NO3.p, FRCH_storm5_08_05_fDOM.p, FRCH_storm5_08_05_SPC.p, FRCH_storm5_08_05_turb.p,
          FRCH_storm6_08_13_fDOM.p, FRCH_storm6_08_13_SPC.p, FRCH_storm6_08_13_turb.p,
          FRCH_storm7_08_24_fDOM.p, FRCH_storm7_08_24_SPC.p, FRCH_storm7_08_24_turb.p,
          FRCH_storm8a_08_26_fDOM.p, FRCH_storm8a_08_26_SPC.p, FRCH_storm8a_08_26_turb.p,
          
          FRCH_storm9_08_30_fDOM.p, FRCH_storm9_08_30_SPC.p, FRCH_storm9_08_30_turb.p,
          FRCH_storm10_09_01_fDOM.p, FRCH_storm10_09_01_SPC.p,
          FRCH_storm11a_09_22_fDOM.p, FRCH_storm11a_09_22_SPC.p, FRCH_storm11a_09_22_turb.p,
          
          cols = 7)

# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2018.pdf",
       path = here("plots", "HI_plots", "2018", "FRCH"),
       width = 20, height = 30, units = "in")

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
#MOOS_storm2a_06_29_NO3.p = hyst_plot(MOOS_storm2a_06_29_Q, MOOS_storm2a_06_29_NO3, "MOOS", "NO3", "0629a")

MOOS_storm3_07_10_NO3.p = hyst_plot(MOOS_storm3_07_10_Q, MOOS_storm3_07_10_NO3, "MOOS", "NO3", "0710")
#MOOS_storm4_07_15_NO3.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_NO3, "MOOS", "NO3", "0715")
MOOS_storm5_08_05_NO3.p = hyst_plot(MOOS_storm5_08_05_Q, MOOS_storm5_08_05_NO3, "MOOS", "NO3", "0805")
MOOS_storm6_08_13_NO3.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_NO3, "MOOS", "NO3", "0813")
MOOS_storm7_08_23_NO3.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_NO3, "MOOS", "NO3", "0823")
MOOS_storm8a_08_26_NO3.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_NO3, "MOOS", "NO3", "0826")

MOOS_storm9_08_30_NO3.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_NO3, "MOOS", "NO3", "0830")
MOOS_storm10_09_01_NO3.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_NO3, "MOOS", "NO3", "0901")
MOOS_storm11a_09_22_NO3.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_NO3, "MOOS", "NO3", "0922")


# fDOM #

MOOS_storm2a_06_30_fDOM.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_fDOM, "MOOS", "fDOM", "0630a")

MOOS_storm3_07_10_fDOM.p = hyst_plot(MOOS_storm3_07_10_Q, MOOS_storm3_07_10_fDOM, "MOOS", "fDOM", "0710")
#MOOS_storm4_07_15_fDOM.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_fDOM, "MOOS", "fDOM", "0715")
MOOS_storm5_08_05_fDOM.p = hyst_plot(MOOS_storm5_08_05_Q, MOOS_storm5_08_05_fDOM, "MOOS", "fDOM", "0805")
MOOS_storm6_08_13_fDOM.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_fDOM, "MOOS", "fDOM", "0813")
MOOS_storm7_08_23_fDOM.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_fDOM, "MOOS", "fDOM", "0823")
MOOS_storm8a_08_26_fDOM.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_fDOM, "MOOS", "fDOM", "0826a")

MOOS_storm9_08_30_fDOM.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_fDOM, "MOOS", "fDOM", "0830")
MOOS_storm10_09_01_fDOM.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_fDOM, "MOOS", "fDOM", "0901")
MOOS_storm11a_09_22_fDOM.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_fDOM, "MOOS", "fDOM", "0922")

# SPC #
MOOS_storm2a_06_30_SPC.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_SPC, "MOOS", "SPC", "0630a")

MOOS_storm3_07_10_SPC.p = hyst_plot(MOOS_storm3_07_10_Q, MOOS_storm3_07_10_SPC, "MOOS", "SPC", "0710")
#MOOS_storm4_07_15_SPC.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_SPC, "MOOS", "SPC", "0715")
MOOS_storm5_08_05_SPC.p = hyst_plot(MOOS_storm5_08_05_Q, MOOS_storm5_08_05_SPC, "MOOS", "SPC", "0805")
MOOS_storm6_08_13_SPC.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_SPC, "MOOS", "SPC", "0813")
MOOS_storm7_08_23_SPC.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_SPC, "MOOS", "SPC", "0823")
MOOS_storm8a_08_26_SPC.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_SPC, "MOOS", "SPC", "0826")

MOOS_storm9_08_30_SPC.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_SPC, "MOOS", "SPC", "0830")
MOOS_storm10_09_01_SPC.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_SPC, "MOOS", "SPC", "0901")
MOOS_storm11a_09_22_SPC.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_SPC, "MOOS", "SPC", "0922")


# turb
MOOS_storm2a_06_30_turb.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_turb, "MOOS", "turb", "0630a")

MOOS_storm3_07_10_turb.p = hyst_plot(MOOS_storm3_07_10_Q, MOOS_storm3_07_10_turb, "MOOS", "turb", "0710")
#MOOS_storm4_07_15_turb.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_turb, "MOOS", "turb", "0715")
MOOS_storm5_08_05_turb.p = hyst_plot(MOOS_storm5_08_05_Q, MOOS_storm5_08_05_turb, "MOOS", "turb", "0805")
MOOS_storm6_08_13_turb.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_turb, "MOOS", "turb", "0813")
MOOS_storm7_08_23_turb.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_turb, "MOOS", "turb", "0823")
MOOS_storm8a_08_26_turb.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_turb, "MOOS", "turb", "0826")

MOOS_storm9_08_30_turb.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_turb, "MOOS", "turb", "0830")
MOOS_storm10_09_01_turb.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_turb, "MOOS", "turb", "0901")
MOOS_storm11a_09_22_turb.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_turb, "MOOS", "turb", "0922")


# abs
MOOS_storm2a_06_30_abs.p = hyst_plot(MOOS_storm2a_06_30_Q, MOOS_storm2a_06_30_abs, "MOOS", "abs", "0630a")

MOOS_storm3_07_10_abs.p = hyst_plot(MOOS_storm3_07_10_Q, MOOS_storm3_07_10_abs, "MOOS", "abs", "0710")
#MOOS_storm4_07_15_abs.p = hyst_plot(MOOS_storm4_07_15_Q, MOOS_storm4_07_15_abs, "MOOS", "abs", "0715")
MOOS_storm5_08_05_abs.p = hyst_plot(MOOS_storm5_08_05_Q, MOOS_storm5_08_05_abs, "MOOS", "abs", "0805")
MOOS_storm6_08_13_abs.p = hyst_plot(MOOS_storm6_08_13_Q, MOOS_storm6_08_13_abs, "MOOS", "abs", "0813")
MOOS_storm7_08_23_abs.p = hyst_plot(MOOS_storm7_08_23_Q, MOOS_storm7_08_23_abs, "MOOS", "abs", "0823")
MOOS_storm8a_08_26_abs.p = hyst_plot(MOOS_storm8a_08_26_Q, MOOS_storm8a_08_26_abs, "MOOS", "abs", "0826")

MOOS_storm9_08_30_abs.p = hyst_plot(MOOS_storm9_08_30_Q, MOOS_storm9_08_30_abs, "MOOS", "abs", "0830")
MOOS_storm10_09_01_abs.p = hyst_plot(MOOS_storm10_09_01_Q, MOOS_storm10_09_01_abs, "MOOS", "abs", "0901")
MOOS_storm11a_09_22_abs.p = hyst_plot(MOOS_storm11a_09_22_Q, MOOS_storm11a_09_22_abs, "MOOS", "abs", "0922")



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

multiplot(
          MOOS_storm2a_06_30_fDOM.p, MOOS_storm2a_06_30_SPC.p, MOOS_storm2a_06_30_turb.p,
         
          MOOS_storm3_07_10_NO3.p, MOOS_storm3_07_10_fDOM.p, MOOS_storm3_07_10_SPC.p, MOOS_storm3_07_10_turb.p,
          
          MOOS_storm5_08_05_NO3.p, MOOS_storm5_08_05_fDOM.p, MOOS_storm5_08_05_SPC.p, MOOS_storm5_08_05_turb.p,
          MOOS_storm6_08_13_NO3.p, MOOS_storm6_08_13_fDOM.p, MOOS_storm6_08_13_SPC.p, MOOS_storm6_08_13_turb.p,
          MOOS_storm7_08_23_NO3.p, MOOS_storm7_08_23_fDOM.p, MOOS_storm7_08_23_SPC.p, MOOS_storm7_08_23_turb.p,
          MOOS_storm8a_08_26_NO3.p, MOOS_storm8a_08_26_fDOM.p, MOOS_storm8a_08_26_SPC.p, MOOS_storm8a_08_26_turb.p,
          
          MOOS_storm9_08_30_NO3.p, MOOS_storm9_08_30_fDOM.p, MOOS_storm9_08_30_SPC.p, MOOS_storm9_08_30_turb.p,
          MOOS_storm10_09_01_NO3.p, MOOS_storm10_09_01_fDOM.p, MOOS_storm10_09_01_SPC.p, MOOS_storm10_09_01_turb.p,
          MOOS_storm11a_09_22_NO3.p, MOOS_storm11a_09_22_fDOM.p, MOOS_storm11a_09_22_SPC.p, MOOS_storm11a_09_22_turb.p,
          
          cols = 7
)

# export pdf 20 x 30 #
ggsave("MOOS_HI_Loops_2018.pdf",
       path = here("plots", "HI_plots", "2018", "MOOS"),
       width = 20, height = 30, units = "in")

### CARI ###
#NO3
#CARI_storm1_06_10_NO3.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_NO3, "CARI", "NO3", "0610")
#CARI_storm2_06_21_NO3.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_NO3, "CARI", "NO3", "0621")
CARI_storm3_06_29_NO3.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_NO3, "CARI", "NO3", "0629")
#CARI_storm4a_06_30_NO3.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_NO3, "CARI", "NO3", "0630a")

CARI_storm5a_08_04_NO3.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_NO3, "CARI", "NO3", "0804a")

CARI_storm6_08_13_NO3.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_NO3, "CARI", "NO3", "0813")
#CARI_storm7_08_21_NO3.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_NO3, "CARI", "NO3", "0821")
CARI_storm8_08_24_NO3.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_NO3, "CARI", "NO3", "0824")
CARI_storm9_08_26_NO3.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_NO3, "CARI", "NO3", "0826")
CARI_storm10_08_30_NO3.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_NO3, "CARI", "NO3", "0830")
CARI_storm11_09_01_NO3.p = hyst_plot(CARI_storm11_09_01_Q, CARI_storm11_09_01_NO3, "CARI", "NO3", "0901")
CARI_storm12a_09_20_NO3.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_NO3, "CARI", "NO3", "0920a")
CARI_storm12b_09_25_NO3.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_NO3, "CARI", "NO3", "0925b")

#fDOM
#CARI_storm1_06_10_fDOM.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_fDOM, "CARI", "fDOM", "0610")
#CARI_storm2_06_21_fDOM.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_fDOM, "CARI", "fDOM", "0621")
CARI_storm3_06_29_fDOM.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_fDOM, "CARI", "fDOM", "0629")
#CARI_storm4a_06_30_fDOM.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_fDOM, "CARI", "fDOM", "0630a")

CARI_storm5a_08_04_fDOM.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_fDOM, "CARI", "fDOM", "0804a")

CARI_storm6_08_13_fDOM.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_fDOM, "CARI", "fDOM", "0813")
#CARI_storm7_08_21_fDOM.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_fDOM, "CARI", "fDOM", "0821")
CARI_storm8_08_24_fDOM.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_fDOM, "CARI", "fDOM", "0824")
CARI_storm9_08_26_fDOM.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_fDOM, "CARI", "fDOM", "0826")
CARI_storm10_08_30_fDOM.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_fDOM, "CARI", "fDOM", "0830")
CARI_storm11_09_01_fDOM.p = hyst_plot(CARI_storm11_09_01_Q, CARI_storm11_09_01_fDOM, "CARI", "fDOM", "0901")
CARI_storm12a_09_20_fDOM.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_fDOM, "CARI", "fDOM", "0920a")
CARI_storm12b_09_25_fDOM.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_fDOM, "CARI", "v", "0925b")


#SPC
#CARI_storm1_06_10_SPC.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_SPC, "CARI", "SPC", "0610")
#CARI_storm2_06_21_SPC.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_SPC, "CARI", "SPC", "0621")
CARI_storm3_06_29_SPC.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_SPC, "CARI", "SPC", "0629")
#CARI_storm4a_06_30_SPC.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_SPC, "CARI", "SPC", "0630a")

CARI_storm5a_08_04_SPC.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_SPC, "CARI", "SPC", "0804a")

CARI_storm6_08_13_SPC.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_SPC, "CARI", "SPC", "0813")
#CARI_storm7_08_21_SPC.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_SPC, "CARI", "SPC", "0821")
CARI_storm8_08_24_SPC.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_SPC, "CARI", "SPC", "0824")
CARI_storm9_08_26_SPC.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_SPC, "CARI", "SPC", "0826")
CARI_storm10_08_30_SPC.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_SPC, "CARI", "SPC", "0830")
CARI_storm11_09_01_SPC.p = hyst_plot(CARI_storm11_09_01_Q, CARI_storm11_09_01_SPC, "CARI", "SPC", "0901")
CARI_storm12a_09_20_SPC.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_SPC, "CARI", "SPC", "0920a")
CARI_storm12b_09_25_SPC.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_SPC, "CARI", "SPC", "0925b")


#Turb
#CARI_storm1_06_10_turb.p = hyst_plot(CARI_storm1_06_10_Q, CARI_storm1_06_10_turb, "CARI", "turb", "0610")
#CARI_storm2_06_21_turb.p = hyst_plot(CARI_storm2_06_21_Q, CARI_storm2_06_21_turb, "CARI", "turb", "0621")
CARI_storm3_06_29_turb.p = hyst_plot(CARI_storm3_06_29_Q, CARI_storm3_06_29_turb, "CARI", "turb", "0629")
#CARI_storm4a_06_30_turb.p = hyst_plot(CARI_storm4a_06_30_Q, CARI_storm4a_06_30_turb, "CARI", "turb", "0630a")

CARI_storm5a_08_04_turb.p = hyst_plot(CARI_storm5a_08_04_Q, CARI_storm5a_08_04_turb, "CARI", "turb", "0804a")

CARI_storm6_08_13_turb.p = hyst_plot(CARI_storm6_08_13_Q, CARI_storm6_08_13_turb, "CARI", "turb", "0813")
#CARI_storm7_08_21_turb.p = hyst_plot(CARI_storm7_08_21_Q, CARI_storm7_08_21_turb, "CARI", "turb", "0821")
CARI_storm8_08_24_turb.p = hyst_plot(CARI_storm8_08_24_Q, CARI_storm8_08_24_turb, "CARI", "turb", "0824")
CARI_storm9_08_26_turb.p = hyst_plot(CARI_storm9_08_26_Q, CARI_storm9_08_26_turb, "CARI", "turb", "0826")
CARI_storm10_08_30_turb.p = hyst_plot(CARI_storm10_08_30_Q, CARI_storm10_08_30_turb, "CARI", "turb", "0830")
CARI_storm11_09_01_turb.p = hyst_plot(CARI_storm11_09_01_Q, CARI_storm11_09_01_turb, "CARI", "turb", "0901")
CARI_storm12a_09_20_turb.p = hyst_plot(CARI_storm12a_09_20_Q, CARI_storm12a_09_20_turb, "CARI", "turb", "0920a")
CARI_storm12b_09_25_turb.p = hyst_plot(CARI_storm12b_09_25_Q, CARI_storm12b_09_25_turb, "CARI", "turb", "0925b")



multiplot(
  CARI_storm3_06_29_NO3.p,CARI_storm3_06_29_fDOM.p,CARI_storm3_06_29_SPC.p,CARI_storm3_06_29_turb.p,
  
  CARI_storm5a_08_04_NO3.p, CARI_storm5a_08_04_fDOM.p,CARI_storm5a_08_04_SPC.p,CARI_storm5a_08_04_turb.p,
 
  
  CARI_storm6_08_13_NO3.p, CARI_storm6_08_13_fDOM.p,CARI_storm6_08_13_SPC.p,CARI_storm6_08_13_turb.p,

  CARI_storm8_08_24_NO3.p, CARI_storm8_08_24_fDOM.p,CARI_storm8_08_24_SPC.p,CARI_storm8_08_24_turb.p,
  CARI_storm9_08_26_NO3.p, CARI_storm9_08_26_fDOM.p,CARI_storm9_08_26_SPC.p,CARI_storm9_08_26_turb.p,
  CARI_storm10_08_30_NO3.p, CARI_storm10_08_30_fDOM.p,CARI_storm10_08_30_SPC.p,CARI_storm10_08_30_turb.p,
  CARI_storm11_09_01_NO3.p, CARI_storm11_09_01_fDOM.p,CARI_storm11_09_01_SPC.p,CARI_storm11_09_01_turb.p,
  CARI_storm12a_09_20_NO3.p, CARI_storm12a_09_20_fDOM.p,CARI_storm12a_09_20_SPC.p,CARI_storm12a_09_20_turb.p,
  CARI_storm12b_09_25_NO3.p, CARI_storm12b_09_25_fDOM.p,CARI_storm12b_09_25_SPC.p,CARI_storm12b_09_25_turb.p,
  cols = 7
)


# export pdf 20 x 30 #
ggsave("CARI_HI_Loops_2018.pdf",
       path = here("plots", "HI_plots", "2018", "CARI"),
       width = 20, height = 30, units = "in")




############################################### 2019 ##############################################
# plot on normalized scale #
#### load data #
STRT_storm1_05_31_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm1_05_31_Q.csv"))
STRT_storm1_05_31_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm1_05_31_NO3.csv"))
STRT_storm1_05_31_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm1_05_31_fDOM.csv"))
STRT_storm1_05_31_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm1_05_31_SPC.csv"))
STRT_storm1_05_31_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm1_05_31_Turb.csv"))
STRT_storm1_05_31_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm1_05_31_abs.csv"))

STRT_storm2_07_12_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm2_07_12_Q.csv"))
STRT_storm2_07_12_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm2_07_12_NO3.csv"))
STRT_storm2_07_12_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm2_07_12_fDOM.csv"))
STRT_storm2_07_12_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm2_07_12_SPC.csv"))
STRT_storm2_07_12_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm2_07_12_Turb.csv"))
STRT_storm2_07_12_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm2_07_12_abs.csv"))

STRT_storm3a_07_25_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3a_07_25_Q.csv"))
STRT_storm3a_07_25_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3a_07_25_NO3.csv"))
STRT_storm3a_07_25_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3a_07_25_fDOM.csv"))
STRT_storm3a_07_25_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3a_07_25_SPC.csv"))
STRT_storm3a_07_25_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3a_07_25_Turb.csv"))
STRT_storm3a_07_25_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3a_07_25_abs.csv"))

STRT_storm3b_08_05_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3b_08_05_Q.csv"))
STRT_storm3b_08_05_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3b_08_05_NO3.csv"))
STRT_storm3b_08_05_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3b_08_05_fDOM.csv"))
STRT_storm3b_08_05_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3b_08_05_SPC.csv"))
STRT_storm3b_08_05_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3b_08_05_Turb.csv"))
STRT_storm3b_08_05_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3b_08_05_abs.csv"))

STRT_storm3c_08_12_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3c_08_12_Q.csv"))
STRT_storm3c_08_12_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3c_08_12_NO3.csv"))
STRT_storm3c_08_12_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3c_08_12_fDOM.csv"))
STRT_storm3c_08_12_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3c_08_12_SPC.csv"))
STRT_storm3c_08_12_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3c_08_12_Turb.csv"))
STRT_storm3c_08_12_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm3c_08_12_abs.csv"))

STRT_storm4_08_15_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm4_08_15_Q.csv"))
STRT_storm4_08_15_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm4_08_15_NO3.csv"))
STRT_storm4_08_15_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm4_08_15_fDOM.csv"))
STRT_storm4_08_15_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm4_08_15_SPC.csv"))
STRT_storm4_08_15_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm4_08_15_Turb.csv"))
STRT_storm4_08_15_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm4_08_15_abs.csv"))

STRT_storm5_08_20_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm5_08_20_Q.csv"))
STRT_storm5_08_20_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm5_08_20_NO3.csv"))
STRT_storm5_08_20_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm5_08_20_fDOM.csv"))
STRT_storm5_08_20_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm5_08_20_SPC.csv"))
STRT_storm5_08_20_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm5_08_20_Turb.csv"))
STRT_storm5_08_20_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm5_08_20_abs.csv"))

STRT_storm6_09_20_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm6_09_20_Q.csv"))
STRT_storm6_09_20_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm6_09_20_NO3.csv"))
STRT_storm6_09_20_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm6_09_20_fDOM.csv"))
STRT_storm6_09_20_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm6_09_20_SPC.csv"))
STRT_storm6_09_20_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm6_09_20_Turb.csv"))
STRT_storm6_09_20_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm6_09_20_abs.csv"))

STRT_storm7_10_01_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7_10_01_Q.csv"))
STRT_storm7_10_01_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7_10_01_NO3.csv"))
STRT_storm7_10_01_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7_10_01_fDOM.csv"))
STRT_storm7_10_01_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7_10_01_SPC.csv"))
STRT_storm7_10_01_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7_10_01_Turb.csv"))
STRT_storm7_10_01_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7_10_01_abs.csv"))

STRT_storm7b_10_04_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7b_10_04_Q.csv"))
STRT_storm7b_10_04_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7b_10_04_NO3.csv"))
STRT_storm7b_10_04_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7b_10_04_fDOM.csv"))
STRT_storm7b_10_04_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7b_10_04_SPC.csv"))
STRT_storm7b_10_04_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7b_10_04_Turb.csv"))
STRT_storm7b_10_04_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7b_10_04_abs.csv"))

STRT_storm7c_10_09_Q <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7c_10_09_Q.csv"))
STRT_storm7c_10_09_NO3 <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7c_10_09_NO3.csv"))
STRT_storm7c_10_09_fDOM <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7c_10_09_fDOM.csv"))
STRT_storm7c_10_09_SPC <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7c_10_09_SPC.csv"))
STRT_storm7c_10_09_turb <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7c_10_09_Turb.csv"))
STRT_storm7c_10_09_abs <- read_csv(here("Storm_Events", "2019", "STRT", "STRT_storm7c_10_09_abs.csv"))


# MOOS #

MOOS_storm1_06_01_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm1_06_01_Q.csv"))
MOOS_storm1_06_01_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm1_06_01_NO3.csv"))
MOOS_storm1_06_01_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm1_06_01_fDOM.csv"))
MOOS_storm1_06_01_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm1_06_01_SPC.csv"))
MOOS_storm1_06_01_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm1_06_01_Turb.csv"))
MOOS_storm1_06_01_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm1_06_01_abs.csv"))

MOOS_storm3_07_12_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm3_07_12_Q.csv"))
MOOS_storm3_07_12_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm3_07_12_NO3.csv"))
MOOS_storm3_07_12_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm3_07_12_fDOM.csv"))
MOOS_storm3_07_12_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm3_07_12_SPC.csv"))
MOOS_storm3_07_12_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm3_07_12_Turb.csv"))
MOOS_storm3_07_12_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm3_07_12_abs.csv"))

MOOS_storm4_07_25_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm4_07_25_Q.csv"))
MOOS_storm4_07_25_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm4_07_25_fDOM.csv"))
MOOS_storm4_07_25_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm4_07_25_SPC.csv"))
MOOS_storm4_07_25_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm4_07_25_Turb.csv"))
MOOS_storm4_07_25_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm4_07_25_abs.csv"))

MOOS_storm5_07_29_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm5_07_29_Q.csv"))
MOOS_storm5_07_29_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm5_07_29_NO3.csv"))
MOOS_storm5_07_29_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm5_07_29_fDOM.csv"))
MOOS_storm5_07_29_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm5_07_29_SPC.csv"))
MOOS_storm5_07_29_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm5_07_29_Turb.csv"))
MOOS_storm5_07_29_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm5_07_29_abs.csv"))

MOOS_storm6a_08_01_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6a_08_01_Q.csv"))
MOOS_storm6a_08_01_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6a_08_01_NO3.csv"))
MOOS_storm6a_08_01_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6a_08_01_fDOM.csv"))
MOOS_storm6a_08_01_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6a_08_01_SPC.csv"))
MOOS_storm6a_08_01_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6a_08_01_Turb.csv"))
MOOS_storm6a_08_01_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6a_08_01_abs.csv"))

MOOS_storm6d_08_05_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6d_08_05_Q.csv"))
MOOS_storm6d_08_05_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6d_08_05_NO3.csv"))
MOOS_storm6d_08_05_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6d_08_05_fDOM.csv"))
MOOS_storm6d_08_05_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6d_08_05_SPC.csv"))
MOOS_storm6d_08_05_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6d_08_05_Turb.csv"))
MOOS_storm6d_08_05_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm6d_08_05_abs.csv"))

MOOS_storm7a_08_13_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7a_08_13_Q.csv"))
MOOS_storm7a_08_13_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7a_08_13_NO3.csv"))
MOOS_storm7a_08_13_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7a_08_13_fDOM.csv"))
MOOS_storm7a_08_13_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7a_08_13_SPC.csv"))
MOOS_storm7a_08_13_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7a_08_13_Turb.csv"))
MOOS_storm7a_08_13_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7a_08_13_abs.csv"))

MOOS_storm7b_08_14_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7b_08_14_Q.csv"))
MOOS_storm7b_08_14_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7b_08_14_NO3.csv"))
MOOS_storm7b_08_14_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7b_08_14_fDOM.csv"))
MOOS_storm7b_08_14_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7b_08_14_SPC.csv"))
MOOS_storm7b_08_14_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7b_08_14_Turb.csv"))
MOOS_storm7b_08_14_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7b_08_14_abs.csv"))

MOOS_storm7c_08_15_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7c_08_15_Q.csv"))
MOOS_storm7c_08_15_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7c_08_15_NO3.csv"))
MOOS_storm7c_08_15_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7c_08_15_fDOM.csv"))
MOOS_storm7c_08_15_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7c_08_15_SPC.csv"))
MOOS_storm7c_08_15_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7c_08_15_Turb.csv"))
MOOS_storm7c_08_15_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm7c_08_15_abs.csv"))

MOOS_storm8_09_21_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm8_09_21_Q.csv"))
MOOS_storm8_09_21_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm8_09_21_NO3.csv"))
MOOS_storm8_09_21_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm8_09_21_fDOM.csv"))
MOOS_storm8_09_21_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm8_09_21_SPC.csv"))
MOOS_storm8_09_21_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm8_09_21_Turb.csv"))
MOOS_storm8_09_21_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm8_09_21_abs.csv"))

MOOS_storm9_10_02_Q <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm9_10_02_Q.csv"))
MOOS_storm9_10_02_NO3 <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm9_10_02_NO3.csv"))
MOOS_storm9_10_02_fDOM <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm9_10_02_fDOM.csv"))
MOOS_storm9_10_02_SPC <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm9_10_02_SPC.csv"))
MOOS_storm9_10_02_turb <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm9_10_02_turb.csv"))
MOOS_storm9_10_02_abs <- read_csv(here("Storm_Events", "2019", "MOOS", "MOOS_storm9_10_02_abs.csv"))

# FRCH #
FRCH_storm1_05_31_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm1_05_31_Q.csv"))
FRCH_storm1_05_31_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm1_05_31_NO3.csv"))
FRCH_storm1_05_31_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm1_05_31_fDOM.csv"))
FRCH_storm1_05_31_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm1_05_31_SPC.csv"))
FRCH_storm1_05_31_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm1_05_31_turb.csv"))
FRCH_storm1_05_31_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm1_05_31_abs.csv"))

FRCH_storm2_06_15_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm2_06_15_Q.csv"))
FRCH_storm2_06_15_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm2_06_15_NO3.csv"))
FRCH_storm2_06_15_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm2_06_15_fDOM.csv"))
FRCH_storm2_06_15_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm2_06_15_SPC.csv"))
FRCH_storm2_06_15_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm2_06_15_turb.csv"))
FRCH_storm2_06_15_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm2_06_15_abs.csv"))

FRCH_storm3_06_18_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm3_06_18_Q.csv"))
FRCH_storm3_06_18_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm3_06_18_NO3.csv"))
FRCH_storm3_06_18_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm3_06_18_fDOM.csv"))
FRCH_storm3_06_18_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm3_06_18_SPC.csv"))
FRCH_storm3_06_18_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm3_06_18_turb.csv"))
FRCH_storm3_06_18_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm3_06_18_abs.csv"))

FRCH_storm4_06_20_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm4_06_20_Q.csv"))
FRCH_storm4_06_20_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm4_06_20_NO3.csv"))
FRCH_storm4_06_20_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm4_06_20_fDOM.csv"))
FRCH_storm4_06_20_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm4_06_20_SPC.csv"))
FRCH_storm4_06_20_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm4_06_20_turb.csv"))
FRCH_storm4_06_20_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm4_06_20_abs.csv"))

FRCH_storm5_06_22_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm5_06_22_Q.csv"))
FRCH_storm5_06_22_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm5_06_22_NO3.csv"))
FRCH_storm5_06_22_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm5_06_22_fDOM.csv"))
FRCH_storm5_06_22_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm5_06_22_SPC.csv"))
FRCH_storm5_06_22_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm5_06_22_turb.csv"))
FRCH_storm5_06_22_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm5_06_22_abs.csv"))

FRCH_storm6_07_12_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm6_07_12_Q.csv"))
FRCH_storm6_07_12_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm6_07_12_NO3.csv"))
FRCH_storm6_07_12_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm6_07_12_fDOM.csv"))
FRCH_storm6_07_12_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm6_07_12_SPC.csv"))
FRCH_storm6_07_12_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm6_07_12_turb.csv"))
FRCH_storm6_07_12_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm6_07_12_abs.csv"))


FRCH_storm10a_08_01_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm10a_08_01_Q.csv"))
FRCH_storm10a_08_01_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm10a_08_01_NO3.csv"))
FRCH_storm10a_08_01_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm10a_08_01_fDOM.csv"))
FRCH_storm10a_08_01_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm10a_08_01_SPC.csv"))
FRCH_storm10a_08_01_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm10a_08_01_turb.csv"))
FRCH_storm10a_08_01_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm10a_08_01_abs.csv"))

FRCH_storm11_08_05_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm11_08_05_Q.csv"))
FRCH_storm11_08_05_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm11_08_05_NO3.csv"))
FRCH_storm11_08_05_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm11_08_05_fDOM.csv"))
FRCH_storm11_08_05_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm11_08_05_SPC.csv"))
FRCH_storm11_08_05_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm11_08_05_turb.csv"))
FRCH_storm11_08_05_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm11_08_05_abs.csv"))

FRCH_storm12a_08_12_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12a_08_12_Q.csv"))
FRCH_storm12a_08_12_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12a_08_12_NO3.csv"))
FRCH_storm12a_08_12_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12a_08_12_fDOM.csv"))
FRCH_storm12a_08_12_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12a_08_12_SPC.csv"))
FRCH_storm12a_08_12_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12a_08_12_turb.csv"))
FRCH_storm12a_08_12_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12a_08_12_abs.csv"))

FRCH_storm12c_08_15_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12c_08_15_Q.csv"))
FRCH_storm12c_08_15_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12c_08_15_NO3.csv"))
FRCH_storm12c_08_15_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12c_08_15_fDOM.csv"))
FRCH_storm12c_08_15_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12c_08_15_SPC.csv"))
FRCH_storm12c_08_15_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12c_08_15_turb.csv"))
FRCH_storm12c_08_15_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm12c_08_15_abs.csv"))

FRCH_storm13_09_20_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm13_09_20_Q.csv"))
FRCH_storm13_09_20_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm13_09_20_NO3.csv"))
FRCH_storm13_09_20_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm13_09_20_fDOM.csv"))
FRCH_storm13_09_20_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm13_09_20_SPC.csv"))
FRCH_storm13_09_20_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm13_09_20_turb.csv"))
FRCH_storm13_09_20_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm13_09_20_abs.csv"))

FRCH_storm14_10_01_Q <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm14_10_01_Q.csv"))
FRCH_storm14_10_01_NO3 <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm14_10_01_NO3.csv"))
FRCH_storm14_10_01_fDOM <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm14_10_01_fDOM.csv"))
FRCH_storm14_10_01_SPC <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm14_10_01_SPC.csv"))
FRCH_storm14_10_01_turb <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm14_10_01_turb.csv"))
FRCH_storm14_10_01_abs <- read_csv(here("Storm_Events", "2019", "FRCH", "FRCH_storm14_10_01_abs.csv"))

# VAUL # 
VAUL_storm1_07_13_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm1_07_13_Q.csv"))
VAUL_storm1_07_13_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm1_07_13_NO3.csv"))
VAUL_storm1_07_13_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm1_07_13_fDOM.csv"))
VAUL_storm1_07_13_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm1_07_13_SPC.csv"))
VAUL_storm1_07_13_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm1_07_13_turb.csv"))
VAUL_storm1_07_13_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm1_07_13_abs.csv"))

VAUL_storm2_07_26_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm2_07_26_Q.csv"))
VAUL_storm2_07_26_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm2_07_26_NO3.csv"))
VAUL_storm2_07_26_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm2_07_26_fDOM.csv"))
VAUL_storm2_07_26_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm2_07_26_SPC.csv"))
VAUL_storm2_07_26_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm2_07_26_turb.csv"))
VAUL_storm2_07_26_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm2_07_26_abs.csv"))

VAUL_storm3_07_29_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm3_07_29_Q.csv"))
VAUL_storm3_07_29_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm3_07_29_NO3.csv"))
VAUL_storm3_07_29_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm3_07_29_fDOM.csv"))
VAUL_storm3_07_29_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm3_07_29_SPC.csv"))
VAUL_storm3_07_29_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm3_07_29_turb.csv"))
VAUL_storm3_07_29_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm3_07_29_abs.csv"))

VAUL_storm4a_08_02_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4a_08_02_Q.csv"))
VAUL_storm4a_08_02_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4a_08_02_NO3.csv"))
VAUL_storm4a_08_02_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4a_08_02_fDOM.csv"))
VAUL_storm4a_08_02_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4a_08_02_SPC.csv"))
VAUL_storm4a_08_02_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4a_08_02_turb.csv"))
VAUL_storm4a_08_02_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4a_08_02_abs.csv"))


VAUL_storm4c_08_05_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4c_08_05_Q.csv"))
VAUL_storm4c_08_05_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4c_08_05_NO3.csv"))
VAUL_storm4c_08_05_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4c_08_05_fDOM.csv"))
VAUL_storm4c_08_05_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4c_08_05_SPC.csv"))
VAUL_storm4c_08_05_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4c_08_05_turb.csv"))
VAUL_storm4c_08_05_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm4c_08_05_abs.csv"))

VAUL_storm5_08_12_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm5_08_12_Q.csv"))
VAUL_storm5_08_12_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm5_08_12_NO3.csv"))
VAUL_storm5_08_12_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm5_08_12_fDOM.csv"))
VAUL_storm5_08_12_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm5_08_12_SPC.csv"))
VAUL_storm5_08_12_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm5_08_12_turb.csv"))
VAUL_storm5_08_12_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm5_08_12_abs.csv"))

VAUL_storm6_08_15_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm6_08_15_Q.csv"))
VAUL_storm6_08_15_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm6_08_15_NO3.csv"))
VAUL_storm6_08_15_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm6_08_15_fDOM.csv"))
VAUL_storm6_08_15_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm6_08_15_SPC.csv"))
VAUL_storm6_08_15_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm6_08_15_turb.csv"))
VAUL_storm6_08_15_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm6_08_15_abs.csv"))

VAUL_storm7_09_19_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm7_09_19_Q.csv"))
VAUL_storm7_09_19_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm7_09_19_NO3.csv"))
VAUL_storm7_09_19_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm7_09_19_fDOM.csv"))
VAUL_storm7_09_19_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm7_09_19_SPC.csv"))
VAUL_storm7_09_19_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm7_09_19_turb.csv"))
VAUL_storm7_09_19_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm7_09_19_abs.csv"))

VAUL_storm8a_09_29_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8a_09_29_Q.csv"))
VAUL_storm8a_09_29_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8a_09_29_NO3.csv"))
VAUL_storm8a_09_29_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8a_09_29_fDOM.csv"))
VAUL_storm8a_09_29_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8a_09_29_SPC.csv"))
VAUL_storm8a_09_29_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8a_09_29_turb.csv"))
VAUL_storm8a_09_29_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8a_09_29_abs.csv"))

VAUL_storm8c_10_04_Q <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8c_10_04_Q.csv"))
VAUL_storm8c_10_04_NO3 <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8c_10_04_NO3.csv"))
VAUL_storm8c_10_04_fDOM <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8c_10_04_fDOM.csv"))
VAUL_storm8c_10_04_SPC <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8c_10_04_SPC.csv"))
VAUL_storm8c_10_04_turb <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8c_10_04_turb.csv"))
VAUL_storm8c_10_04_abs <- read_csv(here("Storm_Events", "2019", "VAUL", "VAUL_storm8c_10_04_abs.csv"))


###

# POKE # 
POKE_storm1_06_30_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm1_06_30_Q.csv"))
POKE_storm1_06_30_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm1_06_30_NO3.csv"))
POKE_storm1_06_30_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm1_06_30_fDOM.csv"))
POKE_storm1_06_30_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm1_06_30_SPC.csv"))
POKE_storm1_06_30_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm1_06_30_Turb.csv"))
POKE_storm1_06_30_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm1_06_30_abs.csv"))

POKE_storm2_07_12_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm2_07_12_Q.csv"))
POKE_storm2_07_12_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm2_07_12_NO3.csv"))
POKE_storm2_07_12_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm2_07_12_fDOM.csv"))
POKE_storm2_07_12_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm2_07_12_SPC.csv"))
POKE_storm2_07_12_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm2_07_12_Turb.csv"))
POKE_storm2_07_12_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm2_07_12_abs.csv"))

POKE_storm3_07_26_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm3_07_26_Q.csv"))
POKE_storm3_07_26_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm3_07_26_NO3.csv"))
POKE_storm3_07_26_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm3_07_26_fDOM.csv"))
POKE_storm3_07_26_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm3_07_26_SPC.csv"))
POKE_storm3_07_26_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm3_07_26_Turb.csv"))
POKE_storm3_07_26_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm3_07_26_abs.csv"))

POKE_storm4_07_31_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm4_07_31_Q.csv"))
POKE_storm4_07_31_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm4_07_31_NO3.csv"))
POKE_storm4_07_31_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm4_07_31_fDOM.csv"))
POKE_storm4_07_31_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm4_07_31_SPC.csv"))
POKE_storm4_07_31_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm4_07_31_Turb.csv"))
POKE_storm4_07_31_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm4_07_31_abs.csv"))

POKE_storm5a_08_02_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5a_08_02_Q.csv"))
POKE_storm5a_08_02_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5a_08_02_NO3.csv"))
POKE_storm5a_08_02_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5a_08_02_fDOM.csv"))
POKE_storm5a_08_02_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5a_08_02_SPC.csv"))
POKE_storm5a_08_02_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5a_08_02_Turb.csv"))
POKE_storm5a_08_02_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5a_08_02_abs.csv"))

POKE_storm5c_08_05_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5c_08_05_Q.csv"))
POKE_storm5c_08_05_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5c_08_05_NO3.csv"))
POKE_storm5c_08_05_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5c_08_05_fDOM.csv"))
POKE_storm5c_08_05_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5c_08_05_SPC.csv"))
POKE_storm5c_08_05_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5c_08_05_Turb.csv"))
POKE_storm5c_08_05_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5c_08_05_abs.csv"))

POKE_storm5d_08_10_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5d_08_10_Q.csv"))
POKE_storm5d_08_10_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5d_08_10_NO3.csv"))
POKE_storm5d_08_10_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5d_08_10_fDOM.csv"))
POKE_storm5d_08_10_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5d_08_10_SPC.csv"))
POKE_storm5d_08_10_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5d_08_10_Turb.csv"))
POKE_storm5d_08_10_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm5d_08_10_abs.csv"))

POKE_storm6a_08_12_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm6a_08_12_Q.csv"))
POKE_storm6a_08_12_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm6a_08_12_NO3.csv"))
POKE_storm6a_08_12_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm6a_08_12_fDOM.csv"))
POKE_storm6a_08_12_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm6a_08_12_SPC.csv"))
POKE_storm6a_08_12_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm6a_08_12_Turb.csv"))
POKE_storm6a_08_12_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm6a_08_12_abs.csv"))

POKE_storm7_08_15_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm7_08_15_Q.csv"))
POKE_storm7_08_15_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm7_08_15_NO3.csv"))
POKE_storm7_08_15_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm7_08_15_fDOM.csv"))
POKE_storm7_08_15_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm7_08_15_SPC.csv"))
POKE_storm7_08_15_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm7_08_15_Turb.csv"))
POKE_storm7_08_15_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm7_08_15_abs.csv"))

POKE_storm8_09_29_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm8_09_29_Q.csv"))
POKE_storm8_09_29_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm8_09_29_NO3.csv"))
POKE_storm8_09_29_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm8_09_29_fDOM.csv"))
POKE_storm8_09_29_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm8_09_29_SPC.csv"))
POKE_storm8_09_29_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm8_09_29_Turb.csv"))
POKE_storm8_09_29_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm8_09_29_abs.csv"))

POKE_storm9_10_04_Q <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm9_10_04_Q.csv"))
POKE_storm9_10_04_NO3 <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm9_10_04_NO3.csv"))
POKE_storm9_10_04_fDOM <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm9_10_04_fDOM.csv"))
POKE_storm9_10_04_SPC <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm9_10_04_SPC.csv"))
POKE_storm9_10_04_turb <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm9_10_04_Turb.csv"))
POKE_storm9_10_04_abs <- read_csv(here("Storm_Events", "2019", "POKE", "POKE_storm9_10_04_abs.csv"))


# CARI # 
CARI_storm1_05_08_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm1_05_08_Q.csv"))
CARI_storm1_05_08_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm1_05_08_NO3.csv"))
CARI_storm1_05_08_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm1_05_08_fDOM.csv"))
CARI_storm1_05_08_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm1_05_08_SPC.csv"))
CARI_storm1_05_08_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm1_05_08_Turb.csv"))

CARI_storm2_06_30_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm2_06_30_Q.csv"))
CARI_storm2_06_30_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm2_06_30_NO3.csv"))
CARI_storm2_06_30_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm2_06_30_fDOM.csv"))
CARI_storm2_06_30_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm2_06_30_SPC.csv"))
CARI_storm2_06_30_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm2_06_30_Turb.csv"))

CARI_storm3_07_12_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm3_07_12_Q.csv"))
CARI_storm3_07_12_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm3_07_12_NO3.csv"))
CARI_storm3_07_12_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm3_07_12_fDOM.csv"))
CARI_storm3_07_12_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm3_07_12_SPC.csv"))
CARI_storm3_07_12_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm3_07_12_Turb.csv"))

CARI_storm5_07_31_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm5_07_31_Q.csv"))
CARI_storm5_07_31_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm5_07_31_NO3.csv"))
CARI_storm5_07_31_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm5_07_31_fDOM.csv"))
CARI_storm5_07_31_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm5_07_31_SPC.csv"))
CARI_storm5_07_31_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm5_07_31_Turb.csv"))

CARI_storm6a_08_02_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6a_08_02_Q.csv"))
CARI_storm6a_08_02_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6a_08_02_NO3.csv"))
CARI_storm6a_08_02_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6a_08_02_fDOM.csv"))
CARI_storm6a_08_02_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6a_08_02_SPC.csv"))
CARI_storm6a_08_02_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6a_08_02_Turb.csv"))

CARI_storm6c_08_05_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6c_08_05_Q.csv"))
CARI_storm6c_08_05_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6c_08_05_NO3.csv"))
CARI_storm6c_08_05_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6c_08_05_fDOM.csv"))
CARI_storm6c_08_05_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6c_08_05_SPC.csv"))
CARI_storm6c_08_05_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6c_08_05_Turb.csv"))

CARI_storm6d_08_10_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6d_08_10_Q.csv"))
CARI_storm6d_08_10_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6d_08_10_NO3.csv"))
CARI_storm6d_08_10_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6d_08_10_fDOM.csv"))
CARI_storm6d_08_10_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6d_08_10_SPC.csv"))
CARI_storm6d_08_10_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm6d_08_10_Turb.csv"))

CARI_storm7a_08_13_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm7a_08_13_Q.csv"))
CARI_storm7a_08_13_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm7a_08_13_NO3.csv"))
CARI_storm7a_08_13_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm7a_08_13_fDOM.csv"))
CARI_storm7a_08_13_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm7a_08_13_SPC.csv"))
CARI_storm7a_08_13_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm7a_08_13_Turb.csv"))

CARI_storm8_08_16_Q <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm8_08_16_Q.csv"))
CARI_storm8_08_16_NO3 <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm8_08_16_NO3.csv"))
CARI_storm8_08_16_fDOM <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm8_08_16_fDOM.csv"))
CARI_storm8_08_16_SPC <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm8_08_16_SPC.csv"))
CARI_storm8_08_16_turb <- read_csv(here("Storm_Events", "2019", "CARI", "CARI_storm8_08_16_Turb.csv"))


# normalize data #
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

#### fxn: plot hysteresis loop ####
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

# plot STRT loops #
# NO3
STRT_storm1_05_31_NO3.p = hyst_plot(STRT_storm1_05_31_Q, STRT_storm1_05_31_NO3, "STRT", "NO3", "0531")
STRT_storm2_07_12_NO3.p = hyst_plot(STRT_storm2_07_12_Q, STRT_storm2_07_12_NO3, "STRT", "NO3", "0712")
STRT_storm3a_07_25_NO3.p = hyst_plot(STRT_storm3a_07_25_Q, STRT_storm3a_07_25_NO3, "STRT", "NO3", "0725a")
STRT_storm3b_08_05_NO3.p = hyst_plot(STRT_storm3b_08_05_Q, STRT_storm3b_08_05_NO3, "STRT", "NO3", "0805b")
STRT_storm3c_08_12_NO3.p = hyst_plot(STRT_storm3c_08_12_Q, STRT_storm3c_08_12_NO3, "STRT", "NO3", "0812c")
STRT_storm4_08_15_NO3.p = hyst_plot(STRT_storm4_08_15_Q, STRT_storm4_08_15_NO3, "STRT", "NO3", "0815")
STRT_storm5_08_20_NO3.p = hyst_plot(STRT_storm5_08_20_Q, STRT_storm5_08_20_NO3, "STRT", "NO3", "0820")
STRT_storm6_09_20_NO3.p = hyst_plot(STRT_storm6_09_20_Q, STRT_storm6_09_20_NO3, "STRT", "NO3", "0920")
STRT_storm7_10_01_NO3.p = hyst_plot(STRT_storm7_10_01_Q, STRT_storm7_10_01_NO3, "STRT", "NO3", "1001a")
STRT_storm7b_10_04_NO3.p = hyst_plot(STRT_storm7b_10_04_Q, STRT_storm7b_10_04_NO3, "STRT", "NO3", "1004b")
STRT_storm7c_10_09_NO3.p = hyst_plot(STRT_storm7c_10_09_Q, STRT_storm7c_10_09_NO3, "STRT", "NO3", "1009c")

# fDOM #
STRT_storm1_05_31_fDOM.p = hyst_plot(STRT_storm1_05_31_Q, STRT_storm1_05_31_fDOM, "STRT", "fDOM", "0531")
STRT_storm2_07_12_fDOM.p = hyst_plot(STRT_storm2_07_12_Q, STRT_storm2_07_12_fDOM, "STRT", "fDOM", "0712")
STRT_storm3a_07_25_fDOM.p = hyst_plot(STRT_storm3a_07_25_Q, STRT_storm3a_07_25_fDOM, "STRT", "fDOM", "0725a")
STRT_storm3b_08_05_fDOM.p = hyst_plot(STRT_storm3b_08_05_Q, STRT_storm3b_08_05_fDOM, "STRT", "fDOM", "0805b")
STRT_storm3c_08_12_fDOM.p = hyst_plot(STRT_storm3c_08_12_Q, STRT_storm3c_08_12_fDOM, "STRT", "fDOM", "0812c")
STRT_storm4_08_15_fDOM.p = hyst_plot(STRT_storm4_08_15_Q, STRT_storm4_08_15_fDOM, "STRT", "fDOM", "0815")
STRT_storm5_08_20_fDOM.p = hyst_plot(STRT_storm5_08_20_Q, STRT_storm5_08_20_fDOM, "STRT", "fDOM", "0820")
STRT_storm6_09_20_fDOM.p = hyst_plot(STRT_storm6_09_20_Q, STRT_storm6_09_20_fDOM, "STRT", "fDOM", "0920")
STRT_storm7_10_01_fDOM.p = hyst_plot(STRT_storm7_10_01_Q, STRT_storm7_10_01_fDOM, "STRT", "fDOM", "1001a")
STRT_storm7b_10_04_fDOM.p = hyst_plot(STRT_storm7b_10_04_Q, STRT_storm7b_10_04_fDOM, "STRT", "fDOM", "1004b")
STRT_storm7c_10_09_fDOM.p = hyst_plot(STRT_storm7c_10_09_Q, STRT_storm7c_10_09_fDOM, "STRT", "fDOM", "1009c")

# SPC #
STRT_storm1_05_31_SPC.p = hyst_plot(STRT_storm1_05_31_Q, STRT_storm1_05_31_SPC, "STRT", "SPC", "0531")
STRT_storm2_07_12_SPC.p = hyst_plot(STRT_storm2_07_12_Q, STRT_storm2_07_12_SPC, "STRT", "SPC", "0712")
STRT_storm3a_07_25_SPC.p = hyst_plot(STRT_storm3a_07_25_Q, STRT_storm3a_07_25_SPC, "STRT", "SPC", "0725a")
STRT_storm3b_08_05_SPC.p = hyst_plot(STRT_storm3b_08_05_Q, STRT_storm3b_08_05_SPC, "STRT", "SPC", "0805b")
STRT_storm3c_08_12_SPC.p = hyst_plot(STRT_storm3c_08_12_Q, STRT_storm3c_08_12_SPC, "STRT", "SPC", "0812c")
STRT_storm4_08_15_SPC.p = hyst_plot(STRT_storm4_08_15_Q, STRT_storm4_08_15_SPC, "STRT", "SPC", "0815")
STRT_storm5_08_20_SPC.p = hyst_plot(STRT_storm5_08_20_Q, STRT_storm5_08_20_SPC, "STRT", "SPC", "0820")
STRT_storm6_09_20_SPC.p = hyst_plot(STRT_storm6_09_20_Q, STRT_storm6_09_20_SPC, "STRT", "SPC", "0920")
STRT_storm7_10_01_SPC.p = hyst_plot(STRT_storm7_10_01_Q, STRT_storm7_10_01_SPC, "STRT", "SPC", "1001a")
STRT_storm7b_10_04_SPC.p = hyst_plot(STRT_storm7b_10_04_Q, STRT_storm7b_10_04_SPC, "STRT", "SPC", "1004b")
STRT_storm7c_10_09_SPC.p = hyst_plot(STRT_storm7c_10_09_Q, STRT_storm7c_10_09_SPC, "STRT", "SPC", "1009c")

# turb
STRT_storm1_05_31_turb.p = hyst_plot(STRT_storm1_05_31_Q, STRT_storm1_05_31_turb, "STRT", "turb", "0531")
STRT_storm2_07_12_turb.p = hyst_plot(STRT_storm2_07_12_Q, STRT_storm2_07_12_turb, "STRT", "turb", "0712")
STRT_storm3a_07_25_turb.p = hyst_plot(STRT_storm3a_07_25_Q, STRT_storm3a_07_25_turb, "STRT", "turb", "0725a")
STRT_storm3b_08_05_turb.p = hyst_plot(STRT_storm3b_08_05_Q, STRT_storm3b_08_05_turb, "STRT", "turb", "0805b")
STRT_storm3c_08_12_turb.p = hyst_plot(STRT_storm3c_08_12_Q, STRT_storm3c_08_12_turb, "STRT", "turb", "0812c")
STRT_storm4_08_15_turb.p = hyst_plot(STRT_storm4_08_15_Q, STRT_storm4_08_15_turb, "STRT", "turb", "0815")
STRT_storm5_08_20_turb.p = hyst_plot(STRT_storm5_08_20_Q, STRT_storm5_08_20_turb, "STRT", "turb", "0820")
STRT_storm6_09_20_turb.p = hyst_plot(STRT_storm6_09_20_Q, STRT_storm6_09_20_turb, "STRT", "turb", "0920")
STRT_storm7_10_01_turb.p = hyst_plot(STRT_storm7_10_01_Q, STRT_storm7_10_01_turb, "STRT", "turb", "1001a")
STRT_storm7b_10_04_turb.p = hyst_plot(STRT_storm7b_10_04_Q, STRT_storm7b_10_04_turb, "STRT", "turb", "1004b")
STRT_storm7c_10_09_turb.p = hyst_plot(STRT_storm7c_10_09_Q, STRT_storm7c_10_09_turb, "STRT", "turb", "1009c")

# abs
STRT_storm1_05_31_abs.p = hyst_plot(STRT_storm1_05_31_Q, STRT_storm1_05_31_abs, "STRT", "abs", "0531")
STRT_storm2_07_12_abs.p = hyst_plot(STRT_storm2_07_12_Q, STRT_storm2_07_12_abs, "STRT", "abs", "0712")
STRT_storm3a_07_25_abs.p = hyst_plot(STRT_storm3a_07_25_Q, STRT_storm3a_07_25_abs, "STRT", "abs", "0725a")
STRT_storm3b_08_05_abs.p = hyst_plot(STRT_storm3b_08_05_Q, STRT_storm3b_08_05_abs, "STRT", "abs", "0805b")
STRT_storm3c_08_12_abs.p = hyst_plot(STRT_storm3c_08_12_Q, STRT_storm3c_08_12_abs, "STRT", "abs", "0812c")
STRT_storm4_08_15_abs.p = hyst_plot(STRT_storm4_08_15_Q, STRT_storm4_08_15_abs, "STRT", "abs", "0815")
STRT_storm5_08_20_abs.p = hyst_plot(STRT_storm5_08_20_Q, STRT_storm5_08_20_abs, "STRT", "abs", "0820")
STRT_storm6_09_20_abs.p = hyst_plot(STRT_storm6_09_20_Q, STRT_storm6_09_20_abs, "STRT", "abs", "0920")
STRT_storm7_10_01_abs.p = hyst_plot(STRT_storm7_10_01_Q, STRT_storm7_10_01_abs, "STRT", "abs", "1001a")
STRT_storm7b_10_04_abs.p = hyst_plot(STRT_storm7b_10_04_Q, STRT_storm7b_10_04_abs, "STRT", "abs", "1004b")
STRT_storm7c_10_09_abs.p = hyst_plot(STRT_storm7c_10_09_Q, STRT_storm7c_10_09_abs, "STRT", "abs", "1009c")

# Multiplots of STRT storms #

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


multiplot(STRT_storm1_05_31_NO3.p, STRT_storm1_05_31_fDOM.p, STRT_storm1_05_31_SPC.p, STRT_storm1_05_31_turb.p,
          STRT_storm2_07_12_NO3.p, STRT_storm2_07_12_fDOM.p, STRT_storm2_07_12_SPC.p, STRT_storm2_07_12_turb.p,
          STRT_storm3a_07_25_NO3.p, STRT_storm3a_07_25_fDOM.p, STRT_storm3a_07_25_SPC.p, STRT_storm3a_07_25_turb.p,
          STRT_storm3b_08_05_NO3.p, STRT_storm3b_08_05_fDOM.p,  STRT_storm3b_08_05_SPC.p, STRT_storm3b_08_05_turb.p,
          STRT_storm3c_08_12_NO3.p, STRT_storm3c_08_12_fDOM.p, STRT_storm3c_08_12_SPC.p, STRT_storm3c_08_12_turb.p,
          STRT_storm4_08_15_NO3.p, STRT_storm4_08_15_fDOM.p, STRT_storm4_08_15_SPC.p, STRT_storm4_08_15_turb.p,
          STRT_storm5_08_20_NO3.p, STRT_storm5_08_20_fDOM.p, STRT_storm5_08_20_SPC.p, STRT_storm5_08_20_turb.p,
          STRT_storm6_09_20_NO3.p, STRT_storm6_09_20_fDOM.p, STRT_storm6_09_20_SPC.p, STRT_storm6_09_20_turb.p,
          STRT_storm7_10_01_NO3.p, STRT_storm7_10_01_fDOM.p, STRT_storm7_10_01_SPC.p, STRT_storm7_10_01_turb.p,
          STRT_storm7b_10_04_NO3.p, STRT_storm7b_10_04_fDOM.p, STRT_storm7b_10_04_SPC.p, STRT_storm7b_10_04_turb.p,
          STRT_storm7c_10_09_NO3.p, STRT_storm7c_10_09_fDOM.p, STRT_storm7c_10_09_SPC.p, STRT_storm7c_10_09_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("STRT_HI_Loops_2019.pdf",
       path = here("plots", "HI_plots", "2019", "STRT"),
       width = 20, height = 30, units = "in")

# Make MOOS loops #
# NO3
MOOS_storm1_06_01_NO3.p = hyst_plot(MOOS_storm1_06_01_Q, MOOS_storm1_06_01_NO3, "MOOS", "NO3", "0601")
MOOS_storm3_07_12_NO3.p = hyst_plot(MOOS_storm3_07_12_Q, MOOS_storm3_07_12_NO3, "MOOS", "NO3", "0712")
MOOS_storm4_07_25_NO3.p = hyst_plot(MOOS_storm4_07_25_Q, MOOS_storm4_07_25_NO3, "MOOS", "NO3", "0725")
MOOS_storm5_07_29_NO3.p = hyst_plot(MOOS_storm5_07_29_Q, MOOS_storm5_07_29_NO3, "MOOS", "NO3", "0729")
MOOS_storm6a_08_01_NO3.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_NO3, "MOOS", "NO3", "0801a")

MOOS_storm6d_08_05_NO3.p = hyst_plot(MOOS_storm6d_08_05_Q, MOOS_storm6d_08_05_NO3, "MOOS", "NO3", "0805d")
MOOS_storm7a_08_13_NO3.p = hyst_plot(MOOS_storm7a_08_13_Q, MOOS_storm7a_08_13_NO3, "MOOS", "NO3", "0813a")
MOOS_storm7b_08_14_NO3.p = hyst_plot(MOOS_storm7b_08_14_Q, MOOS_storm7b_08_14_NO3, "MOOS", "NO3", "0814b")
MOOS_storm7c_08_15_NO3.p = hyst_plot(MOOS_storm7c_08_15_Q, MOOS_storm7c_08_15_NO3, "MOOS", "NO3", "0815c")
MOOS_storm8_09_21_NO3.p = hyst_plot(MOOS_storm8_09_21_Q, MOOS_storm8_09_21_NO3, "MOOS", "NO3", "0921")
MOOS_storm9_10_02_NO3.p = hyst_plot(MOOS_storm9_10_02_Q, MOOS_storm9_10_02_NO3, "MOOS", "NO3", "1002")

# fDOM
MOOS_storm1_06_01_fDOM.p = hyst_plot(MOOS_storm1_06_01_Q, MOOS_storm1_06_01_fDOM, "MOOS", "fDOM", "0601")
MOOS_storm3_07_12_fDOM.p = hyst_plot(MOOS_storm3_07_12_Q, MOOS_storm3_07_12_fDOM, "MOOS", "fDOM", "0712")
MOOS_storm4_07_25_fDOM.p = hyst_plot(MOOS_storm4_07_25_Q, MOOS_storm4_07_25_fDOM, "MOOS", "fDOM", "0725")
MOOS_storm5_07_29_fDOM.p = hyst_plot(MOOS_storm5_07_29_Q, MOOS_storm5_07_29_fDOM, "MOOS", "fDOM", "0729")
MOOS_storm6a_08_01_fDOM.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_fDOM, "MOOS", "fDOM", "0801a")

MOOS_storm6d_08_05_fDOM.p = hyst_plot(MOOS_storm6d_08_05_Q, MOOS_storm6d_08_05_fDOM, "MOOS", "fDOM", "0805d")
MOOS_storm7a_08_13_fDOM.p = hyst_plot(MOOS_storm7a_08_13_Q, MOOS_storm7a_08_13_fDOM, "MOOS", "fDOM", "0813a")
MOOS_storm7b_08_14_fDOM.p = hyst_plot(MOOS_storm7b_08_14_Q, MOOS_storm7b_08_14_fDOM, "MOOS", "fDOM", "0814b")
MOOS_storm7c_08_15_fDOM.p = hyst_plot(MOOS_storm7c_08_15_Q, MOOS_storm7c_08_15_fDOM, "MOOS", "fDOM", "0815c")
MOOS_storm8_09_21_fDOM.p = hyst_plot(MOOS_storm8_09_21_Q, MOOS_storm8_09_21_fDOM, "MOOS", "fDOM", "0921")
MOOS_storm9_10_02_fDOM.p = hyst_plot(MOOS_storm9_10_02_Q, MOOS_storm9_10_02_fDOM, "MOOS", "fDOM", "1002")

# SPC
MOOS_storm1_06_01_SPC.p = hyst_plot(MOOS_storm1_06_01_Q, MOOS_storm1_06_01_SPC, "MOOS", "SPC", "0601")
MOOS_storm3_07_12_SPC.p = hyst_plot(MOOS_storm3_07_12_Q, MOOS_storm3_07_12_SPC, "MOOS", "SPC", "0712")
MOOS_storm4_07_25_SPC.p = hyst_plot(MOOS_storm4_07_25_Q, MOOS_storm4_07_25_SPC, "MOOS", "SPC", "0725")
MOOS_storm5_07_29_SPC.p = hyst_plot(MOOS_storm5_07_29_Q, MOOS_storm5_07_29_SPC, "MOOS", "SPC", "0729")
MOOS_storm6a_08_01_SPC.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_SPC, "MOOS", "SPC", "0801a")

MOOS_storm6d_08_05_SPC.p = hyst_plot(MOOS_storm6d_08_05_Q, MOOS_storm6d_08_05_SPC, "MOOS", "SPC", "0805d")
MOOS_storm7a_08_13_SPC.p = hyst_plot(MOOS_storm7a_08_13_Q, MOOS_storm7a_08_13_SPC, "MOOS", "SPC", "0813a")
MOOS_storm7b_08_14_SPC.p = hyst_plot(MOOS_storm7b_08_14_Q, MOOS_storm7b_08_14_SPC, "MOOS", "SPC", "0814b")
MOOS_storm7c_08_15_SPC.p = hyst_plot(MOOS_storm7c_08_15_Q, MOOS_storm7c_08_15_SPC, "MOOS", "SPC", "0815c")
MOOS_storm8_09_21_SPC.p = hyst_plot(MOOS_storm8_09_21_Q, MOOS_storm8_09_21_SPC, "MOOS", "SPC", "0921")
MOOS_storm9_10_02_SPC.p = hyst_plot(MOOS_storm9_10_02_Q, MOOS_storm9_10_02_SPC, "MOOS", "SPC", "1002")

# turb
which(MOOS_storm3_07_12_turb$datavalue > 0.75)
MOOS_storm3_07_12_turb[114,3] <- NA

MOOS_storm1_06_01_turb.p = hyst_plot(MOOS_storm1_06_01_Q, MOOS_storm1_06_01_turb, "MOOS", "turb", "0601")
MOOS_storm3_07_12_turb.p = hyst_plot(MOOS_storm3_07_12_Q, MOOS_storm3_07_12_turb, "MOOS", "turb", "0712")
MOOS_storm4_07_25_turb.p = hyst_plot(MOOS_storm4_07_25_Q, MOOS_storm4_07_25_turb, "MOOS", "turb", "0725")
MOOS_storm5_07_29_turb.p = hyst_plot(MOOS_storm5_07_29_Q, MOOS_storm5_07_29_turb, "MOOS", "turb", "0729")
MOOS_storm6a_08_01_turb.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_turb, "MOOS", "turb", "0801a")

MOOS_storm6d_08_05_turb.p = hyst_plot(MOOS_storm6d_08_05_Q, MOOS_storm6d_08_05_turb, "MOOS", "turb", "0805d")
MOOS_storm7a_08_13_turb.p = hyst_plot(MOOS_storm7a_08_13_Q, MOOS_storm7a_08_13_turb, "MOOS", "turb", "0813a")
MOOS_storm7b_08_14_turb.p = hyst_plot(MOOS_storm7b_08_14_Q, MOOS_storm7b_08_14_turb, "MOOS", "turb", "0814b")
MOOS_storm7c_08_15_turb.p = hyst_plot(MOOS_storm7c_08_15_Q, MOOS_storm7c_08_15_turb, "MOOS", "turb", "0815c")
MOOS_storm8_09_21_turb.p = hyst_plot(MOOS_storm8_09_21_Q, MOOS_storm8_09_21_turb, "MOOS", "turb", "0921")
MOOS_storm9_10_02_turb.p = hyst_plot(MOOS_storm9_10_02_Q, MOOS_storm9_10_02_turb, "MOOS", "turb", "1002")

# ABS
MOOS_storm1_06_01_abs.p = hyst_plot(MOOS_storm1_06_01_Q, MOOS_storm1_06_01_abs, "MOOS", "abs", "0601")
MOOS_storm3_07_12_abs.p = hyst_plot(MOOS_storm3_07_12_Q, MOOS_storm3_07_12_abs, "MOOS", "abs", "0712")
MOOS_storm4_07_25_abs.p = hyst_plot(MOOS_storm4_07_25_Q, MOOS_storm4_07_25_abs, "MOOS", "abs", "0725")
MOOS_storm5_07_29_abs.p = hyst_plot(MOOS_storm5_07_29_Q, MOOS_storm5_07_29_abs, "MOOS", "abs", "0729")
MOOS_storm6a_08_01_abs.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_abs, "MOOS", "abs", "0801a")

MOOS_storm6d_08_05_abs.p = hyst_plot(MOOS_storm6d_08_05_Q, MOOS_storm6d_08_05_abs, "MOOS", "abs", "0805d")
MOOS_storm7a_08_13_abs.p = hyst_plot(MOOS_storm7a_08_13_Q, MOOS_storm7a_08_13_abs, "MOOS", "abs", "0813a")
MOOS_storm7b_08_14_abs.p = hyst_plot(MOOS_storm7b_08_14_Q, MOOS_storm7b_08_14_abs, "MOOS", "abs", "0814b")
MOOS_storm7c_08_15_abs.p = hyst_plot(MOOS_storm7c_08_15_Q, MOOS_storm7c_08_15_abs, "MOOS", "abs", "0815c")
MOOS_storm8_09_21_abs.p = hyst_plot(MOOS_storm8_09_21_Q, MOOS_storm8_09_21_abs, "MOOS", "abs", "0921")
MOOS_storm9_10_02_abs.p = hyst_plot(MOOS_storm9_10_02_Q, MOOS_storm9_10_02_abs, "MOOS", "abs", "1002")


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

multiplot(MOOS_storm1_06_01_NO3.p, MOOS_storm1_06_01_fDOM.p, MOOS_storm1_06_01_SPC.p, MOOS_storm1_06_01_turb.p,
          MOOS_storm3_07_12_NO3.p, MOOS_storm3_07_12_fDOM.p,MOOS_storm3_07_12_SPC.p, MOOS_storm3_07_12_turb.p,
          MOOS_storm4_07_25_fDOM.p,MOOS_storm4_07_25_SPC.p, MOOS_storm4_07_25_turb.p,
          MOOS_storm5_07_29_NO3.p, MOOS_storm5_07_29_fDOM.p,MOOS_storm5_07_29_SPC.p, MOOS_storm5_07_29_turb.p,
          MOOS_storm6a_08_01_NO3.p, MOOS_storm6a_08_01_fDOM.p,MOOS_storm6a_08_01_SPC.p, MOOS_storm6a_08_01_turb.p,
          
          MOOS_storm6d_08_05_NO3.p, MOOS_storm6d_08_05_fDOM.p,MOOS_storm6d_08_05_SPC.p, MOOS_storm6d_08_05_turb.p,
          MOOS_storm7a_08_13_NO3.p, MOOS_storm7a_08_13_fDOM.p,MOOS_storm7a_08_13_SPC.p, MOOS_storm7a_08_13_turb.p,
          MOOS_storm7b_08_14_NO3.p, MOOS_storm7b_08_14_fDOM.p,MOOS_storm7b_08_14_SPC.p, MOOS_storm7b_08_14_turb.p,
          MOOS_storm7c_08_15_NO3.p, MOOS_storm7c_08_15_fDOM.p,MOOS_storm7c_08_15_SPC.p, MOOS_storm7c_08_15_turb.p,
          MOOS_storm8_09_21_NO3.p, MOOS_storm8_09_21_fDOM.p,MOOS_storm8_09_21_SPC.p, MOOS_storm8_09_21_turb.p,
          MOOS_storm9_10_02_NO3.p, MOOS_storm9_10_02_fDOM.p,MOOS_storm9_10_02_SPC.p, MOOS_storm9_10_02_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("MOOS_HI_Loops_2019.pdf",
       path = here("plots", "HI_plots", "2019", "MOOS"),
       width = 20, height = 30, units = "in")

# Make FRCH loops #
# NO3
# FRCH_storm1_05_31_NO3[c(92,104), 3] <- NA
FRCH_storm1_05_31_NO3.p = hyst_plot(FRCH_storm1_05_31_Q, FRCH_storm1_05_31_NO3, "FRCH", "NO3", "0531")
FRCH_storm2_06_15_NO3.p = hyst_plot(FRCH_storm2_06_15_Q, FRCH_storm2_06_15_NO3, "FRCH", "NO3", "0615")
FRCH_storm3_06_18_NO3.p = hyst_plot(FRCH_storm3_06_18_Q, FRCH_storm3_06_18_NO3, "FRCH", "NO3", "0618")
FRCH_storm4_06_20_NO3.p = hyst_plot(FRCH_storm4_06_20_Q, FRCH_storm4_06_20_NO3, "FRCH", "NO3", "0620")
FRCH_storm5_06_22_NO3.p = hyst_plot(FRCH_storm5_06_22_Q, FRCH_storm5_06_22_NO3, "FRCH", "NO3", "0622")
FRCH_storm6_07_12_NO3.p = hyst_plot(FRCH_storm6_07_12_Q, FRCH_storm6_07_12_NO3, "FRCH", "NO3", "0712")

FRCH_storm10a_08_01_NO3.p = hyst_plot(FRCH_storm10a_08_01_Q, FRCH_storm10a_08_01_NO3, "FRCH", "NO3", "0801a")

FRCH_storm11_08_05_NO3.p = hyst_plot(FRCH_storm11_08_05_Q, FRCH_storm11_08_05_NO3, "FRCH", "NO3", "0805")
FRCH_storm12a_08_12_NO3.p = hyst_plot(FRCH_storm12a_08_12_Q, FRCH_storm12a_08_12_NO3, "FRCH", "NO3", "0812a")

FRCH_storm12c_08_15_NO3.p = hyst_plot(FRCH_storm12c_08_15_Q, FRCH_storm12c_08_15_NO3, "FRCH", "NO3", "0815c")

FRCH_storm13_09_20_NO3.p = hyst_plot(FRCH_storm13_09_20_Q, FRCH_storm13_09_20_NO3, "FRCH", "NO3", "0920")
FRCH_storm14_10_01_NO3.p = hyst_plot(FRCH_storm14_10_01_Q, FRCH_storm14_10_01_NO3, "FRCH", "NO3", "1001")

# fDOM
FRCH_storm1_05_31_fDOM.p = hyst_plot(FRCH_storm1_05_31_Q, FRCH_storm1_05_31_fDOM, "FRCH", "fDOM", "0531")
FRCH_storm2_06_15_fDOM.p = hyst_plot(FRCH_storm2_06_15_Q, FRCH_storm2_06_15_fDOM, "FRCH", "fDOM", "0615")
FRCH_storm3_06_18_fDOM.p = hyst_plot(FRCH_storm3_06_18_Q, FRCH_storm3_06_18_fDOM, "FRCH", "fDOM", "0618")
FRCH_storm4_06_20_fDOM.p = hyst_plot(FRCH_storm4_06_20_Q, FRCH_storm4_06_20_fDOM, "FRCH", "fDOM", "0620")
FRCH_storm5_06_22_fDOM.p = hyst_plot(FRCH_storm5_06_22_Q, FRCH_storm5_06_22_fDOM, "FRCH", "fDOM", "0622")
FRCH_storm6_07_12_fDOM.p = hyst_plot(FRCH_storm6_07_12_Q, FRCH_storm6_07_12_fDOM, "FRCH", "fDOM", "0712")

FRCH_storm10a_08_01_fDOM.p = hyst_plot(FRCH_storm10a_08_01_Q, FRCH_storm10a_08_01_fDOM, "FRCH", "fDOM", "0801a")

FRCH_storm10c_08_03_fDOM.p = hyst_plot(FRCH_storm10c_08_03_Q, FRCH_storm10c_08_03_fDOM, "FRCH", "fDOM", "0803c")
FRCH_storm11_08_05_fDOM.p = hyst_plot(FRCH_storm11_08_05_Q, FRCH_storm11_08_05_fDOM, "FRCH", "fDOM", "0805")
FRCH_storm12a_08_12_fDOM.p = hyst_plot(FRCH_storm12a_08_12_Q, FRCH_storm12a_08_12_fDOM, "FRCH", "fDOM", "0812a")

FRCH_storm12c_08_15_fDOM.p = hyst_plot(FRCH_storm12c_08_15_Q, FRCH_storm12c_08_15_fDOM, "FRCH", "fDOM", "0815c")

FRCH_storm13_09_20_fDOM.p = hyst_plot(FRCH_storm13_09_20_Q, FRCH_storm13_09_20_fDOM, "FRCH", "fDOM", "0920")
FRCH_storm14_10_01_fDOM.p = hyst_plot(FRCH_storm14_10_01_Q, FRCH_storm14_10_01_fDOM, "FRCH", "fDOM", "1001")

# SPC
FRCH_storm1_05_31_SPC.p = hyst_plot(FRCH_storm1_05_31_Q, FRCH_storm1_05_31_SPC, "FRCH", "SPC", "0531")
FRCH_storm2_06_15_SPC.p = hyst_plot(FRCH_storm2_06_15_Q, FRCH_storm2_06_15_SPC, "FRCH", "SPC", "0615")
FRCH_storm3_06_18_SPC.p = hyst_plot(FRCH_storm3_06_18_Q, FRCH_storm3_06_18_SPC, "FRCH", "SPC", "0618")
FRCH_storm4_06_20_SPC.p = hyst_plot(FRCH_storm4_06_20_Q, FRCH_storm4_06_20_SPC, "FRCH", "SPC", "0620")
FRCH_storm5_06_22_SPC.p = hyst_plot(FRCH_storm5_06_22_Q, FRCH_storm5_06_22_SPC, "FRCH", "SPC", "0622")
FRCH_storm6_07_12_SPC.p = hyst_plot(FRCH_storm6_07_12_Q, FRCH_storm6_07_12_SPC, "FRCH", "SPC", "0712")

FRCH_storm10a_08_01_SPC.p = hyst_plot(FRCH_storm10a_08_01_Q, FRCH_storm10a_08_01_SPC, "FRCH", "SPC", "0801a")

FRCH_storm10c_08_03_SPC.p = hyst_plot(FRCH_storm10c_08_03_Q, FRCH_storm10c_08_03_SPC, "FRCH", "SPC", "0803c")
FRCH_storm11_08_05_SPC.p = hyst_plot(FRCH_storm11_08_05_Q, FRCH_storm11_08_05_SPC, "FRCH", "SPC", "0805")
FRCH_storm12a_08_12_SPC.p = hyst_plot(FRCH_storm12a_08_12_Q, FRCH_storm12a_08_12_SPC, "FRCH", "SPC", "0812a")

FRCH_storm12c_08_15_SPC.p = hyst_plot(FRCH_storm12c_08_15_Q, FRCH_storm12c_08_15_SPC, "FRCH", "SPC", "0815c")

FRCH_storm13_09_20_SPC.p = hyst_plot(FRCH_storm13_09_20_Q, FRCH_storm13_09_20_SPC, "FRCH", "SPC", "0920")
FRCH_storm14_10_01_SPC.p = hyst_plot(FRCH_storm14_10_01_Q, FRCH_storm14_10_01_SPC, "FRCH", "SPC", "1001")

# turb
FRCH_storm1_05_31_turb.p = hyst_plot(FRCH_storm1_05_31_Q, FRCH_storm1_05_31_turb, "FRCH", "turb", "0531")
FRCH_storm2_06_15_turb.p = hyst_plot(FRCH_storm2_06_15_Q, FRCH_storm2_06_15_turb, "FRCH", "turb", "0615")
FRCH_storm3_06_18_turb.p = hyst_plot(FRCH_storm3_06_18_Q, FRCH_storm3_06_18_turb, "FRCH", "turb", "0618")
FRCH_storm4_06_20_turb.p = hyst_plot(FRCH_storm4_06_20_Q, FRCH_storm4_06_20_turb, "FRCH", "turb", "0620")
FRCH_storm5_06_22_turb.p = hyst_plot(FRCH_storm5_06_22_Q, FRCH_storm5_06_22_turb, "FRCH", "turb", "0622")
FRCH_storm6_07_12_turb.p = hyst_plot(FRCH_storm6_07_12_Q, FRCH_storm6_07_12_turb, "FRCH", "turb", "0712")

FRCH_storm10a_08_01_turb.p = hyst_plot(FRCH_storm10a_08_01_Q, FRCH_storm10a_08_01_turb, "FRCH", "turb", "0801a")

FRCH_storm10c_08_03_turb.p = hyst_plot(FRCH_storm10c_08_03_Q, FRCH_storm10c_08_03_turb, "FRCH", "turb", "0803c")
FRCH_storm11_08_05_turb.p = hyst_plot(FRCH_storm11_08_05_Q, FRCH_storm11_08_05_turb, "FRCH", "turb", "0805")
FRCH_storm12a_08_12_turb.p = hyst_plot(FRCH_storm12a_08_12_Q, FRCH_storm12a_08_12_turb, "FRCH", "turb", "0812a")

FRCH_storm12c_08_15_turb.p = hyst_plot(FRCH_storm12c_08_15_Q, FRCH_storm12c_08_15_turb, "FRCH", "turb", "0815c")

FRCH_storm13_09_20_turb.p = hyst_plot(FRCH_storm13_09_20_Q, FRCH_storm13_09_20_turb, "FRCH", "turb", "0920")
FRCH_storm14_10_01_turb.p = hyst_plot(FRCH_storm14_10_01_Q, FRCH_storm14_10_01_turb, "FRCH", "turb", "1001")

# ABS
FRCH_storm1_05_31_abs.p = hyst_plot(FRCH_storm1_05_31_Q, FRCH_storm1_05_31_abs, "FRCH", "abs", "0531")
FRCH_storm2_06_15_abs.p = hyst_plot(FRCH_storm2_06_15_Q, FRCH_storm2_06_15_abs, "FRCH", "abs", "0615")
FRCH_storm3_06_18_abs.p = hyst_plot(FRCH_storm3_06_18_Q, FRCH_storm3_06_18_abs, "FRCH", "abs", "0618")
FRCH_storm4_06_20_abs.p = hyst_plot(FRCH_storm4_06_20_Q, FRCH_storm4_06_20_abs, "FRCH", "abs", "0620")
FRCH_storm5_06_22_abs.p = hyst_plot(FRCH_storm5_06_22_Q, FRCH_storm5_06_22_abs, "FRCH", "abs", "0622")
FRCH_storm6_07_12_abs.p = hyst_plot(FRCH_storm6_07_12_Q, FRCH_storm6_07_12_abs, "FRCH", "abs", "0712")

FRCH_storm10a_08_01_abs.p = hyst_plot(FRCH_storm10a_08_01_Q, FRCH_storm10a_08_01_abs, "FRCH", "abs", "0801a")

FRCH_storm10c_08_03_abs.p = hyst_plot(FRCH_storm10c_08_03_Q, FRCH_storm10c_08_03_abs, "FRCH", "abs", "0803c")
FRCH_storm11_08_05_abs.p = hyst_plot(FRCH_storm11_08_05_Q, FRCH_storm11_08_05_abs, "FRCH", "abs", "0805")
FRCH_storm12a_08_12_abs.p = hyst_plot(FRCH_storm12a_08_12_Q, FRCH_storm12a_08_12_abs, "FRCH", "abs", "0812a")

FRCH_storm12c_08_15_abs.p = hyst_plot(FRCH_storm12c_08_15_Q, FRCH_storm12c_08_15_abs, "FRCH", "abs", "0815c")

FRCH_storm13_09_20_abs.p = hyst_plot(FRCH_storm13_09_20_Q, FRCH_storm13_09_20_abs, "FRCH", "abs", "0920")
FRCH_storm14_10_01_abs.p = hyst_plot(FRCH_storm14_10_01_Q, FRCH_storm14_10_01_abs, "FRCH", "abs", "1001")

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


multiplot(FRCH_storm1_05_31_NO3.p, FRCH_storm1_05_31_fDOM.p,FRCH_storm1_05_31_SPC.p, FRCH_storm1_05_31_turb.p,
          FRCH_storm2_06_15_NO3.p, FRCH_storm2_06_15_fDOM.p,FRCH_storm2_06_15_SPC.p, FRCH_storm2_06_15_turb.p,
          FRCH_storm3_06_18_NO3.p, FRCH_storm3_06_18_fDOM.p,FRCH_storm3_06_18_SPC.p, FRCH_storm3_06_18_turb.p,
          FRCH_storm5_06_22_NO3.p, FRCH_storm5_06_22_fDOM.p,FRCH_storm5_06_22_SPC.p, FRCH_storm5_06_22_turb.p,
          FRCH_storm6_07_12_NO3.p, FRCH_storm6_07_12_fDOM.p,FRCH_storm6_07_12_SPC.p, FRCH_storm6_07_12_turb.p,
          
          FRCH_storm10a_08_01_NO3.p, FRCH_storm10a_08_01_fDOM.p,FRCH_storm10a_08_01_SPC.p, FRCH_storm10a_08_01_turb.p,
          
        
          FRCH_storm10c_08_03_fDOM.p,FRCH_storm10c_08_03_SPC.p, FRCH_storm10c_08_03_turb.p,
          FRCH_storm11_08_05_NO3.p, FRCH_storm11_08_05_fDOM.p,FRCH_storm11_08_05_SPC.p, FRCH_storm11_08_05_turb.p,
          FRCH_storm12a_08_12_NO3.p, FRCH_storm12a_08_12_fDOM.p,FRCH_storm12a_08_12_SPC.p, FRCH_storm12a_08_12_turb.p,
          
          FRCH_storm12c_08_15_NO3.p, FRCH_storm12c_08_15_fDOM.p,FRCH_storm12c_08_15_SPC.p, FRCH_storm12c_08_15_turb.p,
          
          FRCH_storm13_09_20_NO3.p, FRCH_storm13_09_20_fDOM.p,FRCH_storm13_09_20_SPC.p, FRCH_storm13_09_20_turb.p,
          FRCH_storm14_10_01_NO3.p, FRCH_storm14_10_01_fDOM.p,FRCH_storm14_10_01_SPC.p, FRCH_storm14_10_01_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2019.pdf",
       path = here("plots", "HI_plots", "2019", "FRCH"),
       width = 20, height = 30, units = "in")

# Make VAUL loops #
# NO3

# VAUL_storm5_08_12_NO3[c(94:95,103:105,108,111), 3] <- NA
# which(VAUL_storm6_08_15_NO3$datavalue > 0.5)
# VAUL_storm6_08_15_NO3[c(51,91,92,96,112,114,117,121,166), 3] <- NA

VAUL_storm1_07_13_NO3.p = hyst_plot(VAUL_storm1_07_13_Q, VAUL_storm1_07_13_NO3, "VAUL", "NO3", "0713")
VAUL_storm2_07_26_NO3.p = hyst_plot(VAUL_storm2_07_26_Q, VAUL_storm2_07_26_NO3, "VAUL", "NO3", "0726")
VAUL_storm3_07_29_NO3.p = hyst_plot(VAUL_storm3_07_29_Q, VAUL_storm3_07_29_NO3, "VAUL", "NO3", "0729")
VAUL_storm4a_08_02_NO3.p = hyst_plot(VAUL_storm4a_08_02_Q, VAUL_storm4a_08_02_NO3, "VAUL", "NO3", "0802a")

VAUL_storm4c_08_05_NO3.p = hyst_plot(VAUL_storm4c_08_05_Q, VAUL_storm4c_08_05_NO3, "VAUL", "NO3", "0805c")
VAUL_storm5_08_12_NO3.p = hyst_plot(VAUL_storm5_08_12_Q, VAUL_storm5_08_12_NO3, "VAUL", "NO3", "0812")
VAUL_storm6_08_15_NO3.p = hyst_plot(VAUL_storm6_08_15_Q, VAUL_storm6_08_15_NO3, "VAUL", "NO3", "0815")
VAUL_storm7_09_19_NO3.p = hyst_plot(VAUL_storm7_09_19_Q, VAUL_storm7_09_19_NO3, "VAUL", "NO3", "0919")
VAUL_storm8a_09_29_NO3.p = hyst_plot(VAUL_storm8a_09_29_Q, VAUL_storm8a_09_29_NO3, "VAUL", "NO3", "0929a")

VAUL_storm8c_10_04_NO3.p = hyst_plot(VAUL_storm8c_10_04_Q, VAUL_storm8c_10_04_NO3, "VAUL", "NO3", "1004c")

# fDOM
# which(VAUL_storm7_09_19_fDOM$datavalue< 0.25)
# VAUL_storm7_09_19_fDOM[9, 3] <- NA
# which(VAUL_storm8b_10_01_fDOM$datavalue < 0.25)
# VAUL_storm8b_10_01_fDOM[21, 3] <- NA

VAUL_storm1_07_13_fDOM.p = hyst_plot(VAUL_storm1_07_13_Q, VAUL_storm1_07_13_fDOM, "VAUL", "fDOM", "0713")
VAUL_storm2_07_26_fDOM.p = hyst_plot(VAUL_storm2_07_26_Q, VAUL_storm2_07_26_fDOM, "VAUL", "fDOM", "0726")
VAUL_storm3_07_29_fDOM.p = hyst_plot(VAUL_storm3_07_29_Q, VAUL_storm3_07_29_fDOM, "VAUL", "fDOM", "0729")
VAUL_storm4a_08_02_fDOM.p = hyst_plot(VAUL_storm4a_08_02_Q, VAUL_storm4a_08_02_fDOM, "VAUL", "fDOM", "0802a")

VAUL_storm4c_08_05_fDOM.p = hyst_plot(VAUL_storm4c_08_05_Q, VAUL_storm4c_08_05_fDOM, "VAUL", "fDOM", "0805c")
VAUL_storm5_08_12_fDOM.p = hyst_plot(VAUL_storm5_08_12_Q, VAUL_storm5_08_12_fDOM, "VAUL", "fDOM", "0812")
VAUL_storm6_08_15_fDOM.p = hyst_plot(VAUL_storm6_08_15_Q, VAUL_storm6_08_15_fDOM, "VAUL", "fDOM", "0815")
VAUL_storm7_09_19_fDOM.p = hyst_plot(VAUL_storm7_09_19_Q, VAUL_storm7_09_19_fDOM, "VAUL", "fDOM", "0919")
VAUL_storm8a_09_29_fDOM.p = hyst_plot(VAUL_storm8a_09_29_Q, VAUL_storm8a_09_29_fDOM, "VAUL", "fDOM", "0929a")

VAUL_storm8c_10_04_fDOM.p = hyst_plot(VAUL_storm8c_10_04_Q, VAUL_storm8c_10_04_fDOM, "VAUL", "fDOM", "1004c")

# SPC
# 
# which(VAUL_storm7_09_19_SPC$datavalue <  0.25)
# VAUL_storm7_09_19_SPC[9, 3] <- NA



VAUL_storm1_07_13_SPC.p = hyst_plot(VAUL_storm1_07_13_Q, VAUL_storm1_07_13_SPC, "VAUL", "SPC", "0713")
VAUL_storm2_07_26_SPC.p = hyst_plot(VAUL_storm2_07_26_Q, VAUL_storm2_07_26_SPC, "VAUL", "SPC", "0726")
VAUL_storm3_07_29_SPC.p = hyst_plot(VAUL_storm3_07_29_Q, VAUL_storm3_07_29_SPC, "VAUL", "SPC", "0729")
VAUL_storm4a_08_02_SPC.p = hyst_plot(VAUL_storm4a_08_02_Q, VAUL_storm4a_08_02_SPC, "VAUL", "SPC", "0802a")

VAUL_storm4c_08_05_SPC.p = hyst_plot(VAUL_storm4c_08_05_Q, VAUL_storm4c_08_05_SPC, "VAUL", "SPC", "0805c")
VAUL_storm5_08_12_SPC.p = hyst_plot(VAUL_storm5_08_12_Q, VAUL_storm5_08_12_SPC, "VAUL", "SPC", "0812")
VAUL_storm6_08_15_SPC.p = hyst_plot(VAUL_storm6_08_15_Q, VAUL_storm6_08_15_SPC, "VAUL", "SPC", "0815")
VAUL_storm7_09_19_SPC.p = hyst_plot(VAUL_storm7_09_19_Q, VAUL_storm7_09_19_SPC, "VAUL", "SPC", "0919")
VAUL_storm8a_09_29_SPC.p = hyst_plot(VAUL_storm8a_09_29_Q, VAUL_storm8a_09_29_SPC, "VAUL", "SPC", "0929a")

VAUL_storm8c_10_04_SPC.p = hyst_plot(VAUL_storm8c_10_04_Q, VAUL_storm8c_10_04_SPC, "VAUL", "SPC", "1004c")

# turb 
# which(VAUL_storm5_08_12_turb$datavalue > 0.5)
# VAUL_storm5_08_12_turb[82, 3] <- NA
# VAUL_storm7_09_19_turb[9, 3] <- NA

VAUL_storm1_07_13_turb.p = hyst_plot(VAUL_storm1_07_13_Q, VAUL_storm1_07_13_turb, "VAUL", "turb", "0713")
VAUL_storm2_07_26_turb.p = hyst_plot(VAUL_storm2_07_26_Q, VAUL_storm2_07_26_turb, "VAUL", "turb", "0726")
VAUL_storm3_07_29_turb.p = hyst_plot(VAUL_storm3_07_29_Q, VAUL_storm3_07_29_turb, "VAUL", "turb", "0729")
VAUL_storm4a_08_02_turb.p = hyst_plot(VAUL_storm4a_08_02_Q, VAUL_storm4a_08_02_turb, "VAUL", "turb", "0802a")

VAUL_storm4c_08_05_turb.p = hyst_plot(VAUL_storm4c_08_05_Q, VAUL_storm4c_08_05_turb, "VAUL", "turb", "0805c")
VAUL_storm5_08_12_turb.p = hyst_plot(VAUL_storm5_08_12_Q, VAUL_storm5_08_12_turb, "VAUL", "turb", "0812")
VAUL_storm6_08_15_turb.p = hyst_plot(VAUL_storm6_08_15_Q, VAUL_storm6_08_15_turb, "VAUL", "turb", "0815")
VAUL_storm7_09_19_turb.p = hyst_plot(VAUL_storm7_09_19_Q, VAUL_storm7_09_19_turb, "VAUL", "turb", "0919")
VAUL_storm8a_09_29_turb.p = hyst_plot(VAUL_storm8a_09_29_Q, VAUL_storm8a_09_29_turb, "VAUL", "turb", "0929a")

VAUL_storm8c_10_04_turb.p = hyst_plot(VAUL_storm8c_10_04_Q, VAUL_storm8c_10_04_turb, "VAUL", "turb", "1004c")

# ABS

VAUL_storm1_07_13_abs.p = hyst_plot(VAUL_storm1_07_13_Q, VAUL_storm1_07_13_abs, "VAUL", "abs", "0713")
VAUL_storm2_07_26_abs.p = hyst_plot(VAUL_storm2_07_26_Q, VAUL_storm2_07_26_abs, "VAUL", "abs", "0726")
VAUL_storm3_07_29_abs.p = hyst_plot(VAUL_storm3_07_29_Q, VAUL_storm3_07_29_abs, "VAUL", "abs", "0729")
VAUL_storm4a_08_02_abs.p = hyst_plot(VAUL_storm4a_08_02_Q, VAUL_storm4a_08_02_abs, "VAUL", "abs", "0802a")

VAUL_storm4c_08_05_abs.p = hyst_plot(VAUL_storm4c_08_05_Q, VAUL_storm4c_08_05_abs, "VAUL", "abs", "0805c")
VAUL_storm5_08_12_abs.p = hyst_plot(VAUL_storm5_08_12_Q, VAUL_storm5_08_12_abs, "VAUL", "abs", "0812")
VAUL_storm6_08_15_abs.p = hyst_plot(VAUL_storm6_08_15_Q, VAUL_storm6_08_15_abs, "VAUL", "abs", "0815")
VAUL_storm7_09_19_abs.p = hyst_plot(VAUL_storm7_09_19_Q, VAUL_storm7_09_19_abs, "VAUL", "abs", "0919")
VAUL_storm8a_09_29_abs.p = hyst_plot(VAUL_storm8a_09_29_Q, VAUL_storm8a_09_29_abs, "VAUL", "abs", "0929a")

VAUL_storm8c_10_04_abs.p = hyst_plot(VAUL_storm8c_10_04_Q, VAUL_storm8c_10_04_abs, "VAUL", "abs", "1004c")

# Multiplots of VAUL storms #

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

multiplot(VAUL_storm1_07_13_NO3.p, VAUL_storm1_07_13_fDOM.p,VAUL_storm1_07_13_SPC.p, VAUL_storm1_07_13_turb.p,
          VAUL_storm2_07_26_NO3.p, VAUL_storm2_07_26_fDOM.p,VAUL_storm2_07_26_SPC.p, VAUL_storm2_07_26_turb.p,
          VAUL_storm3_07_29_NO3.p, VAUL_storm3_07_29_fDOM.p,VAUL_storm3_07_29_SPC.p, VAUL_storm3_07_29_turb.p,
          VAUL_storm4a_08_02_NO3.p, VAUL_storm4a_08_02_fDOM.p,VAUL_storm4a_08_02_SPC.p, VAUL_storm4a_08_02_turb.p,
          
          VAUL_storm4c_08_05_NO3.p, VAUL_storm4c_08_05_fDOM.p,VAUL_storm4c_08_05_SPC.p, VAUL_storm4c_08_05_turb.p,
          VAUL_storm5_08_12_NO3.p, VAUL_storm5_08_12_fDOM.p,VAUL_storm5_08_12_SPC.p, VAUL_storm5_08_12_turb.p,
          VAUL_storm6_08_15_NO3.p, VAUL_storm6_08_15_fDOM.p,VAUL_storm6_08_15_SPC.p, VAUL_storm6_08_15_turb.p,
          VAUL_storm7_09_19_NO3.p, VAUL_storm7_09_19_fDOM.p,VAUL_storm7_09_19_SPC.p, VAUL_storm7_09_19_turb.p,
          VAUL_storm8a_09_29_NO3.p, VAUL_storm8a_09_29_fDOM.p,VAUL_storm8a_09_29_SPC.p, VAUL_storm8a_09_29_turb.p,
          
          VAUL_storm8c_10_04_NO3.p, VAUL_storm8c_10_04_fDOM.p,VAUL_storm8c_10_04_SPC.p, VAUL_storm8c_10_04_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("VAUL_HI_Loops_2019.pdf",
       path = here("plots", "HI_plots", "2019", "VAUL"),
       width = 20, height = 30, units = "in")

# Make POKE loops #
# NO3
POKE_storm1_06_30_NO3.p = hyst_plot(POKE_storm1_06_30_Q, POKE_storm1_06_30_NO3, "POKE", "NO3", "0630")
POKE_storm2_07_12_NO3.p = hyst_plot(POKE_storm2_07_12_Q, POKE_storm2_07_12_NO3, "POKE", "NO3", "0712")
POKE_storm3_07_26_NO3.p = hyst_plot(POKE_storm3_07_26_Q, POKE_storm3_07_26_NO3, "POKE", "NO3", "0726")
POKE_storm4_07_31_NO3.p = hyst_plot(POKE_storm4_07_31_Q, POKE_storm4_07_31_NO3, "POKE", "NO3", "0731")
POKE_storm5a_08_02_NO3.p = hyst_plot(POKE_storm5a_08_02_Q, POKE_storm5a_08_02_NO3, "POKE", "NO3", "0802a")

POKE_storm5c_08_05_NO3.p = hyst_plot(POKE_storm5c_08_05_Q, POKE_storm5c_08_05_NO3, "POKE", "NO3", "0805c")
POKE_storm5d_08_10_NO3.p = hyst_plot(POKE_storm5d_08_10_Q, POKE_storm5d_08_10_NO3, "POKE", "NO3", "0810d")
POKE_storm6a_08_12_NO3.p = hyst_plot(POKE_storm6a_08_12_Q, POKE_storm6a_08_12_NO3, "POKE", "NO3", "0812a")

POKE_storm7_08_15_NO3.p = hyst_plot(POKE_storm7_08_15_Q, POKE_storm7_08_15_NO3, "POKE", "NO3", "0815")
POKE_storm8_09_29_NO3.p = hyst_plot(POKE_storm8_09_29_Q, POKE_storm8_09_29_NO3, "POKE", "NO3", "0929")
POKE_storm9_10_04_NO3.p = hyst_plot(POKE_storm9_10_04_Q, POKE_storm9_10_04_NO3, "POKE", "NO3", "1004")

#fDOM
POKE_storm1_06_30_fDOM.p = hyst_plot(POKE_storm1_06_30_Q, POKE_storm1_06_30_fDOM, "POKE", "fDOM", "0630")
POKE_storm2_07_12_fDOM.p = hyst_plot(POKE_storm2_07_12_Q, POKE_storm2_07_12_fDOM, "POKE", "fDOM", "0712")
POKE_storm3_07_26_fDOM.p = hyst_plot(POKE_storm3_07_26_Q, POKE_storm3_07_26_fDOM, "POKE", "fDOM", "0726")
POKE_storm4_07_31_fDOM.p = hyst_plot(POKE_storm4_07_31_Q, POKE_storm4_07_31_fDOM, "POKE", "fDOM", "0731")
POKE_storm5a_08_02_fDOM.p = hyst_plot(POKE_storm5a_08_02_Q, POKE_storm5a_08_02_fDOM, "POKE", "fDOM", "0802a")

POKE_storm5c_08_05_fDOM.p = hyst_plot(POKE_storm5c_08_05_Q, POKE_storm5c_08_05_fDOM, "POKE", "fDOM", "0805c")
POKE_storm5d_08_10_fDOM.p = hyst_plot(POKE_storm5d_08_10_Q, POKE_storm5d_08_10_fDOM, "POKE", "fDOM", "0810d")
POKE_storm6a_08_12_fDOM.p = hyst_plot(POKE_storm6a_08_12_Q, POKE_storm6a_08_12_fDOM, "POKE", "fDOM", "0812a")

POKE_storm7_08_15_fDOM.p = hyst_plot(POKE_storm7_08_15_Q, POKE_storm7_08_15_fDOM, "POKE", "fDOM", "0815")
POKE_storm8_09_29_fDOM.p = hyst_plot(POKE_storm8_09_29_Q, POKE_storm8_09_29_fDOM, "POKE", "fDOM", "0929")
POKE_storm9_10_04_fDOM.p = hyst_plot(POKE_storm9_10_04_Q, POKE_storm9_10_04_fDOM, "POKE", "fDOM", "1004")

#SPC
POKE_storm1_06_30_SPC.p = hyst_plot(POKE_storm1_06_30_Q, POKE_storm1_06_30_SPC, "POKE", "SPC", "0630")
POKE_storm2_07_12_SPC.p = hyst_plot(POKE_storm2_07_12_Q, POKE_storm2_07_12_SPC, "POKE", "SPC", "0712")
POKE_storm3_07_26_SPC.p = hyst_plot(POKE_storm3_07_26_Q, POKE_storm3_07_26_SPC, "POKE", "SPC", "0726")
POKE_storm4_07_31_SPC.p = hyst_plot(POKE_storm4_07_31_Q, POKE_storm4_07_31_SPC, "POKE", "SPC", "0731")
POKE_storm5a_08_02_SPC.p = hyst_plot(POKE_storm5a_08_02_Q, POKE_storm5a_08_02_SPC, "POKE", "SPC", "0802a")

POKE_storm5c_08_05_SPC.p = hyst_plot(POKE_storm5c_08_05_Q, POKE_storm5c_08_05_SPC, "POKE", "SPC", "0805c")
POKE_storm5d_08_10_SPC.p = hyst_plot(POKE_storm5d_08_10_Q, POKE_storm5d_08_10_SPC, "POKE", "SPC", "0810d")
POKE_storm6a_08_12_SPC.p = hyst_plot(POKE_storm6a_08_12_Q, POKE_storm6a_08_12_SPC, "POKE", "SPC", "0812a")

POKE_storm7_08_15_SPC.p = hyst_plot(POKE_storm7_08_15_Q, POKE_storm7_08_15_SPC, "POKE", "SPC", "0815")
POKE_storm8_09_29_SPC.p = hyst_plot(POKE_storm8_09_29_Q, POKE_storm8_09_29_SPC, "POKE", "SPC", "0929")
POKE_storm9_10_04_SPC.p = hyst_plot(POKE_storm9_10_04_Q, POKE_storm9_10_04_SPC, "POKE", "SPC", "1004")

#turb
POKE_storm1_06_30_turb.p = hyst_plot(POKE_storm1_06_30_Q, POKE_storm1_06_30_turb, "POKE", "turb", "0630")
POKE_storm2_07_12_turb.p = hyst_plot(POKE_storm2_07_12_Q, POKE_storm2_07_12_turb, "POKE", "turb", "0712")
POKE_storm3_07_26_turb.p = hyst_plot(POKE_storm3_07_26_Q, POKE_storm3_07_26_turb, "POKE", "turb", "0726")
POKE_storm4_07_31_turb.p = hyst_plot(POKE_storm4_07_31_Q, POKE_storm4_07_31_turb, "POKE", "turb", "0731")
POKE_storm5a_08_02_turb.p = hyst_plot(POKE_storm5a_08_02_Q, POKE_storm5a_08_02_turb, "POKE", "turb", "0802a")

POKE_storm5c_08_05_turb.p = hyst_plot(POKE_storm5c_08_05_Q, POKE_storm5c_08_05_turb, "POKE", "turb", "0805c")
POKE_storm5d_08_10_turb.p = hyst_plot(POKE_storm5d_08_10_Q, POKE_storm5d_08_10_turb, "POKE", "turb", "0810d")
POKE_storm6a_08_12_turb.p = hyst_plot(POKE_storm6a_08_12_Q, POKE_storm6a_08_12_turb, "POKE", "turb", "0812a")

POKE_storm7_08_15_turb.p = hyst_plot(POKE_storm7_08_15_Q, POKE_storm7_08_15_turb, "POKE", "turb", "0815")
POKE_storm8_09_29_turb.p = hyst_plot(POKE_storm8_09_29_Q, POKE_storm8_09_29_turb, "POKE", "turb", "0929")
POKE_storm9_10_04_turb.p = hyst_plot(POKE_storm9_10_04_Q, POKE_storm9_10_04_turb, "POKE", "turb", "1004")

#ABS
POKE_storm1_06_30_abs.p = hyst_plot(POKE_storm1_06_30_Q, POKE_storm1_06_30_abs, "POKE", "abs", "0630")
POKE_storm2_07_12_abs.p = hyst_plot(POKE_storm2_07_12_Q, POKE_storm2_07_12_abs, "POKE", "abs", "0712")
POKE_storm3_07_26_abs.p = hyst_plot(POKE_storm3_07_26_Q, POKE_storm3_07_26_abs, "POKE", "abs", "0726")
POKE_storm4_07_31_abs.p = hyst_plot(POKE_storm4_07_31_Q, POKE_storm4_07_31_abs, "POKE", "abs", "0731")
POKE_storm5a_08_02_abs.p = hyst_plot(POKE_storm5a_08_02_Q, POKE_storm5a_08_02_abs, "POKE", "abs", "0802a")

POKE_storm5c_08_05_abs.p = hyst_plot(POKE_storm5c_08_05_Q, POKE_storm5c_08_05_abs, "POKE", "abs", "0805c")
POKE_storm5d_08_10_abs.p = hyst_plot(POKE_storm5d_08_10_Q, POKE_storm5d_08_10_abs, "POKE", "abs", "0810d")
POKE_storm6a_08_12_abs.p = hyst_plot(POKE_storm6a_08_12_Q, POKE_storm6a_08_12_abs, "POKE", "abs", "0812a")

POKE_storm7_08_15_abs.p = hyst_plot(POKE_storm7_08_15_Q, POKE_storm7_08_15_abs, "POKE", "abs", "0815")
POKE_storm8_09_29_abs.p = hyst_plot(POKE_storm8_09_29_Q, POKE_storm8_09_29_abs, "POKE", "abs", "0929")
POKE_storm9_10_04_abs.p = hyst_plot(POKE_storm9_10_04_Q, POKE_storm9_10_04_abs, "POKE", "abs", "1004")


# Multiplots of POKE storms #

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

multiplot(POKE_storm1_06_30_NO3.p, POKE_storm1_06_30_fDOM.p,POKE_storm1_06_30_SPC.p, POKE_storm1_06_30_turb.p,
          POKE_storm2_07_12_NO3.p, POKE_storm2_07_12_fDOM.p,POKE_storm2_07_12_SPC.p, POKE_storm2_07_12_turb.p,
          POKE_storm3_07_26_NO3.p, POKE_storm3_07_26_fDOM.p,POKE_storm3_07_26_SPC.p, POKE_storm3_07_26_turb.p,
          POKE_storm4_07_31_NO3.p, POKE_storm4_07_31_fDOM.p,POKE_storm4_07_31_SPC.p, POKE_storm4_07_31_turb.p,
          POKE_storm5a_08_02_NO3.p, POKE_storm5a_08_02_fDOM.p,POKE_storm5a_08_02_SPC.p, POKE_storm5a_08_02_turb.p,
          
          POKE_storm5c_08_05_NO3.p, POKE_storm5c_08_05_fDOM.p,POKE_storm5c_08_05_SPC.p, POKE_storm5c_08_05_turb.p,
          POKE_storm5d_08_10_NO3.p, POKE_storm5d_08_10_fDOM.p,POKE_storm5d_08_10_SPC.p, POKE_storm5d_08_10_turb.p,
          POKE_storm6a_08_12_NO3.p, POKE_storm6a_08_12_fDOM.p,POKE_storm6a_08_12_SPC.p, POKE_storm6a_08_12_turb.p,
          
          POKE_storm7_08_15_NO3.p, POKE_storm7_08_15_fDOM.p,POKE_storm7_08_15_SPC.p, POKE_storm7_08_15_turb.p,
          POKE_storm8_09_29_NO3.p, POKE_storm8_09_29_fDOM.p,POKE_storm8_09_29_SPC.p, POKE_storm8_09_29_turb.p,
          POKE_storm9_10_04_NO3.p, POKE_storm9_10_04_fDOM.p,POKE_storm9_10_04_SPC.p, POKE_storm9_10_04_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("POKE_HI_Loops_2019.pdf",
       path = here("plots", "HI_plots", "2019", "POKE"),
       width = 20, height = 30, units = "in")


# Multiplots of CARI storms #

# Make CARI loops #
# NO3
# which(CARI_storm7b_08_13_NO3$datavalue > 0.5)
# CARI_storm7b_08_13_NO3[c(525,3824), 3] <- NA

CARI_storm1_05_08_NO3.p = hyst_plot(CARI_storm1_05_08_Q, CARI_storm1_05_08_NO3, "CARI", "NO3", "0508")
CARI_storm2_06_30_NO3.p = hyst_plot(CARI_storm2_06_30_Q, CARI_storm2_06_30_NO3, "CARI", "NO3", "0630")
CARI_storm3_07_12_NO3.p = hyst_plot(CARI_storm3_07_12_Q, CARI_storm3_07_12_NO3, "CARI", "NO3", "0712")
#CARI_storm4_07_26_NO3.p = hyst_plot(CARI_storm4_07_26_Q, CARI_storm4_07_26_NO3, "CARI", "NO3", "0726")
CARI_storm5_07_31_NO3.p = hyst_plot(CARI_storm5_07_31_Q, CARI_storm5_07_31_NO3, "CARI", "NO3", "0731")
CARI_storm6a_08_02_NO3.p = hyst_plot(CARI_storm6a_08_02_Q, CARI_storm6a_08_02_NO3, "CARI", "NO3", "0802a")

CARI_storm6c_08_05_NO3.p = hyst_plot(CARI_storm6c_08_05_Q, CARI_storm6c_08_05_NO3, "CARI", "NO3", "0805c")
CARI_storm6d_08_10_NO3.p = hyst_plot(CARI_storm6d_08_10_Q, CARI_storm6d_08_10_NO3, "CARI", "NO3", "0810d")
CARI_storm7a_08_13_NO3.p = hyst_plot(CARI_storm7a_08_13_Q, CARI_storm7a_08_13_NO3, "CARI", "NO3", "0813a")

CARI_storm8_08_16_NO3.p = hyst_plot(CARI_storm8_08_16_Q, CARI_storm8_08_16_NO3, "CARI", "NO3", "0816")


CARI_storm1_05_08_fDOM.p = hyst_plot(CARI_storm1_05_08_Q, CARI_storm1_05_08_fDOM, "CARI", "fDOM", "0508")
CARI_storm2_06_30_fDOM.p = hyst_plot(CARI_storm2_06_30_Q, CARI_storm2_06_30_fDOM, "CARI", "fDOM", "0630")
CARI_storm3_07_12_fDOM.p = hyst_plot(CARI_storm3_07_12_Q, CARI_storm3_07_12_fDOM, "CARI", "fDOM", "0712")
#CARI_storm4_07_26_fDOM.p = hyst_plot(CARI_storm4_07_26_Q, CARI_storm4_07_26_fDOM, "CARI", "fDOM", "0726")
CARI_storm5_07_31_fDOM.p = hyst_plot(CARI_storm5_07_31_Q, CARI_storm5_07_31_fDOM, "CARI", "fDOM", "0731")
CARI_storm6a_08_02_fDOM.p = hyst_plot(CARI_storm6a_08_02_Q, CARI_storm6a_08_02_fDOM, "CARI", "fDOM", "0802a")

CARI_storm6c_08_05_fDOM.p = hyst_plot(CARI_storm6c_08_05_Q, CARI_storm6c_08_05_fDOM, "CARI", "fDOM", "0805c")
CARI_storm6d_08_10_fDOM.p = hyst_plot(CARI_storm6d_08_10_Q, CARI_storm6d_08_10_fDOM, "CARI", "fDOM", "0810d")
CARI_storm7a_08_13_fDOM.p = hyst_plot(CARI_storm7a_08_13_Q, CARI_storm7a_08_13_fDOM, "CARI", "fDOM", "0813a")

CARI_storm8_08_16_fDOM.p = hyst_plot(CARI_storm8_08_16_Q, CARI_storm8_08_16_fDOM, "CARI", "fDOM", "0816")

CARI_storm1_05_08_SPC.p = hyst_plot(CARI_storm1_05_08_Q, CARI_storm1_05_08_SPC, "CARI", "SPC", "0508")
CARI_storm2_06_30_SPC.p = hyst_plot(CARI_storm2_06_30_Q, CARI_storm2_06_30_SPC, "CARI", "SPC", "0630")
CARI_storm3_07_12_SPC.p = hyst_plot(CARI_storm3_07_12_Q, CARI_storm3_07_12_SPC, "CARI", "SPC", "0712")
#CARI_storm4_07_26_SPC.p = hyst_plot(CARI_storm4_07_26_Q, CARI_storm4_07_26_SPC, "CARI", "SPC", "0726")
CARI_storm5_07_31_SPC.p = hyst_plot(CARI_storm5_07_31_Q, CARI_storm5_07_31_SPC, "CARI", "SPC", "0731")
CARI_storm6a_08_02_SPC.p = hyst_plot(CARI_storm6a_08_02_Q, CARI_storm6a_08_02_SPC, "CARI", "SPC", "0802a")

CARI_storm6c_08_05_SPC.p = hyst_plot(CARI_storm6c_08_05_Q, CARI_storm6c_08_05_SPC, "CARI", "SPC", "0805c")
CARI_storm6d_08_10_SPC.p = hyst_plot(CARI_storm6d_08_10_Q, CARI_storm6d_08_10_SPC, "CARI", "SPC", "0810d")
CARI_storm7a_08_13_SPC.p = hyst_plot(CARI_storm7a_08_13_Q, CARI_storm7a_08_13_SPC, "CARI", "SPC", "0813a")

CARI_storm8_08_16_SPC.p = hyst_plot(CARI_storm8_08_16_Q, CARI_storm8_08_16_SPC, "CARI", "SPC", "0816")

# turb
CARI_storm1_05_08_turb.p = hyst_plot(CARI_storm1_05_08_Q, CARI_storm1_05_08_turb, "CARI", "turb", "0508")
CARI_storm2_06_30_turb.p = hyst_plot(CARI_storm2_06_30_Q, CARI_storm2_06_30_turb, "CARI", "turb", "0630")
CARI_storm3_07_12_turb.p = hyst_plot(CARI_storm3_07_12_Q, CARI_storm3_07_12_turb, "CARI", "turb", "0712")
#CARI_storm4_07_26_turb.p = hyst_plot(CARI_storm4_07_26_Q, CARI_storm4_07_26_turb, "CARI", "turb", "0726")
CARI_storm5_07_31_turb.p = hyst_plot(CARI_storm5_07_31_Q, CARI_storm5_07_31_turb, "CARI", "turb", "0731")
CARI_storm6a_08_02_turb.p = hyst_plot(CARI_storm6a_08_02_Q, CARI_storm6a_08_02_turb, "CARI", "turb", "0802a")

CARI_storm6c_08_05_turb.p = hyst_plot(CARI_storm6c_08_05_Q, CARI_storm6c_08_05_turb, "CARI", "turb", "0805c")
CARI_storm6d_08_10_turb.p = hyst_plot(CARI_storm6d_08_10_Q, CARI_storm6d_08_10_turb, "CARI", "turb", "0810d")
CARI_storm7a_08_13_turb.p = hyst_plot(CARI_storm7a_08_13_Q, CARI_storm7a_08_13_turb, "CARI", "turb", "0813a")

CARI_storm8_08_16_turb.p = hyst_plot(CARI_storm8_08_16_Q, CARI_storm8_08_16_turb, "CARI", "turb", "0816")

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

multiplot(CARI_storm1_05_08_NO3.p, CARI_storm1_05_08_fDOM.p,CARI_storm1_05_08_SPC.p, CARI_storm1_05_08_turb.p,
          CARI_storm2_06_30_NO3.p, CARI_storm2_06_30_fDOM.p,CARI_storm2_06_30_SPC.p, CARI_storm2_06_30_turb.p,
          CARI_storm3_07_12_NO3.p, CARI_storm3_07_12_fDOM.p,CARI_storm3_07_12_SPC.p, CARI_storm3_07_12_turb.p,
          
          CARI_storm5_07_31_NO3.p, CARI_storm5_07_31_fDOM.p,CARI_storm5_07_31_SPC.p, CARI_storm5_07_31_turb.p,
          CARI_storm6a_08_02_NO3.p, CARI_storm6a_08_02_fDOM.p,CARI_storm6a_08_02_SPC.p, CARI_storm6a_08_02_turb.p,
          
          CARI_storm6c_08_05_NO3.p, CARI_storm6c_08_05_fDOM.p,CARI_storm6c_08_05_SPC.p, CARI_storm6c_08_05_turb.p,
          CARI_storm6d_08_10_NO3.p, CARI_storm6d_08_10_fDOM.p,CARI_storm6d_08_10_SPC.p, CARI_storm6d_08_10_turb.p,
          CARI_storm7a_08_13_NO3.p, CARI_storm7a_08_13_fDOM.p,CARI_storm7a_08_13_SPC.p, CARI_storm7a_08_13_turb.p,
          
          CARI_storm8_08_16_NO3.p, CARI_storm8_08_16_fDOM.p,CARI_storm8_08_16_SPC.p, CARI_storm8_08_16_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("CARI_HI_Loops_2019.pdf",
       path = here("plots", "HI_plots", "2019", "CARI"),
       width = 20, height = 30, units = "in")




####################################### 2020  ####

# plot on normalized scale # 
#### load data #####
STRT_storm1a_06_18_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1a_06_18_Q.csv"))
STRT_storm1a_06_18_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1a_06_18_NO3.csv"))
STRT_storm1a_06_18_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1a_06_18_fDOM.csv"))
STRT_storm1a_06_18_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1a_06_18_SPC.csv"))
STRT_storm1a_06_18_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1a_06_18_Turb.csv"))
STRT_storm1a_06_18_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1a_06_18_abs.csv"))

STRT_storm1b_06_20_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1b_06_20_Q.csv"))
STRT_storm1b_06_20_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1b_06_20_NO3.csv"))
STRT_storm1b_06_20_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1b_06_20_SPC.csv"))
STRT_storm1b_06_20_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1b_06_20_Turb.csv"))
STRT_storm1b_06_20_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1b_06_20_abs.csv"))

STRT_storm1c_06_21_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1c_06_21_Q.csv"))
STRT_storm1c_06_21_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1c_06_21_NO3.csv"))
STRT_storm1c_06_21_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1c_06_21_fDOM.csv"))
STRT_storm1c_06_21_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1c_06_21_SPC.csv"))
STRT_storm1c_06_21_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1c_06_21_Turb.csv"))
STRT_storm1c_06_21_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1c_06_21_abs.csv"))

STRT_storm1d_06_23_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1d_06_23_Q.csv"))
STRT_storm1d_06_23_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1d_06_23_NO3.csv"))
STRT_storm1d_06_23_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1d_06_23_fDOM.csv"))
STRT_storm1d_06_23_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1d_06_23_SPC.csv"))
STRT_storm1d_06_23_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1d_06_23_Turb.csv"))
STRT_storm1d_06_23_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1d_06_23_abs.csv"))

STRT_storm1e_06_24_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1e_06_24_Q.csv"))
STRT_storm1e_06_24_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1e_06_24_NO3.csv"))
STRT_storm1e_06_24_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1e_06_24_fDOM.csv"))
STRT_storm1e_06_24_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1e_06_24_SPC.csv"))
STRT_storm1e_06_24_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1e_06_24_Turb.csv"))
STRT_storm1e_06_24_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm1e_06_24_abs.csv"))

STRT_storm2_07_09_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm2_07_09_Q.csv"))
STRT_storm2_07_09_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm2_07_09_NO3.csv"))
STRT_storm2_07_09_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm2_07_09_fDOM.csv"))
STRT_storm2_07_09_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm2_07_09_SPC.csv"))
STRT_storm2_07_09_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm2_07_09_Turb.csv"))
STRT_storm2_07_09_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm2_07_09_abs.csv"))

STRT_storm3_07_20_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm3_07_20_Q.csv"))
STRT_storm3_07_20_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm3_07_20_NO3.csv"))
STRT_storm3_07_20_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm3_07_20_fDOM.csv"))
STRT_storm3_07_20_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm3_07_20_SPC.csv"))
STRT_storm3_07_20_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm3_07_20_Turb.csv"))
STRT_storm3_07_20_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm3_07_20_abs.csv"))

STRT_storm4a_08_01_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4a_08_01_Q.csv"))
STRT_storm4a_08_01_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4a_08_01_NO3.csv"))
STRT_storm4a_08_01_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4a_08_01_fDOM.csv"))
STRT_storm4a_08_01_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4a_08_01_SPC.csv"))
STRT_storm4a_08_01_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4a_08_01_Turb.csv"))
STRT_storm4a_08_01_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4a_08_01_abs.csv"))

STRT_storm4b_08_03_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4b_08_03_Q.csv"))
STRT_storm4b_08_03_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4b_08_03_NO3.csv"))
STRT_storm4b_08_03_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4b_08_03_fDOM.csv"))
STRT_storm4b_08_03_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4b_08_03_SPC.csv"))
STRT_storm4b_08_03_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4b_08_03_Turb.csv"))
STRT_storm4b_08_03_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm4b_08_03_abs.csv"))

STRT_storm5_08_09_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm5_08_09_Q.csv"))
STRT_storm5_08_09_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm5_08_09_NO3.csv"))
STRT_storm5_08_09_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm5_08_09_fDOM.csv"))
STRT_storm5_08_09_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm5_08_09_SPC.csv"))
STRT_storm5_08_09_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm5_08_09_Turb.csv"))
STRT_storm5_08_09_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm5_08_09_abs.csv"))

STRT_storm6_08_12_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm6_08_12_Q.csv"))
STRT_storm6_08_12_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm6_08_12_NO3.csv"))
STRT_storm6_08_12_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm6_08_12_fDOM.csv"))
STRT_storm6_08_12_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm6_08_12_SPC.csv"))
STRT_storm6_08_12_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm6_08_12_Turb.csv"))
STRT_storm6_08_12_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm6_08_12_abs.csv"))

STRT_storm7a_08_20_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm7a_08_20_Q.csv"))
STRT_storm7a_08_20_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm7a_08_20_NO3.csv"))
STRT_storm7a_08_20_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm7a_08_20_fDOM.csv"))
STRT_storm7a_08_20_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm7a_08_20_SPC.csv"))
STRT_storm7a_08_20_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm7a_08_20_Turb.csv"))
STRT_storm7a_08_20_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm7a_08_20_abs.csv"))

STRT_storm8_08_28_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm8_08_28_Q.csv"))
STRT_storm8_08_28_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm8_08_28_NO3.csv"))
STRT_storm8_08_28_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm8_08_28_fDOM.csv"))
STRT_storm8_08_28_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm8_08_28_SPC.csv"))
STRT_storm8_08_28_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm8_08_28_Turb.csv"))
STRT_storm8_08_28_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm8_08_28_abs.csv"))

STRT_storm9a_09_03_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9a_09_03_Q.csv"))
STRT_storm9a_09_03_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9a_09_03_NO3.csv"))
STRT_storm9a_09_03_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9a_09_03_fDOM.csv"))
STRT_storm9a_09_03_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9a_09_03_SPC.csv"))
STRT_storm9a_09_03_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9a_09_03_Turb.csv"))
STRT_storm9a_09_03_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9a_09_03_abs.csv"))

STRT_storm9b_09_06_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9b_09_06_Q.csv"))
STRT_storm9b_09_06_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9b_09_06_NO3.csv"))
STRT_storm9b_09_06_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9b_09_06_fDOM.csv"))
STRT_storm9b_09_06_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9b_09_06_SPC.csv"))
STRT_storm9b_09_06_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9b_09_06_Turb.csv"))
STRT_storm9b_09_06_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9b_09_06_abs.csv"))

STRT_storm9c_09_09_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9c_09_09_Q.csv"))
STRT_storm9c_09_09_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9c_09_09_NO3.csv"))
STRT_storm9c_09_09_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9c_09_09_fDOM.csv"))
STRT_storm9c_09_09_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9c_09_09_SPC.csv"))
STRT_storm9c_09_09_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9c_09_09_Turb.csv"))
STRT_storm9c_09_09_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm9c_09_09_abs.csv"))

STRT_storm10_09_23_Q <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm10_09_23_Q.csv"))
STRT_storm10_09_23_NO3 <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm10_09_23_NO3.csv"))
STRT_storm10_09_23_fDOM <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm10_09_23_fDOM.csv"))
STRT_storm10_09_23_SPC <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm10_09_23_SPC.csv"))
STRT_storm10_09_23_turb <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm10_09_23_Turb.csv"))
STRT_storm10_09_23_abs <- read_csv(here("Storm_Events", "2020", "STRT", "STRT_storm10_09_23_abs.csv"))

# FRCH #
FRCH_storm1_06_13_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm1_06_13_Q.csv"))
FRCH_storm1_06_13_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm1_06_13_NO3.csv"))
FRCH_storm1_06_13_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm1_06_13_fDOM.csv"))
FRCH_storm1_06_13_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm1_06_13_SPC.csv"))
FRCH_storm1_06_13_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm1_06_13_Turb.csv"))
FRCH_storm1_06_13_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm1_06_13_abs.csv"))

FRCH_storm2_06_18_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm2_06_18_Q.csv"))
FRCH_storm2_06_18_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm2_06_18_NO3.csv"))
FRCH_storm2_06_18_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm2_06_18_fDOM.csv"))
FRCH_storm2_06_18_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm2_06_18_SPC.csv"))
FRCH_storm2_06_18_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm2_06_18_Turb.csv"))
FRCH_storm2_06_18_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm2_06_18_abs.csv"))

FRCH_storm3a_06_20_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3a_06_20_Q.csv"))
FRCH_storm3a_06_20_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3a_06_20_NO3.csv"))
FRCH_storm3a_06_20_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3a_06_20_fDOM.csv"))
FRCH_storm3a_06_20_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3a_06_20_SPC.csv"))
FRCH_storm3a_06_20_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3a_06_20_Turb.csv"))
FRCH_storm3a_06_20_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3a_06_20_abs.csv"))

FRCH_storm3b_06_21_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3b_06_21_Q.csv"))
FRCH_storm3b_06_21_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3b_06_21_NO3.csv"))
FRCH_storm3b_06_21_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3b_06_21_fDOM.csv"))
FRCH_storm3b_06_21_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3b_06_21_SPC.csv"))
FRCH_storm3b_06_21_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3b_06_21_Turb.csv"))
FRCH_storm3b_06_21_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3b_06_21_abs.csv"))

FRCH_storm3c_06_26_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3c_06_26_Q.csv"))
FRCH_storm3c_06_26_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3c_06_26_NO3.csv"))
FRCH_storm3c_06_26_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3c_06_26_fDOM.csv"))
FRCH_storm3c_06_26_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3c_06_26_SPC.csv"))
FRCH_storm3c_06_26_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3c_06_26_Turb.csv"))
FRCH_storm3c_06_26_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm3c_06_26_abs.csv"))

FRCH_storm4a_07_07_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4a_07_07_Q.csv"))
FRCH_storm4a_07_07_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4a_07_07_NO3.csv"))
FRCH_storm4a_07_07_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4a_07_07_fDOM.csv"))
FRCH_storm4a_07_07_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4a_07_07_SPC.csv"))
FRCH_storm4a_07_07_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4a_07_07_Turb.csv"))
FRCH_storm4a_07_07_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4a_07_07_abs.csv"))

FRCH_storm4b_07_09_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4b_07_09_Q.csv"))
FRCH_storm4b_07_09_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4b_07_09_NO3.csv"))
FRCH_storm4b_07_09_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4b_07_09_fDOM.csv"))
FRCH_storm4b_07_09_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4b_07_09_SPC.csv"))
FRCH_storm4b_07_09_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4b_07_09_Turb.csv"))
FRCH_storm4b_07_09_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm4b_07_09_abs.csv"))

FRCH_storm5_07_15_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm5_07_15_Q.csv"))
FRCH_storm5_07_15_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm5_07_15_NO3.csv"))
FRCH_storm5_07_15_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm5_07_15_fDOM.csv"))
FRCH_storm5_07_15_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm5_07_15_SPC.csv"))
FRCH_storm5_07_15_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm5_07_15_Turb.csv"))
FRCH_storm5_07_15_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm5_07_15_abs.csv"))

FRCH_storm6_07_18_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm6_07_18_Q.csv"))
FRCH_storm6_07_18_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm6_07_18_NO3.csv"))
FRCH_storm6_07_18_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm6_07_18_fDOM.csv"))
FRCH_storm6_07_18_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm6_07_18_SPC.csv"))
FRCH_storm6_07_18_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm6_07_18_Turb.csv"))
FRCH_storm6_07_18_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm6_07_18_abs.csv"))

FRCH_storm7_07_20_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm7_07_20_Q.csv"))
FRCH_storm7_07_20_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm7_07_20_NO3.csv"))
FRCH_storm7_07_20_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm7_07_20_fDOM.csv"))
FRCH_storm7_07_20_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm7_07_20_SPC.csv"))
FRCH_storm7_07_20_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm7_07_20_Turb.csv"))
FRCH_storm7_07_20_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm7_07_20_abs.csv"))

FRCH_storm8_07_26_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm8_07_26_Q.csv"))
FRCH_storm8_07_26_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm8_07_26_NO3.csv"))
FRCH_storm8_07_26_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm8_07_26_fDOM.csv"))
FRCH_storm8_07_26_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm8_07_26_SPC.csv"))
FRCH_storm8_07_26_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm8_07_26_Turb.csv"))
FRCH_storm8_07_26_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm8_07_26_abs.csv"))

FRCH_storm9a_08_01_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9a_08_01_Q.csv"))
FRCH_storm9a_08_01_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9a_08_01_NO3.csv"))
FRCH_storm9a_08_01_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9a_08_01_fDOM.csv"))
FRCH_storm9a_08_01_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9a_08_01_SPC.csv"))
FRCH_storm9a_08_01_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9a_08_01_Turb.csv"))
FRCH_storm9a_08_01_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9a_08_01_abs.csv"))

FRCH_storm9b_08_02_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9b_08_02_Q.csv"))
FRCH_storm9b_08_02_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9b_08_02_NO3.csv"))
FRCH_storm9b_08_02_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9b_08_02_fDOM.csv"))
FRCH_storm9b_08_02_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9b_08_02_SPC.csv"))
FRCH_storm9b_08_02_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9b_08_02_Turb.csv"))
FRCH_storm9b_08_02_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm9b_08_02_abs.csv"))

FRCH_storm10a_08_09_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10a_08_09_Q.csv"))
FRCH_storm10a_08_09_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10a_08_09_NO3.csv"))
FRCH_storm10a_08_09_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10a_08_09_fDOM.csv"))
FRCH_storm10a_08_09_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10a_08_09_SPC.csv"))
FRCH_storm10a_08_09_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10a_08_09_Turb.csv"))
FRCH_storm10a_08_09_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10a_08_09_abs.csv"))

FRCH_storm10b_08_12_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10b_08_12_Q.csv"))
FRCH_storm10b_08_12_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10b_08_12_NO3.csv"))
FRCH_storm10b_08_12_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10b_08_12_fDOM.csv"))
FRCH_storm10b_08_12_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10b_08_12_SPC.csv"))
FRCH_storm10b_08_12_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10b_08_12_Turb.csv"))
FRCH_storm10b_08_12_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm10b_08_12_abs.csv"))

FRCH_storm11_08_20_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm11_08_20_Q.csv"))
FRCH_storm11_08_20_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm11_08_20_NO3.csv"))
FRCH_storm11_08_20_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm11_08_20_fDOM.csv"))
FRCH_storm11_08_20_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm11_08_20_SPC.csv"))
FRCH_storm11_08_20_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm11_08_20_Turb.csv"))
FRCH_storm11_08_20_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm11_08_20_abs.csv"))

FRCH_storm12_09_06_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm12_09_06_Q.csv"))
FRCH_storm12_09_06_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm12_09_06_NO3.csv"))
FRCH_storm12_09_06_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm12_09_06_fDOM.csv"))
FRCH_storm12_09_06_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm12_09_06_SPC.csv"))
FRCH_storm12_09_06_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm12_09_06_Turb.csv"))
FRCH_storm12_09_06_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm12_09_06_abs.csv"))

FRCH_storm13_09_09_Q <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm13_09_09_Q.csv"))
FRCH_storm13_09_09_NO3 <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm13_09_09_NO3.csv"))
FRCH_storm13_09_09_fDOM <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm13_09_09_fDOM.csv"))
FRCH_storm13_09_09_SPC <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm13_09_09_SPC.csv"))
FRCH_storm13_09_09_turb <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm13_09_09_Turb.csv"))
FRCH_storm13_09_09_abs <- read_csv(here("Storm_Events", "2020", "FRCH", "FRCH_storm13_09_09_abs.csv"))


## MOOS ##
MOOS_storm1_06_20_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm1_06_20_Q.csv"))
MOOS_storm1_06_20_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm1_06_20_NO3.csv"))
MOOS_storm1_06_20_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm1_06_20_fDOM.csv"))
MOOS_storm1_06_20_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm1_06_20_SPC.csv"))
MOOS_storm1_06_20_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm1_06_20_Turb.csv"))
MOOS_storm1_06_20_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm1_06_20_abs.csv"))

MOOS_storm2_06_28_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm2_06_28_Q.csv"))
MOOS_storm2_06_28_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm2_06_28_NO3.csv"))
MOOS_storm2_06_28_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm2_06_28_fDOM.csv"))
MOOS_storm2_06_28_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm2_06_28_SPC.csv"))
MOOS_storm2_06_28_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm2_06_28_Turb.csv"))
MOOS_storm2_06_28_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm2_06_28_abs.csv"))

MOOS_storm3_07_18_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm3_07_18_Q.csv"))
MOOS_storm3_07_18_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm3_07_18_NO3.csv"))
MOOS_storm3_07_18_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm3_07_18_fDOM.csv"))
MOOS_storm3_07_18_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm3_07_18_SPC.csv"))
MOOS_storm3_07_18_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm3_07_18_Turb.csv"))
MOOS_storm3_07_18_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm3_07_18_abs.csv"))

MOOS_storm4_07_20_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm4_07_20_Q.csv"))
MOOS_storm4_07_20_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm4_07_20_NO3.csv"))
MOOS_storm4_07_20_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm4_07_20_fDOM.csv"))
MOOS_storm4_07_20_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm4_07_20_SPC.csv"))
MOOS_storm4_07_20_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm4_07_20_Turb.csv"))
MOOS_storm4_07_20_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm4_07_20_abs.csv"))

MOOS_storm5_07_26_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm5_07_26_Q.csv"))
MOOS_storm5_07_26_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm5_07_26_NO3.csv"))
MOOS_storm5_07_26_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm5_07_26_fDOM.csv"))
MOOS_storm5_07_26_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm5_07_26_SPC.csv"))
MOOS_storm5_07_26_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm5_07_26_Turb.csv"))
MOOS_storm5_07_26_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm5_07_26_abs.csv"))

MOOS_storm6a_08_01_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6a_08_01_Q.csv"))
MOOS_storm6a_08_01_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6a_08_01_NO3.csv"))
MOOS_storm6a_08_01_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6a_08_01_fDOM.csv"))
MOOS_storm6a_08_01_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6a_08_01_SPC.csv"))
MOOS_storm6a_08_01_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6a_08_01_Turb.csv"))
MOOS_storm6a_08_01_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6a_08_01_abs.csv"))

MOOS_storm6b_08_02_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6b_08_02_Q.csv"))
MOOS_storm6b_08_02_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6b_08_02_NO3.csv"))
MOOS_storm6b_08_02_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6b_08_02_fDOM.csv"))
MOOS_storm6b_08_02_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6b_08_02_SPC.csv"))
MOOS_storm6b_08_02_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6b_08_02_Turb.csv"))
MOOS_storm6b_08_02_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm6b_08_02_abs.csv"))

MOOS_storm7a_08_09_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7a_08_09_Q.csv"))
MOOS_storm7a_08_09_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7a_08_09_NO3.csv"))
MOOS_storm7a_08_09_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7a_08_09_fDOM.csv"))
MOOS_storm7a_08_09_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7a_08_09_SPC.csv"))
MOOS_storm7a_08_09_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7a_08_09_Turb.csv"))
MOOS_storm7a_08_09_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7a_08_09_abs.csv"))

MOOS_storm7b_08_12_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7b_08_12_Q.csv"))
MOOS_storm7b_08_12_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7b_08_12_NO3.csv"))
MOOS_storm7b_08_12_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7b_08_12_fDOM.csv"))
MOOS_storm7b_08_12_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7b_08_12_SPC.csv"))
MOOS_storm7b_08_12_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7b_08_12_Turb.csv"))
MOOS_storm7b_08_12_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm7b_08_12_abs.csv"))

MOOS_storm8_09_06_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm8_09_06_Q.csv"))
MOOS_storm8_09_06_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm8_09_06_NO3.csv"))
MOOS_storm8_09_06_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm8_09_06_fDOM.csv"))
MOOS_storm8_09_06_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm8_09_06_SPC.csv"))
MOOS_storm8_09_06_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm8_09_06_Turb.csv"))
MOOS_storm8_09_06_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm8_09_06_abs.csv"))

MOOS_storm9_09_09_Q <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm9_09_09_Q.csv"))
MOOS_storm9_09_09_NO3 <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm9_09_09_NO3.csv"))
MOOS_storm9_09_09_fDOM <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm9_09_09_fDOM.csv"))
MOOS_storm9_09_09_SPC <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm9_09_09_SPC.csv"))
MOOS_storm9_09_09_turb <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm9_09_09_Turb.csv"))
MOOS_storm9_09_09_abs <- read_csv(here("Storm_Events", "2020", "MOOS", "MOOS_storm9_09_09_abs.csv"))

## VAUL ## 
VAUL_storm1a_06_19_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1a_06_19_Q.csv"))
VAUL_storm1a_06_19_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1a_06_19_NO3.csv"))
VAUL_storm1a_06_19_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1a_06_19_fDOM.csv"))
VAUL_storm1a_06_19_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1a_06_19_SPC.csv"))
VAUL_storm1a_06_19_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1a_06_19_Turb.csv"))
VAUL_storm1a_06_19_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1a_06_19_abs.csv"))

VAUL_storm1b_06_20_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1b_06_20_Q.csv"))
VAUL_storm1b_06_20_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1b_06_20_NO3.csv"))
VAUL_storm1b_06_20_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1b_06_20_fDOM.csv"))
VAUL_storm1b_06_20_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1b_06_20_SPC.csv"))
VAUL_storm1b_06_20_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1b_06_20_Turb.csv"))
VAUL_storm1b_06_20_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1b_06_20_abs.csv"))

VAUL_storm1c_06_22_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1c_06_22_Q.csv"))
VAUL_storm1c_06_22_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1c_06_22_NO3.csv"))
VAUL_storm1c_06_22_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1c_06_22_fDOM.csv"))
VAUL_storm1c_06_22_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1c_06_22_SPC.csv"))
VAUL_storm1c_06_22_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1c_06_22_Turb.csv"))
VAUL_storm1c_06_22_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm1c_06_22_abs.csv"))

VAUL_storm2_06_28_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm2_06_28_Q.csv"))
VAUL_storm2_06_28_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm2_06_28_NO3.csv"))
VAUL_storm2_06_28_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm2_06_28_fDOM.csv"))
VAUL_storm2_06_28_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm2_06_28_SPC.csv"))
VAUL_storm2_06_28_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm2_06_28_Turb.csv"))
VAUL_storm2_06_28_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm2_06_28_abs.csv"))

VAUL_storm3_07_09_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm3_07_09_Q.csv"))
VAUL_storm3_07_09_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm3_07_09_NO3.csv"))
VAUL_storm3_07_09_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm3_07_09_fDOM.csv"))
VAUL_storm3_07_09_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm3_07_09_SPC.csv"))
VAUL_storm3_07_09_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm3_07_09_Turb.csv"))
VAUL_storm3_07_09_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm3_07_09_abs.csv"))

VAUL_storm4_07_12_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm4_07_12_Q.csv"))
VAUL_storm4_07_12_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm4_07_12_NO3.csv"))
VAUL_storm4_07_12_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm4_07_12_fDOM.csv"))
VAUL_storm4_07_12_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm4_07_12_SPC.csv"))
VAUL_storm4_07_12_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm4_07_12_Turb.csv"))
VAUL_storm4_07_12_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm4_07_12_abs.csv"))

VAUL_storm5_07_26_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm5_07_26_Q.csv"))
VAUL_storm5_07_26_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm5_07_26_NO3.csv"))
VAUL_storm5_07_26_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm5_07_26_fDOM.csv"))
VAUL_storm5_07_26_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm5_07_26_SPC.csv"))
VAUL_storm5_07_26_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm5_07_26_Turb.csv"))
VAUL_storm5_07_26_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm5_07_26_abs.csv"))

VAUL_storm6a_08_01_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6a_08_01_Q.csv"))
VAUL_storm6a_08_01_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6a_08_01_NO3.csv"))
VAUL_storm6a_08_01_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6a_08_01_fDOM.csv"))
VAUL_storm6a_08_01_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6a_08_01_SPC.csv"))
VAUL_storm6a_08_01_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6a_08_01_Turb.csv"))
VAUL_storm6a_08_01_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6a_08_01_abs.csv"))

VAUL_storm6b_08_02_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6b_08_02_Q.csv"))
VAUL_storm6b_08_02_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6b_08_02_NO3.csv"))
VAUL_storm6b_08_02_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6b_08_02_fDOM.csv"))
VAUL_storm6b_08_02_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6b_08_02_SPC.csv"))
VAUL_storm6b_08_02_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6b_08_02_Turb.csv"))
VAUL_storm6b_08_02_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm6b_08_02_abs.csv"))

VAUL_storm7_08_08_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm7_08_08_Q.csv"))
VAUL_storm7_08_08_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm7_08_08_NO3.csv"))
VAUL_storm7_08_08_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm7_08_08_fDOM.csv"))
VAUL_storm7_08_08_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm7_08_08_SPC.csv"))
VAUL_storm7_08_08_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm7_08_08_Turb.csv"))
VAUL_storm7_08_08_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm7_08_08_abs.csv"))

VAUL_storm8_08_11_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm8_08_11_Q.csv"))
VAUL_storm8_08_11_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm8_08_11_NO3.csv"))
VAUL_storm8_08_11_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm8_08_11_fDOM.csv"))
VAUL_storm8_08_11_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm8_08_11_SPC.csv"))
VAUL_storm8_08_11_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm8_08_11_Turb.csv"))
VAUL_storm8_08_11_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm8_08_11_abs.csv"))

VAUL_storm9_08_12_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm9_08_12_Q.csv"))
VAUL_storm9_08_12_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm9_08_12_NO3.csv"))
VAUL_storm9_08_12_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm9_08_12_fDOM.csv"))
VAUL_storm9_08_12_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm9_08_12_SPC.csv"))
VAUL_storm9_08_12_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm9_08_12_Turb.csv"))
VAUL_storm9_08_12_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm9_08_12_abs.csv"))

VAUL_storm10_08_25_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm10_08_25_Q.csv"))
VAUL_storm10_08_25_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm10_08_25_NO3.csv"))
VAUL_storm10_08_25_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm10_08_25_fDOM.csv"))
VAUL_storm10_08_25_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm10_08_25_SPC.csv"))
VAUL_storm10_08_25_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm10_08_25_Turb.csv"))
VAUL_storm10_08_25_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm10_08_25_abs.csv"))

VAUL_storm11_08_27_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm11_08_27_Q.csv"))
VAUL_storm11_08_27_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm11_08_27_NO3.csv"))
VAUL_storm11_08_27_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm11_08_27_fDOM.csv"))
VAUL_storm11_08_27_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm11_08_27_SPC.csv"))
VAUL_storm11_08_27_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm11_08_27_Turb.csv"))
VAUL_storm11_08_27_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm11_08_27_abs.csv"))

VAUL_storm12_09_01_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm12_09_01_Q.csv"))
VAUL_storm12_09_01_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm12_09_01_NO3.csv"))
VAUL_storm12_09_01_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm12_09_01_fDOM.csv"))
VAUL_storm12_09_01_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm12_09_01_SPC.csv"))
VAUL_storm12_09_01_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm12_09_01_Turb.csv"))
VAUL_storm12_09_01_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm12_09_01_abs.csv"))

VAUL_storm13_09_03_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm13_09_03_Q.csv"))
VAUL_storm13_09_03_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm13_09_03_NO3.csv"))
VAUL_storm13_09_03_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm13_09_03_fDOM.csv"))
VAUL_storm13_09_03_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm13_09_03_SPC.csv"))
VAUL_storm13_09_03_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm13_09_03_Turb.csv"))
VAUL_storm13_09_03_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm13_09_03_abs.csv"))

VAUL_storm14_09_06_Q <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm14_09_06_Q.csv"))
VAUL_storm14_09_06_NO3 <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm14_09_06_NO3.csv"))
VAUL_storm14_09_06_fDOM <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm14_09_06_fDOM.csv"))
VAUL_storm14_09_06_SPC <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm14_09_06_SPC.csv"))
VAUL_storm14_09_06_turb <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm14_09_06_Turb.csv"))
VAUL_storm14_09_06_abs <- read_csv(here("Storm_Events", "2020", "VAUL", "VAUL_storm14_09_06_abs.csv"))


## POKE ## 
POKE_storm1_06_09_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm1_06_09_Q.csv"))
POKE_storm1_06_09_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm1_06_09_NO3.csv"))
POKE_storm1_06_09_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm1_06_09_fDOM.csv"))
POKE_storm1_06_09_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm1_06_09_SPC.csv"))
POKE_storm1_06_09_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm1_06_09_Turb.csv"))
POKE_storm1_06_09_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm1_06_09_abs.csv"))

POKE_storm2_06_12_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm2_06_12_Q.csv"))
POKE_storm2_06_12_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm2_06_12_NO3.csv"))
POKE_storm2_06_12_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm2_06_12_fDOM.csv"))
POKE_storm2_06_12_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm2_06_12_SPC.csv"))
POKE_storm2_06_12_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm2_06_12_Turb.csv"))
POKE_storm2_06_12_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm2_06_12_abs.csv"))

POKE_storm3_06_15_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm3_06_15_Q.csv"))
POKE_storm3_06_15_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm3_06_15_NO3.csv"))
POKE_storm3_06_15_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm3_06_15_fDOM.csv"))
POKE_storm3_06_15_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm3_06_15_SPC.csv"))
POKE_storm3_06_15_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm3_06_15_Turb.csv"))
POKE_storm3_06_15_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm3_06_15_abs.csv"))

POKE_storm4a_06_19_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4a_06_19_Q.csv"))
POKE_storm4a_06_19_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4a_06_19_NO3.csv"))
POKE_storm4a_06_19_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4a_06_19_fDOM.csv"))
POKE_storm4a_06_19_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4a_06_19_SPC.csv"))
POKE_storm4a_06_19_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4a_06_19_Turb.csv"))
POKE_storm4a_06_19_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4a_06_19_abs.csv"))

POKE_storm4b_06_20_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4b_06_20_Q.csv"))
POKE_storm4b_06_20_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4b_06_20_NO3.csv"))
POKE_storm4b_06_20_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4b_06_20_fDOM.csv"))
POKE_storm4b_06_20_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4b_06_20_SPC.csv"))
POKE_storm4b_06_20_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4b_06_20_Turb.csv"))
POKE_storm4b_06_20_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4b_06_20_abs.csv"))

POKE_storm4c_06_21_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4c_06_21_Q.csv"))
POKE_storm4c_06_21_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4c_06_21_NO3.csv"))
POKE_storm4c_06_21_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4c_06_21_fDOM.csv"))
POKE_storm4c_06_21_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4c_06_21_SPC.csv"))
POKE_storm4c_06_21_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4c_06_21_Turb.csv"))
POKE_storm4c_06_21_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm4c_06_21_abs.csv"))

POKE_storm5_06_22_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm5_06_22_Q.csv"))
POKE_storm5_06_22_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm5_06_22_NO3.csv"))
POKE_storm5_06_22_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm5_06_22_fDOM.csv"))
POKE_storm5_06_22_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm5_06_22_SPC.csv"))
POKE_storm5_06_22_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm5_06_22_Turb.csv"))
POKE_storm5_06_22_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm5_06_22_abs.csv"))

POKE_storm6_06_29_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm6_06_29_Q.csv"))
POKE_storm6_06_29_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm6_06_29_NO3.csv"))
POKE_storm6_06_29_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm6_06_29_fDOM.csv"))
POKE_storm6_06_29_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm6_06_29_SPC.csv"))
POKE_storm6_06_29_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm6_06_29_Turb.csv"))
POKE_storm6_06_29_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm6_06_29_abs.csv"))

POKE_storm7_07_04_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm7_07_04_Q.csv"))
POKE_storm7_07_04_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm7_07_04_NO3.csv"))
POKE_storm7_07_04_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm7_07_04_fDOM.csv"))
POKE_storm7_07_04_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm7_07_04_SPC.csv"))
POKE_storm7_07_04_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm7_07_04_Turb.csv"))
POKE_storm7_07_04_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm7_07_04_abs.csv"))

POKE_storm8_07_09_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm8_07_09_Q.csv"))
POKE_storm8_07_09_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm8_07_09_NO3.csv"))
POKE_storm8_07_09_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm8_07_09_fDOM.csv"))
POKE_storm8_07_09_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm8_07_09_SPC.csv"))
POKE_storm8_07_09_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm8_07_09_Turb.csv"))
POKE_storm8_07_09_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm8_07_09_abs.csv"))

POKE_storm9_07_12_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm9_07_12_Q.csv"))
POKE_storm9_07_12_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm9_07_12_NO3.csv"))
POKE_storm9_07_12_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm9_07_12_fDOM.csv"))
POKE_storm9_07_12_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm9_07_12_SPC.csv"))
POKE_storm9_07_12_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm9_07_12_Turb.csv"))
POKE_storm9_07_12_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm9_07_12_abs.csv"))

POKE_storm10_07_16_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm10_07_16_Q.csv"))
POKE_storm10_07_16_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm10_07_16_NO3.csv"))
POKE_storm10_07_16_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm10_07_16_fDOM.csv"))
POKE_storm10_07_16_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm10_07_16_SPC.csv"))
POKE_storm10_07_16_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm10_07_16_Turb.csv"))
POKE_storm10_07_16_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm10_07_16_abs.csv"))

POKE_storm11_07_18_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm11_07_18_Q.csv"))
POKE_storm11_07_18_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm11_07_18_NO3.csv"))
POKE_storm11_07_18_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm11_07_18_fDOM.csv"))
POKE_storm11_07_18_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm11_07_18_SPC.csv"))
POKE_storm11_07_18_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm11_07_18_Turb.csv"))
POKE_storm11_07_18_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm11_07_18_abs.csv"))

POKE_storm12_07_20_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm12_07_20_Q.csv"))
POKE_storm12_07_20_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm12_07_20_NO3.csv"))
POKE_storm12_07_20_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm12_07_20_fDOM.csv"))
POKE_storm12_07_20_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm12_07_20_SPC.csv"))
POKE_storm12_07_20_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm12_07_20_Turb.csv"))
POKE_storm12_07_20_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm12_07_20_abs.csv"))

POKE_storm13_07_24_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm13_07_24_Q.csv"))
POKE_storm13_07_24_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm13_07_24_NO3.csv"))
POKE_storm13_07_24_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm13_07_24_fDOM.csv"))
POKE_storm13_07_24_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm13_07_24_SPC.csv"))
POKE_storm13_07_24_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm13_07_24_Turb.csv"))
POKE_storm13_07_24_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm13_07_24_abs.csv"))

POKE_storm14_07_26_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm14_07_26_Q.csv"))
POKE_storm14_07_26_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm14_07_26_NO3.csv"))
POKE_storm14_07_26_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm14_07_26_fDOM.csv"))
POKE_storm14_07_26_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm14_07_26_SPC.csv"))
POKE_storm14_07_26_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm14_07_26_Turb.csv"))
POKE_storm14_07_26_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm14_07_26_abs.csv"))

POKE_storm15_08_02_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm15_08_02_Q.csv"))
POKE_storm15_08_02_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm15_08_02_NO3.csv"))
POKE_storm15_08_02_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm15_08_02_fDOM.csv"))
POKE_storm15_08_02_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm15_08_02_SPC.csv"))
POKE_storm15_08_02_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm15_08_02_Turb.csv"))
POKE_storm15_08_02_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm15_08_02_abs.csv"))

POKE_storm16_08_12_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm16_08_12_Q.csv"))
POKE_storm16_08_12_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm16_08_12_NO3.csv"))
POKE_storm16_08_12_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm16_08_12_fDOM.csv"))
POKE_storm16_08_12_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm16_08_12_SPC.csv"))
POKE_storm16_08_12_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm16_08_12_Turb.csv"))
POKE_storm16_08_12_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm16_08_12_abs.csv"))

POKE_storm17_08_23_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm17_08_23_Q.csv"))
POKE_storm17_08_23_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm17_08_23_NO3.csv"))
POKE_storm17_08_23_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm17_08_23_fDOM.csv"))
POKE_storm17_08_23_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm17_08_23_SPC.csv"))
POKE_storm17_08_23_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm17_08_23_Turb.csv"))
POKE_storm17_08_23_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm17_08_23_abs.csv"))

POKE_storm18_08_25_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm18_08_25_Q.csv"))
POKE_storm18_08_25_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm18_08_25_NO3.csv"))
POKE_storm18_08_25_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm18_08_25_fDOM.csv"))
POKE_storm18_08_25_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm18_08_25_SPC.csv"))
POKE_storm18_08_25_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm18_08_25_Turb.csv"))
POKE_storm18_08_25_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm18_08_25_abs.csv"))

POKE_storm19_08_27_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm19_08_27_Q.csv"))
POKE_storm19_08_27_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm19_08_27_NO3.csv"))
POKE_storm19_08_27_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm19_08_27_fDOM.csv"))
POKE_storm19_08_27_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm19_08_27_SPC.csv"))
POKE_storm19_08_27_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm19_08_27_Turb.csv"))
POKE_storm19_08_27_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm19_08_27_abs.csv"))

POKE_storm20_09_01_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm20_09_01_Q.csv"))
POKE_storm20_09_01_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm20_09_01_NO3.csv"))
POKE_storm20_09_01_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm20_09_01_fDOM.csv"))
POKE_storm20_09_01_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm20_09_01_SPC.csv"))
POKE_storm20_09_01_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm20_09_01_Turb.csv"))
POKE_storm20_09_01_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm20_09_01_abs.csv"))

POKE_storm21_09_03_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm21_09_03_Q.csv"))
POKE_storm21_09_03_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm21_09_03_NO3.csv"))
POKE_storm21_09_03_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm21_09_03_fDOM.csv"))
POKE_storm21_09_03_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm21_09_03_SPC.csv"))
POKE_storm21_09_03_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm21_09_03_Turb.csv"))
POKE_storm21_09_03_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm21_09_03_abs.csv"))

POKE_storm22a_09_07_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22a_09_07_Q.csv"))
POKE_storm22a_09_07_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22a_09_07_NO3.csv"))
POKE_storm22a_09_07_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22a_09_07_fDOM.csv"))
POKE_storm22a_09_07_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22a_09_07_SPC.csv"))
POKE_storm22a_09_07_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22a_09_07_Turb.csv"))
POKE_storm22a_09_07_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22a_09_07_abs.csv"))

POKE_storm22b_09_09_Q <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22b_09_09_Q.csv"))
POKE_storm22b_09_09_NO3 <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22b_09_09_NO3.csv"))
POKE_storm22b_09_09_fDOM <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22b_09_09_fDOM.csv"))
POKE_storm22b_09_09_SPC <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22b_09_09_SPC.csv"))
POKE_storm22b_09_09_turb <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22b_09_09_Turb.csv"))
POKE_storm22b_09_09_abs <- read_csv(here("Storm_Events", "2020", "POKE", "POKE_storm22b_09_09_abs.csv"))

## CARI ## 
CARI_storm2a_06_19_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2a_06_19_Q.csv"))
CARI_storm2a_06_19_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2a_06_19_NO3.csv"))
CARI_storm2a_06_19_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2a_06_19_fDOM.csv"))
CARI_storm2a_06_19_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2a_06_19_SPC.csv"))
CARI_storm2a_06_19_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2a_06_19_Turb.csv"))

CARI_storm2b_06_20_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2b_06_20_Q.csv"))
CARI_storm2b_06_20_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2b_06_20_NO3.csv"))
CARI_storm2b_06_20_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2b_06_20_fDOM.csv"))
CARI_storm2b_06_20_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2b_06_20_SPC.csv"))
CARI_storm2b_06_20_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2b_06_20_Turb.csv"))

CARI_storm2c_06_21_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2c_06_21_Q.csv"))
CARI_storm2c_06_21_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2c_06_21_NO3.csv"))
CARI_storm2c_06_21_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2c_06_21_fDOM.csv"))
CARI_storm2c_06_21_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2c_06_21_SPC.csv"))
CARI_storm2c_06_21_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm2c_06_21_Turb.csv"))

CARI_storm3_06_23_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm3_06_23_Q.csv"))
CARI_storm3_06_23_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm3_06_23_NO3.csv"))
CARI_storm3_06_23_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm3_06_23_fDOM.csv"))
CARI_storm3_06_23_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm3_06_23_SPC.csv"))
CARI_storm3_06_23_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm3_06_23_Turb.csv"))

CARI_storm4_07_09_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm4_07_09_Q.csv"))
CARI_storm4_07_09_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm4_07_09_NO3.csv"))
CARI_storm4_07_09_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm4_07_09_fDOM.csv"))
CARI_storm4_07_09_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm4_07_09_SPC.csv"))
CARI_storm4_07_09_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm4_07_09_Turb.csv"))

CARI_storm5_07_13_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm5_07_13_Q.csv"))
CARI_storm5_07_13_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm5_07_13_NO3.csv"))
CARI_storm5_07_13_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm5_07_13_fDOM.csv"))
CARI_storm5_07_13_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm5_07_13_SPC.csv"))
CARI_storm5_07_13_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm5_07_13_Turb.csv"))

CARI_storm6_07_24_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm6_07_24_Q.csv"))
CARI_storm6_07_24_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm6_07_24_NO3.csv"))
CARI_storm6_07_24_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm6_07_24_fDOM.csv"))
CARI_storm6_07_24_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm6_07_24_SPC.csv"))
CARI_storm6_07_24_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm6_07_24_Turb.csv"))

CARI_storm7_07_27_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm7_07_27_Q.csv"))
CARI_storm7_07_27_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm7_07_27_NO3.csv"))
CARI_storm7_07_27_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm7_07_27_fDOM.csv"))
CARI_storm7_07_27_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm7_07_27_SPC.csv"))
CARI_storm7_07_27_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm7_07_27_Turb.csv"))

CARI_storm8a_08_02_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8a_08_02_Q.csv"))
CARI_storm8a_08_02_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8a_08_02_NO3.csv"))
CARI_storm8a_08_02_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8a_08_02_fDOM.csv"))
CARI_storm8a_08_02_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8a_08_02_SPC.csv"))
CARI_storm8a_08_02_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8a_08_02_Turb.csv"))

CARI_storm8b_08_03_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8b_08_03_Q.csv"))
CARI_storm8b_08_03_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8b_08_03_NO3.csv"))
CARI_storm8b_08_03_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8b_08_03_fDOM.csv"))
CARI_storm8b_08_03_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8b_08_03_SPC.csv"))
CARI_storm8b_08_03_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm8b_08_03_Turb.csv"))

CARI_storm9_09_07_Q <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm9_09_07_Q.csv"))
CARI_storm9_09_07_NO3 <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm9_09_07_NO3.csv"))
CARI_storm9_09_07_fDOM <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm9_09_07_fDOM.csv"))
CARI_storm9_09_07_SPC <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm9_09_07_SPC.csv"))
CARI_storm9_09_07_turb <- read_csv(here("Storm_Events", "2020", "CARI", "CARI_storm9_09_07_Turb.csv"))

# normalize data #
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

#### fxn: plot hysteresis loop ####
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

# plot STRT loops #
STRT_storm1a_06_18_NO3.p = hyst_plot(STRT_storm1a_06_18_Q, STRT_storm1a_06_18_NO3, "STRT", "NO3", "0618a")
STRT_storm1b_06_20_NO3.p = hyst_plot(STRT_storm1b_06_20_Q, STRT_storm1b_06_20_NO3, "STRT", "NO3", "0620b")
STRT_storm1c_06_21_NO3.p = hyst_plot(STRT_storm1c_06_21_Q, STRT_storm1c_06_21_NO3, "STRT", "NO3", "0621c")
STRT_storm1d_06_23_NO3.p = hyst_plot(STRT_storm1d_06_23_Q, STRT_storm1d_06_23_NO3, "STRT", "NO3", "0623d")
STRT_storm1e_06_24_NO3.p = hyst_plot(STRT_storm1e_06_24_Q, STRT_storm1e_06_24_NO3, "STRT", "NO3", "0624e")
STRT_storm2_07_09_NO3.p = hyst_plot(STRT_storm2_07_09_Q, STRT_storm2_07_09_NO3, "STRT", "NO3", "0709")
STRT_storm3_07_20_NO3.p = hyst_plot(STRT_storm3_07_20_Q, STRT_storm3_07_20_NO3, "STRT", "NO3", "0720")
STRT_storm4a_08_01_NO3.p = hyst_plot(STRT_storm4a_08_01_Q, STRT_storm4a_08_01_NO3, "STRT", "NO3", "0801a")
STRT_storm4b_08_03_NO3.p = hyst_plot(STRT_storm4b_08_03_Q, STRT_storm4b_08_03_NO3, "STRT", "NO3", "0803b")
STRT_storm5_08_09_NO3.p = hyst_plot(STRT_storm5_08_09_Q, STRT_storm5_08_09_NO3, "STRT", "NO3", "0809")
STRT_storm6_08_12_NO3.p = hyst_plot(STRT_storm6_08_12_Q, STRT_storm6_08_12_NO3, "STRT", "NO3", "0812")
STRT_storm7a_08_20_NO3.p = hyst_plot(STRT_storm7a_08_20_Q, STRT_storm7a_08_20_NO3, "STRT", "NO3", "0820a")
#STRT_storm7b_08_21_NO3.p = hyst_plot(STRT_storm7b, STRT_storm7b_08_21_NO3, "STRT", "NO3", "0821b")
STRT_storm8_08_28_NO3.p = hyst_plot(STRT_storm8_08_28_Q, STRT_storm8_08_28_NO3, "STRT", "NO3", "0828")
STRT_storm9a_09_03_NO3.p = hyst_plot(STRT_storm9a_09_03_Q, STRT_storm9a_09_03_NO3, "STRT", "NO3", "0903a")
STRT_storm9b_09_06_NO3.p = hyst_plot(STRT_storm9b_09_06_Q, STRT_storm9b_09_06_NO3, "STRT", "NO3", "0906b")
STRT_storm9c_09_09_NO3.p = hyst_plot(STRT_storm9c_09_09_Q, STRT_storm9c_09_09_NO3, "STRT", "NO3", "0909c")
STRT_storm10_09_23_NO3.p = hyst_plot(STRT_storm10_09_23_Q, STRT_storm10_09_23_NO3, "STRT", "NO3", "0923")

# NO3
multiplot(STRT_storm1a_06_18_NO3.p) # works 
multiplot(STRT_storm1b_06_20_NO3.p)# fdom  does not work
multiplot(STRT_storm1c_06_21_NO3.p) # works 
multiplot(STRT_storm1d_06_23_NO3.p) # works 
multiplot(STRT_storm1e_06_24_NO3.p) # works
multiplot(STRT_storm2_07_09_NO3.p) 
multiplot(STRT_storm3_07_20_NO3.p) # works 
multiplot(STRT_storm4a_08_01_NO3.p) # works
multiplot(STRT_storm4b_08_03_NO3.p) # works 
multiplot(STRT_storm5_08_09_NO3.p) # works 
multiplot(STRT_storm6_08_12_NO3.p) # works 
multiplot(STRT_storm7a_08_20_NO3.p) # works
#multiplot(STRT_storm7b_08_21_NO3.p) # works 
multiplot(STRT_storm8_08_28_NO3.p) # works
multiplot(STRT_storm9a_09_03_NO3.p)
multiplot(STRT_storm9b_09_06_NO3.p)
multiplot(STRT_storm9c_09_09_NO3.p) # works
multiplot(STRT_storm10_09_23_NO3.p) # works 

# fDOM #
STRT_storm1a_06_18_fDOM.p = hyst_plot(STRT_storm1a_06_18_Q, STRT_storm1a_06_18_fDOM, "STRT", "fDOM", "0618a")
STRT_storm1b_06_20_fDOM.p = hyst_plot(STRT_storm1b_06_20_Q, STRT_storm1b_06_20_fDOM, "STRT", "fDOM", "0620b")
STRT_storm1c_06_21_fDOM.p = hyst_plot(STRT_storm1c_06_21_Q, STRT_storm1c_06_21_fDOM, "STRT", "fDOM", "0621c")
STRT_storm1d_06_23_fDOM.p = hyst_plot(STRT_storm1d_06_23_Q, STRT_storm1d_06_23_fDOM, "STRT", "fDOM", "0623d")
STRT_storm1e_06_24_fDOM.p = hyst_plot(STRT_storm1e_06_24_Q, STRT_storm1e_06_24_fDOM, "STRT", "fDOM", "0624e")
STRT_storm2_07_09_fDOM.p = hyst_plot(STRT_storm2_07_09_Q, STRT_storm2_07_09_fDOM, "STRT", "fDOM", "0709")
STRT_storm3_07_20_fDOM.p = hyst_plot(STRT_storm3_07_20_Q, STRT_storm3_07_20_fDOM, "STRT", "fDOM", "0720")
STRT_storm4a_08_01_fDOM.p = hyst_plot(STRT_storm4a_08_01_Q, STRT_storm4a_08_01_fDOM, "STRT", "fDOM", "0801a")
STRT_storm4b_08_03_fDOM.p = hyst_plot(STRT_storm4b_08_03_Q, STRT_storm4b_08_03_fDOM, "STRT", "fDOM", "0803b")
STRT_storm5_08_09_fDOM.p = hyst_plot(STRT_storm5_08_09_Q, STRT_storm5_08_09_fDOM, "STRT", "fDOM", "0809")
STRT_storm6_08_12_fDOM.p = hyst_plot(STRT_storm6_08_12_Q, STRT_storm6_08_12_fDOM, "STRT", "fDOM", "0812")
STRT_storm7a_08_20_fDOM.p = hyst_plot(STRT_storm7a_08_20_Q, STRT_storm7a_08_20_fDOM, "STRT", "fDOM", "0820a")
#STRT_storm7b_08_21_fDOM.p = hyst_plot(STRT_storm7b_08_21_Q, STRT_storm7b_08_21_fDOM, "STRT", "fDOM", "0821b")
STRT_storm8_08_28_fDOM.p = hyst_plot(STRT_storm8_08_28_Q, STRT_storm8_08_28_fDOM, "STRT", "fDOM", "0828")
STRT_storm9a_09_03_fDOM.p = hyst_plot(STRT_storm9a_09_03_Q, STRT_storm9a_09_03_fDOM, "STRT", "fDOM", "0903a")
STRT_storm9b_09_06_fDOM.p = hyst_plot(STRT_storm9b_09_06_Q, STRT_storm9b_09_06_fDOM, "STRT", "fDOM", "0906b")
STRT_storm9c_09_09_fDOM.p = hyst_plot(STRT_storm9c_09_09_Q, STRT_storm9c_09_09_fDOM, "STRT", "fDOM", "0909c")
STRT_storm10_09_23_fDOM.p = hyst_plot(STRT_storm10_09_23_Q, STRT_storm10_09_23_fDOM, "STRT", "fDOM", "0923")

multiplot(STRT_storm1a_06_18_fDOM.p) 
#multiplot(STRT_storm1b_06_20_fDOM.p) # doesnt work
multiplot(STRT_storm1c_06_21_fDOM.p) 
multiplot(STRT_storm1d_06_23_fDOM.p) 
multiplot(STRT_storm1e_06_24_fDOM.p) 
# multiplot(STRT_storm2_07_09_fDOM.p) # doesnt work
multiplot(STRT_storm3_07_20_fDOM.p) 
multiplot(STRT_storm4a_08_01_fDOM.p) 
multiplot(STRT_storm4b_08_03_fDOM.p) 
multiplot(STRT_storm5_08_09_fDOM.p) 
multiplot(STRT_storm6_08_12_fDOM.p) 
multiplot(STRT_storm7a_08_20_fDOM.p) 
#multiplot(STRT_storm7b_08_21_fDOM.p) 
multiplot(STRT_storm8_08_28_fDOM.p) 
multiplot(STRT_storm9a_09_03_fDOM.p)
multiplot(STRT_storm9b_09_06_fDOM.p)
multiplot(STRT_storm9c_09_09_fDOM.p) 
multiplot(STRT_storm10_09_23_fDOM.p) 

# SPC #
STRT_storm1a_06_18_SPC.p = hyst_plot(STRT_storm1a_06_18_Q, STRT_storm1a_06_18_SPC, "STRT", "SPC", "0618a")
STRT_storm1b_06_20_SPC.p = hyst_plot(STRT_storm1b_06_20_Q, STRT_storm1b_06_20_SPC, "STRT", "SPC", "0620b")
STRT_storm1c_06_21_SPC.p = hyst_plot(STRT_storm1c_06_21_Q, STRT_storm1c_06_21_SPC, "STRT", "SPC", "0621c")
STRT_storm1d_06_23_SPC.p = hyst_plot(STRT_storm1d_06_23_Q, STRT_storm1d_06_23_SPC, "STRT", "SPC", "0623d")
STRT_storm1e_06_24_SPC.p = hyst_plot(STRT_storm1e_06_24_Q, STRT_storm1e_06_24_SPC, "STRT", "SPC", "0624e")
STRT_storm2_07_09_SPC.p = hyst_plot(STRT_storm2_07_09_Q, STRT_storm2_07_09_SPC, "STRT", "SPC", "0709")
STRT_storm3_07_20_SPC.p = hyst_plot(STRT_storm3_07_20_Q, STRT_storm3_07_20_SPC, "STRT", "SPC", "0720")
STRT_storm4a_08_01_SPC.p = hyst_plot(STRT_storm4a_08_01_Q, STRT_storm4a_08_01_SPC, "STRT", "SPC", "0801a")
STRT_storm4b_08_03_SPC.p = hyst_plot(STRT_storm4b_08_03_Q, STRT_storm4b_08_03_SPC, "STRT", "SPC", "0803b")
STRT_storm5_08_09_SPC.p = hyst_plot(STRT_storm5_08_09_Q, STRT_storm5_08_09_SPC, "STRT", "SPC", "0809")
STRT_storm6_08_12_SPC.p = hyst_plot(STRT_storm6_08_12_Q, STRT_storm6_08_12_SPC, "STRT", "SPC", "0812")
STRT_storm7a_08_20_SPC.p = hyst_plot(STRT_storm7a_08_20_Q, STRT_storm7a_08_20_SPC, "STRT", "SPC", "0820a")
#STRT_storm7b_08_21_SPC.p = hyst_plot(STRT_storm7b_08_21_Q, STRT_storm7b_08_21_SPC, "STRT", "SPC", "0821b")
STRT_storm8_08_28_SPC.p = hyst_plot(STRT_storm8_08_28_Q, STRT_storm8_08_28_SPC, "STRT", "SPC", "0828")
STRT_storm9a_09_03_SPC.p = hyst_plot(STRT_storm9a_09_03_Q, STRT_storm9a_09_03_SPC, "STRT", "SPC", "0903a")
STRT_storm9b_09_06_SPC.p = hyst_plot(STRT_storm9b_09_06_Q, STRT_storm9b_09_06_SPC, "STRT", "SPC", "0906b")
STRT_storm9c_09_09_SPC.p = hyst_plot(STRT_storm9c_09_09_Q, STRT_storm9c_09_09_SPC, "STRT", "SPC", "0909c")
STRT_storm10_09_23_SPC.p = hyst_plot(STRT_storm10_09_23_Q, STRT_storm10_09_23_SPC, "STRT", "SPC", "0923")

multiplot(STRT_storm1a_06_18_SPC.p) 
multiplot(STRT_storm1b_06_20_SPC.p)
multiplot(STRT_storm1c_06_21_SPC.p) 
multiplot(STRT_storm1d_06_23_SPC.p) 
multiplot(STRT_storm1e_06_24_SPC.p) 
#multiplot(STRT_storm2_07_09_SPC.p)  # doesnt work
multiplot(STRT_storm3_07_20_SPC.p) 
multiplot(STRT_storm4a_08_01_SPC.p) 
multiplot(STRT_storm4b_08_03_SPC.p) 
multiplot(STRT_storm5_08_09_SPC.p) 
multiplot(STRT_storm6_08_12_SPC.p) 
multiplot(STRT_storm7a_08_20_SPC.p) 
#multiplot(STRT_storm7b_08_21_NO3.p) 
multiplot(STRT_storm8_08_28_SPC.p) 
multiplot(STRT_storm9a_09_03_SPC.p)
multiplot(STRT_storm9b_09_06_SPC.p)
multiplot(STRT_storm9c_09_09_SPC.p) 
multiplot(STRT_storm10_09_23_SPC.p) 

# turb
STRT_storm1a_06_18_turb.p = hyst_plot(STRT_storm1a_06_18_Q, STRT_storm1a_06_18_turb, "STRT", "turb", "0618a")
STRT_storm1b_06_20_turb.p = hyst_plot(STRT_storm1b_06_20_Q, STRT_storm1b_06_20_turb, "STRT", "turb", "0620b")
STRT_storm1c_06_21_turb.p = hyst_plot(STRT_storm1c_06_21_Q, STRT_storm1c_06_21_turb, "STRT", "turb", "0621c")
STRT_storm1d_06_23_turb.p = hyst_plot(STRT_storm1d_06_23_Q, STRT_storm1d_06_23_turb, "STRT", "turb", "0623d")
STRT_storm1e_06_24_turb.p = hyst_plot(STRT_storm1e_06_24_Q, STRT_storm1e_06_24_turb, "STRT", "turb", "0624e")
STRT_storm2_07_09_turb.p = hyst_plot(STRT_storm2_07_09_Q, STRT_storm2_07_09_turb, "STRT", "turb", "0709")
STRT_storm3_07_20_turb.p = hyst_plot(STRT_storm3_07_20_Q, STRT_storm3_07_20_turb, "STRT", "turb", "0720")
STRT_storm4a_08_01_turb.p = hyst_plot(STRT_storm4a_08_01_Q, STRT_storm4a_08_01_turb, "STRT", "turb", "0801a")
STRT_storm4b_08_03_turb.p = hyst_plot(STRT_storm4b_08_03_Q, STRT_storm4b_08_03_turb, "STRT", "turb", "0803b")
STRT_storm5_08_09_turb.p = hyst_plot(STRT_storm5_08_09_Q, STRT_storm5_08_09_turb, "STRT", "turb", "0809")
STRT_storm6_08_12_turb.p = hyst_plot(STRT_storm6_08_12_Q, STRT_storm6_08_12_turb, "STRT", "turb", "0812")
STRT_storm7a_08_20_turb.p = hyst_plot(STRT_storm7a_08_20_Q, STRT_storm7a_08_20_turb, "STRT", "turb", "0820a")
#STRT_storm7b_08_21_turb.p = hyst_plot(STRT_storm7b_08_21_Q, STRT_storm7b_08_21_turb, "STRT", "turb", "0821b")
STRT_storm8_08_28_turb.p = hyst_plot(STRT_storm8_08_28_Q, STRT_storm8_08_28_turb, "STRT", "turb", "0828")
STRT_storm9a_09_03_turb.p = hyst_plot(STRT_storm9a_09_03_Q, STRT_storm9a_09_03_turb, "STRT", "turb", "0903a")
STRT_storm9b_09_06_turb.p = hyst_plot(STRT_storm9b_09_06_Q, STRT_storm9b_09_06_turb, "STRT", "turb", "0906b")
STRT_storm9c_09_09_turb.p = hyst_plot(STRT_storm9c_09_09_Q, STRT_storm9c_09_09_turb, "STRT", "turb", "0909c")
STRT_storm10_09_23_turb.p = hyst_plot(STRT_storm10_09_23_Q, STRT_storm10_09_23_turb, "STRT", "turb", "0923")


multiplot(STRT_storm1a_06_18_turb.p) 
multiplot(STRT_storm1b_06_20_turb.p)
multiplot(STRT_storm1c_06_21_turb.p) 
multiplot(STRT_storm1d_06_23_turb.p) 
multiplot(STRT_storm1e_06_24_turb.p) 
multiplot(STRT_storm2_07_09_turb.p) 
multiplot(STRT_storm3_07_20_turb.p) 
multiplot(STRT_storm4a_08_01_turb.p) 
multiplot(STRT_storm4b_08_03_turb.p) 
multiplot(STRT_storm5_08_09_turb.p) 
multiplot(STRT_storm6_08_12_turb.p) 
multiplot(STRT_storm7a_08_20_turb.p) 
#multiplot(STRT_storm7b_08_21_NO3.p) 
multiplot(STRT_storm8_08_28_turb.p) 
multiplot(STRT_storm9a_09_03_turb.p)
multiplot(STRT_storm9b_09_06_turb.p)
multiplot(STRT_storm9c_09_09_turb.p) 
multiplot(STRT_storm10_09_23_turb.p)

# ABS #
STRT_storm1a_06_18_abs.p = hyst_plot(STRT_storm1a_06_18_Q, STRT_storm1a_06_18_abs, "STRT", "abs", "0618a")
STRT_storm1b_06_20_abs.p = hyst_plot(STRT_storm1b_06_20_Q, STRT_storm1b_06_20_abs, "STRT", "abs", "0620b")
STRT_storm1c_06_21_abs.p = hyst_plot(STRT_storm1c_06_21_Q, STRT_storm1c_06_21_abs, "STRT", "abs", "0621c")
STRT_storm1d_06_23_abs.p = hyst_plot(STRT_storm1d_06_23_Q, STRT_storm1d_06_23_abs, "STRT", "abs", "0623d")
STRT_storm1e_06_24_abs.p = hyst_plot(STRT_storm1e_06_24_Q, STRT_storm1e_06_24_abs, "STRT", "abs", "0624e")
STRT_storm2_07_09_abs.p = hyst_plot(STRT_storm2_07_09_Q, STRT_storm2_07_09_abs, "STRT", "abs", "0709")
STRT_storm3_07_20_abs.p = hyst_plot(STRT_storm3_07_20_Q, STRT_storm3_07_20_abs, "STRT", "abs", "0720")
STRT_storm4a_08_01_abs.p = hyst_plot(STRT_storm4a_08_01_Q, STRT_storm4a_08_01_abs, "STRT", "abs", "0801a")
STRT_storm4b_08_03_abs.p = hyst_plot(STRT_storm4b_08_03_Q, STRT_storm4b_08_03_abs, "STRT", "abs", "0803b")
STRT_storm5_08_09_abs.p = hyst_plot(STRT_storm5_08_09_Q, STRT_storm5_08_09_abs, "STRT", "abs", "0809")
STRT_storm6_08_12_abs.p = hyst_plot(STRT_storm6_08_12_Q, STRT_storm6_08_12_abs, "STRT", "abs", "0812")
STRT_storm7a_08_20_abs.p = hyst_plot(STRT_storm7a_08_20_Q, STRT_storm7a_08_20_abs, "STRT", "abs", "0820a")
#STRT_storm7b_08_21_abs.p = hyst_plot(STRT_storm7b_08_21_Q, STRT_storm7b_08_21_abs, "STRT", "abs", "0821b")
STRT_storm8_08_28_abs.p = hyst_plot(STRT_storm8_08_28_Q, STRT_storm8_08_28_abs, "STRT", "abs", "0828")
STRT_storm9a_09_03_abs.p = hyst_plot(STRT_storm9a_09_03_Q, STRT_storm9a_09_03_abs, "STRT", "abs", "0903a")
STRT_storm9b_09_06_abs.p = hyst_plot(STRT_storm9b_09_06_Q, STRT_storm9b_09_06_abs, "STRT", "abs", "0906b")
STRT_storm9c_09_09_abs.p = hyst_plot(STRT_storm9c_09_09_Q, STRT_storm9c_09_09_abs, "STRT", "abs", "0909c")
STRT_storm10_09_23_abs.p = hyst_plot(STRT_storm10_09_23_Q, STRT_storm10_09_23_abs, "STRT", "abs", "0923")

multiplot(STRT_storm1a_06_18_abs.p) 
multiplot(STRT_storm1b_06_20_abs.p)
multiplot(STRT_storm1c_06_21_abs.p) 
multiplot(STRT_storm1d_06_23_abs.p) 
multiplot(STRT_storm1e_06_24_abs.p) 
#multiplot(STRT_storm2_07_09_abs.p)  # doesnt work
multiplot(STRT_storm3_07_20_abs.p) 
multiplot(STRT_storm4a_08_01_abs.p) 
multiplot(STRT_storm4b_08_03_abs.p) 
multiplot(STRT_storm5_08_09_abs.p) 
multiplot(STRT_storm6_08_12_abs.p) 
multiplot(STRT_storm7a_08_20_abs.p) 
#multiplot(STRT_storm7b_08_21_abs.p) 
multiplot(STRT_storm8_08_28_abs.p) 
multiplot(STRT_storm9a_09_03_abs.p)
multiplot(STRT_storm9b_09_06_abs.p)
multiplot(STRT_storm9c_09_09_abs.p) 
multiplot(STRT_storm10_09_23_abs.p) 

# Multiplots of STRT storms #

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


multiplot(STRT_storm1a_06_18_NO3.p, STRT_storm1a_06_18_fDOM.p, STRT_storm1a_06_18_SPC.p, STRT_storm1a_06_18_turb.p,
          STRT_storm1b_06_20_NO3.p, STRT_storm1b_06_20_SPC.p, STRT_storm1b_06_20_turb.p,
          STRT_storm1c_06_21_NO3.p, STRT_storm1c_06_21_fDOM.p, STRT_storm1c_06_21_SPC.p, STRT_storm1c_06_21_turb.p,
          STRT_storm1d_06_23_NO3.p, STRT_storm1d_06_23_fDOM.p, STRT_storm1d_06_23_SPC.p, STRT_storm1d_06_23_turb.p,
          STRT_storm1e_06_24_NO3.p, STRT_storm1e_06_24_fDOM.p, STRT_storm1e_06_24_SPC.p, STRT_storm1e_06_24_turb.p,
          STRT_storm2_07_09_NO3.p, STRT_storm2_07_09_fDOM.p, STRT_storm2_07_09_SPC.p, STRT_storm2_07_09_turb.p,
          STRT_storm3_07_20_NO3.p, STRT_storm3_07_20_fDOM.p, STRT_storm3_07_20_SPC.p, STRT_storm3_07_20_turb.p,
          STRT_storm4a_08_01_NO3.p, STRT_storm4a_08_01_fDOM.p, STRT_storm4a_08_01_SPC.p, STRT_storm4a_08_01_turb.p,
          STRT_storm4b_08_03_NO3.p, STRT_storm4b_08_03_fDOM.p, STRT_storm4b_08_03_SPC.p, STRT_storm4b_08_03_turb.p,
          STRT_storm5_08_09_NO3.p, STRT_storm5_08_09_fDOM.p, STRT_storm5_08_09_SPC.p, STRT_storm5_08_09_turb.p,
          STRT_storm6_08_12_NO3.p, STRT_storm6_08_12_fDOM.p, STRT_storm6_08_12_SPC.p, STRT_storm6_08_12_turb.p,
          STRT_storm7a_08_20_NO3.p, STRT_storm7a_08_20_fDOM.p, STRT_storm7a_08_20_SPC.p, STRT_storm7a_08_20_turb.p,
          STRT_storm8_08_28_NO3.p, STRT_storm8_08_28_fDOM.p, STRT_storm8_08_28_SPC.p, STRT_storm8_08_28_turb.p,
          STRT_storm9a_09_03_NO3.p, STRT_storm9a_09_03_fDOM.p, STRT_storm9a_09_03_SPC.p, STRT_storm9a_09_03_turb.p,
          STRT_storm9b_09_06_NO3.p, STRT_storm9b_09_06_fDOM.p, STRT_storm9b_09_06_SPC.p, STRT_storm9b_09_06_turb.p,
          STRT_storm9c_09_09_NO3.p, STRT_storm9c_09_09_fDOM.p, STRT_storm9c_09_09_SPC.p, STRT_storm9c_09_09_turb.p,
          STRT_storm10_09_23_NO3.p, STRT_storm10_09_23_fDOM.p, STRT_storm10_09_23_SPC.p, STRT_storm10_09_23_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("STRT_HI_Loops_2020.pdf",
       path = here("plots", "HI_plots", "2020", "STRT"),
       width = 20, height = 30, units = "in")


# Make MOOS loops #
MOOS_storm1_06_21_NO3 <- filter(MOOS_storm1_06_21_NO3, datavalue >= 10)
# NO3
MOOS_storm1_06_20_NO3.p = hyst_plot(MOOS_storm1_06_20_Q, MOOS_storm1_06_20_NO3, "MOOS", "NO3", "0620")
MOOS_storm2_06_28_NO3.p = hyst_plot(MOOS_storm2_06_28_Q, MOOS_storm2_06_28_NO3, "MOOS", "NO3", "0628")
MOOS_storm3_07_18_NO3.p = hyst_plot(MOOS_storm3_07_18_Q, MOOS_storm3_07_18_NO3, "MOOS", "NO3", "0718")
MOOS_storm4_07_20_NO3.p = hyst_plot(MOOS_storm4_07_20_Q, MOOS_storm4_07_20_NO3, "MOOS", "NO3", "0720")
MOOS_storm5_07_26_NO3.p = hyst_plot(MOOS_storm5_07_26_Q, MOOS_storm5_07_26_NO3, "MOOS", "NO3", "0726")
MOOS_storm6a_08_01_NO3.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_NO3, "MOOS", "NO3", "0801")
MOOS_storm6b_08_02_NO3.p = hyst_plot(MOOS_storm6b_08_02_Q, MOOS_storm6b_08_02_NO3, "MOOS", "NO3", "0802")
MOOS_storm7a_08_09_NO3.p = hyst_plot(MOOS_storm7a_08_09_Q, MOOS_storm7a_08_09_NO3, "MOOS", "NO3", "0809")
MOOS_storm7b_08_12_NO3.p = hyst_plot(MOOS_storm7b_08_12_Q, MOOS_storm7b_08_12_NO3, "MOOS", "NO3", "0812")
MOOS_storm8_09_06_NO3.p = hyst_plot(MOOS_storm8_09_06_Q, MOOS_storm8_09_06_NO3, "MOOS", "NO3", "0906")
MOOS_storm9_09_09_NO3.p = hyst_plot(MOOS_storm9_09_09_Q, MOOS_storm9_09_09_NO3, "MOOS", "NO3", "0909")
# fDOM #
MOOS_storm1_06_20_fDOM.p = hyst_plot(MOOS_storm1_06_20_Q, MOOS_storm1_06_20_fDOM, "MOOS", "fDOM", "0620")
MOOS_storm2_06_28_fDOM.p = hyst_plot(MOOS_storm2_06_28_Q, MOOS_storm2_06_28_fDOM, "MOOS", "fDOM", "0628")
MOOS_storm3_07_18_fDOM.p = hyst_plot(MOOS_storm3_07_18_Q, MOOS_storm3_07_18_fDOM, "MOOS", "fDOM", "0718")
MOOS_storm4_07_20_fDOM.p = hyst_plot(MOOS_storm4_07_20_Q, MOOS_storm4_07_20_fDOM, "MOOS", "fDOM", "0720")
MOOS_storm5_07_26_fDOM.p = hyst_plot(MOOS_storm5_07_26_Q, MOOS_storm5_07_26_fDOM, "MOOS", "fDOM", "0726")
MOOS_storm6a_08_01_fDOM.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_fDOM, "MOOS", "fDOM", "0801")
MOOS_storm6b_08_02_fDOM.p = hyst_plot(MOOS_storm6b_08_02_Q, MOOS_storm6b_08_02_fDOM, "MOOS", "fDOM", "0802")
MOOS_storm7a_08_09_fDOM.p = hyst_plot(MOOS_storm7a_08_09_Q, MOOS_storm7a_08_09_fDOM, "MOOS", "fDOM", "0809")
MOOS_storm7b_08_12_fDOM.p = hyst_plot(MOOS_storm7b_08_12_Q, MOOS_storm7b_08_12_fDOM, "MOOS", "fDOM", "0812")
MOOS_storm8_09_06_fDOM.p = hyst_plot(MOOS_storm8_09_06_Q, MOOS_storm8_09_06_fDOM, "MOOS", "fDOM", "0906")
MOOS_storm9_09_09_fDOM.p = hyst_plot(MOOS_storm9_09_09_Q, MOOS_storm9_09_09_fDOM, "MOOS", "fDOM", "0909")
# SPC #
MOOS_storm1_06_20_SPC.p = hyst_plot(MOOS_storm1_06_20_Q, MOOS_storm1_06_20_SPC, "MOOS", "SPC", "0620")
MOOS_storm2_06_28_SPC.p = hyst_plot(MOOS_storm2_06_28_Q, MOOS_storm2_06_28_SPC, "MOOS", "SPC", "0628")
MOOS_storm3_07_18_SPC.p = hyst_plot(MOOS_storm3_07_18_Q, MOOS_storm3_07_18_SPC, "MOOS", "SPC", "0718")
MOOS_storm4_07_20_SPC.p = hyst_plot(MOOS_storm4_07_20_Q, MOOS_storm4_07_20_SPC, "MOOS", "SPC", "0720")
MOOS_storm5_07_26_SPC.p = hyst_plot(MOOS_storm5_07_26_Q, MOOS_storm5_07_26_SPC, "MOOS", "SPC", "0726")
MOOS_storm6a_08_01_SPC.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_SPC, "MOOS", "SPC", "0801")
MOOS_storm6b_08_02_SPC.p = hyst_plot(MOOS_storm6b_08_02_Q, MOOS_storm6b_08_02_SPC, "MOOS", "SPC", "0802")
MOOS_storm7a_08_09_SPC.p = hyst_plot(MOOS_storm7a_08_09_Q, MOOS_storm7a_08_09_SPC, "MOOS", "SPC", "0809")
MOOS_storm7b_08_12_SPC.p = hyst_plot(MOOS_storm7b_08_12_Q, MOOS_storm7b_08_12_SPC, "MOOS", "SPC", "0812")
MOOS_storm8_09_06_SPC.p = hyst_plot(MOOS_storm8_09_06_Q, MOOS_storm8_09_06_SPC, "MOOS", "SPC", "0906")
MOOS_storm9_09_09_SPC.p = hyst_plot(MOOS_storm9_09_09_Q, MOOS_storm9_09_09_SPC, "MOOS", "SPC", "0909")
# turb #
MOOS_storm1_06_20_turb.p = hyst_plot(MOOS_storm1_06_20_Q, MOOS_storm1_06_20_turb, "MOOS", "turb", "0620")
MOOS_storm2_06_28_turb.p = hyst_plot(MOOS_storm2_06_28_Q, MOOS_storm2_06_28_turb, "MOOS", "turb", "0628")
MOOS_storm3_07_18_turb.p = hyst_plot(MOOS_storm3_07_18_Q, MOOS_storm3_07_18_turb, "MOOS", "turb", "0718")
MOOS_storm4_07_20_turb.p = hyst_plot(MOOS_storm4_07_20_Q, MOOS_storm4_07_20_turb, "MOOS", "turb", "0720")
MOOS_storm5_07_26_turb.p = hyst_plot(MOOS_storm5_07_26_Q, MOOS_storm5_07_26_turb, "MOOS", "turb", "0726")
MOOS_storm6a_08_01_turb.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_turb, "MOOS", "turb", "0801")
MOOS_storm6b_08_02_turb.p = hyst_plot(MOOS_storm6b_08_02_Q, MOOS_storm6b_08_02_turb, "MOOS", "turb", "0802")
MOOS_storm7a_08_09_turb.p = hyst_plot(MOOS_storm7a_08_09_Q, MOOS_storm7a_08_09_turb, "MOOS", "turb", "0809")
MOOS_storm7b_08_12_turb.p = hyst_plot(MOOS_storm7b_08_12_Q, MOOS_storm7b_08_12_turb, "MOOS", "turb", "0812")
MOOS_storm8_09_06_turb.p = hyst_plot(MOOS_storm8_09_06_Q, MOOS_storm8_09_06_turb, "MOOS", "turb", "0906")
MOOS_storm9_09_09_turb.p = hyst_plot(MOOS_storm9_09_09_Q, MOOS_storm9_09_09_turb, "MOOS", "turb", "0909")

# ABS #
MOOS_storm1_06_20_abs.p = hyst_plot(MOOS_storm1_06_20_Q, MOOS_storm1_06_20_abs, "MOOS", "abs", "0620")
MOOS_storm2_06_28_abs.p = hyst_plot(MOOS_storm2_06_28_Q, MOOS_storm2_06_28_abs, "MOOS", "abs", "0628")
MOOS_storm3_07_18_abs.p = hyst_plot(MOOS_storm3_07_18_Q, MOOS_storm3_07_18_abs, "MOOS", "abs", "0718")
MOOS_storm4_07_20_abs.p = hyst_plot(MOOS_storm4_07_20_Q, MOOS_storm4_07_20_abs, "MOOS", "abs", "0720")
MOOS_storm5_07_26_abs.p = hyst_plot(MOOS_storm5_07_26_Q, MOOS_storm5_07_26_abs, "MOOS", "abs", "0726")
MOOS_storm6a_08_01_abs.p = hyst_plot(MOOS_storm6a_08_01_Q, MOOS_storm6a_08_01_abs, "MOOS", "abs", "0801")
MOOS_storm6b_08_02_abs.p = hyst_plot(MOOS_storm6b_08_02_Q, MOOS_storm6b_08_02_abs, "MOOS", "abs", "0802")
MOOS_storm7a_08_09_abs.p = hyst_plot(MOOS_storm7a_08_09_Q, MOOS_storm7a_08_09_abs, "MOOS", "abs", "0809")
MOOS_storm7b_08_12_abs.p = hyst_plot(MOOS_storm7b_08_12_Q, MOOS_storm7b_08_12_abs, "MOOS", "abs", "0812")
MOOS_storm8_09_06_abs.p = hyst_plot(MOOS_storm8_09_06_Q, MOOS_storm8_09_06_abs, "MOOS", "abs", "0906")
MOOS_storm9_09_09_abs.p = hyst_plot(MOOS_storm9_09_09_Q, MOOS_storm9_09_09_abs, "MOOS", "abs", "0909")


#### Multiplots of MOOS storms ####

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
# Plot MOOS loops #
multiplot(MOOS_storm1_06_20_NO3.p, MOOS_storm1_06_20_fDOM.p) # need to clean values that are below 10 but if i do clear it then i get the error code so i gotta figure that out 
multiplot(MOOS_storm2_06_28_NO3.p, MOOS_storm2_06_28_fDOM.p) # works
multiplot(MOOS_storm3_07_18_NO3.p, MOOS_storm3_07_18_fDOM.p) # works
multiplot(MOOS_storm4_07_20_NO3.p, MOOS_storm4_07_20_fDOM.p) # works 
multiplot(MOOS_storm5_07_26_NO3.p, MOOS_storm5_07_26_fDOM.p) # works 
multiplot(MOOS_storm6a_08_01_NO3.p, MOOS_storm6a_08_01_fDOM.p) # works
multiplot(MOOS_storm6b_08_02_NO3.p, MOOS_storm6b_08_02_fDOM.p) # works but needs to be cleaned for values below 10
multiplot(MOOS_storm7a_08_09_NO3.p, MOOS_storm7a_08_09_fDOM.p) # works 
multiplot(MOOS_storm7b_08_12_NO3.p, MOOS_storm7b_08_12_fDOM.p) # works 
multiplot(MOOS_storm8_09_06_NO3.p, MOOS_storm8_09_06_fDOM.p) # works
multiplot(MOOS_storm9_09_09_NO3.p, MOOS_storm9_09_09_fDOM.p) # works better for fDOM

multiplot(MOOS_storm1_06_20_SPC.p, MOOS_storm1_06_20_turb.p) # need to clean values that are below 10 but if i do clear it then i get the error code so i gotta figure that out 
multiplot(MOOS_storm2_06_28_SPC.p, MOOS_storm2_06_28_turb.p) # works
multiplot(MOOS_storm3_07_18_SPC.p, MOOS_storm3_07_18_turb.p) # works
multiplot(MOOS_storm4_07_20_SPC.p, MOOS_storm4_07_20_turb.p) # works 
multiplot(MOOS_storm5_07_26_SPC.p, MOOS_storm5_07_26_turb.p) # works 
multiplot(MOOS_storm6a_08_01_SPC.p, MOOS_storm6a_08_01_turb.p) # works
multiplot(MOOS_storm6b_08_02_SPC.p, MOOS_storm6b_08_02_turb.p) # works but needs to be cleaned for values below 10
multiplot(MOOS_storm7a_08_09_SPC.p, MOOS_storm7a_08_09_turb.p) # works 
multiplot(MOOS_storm7b_08_12_SPC.p, MOOS_storm7b_08_12_turb.p) # works 
multiplot(MOOS_storm8_09_06_SPC.p, MOOS_storm8_09_06_turb.p) # works
multiplot(MOOS_storm9_09_09_SPC.p, MOOS_storm9_09_09_turb.p) # works


multiplot(MOOS_storm1_06_20_NO3.p, MOOS_storm1_06_20_fDOM.p, MOOS_storm1_06_20_SPC.p, MOOS_storm1_06_20_turb.p,
          MOOS_storm2_06_28_NO3.p, MOOS_storm2_06_28_fDOM.p, MOOS_storm2_06_28_SPC.p, MOOS_storm2_06_28_turb.p,
          MOOS_storm3_07_18_NO3.p, MOOS_storm3_07_18_fDOM.p, MOOS_storm3_07_18_SPC.p, MOOS_storm3_07_18_turb.p,
          MOOS_storm4_07_20_NO3.p, MOOS_storm4_07_20_fDOM.p, MOOS_storm4_07_20_SPC.p, MOOS_storm4_07_20_turb.p,
          MOOS_storm5_07_26_NO3.p, MOOS_storm5_07_26_fDOM.p, MOOS_storm5_07_26_SPC.p, MOOS_storm5_07_26_turb.p,
          MOOS_storm6a_08_01_NO3.p, MOOS_storm6a_08_01_fDOM.p, MOOS_storm6a_08_01_SPC.p, MOOS_storm6a_08_01_turb.p,
          MOOS_storm6b_08_02_NO3.p, MOOS_storm6b_08_02_fDOM.p,MOOS_storm6b_08_02_SPC.p, MOOS_storm6b_08_02_turb.p,
          MOOS_storm7a_08_09_NO3.p, MOOS_storm7a_08_09_fDOM.p, MOOS_storm7a_08_09_SPC.p, MOOS_storm7a_08_09_turb.p,
          MOOS_storm7b_08_12_NO3.p, MOOS_storm7b_08_12_fDOM.p, MOOS_storm7b_08_12_SPC.p, MOOS_storm7b_08_12_turb.p,
          MOOS_storm8_09_06_NO3.p, MOOS_storm8_09_06_fDOM.p, MOOS_storm8_09_06_SPC.p, MOOS_storm8_09_06_turb.p,
          MOOS_storm9_09_09_NO3.p, MOOS_storm9_09_09_fDOM.p, MOOS_storm9_09_09_SPC.p, MOOS_storm9_09_09_turb.p,
          cols = 7) # works

# export pdf 20 x 30 #
ggsave("MOOS_HI_Loops_2020.pdf",
       path = here("plots", "HI_plots", "2020", "MOOS"),
       width = 20, height = 30, units = "in")


#### Make FRCH loops ####

FRCH_storm1_06_13_NO3.p = hyst_plot(FRCH_storm1_06_13_Q, FRCH_storm1_06_13_NO3, "FRCH", "NO3", "0613")
FRCH_storm2_06_18_NO3.p = hyst_plot(FRCH_storm2_06_18_Q, FRCH_storm2_06_18_NO3, "FRCH", "NO3", "0618")
FRCH_storm3a_06_20_NO3.p = hyst_plot(FRCH_storm3a_06_20_Q, FRCH_storm3a_06_20_NO3, "FRCH", "NO3", "0620a")
FRCH_storm3b_06_21_NO3.p = hyst_plot(FRCH_storm3b_06_21_Q, FRCH_storm3b_06_21_NO3, "FRCH", "NO3", "0621b")
FRCH_storm3c_06_26_NO3.p = hyst_plot(FRCH_storm3c_06_26_Q, FRCH_storm3c_06_26_NO3, "FRCH", "NO3", "0626c")
FRCH_storm4a_07_07_NO3.p = hyst_plot(FRCH_storm4a_07_07_Q, FRCH_storm4a_07_07_NO3, "FRCH", "NO3", "0707a")
FRCH_storm4b_07_09_NO3.p = hyst_plot(FRCH_storm4b_07_09_Q, FRCH_storm4b_07_09_NO3, "FRCH", "NO3", "0709b")
FRCH_storm5_07_15_NO3.p = hyst_plot(FRCH_storm5_07_15_Q, FRCH_storm5_07_15_NO3, "FRCH", "NO3", "0715")
FRCH_storm6_07_18_NO3.p = hyst_plot(FRCH_storm6_07_18_Q, FRCH_storm6_07_18_NO3, "FRCH", "NO3", "0718")
FRCH_storm7_07_20_NO3.p = hyst_plot(FRCH_storm7_07_20_Q, FRCH_storm7_07_20_NO3, "FRCH", "NO3", "0720")
FRCH_storm8_07_26_NO3.p = hyst_plot(FRCH_storm8_07_26_Q, FRCH_storm8_07_26_NO3, "FRCH", "NO3", "0726")
FRCH_storm9a_08_01_NO3.p = hyst_plot(FRCH_storm9a_08_01_Q, FRCH_storm9a_08_01_NO3, "FRCH", "NO3", "0801a")
FRCH_storm9b_08_02_NO3.p = hyst_plot(FRCH_storm9b_08_02_Q, FRCH_storm9b_08_023_NO3, "FRCH", "NO3", "0802b")
FRCH_storm10a_08_09_NO3.p = hyst_plot(FRCH_storm10a_08_09_Q, FRCH_storm10a_08_09_NO3, "FRCH", "NO3", "0809a")
FRCH_storm10b_08_12_NO3.p = hyst_plot(FRCH_storm10b_08_12_Q, FRCH_storm10b_08_12_NO3, "FRCH", "NO3", "0812b")
FRCH_storm11_08_20_NO3.p = hyst_plot(FRCH_storm11_08_20_Q, FRCH_storm11_08_20_NO3, "FRCH", "NO3", "0820")
FRCH_storm12_09_06_NO3.p = hyst_plot(FRCH_storm12_09_06_Q, FRCH_storm12_09_06_NO3, "FRCH", "NO3", "0906")


FRCH_storm1_06_13_fDOM.p = hyst_plot(FRCH_storm1_06_13_Q, FRCH_storm1_06_13_fDOM, "FRCH", "fDOM", "0613")
FRCH_storm2_06_18_fDOM.p = hyst_plot(FRCH_storm2_06_18_Q, FRCH_storm2_06_18_fDOM, "FRCH", "fDOM", "0618")
FRCH_storm3a_06_20_fDOM.p = hyst_plot(FRCH_storm3a_06_20_Q, FRCH_storm3a_06_20_fDOM, "FRCH", "fDOM", "0620a")
FRCH_storm3b_06_21_fDOM.p = hyst_plot(FRCH_storm3b_06_21_Q, FRCH_storm3b_06_21_fDOM, "FRCH", "fDOM", "0621b")
FRCH_storm3c_06_26_fDOM.p = hyst_plot(FRCH_storm3c_06_26_Q, FRCH_storm3c_06_26_fDOM, "FRCH", "fDOM", "0626c")
FRCH_storm4a_07_07_fDOM.p = hyst_plot(FRCH_storm4a_07_07_Q, FRCH_storm4a_07_07_fDOM, "FRCH", "fDOM", "0707a")
FRCH_storm4b_07_09_fDOM.p = hyst_plot(FRCH_storm4b_07_09_Q, FRCH_storm4b_07_09_fDOM, "FRCH", "fDOM", "0709b")
FRCH_storm5_07_15_fDOM.p = hyst_plot(FRCH_storm5_07_15_Q, FRCH_storm5_07_15_fDOM, "FRCH", "fDOM", "0715")
FRCH_storm6_07_18_fDOM.p = hyst_plot(FRCH_storm6_07_18_Q, FRCH_storm6_07_18_fDOM, "FRCH", "fDOM", "0718")
FRCH_storm7_07_20_fDOM.p = hyst_plot(FRCH_storm7_07_20_Q, FRCH_storm7_07_20_fDOM, "FRCH", "fDOM", "0720")
FRCH_storm8_07_26_fDOM.p = hyst_plot(FRCH_storm8_07_26_Q, FRCH_storm8_07_26_fDOM, "FRCH", "fDOM", "0726")
FRCH_storm9a_08_01_fDOM.p = hyst_plot(FRCH_storm9a_08_01_Q, FRCH_storm9a_08_01_fDOM, "FRCH", "fDOM", "0801a")
FRCH_storm9b_08_02_fDOM.p = hyst_plot(FRCH_storm9b_08_02_Q, FRCH_storm9b_08_02_fDOM, "FRCH", "fDOM", "0802b")
FRCH_storm10a_08_09_fDOM.p = hyst_plot(FRCH_storm10a_08_09_Q, FRCH_storm10a_08_09_fDOM, "FRCH", "fDOM", "0809a")
FRCH_storm10b_08_12_fDOM.p = hyst_plot(FRCH_storm10b_08_12_Q, FRCH_storm10b_08_12_fDOM, "FRCH", "fDOM", "0812b")
FRCH_storm11_08_20_fDOM.p = hyst_plot(FRCH_storm11_08_20_Q, FRCH_storm11_08_20_fDOM, "FRCH", "fDOM", "0820")
FRCH_storm12_09_06_fDOM.p = hyst_plot(FRCH_storm12_09_06_Q, FRCH_storm11_09_06_fDOM, "FRCH", "fDOM", "0906")

FRCH_storm1_06_13_SPC.p = hyst_plot(FRCH_storm1_06_13_Q, FRCH_storm1_06_13_SPC, "FRCH", "SPC", "0613")
FRCH_storm2_06_18_SPC.p = hyst_plot(FRCH_storm2_06_18_Q, FRCH_storm2_06_18_SPC, "FRCH", "SPC", "0618")
FRCH_storm3a_06_20_SPC.p = hyst_plot(FRCH_storm3a_06_20_Q, FRCH_storm3a_06_20_SPC, "FRCH", "SPC", "0620a")
FRCH_storm3b_06_21_SPC.p = hyst_plot(FRCH_storm3b_06_21_Q, FRCH_storm3b_06_21_SPC, "FRCH", "SPC", "0621b")
FRCH_storm3c_06_26_SPC.p = hyst_plot(FRCH_storm3c_06_26_Q, FRCH_storm3c_06_26_SPC, "FRCH", "SPC", "0626c")
FRCH_storm4a_07_07_SPC.p = hyst_plot(FRCH_storm4a_07_07_Q, FRCH_storm4a_07_07_SPC, "FRCH", "SPC", "0707a")
FRCH_storm4b_07_09_SPC.p = hyst_plot(FRCH_storm4b_07_09_Q, FRCH_storm4b_07_09_SPC, "FRCH", "SPC", "0709b")
FRCH_storm5_07_15_SPC.p = hyst_plot(FRCH_storm5_07_15_Q, FRCH_storm5_07_15_SPC, "FRCH", "SPC", "0715")
FRCH_storm6_07_18_SPC.p = hyst_plot(FRCH_storm6_07_18_Q, FRCH_storm6_07_18_SPC, "FRCH", "SPC", "0718")
FRCH_storm7_07_20_SPC.p = hyst_plot(FRCH_storm7_07_20_Q, FRCH_storm7_07_20_SPC, "FRCH", "SPC", "0720")
FRCH_storm8_07_26_SPC.p = hyst_plot(FRCH_storm8_07_26_Q, FRCH_storm8_07_26_SPC, "FRCH", "SPC", "0726")
FRCH_storm9a_08_01_SPC.p = hyst_plot(FRCH_storm9a_08_01_Q, FRCH_storm9a_08_01_SPC, "FRCH", "SPC", "0801a")
FRCH_storm9b_08_02_SPC.p = hyst_plot(FRCH_storm9b_08_02_Q, FRCH_storm9b_08_023_SPC, "FRCH", "SPC", "0802b")
FRCH_storm10a_08_09_SPC.p = hyst_plot(FRCH_storm10a_08_09_Q, FRCH_storm10a_08_09_SPC, "FRCH", "SPC", "0809a")
FRCH_storm10b_08_12_SPC.p = hyst_plot(FRCH_storm10b_08_12_Q, FRCH_storm10b_08_12_SPC, "FRCH", "SPC", "0812b")
FRCH_storm11_08_20_SPC.p = hyst_plot(FRCH_storm11_08_20_Q, FRCH_storm11_08_20_SPC, "FRCH", "SPC", "0820")
FRCH_storm12_09_06_SPC.p = hyst_plot(FRCH_storm12_09_06_Q, FRCH_storm12_09_06_SPC, "FRCH", "SPC", "0906")

FRCH_storm1_06_13_turb.p = hyst_plot(FRCH_storm1_06_13_Q, FRCH_storm1_06_13_turb, "FRCH", "turb", "0613")
FRCH_storm2_06_18_turb.p = hyst_plot(FRCH_storm2_06_18_Q, FRCH_storm2_06_18_turb, "FRCH", "turb", "0618")
FRCH_storm3a_06_20_turb.p = hyst_plot(FRCH_storm3a_06_20_Q, FRCH_storm3a_06_20_turb, "FRCH", "turb", "0620a")
FRCH_storm3b_06_21_turb.p = hyst_plot(FRCH_storm3b_06_21_Q, FRCH_storm3b_06_21_turb, "FRCH", "turb", "0621b")
FRCH_storm3c_06_26_turb.p = hyst_plot(FRCH_storm3c_06_26_Q, FRCH_storm3c_06_26_turb, "FRCH", "turb", "0626c")
FRCH_storm4a_07_07_turb.p = hyst_plot(FRCH_storm4a_07_07_Q, FRCH_storm4a_07_07_turb, "FRCH", "turb", "0707a")
FRCH_storm4b_07_09_turb.p = hyst_plot(FRCH_storm4b_07_09_Q, FRCH_storm4b_07_09_turb, "FRCH", "turb", "0709b")
FRCH_storm5_07_15_turb.p = hyst_plot(FRCH_storm5_07_15_Q, FRCH_storm5_07_15_turb, "FRCH", "turb", "0715")
FRCH_storm6_07_18_turb.p = hyst_plot(FRCH_storm6_07_18_Q, FRCH_storm6_07_18_turb, "FRCH", "turb", "0718")
FRCH_storm7_07_20_turb.p = hyst_plot(FRCH_storm7_07_20_Q, FRCH_storm7_07_20_turb, "FRCH", "turb", "0720")
FRCH_storm8_07_26_turb.p = hyst_plot(FRCH_storm8_07_26_Q, FRCH_storm8_07_26_turb, "FRCH", "turb", "0726")
FRCH_storm9a_08_01_turb.p = hyst_plot(FRCH_storm9a_08_01_Q, FRCH_storm9a_08_01_turb, "FRCH", "turb", "0801a")
FRCH_storm9b_08_02_turb.p = hyst_plot(FRCH_storm9b_08_02_Q, FRCH_storm9b_08_023_turb, "FRCH", "turb", "0802b")
FRCH_storm10a_08_09_turb.p = hyst_plot(FRCH_storm10a_08_09_Q, FRCH_storm10a_08_09_turb, "FRCH", "turb", "0809a")
FRCH_storm10b_08_12_turb.p = hyst_plot(FRCH_storm10b_08_12_Q, FRCH_storm10b_08_12_turb, "FRCH", "turb", "0812b")
FRCH_storm11_08_20_turb.p = hyst_plot(FRCH_storm11_08_20_Q, FRCH_storm11_08_20_turb, "FRCH", "turb", "0820")
FRCH_storm12_09_06_turb.p = hyst_plot(FRCH_storm12_09_06_Q, FRCH_storm12_09_06_turb, "FRCH", "turb", "0906")

FRCH_storm1_06_13_abs.p = hyst_plot(FRCH_storm1_06_13_Q, FRCH_storm1_06_13_abs, "FRCH", "abs", "0613")
FRCH_storm2_06_18_abs.p = hyst_plot(FRCH_storm2_06_18_Q, FRCH_storm2_06_18_abs, "FRCH", "abs", "0618")
FRCH_storm3a_06_20_abs.p = hyst_plot(FRCH_storm3a_06_20_Q, FRCH_storm3a_06_20_abs, "FRCH", "abs", "0620a")
FRCH_storm3b_06_21_abs.p = hyst_plot(FRCH_storm3b_06_21_Q, FRCH_storm3b_06_21_abs, "FRCH", "abs", "0621b")
FRCH_storm3c_06_26_abs.p = hyst_plot(FRCH_storm3c_06_26_Q, FRCH_storm3c_06_26_abs, "FRCH", "abs", "0626c")
FRCH_storm4a_07_07_abs.p = hyst_plot(FRCH_storm4a_07_07_Q, FRCH_storm4a_07_07_abs, "FRCH", "abs", "0707a")
FRCH_storm4b_07_09_abs.p = hyst_plot(FRCH_storm4b_07_09_Q, FRCH_storm4b_07_09_abs, "FRCH", "abs", "0709b")
FRCH_storm5_07_15_abs.p = hyst_plot(FRCH_storm5_07_15_Q, FRCH_storm5_07_15_abs, "FRCH", "abs", "0715")
FRCH_storm6_07_18_abs.p = hyst_plot(FRCH_storm6_07_18_Q, FRCH_storm6_07_18_abs, "FRCH", "abs", "0718")
FRCH_storm7_07_20_abs.p = hyst_plot(FRCH_storm7_07_20_Q, FRCH_storm7_07_20_abs, "FRCH", "abs", "0720")
FRCH_storm8_07_26_abs.p = hyst_plot(FRCH_storm8_07_26_Q, FRCH_storm8_07_26_abs, "FRCH", "abs", "0726")
FRCH_storm9a_08_01_abs.p = hyst_plot(FRCH_storm9a_08_01_Q, FRCH_storm9a_08_01_abs, "FRCH", "abs", "0801a")
FRCH_storm9b_08_02_abs.p = hyst_plot(FRCH_storm9b_08_02_Q, FRCH_storm9b_08_023_abs, "FRCH", "abs", "0802b")
FRCH_storm10a_08_09_abs.p = hyst_plot(FRCH_storm10a_08_09_Q, FRCH_storm10a_08_09_abs, "FRCH", "abs", "0809a")
FRCH_storm10b_08_12_abs.p = hyst_plot(FRCH_storm10b_08_12_Q, FRCH_storm10b_08_12_abs, "FRCH", "abs", "0812b")
FRCH_storm11_08_20_abs.p = hyst_plot(FRCH_storm11_08_20_Q, FRCH_storm11_08_20_abs, "FRCH", "abs", "0820")
FRCH_storm12_09_06_abs.p = hyst_plot(FRCH_storm12_09_06_Q, FRCH_storm12_09_06_abs, "FRCH", "abs", "0906")


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
# Plot FRCH Loops #

multiplot(FRCH_storm1_06_13_NO3.p, FRCH_storm1_06_13_fDOM.p) # clean "0" points
multiplot(FRCH_storm2_06_18_NO3.p, FRCH_storm2_06_18_fDOM.p) # works
multiplot(FRCH_storm3a_06_20_NO3.p, FRCH_storm3a_06_20_fDOM.p) # works
multiplot(FRCH_storm3b_06_22_NO3.p, FRCH_storm3b_06_22_fDOM.p) # works 
multiplot(FRCH_storm3c_06_28_NO3.p, FRCH_storm3c_06_28_fDOM.p) # works 
multiplot(FRCH_storm4a_07_07_NO3.p, FRCH_storm4a_07_07_fDOM.p) # works
multiplot(FRCH_storm4b_07_09_NO3.p, FRCH_storm4b_07_09_fDOM.p) # works 
multiplot(FRCH_storm5_07_16_NO3.p, FRCH_storm5_07_16_fDOM.p) # works 
multiplot(FRCH_storm6_07_17_NO3.p, FRCH_storm6_07_17_fDOM.p) # works 
multiplot(FRCH_storm7_07_20_NO3.p, FRCH_storm7_07_20_fDOM.p) # works
multiplot(FRCH_storm8_07_26_NO3.p, FRCH_storm8_07_26_fDOM.p) # works 
multiplot(FRCH_storm9a_08_01_NO3.p, FRCH_storm9a_08_01_fDOM.p) # works 
multiplot(FRCH_storm9b_08_02_NO3.p, FRCH_storm9b_08_02_fDOM.p) # Does not work
multiplot(FRCH_storm10a_08_09_NO3.p, FRCH_storm10a_08_09_fDOM.p) # works 
multiplot(FRCH_storm10b_08_12_NO3.p, FRCH_storm10b_08_12_fDOM.p) # works 
multiplot(FRCH_storm11_08_20_NO3.p, FRCH_storm11_08_20_fDOM.p) # works 
multiplot(FRCH_storm12_09_06_NO3.p, FRCH_storm12_09_06_fDOM.p) # No3 works 

multiplot(FRCH_storm1_06_13_SPC.p, FRCH_storm1_06_13_turb.p) # clean "0" points
multiplot(FRCH_storm2_06_18_SPC.p, FRCH_storm2_06_18_turb.p) # works
multiplot(FRCH_storm3a_06_20_SPC.p, FRCH_storm3a_06_20_turb.p) # works
multiplot(FRCH_storm3b_06_21_SPC.p, FRCH_storm3b_06_21_turb.p) # works 
multiplot(FRCH_storm3c_06_26_SPC.p, FRCH_storm3c_06_26_turb.p) # works 
multiplot(FRCH_storm4a_07_07_SPC.p, FRCH_storm4a_07_07_turb.p) # works
multiplot(FRCH_storm4b_07_09_SPC.p, FRCH_storm4b_07_09_turb.p) # works 
multiplot(FRCH_storm5_07_15_SPC.p, FRCH_storm5_07_15_turb.p) # works 
multiplot(FRCH_storm6_07_18_SPC.p, FRCH_storm6_07_18_turb.p) # works 
multiplot(FRCH_storm7_07_20_SPC.p, FRCH_storm7_07_20_turb.p) # works
multiplot(FRCH_storm8_07_26_SPC.p, FRCH_storm8_07_26_turb.p) # works 
multiplot(FRCH_storm9a_08_01_SPC.p, FRCH_storm9a_08_01_turb.p) # works 
#multiplot(FRCH_storm9b_08_02_SPC.p, FRCH_storm9b_08_02_turb.p) # Does not work
multiplot(FRCH_storm10a_08_09_SPC.p, FRCH_storm10a_08_09_turb.p) # works 
multiplot(FRCH_storm10b_08_12_SPC.p, FRCH_storm10b_08_12_turb.p) # works 
multiplot(FRCH_storm11_08_20_SPC.p, FRCH_storm11_08_20_turb.p) # works 
multiplot(FRCH_storm12_09_06_SPC.p, FRCH_storm12_09_06_turb.p) # works


multiplot(FRCH_storm1_06_13_NO3.p, FRCH_storm1_06_13_fDOM.p, FRCH_storm1_06_13_SPC.p, FRCH_storm1_06_13_turb.p,
          FRCH_storm2_06_18_NO3.p, FRCH_storm2_06_18_fDOM.p, FRCH_storm2_06_18_SPC.p, FRCH_storm2_06_18_turb.p,
          FRCH_storm3a_06_20_NO3.p, FRCH_storm3a_06_20_fDOM.p, FRCH_storm3a_06_20_SPC.p, FRCH_storm3a_06_20_turb.p,
          FRCH_storm3b_06_21_NO3.p, FRCH_storm3b_06_21_fDOM.p, FRCH_storm3b_06_21_SPC.p, FRCH_storm3b_06_21_turb.p,
          FRCH_storm3c_06_26_NO3.p, FRCH_storm3c_06_26_fDOM.p, FRCH_storm3c_06_26_SPC.p, FRCH_storm3c_06_26_turb.p,
          FRCH_storm4a_07_07_NO3.p, FRCH_storm4a_07_07_fDOM.p, FRCH_storm4a_07_07_SPC.p, FRCH_storm4a_07_07_turb.p,
          FRCH_storm4b_07_09_NO3.p, FRCH_storm4b_07_09_fDOM.p, FRCH_storm4b_07_09_SPC.p, FRCH_storm4b_07_09_turb.p,
          FRCH_storm5_07_15_NO3.p, FRCH_storm5_07_15_fDOM.p, FRCH_storm5_07_15_SPC.p, FRCH_storm5_07_15_turb.p,
          FRCH_storm6_07_18_NO3.p, FRCH_storm6_07_18_fDOM.p,FRCH_storm6_07_18_SPC.p, FRCH_storm6_07_18_turb.p,
          FRCH_storm7_07_20_NO3.p, FRCH_storm7_07_20_fDOM.p, FRCH_storm7_07_20_SPC.p, FRCH_storm7_07_20_turb.p,
          FRCH_storm8_07_26_NO3.p, FRCH_storm8_07_26_fDOM.p, FRCH_storm8_07_26_SPC.p, FRCH_storm8_07_26_turb.p,
          FRCH_storm9a_08_01_NO3.p, FRCH_storm9a_08_01_fDOM.p, FRCH_storm9a_08_01_SPC.p, FRCH_storm9a_08_01_turb.p,
          FRCH_storm10a_08_09_NO3.p, FRCH_storm10a_08_09_fDOM.p,FRCH_storm10a_08_09_SPC.p, FRCH_storm10a_08_09_turb.p,
          FRCH_storm10b_08_12_NO3.p, FRCH_storm10b_08_12_fDOM.p, FRCH_storm10b_08_12_SPC.p, FRCH_storm10b_08_12_turb.p,
          FRCH_storm11_08_20_NO3.p, FRCH_storm11_08_20_fDOM.p, FRCH_storm11_08_20_SPC.p, FRCH_storm11_08_20_turb.p,
          FRCH_storm12_09_06_NO3.p, FRCH_storm12_09_06_SPC.p, FRCH_storm12_09_06_turb.p,
          cols = 7) # works

# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2020.pdf",
       path = here("plots", "HI_plots", "2020", "FRCH"),
       width = 20, height = 30, units = "in")


#### Make POKE loops ####

POKE_storm1_06_09_NO3.p = hyst_plot(POKE_storm1_06_09_Q, POKE_storm1_06_09_NO3, "POKE", "NO3", "0609")
POKE_storm2_06_12_NO3.p = hyst_plot(POKE_storm2_06_12_Q, POKE_storm2_06_12_NO3, "POKE", "NO3", "0612")
POKE_storm3_06_15_NO3.p = hyst_plot(POKE_storm3_06_15_Q, POKE_storm3_06_15_NO3, "POKE", "NO3", "0615")
POKE_storm4a_06_19_NO3.p = hyst_plot(POKE_storm4a_06_19_Q, POKE_storm4a_06_19_NO3, "POKE", "NO3", "0619a")
POKE_storm4b_06_20_NO3.p = hyst_plot(POKE_storm4b_06_20_Q, POKE_storm4b_06_20_NO3, "POKE", "NO3", "0620b")
POKE_storm4c_06_21_NO3.p = hyst_plot(POKE_storm4c_06_21_Q, POKE_storm4c_06_21_NO3, "POKE", "NO3", "0621c")
POKE_storm5_06_22_NO3.p = hyst_plot(POKE_storm5_06_22_Q, POKE_storm5_06_22_NO3, "POKE", "NO3", "0622")
POKE_storm6_06_29_NO3.p = hyst_plot(POKE_storm6_06_29_Q, POKE_storm6_06_29_NO3, "POKE", "NO3", "0629")
POKE_storm7_07_04_NO3.p = hyst_plot(POKE_storm7_07_04_Q, POKE_storm7_07_04_NO3, "POKE", "NO3", "0704")
POKE_storm8_07_09_NO3.p = hyst_plot(POKE_storm8_07_09_Q, POKE_storm8_07_09_NO3, "POKE", "NO3", "0709")
POKE_storm9_07_12_NO3.p = hyst_plot(POKE_storm9_07_12_Q, POKE_storm9_07_12_NO3, "POKE", "NO3", "0712")
POKE_storm10_07_16_NO3.p = hyst_plot(POKE_storm10_07_16_Q, POKE_storm10_07_16_NO3, "POKE", "NO3", "0716")
POKE_storm11_07_18_NO3.p = hyst_plot(POKE_storm11_07_18_Q, POKE_storm11_07_18_NO3, "POKE", "NO3", "0718")
POKE_storm12_07_20_NO3.p = hyst_plot(POKE_storm12_07_20_Q, POKE_storm12_07_20_NO3, "POKE", "NO3", "0720")
POKE_storm13_07_24_NO3.p = hyst_plot(POKE_storm13_07_24_Q, POKE_storm13_07_24_NO3, "POKE", "NO3", "0724")
POKE_storm14_07_26_NO3.p = hyst_plot(POKE_storm14_07_26_Q, POKE_storm14_07_26_NO3, "POKE", "NO3", "0726")
POKE_storm15_08_02_NO3.p = hyst_plot(POKE_storm15_08_02_Q, POKE_storm15_08_02_NO3, "POKE", "NO3", "0802")
POKE_storm16_08_12_NO3.p = hyst_plot(POKE_storm16_08_12_Q, POKE_storm16_08_12_NO3, "POKE", "NO3", "0812")
POKE_storm17_08_23_NO3.p = hyst_plot(POKE_storm17_08_23_Q, POKE_storm17_08_23_NO3, "POKE", "NO3", "0824")
POKE_storm18_08_25_NO3.p = hyst_plot(POKE_storm18_08_25_Q, POKE_storm18_08_25_NO3, "POKE", "NO3", "0825")
POKE_storm19_08_27_NO3.p = hyst_plot(POKE_storm19_08_27_Q, POKE_storm19_08_27_NO3, "POKE", "NO3", "0827")
POKE_storm20_09_01_NO3.p = hyst_plot(POKE_storm20_09_01_Q, POKE_storm20_09_01_NO3, "POKE", "NO3", "0901")
POKE_storm21_09_03_NO3.p = hyst_plot(POKE_storm21_09_03_Q, POKE_storm21_09_03_NO3, "POKE", "NO3", "0903")
POKE_storm22a_09_07_NO3.p = hyst_plot(POKE_storm22a_09_07_Q, POKE_storm22a_09_07_NO3, "POKE", "NO3", "0907a")
POKE_storm22b_09_09_NO3.p = hyst_plot(POKE_storm22b_09_09_Q, POKE_storm22b_09_09_NO3, "POKE", "NO3", "0909b")

POKE_storm1_06_09_fDOM.p = hyst_plot(POKE_storm1_06_09_Q, POKE_storm1_06_09_fDOM, "POKE", "fDOM", "0609")
POKE_storm2_06_12_fDOM.p = hyst_plot(POKE_storm2_06_12_Q, POKE_storm2_06_12_fDOM, "POKE", "fDOM", "0612")
POKE_storm3_06_15_fDOM.p = hyst_plot(POKE_storm3_06_15_Q, POKE_storm3_06_15_fDOM, "POKE", "fDOM", "0615")
POKE_storm4a_06_19_fDOM.p = hyst_plot(POKE_storm4a_06_19_Q, POKE_storm4a_06_19_fDOM, "POKE", "fDOM", "0619a")
POKE_storm4b_06_20_fDOM.p = hyst_plot(POKE_storm4b_06_20_Q, POKE_storm4b_06_20_fDOM, "POKE", "fDOM", "0620b")
POKE_storm4c_06_21_fDOM.p = hyst_plot(POKE_storm4c_06_21_Q, POKE_storm4c_06_21_fDOM, "POKE", "fDOM", "0621c")
POKE_storm5_06_22_fDOM.p = hyst_plot(POKE_storm5_06_22_Q, POKE_storm5_06_22_fDOM, "POKE", "fDOM", "0622")
POKE_storm6_06_29_fDOM.p = hyst_plot(POKE_storm6_06_29_Q, POKE_storm6_06_29_fDOM, "POKE", "fDOM", "0629")
POKE_storm7_07_04_fDOM.p = hyst_plot(POKE_storm7_07_04_Q, POKE_storm7_07_04_fDOM, "POKE", "fDOM", "0704")
POKE_storm8_07_09_fDOM.p = hyst_plot(POKE_storm8_07_09_Q, POKE_storm8_07_09_fDOM, "POKE", "fDOM", "0709")
POKE_storm9_07_12_fDOM.p = hyst_plot(POKE_storm9_07_12_Q, POKE_storm9_07_12_fDOM, "POKE", "fDOM", "0712")
POKE_storm10_07_16_fDOM.p = hyst_plot(POKE_storm10_07_16_Q, POKE_storm10_07_16_fDOM, "POKE", "fDOM", "0716")
POKE_storm12_07_20_fDOM.p = hyst_plot(POKE_storm12_07_20_Q, POKE_storm12_07_20_fDOM, "POKE", "fDOM", "0720")
POKE_storm11_07_18_fDOM.p = hyst_plot(POKE_storm11_07_18_Q, POKE_storm11_07_18_fDOM, "POKE", "fDOM", "0718")
POKE_storm13_07_24_fDOM.p = hyst_plot(POKE_storm13_07_24_Q, POKE_storm13_07_24_fDOM, "POKE", "fDOM", "0724")
POKE_storm14_07_26_fDOM.p = hyst_plot(POKE_storm14_07_26_Q, POKE_storm14_07_26_fDOM, "POKE", "fDOM", "0726")
POKE_storm15_08_02_fDOM.p = hyst_plot(POKE_storm15_08_02_Q, POKE_storm15_08_02_fDOM, "POKE", "fDOM", "0802")
POKE_storm16_08_12_fDOM.p = hyst_plot(POKE_storm16_08_12_Q, POKE_storm16_08_12_fDOM, "POKE", "fDOM", "0812")
POKE_storm17_08_23_fDOM.p = hyst_plot(POKE_storm17_08_23_Q, POKE_storm17_08_23_fDOM, "POKE", "fDOM", "0824")
POKE_storm18_08_25_fDOM.p = hyst_plot(POKE_storm18_08_25_Q, POKE_storm18_08_25_fDOM, "POKE", "fDOM", "0825")
POKE_storm19_08_27_fDOM.p = hyst_plot(POKE_storm19_08_27_Q, POKE_storm19_08_27_fDOM, "POKE", "fDOM", "0827")
POKE_storm20_09_01_fDOM.p = hyst_plot(POKE_storm20_09_01_Q, POKE_storm20_09_01_fDOM, "POKE", "fDOM", "0901")
POKE_storm21_09_03_fDOM.p = hyst_plot(POKE_storm21_09_03_Q, POKE_storm21_09_03_fDOM, "POKE", "fDOM", "0903")
POKE_storm22a_09_07_fDOM.p = hyst_plot(POKE_storm22a_09_07_Q, POKE_storm22a_09_07_fDOM, "POKE", "fDOM", "0907a")
POKE_storm22b_09_09_fDOM.p = hyst_plot(POKE_storm22b_09_09_Q, POKE_storm22b_09_09_fDOM, "POKE", "fDOM", "0909b")


POKE_storm1_06_09_SPC.p = hyst_plot(POKE_storm1_06_09_Q, POKE_storm1_06_09_SPC, "POKE", "SPC", "0609")
POKE_storm2_06_12_SPC.p = hyst_plot(POKE_storm2_06_12_Q, POKE_storm2_06_12_SPC, "POKE", "SPC", "0612")
POKE_storm3_06_15_SPC.p = hyst_plot(POKE_storm3_06_15_Q, POKE_storm3_06_15_SPC, "POKE", "SPC", "0615")
POKE_storm4a_06_19_SPC.p = hyst_plot(POKE_storm4a_06_19_Q, POKE_storm4a_06_19_SPC, "POKE", "SPC", "0619a")
POKE_storm4b_06_20_SPC.p = hyst_plot(POKE_storm4b_06_20_Q, POKE_storm4b_06_20_SPC, "POKE", "SPC", "0620b")
POKE_storm4c_06_21_SPC.p = hyst_plot(POKE_storm4c_06_21_Q, POKE_storm4c_06_219_SPC, "POKE", "SPC", "0621c")
POKE_storm5_06_22_SPC.p = hyst_plot(POKE_storm5_06_22_Q, POKE_storm5_06_22_SPC, "POKE", "SPC", "0622")
POKE_storm6_06_29_SPC.p = hyst_plot(POKE_storm6_06_29_Q, POKE_storm6_06_29_SPC, "POKE", "SPC", "0629")
POKE_storm7_07_04_SPC.p = hyst_plot(POKE_storm7_07_04_Q, POKE_storm7_07_04_SPC, "POKE", "SPC", "0704")
POKE_storm8_07_09_SPC.p = hyst_plot(POKE_storm8_07_09_Q, POKE_storm8_07_09_SPC, "POKE", "SPC", "0709")
POKE_storm9_07_12_SPC.p = hyst_plot(POKE_storm9_07_12_Q, POKE_storm9_07_12_SPC, "POKE", "SPC", "0712")
POKE_storm10_07_16_SPC.p = hyst_plot(POKE_storm10_07_16_Q, POKE_storm10_07_16_SPC, "POKE", "SPC", "0716")
POKE_storm11_07_18_SPC.p = hyst_plot(POKE_storm11_07_18_Q, POKE_storm11_07_18_SPC, "POKE", "SPC", "0718")
POKE_storm12_07_20_SPC.p = hyst_plot(POKE_storm12_07_20_Q, POKE_storm12_07_20_SPC, "POKE", "SPC", "0720")
POKE_storm13_07_24_SPC.p = hyst_plot(POKE_storm13_07_24_Q, POKE_storm13_07_24_SPC, "POKE", "SPC", "0724")
POKE_storm14_07_26_SPC.p = hyst_plot(POKE_storm14_07_26_Q, POKE_storm14_07_26_SPC, "POKE", "SPC", "0726")
POKE_storm15_08_02_SPC.p = hyst_plot(POKE_storm15_08_02_Q, POKE_storm15_08_02_SPC, "POKE", "SPC", "0802")
POKE_storm16_08_12_SPC.p = hyst_plot(POKE_storm16_08_12_Q, POKE_storm16_08_12_SPC, "POKE", "SPC", "0812")
POKE_storm17_08_23_SPC.p = hyst_plot(POKE_storm17_08_23_Q, POKE_storm17_08_23_SPC, "POKE", "SPC", "0824")
POKE_storm18_08_25_SPC.p = hyst_plot(POKE_storm18_08_25_Q, POKE_storm18_08_25_SPC, "POKE", "SPC", "0825")
POKE_storm19_08_27_SPC.p = hyst_plot(POKE_storm19_08_27_Q, POKE_storm19_08_27_SPC, "POKE", "SPC", "0827")
POKE_storm20_09_01_SPC.p = hyst_plot(POKE_storm20_09_01_Q, POKE_storm20_09_01_SPC, "POKE", "SPC", "0901")
POKE_storm21_09_03_SPC.p = hyst_plot(POKE_storm21_09_03_Q, POKE_storm21_09_03_SPC, "POKE", "SPC", "0903")
POKE_storm22a_09_07_SPC.p = hyst_plot(POKE_storm22a_09_07_Q, POKE_storm22a_09_07_SPC, "POKE", "SPC", "0907a")
POKE_storm22b_09_09_SPC.p = hyst_plot(POKE_storm22b_09_09_Q, POKE_storm22b_09_09_SPC, "POKE", "SPC", "0909b")

POKE_storm1_06_09_turb.p = hyst_plot(POKE_storm1_06_09_Q, POKE_storm1_06_09_turb, "POKE", "turb", "0609")
POKE_storm2_06_12_turb.p = hyst_plot(POKE_storm2_06_12_Q, POKE_storm2_06_12_turb, "POKE", "turb", "0612")
POKE_storm3_06_15_turb.p = hyst_plot(POKE_storm3_06_15_Q, POKE_storm3_06_15_turb, "POKE", "turb", "0615")
POKE_storm4a_06_19_turb.p = hyst_plot(POKE_storm4a_06_19_Q, POKE_storm4a_06_19_turb, "POKE", "turb", "0619a")
POKE_storm4b_06_20_turb.p = hyst_plot(POKE_storm4b_06_20_Q, POKE_storm4b_06_20_turb, "POKE", "turb", "0620b")
POKE_storm4c_06_21_turb.p = hyst_plot(POKE_storm4c_06_21_Q, POKE_storm4c_06_219_turb, "POKE", "turb", "0621c")
POKE_storm5_06_22_turb.p = hyst_plot(POKE_storm5_06_22_Q, POKE_storm5_06_22_turb, "POKE", "turb", "0622")
POKE_storm6_06_29_turb.p = hyst_plot(POKE_storm6_06_29_Q, POKE_storm6_06_29_turb, "POKE", "turb", "0629")
POKE_storm7_07_04_turb.p = hyst_plot(POKE_storm7_07_04_Q, POKE_storm7_07_04_turb, "POKE", "turb", "0704")
POKE_storm8_07_09_turb.p = hyst_plot(POKE_storm8_07_09_Q, POKE_storm8_07_09_turb, "POKE", "turb", "0709")
POKE_storm9_07_12_turb.p = hyst_plot(POKE_storm9_07_12_Q, POKE_storm9_07_12_turb, "POKE", "turb", "0712")
POKE_storm10_07_16_turb.p = hyst_plot(POKE_storm10_07_16_Q, POKE_storm10_07_16_turb, "POKE", "turb", "0716")
POKE_storm11_07_18_turb.p = hyst_plot(POKE_storm11_07_18_Q, POKE_storm11_07_18_turb, "POKE", "turb", "0718")
POKE_storm12_07_20_turb.p = hyst_plot(POKE_storm12_07_20_Q, POKE_storm12_07_20_turb, "POKE", "turb", "0720")
POKE_storm13_07_24_turb.p = hyst_plot(POKE_storm13_07_24_Q, POKE_storm13_07_24_turb, "POKE", "turb", "0724")
POKE_storm14_07_26_turb.p = hyst_plot(POKE_storm14_07_26_Q, POKE_storm14_07_26_turb, "POKE", "turb", "0726")
POKE_storm15_08_02_turb.p = hyst_plot(POKE_storm15_08_02_Q, POKE_storm15_08_02_turb, "POKE", "turb", "0802")
POKE_storm16_08_12_turb.p = hyst_plot(POKE_storm16_08_12_Q, POKE_storm16_08_12_turb, "POKE", "turb", "0812")
POKE_storm17_08_23_turb.p = hyst_plot(POKE_storm17_08_23_Q, POKE_storm17_08_23_turb, "POKE", "turb", "0824")
POKE_storm18_08_25_turb.p = hyst_plot(POKE_storm18_08_25_Q, POKE_storm18_08_25_turb, "POKE", "turb", "0825")
POKE_storm19_08_27_turb.p = hyst_plot(POKE_storm19_08_27_Q, POKE_storm19_08_27_turb, "POKE", "turb", "0827")
POKE_storm20_09_01_turb.p = hyst_plot(POKE_storm20_09_01_Q, POKE_storm20_09_01_turb, "POKE", "turb", "0901")
POKE_storm21_09_03_turb.p = hyst_plot(POKE_storm21_09_03_Q, POKE_storm21_09_03_turb, "POKE", "turb", "0903")
POKE_storm22a_09_07_turb.p = hyst_plot(POKE_storm22a_09_07_Q, POKE_storm22a_09_07_turb, "POKE", "turb", "0907a")
POKE_storm22b_09_09_turb.p = hyst_plot(POKE_storm22b_09_09_Q, POKE_storm22b_09_09_turb, "POKE", "turb", "0909b")

POKE_storm1_06_09_abs.p = hyst_plot(POKE_storm1_06_09_Q, POKE_storm1_06_09_abs, "POKE", "abs", "0609")
POKE_storm2_06_12_abs.p = hyst_plot(POKE_storm2_06_12_Q, POKE_storm2_06_12_abs, "POKE", "abs", "0612")
POKE_storm3_06_15_abs.p = hyst_plot(POKE_storm3_06_15_Q, POKE_storm3_06_15_abs, "POKE", "abs", "0615")
POKE_storm4a_06_19_abs.p = hyst_plot(POKE_storm4a_06_19_Q, POKE_storm4a_06_19_abs, "POKE", "abs", "0619a")
POKE_storm4b_06_20_abs.p = hyst_plot(POKE_storm4b_06_20_Q, POKE_storm4b_06_20_abs, "POKE", "abs", "0620b")
POKE_storm4c_06_21_abs.p = hyst_plot(POKE_storm4c_06_21_Q, POKE_storm4c_06_219_abs, "POKE", "abs", "0621c")
POKE_storm5_06_22_abs.p = hyst_plot(POKE_storm5_06_22_Q, POKE_storm5_06_22_abs, "POKE", "abs", "0622")
POKE_storm6_06_29_abs.p = hyst_plot(POKE_storm6_06_29_Q, POKE_storm6_06_29_abs, "POKE", "abs", "0629")
POKE_storm7_07_04_abs.p = hyst_plot(POKE_storm7_07_04_Q, POKE_storm7_07_04_abs, "POKE", "abs", "0704")
POKE_storm8_07_09_abs.p = hyst_plot(POKE_storm8_07_09_Q, POKE_storm8_07_09_abs, "POKE", "abs", "0709")
POKE_storm9_07_12_abs.p = hyst_plot(POKE_storm9_07_12_Q, POKE_storm9_07_12_abs, "POKE", "abs", "0712")
POKE_storm10_07_16_abs.p = hyst_plot(POKE_storm10_07_16_Q, POKE_storm10_07_16_abs, "POKE", "abs", "0716")
POKE_storm11_07_18_abs.p = hyst_plot(POKE_storm11_07_18_Q, POKE_storm11_07_18_abs, "POKE", "abs", "0718")
POKE_storm12_07_20_abs.p = hyst_plot(POKE_storm12_07_20_Q, POKE_storm12_07_20_abs, "POKE", "abs", "0720")
POKE_storm13_07_24_abs.p = hyst_plot(POKE_storm13_07_24_Q, POKE_storm13_07_24_abs, "POKE", "abs", "0724")
POKE_storm14_07_26_abs.p = hyst_plot(POKE_storm14_07_26_Q, POKE_storm14_07_26_abs, "POKE", "abs", "0726")
POKE_storm15_08_02_abs.p = hyst_plot(POKE_storm15_08_02_Q, POKE_storm15_08_02_abs, "POKE", "abs", "0802")
POKE_storm16_08_12_abs.p = hyst_plot(POKE_storm16_08_12_Q, POKE_storm16_08_12_abs, "POKE", "abs", "0812")
POKE_storm17_08_23_abs.p = hyst_plot(POKE_storm17_08_23_Q, POKE_storm17_08_23_abs, "POKE", "abs", "0824")
POKE_storm18_08_25_abs.p = hyst_plot(POKE_storm18_08_25_Q, POKE_storm18_08_25_abs, "POKE", "abs", "0825")
POKE_storm19_08_27_abs.p = hyst_plot(POKE_storm19_08_27_Q, POKE_storm19_08_27_abs, "POKE", "abs", "0827")
POKE_storm20_09_01_abs.p = hyst_plot(POKE_storm20_09_01_Q, POKE_storm20_09_01_abs, "POKE", "abs", "0901")
POKE_storm21_09_03_abs.p = hyst_plot(POKE_storm21_09_03_Q, POKE_storm21_09_03_abs, "POKE", "abs", "0903")
POKE_storm22a_09_07_abs.p = hyst_plot(POKE_storm22a_09_07_Q, POKE_storm22a_09_07_abs, "POKE", "abs", "0907a")
POKE_storm22b_09_09_abs.p = hyst_plot(POKE_storm22b_09_09_Q, POKE_storm22b_09_09_abs, "POKE", "abs", "0909b")

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

# Plot POKE Loops #

multiplot(POKE_storm1_06_09_NO3.p, POKE_storm1_06_09_fDOM.p, POKE_storm1_06_09_SPC.p, POKE_storm1_06_09_turb.p) # works
multiplot(POKE_storm2_06_12_NO3.p, POKE_storm2_06_12_fDOM.p, POKE_storm2_06_12_SPC.p, POKE_storm2_06_12_turb.p) # works
multiplot(POKE_storm3_06_15_NO3.p, POKE_storm3_06_15_fDOM.p, POKE_storm3_06_15_SPC.p, POKE_storm3_06_15_turb.p) # clean nitrate/ works
multiplot(POKE_storm4a_06_19_NO3.p, POKE_storm4a_06_19_fDOM.p,POKE_storm4a_06_19_SPC.p, POKE_storm4a_06_19_turb.p) # works
multiplot(POKE_storm4b_06_20_NO3.p, POKE_storm4b_06_20_fDOM.p, POKE_storm4b_06_20_SPC.p, POKE_storm4b_06_20_turb.p) # works
multiplot(POKE_storm4c_06_21_NO3.p, POKE_storm4c_06_21_fDOM.p) # does not work
multiplot(POKE_storm5_06_22_NO3.p, POKE_storm5_06_22_fDOM.p, POKE_storm5_06_22_SPC.p, POKE_storm5_06_22_turb.p) # works
multiplot(POKE_storm6_06_29_NO3.p, POKE_storm6_06_29_fDOM.p, POKE_storm6_06_29_SPC.p, POKE_storm6_06_29_turb.p) # works
multiplot(POKE_storm7_07_03_NO3.p, POKE_storm7_07_03_fDOM.p, POKE_storm7_07_03_SPC.p, POKE_storm7_07_03_turb.p) # works
multiplot(POKE_storm8_07_08_NO3.p, POKE_storm8_07_08_fDOM.p, POKE_storm8_07_08_SPC.p, POKE_storm8_07_08_turb.p) # works
multiplot(POKE_storm9_07_12_NO3.p, POKE_storm9_07_12_fDOM.p, POKE_storm9_07_12_SPC.p, POKE_storm9_07_12_turb.p)# works
multiplot(POKE_storm10_07_16_NO3.p, POKE_storm10_07_16_fDOM.p, POKE_storm10_07_16_SPC.p, POKE_storm10_07_16_turb.p) # works
multiplot(POKE_storm11_07_18_NO3.p, POKE_storm11_07_18_fDOM.p, POKE_storm11_07_18_SPC.p, POKE_storm11_07_18_turb.p) # clean nitrate/works
multiplot(POKE_storm12_07_20_NO3.p, POKE_storm12_07_20_fDOM.p, POKE_storm12_07_20_SPC.p, POKE_storm12_07_20_turb.p) # works
multiplot(POKE_storm13_07_24_NO3.p, POKE_storm13_07_24_fDOM.p, POKE_storm13_07_24_SPC.p, POKE_storm13_07_24_turb.p)# works
multiplot(POKE_storm14_07_26_NO3.p, POKE_storm14_07_26_fDOM.p, POKE_storm14_07_26_SPC.p, POKE_storm14_07_26_turb.p)# clean nitrate/works
multiplot(POKE_storm15_08_02_NO3.p, POKE_storm15_08_02_fDOM.p, POKE_storm15_08_02_SPC.p, POKE_storm15_08_02_turb.p)# works
multiplot(POKE_storm16_08_12_NO3.p, POKE_storm16_08_12_fDOM.p, POKE_storm16_08_12_SPC.p, POKE_storm16_08_12_turb.p)# works
multiplot(POKE_storm17_08_23_NO3.p, POKE_storm17_08_23_fDOM.p, POKE_storm17_08_23_SPC.p, POKE_storm17_08_23_turb.p)# works
multiplot(POKE_storm18_08_24_NO3.p, POKE_storm18_08_24_fDOM.p, POKE_storm18_08_24_SPC.p, POKE_storm18_08_24_turb.p)# works
multiplot(POKE_storm19_08_27_NO3.p, POKE_storm19_08_27_fDOM.p, POKE_storm19_08_27_SPC.p, POKE_storm19_08_27_turb.p)# works
multiplot(POKE_storm20_09_01_NO3.p, POKE_storm20_09_01_fDOM.p, POKE_storm20_09_01_SPC.p, POKE_storm20_09_01_turb.p)# works
multiplot(POKE_storm21_09_03_NO3.p, POKE_storm21_09_03_fDOM.p, POKE_storm21_09_03_SPC.p, POKE_storm21_09_03_turb.p)# works
multiplot(POKE_storm22a_09_07_NO3.p, POKE_storm22a_09_07_fDOM.p, POKE_storm22a_09_07_SPC.p, POKE_storm22a_09_07_turb.p)# works
multiplot(POKE_storm22b_09_08_NO3.p, POKE_storm22b_09_08_fDOM.p, POKE_storm22b_09_08_SPC.p, POKE_storm22b_09_08_turb.p)# works


multiplot(POKE_storm1_06_09_NO3.p, POKE_storm1_06_09_fDOM.p, POKE_storm1_06_09_SPC.p, POKE_storm1_06_09_turb.p,
          POKE_storm2_06_12_NO3.p, POKE_storm2_06_12_fDOM.p, POKE_storm2_06_12_SPC.p, POKE_storm2_06_12_turb.p,
          POKE_storm3_06_15_NO3.p, POKE_storm3_06_15_fDOM.p, POKE_storm3_06_15_SPC.p, POKE_storm3_06_15_turb.p,
          POKE_storm4a_06_19_NO3.p, POKE_storm4a_06_19_fDOM.p,POKE_storm4a_06_19_SPC.p, POKE_storm4a_06_19_turb.p,
          POKE_storm4b_06_20_NO3.p, POKE_storm4b_06_20_fDOM.p,
          POKE_storm5_06_22_NO3.p, POKE_storm5_06_22_fDOM.p, POKE_storm5_06_22_SPC.p, POKE_storm5_06_22_turb.p,
          POKE_storm6_06_29_NO3.p, POKE_storm6_06_29_fDOM.p, POKE_storm6_06_29_SPC.p, POKE_storm6_06_29_turb.p,
          POKE_storm7_07_04_NO3.p, POKE_storm7_07_04_fDOM.p, POKE_storm7_07_04_SPC.p, POKE_storm7_07_04_turb.p,
          POKE_storm8_07_09_NO3.p, POKE_storm8_07_09_fDOM.p, POKE_storm8_07_09_SPC.p, POKE_storm8_07_09_turb.p,
          POKE_storm9_07_12_NO3.p, POKE_storm9_07_12_fDOM.p, POKE_storm9_07_12_SPC.p, POKE_storm9_07_12_turb.p,
          POKE_storm10_07_16_NO3.p, POKE_storm10_07_16_fDOM.p, POKE_storm10_07_16_SPC.p, POKE_storm10_07_16_turb.p,
          POKE_storm11_07_18_NO3.p, POKE_storm11_07_18_fDOM.p, POKE_storm11_07_18_SPC.p, POKE_storm11_07_18_turb.p,
          POKE_storm12_07_20_NO3.p, POKE_storm12_07_20_fDOM.p, POKE_storm12_07_20_SPC.p, POKE_storm12_07_20_turb.p,
          POKE_storm13_07_24_NO3.p, POKE_storm13_07_24_fDOM.p, POKE_storm13_07_24_SPC.p, POKE_storm13_07_24_turb.p,
          POKE_storm14_07_26_NO3.p, POKE_storm14_07_26_fDOM.p, POKE_storm14_07_26_SPC.p, POKE_storm14_07_26_turb.p,
          POKE_storm15_08_02_NO3.p, POKE_storm15_08_02_fDOM.p, POKE_storm15_08_02_SPC.p, POKE_storm15_08_02_turb.p,
          POKE_storm16_08_12_NO3.p, POKE_storm16_08_12_fDOM.p, POKE_storm16_08_12_SPC.p, POKE_storm16_08_12_turb.p,
          POKE_storm17_08_23_NO3.p, POKE_storm17_08_23_fDOM.p, POKE_storm17_08_23_SPC.p, POKE_storm17_08_23_turb.p,
          POKE_storm18_08_25_NO3.p, POKE_storm18_08_25_fDOM.p, POKE_storm18_08_25_SPC.p, POKE_storm18_08_25_turb.p,
          POKE_storm19_08_27_NO3.p, POKE_storm19_08_27_fDOM.p, POKE_storm19_08_27_SPC.p, POKE_storm19_08_27_turb.p,
          POKE_storm20_09_01_NO3.p, POKE_storm20_09_01_fDOM.p, POKE_storm20_09_01_SPC.p, POKE_storm20_09_01_turb.p,
          POKE_storm21_09_03_NO3.p, POKE_storm21_09_03_fDOM.p, POKE_storm21_09_03_SPC.p, POKE_storm21_09_03_turb.p,
          POKE_storm22a_09_07_NO3.p, POKE_storm22a_09_07_fDOM.p, POKE_storm22a_09_07_SPC.p, POKE_storm22a_09_07_turb.p,
          POKE_storm22b_09_09_NO3.p, POKE_storm22b_09_09_fDOM.p, POKE_storm22b_09_09_SPC.p, POKE_storm22b_09_09_turb.p,
          cols = 7) # works

# export pdf 20 x 30 #
ggsave("POKE_HI_Loops_2020.pdf",
       path = here("plots", "HI_plots", "2020", "POKE"),
       width = 20, height = 30, units = "in")


#### VAUL Hysteresis ####
# NO3-#
VAUL_storm1a_06_19_NO3.p = hyst_plot(VAUL_storm1a_06_19_Q, VAUL_storm1a_06_19_NO3, "VAUL", "NO3", "0619a")
VAUL_storm1b_06_20_NO3.p = hyst_plot(VAUL_storm1b_06_20_Q, VAUL_storm1b_06_20_NO3, "VAUL", "NO3", "0620b")
VAUL_storm1c_06_22_NO3.p = hyst_plot(VAUL_storm1c_06_22_Q, VAUL_storm1c_06_22_NO3, "VAUL", "NO3", "0622c")
VAUL_storm2_06_28_NO3.p = hyst_plot(VAUL_storm2_06_28_Q, VAUL_storm2_06_28_NO3, "VAUL", "NO3", "0628")
VAUL_storm3_07_09_NO3.p = hyst_plot(VAUL_storm3_07_09_Q, VAUL_storm3_07_09_NO3, "VAUL", "NO3", "0709")
VAUL_storm4_07_12_NO3.p = hyst_plot(VAUL_storm4_07_12_Q, VAUL_storm4_07_12_NO3, "VAUL", "NO3", "0712")
VAUL_storm5_07_26_NO3.p = hyst_plot(VAUL_storm5_07_26_Q, VAUL_storm5_07_26_NO3, "VAUL", "NO3", "0726")
VAUL_storm6a_08_01_NO3.p = hyst_plot(VAUL_storm6a_08_01_Q, VAUL_storm6a_08_01_NO3, "VAUL", "NO3", "0801a")
VAUL_storm6b_08_02_NO3.p = hyst_plot(VAUL_storm6b_08_02_Q, VAUL_storm6b_08_02_NO3, "VAUL", "NO3", "0802b")
VAUL_storm7_08_08_NO3.p = hyst_plot(VAUL_storm7_08_08_Q, VAUL_storm7_08_08_NO3, "VAUL", "NO3", "0808")
VAUL_storm8_08_11_NO3.p = hyst_plot(VAUL_storm8_08_11_Q, VAUL_storm8_08_11_NO3, "VAUL", "NO3", "0811")
VAUL_storm9_08_12_NO3.p = hyst_plot(VAUL_storm9_08_12_Q, VAUL_storm9_08_12_NO3, "VAUL", "NO3", "0812")
VAUL_storm10_08_25_NO3.p = hyst_plot(VAUL_storm10_08_25_Q, VAUL_storm10_08_25_NO3, "VAUL", "NO3", "0825")
VAUL_storm11_08_27_NO3.p = hyst_plot(VAUL_storm11_08_27_Q, VAUL_storm11_08_27_NO3, "VAUL", "NO3", "0827")
VAUL_storm12_09_01_NO3.p = hyst_plot(VAUL_storm12_09_01_Q, VAUL_storm12_09_01_NO3, "VAUL", "NO3", "0901")
VAUL_storm13_09_03_NO3.p = hyst_plot(VAUL_storm13_09_03_Q, VAUL_storm13_09_03_NO3, "VAUL", "NO3", "0903")
VAUL_storm14_09_06_NO3.p = hyst_plot(VAUL_storm14_09_06_Q, VAUL_storm14_09_06_NO3, "VAUL", "NO3", "0906")

#fDOM#
VAUL_storm1a_06_19_fDOM.p = hyst_plot(VAUL_storm1a_06_19_Q, VAUL_storm1a_06_19_fDOM, "VAUL", "fDOM", "0619a")
VAUL_storm1b_06_20_fDOM.p = hyst_plot(VAUL_storm1b_06_20_Q, VAUL_storm1b_06_20_fDOM, "VAUL", "fDOM", "0620b")
VAUL_storm1c_06_22_fDOM.p = hyst_plot(VAUL_storm1c_06_22_Q, VAUL_storm1c_06_22_fDOM, "VAUL", "fDOM", "0622c")
VAUL_storm2_06_28_fDOM.p = hyst_plot(VAUL_storm2_06_28_Q, VAUL_storm2_06_28_fDOM, "VAUL", "fDOM", "0628")
VAUL_storm3_07_09_fDOM.p = hyst_plot(VAUL_storm3_07_09_Q, VAUL_storm3_07_09_fDOM, "VAUL", "fDOM", "0709")
VAUL_storm4_07_12_fDOM.p = hyst_plot(VAUL_storm4_07_12_Q, VAUL_storm4_07_12_fDOM, "VAUL", "fDOM", "0712")
VAUL_storm5_07_26_fDOM.p = hyst_plot(VAUL_storm5_07_26_Q, VAUL_storm5_07_26_fDOM, "VAUL", "fDOM", "0726")
VAUL_storm6a_08_01_fDOM.p = hyst_plot(VAUL_storm6a_08_01_Q, VAUL_storm6a_08_01_fDOM, "VAUL", "fDOM", "0801a")
VAUL_storm6b_08_02_fDOM.p = hyst_plot(VAUL_storm6b_08_02_Q, VAUL_storm6b_08_02_fDOM, "VAUL", "fDOM", "0802b")
VAUL_storm7_08_08_fDOM.p = hyst_plot(VAUL_storm7_08_08_Q, VAUL_storm7_08_08_fDOM, "VAUL", "fDOM", "0808")
VAUL_storm8_08_11_fDOM.p = hyst_plot(VAUL_storm8_08_11_Q, VAUL_storm8_08_11_fDOM, "VAUL", "fDOM", "0811")
VAUL_storm9_08_12_fDOM.p = hyst_plot(VAUL_storm9_08_12_Q, VAUL_storm9_08_12_fDOM, "VAUL", "fDOM", "0812")
VAUL_storm10_08_25_fDOM.p = hyst_plot(VAUL_storm10_08_25_Q, VAUL_storm10_08_25_fDOM, "VAUL", "fDOM", "0825")
VAUL_storm11_08_27_fDOM.p = hyst_plot(VAUL_storm11_08_27_Q, VAUL_storm11_08_27_fDOM, "VAUL", "fDOM", "0827")
VAUL_storm12_09_01_fDOM.p = hyst_plot(VAUL_storm12_09_01_Q, VAUL_storm12_09_01_fDOM, "VAUL", "fDOM", "0901")
VAUL_storm13_09_03_fDOM.p = hyst_plot(VAUL_storm13_09_03_Q, VAUL_storm13_09_03_fDOM, "VAUL", "fDOM", "0903")
VAUL_storm14_09_06_fDOM.p = hyst_plot(VAUL_storm14_09_06_Q, VAUL_storm14_09_06_fDOM, "VAUL", "fDOM", "0906")
# SPC #
VAUL_storm1a_06_19_SPC.p = hyst_plot(VAUL_storm1a_06_19_Q, VAUL_storm1a_06_19_SPC, "VAUL", "SPC", "0619a")
VAUL_storm1b_06_20_SPC.p = hyst_plot(VAUL_storm1b_06_20_Q, VAUL_storm1b_06_20_SPC, "VAUL", "SPC", "0620b")
VAUL_storm1c_06_22_SPC.p = hyst_plot(VAUL_storm1c_06_22_Q, VAUL_storm1c_06_22_SPC, "VAUL", "SPC", "0622c")
VAUL_storm2_06_28_SPC.p = hyst_plot(VAUL_storm2_06_28_Q, VAUL_storm2_06_28_SPC, "VAUL", "SPC", "0628")
VAUL_storm3_07_09_SPC.p = hyst_plot(VAUL_storm3_07_09_Q, VAUL_storm3_07_09_SPC, "VAUL", "SPC", "0709")
VAUL_storm4_07_12_SPC.p = hyst_plot(VAUL_storm4_07_12_Q, VAUL_storm4_07_12_SPC, "VAUL", "SPC", "0712")
VAUL_storm5_07_26_SPC.p = hyst_plot(VAUL_storm5_07_26_Q, VAUL_storm5_07_26_SPC, "VAUL", "SPC", "0726")
VAUL_storm6a_08_01_SPC.p = hyst_plot(VAUL_storm6a_08_01_Q, VAUL_storm6a_08_01_SPC, "VAUL", "SPC", "0801a")
VAUL_storm6b_08_02_SPC.p = hyst_plot(VAUL_storm6b_08_02_Q, VAUL_storm6b_08_02_SPC, "VAUL", "SPC", "0802b")
VAUL_storm7_08_08_SPC.p = hyst_plot(VAUL_storm7_08_08_Q, VAUL_storm7_08_08_SPC, "VAUL", "SPC", "0808")
VAUL_storm8_08_11_SPC.p = hyst_plot(VAUL_storm8_08_11_Q, VAUL_storm8_08_11_SPC, "VAUL", "SPC", "0811")
VAUL_storm9_08_12_SPC.p = hyst_plot(VAUL_storm9_08_12_Q, VAUL_storm9_08_12_SPC, "VAUL", "SPC", "0812")
VAUL_storm10_08_25_SPC.p = hyst_plot(VAUL_storm10_08_25_Q, VAUL_storm10_08_25_SPC, "VAUL", "SPC", "0825")
VAUL_storm11_08_27_SPC.p = hyst_plot(VAUL_storm11_08_27_Q, VAUL_storm11_08_27_SPC, "VAUL", "SPC", "0827")
VAUL_storm12_09_01_SPC.p = hyst_plot(VAUL_storm12_09_01_Q, VAUL_storm12_09_01_SPC, "VAUL", "SPC", "0901")
VAUL_storm13_09_03_SPC.p = hyst_plot(VAUL_storm13_09_03_Q, VAUL_storm13_09_03_SPC, "VAUL", "SPC", "0903")
VAUL_storm14_09_06_SPC.p = hyst_plot(VAUL_storm14_09_06_Q, VAUL_storm14_09_06_SPC, "VAUL", "SPC", "0906")

# turb #
VAUL_storm1a_06_19_turb.p = hyst_plot(VAUL_storm1a_06_19_Q, VAUL_storm1a_06_19_turb, "VAUL", "turb", "0619a")
VAUL_storm1b_06_20_turb.p = hyst_plot(VAUL_storm1b_06_20_Q, VAUL_storm1b_06_20_turb, "VAUL", "turb", "0620b")
VAUL_storm1c_06_22_turb.p = hyst_plot(VAUL_storm1c_06_22_Q, VAUL_storm1c_06_22_turb, "VAUL", "turb", "0622c")
VAUL_storm2_06_28_turb.p = hyst_plot(VAUL_storm2_06_28_Q, VAUL_storm2_06_28_turb, "VAUL", "turb", "0628")
VAUL_storm3_07_09_turb.p = hyst_plot(VAUL_storm3_07_09_Q, VAUL_storm3_07_09_turb, "VAUL", "turb", "0709")
VAUL_storm4_07_12_turb.p = hyst_plot(VAUL_storm4_07_12_Q, VAUL_storm4_07_12_turb, "VAUL", "turb", "0712")
VAUL_storm5_07_26_turb.p = hyst_plot(VAUL_storm5_07_26_Q, VAUL_storm5_07_26_turb, "VAUL", "turb", "0726")
VAUL_storm6a_08_01_turb.p = hyst_plot(VAUL_storm6a_08_01_Q, VAUL_storm6a_08_01_turb, "VAUL", "turb", "0801a")
VAUL_storm6b_08_02_turb.p = hyst_plot(VAUL_storm6b_08_02_Q, VAUL_storm6b_08_02_turb, "VAUL", "turb", "0802b")
VAUL_storm7_08_08_turb.p = hyst_plot(VAUL_storm7_08_08_Q, VAUL_storm7_08_08_turb, "VAUL", "turb", "0808")
VAUL_storm8_08_11_turb.p = hyst_plot(VAUL_storm8_08_11_Q, VAUL_storm8_08_11_turb, "VAUL", "turb", "0811")
VAUL_storm9_08_12_turb.p = hyst_plot(VAUL_storm9_08_12_Q, VAUL_storm9_08_12_turb, "VAUL", "turb", "0812")
VAUL_storm10_08_25_turb.p = hyst_plot(VAUL_storm10_08_25_Q, VAUL_storm10_08_25_turb, "VAUL", "turb", "0825")
VAUL_storm11_08_27_turb.p = hyst_plot(VAUL_storm11_08_27_Q, VAUL_storm11_08_27_turb, "VAUL", "turb", "0827")
VAUL_storm12_09_01_turb.p = hyst_plot(VAUL_storm12_09_01_Q, VAUL_storm12_09_01_turb, "VAUL", "turb", "0901")
VAUL_storm13_09_03_turb.p = hyst_plot(VAUL_storm13_09_03_Q, VAUL_storm13_09_03_turb, "VAUL", "turb", "0903")
VAUL_storm14_09_06_turb.p = hyst_plot(VAUL_storm14_09_06_Q, VAUL_storm14_09_06_turb, "VAUL", "turb", "0906")

# ABS #
VAUL_storm1a_06_19_abs.p = hyst_plot(VAUL_storm1a_06_19_Q, VAUL_storm1a_06_19_abs, "VAUL", "abs", "0619a")
VAUL_storm1b_06_20_abs.p = hyst_plot(VAUL_storm1b_06_20_Q, VAUL_storm1b_06_20_abs, "VAUL", "abs", "0620b")
VAUL_storm1c_06_22_abs.p = hyst_plot(VAUL_storm1c_06_22_Q, VAUL_storm1c_06_22_abs, "VAUL", "abs", "0622c")
VAUL_storm2_06_28_abs.p = hyst_plot(VAUL_storm2_06_28_Q, VAUL_storm2_06_28_abs, "VAUL", "abs", "0628")
VAUL_storm3_07_09_abs.p = hyst_plot(VAUL_storm3_07_09_Q, VAUL_storm3_07_09_abs, "VAUL", "abs", "0709")
VAUL_storm4_07_12_abs.p = hyst_plot(VAUL_storm4_07_12_Q, VAUL_storm4_07_12_abs, "VAUL", "abs", "0712")
VAUL_storm5_07_26_abs.p = hyst_plot(VAUL_storm5_07_26_Q, VAUL_storm5_07_26_abs, "VAUL", "abs", "0726")
VAUL_storm6a_08_01_abs.p = hyst_plot(VAUL_storm6a_08_01_Q, VAUL_storm6a_08_01_abs, "VAUL", "abs", "0801a")
VAUL_storm6b_08_02_abs.p = hyst_plot(VAUL_storm6b_08_02_Q, VAUL_storm6b_08_02_abs, "VAUL", "abs", "0802b")
VAUL_storm7_08_08_abs.p = hyst_plot(VAUL_storm7_08_08_Q, VAUL_storm7_08_08_abs, "VAUL", "abs", "0808")
VAUL_storm8_08_11_abs.p = hyst_plot(VAUL_storm8_08_11_Q, VAUL_storm8_08_11_abs, "VAUL", "abs", "0811")
VAUL_storm9_08_12_abs.p = hyst_plot(VAUL_storm9_08_12_Q, VAUL_storm9_08_12_abs, "VAUL", "abs", "0812")
VAUL_storm10_08_25_abs.p = hyst_plot(VAUL_storm10_08_25_Q, VAUL_storm10_08_25_abs, "VAUL", "abs", "0825")
VAUL_storm11_08_27_abs.p = hyst_plot(VAUL_storm11_08_27_Q, VAUL_storm11_08_27_abs, "VAUL", "abs", "0827")
VAUL_storm12_09_01_abs.p = hyst_plot(VAUL_storm12_09_01_Q, VAUL_storm12_09_01_abs, "VAUL", "abs", "0901")
VAUL_storm13_09_03_abs.p = hyst_plot(VAUL_storm13_09_03_Q, VAUL_storm13_09_03_abs, "VAUL", "abs", "0903")
VAUL_storm14_09_06_abs.p = hyst_plot(VAUL_storm14_09_06_Q, VAUL_storm14_09_06_abs, "VAUL", "abs", "0906")


# Plot Loops #
multiplot(VAUL_storm1a_06_19_NO3.p, VAUL_storm1a_06_19_fDOM.p) # No3 doesnt have much of a relationship by fdom does 
multiplot(VAUL_storm1b_06_20_NO3.p, VAUL_storm1b_06_20_fDOM.p) # No3 doesnt have much of a relationship by fdom does 
multiplot(VAUL_storm2_06_27_NO3.p, VAUL_storm2_06_27_fDOM.p) # fdom NAs? 
multiplot(VAUL_storm3_07_08_NO3.p, VAUL_storm3_07_08_fDOM.p) # works 
multiplot(VAUL_storm4_07_12_NO3.p, VAUL_storm4_07_12_fDOM.p) # works 
multiplot(VAUL_storm5_07_26_NO3.p, VAUL_storm5_07_26_fDOM.p) # works
multiplot(VAUL_storm6a_08_01_NO3.p, VAUL_storm6a_08_01_fDOM.p) # works but needs to be cleaned for values below 10
multiplot(VAUL_storm6b_08_02_NO3.p, VAUL_storm6b_08_02_fDOM.p) # works but needs to be cleaned 
multiplot(VAUL_storm7_08_08_NO3.p, VAUL_storm7_08_08_fDOM.p) # fdom has one really big value...nitrate needs cleaning 
multiplot(VAUL_storm8_08_11_NO3.p, VAUL_storm8_08_11_fDOM.p) # works
multiplot(VAUL_storm9_08_12_NO3.p, VAUL_storm9_08_12_fDOM.p)# works
multiplot(VAUL_storm10_08_25_NO3.p, VAUL_storm10_08_25_fDOM.p) # works 
multiplot(VAUL_storm11_08_27_NO3.p, VAUL_storm11_08_27_fDOM.p) # works 
multiplot(VAUL_storm12_09_01_NO3.p, VAUL_storm12_09_01_fDOM.p) # works
multiplot(VAUL_storm13_09_03_NO3.p, VAUL_storm13_09_03_fDOM.p) # works
multiplot(VAUL_storm14_09_06_NO3.p, VAUL_storm14_09_06_fDOM.p) # works


multiplot(VAUL_storm1a_06_19_SPC.p, VAUL_storm1a_06_19_turb.p) # works
multiplot(VAUL_storm1b_06_20_SPC.p, VAUL_storm1b_06_20_turb.p) # works
multiplot(VAUL_storm2_06_28_SPC.p, VAUL_storm2_06_28_turb.p) # empty
multiplot(VAUL_storm3_07_08_SPC.p, VAUL_storm3_07_08_turb.p) # works 
multiplot(VAUL_storm4_07_12_SPC.p, VAUL_storm4_07_12_turb.p) # works 
multiplot(VAUL_storm5_07_26_SPC.p, VAUL_storm5_07_26_turb.p) # works
multiplot(VAUL_storm6a_08_01_SPC.p, VAUL_storm6a_08_01_turb.p) # works 
multiplot(VAUL_storm6b_08_02_SPC.p, VAUL_storm6b_08_02_turb.p) # works 
multiplot(VAUL_storm7_08_08_SPC.p, VAUL_storm7_08_08_turb.p) # works
multiplot(VAUL_storm8_08_11_SPC.p, VAUL_storm8_08_11_turb.p) # works
multiplot(VAUL_storm9_08_12_SPC.p, VAUL_storm9_08_12_turb.p)# works
multiplot(VAUL_storm10_08_25_SPC.p, VAUL_storm10_08_25_turb.p) # works 
multiplot(VAUL_storm11_08_27_SPC.p, VAUL_storm11_08_27_turb.p) # works 
multiplot(VAUL_storm12_09_01_SPC.p, VAUL_storm12_09_01_turb.p) # works
multiplot(VAUL_storm13_09_03_SPC.p, VAUL_storm13_09_03_turb.p) # works
multiplot(VAUL_storm14_09_06_SPC.p, VAUL_storm14_09_06_turb.p) # works


multiplot(VAUL_storm1a_06_19_NO3.p, VAUL_storm1a_06_19_fDOM.p, VAUL_storm1a_06_19_SPC.p, VAUL_storm1a_06_19_turb.p,
          VAUL_storm1b_06_20_NO3.p, VAUL_storm1b_06_20_fDOM.p, VAUL_storm1b_06_20_SPC.p, VAUL_storm1b_06_20_turb.p,
          VAUL_storm2_06_28_NO3.p, VAUL_storm2_06_28_fDOM.p, VAUL_storm2_06_28_SPC.p, VAUL_storm2_06_28_turb.p,
          VAUL_storm3_07_09_NO3.p, VAUL_storm3_07_09_fDOM.p, VAUL_storm3_07_09_SPC.p, VAUL_storm3_07_09_turb.p,
          VAUL_storm4_07_12_NO3.p, VAUL_storm4_07_12_fDOM.p, VAUL_storm4_07_12_SPC.p, VAUL_storm4_07_12_turb.p,
          VAUL_storm5_07_26_NO3.p, VAUL_storm5_07_26_fDOM.p, VAUL_storm5_07_26_SPC.p, VAUL_storm5_07_26_turb.p,
          VAUL_storm6a_08_01_NO3.p, VAUL_storm6a_08_01_fDOM.p,VAUL_storm6a_08_01_SPC.p, VAUL_storm6a_08_01_turb.p,
          VAUL_storm6b_08_02_NO3.p, VAUL_storm6b_08_02_fDOM.p, VAUL_storm6b_08_02_SPC.p, VAUL_storm6b_08_02_turb.p,
          VAUL_storm7_08_08_NO3.p, VAUL_storm7_08_08_fDOM.p, VAUL_storm7_08_08_SPC.p, VAUL_storm7_08_08_turb.p,
          VAUL_storm8_08_11_NO3.p, VAUL_storm8_08_11_fDOM.p, VAUL_storm8_08_11_SPC.p, VAUL_storm8_08_11_turb.p,
          VAUL_storm9_08_12_NO3.p, VAUL_storm9_08_12_fDOM.p, VAUL_storm9_08_12_SPC.p, VAUL_storm9_08_12_turb.p,
          VAUL_storm10_08_25_NO3.p, VAUL_storm10_08_25_fDOM.p, VAUL_storm10_08_25_SPC.p, VAUL_storm10_08_25_turb.p,
          VAUL_storm11_08_27_NO3.p, VAUL_storm11_08_27_fDOM.p, VAUL_storm11_08_27_SPC.p, VAUL_storm11_08_27_turb.p,
          VAUL_storm12_09_01_NO3.p, VAUL_storm12_09_01_fDOM.p, VAUL_storm12_09_01_SPC.p, VAUL_storm12_09_01_turb.p,
          VAUL_storm13_09_03_NO3.p, VAUL_storm13_09_03_fDOM.p, VAUL_storm13_09_03_SPC.p, VAUL_storm13_09_03_turb.p,
          VAUL_storm14_09_06_NO3.p, VAUL_storm14_09_06_fDOM.p,VAUL_storm14_09_06_SPC.p, VAUL_storm14_09_06_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("VAUL_HI_Loops_2020.pdf",
       path = here("plots", "HI_plots", "2020", "VAUL"),
       width = 20, height = 30, units = "in")


#### CARI Hysteresis ####
# NO3-#
#CARI_storm1_06_15_NO3.p = hyst_plot(CARI_storm1_06_15_Q, CARI_storm1_06_15_NO3, "CARI", "NO3", "0615")
CARI_storm2a_06_19_NO3.p = hyst_plot(CARI_storm2a_06_19_Q, CARI_storm2a_06_19_NO3, "CARI", "NO3", "0619a")
CARI_storm2b_06_20_NO3.p = hyst_plot(CARI_storm2b_06_20_Q, CARI_storm2b_06_20_NO3, "CARI", "NO3", "0620b")
CARI_storm2c_06_21_NO3.p = hyst_plot(CARI_storm2c_06_21_Q, CARI_storm2c_06_21_NO3, "CARI", "NO3", "0621c")
CARI_storm3_06_23_NO3.p = hyst_plot(CARI_storm3_06_23_Q, CARI_storm3_06_23_NO3, "CARI", "NO3", "0623")
CARI_storm4_07_09_NO3.p = hyst_plot(CARI_storm4_07_09_Q, CARI_storm4_07_09_NO3, "CARI", "NO3", "0709")
CARI_storm5_07_13_NO3.p = hyst_plot(CARI_storm5_07_13_Q, CARI_storm5_07_13_NO3, "CARI", "NO3", "0713")
CARI_storm6_07_24_NO3.p = hyst_plot(CARI_storm6_07_24_Q, CARI_storm6_07_24_NO3, "CARI", "NO3", "0724")
CARI_storm7_07_27_NO3.p = hyst_plot(CARI_storm7_07_27_Q, CARI_storm7_07_27_NO3, "CARI", "NO3", "0727")
CARI_storm8a_08_02_NO3.p = hyst_plot(CARI_storm8a_08_02_Q, CARI_storm8a_08_02_NO3, "CARI", "NO3", "0802a")
CARI_storm8b_08_03_NO3.p = hyst_plot(CARI_storm8b_08_03_Q, CARI_storm8b_08_03_NO3, "CARI", "NO3", "0803b")
CARI_storm9_09_07_NO3.p = hyst_plot(CARI_storm9_09_07_Q, CARI_storm9_09_07_NO3, "CARI", "NO3", "0907")

#fDOM#

#CARI_storm1_06_15_fDOM.p = hyst_plot(CARI_storm1_06_15_Q, CARI_storm1_06_15_fDOM, "CARI", "fDOM", "0615")
CARI_storm2a_06_19_fDOM.p = hyst_plot(CARI_storm2a_06_19_Q, CARI_storm2a_06_19_fDOM, "CARI", "fDOM", "0619a")
CARI_storm2b_06_20_fDOM.p = hyst_plot(CARI_storm2b_06_20_Q, CARI_storm2b_06_20_fDOM, "CARI", "fDOM", "0620b")
CARI_storm2c_06_21_fDOM.p = hyst_plot(CARI_storm2c_06_21_Q, CARI_storm2c_06_21_fDOM, "CARI", "fDOM", "0621c")
CARI_storm3_06_23_fDOM.p = hyst_plot(CARI_storm3_06_23_Q, CARI_storm3_06_23_fDOM, "CARI", "fDOM", "0623")
CARI_storm4_07_09_fDOM.p = hyst_plot(CARI_storm4_07_09_Q, CARI_storm4_07_09_fDOM, "CARI", "fDOM", "0709")
CARI_storm5_07_13_fDOM.p = hyst_plot(CARI_storm5_07_13_Q, CARI_storm5_07_13_fDOM, "CARI", "fDOM", "0713")
CARI_storm6_07_24_fDOM.p = hyst_plot(CARI_storm6_07_24_Q, CARI_storm6_07_24_fDOM, "CARI", "fDOM", "0724")
CARI_storm7_07_27_fDOM.p = hyst_plot(CARI_storm7_07_27_Q, CARI_storm7_07_27_fDOM, "CARI", "fDOM", "0727")
CARI_storm8a_08_02_fDOM.p = hyst_plot(CARI_storm8a_08_02_Q, CARI_storm8a_08_02_fDOM, "CARI", "fDOM", "0802a")
CARI_storm8b_08_03_fDOM.p = hyst_plot(CARI_storm8b_08_03_Q, CARI_storm8b_08_03_fDOM, "CARI", "fDOM", "0803b")
CARI_storm9_09_07_fDOM.p = hyst_plot(CARI_storm9_09_07_Q, CARI_storm9_09_07_fDOM, "CARI", "fDOM", "0907")

#SPC #
CARI_storm1_06_15_SPC <- na.omit(CARI_storm1_06_15_SPC)
CARI_storm1_06_15_SPC <- CARI_storm1_06_15_SPC[!duplicated(CARI_storm1_06_15_SPC$valuedatetime), ]

CARI_storm2a_06_19_Q <- CARI_storm2a_06_19_Q[!duplicated(CARI_storm2a_06_19_Q$valuedatetime), ]
CARI_storm2a_06_19_SPC <- CARI_storm2a_06_19_SPC[!duplicated(CARI_storm2a_06_19_SPC$valuedatetime), ]

CARI_storm2b_06_20_Q <- CARI_storm2b_06_20_Q[!duplicated(CARI_storm2b_06_20_Q$valuedatetime), ]
CARI_storm2b_06_20_SPC <- CARI_storm2b_06_20_SPC[!duplicated(CARI_storm2b_06_20_SPC$valuedatetime), ]

CARI_storm3_06_23_Q <- CARI_storm3_06_23_Q[!duplicated(CARI_storm3_06_23_Q$valuedatetime), ]
CARI_storm3_06_23_SPC <- CARI_storm3_06_23_SPC[!duplicated(CARI_storm3_06_23_SPC$valuedatetime), ]

CARI_storm4_07_09_Q <- CARI_storm4_07_09_Q[!duplicated(CARI_storm4_07_09_Q$valuedatetime), ]
CARI_storm4_07_09_SPC <- CARI_storm4_07_09_SPC[!duplicated(CARI_storm4_07_09_SPC$valuedatetime), ]

CARI_storm5_07_13_Q <- CARI_storm5_07_13_Q[!duplicated(CARI_storm5_07_13_Q$valuedatetime), ]
CARI_storm5_07_13_SPC <- CARI_storm5_07_13_SPC[!duplicated(CARI_storm5_07_13_SPC$valuedatetime), ]

CARI_storm6_07_24_Q <- CARI_storm6_07_24_Q[!duplicated(CARI_storm6_07_24_Q$valuedatetime), ]
CARI_storm6_07_24_SPC <- CARI_storm6_07_24_SPC[!duplicated(CARI_storm6_07_24_SPC$valuedatetime), ]

CARI_storm7_07_27_Q <- CARI_storm7_07_27_Q[!duplicated(CARI_storm7_07_27_Q$valuedatetime), ]
CARI_storm7_07_27_SPC <- CARI_storm7_07_27_SPC[!duplicated(CARI_storm7_07_27_SPC$valuedatetime), ]

CARI_storm8a_08_02_Q <- CARI_storm8a_08_02_Q[!duplicated(CARI_storm8a_08_02_Q$valuedatetime), ]
CARI_storm8a_08_02_SPC <- CARI_storm8a_08_02_SPC[!duplicated(CARI_storm8a_08_02_SPC$valuedatetime), ]

CARI_storm8b_08_03_Q <- CARI_storm8b_08_03_Q[!duplicated(CARI_storm8b_08_03_Q$valuedatetime), ]
CARI_storm8b_08_03_SPC <- CARI_storm8b_08_03_SPC[!duplicated(CARI_storm8b_08_03_SPC$valuedatetime), ]

CARI_storm9_09_07_Q <- CARI_storm9_09_07_Q[!duplicated(CARI_storm9_09_07_Q$valuedatetime), ]
CARI_storm9_09_07_SPC <- CARI_storm9_09_07_SPC[!duplicated(CARI_storm9_09_07_SPC$valuedatetime), ]


#CARI_storm1_06_15_SPC.p = hyst_plot(CARI_storm1_06_15_Q, CARI_storm1_06_15_SPC, "CARI", "SPC", "0615")
CARI_storm2a_06_19_SPC.p = hyst_plot(CARI_storm2a_06_19_Q, CARI_storm2a_06_19_SPC, "CARI", "SPC", "0619a")
CARI_storm2b_06_20_SPC.p = hyst_plot(CARI_storm2b_06_20_Q, CARI_storm2b_06_20_SPC, "CARI", "SPC", "0620b")
CARI_storm2c_06_21_SPC.p = hyst_plot(CARI_storm2c_06_21_Q, CARI_storm2c_06_21_SPC, "CARI", "SPC", "0621c")
CARI_storm3_06_23_SPC.p = hyst_plot(CARI_storm3_06_23_Q, CARI_storm3_06_23_SPC, "CARI", "SPC", "0623")
CARI_storm4_07_09_SPC.p = hyst_plot(CARI_storm4_07_09_Q, CARI_storm4_07_09_SPC, "CARI", "SPC", "0709")
CARI_storm5_07_13_SPC.p = hyst_plot(CARI_storm5_07_13_Q, CARI_storm5_07_13_SPC, "CARI", "SPC", "0713")
CARI_storm6_07_24_SPC.p = hyst_plot(CARI_storm6_07_24_Q, CARI_storm6_07_24_SPC, "CARI", "SPC", "0724")
CARI_storm7_07_27_SPC.p = hyst_plot(CARI_storm7_07_27_Q, CARI_storm7_07_27_SPC, "CARI", "SPC", "0727")
CARI_storm8a_08_02_SPC.p = hyst_plot(CARI_storm8a_08_02_Q, CARI_storm8a_08_02_SPC, "CARI", "SPC", "0802a")
CARI_storm8b_08_03_SPC.p = hyst_plot(CARI_storm8b_08_03_Q, CARI_storm8b_08_03_SPC, "CARI", "SPC", "0803b")
CARI_storm9_09_07_SPC.p = hyst_plot(CARI_storm9_09_07_Q, CARI_storm9_09_07_SPC, "CARI", "SPC", "0907")

#turb #
#CARI_storm2a_06_19_turb <- CARI_storm2a_06_19_turb[!duplicated(CARI_storm2a_06_19_turb$valuedatetime), ]
CARI_storm2b_06_20_turb <- CARI_storm2b_06_20_turb[!duplicated(CARI_storm2b_06_20_turb$valuedatetime), ]
CARI_storm3_06_23_turb <- CARI_storm3_06_23_turb[!duplicated(CARI_storm3_06_23_turb$valuedatetime), ]
CARI_storm4_07_09_turb <- CARI_storm4_07_09_turb[!duplicated(CARI_storm4_07_09_turb$valuedatetime), ]
CARI_storm5_07_13_turb <- CARI_storm5_07_13_turb[!duplicated(CARI_storm5_07_13_turb$valuedatetime), ]
CARI_storm6_07_24_turb <- CARI_storm6_07_24_turb[!duplicated(CARI_storm6_07_24_turb$valuedatetime), ]
CARI_storm7_07_27_turb <- CARI_storm7_07_27_turb[!duplicated(CARI_storm7_07_27_turb$valuedatetime), ]
CARI_storm8a_08_02_turb <- CARI_storm8a_08_02_turb[!duplicated(CARI_storm8a_08_02_turb$valuedatetime), ]
CARI_storm8b_08_03_turb <- CARI_storm8b_08_03_turb[!duplicated(CARI_storm8b_08_03_turb$valuedatetime), ]
CARI_storm9_09_07_turb <- CARI_storm9_09_07_turb[!duplicated(CARI_storm9_09_07_turb$valuedatetime), ]


#CARI_storm1_06_15_turb.p = hyst_plot(CARI_storm1_06_15_Q, CARI_storm1_06_15_turb, "CARI", "turb", "0615")
CARI_storm2a_06_19_turb.p = hyst_plot(CARI_storm2a_06_19_Q, CARI_storm2a_06_19_turb, "CARI", "turb", "0619a")
CARI_storm2b_06_20_turb.p = hyst_plot(CARI_storm2b_06_20_Q, CARI_storm2b_06_20_turb, "CARI", "turb", "0620b")
CARI_storm2c_06_21_turb.p = hyst_plot(CARI_storm2c_06_21_Q, CARI_storm2c_06_21_turb, "CARI", "turb", "0621c")
CARI_storm3_06_23_turb.p = hyst_plot(CARI_storm3_06_23_Q, CARI_storm3_06_23_turb, "CARI", "turb", "0623")
CARI_storm4_07_09_turb.p = hyst_plot(CARI_storm4_07_09_Q, CARI_storm4_07_09_turb, "CARI", "turb", "0709")
CARI_storm5_07_13_turb.p = hyst_plot(CARI_storm5_07_13_Q, CARI_storm5_07_13_turb, "CARI", "turb", "0713")
CARI_storm6_07_24_turb.p = hyst_plot(CARI_storm6_07_24_Q, CARI_storm6_07_24_turb, "CARI", "turb", "0724")
CARI_storm7_07_27_turb.p = hyst_plot(CARI_storm7_07_27_Q, CARI_storm7_07_27_turb, "CARI", "turb", "0727")
CARI_storm8a_08_02_turb.p = hyst_plot(CARI_storm8a_08_02_Q, CARI_storm8a_08_02_turb, "CARI", "turb", "0802a")
CARI_storm8b_08_03_turb.p = hyst_plot(CARI_storm8b_08_03_Q, CARI_storm8b_08_03_turb, "CARI", "turb", "0803b")
CARI_storm9_09_07_turb.p = hyst_plot(CARI_storm9_09_07_Q, CARI_storm9_09_07_turb, "CARI", "turb", "0907")

# Plot Loops #


multiplot(
  CARI_storm2a_06_19_NO3.p,CARI_storm2a_06_19_fDOM.p,CARI_storm2a_06_19_SPC.p,CARI_storm2a_06_19_turb.p,
  CARI_storm2b_06_20_NO3.p,CARI_storm2b_06_20_fDOM.p,CARI_storm2b_06_20_SPC.p,CARI_storm2b_06_20_turb.p,
  CARI_storm2c_06_21_NO3.p,
  CARI_storm3_06_23_NO3.p,CARI_storm3_06_23_fDOM.p,CARI_storm3_06_23_SPC.p,CARI_storm3_06_23_turb.p,
  CARI_storm4_07_09_NO3.p,CARI_storm4_07_09_fDOM.p,CARI_storm4_07_09_SPC.p,CARI_storm4_07_09_turb.p,
  CARI_storm5_07_13_NO3.p,CARI_storm5_07_13_fDOM.p,CARI_storm5_07_13_SPC.p,CARI_storm5_07_13_turb.p,
  CARI_storm6_07_24_NO3.p,CARI_storm6_07_24_fDOM.p,CARI_storm6_07_24_SPC.p,CARI_storm6_07_24_turb.p,
  CARI_storm7_07_27_NO3.p,CARI_storm7_07_27_fDOM.p,CARI_storm7_07_27_SPC.p,CARI_storm7_07_27_turb.p,
  CARI_storm8a_08_02_NO3.p,CARI_storm8a_08_02_fDOM.p,CARI_storm8a_08_02_SPC.p,CARI_storm8a_08_02_turb.p,
  CARI_storm8b_08_03_NO3.p,CARI_storm8b_08_03_fDOM.p,CARI_storm8b_08_03_SPC.p,CARI_storm8b_08_03_turb.p,
  CARI_storm9_09_07_NO3.p,CARI_storm9_09_07_fDOM.p,CARI_storm9_09_07_SPC.p,CARI_storm9_09_07_turb.p,
  cols = 7)


# export pdf 20 x 30 #
ggsave("CARI_HI_Loops_2020.pdf",
       path = here("plots", "HI_plots", "2020", "CARI"),
       width = 20, height = 30, units = "in")




####################################### 2021  ####

# plot on normalized scale # 
#### load data #####
VAUL_storm1b_07_27_Q <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm1b_07_27_Q.csv"))
VAUL_storm1b_07_27_NO3 <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm1b_07_27_NO3.csv"))
VAUL_storm1b_07_27_fDOM <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm1b_07_27_fDOM.csv"))
VAUL_storm1b_07_27_SPC <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm1b_07_27_SPC.csv"))
VAUL_storm1b_07_27_turb <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm1b_07_27_turb.csv"))
VAUL_storm1b_07_27_abs <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm1b_07_27_abs.csv"))

VAUL_storm3_08_08_Q <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm3_08_08_Q.csv"))
VAUL_storm3_08_08_NO3 <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm3_08_08_NO3.csv"))
VAUL_storm3_08_08_fDOM <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm3_08_08_fDOM.csv"))
VAUL_storm3_08_08_SPC <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm3_08_08_SPC.csv"))
VAUL_storm3_08_08_turb <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm3_08_08_turb.csv"))
VAUL_storm3_08_08_abs <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm3_08_08_abs.csv"))

VAUL_storm4a_08_15_Q <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4a_08_15_Q.csv"))
VAUL_storm4a_08_15_NO3 <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4a_08_15_NO3.csv"))
VAUL_storm4a_08_15_fDOM <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4a_08_15_fDOM.csv"))
VAUL_storm4a_08_15_SPC <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4a_08_15_SPC.csv"))
VAUL_storm4a_08_15_turb <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4a_08_15_turb.csv"))
VAUL_storm4a_08_15_abs <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4a_08_15_abs.csv"))

VAUL_storm4b_08_20_Q <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4b_08_20_Q.csv"))
VAUL_storm4b_08_20_NO3 <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4b_08_20_NO3.csv"))
VAUL_storm4b_08_20_fDOM <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4b_08_20_fDOM.csv"))
VAUL_storm4b_08_20_SPC <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4b_08_20_SPC.csv"))
VAUL_storm4b_08_20_turb <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4b_08_20_turb.csv"))
VAUL_storm4b_08_20_abs <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm4b_08_20_abs.csv"))

VAUL_storm5a_08_23_Q <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5a_08_23_Q.csv"))
VAUL_storm5a_08_23_NO3 <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5a_08_23_NO3.csv"))
VAUL_storm5a_08_23_fDOM <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5a_08_23_fDOM.csv"))
VAUL_storm5a_08_23_SPC <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5a_08_23_SPC.csv"))
VAUL_storm5a_08_23_turb <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5a_08_23_turb.csv"))
VAUL_storm5a_08_23_abs <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5a_08_23_abs.csv"))

VAUL_storm5b_08_26_Q <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5b_08_26_Q.csv"))
VAUL_storm5b_08_26_NO3 <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5b_08_26_NO3.csv"))
VAUL_storm5b_08_26_fDOM <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5b_08_26_fDOM.csv"))
VAUL_storm5b_08_26_SPC <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5b_08_26_SPC.csv"))
VAUL_storm5b_08_26_turb <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5b_08_26_turb.csv"))
VAUL_storm5b_08_26_abs <- read_csv(here("Storm_Events", "2021", "VAUL", "VAUL_storm5b_08_26_abs.csv"))

# STRT # 
STRT_storm1a_08_15_Q <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1a_08_15_Q.csv"))
STRT_storm1a_08_15_NO3 <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1a_08_15_NO3.csv"))
STRT_storm1a_08_15_fDOM <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1a_08_15_fDOM.csv"))
STRT_storm1a_08_15_SPC <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1a_08_15_SPC.csv"))
STRT_storm1a_08_15_turb <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1a_08_15_turb.csv"))
STRT_storm1a_08_15_abs <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1a_08_15_abs.csv"))

STRT_storm1b_08_17_Q <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1b_08_17_Q.csv"))
STRT_storm1b_08_17_NO3 <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1b_08_17_NO3.csv"))
STRT_storm1b_08_17_fDOM <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1b_08_17_fDOM.csv"))
STRT_storm1b_08_17_SPC <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1b_08_17_SPC.csv"))
STRT_storm1b_08_17_turb <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1b_08_17_turb.csv"))
STRT_storm1b_08_17_abs <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm1b_08_17_abs.csv"))

STRT_storm2a_08_19_Q <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2a_08_19_Q.csv"))
STRT_storm2a_08_19_NO3 <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2a_08_19_NO3.csv"))
STRT_storm2a_08_19_fDOM <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2a_08_19_fDOM.csv"))
STRT_storm2a_08_19_SPC <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2a_08_19_SPC.csv"))
STRT_storm2a_08_19_turb <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2a_08_19_turb.csv"))
STRT_storm2a_08_19_abs <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2a_08_19_abs.csv"))

STRT_storm2b_08_20_Q <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2b_08_20_Q.csv"))
STRT_storm2b_08_20_NO3 <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2b_08_20_NO3.csv"))
STRT_storm2b_08_20_fDOM <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2b_08_20_fDOM.csv"))
STRT_storm2b_08_20_SPC <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2b_08_20_SPC.csv"))
STRT_storm2b_08_20_turb <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2b_08_20_turb.csv"))
STRT_storm2b_08_20_abs <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm2b_08_20_abs.csv"))

STRT_storm3_08_25_Q <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm3_08_25_Q.csv"))
STRT_storm3_08_25_NO3 <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm3_08_25_NO3.csv"))
STRT_storm3_08_25_fDOM <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm3_08_25_fDOM.csv"))
STRT_storm3_08_25_SPC <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm3_08_25_SPC.csv"))
STRT_storm3_08_25_turb <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm3_08_25_turb.csv"))
STRT_storm3_08_25_abs <- read_csv(here("Storm_Events", "2021", "STRT", "STRT_storm3_08_25_abs.csv"))

# POKE # 
POKE_storm1_05_16_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm1_05_16_Q.csv"))
POKE_storm1_05_16_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm1_05_16_NO3.csv"))
POKE_storm1_05_16_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm1_05_16_fDOM.csv"))
POKE_storm1_05_16_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm1_05_16_SPC.csv"))
POKE_storm1_05_16_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm1_05_16_turb.csv"))
POKE_storm1_05_16_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm1_05_16_abs.csv"))

POKE_storm2_06_01_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm2_06_01_Q.csv"))
POKE_storm2_06_01_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm2_06_01_NO3.csv"))
POKE_storm2_06_01_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm2_06_01_fDOM.csv"))
POKE_storm2_06_01_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm2_06_01_SPC.csv"))
POKE_storm2_06_01_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm2_06_01_turb.csv"))
POKE_storm2_06_01_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm2_06_01_abs.csv"))

POKE_storm3_06_19_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm3_06_19_Q.csv"))
POKE_storm3_06_19_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm3_06_19_NO3.csv"))
POKE_storm3_06_19_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm3_06_19_fDOM.csv"))
POKE_storm3_06_19_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm3_06_19_SPC.csv"))
POKE_storm3_06_19_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm3_06_19_turb.csv"))
POKE_storm3_06_19_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm3_06_19_abs.csv"))

POKE_storm4_07_23_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm4_07_23_Q.csv"))
POKE_storm4_07_23_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm4_07_23_NO3.csv"))
POKE_storm4_07_23_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm4_07_23_fDOM.csv"))
POKE_storm4_07_23_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm4_07_23_SPC.csv"))
POKE_storm4_07_23_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm4_07_23_turb.csv"))
POKE_storm4_07_23_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm4_07_23_abs.csv"))

POKE_storm5_07_27_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm5_07_27_Q.csv"))
POKE_storm5_07_27_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm5_07_27_NO3.csv"))
POKE_storm5_07_27_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm5_07_27_fDOM.csv"))
POKE_storm5_07_27_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm5_07_27_SPC.csv"))
POKE_storm5_07_27_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm5_07_27_turb.csv"))
POKE_storm5_07_27_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm5_07_27_abs.csv"))

POKE_storm6_08_08_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm6_08_08_Q.csv"))
POKE_storm6_08_08_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm6_08_08_NO3.csv"))
POKE_storm6_08_08_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm6_08_08_fDOM.csv"))
POKE_storm6_08_08_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm6_08_08_SPC.csv"))
POKE_storm6_08_08_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm6_08_08_turb.csv"))
POKE_storm6_08_08_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm6_08_08_abs.csv"))

POKE_storm7a_08_14_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7a_08_14_Q.csv"))
POKE_storm7a_08_14_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7a_08_14_NO3.csv"))
POKE_storm7a_08_14_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7a_08_14_fDOM.csv"))
POKE_storm7a_08_14_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7a_08_14_SPC.csv"))
POKE_storm7a_08_14_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7a_08_14_turb.csv"))
POKE_storm7a_08_14_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7a_08_14_abs.csv"))

POKE_storm7b_08_19_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7b_08_19_Q.csv"))
POKE_storm7b_08_19_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7b_08_19_NO3.csv"))
POKE_storm7b_08_19_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7b_08_19_fDOM.csv"))
POKE_storm7b_08_19_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7b_08_19_SPC.csv"))
POKE_storm7b_08_19_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7b_08_19_turb.csv"))
POKE_storm7b_08_19_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7b_08_19_abs.csv"))

POKE_storm7c_08_23_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7c_08_23_Q.csv"))
POKE_storm7c_08_23_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7c_08_23_NO3.csv"))
POKE_storm7c_08_23_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7c_08_23_fDOM.csv"))
POKE_storm7c_08_23_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7c_08_23_SPC.csv"))
POKE_storm7c_08_23_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7c_08_23_turb.csv"))
POKE_storm7c_08_23_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7c_08_23_abs.csv"))

POKE_storm7d_08_26_Q <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7d_08_26_Q.csv"))
POKE_storm7d_08_26_NO3 <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7d_08_26_NO3.csv"))
POKE_storm7d_08_26_fDOM <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7d_08_26_fDOM.csv"))
POKE_storm7d_08_26_SPC <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7d_08_26_SPC.csv"))
POKE_storm7d_08_26_turb <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7d_08_26_turb.csv"))
POKE_storm7d_08_26_abs <- read_csv(here("Storm_Events", "2021", "POKE", "POKE_storm7d_08_26_abs.csv"))

# MOOS # 
MOOS_storm1_07_23_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm1_07_23_Q.csv"))
MOOS_storm1_07_23_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm1_07_23_NO3.csv"))
MOOS_storm1_07_23_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm1_07_23_fDOM.csv"))
MOOS_storm1_07_23_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm1_07_23_SPC.csv"))
MOOS_storm1_07_23_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm1_07_23_turb.csv"))
MOOS_storm1_07_23_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm1_07_23_abs.csv"))

MOOS_storm2_07_27_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm2_07_27_Q.csv"))
MOOS_storm2_07_27_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm2_07_27_NO3.csv"))
MOOS_storm2_07_27_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm2_07_27_fDOM.csv"))
MOOS_storm2_07_27_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm2_07_27_SPC.csv"))
MOOS_storm2_07_27_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm2_07_27_turb.csv"))
MOOS_storm2_07_27_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm2_07_27_abs.csv"))

MOOS_storm3a_08_06_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3a_08_06_Q.csv"))
MOOS_storm3a_08_06_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3a_08_06_NO3.csv"))
MOOS_storm3a_08_06_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3a_08_06_fDOM.csv"))
MOOS_storm3a_08_06_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3a_08_06_SPC.csv"))
MOOS_storm3a_08_06_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3a_08_06_turb.csv"))
MOOS_storm3a_08_06_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3a_08_06_abs.csv"))

MOOS_storm3b_08_08_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3b_08_08_Q.csv"))
MOOS_storm3b_08_08_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3b_08_08_NO3.csv"))
MOOS_storm3b_08_08_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3b_08_08_fDOM.csv"))
MOOS_storm3b_08_08_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3b_08_08_SPC.csv"))
MOOS_storm3b_08_08_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3b_08_08_turb.csv"))
MOOS_storm3b_08_08_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm3b_08_08_abs.csv"))

MOOS_storm4a_08_15_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4a_08_15_Q.csv"))
MOOS_storm4a_08_15_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4a_08_15_NO3.csv"))
MOOS_storm4a_08_15_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4a_08_15_fDOM.csv"))
MOOS_storm4a_08_15_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4a_08_15_SPC.csv"))
MOOS_storm4a_08_15_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4a_08_15_turb.csv"))
MOOS_storm4a_08_15_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4a_08_15_abs.csv"))

MOOS_storm4b_08_17_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4b_08_17_Q.csv"))
MOOS_storm4b_08_17_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4b_08_17_NO3.csv"))
MOOS_storm4b_08_17_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4b_08_17_fDOM.csv"))
MOOS_storm4b_08_17_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4b_08_17_SPC.csv"))
MOOS_storm4b_08_17_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4b_08_17_turb.csv"))
MOOS_storm4b_08_17_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm4b_08_17_abs.csv"))

MOOS_storm5a_08_19_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5a_08_19_Q.csv"))
MOOS_storm5a_08_19_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5a_08_19_NO3.csv"))
MOOS_storm5a_08_19_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5a_08_19_fDOM.csv"))
MOOS_storm5a_08_19_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5a_08_19_SPC.csv"))
MOOS_storm5a_08_19_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5a_08_19_turb.csv"))
MOOS_storm5a_08_19_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5a_08_19_abs.csv"))

MOOS_storm5b_08_21_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5b_08_21_Q.csv"))
MOOS_storm5b_08_21_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5b_08_21_NO3.csv"))
MOOS_storm5b_08_21_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5b_08_21_fDOM.csv"))
MOOS_storm5b_08_21_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5b_08_21_SPC.csv"))
MOOS_storm5b_08_21_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5b_08_21_turb.csv"))
MOOS_storm5b_08_21_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm5b_08_21_abs.csv"))

MOOS_storm6_08_25_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm6_08_25_Q.csv"))
MOOS_storm6_08_25_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm6_08_25_NO3.csv"))
MOOS_storm6_08_25_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm6_08_25_fDOM.csv"))
MOOS_storm6_08_25_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm6_08_25_SPC.csv"))
MOOS_storm6_08_25_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm6_08_25_turb.csv"))
MOOS_storm6_08_25_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm6_08_25_abs.csv"))

MOOS_storm7_08_27_Q <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm7_08_27_Q.csv"))
MOOS_storm7_08_27_NO3 <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm7_08_27_NO3.csv"))
MOOS_storm7_08_27_fDOM <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm7_08_27_fDOM.csv"))
MOOS_storm7_08_27_SPC <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm7_08_27_SPC.csv"))
MOOS_storm7_08_27_turb <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm7_08_27_turb.csv"))
MOOS_storm7_08_27_abs <- read_csv(here("Storm_Events", "2021", "MOOS", "MOOS_storm7_08_27_abs.csv"))

# FRCH # 
FRCH_storm2_07_27_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm2_07_27_Q.csv"))
FRCH_storm2_07_27_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm2_07_27_NO3.csv"))
FRCH_storm2_07_27_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm2_07_27_fDOM.csv"))
FRCH_storm2_07_27_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm2_07_27_SPC.csv"))
FRCH_storm2_07_27_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm2_07_27_turb.csv"))
FRCH_storm2_07_27_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm2_07_27_abs.csv"))

FRCH_storm3_08_05_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm3_08_05_Q.csv"))
FRCH_storm3_08_05_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm3_08_05_NO3.csv"))
FRCH_storm3_08_05_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm3_08_05_fDOM.csv"))
FRCH_storm3_08_05_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm3_08_05_SPC.csv"))
FRCH_storm3_08_05_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm3_08_05_turb.csv"))
FRCH_storm3_08_05_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm3_08_05_abs.csv"))

FRCH_storm4_08_08_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm4_08_08_Q.csv"))
FRCH_storm4_08_08_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm4_08_08_NO3.csv"))
FRCH_storm4_08_08_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm4_08_08_fDOM.csv"))
FRCH_storm4_08_08_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm4_08_08_SPC.csv"))
FRCH_storm4_08_08_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm4_08_08_turb.csv"))
FRCH_storm4_08_08_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm4_08_08_abs.csv"))

FRCH_storm5a_08_15_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5a_08_15_Q.csv"))
FRCH_storm5a_08_15_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5a_08_15_NO3.csv"))
FRCH_storm5a_08_15_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5a_08_15_fDOM.csv"))
FRCH_storm5a_08_15_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5a_08_15_SPC.csv"))
FRCH_storm5a_08_15_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5a_08_15_turb.csv"))
FRCH_storm5a_08_15_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5a_08_15_abs.csv"))

FRCH_storm5b_08_17_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5b_08_17_Q.csv"))
FRCH_storm5b_08_17_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5b_08_17_NO3.csv"))
FRCH_storm5b_08_17_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5b_08_17_fDOM.csv"))
FRCH_storm5b_08_17_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5b_08_17_SPC.csv"))
FRCH_storm5b_08_17_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5b_08_17_turb.csv"))
FRCH_storm5b_08_17_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm5b_08_17_abs.csv"))

FRCH_storm6a_08_19_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6a_08_19_Q.csv"))
FRCH_storm6a_08_19_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6a_08_19_NO3.csv"))
FRCH_storm6a_08_19_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6a_08_19_fDOM.csv"))
FRCH_storm6a_08_19_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6a_08_19_SPC.csv"))
FRCH_storm6a_08_19_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6a_08_19_turb.csv"))
FRCH_storm6a_08_19_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6a_08_19_abs.csv"))

FRCH_storm6b_08_20_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6b_08_20_Q.csv"))
FRCH_storm6b_08_20_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6b_08_20_NO3.csv"))
FRCH_storm6b_08_20_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6b_08_20_fDOM.csv"))
FRCH_storm6b_08_20_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6b_08_20_SPC.csv"))
FRCH_storm6b_08_20_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6b_08_20_turb.csv"))
FRCH_storm6b_08_20_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm6b_08_20_abs.csv"))

FRCH_storm7_08_25_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm7_08_25_Q.csv"))
FRCH_storm7_08_25_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm7_08_25_NO3.csv"))
FRCH_storm7_08_25_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm7_08_25_fDOM.csv"))
FRCH_storm7_08_25_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm7_08_25_SPC.csv"))
FRCH_storm7_08_25_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm7_08_25_turb.csv"))
FRCH_storm7_08_25_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm7_08_25_abs.csv"))

FRCH_storm8_08_27_Q <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm8_08_27_Q.csv"))
FRCH_storm8_08_27_NO3 <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm8_08_27_NO3.csv"))
FRCH_storm8_08_27_fDOM <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm8_08_27_fDOM.csv"))
FRCH_storm8_08_27_SPC <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm8_08_27_SPC.csv"))
FRCH_storm8_08_27_turb <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm8_08_27_turb.csv"))
FRCH_storm8_08_27_abs <- read_csv(here("Storm_Events", "2021", "FRCH", "FRCH_storm8_08_27_abs.csv"))

# CARI # 
CARI_storm1_05_16_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm1_05_16_Q.csv"))
CARI_storm1_05_16_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm1_05_16_NO3.csv"))
CARI_storm1_05_16_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm1_05_16_fDOM.csv"))
CARI_storm1_05_16_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm1_05_16_SPC.csv"))
CARI_storm1_05_16_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm1_05_16_turb.csv"))

CARI_storm2_06_01_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm2_06_01_Q.csv"))
CARI_storm2_06_01_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm2_06_01_NO3.csv"))
CARI_storm2_06_01_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm2_06_01_fDOM.csv"))
CARI_storm2_06_01_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm2_06_01_SPC.csv"))
CARI_storm2_06_01_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm2_06_01_turb.csv"))

CARI_storm3_06_19_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm3_06_19_Q.csv"))
CARI_storm3_06_19_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm3_06_19_NO3.csv"))
CARI_storm3_06_19_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm3_06_19_fDOM.csv"))
CARI_storm3_06_19_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm3_06_19_SPC.csv"))
CARI_storm3_06_19_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm3_06_19_turb.csv"))

CARI_storm4_07_24_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm4_07_24_Q.csv"))
CARI_storm4_07_24_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm4_07_24_NO3.csv"))
CARI_storm4_07_24_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm4_07_24_fDOM.csv"))
CARI_storm4_07_24_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm4_07_24_SPC.csv"))
CARI_storm4_07_24_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm4_07_24_turb.csv"))

CARI_storm5_07_27_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm5_07_27_Q.csv"))
CARI_storm5_07_27_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm5_07_27_NO3.csv"))
CARI_storm5_07_27_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm5_07_27_fDOM.csv"))
CARI_storm5_07_27_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm5_07_27_SPC.csv"))
CARI_storm5_07_27_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm5_07_27_turb.csv"))

CARI_storm6_08_08_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm6_08_08_Q.csv"))
CARI_storm6_08_08_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm6_08_08_NO3.csv"))
CARI_storm6_08_08_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm6_08_08_fDOM.csv"))
CARI_storm6_08_08_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm6_08_08_SPC.csv"))
CARI_storm6_08_08_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm6_08_08_turb.csv"))

CARI_storm7_08_15_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm7_08_15_Q.csv"))
CARI_storm7_08_15_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm7_08_15_NO3.csv"))
CARI_storm7_08_15_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm7_08_15_fDOM.csv"))
CARI_storm7_08_15_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm7_08_15_SPC.csv"))
CARI_storm7_08_15_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm7_08_15_turb.csv"))

CARI_storm8_08_20_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm8_08_20_Q.csv"))
CARI_storm8_08_20_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm8_08_20_NO3.csv"))
CARI_storm8_08_20_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm8_08_20_fDOM.csv"))
CARI_storm8_08_20_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm8_08_20_SPC.csv"))
CARI_storm8_08_20_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm8_08_20_turb.csv"))

CARI_storm9_08_23_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm9_08_23_Q.csv"))
CARI_storm9_08_23_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm9_08_23_NO3.csv"))
CARI_storm9_08_23_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm9_08_23_fDOM.csv"))
CARI_storm9_08_23_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm9_08_23_SPC.csv"))
CARI_storm9_08_23_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm9_08_23_turb.csv"))

CARI_storm10_08_27_Q <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm10_08_27_Q.csv"))
CARI_storm10_08_27_NO3 <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm10_08_27_NO3.csv"))
CARI_storm10_08_27_fDOM <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm10_08_27_fDOM.csv"))
CARI_storm10_08_27_SPC <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm10_08_27_SPC.csv"))
CARI_storm10_08_27_turb <- read_csv(here("Storm_Events", "2021", "CARI", "CARI_storm10_08_27_turb.csv"))

# normalize data #
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

#### fxn: plot hysteresis loop ###
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

# plot VAUL loops #
# NO3

VAUL_storm1b_07_27_NO3.p = hyst_plot(VAUL_storm1b_07_27_Q, VAUL_storm1b_07_27_NO3, "VAUL", "NO3", "0727b")
VAUL_storm3_08_08_NO3.p = hyst_plot(VAUL_storm3_08_08_Q, VAUL_storm3_08_08_NO3, "VAUL", "NO3", "0808")
VAUL_storm4a_08_15_NO3.p = hyst_plot(VAUL_storm4a_08_15_Q, VAUL_storm4a_08_15_NO3, "VAUL", "NO3", "0815a")
VAUL_storm4b_08_20_NO3.p = hyst_plot(VAUL_storm4b_08_20_Q, VAUL_storm4b_08_20_NO3, "VAUL", "NO3", "0820b")
VAUL_storm5a_08_23_NO3.p = hyst_plot(VAUL_storm5a_08_23_Q, VAUL_storm5a_08_23_NO3, "VAUL", "NO3", "0823a")
VAUL_storm5b_08_26_NO3.p = hyst_plot(VAUL_storm5b_08_26_Q, VAUL_storm5b_08_26_NO3, "VAUL", "NO3", "0826b")

# fDOM #
VAUL_storm1b_07_27_fDOM.p = hyst_plot(VAUL_storm1b_07_27_Q, VAUL_storm1b_07_27_fDOM, "VAUL", "fDOM", "0727b")
VAUL_storm3_08_08_fDOM.p = hyst_plot(VAUL_storm3_08_08_Q, VAUL_storm3_08_08_fDOM, "VAUL", "fDOM", "0808")
VAUL_storm4a_08_15_fDOM.p = hyst_plot(VAUL_storm4a_08_15_Q, VAUL_storm4a_08_15_fDOM, "VAUL", "fDOM", "0815a")
VAUL_storm4b_08_20_fDOM.p = hyst_plot(VAUL_storm4b_08_20_Q, VAUL_storm4b_08_20_fDOM, "VAUL", "fDOM", "0820b")
VAUL_storm5a_08_23_fDOM.p = hyst_plot(VAUL_storm5a_08_23_Q, VAUL_storm5a_08_23_fDOM, "VAUL", "fDOM", "0823a")
VAUL_storm5b_08_26_fDOM.p = hyst_plot(VAUL_storm5b_08_26_Q, VAUL_storm5b_08_26_fDOM, "VAUL", "fDOM", "0826b")

# SPC #
VAUL_storm1b_07_27_SPC.p = hyst_plot(VAUL_storm1b_07_27_Q, VAUL_storm1b_07_27_SPC, "VAUL", "SPC", "0727b")
VAUL_storm3_08_08_SPC.p = hyst_plot(VAUL_storm3_08_08_Q, VAUL_storm3_08_08_SPC, "VAUL", "SPC", "0808")
VAUL_storm4a_08_15_SPC.p = hyst_plot(VAUL_storm4a_08_15_Q, VAUL_storm4a_08_15_SPC, "VAUL", "SPC", "0815a")
VAUL_storm4b_08_20_SPC.p = hyst_plot(VAUL_storm4b_08_20_Q, VAUL_storm4b_08_20_SPC, "VAUL", "SPC", "0820b")
VAUL_storm5a_08_23_SPC.p = hyst_plot(VAUL_storm5a_08_23_Q, VAUL_storm5a_08_23_SPC, "VAUL", "SPC", "0823a")
VAUL_storm5b_08_26_SPC.p = hyst_plot(VAUL_storm5b_08_26_Q, VAUL_storm5b_08_26_SPC, "VAUL", "SPC", "0826b")

# turb
VAUL_storm1b_07_27_turb.p = hyst_plot(VAUL_storm1b_07_27_Q, VAUL_storm1b_07_27_turb, "VAUL", "turb", "0727b")
VAUL_storm3_08_08_turb.p = hyst_plot(VAUL_storm3_08_08_Q, VAUL_storm3_08_08_turb, "VAUL", "turb", "0808")
VAUL_storm4a_08_15_turb.p = hyst_plot(VAUL_storm4a_08_15_Q, VAUL_storm4a_08_15_turb, "VAUL", "turb", "0815a")
VAUL_storm4b_08_20_turb.p = hyst_plot(VAUL_storm4b_08_20_Q, VAUL_storm4b_08_20_turb, "VAUL", "turb", "0820b")
VAUL_storm5a_08_23_turb.p = hyst_plot(VAUL_storm5a_08_23_Q, VAUL_storm5a_08_23_turb, "VAUL", "turb", "0823a")
VAUL_storm5b_08_26_turb.p = hyst_plot(VAUL_storm5b_08_26_Q, VAUL_storm5b_08_26_turb, "VAUL", "turb", "0826b")

# ABS #
VAUL_storm1b_07_27_abs.p = hyst_plot(VAUL_storm1b_07_27_Q, VAUL_storm1b_07_27_abs, "VAUL", "abs", "0727b")
VAUL_storm3_08_08_abs.p = hyst_plot(VAUL_storm3_08_08_Q, VAUL_storm3_08_08_abs, "VAUL", "abs", "0808")
VAUL_storm4a_08_15_abs.p = hyst_plot(VAUL_storm4a_08_15_Q, VAUL_storm4a_08_15_abs, "VAUL", "abs", "0815a")
VAUL_storm4b_08_20_abs.p = hyst_plot(VAUL_storm4b_08_20_Q, VAUL_storm4b_08_20_abs, "VAUL", "abs", "0820b")
VAUL_storm5a_08_23_abs.p = hyst_plot(VAUL_storm5a_08_23_Q, VAUL_storm5a_08_23_abs, "VAUL", "abs", "0823a")
VAUL_storm5b_08_26_abs.p = hyst_plot(VAUL_storm5b_08_26_Q, VAUL_storm5b_08_26_abs, "VAUL", "abs", "0826b")

# Multiplots of VAUL storms #

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

multiplot(VAUL_storm1b_07_27_NO3.p, VAUL_storm1b_07_27_fDOM.p, VAUL_storm1b_07_27_SPC.p, VAUL_storm1b_07_27_turb.p) # works
multiplot(VAUL_storm3_08_08_NO3.p, VAUL_storm3_08_08_fDOM.p, VAUL_storm3_08_08_SPC.p, VAUL_storm3_08_08_turb.p) # works/empty
multiplot(VAUL_storm4a_08_15_NO3.p, VAUL_storm4a_08_15_fDOM.p, VAUL_storm4a_08_15_SPC.p, VAUL_storm4a_08_15_turb.p) # works 
multiplot(VAUL_storm4b_08_20_NO3.p, VAUL_storm4b_08_20_fDOM.p, VAUL_storm4b_08_20_SPC.p, VAUL_storm4b_08_20_turb.p) # works 
multiplot(VAUL_storm5a_08_23_NO3.p, VAUL_storm5a_08_23_fDOM.p, VAUL_storm5a_08_23_SPC.p, VAUL_storm5a_08_23_turb.p) # works 
multiplot(VAUL_storm5b_08_26_NO3.p, VAUL_storm5b_08_26_fDOM.p, VAUL_storm5b_08_26_SPC.p, VAUL_storm5b_08_26_turb.p) # works 


multiplot(
          VAUL_storm1b_07_27_NO3.p, VAUL_storm1b_07_27_fDOM.p, VAUL_storm1b_07_27_SPC.p, VAUL_storm1b_07_27_turb.p,
          
          VAUL_storm3_08_08_NO3.p, VAUL_storm3_08_08_fDOM.p, VAUL_storm3_08_08_SPC.p, VAUL_storm3_08_08_turb.p,
          VAUL_storm4a_08_15_NO3.p, VAUL_storm4a_08_15_fDOM.p, VAUL_storm4a_08_15_SPC.p, VAUL_storm4a_08_15_turb.p,
          VAUL_storm4b_08_20_NO3.p, VAUL_storm4b_08_20_fDOM.p, VAUL_storm4b_08_20_SPC.p, VAUL_storm4b_08_20_turb.p,
          VAUL_storm5a_08_23_NO3.p, VAUL_storm5a_08_23_fDOM.p, VAUL_storm5a_08_23_SPC.p, VAUL_storm5a_08_23_turb.p,
          VAUL_storm5b_08_26_NO3.p, VAUL_storm5b_08_26_fDOM.p, VAUL_storm5b_08_26_SPC.p, VAUL_storm5b_08_26_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("VAUL_HI_Loops_2021.pdf",
       path = here("plots", "HI_plots", "2021", "VAUL"),
       width = 20, height = 30, units = "in")


# STRT #
# plot STRT loops #
# NO3 #
STRT_storm1a_08_15_NO3.p = hyst_plot(STRT_storm1a_08_15_Q, STRT_storm1a_08_15_NO3, "STRT", "NO3", "0815a")
STRT_storm1b_08_17_NO3.p = hyst_plot(STRT_storm1b_08_17_Q, STRT_storm1b_08_17_NO3, "STRT", "NO3", "0817a")
STRT_storm2a_08_19_NO3.p = hyst_plot(STRT_storm2a_08_19_Q, STRT_storm2a_08_19_NO3, "STRT", "NO3", "0819a")
STRT_storm2b_08_20_NO3.p = hyst_plot(STRT_storm2b_08_20_Q, STRT_storm2b_08_20_NO3, "STRT", "NO3", "0820b")
STRT_storm3_08_25_NO3.p = hyst_plot(STRT_storm3_08_25_Q, STRT_storm3_08_25_NO3, "STRT", "NO3", "0825")

# fDOM #
STRT_storm1a_08_15_fDOM.p = hyst_plot(STRT_storm1a_08_15_Q, STRT_storm1a_08_15_fDOM, "STRT", "fDOM", "0815a")
STRT_storm1b_08_17_fDOM.p = hyst_plot(STRT_storm1b_08_17_Q, STRT_storm1b_08_17_fDOM, "STRT", "fDOM", "0817a")
STRT_storm2a_08_19_fDOM.p = hyst_plot(STRT_storm2a_08_19_Q, STRT_storm2a_08_19_fDOM, "STRT", "fDOM", "0819a")
STRT_storm2b_08_20_fDOM.p = hyst_plot(STRT_storm2b_08_20_Q, STRT_storm2b_08_20_fDOM, "STRT", "fDOM", "0820b")
STRT_storm3_08_25_fDOM.p = hyst_plot(STRT_storm3_08_25_Q, STRT_storm3_08_25_fDOM, "STRT", "fDOM", "0825")

# SPC # 
STRT_storm1a_08_15_SPC.p = hyst_plot(STRT_storm1a_08_15_Q, STRT_storm1a_08_15_SPC, "STRT", "SPC", "0815a")
STRT_storm1b_08_17_SPC.p = hyst_plot(STRT_storm1b_08_17_Q, STRT_storm1b_08_17_SPC, "STRT", "SPC", "0817a")
STRT_storm2a_08_19_SPC.p = hyst_plot(STRT_storm2a_08_19_Q, STRT_storm2a_08_19_SPC, "STRT", "SPC", "0819a")
STRT_storm2b_08_20_SPC.p = hyst_plot(STRT_storm2b_08_20_Q, STRT_storm2b_08_20_SPC, "STRT", "SPC", "0820b")
STRT_storm3_08_25_SPC.p = hyst_plot(STRT_storm3_08_25_Q, STRT_storm3_08_25_SPC, "STRT", "SPC", "0825")

# Turb #
STRT_storm1a_08_15_turb.p = hyst_plot(STRT_storm1a_08_15_Q, STRT_storm1a_08_15_turb, "STRT", "turb", "0815a")
STRT_storm1b_08_17_turb.p = hyst_plot(STRT_storm1b_08_17_Q, STRT_storm1b_08_17_turb, "STRT", "turb", "0817a")
STRT_storm2a_08_19_turb.p = hyst_plot(STRT_storm2a_08_19_Q, STRT_storm2a_08_19_turb, "STRT", "turb", "0819a")
STRT_storm2b_08_20_turb.p = hyst_plot(STRT_storm2b_08_20_Q, STRT_storm2b_08_20_turb, "STRT", "turb", "0820b")
STRT_storm3_08_25_turb.p = hyst_plot(STRT_storm3_08_25_Q, STRT_storm3_08_25_turb, "STRT", "turb", "0825")

# ABS # 
STRT_storm1a_08_15_abs.p = hyst_plot(STRT_storm1a_08_15_Q, STRT_storm1a_08_15_abs, "STRT", "abs", "0815a")
STRT_storm1b_08_17_abs.p = hyst_plot(STRT_storm1b_08_17_Q, STRT_storm1b_08_17_abs, "STRT", "abs", "0817a")
STRT_storm2a_08_19_abs.p = hyst_plot(STRT_storm2a_08_19_Q, STRT_storm2a_08_19_abs, "STRT", "abs", "0819a")
STRT_storm2b_08_20_abs.p = hyst_plot(STRT_storm2b_08_20_Q, STRT_storm2b_08_20_abs, "STRT", "abs", "0820b")
STRT_storm3_08_25_abs.p = hyst_plot(STRT_storm3_08_25_Q, STRT_storm3_08_25_abs, "STRT", "abs", "0825")

# Multiplots of STRT storms #

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

multiplot(STRT_storm1a_08_15_NO3.p,STRT_storm1a_08_15_fDOM.p,STRT_storm1a_08_15_SPC.p,STRT_storm1a_08_15_turb.p,
          STRT_storm1b_08_17_NO3.p,STRT_storm1b_08_17_fDOM.p,STRT_storm1b_08_17_SPC.p,STRT_storm1b_08_17_turb.p,
          STRT_storm2a_08_19_NO3.p,STRT_storm2a_08_19_fDOM.p,STRT_storm2a_08_19_SPC.p,STRT_storm2a_08_19_turb.p,
          STRT_storm2b_08_20_NO3.p,STRT_storm2b_08_20_fDOM.p,STRT_storm2b_08_20_SPC.p,STRT_storm2b_08_20_turb.p,
          STRT_storm3_08_25_NO3.p,STRT_storm3_08_25_fDOM.p,STRT_storm3_08_25_SPC.p,STRT_storm3_08_25_turb.p,
          cols = 7)


# export pdf 20 x 30 #
ggsave("STRT_HI_Loops_2021.pdf",
       path = here("plots", "HI_plots", "2021", "STRT"),
       width = 20, height = 30, units = "in")

# POKE #
# plot STRT loops #
# NO3 #
POKE_storm1_05_16_NO3.p = hyst_plot(POKE_storm1_05_16_Q, POKE_storm1_05_16_NO3, "POKE", "NO3", "0516")
POKE_storm2_06_01_NO3.p = hyst_plot(POKE_storm2_06_01_Q, POKE_storm2_06_01_NO3, "POKE", "NO3", "0601")
POKE_storm3_06_19_NO3.p = hyst_plot(POKE_storm3_06_19_Q, POKE_storm3_06_19_NO3, "POKE", "NO3", "0619")
POKE_storm4_07_23_NO3.p = hyst_plot(POKE_storm4_07_23_Q, POKE_storm4_07_23_NO3, "POKE", "NO3", "0723")
POKE_storm5_07_27_NO3.p = hyst_plot(POKE_storm5_07_27_Q, POKE_storm5_07_27_NO3, "POKE", "NO3", "0727")
POKE_storm6_08_08_NO3.p = hyst_plot(POKE_storm6_08_08_Q, POKE_storm6_08_08_NO3, "POKE", "NO3", "0808")
POKE_storm7a_08_14_NO3.p = hyst_plot(POKE_storm7a_08_14_Q, POKE_storm7a_08_14_NO3, "POKE", "NO3", "0814a")
POKE_storm7b_08_19_NO3.p = hyst_plot(POKE_storm7b_08_19_Q, POKE_storm7b_08_19_NO3, "POKE", "NO3", "0819b")
POKE_storm7c_08_23_NO3.p = hyst_plot(POKE_storm7c_08_23_Q, POKE_storm7c_08_23_NO3, "POKE", "NO3", "0823c")
POKE_storm7d_08_26_NO3.p = hyst_plot(POKE_storm7d_08_26_Q, POKE_storm7d_08_26_NO3, "POKE", "NO3", "0826d")

# fDOM # 
POKE_storm1_05_16_fDOM.p = hyst_plot(POKE_storm1_05_16_Q, POKE_storm1_05_16_fDOM, "POKE", "fDOM", "0516")
POKE_storm2_06_01_fDOM.p = hyst_plot(POKE_storm2_06_01_Q, POKE_storm2_06_01_fDOM, "POKE", "fDOM", "0601")
POKE_storm3_06_19_fDOM.p = hyst_plot(POKE_storm3_06_19_Q, POKE_storm3_06_19_fDOM, "POKE", "fDOM", "0619")
POKE_storm4_07_23_fDOM.p = hyst_plot(POKE_storm4_07_23_Q, POKE_storm4_07_23_fDOM, "POKE", "fDOM", "0723")
POKE_storm5_07_27_fDOM.p = hyst_plot(POKE_storm5_07_27_Q, POKE_storm5_07_27_fDOM, "POKE", "fDOM", "0727")
POKE_storm6_08_08_fDOM.p = hyst_plot(POKE_storm6_08_08_Q, POKE_storm6_08_08_fDOM, "POKE", "fDOM", "0808")

# SPC # 
POKE_storm1_05_16_SPC.p = hyst_plot(POKE_storm1_05_16_Q, POKE_storm1_05_16_SPC, "POKE", "SPC", "0516")
POKE_storm2_06_01_SPC.p = hyst_plot(POKE_storm2_06_01_Q, POKE_storm2_06_01_SPC, "POKE", "SPC", "0601")
POKE_storm3_06_19_SPC.p = hyst_plot(POKE_storm3_06_19_Q, POKE_storm3_06_19_SPC, "POKE", "SPC", "0619")
POKE_storm4_07_23_SPC.p = hyst_plot(POKE_storm4_07_23_Q, POKE_storm4_07_23_SPC, "POKE", "SPC", "0723")
POKE_storm5_07_27_SPC.p = hyst_plot(POKE_storm5_07_27_Q, POKE_storm5_07_27_SPC, "POKE", "SPC", "0727")
POKE_storm6_08_08_SPC.p = hyst_plot(POKE_storm6_08_08_Q, POKE_storm6_08_08_SPC, "POKE", "SPC", "0808")

# Turb # 
POKE_storm1_05_16_turb.p = hyst_plot(POKE_storm1_05_16_Q, POKE_storm1_05_16_turb, "POKE", "turb", "0516")
POKE_storm2_06_01_turb.p = hyst_plot(POKE_storm2_06_01_Q, POKE_storm2_06_01_turb, "POKE", "turb", "0601")
POKE_storm3_06_19_turb.p = hyst_plot(POKE_storm3_06_19_Q, POKE_storm3_06_19_turb, "POKE", "turb", "0619")
POKE_storm4_07_23_turb.p = hyst_plot(POKE_storm4_07_23_Q, POKE_storm4_07_23_turb, "POKE", "turb", "0723")
POKE_storm5_07_27_turb.p = hyst_plot(POKE_storm5_07_27_Q, POKE_storm5_07_27_turb, "POKE", "turb", "0727")
POKE_storm6_08_08_turb.p = hyst_plot(POKE_storm6_08_08_Q, POKE_storm6_08_08_turb, "POKE", "turb", "0808")

# ABS # 
POKE_storm1_05_16_abs.p = hyst_plot(POKE_storm1_05_16_Q, POKE_storm1_05_16_abs, "POKE", "abs", "0516")
POKE_storm2_06_01_abs.p = hyst_plot(POKE_storm2_06_01_Q, POKE_storm2_06_01_abs, "POKE", "abs", "0601")
POKE_storm3_06_19_abs.p = hyst_plot(POKE_storm3_06_19_Q, POKE_storm3_06_19_abs, "POKE", "abs", "0619")
POKE_storm4_07_23_abs.p = hyst_plot(POKE_storm4_07_23_Q, POKE_storm4_07_23_abs, "POKE", "abs", "0723")
POKE_storm5_07_27_abs.p = hyst_plot(POKE_storm5_07_27_Q, POKE_storm5_07_27_abs, "POKE", "abs", "0727")
POKE_storm6_08_08_abs.p = hyst_plot(POKE_storm6_08_08_Q, POKE_storm6_08_08_abs, "POKE", "abs", "0808")

# Multiplots of POKE storms #

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


multiplot(POKE_storm1_05_16_NO3.p, POKE_storm1_05_16_fDOM.p,POKE_storm1_05_16_SPC.p,POKE_storm1_05_16_turb.p,
          POKE_storm2_06_01_NO3.p,POKE_storm2_06_01_fDOM.p,POKE_storm2_06_01_SPC.p,POKE_storm2_06_01_turb.p,
          POKE_storm3_06_19_NO3.p,
          POKE_storm4_07_23_NO3.p,POKE_storm4_07_23_fDOM.p,POKE_storm4_07_23_SPC.p,POKE_storm4_07_23_turb.p,
          POKE_storm5_07_27_NO3.p,POKE_storm5_07_27_fDOM.p,POKE_storm5_07_27_SPC.p,POKE_storm5_07_27_turb.p,
          POKE_storm6_08_08_NO3.p,POKE_storm6_08_08_fDOM.p,POKE_storm6_08_08_SPC.p,POKE_storm6_08_08_turb.p,
          POKE_storm7a_08_14_NO3.p,
          POKE_storm7b_08_19_NO3.p,
          POKE_storm7c_08_23_NO3.p,
          POKE_storm7d_08_26_NO3.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("POKE_HI_Loops_2021.pdf",
       path = here("plots", "HI_plots", "2021", "POKE"),
       width = 20, height = 30, units = "in")


# plot MOOS loops #
# NO3
MOOS_storm1_07_23_NO3.p = hyst_plot(MOOS_storm1_07_23_Q, MOOS_storm1_07_23_NO3, "MOOS", "NO3", "0723")
MOOS_storm2_07_27_NO3.p = hyst_plot(MOOS_storm2_07_27_Q, MOOS_storm2_07_27_NO3, "MOOS", "NO3", "0727")
MOOS_storm3a_08_06_NO3.p = hyst_plot(MOOS_storm3a_08_06_Q, MOOS_storm3a_08_06_NO3, "MOOS", "NO3", "0806a")
MOOS_storm3b_08_08_NO3.p = hyst_plot(MOOS_storm3b_08_08_Q, MOOS_storm3b_08_08_NO3, "MOOS", "NO3", "0808b")
MOOS_storm4a_08_15_NO3.p = hyst_plot(MOOS_storm4a_08_15_Q, MOOS_storm4a_08_15_NO3, "MOOS", "NO3", "0815a")
MOOS_storm4b_08_17_NO3.p = hyst_plot(MOOS_storm4b_08_17_Q, MOOS_storm4b_08_17_NO3, "MOOS", "NO3", "0817b")
MOOS_storm5a_08_19_NO3.p = hyst_plot(MOOS_storm5a_08_19_Q, MOOS_storm5a_08_19_NO3, "MOOS", "NO3", "0819a")
MOOS_storm5b_08_21_NO3.p = hyst_plot(MOOS_storm5b_08_21_Q, MOOS_storm5b_08_21_NO3, "MOOS", "NO3", "0821b")
MOOS_storm6_08_25_NO3.p = hyst_plot(MOOS_storm6_08_25_Q, MOOS_storm6_08_25_NO3, "MOOS", "NO3", "0825")
MOOS_storm7_08_27_NO3.p = hyst_plot(MOOS_storm7_08_27_Q, MOOS_storm7_08_27_NO3, "MOOS", "NO3", "0827")

# fDOM #
MOOS_storm1_07_23_fDOM.p = hyst_plot(MOOS_storm1_07_23_Q, MOOS_storm1_07_23_fDOM, "MOOS", "fDOM", "0723")
MOOS_storm2_07_27_fDOM.p = hyst_plot(MOOS_storm2_07_27_Q, MOOS_storm2_07_27_fDOM, "MOOS", "fDOM", "0727")
MOOS_storm3a_08_06_fDOM.p = hyst_plot(MOOS_storm3a_08_06_Q, MOOS_storm3a_08_06_fDOM, "MOOS", "fDOM", "0806a")
MOOS_storm3b_08_08_fDOM.p = hyst_plot(MOOS_storm3b_08_08_Q, MOOS_storm3b_08_08_fDOM, "MOOS", "fDOM", "0808b")
MOOS_storm4a_08_15_fDOM.p = hyst_plot(MOOS_storm4a_08_15_Q, MOOS_storm4a_08_15_fDOM, "MOOS", "fDOM", "0815a")
MOOS_storm4b_08_17_fDOM.p = hyst_plot(MOOS_storm4b_08_17_Q, MOOS_storm4b_08_17_fDOM, "MOOS", "fDOM", "0817b")
MOOS_storm5a_08_19_fDOM.p = hyst_plot(MOOS_storm5a_08_19_Q, MOOS_storm5a_08_19_fDOM, "MOOS", "fDOM", "0819a")
MOOS_storm5b_08_21_fDOM.p = hyst_plot(MOOS_storm5b_08_21_Q, MOOS_storm5b_08_21_fDOM, "MOOS", "fDOM", "0821b")
MOOS_storm6_08_25_fDOM.p = hyst_plot(MOOS_storm6_08_25_Q, MOOS_storm6_08_25_fDOM, "MOOS", "fDOM", "0825")
MOOS_storm7_08_27_fDOM.p = hyst_plot(MOOS_storm7_08_27_Q, MOOS_storm7_08_27_fDOM, "MOOS", "fDOM", "0827")

# SPC #
MOOS_storm1_07_23_SPC.p = hyst_plot(MOOS_storm1_07_23_Q, MOOS_storm1_07_23_SPC, "MOOS", "SPC", "0723")
MOOS_storm2_07_27_SPC.p = hyst_plot(MOOS_storm2_07_27_Q, MOOS_storm2_07_27_SPC, "MOOS", "SPC", "0727")
MOOS_storm3a_08_06_SPC.p = hyst_plot(MOOS_storm3a_08_06_Q, MOOS_storm3a_08_06_SPC, "MOOS", "SPC", "0806a")
MOOS_storm3b_08_08_SPC.p = hyst_plot(MOOS_storm3b_08_08_Q, MOOS_storm3b_08_08_SPC, "MOOS", "SPC", "0808b")
MOOS_storm4a_08_15_SPC.p = hyst_plot(MOOS_storm4a_08_15_Q, MOOS_storm4a_08_15_SPC, "MOOS", "SPC", "0815a")
MOOS_storm4b_08_17_SPC.p = hyst_plot(MOOS_storm4b_08_17_Q, MOOS_storm4b_08_17_SPC, "MOOS", "SPC", "0817b")
MOOS_storm5a_08_19_SPC.p = hyst_plot(MOOS_storm5a_08_19_Q, MOOS_storm5a_08_19_SPC, "MOOS", "SPC", "0819a")
MOOS_storm5b_08_21_SPC.p = hyst_plot(MOOS_storm5b_08_21_Q, MOOS_storm5b_08_21_SPC, "MOOS", "SPC", "0821b")
MOOS_storm6_08_25_SPC.p = hyst_plot(MOOS_storm6_08_25_Q, MOOS_storm6_08_25_SPC, "MOOS", "SPC", "0825")
MOOS_storm7_08_27_SPC.p = hyst_plot(MOOS_storm7_08_27_Q, MOOS_storm7_08_27_SPC, "MOOS", "SPC", "0827")

# turb
MOOS_storm1_07_23_turb.p = hyst_plot(MOOS_storm1_07_23_Q, MOOS_storm1_07_23_turb, "MOOS", "turb", "0723")
MOOS_storm2_07_27_turb.p = hyst_plot(MOOS_storm2_07_27_Q, MOOS_storm2_07_27_turb, "MOOS", "turb", "0727")
MOOS_storm3a_08_06_turb.p = hyst_plot(MOOS_storm3a_08_06_Q, MOOS_storm3a_08_06_turb, "MOOS", "turb", "0806a")
MOOS_storm3b_08_08_turb.p = hyst_plot(MOOS_storm3b_08_08_Q, MOOS_storm3b_08_08_turb, "MOOS", "turb", "0808b")
MOOS_storm4a_08_15_turb.p = hyst_plot(MOOS_storm4a_08_15_Q, MOOS_storm4a_08_15_turb, "MOOS", "turb", "0815a")
MOOS_storm4b_08_17_turb.p = hyst_plot(MOOS_storm4b_08_17_Q, MOOS_storm4b_08_17_turb, "MOOS", "turb", "0817b")
MOOS_storm5a_08_19_turb.p = hyst_plot(MOOS_storm5a_08_19_Q, MOOS_storm5a_08_19_turb, "MOOS", "turb", "0819a")
MOOS_storm5b_08_21_turb.p = hyst_plot(MOOS_storm5b_08_21_Q, MOOS_storm5b_08_21_turb, "MOOS", "turb", "0821b")
MOOS_storm6_08_25_turb.p = hyst_plot(MOOS_storm6_08_25_Q, MOOS_storm6_08_25_turb, "MOOS", "turb", "0825")
MOOS_storm7_08_27_turb.p = hyst_plot(MOOS_storm7_08_27_Q, MOOS_storm7_08_27_turb, "MOOS", "turb", "0827")

# ABS #
MOOS_storm1_07_23_abs.p = hyst_plot(MOOS_storm1_07_23_Q, MOOS_storm1_07_23_abs, "MOOS", "abs", "0723")
MOOS_storm2_07_27_abs.p = hyst_plot(MOOS_storm2_07_27_Q, MOOS_storm2_07_27_abs, "MOOS", "abs", "0727")
MOOS_storm3a_08_06_abs.p = hyst_plot(MOOS_storm3a_08_06_Q, MOOS_storm3a_08_06_abs, "MOOS", "abs", "0806a")
MOOS_storm3b_08_08_abs.p = hyst_plot(MOOS_storm3b_08_08_Q, MOOS_storm3b_08_08_abs, "MOOS", "abs", "0808b")
MOOS_storm4a_08_15_abs.p = hyst_plot(MOOS_storm4a_08_15_Q, MOOS_storm4a_08_15_abs, "MOOS", "abs", "0815a")
MOOS_storm4b_08_17_abs.p = hyst_plot(MOOS_storm4b_08_17_Q, MOOS_storm4b_08_17_abs, "MOOS", "abs", "0817b")
MOOS_storm5a_08_19_abs.p = hyst_plot(MOOS_storm5a_08_19_Q, MOOS_storm5a_08_19_abs, "MOOS", "abs", "0819a")
MOOS_storm5b_08_21_abs.p = hyst_plot(MOOS_storm5b_08_21_Q, MOOS_storm5b_08_21_abs, "MOOS", "abs", "0821b")
MOOS_storm6_08_25_abs.p = hyst_plot(MOOS_storm6_08_25_Q, MOOS_storm6_08_25_abs, "MOOS", "abs", "0825")
MOOS_storm7_08_27_abs.p = hyst_plot(MOOS_storm7_08_27_Q, MOOS_storm7_08_27_abs, "MOOS", "abs", "0827")

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

multiplot(MOOS_storm1_07_23_fDOM.p, MOOS_storm1_07_23_SPC.p, MOOS_storm1_07_23_turb.p,
          MOOS_storm2_07_27_fDOM.p, MOOS_storm2_07_27_SPC.p, MOOS_storm2_07_27_turb.p,
          MOOS_storm3a_08_06_fDOM.p, MOOS_storm3a_08_06_SPC.p, MOOS_storm3a_08_06_turb.p,
          MOOS_storm3b_08_08_fDOM.p, MOOS_storm3b_08_08_SPC.p, MOOS_storm3b_08_08_turb.p,
          MOOS_storm4a_08_15_fDOM.p, MOOS_storm4a_08_15_SPC.p, MOOS_storm4a_08_15_turb.p,
          MOOS_storm4b_08_17_fDOM.p, MOOS_storm4b_08_17_SPC.p, MOOS_storm4b_08_17_turb.p,
          MOOS_storm5a_08_19_fDOM.p, MOOS_storm5a_08_19_SPC.p, MOOS_storm5a_08_19_turb.p,
          MOOS_storm5b_08_21_fDOM.p, MOOS_storm5b_08_21_SPC.p, MOOS_storm5b_08_21_turb.p,
          MOOS_storm6_08_25_fDOM.p, MOOS_storm6_08_25_SPC.p, MOOS_storm6_08_25_turb.p,
          MOOS_storm7_08_27_fDOM.p, MOOS_storm7_08_27_SPC.p, MOOS_storm7_08_27_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("MOOS_HI_Loops_2021.pdf",
       path = here("plots", "HI_plots", "2021", "MOOS"),
       width = 20, height = 30, units = "in")

# plot VAUL loops #
# NO3
FRCH_storm2_07_27_NO3.p = hyst_plot(FRCH_storm2_07_27_Q, FRCH_storm2_07_27_NO3, "FRCH", "NO3", "0727")
FRCH_storm3_08_05_NO3.p = hyst_plot(FRCH_storm3_08_05_Q, FRCH_storm3_08_05_NO3, "VAUL", "NO3", "0805")
FRCH_storm4_08_08_NO3.p = hyst_plot(FRCH_storm4_08_08_Q, FRCH_storm4_08_08_NO3, "FRCH", "NO3", "0808")
FRCH_storm5a_08_15_NO3.p = hyst_plot(FRCH_storm5a_08_15_Q, FRCH_storm5a_08_15_NO3, "FRCH", "NO3", "0815a")
FRCH_storm5b_08_17_NO3.p = hyst_plot(FRCH_storm5b_08_17_Q, FRCH_storm5b_08_17_NO3, "FRCH", "NO3", "0817b")
FRCH_storm6a_08_19_NO3.p = hyst_plot(FRCH_storm6a_08_19_Q, FRCH_storm6a_08_19_NO3, "FRCH", "NO3", "0819q")
FRCH_storm6b_08_20_NO3.p = hyst_plot(FRCH_storm6b_08_20_Q, FRCH_storm6b_08_20_NO3, "FRCH", "NO3", "0820b")
FRCH_storm7_08_25_NO3.p = hyst_plot(FRCH_storm7_08_25_Q, FRCH_storm7_08_25_NO3, "FRCH", "NO3", "0825")
FRCH_storm8_08_27_NO3.p = hyst_plot(FRCH_storm8_08_27_Q, FRCH_storm8_08_27_NO3, "FRCH", "NO3", "0827")

# fDOM #
FRCH_storm2_07_27_fDOM.p = hyst_plot(FRCH_storm2_07_27_Q, FRCH_storm2_07_27_fDOM, "FRCH", "fDOM", "0727")
FRCH_storm3_08_05_fDOM.p = hyst_plot(FRCH_storm3_08_05_Q, FRCH_storm3_08_05_fDOM, "VAUL", "fDOM", "0805")
FRCH_storm4_08_08_fDOM.p = hyst_plot(FRCH_storm4_08_08_Q, FRCH_storm4_08_08_fDOM, "FRCH", "fDOM", "0808")
FRCH_storm5a_08_15_fDOM.p = hyst_plot(FRCH_storm5a_08_15_Q, FRCH_storm5a_08_15_fDOM, "FRCH", "fDOM", "0815a")
FRCH_storm5b_08_17_fDOM.p = hyst_plot(FRCH_storm5b_08_17_Q, FRCH_storm5b_08_17_fDOM, "FRCH", "fDOM", "0817b")
FRCH_storm6a_08_19_fDOM.p = hyst_plot(FRCH_storm6a_08_19_Q, FRCH_storm6a_08_19_fDOM, "FRCH", "fDOM", "0819q")
FRCH_storm6b_08_20_fDOM.p = hyst_plot(FRCH_storm6b_08_20_Q, FRCH_storm6b_08_20_fDOM, "FRCH", "fDOM", "0820b")
FRCH_storm7_08_25_fDOM.p = hyst_plot(FRCH_storm7_08_25_Q, FRCH_storm7_08_25_fDOM, "FRCH", "fDOM", "0825")
FRCH_storm8_08_27_fDOM.p = hyst_plot(FRCH_storm8_08_27_Q, FRCH_storm8_08_27_fDOM, "FRCH", "fDOM", "0827")

# SPC #
FRCH_storm2_07_27_SPC.p = hyst_plot(FRCH_storm2_07_27_Q, FRCH_storm2_07_27_SPC, "FRCH", "SPC", "0727")
FRCH_storm3_08_05_SPC.p = hyst_plot(FRCH_storm3_08_05_Q, FRCH_storm3_08_05_SPC, "VAUL", "SPC", "0805")
FRCH_storm4_08_08_SPC.p = hyst_plot(FRCH_storm4_08_08_Q, FRCH_storm4_08_08_SPC, "FRCH", "SPC", "0808")
FRCH_storm5a_08_15_SPC.p = hyst_plot(FRCH_storm5a_08_15_Q, FRCH_storm5a_08_15_SPC, "FRCH", "SPC", "0815a")
FRCH_storm5b_08_17_SPC.p = hyst_plot(FRCH_storm5b_08_17_Q, FRCH_storm5b_08_17_SPC, "FRCH", "SPC", "0817b")
FRCH_storm6a_08_19_SPC.p = hyst_plot(FRCH_storm6a_08_19_Q, FRCH_storm6a_08_19_SPC, "FRCH", "SPC", "0819q")
FRCH_storm6b_08_20_SPC.p = hyst_plot(FRCH_storm6b_08_20_Q, FRCH_storm6b_08_20_SPC, "FRCH", "SPC", "0820b")
FRCH_storm7_08_25_SPC.p = hyst_plot(FRCH_storm7_08_25_Q, FRCH_storm7_08_25_SPC, "FRCH", "SPC", "0825")
FRCH_storm8_08_27_SPC.p = hyst_plot(FRCH_storm8_08_27_Q, FRCH_storm8_08_27_SPC, "FRCH", "SPC", "0827")

# turb
FRCH_storm2_07_27_turb.p = hyst_plot(FRCH_storm2_07_27_Q, FRCH_storm2_07_27_turb, "FRCH", "turb", "0727")
FRCH_storm3_08_05_turb.p = hyst_plot(FRCH_storm3_08_05_Q, FRCH_storm3_08_05_turb, "VAUL", "turb", "0805")
FRCH_storm4_08_08_turb.p = hyst_plot(FRCH_storm4_08_08_Q, FRCH_storm4_08_08_turb, "FRCH", "turb", "0808")
FRCH_storm5a_08_15_turb.p = hyst_plot(FRCH_storm5a_08_15_Q, FRCH_storm5a_08_15_turb, "FRCH", "turb", "0815a")
FRCH_storm5b_08_17_turb.p = hyst_plot(FRCH_storm5b_08_17_Q, FRCH_storm5b_08_17_turb, "FRCH", "turb", "0817b")
FRCH_storm6a_08_19_turb.p = hyst_plot(FRCH_storm6a_08_19_Q, FRCH_storm6a_08_19_turb, "FRCH", "turb", "0819q")
FRCH_storm6b_08_20_turb.p = hyst_plot(FRCH_storm6b_08_20_Q, FRCH_storm6b_08_20_turb, "FRCH", "turb", "0820b")
FRCH_storm7_08_25_turb.p = hyst_plot(FRCH_storm7_08_25_Q, FRCH_storm7_08_25_turb, "FRCH", "turb", "0825")
FRCH_storm8_08_27_turb.p = hyst_plot(FRCH_storm8_08_27_Q, FRCH_storm8_08_27_turb, "FRCH", "turb", "0827")

# ABS #
FRCH_storm2_07_27_abs.p = hyst_plot(FRCH_storm2_07_27_Q, FRCH_storm2_07_27_abs, "FRCH", "abs", "0727")
FRCH_storm3_08_05_abs.p = hyst_plot(FRCH_storm3_08_05_Q, FRCH_storm3_08_05_abs, "VAUL", "abs", "0805")
FRCH_storm4_08_08_abs.p = hyst_plot(FRCH_storm4_08_08_Q, FRCH_storm4_08_08_abs, "FRCH", "abs", "0808")
FRCH_storm5a_08_15_abs.p = hyst_plot(FRCH_storm5a_08_15_Q, FRCH_storm5a_08_15_abs, "FRCH", "abs", "0815a")
FRCH_storm5b_08_17_abs.p = hyst_plot(FRCH_storm5b_08_17_Q, FRCH_storm5b_08_17_abs, "FRCH", "abs", "0817b")
FRCH_storm6a_08_19_abs.p = hyst_plot(FRCH_storm6a_08_19_Q, FRCH_storm6a_08_19_abs, "FRCH", "abs", "0819q")
FRCH_storm6b_08_20_abs.p = hyst_plot(FRCH_storm6b_08_20_Q, FRCH_storm6b_08_20_abs, "FRCH", "abs", "0820b")
FRCH_storm7_08_25_abs.p = hyst_plot(FRCH_storm7_08_25_Q, FRCH_storm7_08_25_abs, "FRCH", "abs", "0825")
FRCH_storm8_08_27_abs.p = hyst_plot(FRCH_storm8_08_27_Q, FRCH_storm8_08_27_abs, "FRCH", "abs", "0827")

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

multiplot(
          FRCH_storm2_07_27_NO3.p, FRCH_storm2_07_27_fDOM.p, FRCH_storm2_07_27_SPC.p, FRCH_storm2_07_27_turb.p,
          FRCH_storm3_08_05_NO3.p, FRCH_storm3_08_05_fDOM.p, FRCH_storm3_08_05_SPC.p, FRCH_storm3_08_05_turb.p,
          FRCH_storm4_08_08_NO3.p, FRCH_storm4_08_08_fDOM.p, FRCH_storm4_08_08_SPC.p, FRCH_storm4_08_08_turb.p,
          FRCH_storm5a_08_15_NO3.p, FRCH_storm5a_08_15_fDOM.p, FRCH_storm5a_08_15_SPC.p, FRCH_storm5a_08_15_turb.p,
          FRCH_storm5b_08_17_NO3.p, FRCH_storm5b_08_17_fDOM.p, FRCH_storm5b_08_17_SPC.p, FRCH_storm5b_08_17_turb.p,
          FRCH_storm6a_08_19_NO3.p, FRCH_storm6a_08_19_fDOM.p, FRCH_storm6a_08_19_SPC.p, FRCH_storm6a_08_19_turb.p,
          FRCH_storm6b_08_20_NO3.p, FRCH_storm6b_08_20_fDOM.p, FRCH_storm6b_08_20_SPC.p, FRCH_storm6b_08_20_turb.p,
          FRCH_storm7_08_25_NO3.p, FRCH_storm7_08_25_fDOM.p, FRCH_storm7_08_25_SPC.p, FRCH_storm7_08_25_turb.p,
          FRCH_storm8_08_27_NO3.p, FRCH_storm8_08_27_fDOM.p, FRCH_storm8_08_27_SPC.p, FRCH_storm8_08_27_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2021.pdf",
       path = here("plots", "HI_plots", "2021", "FRCH"),
       width = 20, height = 30, units = "in")


# plot CARI loops #
# NO3
CARI_storm1_05_16_NO3.p = hyst_plot(CARI_storm1_05_16_Q, CARI_storm1_05_16_NO3, "CARI", "NO3", "0516")
CARI_storm2_06_01_NO3.p = hyst_plot(CARI_storm2_06_01_Q, CARI_storm2_06_01_NO3, "CARI", "NO3", "0601")
CARI_storm3_06_19_NO3.p = hyst_plot(CARI_storm3_06_19_Q, CARI_storm3_06_19_NO3, "CARI", "NO3", "0619")
CARI_storm4_07_24_NO3.p = hyst_plot(CARI_storm4_07_24_Q, CARI_storm4_07_24_NO3, "CARI", "NO3", "0724")
CARI_storm5_07_27_NO3.p = hyst_plot(CARI_storm5_07_27_Q, CARI_storm5_07_27_NO3, "CARI", "NO3", "0727")
CARI_storm6_08_08_NO3.p = hyst_plot(CARI_storm6_08_08_Q, CARI_storm6_08_08_NO3, "CARI", "NO3", "0808")
CARI_storm7_08_15_NO3.p = hyst_plot(CARI_storm7_08_15_Q, CARI_storm7_08_15_NO3, "CARI", "NO3", "0815")
CARI_storm8_08_20_NO3.p = hyst_plot(CARI_storm8_08_20_Q, CARI_storm8_08_20_NO3, "CARI", "NO3", "0820")
CARI_storm9_08_23_NO3.p = hyst_plot(CARI_storm9_08_23_Q, CARI_storm9_08_23_NO3, "CARI", "NO3", "0823")
CARI_storm10_08_27_NO3.p = hyst_plot(CARI_storm10_08_27_Q, CARI_storm10_08_27_NO3, "CARI", "NO3", "0827")

# fDOM #
CARI_storm1_05_16_fDOM.p = hyst_plot(CARI_storm1_05_16_Q, CARI_storm1_05_16_fDOM, "CARI", "fDOM", "0516")
CARI_storm2_06_01_fDOM.p = hyst_plot(CARI_storm2_06_01_Q, CARI_storm2_06_01_fDOM, "CARI", "fDOM", "0601")
CARI_storm3_06_19_fDOM.p = hyst_plot(CARI_storm3_06_19_Q, CARI_storm3_06_19_fDOM, "CARI", "fDOM", "0619")
CARI_storm4_07_24_fDOM.p = hyst_plot(CARI_storm4_07_24_Q, CARI_storm4_07_24_fDOM, "CARI", "fDOM", "0724")
CARI_storm5_07_27_fDOM.p = hyst_plot(CARI_storm5_07_27_Q, CARI_storm5_07_27_fDOM, "CARI", "fDOM", "0727")
CARI_storm6_08_08_fDOM.p = hyst_plot(CARI_storm6_08_08_Q, CARI_storm6_08_08_fDOM, "CARI", "fDOM", "0808")
CARI_storm7_08_15_fDOM.p = hyst_plot(CARI_storm7_08_15_Q, CARI_storm7_08_15_fDOM, "CARI", "fDOM", "0815")
CARI_storm8_08_20_fDOM.p = hyst_plot(CARI_storm8_08_20_Q, CARI_storm8_08_20_fDOM, "CARI", "fDOM", "0820")
CARI_storm9_08_23_fDOM.p = hyst_plot(CARI_storm9_08_23_Q, CARI_storm9_08_23_fDOM, "CARI", "fDOM", "0823")
CARI_storm10_08_27_fDOM.p = hyst_plot(CARI_storm10_08_27_Q, CARI_storm10_08_27_fDOM, "CARI", "fDOM", "0827")

# SPC #
CARI_storm1_05_16_SPC.p = hyst_plot(CARI_storm1_05_16_Q, CARI_storm1_05_16_SPC, "CARI", "SPC", "0516")
CARI_storm2_06_01_SPC.p = hyst_plot(CARI_storm2_06_01_Q, CARI_storm2_06_01_SPC, "CARI", "SPC", "0601")
CARI_storm3_06_19_SPC.p = hyst_plot(CARI_storm3_06_19_Q, CARI_storm3_06_19_SPC, "CARI", "SPC", "0619")
CARI_storm4_07_24_SPC.p = hyst_plot(CARI_storm4_07_24_Q, CARI_storm4_07_24_SPC, "CARI", "SPC", "0724")
CARI_storm5_07_27_SPC.p = hyst_plot(CARI_storm5_07_27_Q, CARI_storm5_07_27_SPC, "CARI", "SPC", "0727")
CARI_storm6_08_08_SPC.p = hyst_plot(CARI_storm6_08_08_Q, CARI_storm6_08_08_SPC, "CARI", "SPC", "0808")
CARI_storm7_08_15_SPC.p = hyst_plot(CARI_storm7_08_15_Q, CARI_storm7_08_15_SPC, "CARI", "SPC", "0815")
CARI_storm8_08_20_SPC.p = hyst_plot(CARI_storm8_08_20_Q, CARI_storm8_08_20_SPC, "CARI", "SPC", "0820")
CARI_storm9_08_23_SPC.p = hyst_plot(CARI_storm9_08_23_Q, CARI_storm9_08_23_SPC, "CARI", "SPC", "0823")
CARI_storm10_08_27_SPC.p = hyst_plot(CARI_storm10_08_27_Q, CARI_storm10_08_27_SPC, "CARI", "SPC", "0827")


# Turb #
CARI_storm1_05_16_turb.p = hyst_plot(CARI_storm1_05_16_Q, CARI_storm1_05_16_turb, "CARI", "turb", "0516")
CARI_storm2_06_01_turb.p = hyst_plot(CARI_storm2_06_01_Q, CARI_storm2_06_01_turb, "CARI", "turb", "0601")
CARI_storm3_06_19_turb.p = hyst_plot(CARI_storm3_06_19_Q, CARI_storm3_06_19_turb, "CARI", "turb", "0619")
CARI_storm4_07_24_turb.p = hyst_plot(CARI_storm4_07_24_Q, CARI_storm4_07_24_turb, "CARI", "turb", "0724")
CARI_storm5_07_27_turb.p = hyst_plot(CARI_storm5_07_27_Q, CARI_storm5_07_27_turb, "CARI", "turb", "0727")
CARI_storm6_08_08_turb.p = hyst_plot(CARI_storm6_08_08_Q, CARI_storm6_08_08_turb, "CARI", "turb", "0808")
CARI_storm7_08_15_turb.p = hyst_plot(CARI_storm7_08_15_Q, CARI_storm7_08_15_turb, "CARI", "turb", "0815")
CARI_storm8_08_20_turb.p = hyst_plot(CARI_storm8_08_20_Q, CARI_storm8_08_20_turb, "CARI", "turb", "0820")
CARI_storm9_08_23_turb.p = hyst_plot(CARI_storm9_08_23_Q, CARI_storm9_08_23_turb, "CARI", "turb", "0823")
CARI_storm10_08_27_turb.p = hyst_plot(CARI_storm10_08_27_Q, CARI_storm10_08_27_turb, "CARI", "turb", "0827")

multiplot(CARI_storm1_05_16_NO3.p, CARI_storm1_05_16_fDOM.p, CARI_storm1_05_16_SPC.p, CARI_storm1_05_16_turb.p,
          CARI_storm2_06_01_NO3.p, CARI_storm2_06_01_fDOM.p, CARI_storm2_06_01_SPC.p, CARI_storm2_06_01_turb.p,
          CARI_storm3_06_19_NO3.p, CARI_storm3_06_19_fDOM.p, CARI_storm3_06_19_SPC.p, CARI_storm3_06_19_turb.p,
          CARI_storm4_07_24_NO3.p, CARI_storm4_07_24_fDOM.p, CARI_storm4_07_24_SPC.p, CARI_storm4_07_24_turb.p,
          CARI_storm5_07_27_NO3.p, CARI_storm5_07_27_fDOM.p, CARI_storm5_07_27_SPC.p, CARI_storm5_07_27_turb.p,
          CARI_storm6_08_08_NO3.p, CARI_storm6_08_08_fDOM.p, CARI_storm6_08_08_SPC.p, CARI_storm6_08_08_turb.p,
          CARI_storm7_08_15_NO3.p, CARI_storm7_08_15_fDOM.p, CARI_storm7_08_15_SPC.p, CARI_storm7_08_15_turb.p,
          CARI_storm8_08_20_NO3.p, CARI_storm8_08_20_fDOM.p, CARI_storm8_08_20_SPC.p, CARI_storm8_08_20_turb.p,
          CARI_storm9_08_23_NO3.p, CARI_storm9_08_23_fDOM.p, CARI_storm9_08_23_SPC.p, CARI_storm9_08_23_turb.p,
          CARI_storm10_08_27_NO3.p, CARI_storm10_08_27_fDOM.p, CARI_storm10_08_27_SPC.p, CARI_storm10_08_27_turb.p,
          cols = 7)

# export pdf 20 x 30 #
ggsave("CARI_HI_Loops_2021.pdf",
       path = here("plots", "HI_plots", "2021", "CARI"),
       width = 20, height = 30, units = "in")


######################################## 2022 ####################################
# load data #
library(dataRetrieval)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(here)
library(tidyverse)
library(zoo)

# FRCH #

# FRCH_storm1_07_10 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10.csv"))
FRCH_storm1_07_10_Q <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10_Q.csv"))
FRCH_storm1_07_10_NO3 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10_NO3.csv"))
FRCH_storm1_07_10_fDOM <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10_fDOM.csv"))
FRCH_storm1_07_10_SPC <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10_SPC.csv"))
FRCH_storm1_07_10_Turb <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10_turb.csv"))
FRCH_storm1_07_10_abs <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm1_07_10_abs.csv"))

# FRCH_storm2_08_05 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05.csv"))
FRCH_storm2_08_05_Q <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05_Q.csv"))
FRCH_storm2_08_05_NO3 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05_NO3.csv"))
FRCH_storm2_08_05_fDOM <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05_fDOM.csv"))
FRCH_storm2_08_05_SPC <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05_SPC.csv"))
FRCH_storm2_08_05_Turb <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05_turb.csv"))
FRCH_storm2_08_05_abs <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm2_08_05_abs.csv"))

# FRCH_storm3_09_14 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14.csv"))
FRCH_storm3_09_14_Q <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14_Q.csv"))
FRCH_storm3_09_14_NO3 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14_NO3.csv"))
FRCH_storm3_09_14_fDOM <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14_fDOM.csv"))
FRCH_storm3_09_14_SPC <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14_SPC.csv"))
FRCH_storm3_09_14_Turb <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14_turb.csv"))
FRCH_storm3_09_14_abs <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm3_09_14_abs.csv"))

# FRCH_storm4_09_19 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19.csv"))
FRCH_storm4_09_19_Q <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19_Q.csv"))
FRCH_storm4_09_19_NO3 <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19_NO3.csv"))
FRCH_storm4_09_19_fDOM <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19_fDOM.csv"))
FRCH_storm4_09_19_SPC <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19_SPC.csv"))
FRCH_storm4_09_19_Turb <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19_turb.csv"))
FRCH_storm4_09_19_abs <- read_csv(here("Storm_events", "2022", "FRCH", "FRCH_storm4_09_19_abs.csv"))


# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# NO3 #
FRCH_storm1_07_10_NO3.p = hyst_plot(FRCH_storm1_07_10_Q, FRCH_storm1_07_10_NO3, "FRCH", "NO3", "0710")
FRCH_storm2_08_05_NO3.p = hyst_plot(FRCH_storm2_08_05_Q, FRCH_storm2_08_05_NO3, "FRCH", "NO3", "0805")
FRCH_storm3_09_14_NO3.p = hyst_plot(FRCH_storm3_09_14_Q, FRCH_storm3_09_14_NO3, "FRCH", "NO3", "0914")
FRCH_storm4_09_19_NO3.p = hyst_plot(FRCH_storm4_09_19_Q, FRCH_storm4_09_19_NO3, "FRCH", "NO3", "0919")

multiplot(FRCH_storm1_07_10_NO3.p) 
multiplot(FRCH_storm2_08_05_NO3.p) 
multiplot(FRCH_storm3_09_14_NO3.p) 
multiplot(FRCH_storm4_09_19_NO3.p)


# fDOM #
FRCH_storm1_07_10_fDOM.p = hyst_plot(FRCH_storm1_07_10_Q, FRCH_storm1_07_10_fDOM, "FRCH", "fDOM", "0710")
FRCH_storm2_08_05_fDOM.p = hyst_plot(FRCH_storm2_08_05_Q, FRCH_storm2_08_05_fDOM, "FRCH", "fDOM", "0805")
FRCH_storm3_09_14_fDOM.p = hyst_plot(FRCH_storm3_09_14_Q, FRCH_storm3_09_14_fDOM, "FRCH", "fDOM", "0914")
FRCH_storm4_09_19_fDOM.p = hyst_plot(FRCH_storm4_09_19_Q, FRCH_storm4_09_19_fDOM, "FRCH", "fDOM", "0919")

multiplot(FRCH_storm1_07_10_fDOM.p) 
multiplot(FRCH_storm2_08_05_fDOM.p) 
multiplot(FRCH_storm3_09_14_fDOM.p) 
multiplot(FRCH_storm4_09_19_fDOM.p)

# SPC #
FRCH_storm1_07_10_SPC.p = hyst_plot(FRCH_storm1_07_10_Q, FRCH_storm1_07_10_SPC, "FRCH", "SPC", "0710")
FRCH_storm2_08_05_SPC.p = hyst_plot(FRCH_storm2_08_05_Q, FRCH_storm2_08_05_SPC, "FRCH", "SPC", "0805")
FRCH_storm3_09_14_SPC.p = hyst_plot(FRCH_storm3_09_14_Q, FRCH_storm3_09_14_SPC, "FRCH", "SPC", "0914")
FRCH_storm4_09_19_SPC.p = hyst_plot(FRCH_storm4_09_19_Q, FRCH_storm4_09_19_SPC, "FRCH", "SPC", "0919")

multiplot(FRCH_storm1_07_10_SPC.p) 
multiplot(FRCH_storm2_08_05_SPC.p) 
multiplot(FRCH_storm3_09_14_SPC.p) 
multiplot(FRCH_storm4_09_19_SPC.p)

# turb #
FRCH_storm1_07_10_turb.p = hyst_plot(FRCH_storm1_07_10_Q, FRCH_storm1_07_10_Turb, "FRCH", "Turb", "0710")
FRCH_storm2_08_05_turb.p = hyst_plot(FRCH_storm2_08_05_Q, FRCH_storm2_08_05_Turb, "FRCH", "Turb", "0805")
FRCH_storm3_09_14_turb.p = hyst_plot(FRCH_storm3_09_14_Q, FRCH_storm3_09_14_Turb, "FRCH", "Turb", "0914")
FRCH_storm4_09_19_turb.p = hyst_plot(FRCH_storm4_09_19_Q, FRCH_storm4_09_19_Turb, "FRCH", "Turb", "0919")

multiplot(FRCH_storm1_07_10_turb.p) 
multiplot(FRCH_storm2_08_05_turb.p) 
multiplot(FRCH_storm3_09_14_turb.p) 
multiplot(FRCH_storm4_09_19_turb.p)

# abs #
FRCH_storm1_07_10_abs.p = hyst_plot(FRCH_storm1_07_10_Q, FRCH_storm1_07_10_abs, "FRCH", "abs", "0710")
FRCH_storm2_08_05_abs.p = hyst_plot(FRCH_storm2_08_05_Q, FRCH_storm2_08_05_abs, "FRCH", "abs", "0805")
FRCH_storm3_09_14_abs.p = hyst_plot(FRCH_storm3_09_14_Q, FRCH_storm3_09_14_abs, "FRCH", "abs", "0914")
FRCH_storm4_09_19_abs.p = hyst_plot(FRCH_storm4_09_19_Q, FRCH_storm4_09_19_abs, "FRCH", "abs", "0919")

multiplot(FRCH_storm1_07_10_abs.p) 
multiplot(FRCH_storm2_08_05_abs.p) 
multiplot(FRCH_storm3_09_14_abs.p) 
multiplot(FRCH_storm4_09_19_abs.p)

# Plot all the storms that are correct:

FRCH_HI_Loop <- multiplot(FRCH_storm1_07_10_NO3.p,FRCH_storm1_07_10_fDOM.p, FRCH_storm1_07_10_SPC.p,FRCH_storm1_07_10_turb.p,FRCH_storm1_07_10_abs.p,
                          FRCH_storm2_08_05_NO3.p,FRCH_storm2_08_05_fDOM.p, FRCH_storm2_08_05_SPC.p,FRCH_storm2_08_05_turb.p, FRCH_storm2_08_05_abs.p,
                          FRCH_storm3_09_14_NO3.p,FRCH_storm3_09_14_fDOM.p, FRCH_storm3_09_14_SPC.p,FRCH_storm3_09_14_turb.p,FRCH_storm3_09_14_abs.p,
                          FRCH_storm4_09_19_NO3.p,FRCH_storm4_09_19_fDOM.p, FRCH_storm4_09_19_SPC.p,FRCH_storm4_09_19_turb.p,FRCH_storm4_09_19_abs.p)
                          
# export pdf 20 x 30 #
ggsave("FRCH_HI_Loops_2022.pdf",
       path = here("plots", "02_Hysteresis", "2022"),
       width = 20, height = 30, units = "in")


# MOOS #
# MOOS_storm1_07_10 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10.csv"))
MOOS_storm1_07_10_Q <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10_Q.csv"))
MOOS_storm1_07_10_NO3 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10_NO3.csv"))
MOOS_storm1_07_10_fDOM <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10_fDOM.csv"))
MOOS_storm1_07_10_SPC <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10_SPC.csv"))
MOOS_storm1_07_10_Turb <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10_turb.csv"))
MOOS_storm1_07_10_abs <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm1_07_10_abs.csv"))

# MOOS_storm2b_08_06 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06.csv"))
MOOS_storm2b_08_06_Q <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06_Q.csv"))
MOOS_storm2b_08_06_NO3 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06_NO3.csv"))
MOOS_storm2b_08_06_fDOM <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06_fDOM.csv"))
MOOS_storm2b_08_06_SPC <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06_SPC.csv"))
MOOS_storm2b_08_06_Turb <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06_turb.csv"))
MOOS_storm2b_08_06_abs <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm2b_08_06_abs.csv"))

# MOOS_storm3_08_19 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19.csv"))
MOOS_storm3_08_19_Q <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19_Q.csv"))
MOOS_storm3_08_19_NO3 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19_NO3.csv"))
MOOS_storm3_08_19_fDOM <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19_fDOM.csv"))
MOOS_storm3_08_19_SPC <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19_SPC.csv"))
MOOS_storm3_08_19_Turb <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19_turb.csv"))
MOOS_storm3_08_19_abs <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm3_08_19_abs.csv"))

# MOOS_storm4_09_15 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15.csv"))
MOOS_storm4_09_15_Q <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15_Q.csv"))
MOOS_storm4_09_15_NO3 <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15_NO3.csv"))
MOOS_storm4_09_15_fDOM <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15_fDOM.csv"))
MOOS_storm4_09_15_SPC <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15_SPC.csv"))
MOOS_storm4_09_15_Turb <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15_turb.csv"))
MOOS_storm4_09_15_abs <- read_csv(here("Storm_events", "2022", "MOOS", "MOOS_storm4_09_15_abs.csv"))


# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# plot MOOS loops #

# NO3 #
MOOS_storm1_07_10_NO3.p = hyst_plot(MOOS_storm1_07_10_Q, MOOS_storm1_07_10_NO3, "MOOS", "NO3", "0710")
MOOS_storm2b_08_06_NO3.p = hyst_plot(MOOS_storm2b_08_06_Q, MOOS_storm2b_08_06_NO3, "MOOS", "NO3", "0805b")
MOOS_storm3_08_19_NO3.p = hyst_plot(MOOS_storm3_08_19_Q, MOOS_storm3_08_19_NO3, "MOOS", "NO3", "0914")
MOOS_storm4_09_15_NO3.p = hyst_plot(MOOS_storm4_09_15_Q, MOOS_storm4_09_15_NO3, "MOOS", "NO3", "0919")

multiplot(MOOS_storm1_07_10_NO3.p) 
multiplot(MOOS_storm2b_08_06_NO3.p) 
multiplot(MOOS_storm3_08_19_NO3.p) 
multiplot(MOOS_storm4_09_15_NO3.p)


# fDOM #
MOOS_storm1_07_10_fDOM.p = hyst_plot(MOOS_storm1_07_10_Q, MOOS_storm1_07_10_fDOM, "MOOS", "fDOM", "0710")
MOOS_storm2b_08_06_fDOM.p = hyst_plot(MOOS_storm2b_08_06_Q, MOOS_storm2b_08_06_fDOM, "MOOS", "fDOM", "0805b")
MOOS_storm3_08_19_fDOM.p = hyst_plot(MOOS_storm3_08_19_Q, MOOS_storm3_08_19_fDOM, "MOOS", "fDOM", "0914")
MOOS_storm4_09_15_fDOM.p = hyst_plot(MOOS_storm4_09_15_Q, MOOS_storm4_09_15_fDOM, "MOOS", "fDOM", "0919")

multiplot(MOOS_storm1_07_10_fDOM.p) 
multiplot(MOOS_storm2b_08_06_fDOM.p) 
multiplot(MOOS_storm3_08_19_fDOM.p) 
multiplot(MOOS_storm4_09_15_fDOM.p)

# SPC #
MOOS_storm1_07_10_SPC.p = hyst_plot(MOOS_storm1_07_10_Q, MOOS_storm1_07_10_SPC, "MOOS", "SPC", "0710")
MOOS_storm2b_08_06_SPC.p = hyst_plot(MOOS_storm2b_08_06_Q, MOOS_storm2b_08_06_SPC, "MOOS", "SPC", "0805b")
MOOS_storm3_08_19_SPC.p = hyst_plot(MOOS_storm3_08_19_Q, MOOS_storm3_08_19_SPC, "MOOS", "SPC", "0914")
MOOS_storm4_09_15_SPC.p = hyst_plot(MOOS_storm4_09_15_Q, MOOS_storm4_09_15_SPC, "MOOS", "SPC", "0919")

multiplot(MOOS_storm1_07_10_SPC.p) 
multiplot(MOOS_storm2b_08_06_SPC.p) 
multiplot(MOOS_storm3_08_19_SPC.p) 
multiplot(MOOS_storm4_09_15_SPC.p)

# turb #
MOOS_storm1_07_10_turb.p = hyst_plot(MOOS_storm1_07_10_Q, MOOS_storm1_07_10_Turb, "MOOS", "Turb", "0710")
MOOS_storm2b_08_06_turb.p = hyst_plot(MOOS_storm2b_08_06_Q, MOOS_storm2b_08_06_Turb, "MOOS", "Turb", "0805b")
MOOS_storm3_08_19_turb.p = hyst_plot(MOOS_storm3_08_19_Q, MOOS_storm3_08_19_Turb, "MOOS", "Turb", "0914")
MOOS_storm4_09_15_turb.p = hyst_plot(MOOS_storm4_09_15_Q, MOOS_storm4_09_15_Turb, "MOOS", "Turb", "0919")

multiplot(MOOS_storm1_07_10_turb.p) 
multiplot(MOOS_storm2b_08_06_turb.p) 
multiplot(MOOS_storm3_08_19_turb.p) 
multiplot(MOOS_storm4_09_15_turb.p)

# abs #
MOOS_storm1_07_10_abs.p = hyst_plot(MOOS_storm1_07_10_Q, MOOS_storm1_07_10_abs, "MOOS", "abs", "0710")
MOOS_storm2b_08_06_abs.p = hyst_plot(MOOS_storm2b_08_06_Q, MOOS_storm2b_08_06_abs, "MOOS", "abs", "0805b")
MOOS_storm3_08_19_abs.p = hyst_plot(MOOS_storm3_08_19_Q, MOOS_storm3_08_19_abs, "MOOS", "abs", "0914")
MOOS_storm4_09_15_abs.p = hyst_plot(MOOS_storm4_09_15_Q, MOOS_storm4_09_15_abs, "MOOS", "abs", "0919")

multiplot(MOOS_storm1_07_10_abs.p) 
multiplot(MOOS_storm2b_08_06_abs.p) 
multiplot(MOOS_storm3_08_19_abs.p) 
multiplot(MOOS_storm4_09_15_abs.p)

# Plot all the storms that are correct:

MOOS_HI_Loop <- multiplot(MOOS_storm1_07_10_NO3.p,MOOS_storm1_07_10_fDOM.p, MOOS_storm1_07_10_SPC.p,MOOS_storm1_07_10_turb.p,MOOS_storm1_07_10_abs.p,
                          MOOS_storm2b_08_06_NO3.p,MOOS_storm2b_08_06_fDOM.p, MOOS_storm2b_08_06_SPC.p,MOOS_storm2b_08_06_turb.p, MOOS_storm2b_08_06_abs.p,
                          MOOS_storm3_08_19_NO3.p,MOOS_storm3_08_19_fDOM.p, MOOS_storm3_08_19_SPC.p,MOOS_storm3_08_19_turb.p,MOOS_storm3_08_19_abs.p,
                          MOOS_storm4_09_15_NO3.p,MOOS_storm4_09_15_fDOM.p, MOOS_storm4_09_15_SPC.p,MOOS_storm4_09_15_turb.p,MOOS_storm4_09_15_abs.p)

# export pdf 20 x 30 #
ggsave("MOOS_HI_Loops_2022.pdf",
       path = here("plots", "02_Hysteresis", "2022"),
       width = 20, height = 30, units = "in")


# POKE #
# POKE_storm1_07_15 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15.csv"))
POKE_storm1_07_15_Q <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15_Q.csv"))
POKE_storm1_07_15_NO3 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15_NO3.csv"))
POKE_storm1_07_15_fDOM <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15_fDOM.csv"))
POKE_storm1_07_15_SPC <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15_SPC.csv"))
POKE_storm1_07_15_Turb <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15_turb.csv"))
POKE_storm1_07_15_abs <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm1_07_15_abs.csv"))

# POKE_storm2_09_03 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03.csv"))
POKE_storm2_09_03_Q <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03_Q.csv"))
POKE_storm2_09_03_NO3 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03_NO3.csv"))
POKE_storm2_09_03_fDOM <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03_fDOM.csv"))
POKE_storm2_09_03_SPC <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03_SPC.csv"))
POKE_storm2_09_03_Turb <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03_turb.csv"))
POKE_storm2_09_03_abs <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm2_09_03_abs.csv"))

# POKE_storm3_09_09 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09.csv"))
POKE_storm3_09_09_Q <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09_Q.csv"))
POKE_storm3_09_09_NO3 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09_NO3.csv"))
POKE_storm3_09_09_fDOM <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09_fDOM.csv"))
POKE_storm3_09_09_SPC <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09_SPC.csv"))
POKE_storm3_09_09_Turb <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09_turb.csv"))
POKE_storm3_09_09_abs <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm3_09_09_abs.csv"))

# POKE_storm4_09_14 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14.csv"))
POKE_storm4_09_14_Q <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14_Q.csv"))
POKE_storm4_09_14_NO3 <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14_NO3.csv"))
POKE_storm4_09_14_fDOM <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14_fDOM.csv"))
POKE_storm4_09_14_SPC <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14_SPC.csv"))
POKE_storm4_09_14_Turb <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14_turb.csv"))
POKE_storm4_09_14_abs <- read_csv(here("Storm_events", "2022", "POKE", "POKE_storm4_09_14_abs.csv"))



# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# plot POKE loops #

# NO3 #
POKE_storm1_07_15_NO3.p = hyst_plot(POKE_storm1_07_15_Q, POKE_storm1_07_15_NO3, "POKE", "NO3", "0715")
POKE_storm2_09_03_NO3.p = hyst_plot(POKE_storm2_09_03_Q, POKE_storm2_09_03_NO3, "POKE", "NO3", "0903")
POKE_storm3_09_09_NO3.p = hyst_plot(POKE_storm3_09_09_Q, POKE_storm3_09_09_NO3, "POKE", "NO3", "0909")
POKE_storm4_09_14_NO3.p = hyst_plot(POKE_storm4_09_14_Q, POKE_storm4_09_14_NO3, "POKE", "NO3", "0914")

multiplot(POKE_storm1_07_15_NO3.p) 
multiplot(POKE_storm2_09_03_NO3.p) 
multiplot(POKE_storm3_09_09_NO3.p) 
multiplot(POKE_storm4_09_14_NO3.p)


# fDOM #
POKE_storm1_07_15_fDOM.p = hyst_plot(POKE_storm1_07_15_Q, POKE_storm1_07_15_fDOM, "POKE", "fDOM", "0715")
POKE_storm2_09_03_fDOM.p = hyst_plot(POKE_storm2_09_03_Q, POKE_storm2_09_03_fDOM, "POKE", "fDOM", "0903")
POKE_storm3_09_09_fDOM.p = hyst_plot(POKE_storm3_09_09_Q, POKE_storm3_09_09_fDOM, "POKE", "fDOM", "0909")
POKE_storm4_09_14_fDOM.p = hyst_plot(POKE_storm4_09_14_Q, POKE_storm4_09_14_fDOM, "POKE", "fDOM", "0914")

multiplot(POKE_storm1_07_15_fDOM.p) 
multiplot(POKE_storm2_09_03_fDOM.p) 
multiplot(POKE_storm3_09_09_fDOM.p) 
multiplot(POKE_storm4_09_14_fDOM.p)

# SPC #
POKE_storm1_07_15_SPC.p = hyst_plot(POKE_storm1_07_15_Q, POKE_storm1_07_15_SPC, "POKE", "SPC", "0715")
POKE_storm2_09_03_SPC.p = hyst_plot(POKE_storm2_09_03_Q, POKE_storm2_09_03_SPC, "POKE", "SPC", "0903")
POKE_storm3_09_09_SPC.p = hyst_plot(POKE_storm3_09_09_Q, POKE_storm3_09_09_SPC, "POKE", "SPC", "0909")
POKE_storm4_09_14_SPC.p = hyst_plot(POKE_storm4_09_14_Q, POKE_storm4_09_14_SPC, "POKE", "SPC", "0914")

multiplot(POKE_storm1_07_15_SPC.p) 
multiplot(POKE_storm2_09_03_SPC.p) 
multiplot(POKE_storm3_09_09_SPC.p) 
multiplot(POKE_storm4_09_14_SPC.p)

# turb #
POKE_storm1_07_15_turb.p = hyst_plot(POKE_storm1_07_15_Q, POKE_storm1_07_15_Turb, "POKE", "Turb", "0715")
POKE_storm2_09_03_turb.p = hyst_plot(POKE_storm2_09_03_Q, POKE_storm2_09_03_Turb, "POKE", "Turb", "0903")
POKE_storm3_09_09_turb.p = hyst_plot(POKE_storm3_09_09_Q, POKE_storm3_09_09_Turb, "POKE", "Turb", "0909")
POKE_storm4_09_14_turb.p = hyst_plot(POKE_storm4_09_14_Q, POKE_storm4_09_14_Turb, "POKE", "Turb", "0914")

multiplot(POKE_storm1_07_15_turb.p) 
multiplot(POKE_storm2_09_03_turb.p) 
multiplot(POKE_storm3_09_09_turb.p) 
multiplot(POKE_storm4_09_14_turb.p)

# abs #
POKE_storm1_07_15_abs.p = hyst_plot(POKE_storm1_07_15_Q, POKE_storm1_07_15_abs, "POKE", "abs", "0715")
POKE_storm2_09_03_abs.p = hyst_plot(POKE_storm2_09_03_Q, POKE_storm2_09_03_abs, "POKE", "abs", "0903")
POKE_storm3_09_09_abs.p = hyst_plot(POKE_storm3_09_09_Q, POKE_storm3_09_09_abs, "POKE", "abs", "0909")
POKE_storm4_09_14_abs.p = hyst_plot(POKE_storm4_09_14_Q, POKE_storm4_09_14_abs, "POKE", "abs", "0914")

multiplot(POKE_storm1_07_15_abs.p) 
multiplot(POKE_storm2_09_03_abs.p) 
multiplot(POKE_storm3_09_09_abs.p) 
multiplot(POKE_storm4_09_14_abs.p)

# Plot all the storms that are correct:

POKE_HI_Loop <- multiplot(POKE_storm1_07_15_NO3.p,POKE_storm1_07_15_fDOM.p, POKE_storm1_07_15_SPC.p,POKE_storm1_07_15_turb.p,POKE_storm1_07_15_abs.p,
                          POKE_storm2_09_03_NO3.p,POKE_storm2_09_03_fDOM.p, POKE_storm2_09_03_SPC.p,POKE_storm2_09_03_turb.p, POKE_storm2_09_03_abs.p,
                          POKE_storm3_09_09_NO3.p,POKE_storm3_09_09_fDOM.p, POKE_storm3_09_09_SPC.p,POKE_storm3_09_09_turb.p,POKE_storm3_09_09_abs.p,
                          POKE_storm4_09_14_NO3.p,POKE_storm4_09_14_fDOM.p, POKE_storm4_09_14_SPC.p,POKE_storm4_09_14_turb.p,POKE_storm4_09_14_abs.p)

# export pdf 20 x 30 #
ggsave("POKE_HI_Loops_2022.pdf",
       path = here("plots", "02_Hysteresis", "2022"),
       width = 20, height = 30, units = "in")


# VAUL #
# VAUL_storm1_08_01 <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01.csv"))
VAUL_storm1_08_01_Q <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01_Q.csv"))
VAUL_storm1_08_01_NO3 <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01_NO3.csv"))
VAUL_storm1_08_01_fDOM <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01_fDOM.csv"))
VAUL_storm1_08_01_SPC <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01_SPC.csv"))
VAUL_storm1_08_01_Turb <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01_turb.csv"))
VAUL_storm1_08_01_abs <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm1_08_01_abs.csv"))

# VAUL_storm2_09_14 <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14.csv"))
VAUL_storm2_09_14_Q <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14_Q.csv"))
VAUL_storm2_09_14_NO3 <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14_NO3.csv"))
VAUL_storm2_09_14_fDOM <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14_fDOM.csv"))
VAUL_storm2_09_14_SPC <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14_SPC.csv"))
VAUL_storm2_09_14_Turb <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14_turb.csv"))
VAUL_storm2_09_14_abs <- read_csv(here("Storm_events", "2022", "VAUL", "VAUL_storm2_09_14_abs.csv"))



# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# plot VAUL loops #

# NO3 #
VAUL_storm1_08_01_NO3.p = hyst_plot(VAUL_storm1_08_01_Q, VAUL_storm1_08_01_NO3, "VAUL", "NO3", "0801")
VAUL_storm2_09_14_NO3.p = hyst_plot(VAUL_storm2_09_14_Q, VAUL_storm2_09_14_NO3, "VAUL", "NO3", "0914")

multiplot(VAUL_storm1_08_01_NO3.p) 
multiplot(VAUL_storm2_09_14_NO3.p) 


# fDOM #
VAUL_storm1_08_01_fDOM.p = hyst_plot(VAUL_storm1_08_01_Q, VAUL_storm1_08_01_fDOM, "VAUL", "fDOM", "0801")
VAUL_storm2_09_14_fDOM.p = hyst_plot(VAUL_storm2_09_14_Q, VAUL_storm2_09_14_fDOM, "VAUL", "fDOM", "0914")

multiplot(VAUL_storm1_08_01_fDOM.p) 
multiplot(VAUL_storm2_09_14_fDOM.p) 

# SPC #
VAUL_storm1_08_01_SPC.p = hyst_plot(VAUL_storm1_08_01_Q, VAUL_storm1_08_01_SPC, "VAUL", "SPC", "0801")
VAUL_storm2_09_14_SPC.p = hyst_plot(VAUL_storm2_09_14_Q, VAUL_storm2_09_14_SPC, "VAUL", "SPC", "0914")

multiplot(VAUL_storm1_08_01_SPC.p) 
multiplot(VAUL_storm2_09_14_SPC.p) 

# turb #
VAUL_storm1_08_01_turb.p = hyst_plot(VAUL_storm1_08_01_Q, VAUL_storm1_08_01_Turb, "VAUL", "turb", "0801")
VAUL_storm2_09_14_turb.p = hyst_plot(VAUL_storm2_09_14_Q, VAUL_storm2_09_14_Turb, "VAUL", "turb", "0914")

multiplot(VAUL_storm1_08_01_turb.p) 
multiplot(VAUL_storm2_09_14_turb.p) 

# abs #
VAUL_storm1_08_01_abs.p = hyst_plot(VAUL_storm1_08_01_Q, VAUL_storm1_08_01_abs, "VAUL", "abs", "0801")
VAUL_storm2_09_14_abs.p = hyst_plot(VAUL_storm2_09_14_Q, VAUL_storm2_09_14_abs, "VAUL", "abs", "0914")

multiplot(VAUL_storm1_08_01_abs.p) 
multiplot(VAUL_storm2_09_14_abs.p) 

# Plot all the storms that are correct:

VAUL_HI_Loop <- multiplot(VAUL_storm1_08_01_NO3.p,VAUL_storm1_08_01_fDOM.p, VAUL_storm1_08_01_SPC.p,VAUL_storm1_08_01_turb.p,VAUL_storm1_08_01_abs.p,
                          VAUL_storm2_09_14_NO3.p,VAUL_storm2_09_14_fDOM.p, VAUL_storm2_09_14_SPC.p,VAUL_storm2_09_14_turb.p, VAUL_storm2_09_14_abs.p)
                          
# export pdf 20 x 30 #
ggsave("VAUL_HI_Loops_2022.pdf",
       path = here("plots", "02_Hysteresis", "2022"),
       width = 20, height = 30, units = "in")


# STRT #
# STRT_storm1_08_19 <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19.csv"))
STRT_storm1_08_19_Q <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19_Q.csv"))
STRT_storm1_08_19_NO3 <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19_NO3.csv"))
STRT_storm1_08_19_fDOM <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19_fDOM.csv"))
STRT_storm1_08_19_SPC <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19_SPC.csv"))
STRT_storm1_08_19_Turb <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19_turb.csv"))
STRT_storm1_08_19_abs <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm1_08_19_abs.csv"))

# STRT_storm2_09_04 <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04.csv"))
STRT_storm2_09_04_Q <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04_Q.csv"))
STRT_storm2_09_04_NO3 <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04_NO3.csv"))
STRT_storm2_09_04_fDOM <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04_fDOM.csv"))
STRT_storm2_09_04_SPC <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04_SPC.csv"))
STRT_storm2_09_04_Turb <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04_turb.csv"))
STRT_storm2_09_04_abs <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm2_09_04_abs.csv"))

# STRT_storm3_09_22 <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22.csv"))
STRT_storm3_09_22_Q <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22_Q.csv"))
STRT_storm3_09_22_NO3 <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22_NO3.csv"))
STRT_storm3_09_22_fDOM <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22_fDOM.csv"))
STRT_storm3_09_22_SPC <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22_SPC.csv"))
STRT_storm3_09_22_Turb <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22_turb.csv"))
STRT_storm3_09_22_abs <- read_csv(here("Storm_events", "2022", "STRT", "STRT_storm3_09_22_abs.csv"))

# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# plot STRT loops #

# NO3 #
STRT_storm1_08_19_NO3.p = hyst_plot(STRT_storm1_08_19_Q, STRT_storm1_08_19_NO3, "STRT", "NO3", "0819")
STRT_storm2_09_04_NO3.p = hyst_plot(STRT_storm2_09_04_Q, STRT_storm2_09_04_NO3, "STRT", "NO3", "0904")
STRT_storm3_09_22_NO3.p = hyst_plot(STRT_storm3_09_22_Q, STRT_storm3_09_22_NO3, "STRT", "NO3", "0922")

multiplot(STRT_storm1_08_19_NO3.p) 
multiplot(STRT_storm2_09_04_NO3.p) 
multiplot(STRT_storm3_09_22_NO3.p) 

# fDOM #
STRT_storm1_08_19_fDOM.p = hyst_plot(STRT_storm1_08_19_Q, STRT_storm1_08_19_fDOM, "STRT", "fDOM", "0819")
STRT_storm2_09_04_fDOM.p = hyst_plot(STRT_storm2_09_04_Q, STRT_storm2_09_04_fDOM, "STRT", "fDOM", "0904")
STRT_storm3_09_22_fDOM.p = hyst_plot(STRT_storm3_09_22_Q, STRT_storm3_09_22_fDOM, "STRT", "fDOM", "0922")

multiplot(STRT_storm1_08_19_fDOM.p) 
multiplot(STRT_storm2_09_04_fDOM.p) 
multiplot(STRT_storm3_09_22_fDOM.p) 

# SPC #
STRT_storm1_08_19_SPC.p = hyst_plot(STRT_storm1_08_19_Q, STRT_storm1_08_19_SPC, "STRT", "SPC", "0819")
STRT_storm2_09_04_SPC.p = hyst_plot(STRT_storm2_09_04_Q, STRT_storm2_09_04_SPC, "STRT", "SPC", "0904")
STRT_storm3_09_22_SPC.p = hyst_plot(STRT_storm3_09_22_Q, STRT_storm3_09_22_SPC, "STRT", "SPC", "0922")

multiplot(STRT_storm1_08_19_SPC.p) 
multiplot(STRT_storm2_09_04_SPC.p) 
multiplot(STRT_storm3_09_22_SPC.p) 

# turb #
STRT_storm1_08_19_turb.p = hyst_plot(STRT_storm1_08_19_Q, STRT_storm1_08_19_Turb, "STRT", "Turb", "0819")
STRT_storm2_09_04_turb.p = hyst_plot(STRT_storm2_09_04_Q, STRT_storm2_09_04_Turb, "STRT", "Turb", "0904")
STRT_storm3_09_22_turb.p = hyst_plot(STRT_storm3_09_22_Q, STRT_storm3_09_22_Turb, "STRT", "Turb", "0922")

multiplot(STRT_storm1_08_19_turb.p) 
multiplot(STRT_storm2_09_04_turb.p) 
multiplot(STRT_storm3_09_22_turb.p) 

# abs #
STRT_storm1_08_19_abs.p = hyst_plot(STRT_storm1_08_19_Q, STRT_storm1_08_19_abs, "STRT", "abs", "0819")
STRT_storm2_09_04_abs.p = hyst_plot(STRT_storm2_09_04_Q, STRT_storm2_09_04_abs, "STRT", "abs", "0904")
STRT_storm3_09_22_abs.p = hyst_plot(STRT_storm3_09_22_Q, STRT_storm3_09_22_abs, "STRT", "abs", "0922")

multiplot(STRT_storm1_08_19_abs.p) 
multiplot(STRT_storm2_09_04_abs.p) 
multiplot(STRT_storm3_09_22_abs.p) 

# Plot all the storms that are correct:

STRT_HI_Loop <- multiplot(STRT_storm1_08_19_NO3.p,STRT_storm1_08_19_fDOM.p, STRT_storm1_08_19_SPC.p,STRT_storm1_08_19_turb.p,STRT_storm1_08_19_abs.p,
                          STRT_storm2_09_04_NO3.p,STRT_storm2_09_04_fDOM.p, STRT_storm2_09_04_SPC.p,STRT_storm2_09_04_turb.p, STRT_storm2_09_04_abs.p,
                          STRT_storm3_09_22_NO3.p,STRT_storm3_09_22_fDOM.p, STRT_storm3_09_22_SPC.p,STRT_storm3_09_22_turb.p, STRT_storm3_09_22_abs.p)

# export pdf 20 x 30 #
ggsave("STRT_HI_Loops_2022.pdf",
       path = here("plots", "02_Hysteresis", "2022"),
       width = 20, height = 30, units = "in")


# CARI #
# CARI_storm1_07_15 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm1_07_15.csv"))
CARI_storm1_07_15_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm1_07_15_Q.csv"))
CARI_storm1_07_15_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm1_07_15_NO3.csv"))
CARI_storm1_07_15_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm1_07_15_fDOM.csv"))
CARI_storm1_07_15_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm1_07_15_SPC.csv"))
CARI_storm1_07_15_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm1_07_15_turb.csv"))

# CARI_storm2_07_19 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm2_07_19.csv"))
CARI_storm2_07_19_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm2_07_19_Q.csv"))
CARI_storm2_07_19_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm2_07_19_NO3.csv"))
CARI_storm2_07_19_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm2_07_19_fDOM.csv"))
CARI_storm2_07_19_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm2_07_19_SPC.csv"))
CARI_storm2_07_19_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm2_07_19_turb.csv"))

# CARI_storm3_08_01 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm3_08_01.csv"))
CARI_storm3_08_01_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm3_08_01_Q.csv"))
CARI_storm3_08_01_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm3_08_01_NO3.csv"))
CARI_storm3_08_01_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm3_08_01_fDOM.csv"))
CARI_storm3_08_01_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm3_08_01_SPC.csv"))
CARI_storm3_08_01_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm3_08_01_turb.csv"))

# CARI_storm4a_08_05 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4a_08_05.csv"))
CARI_storm4a_08_05_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4a_08_05_Q.csv"))
CARI_storm4a_08_05_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4a_08_05_NO3.csv"))
CARI_storm4a_08_05_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4a_08_05_fDOM.csv"))
CARI_storm4a_08_05_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4a_08_05_SPC.csv"))
CARI_storm4a_08_05_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4a_08_05_turb.csv"))

# CARI_storm4b_08_06 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4b_08_06.csv"))
CARI_storm4b_08_06_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4b_08_06_Q.csv"))
CARI_storm4b_08_06_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4b_08_06_NO3.csv"))
CARI_storm4b_08_06_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4b_08_06_fDOM.csv"))
CARI_storm4b_08_06_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4b_08_06_SPC.csv"))
CARI_storm4b_08_06_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm4b_08_06_turb.csv"))

# CARI_storm5_08_08 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm5_08_08.csv"))
CARI_storm5_08_08_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm5_08_08_Q.csv"))
CARI_storm5_08_08_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm5_08_08_NO3.csv"))
CARI_storm5_08_08_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm5_08_08_fDOM.csv"))
CARI_storm5_08_08_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm5_08_08_SPC.csv"))
CARI_storm5_08_08_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm5_08_08_turb.csv"))

# CARI_storm6_08_15 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm6_08_15.csv"))
CARI_storm6_08_15_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm6_08_15_Q.csv"))
CARI_storm6_08_15_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm6_08_15_NO3.csv"))
CARI_storm6_08_15_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm6_08_15_fDOM.csv"))
CARI_storm6_08_15_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm6_08_15_SPC.csv"))
CARI_storm6_08_15_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm6_08_15_turb.csv"))

# CARI_storm7_08_18 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm7_08_18.csv"))
CARI_storm7_08_18_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm7_08_18_Q.csv"))
CARI_storm7_08_18_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm7_08_18_NO3.csv"))
CARI_storm7_08_18_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm7_08_18_fDOM.csv"))
CARI_storm7_08_18_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm7_08_18_SPC.csv"))
CARI_storm7_08_18_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm7_08_18_turb.csv"))

# CARI_storm8_09_04 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm8_09_04.csv"))
CARI_storm8_09_04_Q <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm8_09_04_Q.csv"))
CARI_storm8_09_04_NO3 <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm8_09_04_NO3.csv"))
CARI_storm8_09_04_fDOM <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm8_09_04_fDOM.csv"))
CARI_storm8_09_04_SPC <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm8_09_04_SPC.csv"))
CARI_storm8_09_04_Turb <- read_csv(here("Storm_events", "2022", "CARI", "CARI_storm8_09_04_turb.csv"))

# normalize
dfList <- Filter(function(x) is(x, "data.frame"), mget(ls()))

for(i in 1:length(dfList)) {
  dfList[[i]][["datavalue"]] = 
    (dfList[[i]][["datavalue"]] - min(dfList[[i]][["datavalue"]], na.rm=T)) / (max(dfList[[i]][["datavalue"]], na.rm=T) - min(dfList[[i]][["datavalue"]], na.rm=T))
}
list2env(dfList ,.GlobalEnv)

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

# plot CARI loops #

# NO3 #
CARI_storm1_07_15_NO3.p = hyst_plot(CARI_storm1_07_15_Q, CARI_storm1_07_15_NO3, "CARI", "NO3", "0715")
CARI_storm2_07_19_NO3.p = hyst_plot(CARI_storm2_07_19_Q, CARI_storm2_07_19_NO3, "CARI", "NO3", "0719")
CARI_storm3_08_01_NO3.p = hyst_plot(CARI_storm3_08_01_Q, CARI_storm3_08_01_NO3, "CARI", "NO3", "0801")
CARI_storm4a_08_05_NO3.p = hyst_plot(CARI_storm4a_08_05_Q, CARI_storm4a_08_05_NO3, "CARI", "NO3", "0805a")
CARI_storm4b_08_06_NO3.p = hyst_plot(CARI_storm4b_08_06_Q, CARI_storm4b_08_06_NO3, "CARI", "NO3", "0806b")
CARI_storm5_08_08_NO3.p = hyst_plot(CARI_storm5_08_08_Q, CARI_storm5_08_08_NO3, "CARI", "NO3", "0808")
CARI_storm6_08_15_NO3.p = hyst_plot(CARI_storm6_08_15_Q, CARI_storm6_08_15_NO3, "CARI", "NO3", "0815")
CARI_storm7_08_18_NO3.p = hyst_plot(CARI_storm7_08_18_Q, CARI_storm7_08_18_NO3, "CARI", "NO3", "0818")
CARI_storm8_09_04_NO3.p = hyst_plot(CARI_storm8_09_04_Q, CARI_storm8_09_04_NO3, "CARI", "NO3", "0904")

multiplot(CARI_storm1_07_15_NO3.p) 
multiplot(CARI_storm2_07_19_NO3.p) 
multiplot(CARI_storm3_08_01_NO3.p) 
multiplot(CARI_storm4a_08_05_NO3.p) 
multiplot(CARI_storm4b_08_06_NO3.p) 
multiplot(CARI_storm5_08_08_NO3.p) 
multiplot(CARI_storm6_08_15_NO3.p) 
multiplot(CARI_storm7_08_18_NO3.p) 
multiplot(CARI_storm8_09_04_NO3.p) 

# fDOM #
CARI_storm1_07_15_fDOM.p = hyst_plot(CARI_storm1_07_15_Q, CARI_storm1_07_15_fDOM, "CARI", "fDOM", "0715")
CARI_storm2_07_19_fDOM.p = hyst_plot(CARI_storm2_07_19_Q, CARI_storm2_07_19_fDOM, "CARI", "fDOM", "0719")
CARI_storm3_08_01_fDOM.p = hyst_plot(CARI_storm3_08_01_Q, CARI_storm3_08_01_fDOM, "CARI", "fDOM", "0801")
CARI_storm4a_08_05_fDOM.p = hyst_plot(CARI_storm4a_08_05_Q, CARI_storm4a_08_05_fDOM, "CARI", "fDOM", "0805a")
CARI_storm4b_08_06_fDOM.p = hyst_plot(CARI_storm4b_08_06_Q, CARI_storm4b_08_06_fDOM, "CARI", "fDOM", "0806b")
CARI_storm5_08_08_fDOM.p = hyst_plot(CARI_storm5_08_08_Q, CARI_storm5_08_08_fDOM, "CARI", "fDOM", "0808")
CARI_storm6_08_15_fDOM.p = hyst_plot(CARI_storm6_08_15_Q, CARI_storm6_08_15_fDOM, "CARI", "fDOM", "0815")
CARI_storm7_08_18_fDOM.p = hyst_plot(CARI_storm7_08_18_Q, CARI_storm7_08_18_fDOM, "CARI", "fDOM", "0818")
CARI_storm8_09_04_fDOM.p = hyst_plot(CARI_storm8_09_04_Q, CARI_storm8_09_04_fDOM, "CARI", "fDOM", "0904")

multiplot(CARI_storm1_07_15_fDOM.p) 
multiplot(CARI_storm2_07_19_fDOM.p) 
multiplot(CARI_storm3_08_01_fDOM.p) 
multiplot(CARI_storm4a_08_05_fDOM.p) 
multiplot(CARI_storm4b_08_06_fDOM.p) 
multiplot(CARI_storm5_08_08_fDOM.p) 
multiplot(CARI_storm6_08_15_fDOM.p) 
multiplot(CARI_storm7_08_18_fDOM.p) 
multiplot(CARI_storm8_09_04_fDOM.p)  

# SPC #
CARI_storm1_07_15_SPC.p = hyst_plot(CARI_storm1_07_15_Q, CARI_storm1_07_15_SPC, "CARI", "SPC", "0715")
CARI_storm2_07_19_SPC.p = hyst_plot(CARI_storm2_07_19_Q, CARI_storm2_07_19_SPC, "CARI", "SPC", "0719")
CARI_storm3_08_01_SPC.p = hyst_plot(CARI_storm3_08_01_Q, CARI_storm3_08_01_SPC, "CARI", "SPC", "0801")
CARI_storm4a_08_05_SPC.p = hyst_plot(CARI_storm4a_08_05_Q, CARI_storm4a_08_05_SPC, "CARI", "SPC", "0805a")
CARI_storm4b_08_06_SPC.p = hyst_plot(CARI_storm4b_08_06_Q, CARI_storm4b_08_06_SPC, "CARI", "SPC", "0806b")
CARI_storm5_08_08_SPC.p = hyst_plot(CARI_storm5_08_08_Q, CARI_storm5_08_08_SPC, "CARI", "SPC", "0808")
CARI_storm6_08_15_SPC.p = hyst_plot(CARI_storm6_08_15_Q, CARI_storm6_08_15_SPC, "CARI", "SPC", "0815")
CARI_storm7_08_18_SPC.p = hyst_plot(CARI_storm7_08_18_Q, CARI_storm7_08_18_SPC, "CARI", "SPC", "0818")
CARI_storm8_09_04_SPC.p = hyst_plot(CARI_storm8_09_04_Q, CARI_storm8_09_04_SPC, "CARI", "SPC", "0904")

multiplot(CARI_storm1_07_15_SPC.p) 
multiplot(CARI_storm2_07_19_SPC.p) 
multiplot(CARI_storm3_08_01_SPC.p) 
multiplot(CARI_storm4a_08_05_SPC.p) 
multiplot(CARI_storm4b_08_06_SPC.p) 
multiplot(CARI_storm5_08_08_SPC.p) 
multiplot(CARI_storm6_08_15_SPC.p) 
multiplot(CARI_storm7_08_18_SPC.p) 
multiplot(CARI_storm8_09_04_SPC.p) 

# turb #
CARI_storm1_07_15_turb.p = hyst_plot(CARI_storm1_07_15_Q, CARI_storm1_07_15_Turb, "CARI", "Turb", "0715")
CARI_storm2_07_19_turb.p = hyst_plot(CARI_storm2_07_19_Q, CARI_storm2_07_19_Turb, "CARI", "Turb", "0719")
CARI_storm3_08_01_turb.p = hyst_plot(CARI_storm3_08_01_Q, CARI_storm3_08_01_Turb, "CARI", "Turb", "0801")
CARI_storm4a_08_05_turb.p = hyst_plot(CARI_storm4a_08_05_Q, CARI_storm4a_08_05_Turb, "CARI", "Turb", "0805a")
CARI_storm4b_08_06_turb.p = hyst_plot(CARI_storm4b_08_06_Q, CARI_storm4b_08_06_Turb, "CARI", "Turb", "0806b")
CARI_storm5_08_08_turb.p = hyst_plot(CARI_storm5_08_08_Q, CARI_storm5_08_08_Turb, "CARI", "Turb", "0808")
CARI_storm6_08_15_turb.p = hyst_plot(CARI_storm6_08_15_Q, CARI_storm6_08_15_Turb, "CARI", "Turb", "0815")
CARI_storm7_08_18_turb.p = hyst_plot(CARI_storm7_08_18_Q, CARI_storm7_08_18_Turb, "CARI", "Turb", "0818")
CARI_storm8_09_04_turb.p = hyst_plot(CARI_storm8_09_04_Q, CARI_storm8_09_04_Turb, "CARI", "Turb", "0904")

multiplot(CARI_storm1_07_15_turb.p) 
multiplot(CARI_storm2_07_19_turb.p) 
multiplot(CARI_storm3_08_01_turb.p) 
multiplot(CARI_storm4a_08_05_turb.p) 
multiplot(CARI_storm4b_08_06_turb.p) 
multiplot(CARI_storm5_08_08_turb.p) 
multiplot(CARI_storm6_08_15_turb.p) 
multiplot(CARI_storm7_08_18_turb.p) 
multiplot(CARI_storm8_09_04_turb.p) 

# Plot all the storms that are correct:

CARI_HI_Loop <- multiplot(CARI_storm1_07_15_NO3.p,CARI_storm1_07_15_fDOM.p, CARI_storm1_07_15_SPC.p,CARI_storm1_07_15_turb.p,
                          CARI_storm2_07_19_NO3.p,CARI_storm2_07_19_fDOM.p, CARI_storm2_07_19_SPC.p,CARI_storm2_07_19_turb.p, 
                          CARI_storm3_08_01_NO3.p,CARI_storm3_08_01_fDOM.p, CARI_storm3_08_01_SPC.p,CARI_storm3_08_01_turb.p, 
                          CARI_storm4a_08_05_NO3.p,CARI_storm4a_08_05_fDOM.p, CARI_storm4a_08_05_SPC.p,CARI_storm4a_08_05_turb.p, 
                          CARI_storm4b_08_06_NO3.p,CARI_storm4b_08_06_fDOM.p, CARI_storm4b_08_06_SPC.p,CARI_storm4b_08_06_turb.p, 
                          CARI_storm5_08_08_NO3.p,CARI_storm5_08_08_fDOM.p, CARI_storm5_08_08_SPC.p,CARI_storm5_08_08_turb.p, 
                          CARI_storm6_08_15_NO3.p,CARI_storm6_08_15_fDOM.p, CARI_storm6_08_15_SPC.p,CARI_storm6_08_15_turb.p, 
                          CARI_storm7_08_18_NO3.p,CARI_storm7_08_18_fDOM.p, CARI_storm7_08_18_SPC.p,CARI_storm7_08_18_turb.p, 
                          CARI_storm8_09_04_NO3.p,CARI_storm8_09_04_fDOM.p, CARI_storm8_09_04_SPC.p,CARI_storm8_09_04_turb.p)

# export pdf 20 x 30 #
ggsave("CARI_HI_Loops_2022.pdf",
       path = here("plots", "02_Hysteresis", "2022"),
       width = 20, height = 30, units = "in")








