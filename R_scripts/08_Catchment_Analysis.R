#### READ ME ####
# The purpose of this script is to read in catchment characteristics and determine which 
  # candidate predictors to ues for our models 
# Step 1: Load in AK_Polygon data with catchment characteristcs for all sites
# Step 2: scatterplot matrix of all candidate predictors 
# Step 3: VIFs for catchment characteristics (>2 is no bueno)

library(tidyverse)
library(stats)
library(readr)
library(plotly)
library(GGally)
library(ggpmisc)
library(ggpubr)
library(ggExtra)
library(lubridate)
library(nlme)
library(MuMIn)
library(multcomp)
library(here)
library(dataRetrieval)
library(RColorBrewer)
library(gridExtra)
library(zoo)

###### CATCHMENT CHARACTERISTICS ####
# Read in polygon data 
catchment <- read.csv(here("Ancillary_data", "AK_polys_190903_Predictors.csv"))

catchment <- catchment[c("site","SLOPE_MEAN", "areaburn_lg", "pctburn_lg", "Pf_Prob_1m_mean_x", "NDVI_p50__mean")] # selecting the columns that I want

# Manually adjusting the PF extent from the most updated torre and Neal PF maps (11/1/2023)

catchment <- catchment %>% 
  mutate(Pf_Prob_1m_mean_x = case_when(site == 'Caribou_CJ' ~ 29.3,
                                       site == 'French' ~ 32.9,
                                       site == 'Poker_PJ' ~ 25.3,
                                       site == 'Moose' ~ 38.4,
                                       site == 'Vault' ~ 58.4,
                                       site == 'Stuart' ~ 30.8,
                                       TRUE ~ Pf_Prob_1m_mean_x))


ggpairs(catchment,
        columns = c("SLOPE_MEAN", "areaburn_lg", "pctburn_lg", "Pf_Prob_1m_mean_x", "NDVI_p50__mean"),
        title="Correlation matrix: All sites") 
# this shows that slope and PF are highly correlated
###***TKH: Slope & pfrost r = -0.34. Coefficients <0.5 are weak. This is across the full dataset though. Let's look at just the 6 catchments we are addressing here.

highlight_df <- filter(catchment, site %in% c("Caribou_CJ", "French", "Poker_PJ",
                                              "Moose", "Vault", "Stuart"))

ggpairs(highlight_df,
        columns = c("SLOPE_MEAN", "areaburn_lg", "pctburn_lg", "Pf_Prob_1m_mean_x", "NDVI_p50__mean"),
        title="Correlation matrix: All sites") 
## pfrost & slope negatively correlated, but driven largely by one point with strong leverage (VAUL)
## pfrost & NDVI also negatively correlated, one major outlier

catchment %>% 
  ggplot(aes(x = SLOPE_MEAN, y = NDVI_p50__mean)) + 
  geom_point(alpha=0.3) +
  geom_point(data = highlight_df, 
             aes(x = SLOPE_MEAN, y = NDVI_p50__mean), 
             color='red',
             size=3)


# combine AMC and catchment_characteristics
AMC <- read.csv(here("Output_from_analysis", "07_Combine_HI_BETA_FI", "antecedent_HI_FI_AllYears.csv"))

DOD_catchment <- read.csv(here("Ancillary_data", "DOD_Sites_AK_polys_190903_Predictors.csv"))

# filter to 2020 attributes only. NDVI was available beginning 1984.
DOD_catchment <- DOD_catchment %>% filter(Year == 2020)

# Manually adjusting the PF extent from the most updated torre and Neal PF maps (11/1/2023)

DOD_catchment <- DOD_catchment %>% 
  mutate(Pf_Prob_1m_mean_x = case_when(site.ID == 'CARI' ~ 29.3,
                                       site.ID == 'FRCH' ~ 32.9,
                                       site.ID == 'POKE' ~ 25.3,
                                       site.ID == 'MOOS' ~ 38.4,
                                       site.ID == 'VAUL' ~ 58.4,
                                       site.ID == 'STRT' ~ 30.8,
                                       TRUE ~ Pf_Prob_1m_mean_x))


# 
AMC <- full_join(AMC, DOD_catchment)
# 
write.csv(AMC, here("Output_from_analysis", "08_Catchment_characteristics", "Antecedent_HI_BETA_Catchment.csv"))
# 
# 
AMC <- read.csv(here("Output_from_analysis", "08_Catchment_characteristics", "Antecedent_HI_BETA_Catchment.csv"))

# AMC <- read_csv("Output_from_analysis/08_Catchment_characteristics/Antecedent_HI_BETA_Catchment.csv")

## Fill missing pf & burn categories
AMC <- AMC %>% mutate(burn = ifelse(site.ID %in% c("POKE","STRT","MOOS"), "burned", "unburned")) %>%
               mutate(pf = ifelse(site.ID == "POKE", "low",
                              ifelse(site.ID %in% c("VAUL", "MOOS"), "high", "medium")))

########################################
### Summary statistics across storms ###
########################################
## Function to calculate CV
cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

## Across all years
st.all <- AMC %>% group_by(response_var, site.ID, pf, burn, Pf_Prob_1m_mean_x, pctburn_lg, SLOPE_MEAN, NDVI_p50__mean) %>%
                    summarize(across(c(Hyst_index, Flush_index, Beta_index), 
                                list(mn = ~ mean(.x, na.rm = TRUE),
                                     md = ~ median(.x, na.rm = TRUE),
                                     SD = ~ sd(.x, na.rm = TRUE),
                                     CV = ~ cv(.x))
                                    ))

## Within years
yr.st <- AMC %>% group_by(response_var, site.ID, year, pf, burn, Pf_Prob_1m_mean_x, pctburn_lg, SLOPE_MEAN, NDVI_p50__mean) %>%
                     summarize(across(c(Hyst_index, Flush_index, Beta_index), 
                                  list(mn = ~ mean(.x, na.rm = TRUE),
                                       md = ~ median(.x, na.rm = TRUE),
                                       SD = ~ sd(.x, na.rm = TRUE),
                                       CV = ~ cv(.x))
                                      ))

# Output summarized data
dir.create("summary_tables")                               
write.csv(st.all, here("summary_tables", "storms_summ_allyears.csv"), row.names = FALSE)
write.csv(yr.st, here("summary_tables", "storms_summ_byyear.csv"), row.names = FALSE)

###***TKH: replaced by st.all?
CV.all <- HI.median[!duplicated(HI.median$MedianHI), ] # removing duplicated rows 
CV.average.pf <- HI.median %>% 
  group_by(pf, response_var) %>% 
  dplyr::summarise(AverageCVHI = mean(CVhi, na.rm = TRUE),
                   AverageCVFI = mean(CVfi, na.rm = TRUE),
                   AverageCVBETA = mean(CVbeta, na.rm = TRUE))
CV.average.pf <- CV.average.pf[!duplicated(CV.average.pf$AverageCVHI), ] # removing duplicated rows 
write.table(CV.average.pf, file = "CV.average.pf.csv", sep = ",", col.names = NA,
            qmethod = "double")

NO3.catchment <- subset(HI.median, HI.median$response_var == "NO3")
fDOM.catchment <- subset(HI.median, HI.median$response_var == "fDOM")
SPC.catchment <- subset(HI.median, HI.median$response_var == "SPC")
turb.catchment <- subset(HI.median, HI.median$response_var == "turb")

###***TKH: I'm not following the analyses lines 148-247. These seem to be using annual values as replicates within each catchment. Would need a random effect of year.
#### HI ###
# PF 
ggplot(yr.st, aes(x = as.character(Pf_Prob_1m_mean_x), y = Hyst_index_md, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("HI_PF.pdf",
       path = here("plots", "Catchment_characteristics", "HI"),
       width = 7, height = 7)

# This is taking an ANOVA of the mean HI for each catchment 
one.way.no3 <- aov(MedianHI ~ Pf_Prob_1m_mean_x, data = NO3.catchment)
summary(one.way.no3) # seemingly not significant  (p-value =0.679)


TukeyHSD(one.way.no3, which = "site.ID", na.rm = TRUE)


# burn 
ggplot(HI.median, aes(x = pctburn_lg, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("HI_burn.pdf",
       path = here("plots", "Catchment_characteristics", "HI"),
       width = 7, height = 7)

# SLOPE 
ggplot(HI.median, aes(x = Slope, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("HI_slope.pdf",
       path = here("plots", "Catchment_characteristics", "HI"),
       width = 7, height = 7)

# NDVI 
ggplot(HI.median, aes(x = NDVI, y = MedianHI, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("HI_NDVI.pdf",
       path = here("plots", "Catchment_characteristics", "HI"),
       width = 7, height = 7)

#### BETA ###
# PF 
ggplot(HI.median, aes(x = Pf_Prob_1m_mean_x, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("BETA_PF.pdf",
       path = here("plots", "Catchment_characteristics", "BETA"),
       width = 7, height = 7)

# WF 
ggplot(HI.median, aes(x = pctburn_lg, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("BETA_WF.pdf",
       path = here("plots", "Catchment_characteristics", "BETA"),
       width = 7, height = 7)

# Slope 
ggplot(HI.median, aes(x = Slope, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("BETA_slope.pdf",
       path = here("plots", "Catchment_characteristics", "BETA"),
       width = 7, height = 7)

# NDVI 
ggplot(HI.median, aes(x = NDVI, y = MedianBETA, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic() +
  facet_wrap(~response_var)

ggsave("BETA_NDVI.pdf",
       path = here("plots", "Catchment_characteristics", "BETA"),
       width = 7, height = 7)

#


###### HI_BETA PLOTS ####
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
library(ggExtra)
library(ggpmisc)
library(ggpubr)

# Load in Antecedent moisture conditions dataframe
AMC <- read.csv(here("Output_from_analysis", "07_Combine_HI_BETA_FI", "antecedent_HI_FI_AllYears.csv"))

AMC <- AMC[c("Hyst_index","HI_ymin", "HI_ymax", "site.ID", "storm.ID", "month.x", "day.x",
             "response_var", "Flush_index","FI_ymin", "FI_ymax", "year", 
             "Parameter", "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", "t", 
             "df", "p", "precip", "temp", "precip.week", "precip.month", 
             "ThreeMonth", "temp.week", "TOTAL.TIME", "Intensity", "doy", "burn", "pf", 
             "date", "TimeSinceChena")] # selecting the columns that I want

colNames <- c("Hyst_index", "HI_ymin", "HI_ymax", "site.ID", "storm.ID", "month", 
              "day", "response_var", "Flush_index", "FI_ymin", "FI_ymax", "year", 
              "Parameter", "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", "t", 
              "df", "p", "StormPrecip", "StormTemp", "PrecipWeek", "PrecipMonth", 
              "ThreeMonth", "TempWeek", "Duration", "Intensity", "doy", "burn", "PF", 
              "date", "TimeSinceChena")

names(AMC)<- colNames # renaming columns

AMC <- AMC %>% 
  dplyr::mutate(across(c(PF),
                       ~ifelse(site.ID == "STRT" | site.ID == "VAUL", "High", "Moderate")))


vn = expression(paste(N*O[3]^"-"))
##subsetting by solute 
# NO3 #
HI_FI_NO3 = subset(AMC, response_var == "NO3")
# fDOM #
HI_FI_fDOM = subset(AMC, response_var == "fDOM")
# SPC #
HI_FI_SPC = subset(AMC, response_var == "SPC")
# turb #
HI_FI_turb = subset(AMC, response_var == "turb")



#### PLOTS ####

# 2015-2022 # 
HI_FI_NO3 <- subset(HI_FI_NO3, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
HI_FI_fDOM <- subset(HI_FI_fDOM, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
HI_FI_SPC <- subset(HI_FI_SPC, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
HI_FI_turb <- subset(HI_FI_turb, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")

# plots 
# NO3
vn = expression(paste(N*O[3]^"-"))

HI_BETA_NO3.p = 
  ggplot(HI_FI_NO3, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = PF), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle(vn)+ 
  ylab("") +
  xlab("") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "none") +
    labs(
      colour = "Catchment",
      shape = "PF Extent")

a <- ggMarginal(HI_BETA_NO3.p, groupColour = TRUE, groupFill = TRUE)
  
  
# fDOM
HI_FI_fDOM <- HI_FI_fDOM[-c(131, 190, 219,237), ]

HI_FI_fDOM <- HI_FI_fDOM %>% 
  mutate(across(c(pf),
                ~ifelse(pf == "medium", "Moderate", "High")))

HI_BETA_fDOM.p = 
    ggplot(HI_FI_fDOM, aes(Beta_index, Hyst_index)) + 
    geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
    geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
    geom_point(aes(colour = factor(site.ID), shape = PF), size = 2.5) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
    theme_bw() +
    ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
    ggtitle("fDOM")+ 
    ylab("HI") +
    xlab("") +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          text = element_text(size = 15),
          legend.position = "none") +
  guides(shape=guide_legend("Permafrost Extent"),
         col=guide_legend("Catchment"))

ggsave("fDOM_HI_BETA.pdf",
       path = here("plots", "HI_BETA"),
       width = 9, height = 9)

  
b <- ggMarginal(HI_BETA_fDOM.p, groupColour = TRUE, groupFill = TRUE)
 

# SPC
HI_BETA_SPC.p = 
  ggplot(HI_FI_SPC, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = PF), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
  ggtitle("SPC")+ 
  ylab("HI") +
  xlab("ß") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "none") 

c <- ggMarginal(HI_BETA_SPC.p, groupColour = TRUE, groupFill = TRUE)


# turb
HI_BETA_turb.p = 
  ggplot(HI_FI_turb, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = PF), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A"), "Permafrost Extent") + 
  theme_bw() +
  ylim(-1.5, 1.5) + 
  xlim(-1.5, 1.5) +
  ggtitle("Turbidity")+ 
  ylab("") +
  xlab("ß") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "none") 

d <- ggMarginal(HI_BETA_turb.p, groupColour = TRUE, groupFill = TRUE)



ggarrange(b,a,
          c,d,
          labels = c("A)", "B)",
                     "C)", "D)"))

ggsave("HI_BETA.pdf",
       path = here("plots", "HI_BETA"),
       width = 9, height = 9)


#### Storm summary stats ####
### Investigating which storms are negative beta for Turbidity ####
# by year 
HI_FI_turb_year <- HI_FI_turb
HI_FI_turb_year$year <- as.character(HI_FI_turb_year$year)

HI_FI_turb_year_2021 <- subset(HI_FI_turb_year, year == "2021")

ggplot(HI_FI_turb_year_2021, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID)), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-5, 5)+
  ggtitle("Turb")+ 
  ylab("") +
  xlab("BETA") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "bottom") 

ggplot(HI_FI_turb_year, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
  geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
  geom_point(aes(colour = factor(site.ID), shape = year), size = 2.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
  theme_bw() +
  ylim(-1.5, 1.5) + xlim(-5, 5)+
  ggtitle("Turb")+ 
  ylab("") +
  xlab("BETA") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(size = 15),
        legend.position = "bottom") 

#######################################################
### Count storms in each of the 4 FI_Beta quadrants ###
#######################################################
# this is telling me what percentage of storms fall in each quadrant and tells me how many cross 0 which are non-significant

#NO3
table(sign(HI_FI_NO3$Hyst_index))
which(HI_FI_NO3$HI_ymin < 0 & HI_FI_NO3$HI_ymax > 0 & HI_FI_NO3$Hyst_index > 0)
which(HI_FI_NO3$HI_ymin < 0 & HI_FI_NO3$HI_ymax > 0 & HI_FI_NO3$Hyst_index < 0)

table(sign(HI_FI_NO3$Beta_index))
which(HI_FI_NO3$Beta_ymin < 0 & HI_FI_NO3$Beta_ymax > 0 & HI_FI_NO3$Beta_index > 0)
which(HI_FI_NO3$Beta_ymin < 0 & HI_FI_NO3$Beta_ymax > 0 & HI_FI_NO3$Beta_index < 0)



#fDOM
table(sign(HI_FI_fDOM$Hyst_index))
which(HI_FI_fDOM$HI_ymin < 0 & HI_FI_fDOM$HI_ymax > 0 & HI_FI_fDOM$Hyst_index > 0)
which(HI_FI_fDOM$HI_ymin < 0 & HI_FI_fDOM$HI_ymax > 0 & HI_FI_fDOM$Hyst_index < 0)

table(sign(HI_FI_fDOM$Beta_index))
which(HI_FI_fDOM$Beta_ymin < 0 & HI_FI_fDOM$Beta_ymax > 0 & HI_FI_fDOM$Beta_index > 0)
which(HI_FI_fDOM$Beta_ymin < 0 & HI_FI_fDOM$Beta_ymax > 0 & HI_FI_fDOM$Beta_index < 0)

MOOS.fDOM <- subset(HI_FI_fDOM, site.ID == "MOOS")
table(sign(MOOS.fDOM$Beta_index))
which(MOOS.fDOM$Beta_ymin < 0 & MOOS.fDOM$Beta_ymax > 0 & MOOS.fDOM$Beta_index > 0)
which(MOOS.fDOM$Beta_ymin < 0 & MOOS.fDOM$Beta_ymax > 0 & MOOS.fDOM$Beta_index < 0)

VAUL.fDOM <- subset(HI_FI_fDOM, site.ID == "VAUL")
table(sign(VAUL.fDOM$Beta_index))
which(VAUL.fDOM$Beta_ymin < 0 & VAUL.fDOM$Beta_ymax > 0 & VAUL.fDOM$Beta_index > 0)
which(VAUL.fDOM$Beta_ymin < 0 & VAUL.fDOM$Beta_ymax > 0 & VAUL.fDOM$Beta_index < 0)

CARI.fDOM <- subset(HI_FI_fDOM, site.ID == "CARI")
table(sign(CARI.fDOM$Beta_index))
which(CARI.fDOM$Beta_ymin < 0 & CARI.fDOM$Beta_ymax > 0 & CARI.fDOM$Beta_index > 0)
which(CARI.fDOM$Beta_ymin < 0 & CARI.fDOM$Beta_ymax > 0 & CARI.fDOM$Beta_index < 0)

#SPC
table(sign(HI_FI_SPC$Hyst_index))
which(HI_FI_SPC$HI_ymin < 0 & HI_FI_SPC$HI_ymax > 0 & HI_FI_SPC$Hyst_index > 0)
which(HI_FI_SPC$HI_ymin < 0 & HI_FI_SPC$HI_ymax > 0 & HI_FI_SPC$Hyst_index < 0)

table(sign(HI_FI_SPC$Beta_index))
which(HI_FI_SPC$Beta_ymin < 0 & HI_FI_SPC$Beta_ymax > 0 & HI_FI_SPC$Beta_index > 0)
which(HI_FI_SPC$Beta_ymin < 0 & HI_FI_SPC$Beta_ymax > 0 & HI_FI_SPC$Beta_index < 0)


#Turb
table(sign(HI_FI_turb$Hyst_index))
which(HI_FI_turb$HI_ymin < 0 & HI_FI_turb$HI_ymax > 0 & HI_FI_turb$Hyst_index > 0)
which(HI_FI_turb$HI_ymin < 0 & HI_FI_turb$HI_ymax > 0 & HI_FI_turb$Hyst_index < 0)

table(sign(HI_FI_turb$Beta_index))
which(HI_FI_turb$Beta_ymin < 0 & HI_FI_turb$Beta_ymax > 0 & HI_FI_turb$Beta_index > 0)
which(HI_FI_turb$Beta_ymin < 0 & HI_FI_turb$Beta_ymax > 0 & HI_FI_turb$Beta_index < 0)


# by site and year  ####
# 2015  ####
# FRCH ####
# NO3
HI_FI_NO3_FRCH_2015 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2015")

table(sign(HI_FI_NO3_FRCH_2015$Hyst_index))
which(HI_FI_NO3_FRCH_2015$HI_ymin < 0 & HI_FI_NO3_FRCH_2015$HI_ymax > 0 & HI_FI_NO3_FRCH_2015$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2015$HI_ymin < 0 & HI_FI_NO3_FRCH_2015$HI_ymax > 0 & HI_FI_NO3_FRCH_2015$Hyst_index < 0)

table(sign(HI_FI_NO3_FRCH_2015$Beta_index))
which(HI_FI_NO3_FRCH_2015$Beta_ymin < 0 & HI_FI_NO3_FRCH_2015$Beta_ymax > 0 & HI_FI_NO3_FRCH_2015$Beta_index > 0)
which(HI_FI_NO3_FRCH_2015$Beta_ymin < 0 & HI_FI_NO3_FRCH_2015$Beta_ymax > 0 & HI_FI_NO3_FRCH_2015$Beta_index < 0)

# fDOM
HI_FI_fDOM_FRCH_2015 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2015")

table(sign(HI_FI_fDOM_FRCH_2015$Hyst_index))
which(HI_FI_fDOM_FRCH_2015$HI_ymin < 0 & HI_FI_fDOM_FRCH_2015$HI_ymax > 0 & HI_FI_fDOM_FRCH_2015$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2015$HI_ymin < 0 & HI_FI_fDOM_FRCH_2015$HI_ymax > 0 & HI_FI_fDOM_FRCH_2015$Hyst_index < 0)

table(sign(HI_FI_fDOM_FRCH_2015$Beta_index))
which(HI_FI_fDOM_FRCH_2015$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2015$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2015$Beta_index > 0)
which(HI_FI_fDOM_FRCH_2015$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2015$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2015$Beta_index < 0)

#SPC
HI_FI_SPC_FRCH_2015 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2015")

table(sign(HI_FI_SPC_FRCH_2015$Hyst_index))
which(HI_FI_SPC_FRCH_2015$HI_ymin < 0 & HI_FI_SPC_FRCH_2015$HI_ymax > 0 & HI_FI_SPC_FRCH_2015$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2015$HI_ymin < 0 & HI_FI_SPC_FRCH_2015$HI_ymax > 0 & HI_FI_SPC_FRCH_2015$Hyst_index < 0)

table(sign(HI_FI_SPC_FRCH_2015$Beta_index))
which(HI_FI_SPC_FRCH_2015$Beta_ymin < 0 & HI_FI_SPC_FRCH_2015$Beta_ymax > 0 & HI_FI_SPC_FRCH_2015$Beta_index > 0)
which(HI_FI_SPC_FRCH_2015$Beta_ymin < 0 & HI_FI_SPC_FRCH_2015$Beta_ymax > 0 & HI_FI_SPC_FRCH_2015$Beta_index < 0)

#turb
HI_FI_turb_FRCH_2015 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2015")

table(sign(HI_FI_turb_FRCH_2015$Hyst_index))
which(HI_FI_turb_FRCH_2015$HI_ymin < 0 & HI_FI_turb_FRCH_2015$HI_ymax > 0 & HI_FI_turb_FRCH_2015$Hyst_index > 0)
which(HI_FI_turb_FRCH_2015$HI_ymin < 0 & HI_FI_turb_FRCH_2015$HI_ymax > 0 & HI_FI_turb_FRCH_2015$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2015$Beta_index))
which(HI_FI_turb_FRCH_2015$Beta_ymin < 0 & HI_FI_turb_FRCH_2015$Beta_ymax > 0 & HI_FI_turb_FRCH_2015$Beta_index > 0)
which(HI_FI_turb_FRCH_2015$Beta_ymin < 0 & HI_FI_turb_FRCH_2015$Beta_ymax > 0 & HI_FI_turb_FRCH_2015$Beta_index < 0)

# MOOS ####
# NO3 
HI_FI_NO3_MOOS_2015 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2015")

table(sign(HI_FI_NO3_MOOS_2015$Hyst_index))
which(HI_FI_NO3_MOOS_2015$HI_ymin < 0 & HI_FI_NO3_MOOS_2015$HI_ymax > 0 & HI_FI_NO3_MOOS_2015$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2015$HI_ymin < 0 & HI_FI_NO3_MOOS_2015$HI_ymax > 0 & HI_FI_NO3_MOOS_2015$Hyst_index < 0)

table(sign(HI_FI_NO3_MOOS_2015$Beta_index))
which(HI_FI_NO3_MOOS_2015$Beta_ymin < 0 & HI_FI_NO3_MOOS_2015$Beta_ymax > 0 & HI_FI_NO3_MOOS_2015$Beta_index > 0)
which(HI_FI_NO3_MOOS_2015$Beta_ymin < 0 & HI_FI_NO3_MOOS_2015$Beta_ymax > 0 & HI_FI_NO3_MOOS_2015$Beta_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2015 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2015")

table(sign(HI_FI_fDOM_MOOS_2015$Hyst_index))
which(HI_FI_fDOM_MOOS_2015$HI_ymin < 0 & HI_FI_fDOM_MOOS_2015$HI_ymax > 0 & HI_FI_fDOM_MOOS_2015$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2015$HI_ymin < 0 & HI_FI_fDOM_MOOS_2015$HI_ymax > 0 & HI_FI_fDOM_MOOS_2015$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2015$Beta_index))
which(HI_FI_fDOM_MOOS_2015$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2015$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2015$Beta_index > 0)
which(HI_FI_fDOM_MOOS_2015$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2015$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2015$Beta_index < 0)

#SPC
HI_FI_SPC_MOOS_2015 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2015")

table(sign(HI_FI_SPC_MOOS_2015$Hyst_index))
which(HI_FI_SPC_MOOS_2015$HI_ymin < 0 & HI_FI_SPC_MOOS_2015$HI_ymax > 0 & HI_FI_SPC_MOOS_2015$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2015$HI_ymin < 0 & HI_FI_SPC_MOOS_2015$HI_ymax > 0 & HI_FI_SPC_MOOS_2015$Hyst_index < 0)

table(sign(HI_FI_SPC_MOOS_2015$Beta_index))
which(HI_FI_SPC_MOOS_2015$Beta_ymin < 0 & HI_FI_SPC_MOOS_2015$Beta_ymax > 0 & HI_FI_SPC_MOOS_2015$Beta_index > 0)
which(HI_FI_SPC_MOOS_2015$Beta_ymin < 0 & HI_FI_SPC_MOOS_2015$Beta_ymax > 0 & HI_FI_SPC_MOOS_2015$Beta_index < 0)

#turb
HI_FI_turb_MOOS_2015 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2015")

table(sign(HI_FI_turb_MOOS_2015$Hyst_index))
which(HI_FI_turb_MOOS_2015$HI_ymin < 0 & HI_FI_turb_MOOS_2015$HI_ymax > 0 & HI_FI_turb_MOOS_2015$Hyst_index > 0)
which(HI_FI_turb_MOOS_2015$HI_ymin < 0 & HI_FI_turb_MOOS_2015$HI_ymax > 0 & HI_FI_turb_MOOS_2015$Hyst_index < 0)

table(sign(HI_FI_turb_MOOS_2015$Beta_index))
which(HI_FI_turb_MOOS_2015$Beta_ymin < 0 & HI_FI_turb_MOOS_2015$Beta_ymax > 0 & HI_FI_turb_MOOS_2015$Beta_index > 0)
which(HI_FI_turb_MOOS_2015$Beta_ymin < 0 & HI_FI_turb_MOOS_2015$Beta_ymax > 0 & HI_FI_turb_MOOS_2015$Beta_index < 0)


# 2018  ####
# FRCH ####
# NO3
HI_FI_NO3_FRCH_2018 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_NO3_FRCH_2018$Hyst_index))
which(HI_FI_NO3_FRCH_2018$HI_ymin < 0 & HI_FI_NO3_FRCH_2018$HI_ymax > 0 & HI_FI_NO3_FRCH_2018$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2018$HI_ymin < 0 & HI_FI_NO3_FRCH_2018$HI_ymax > 0 & HI_FI_NO3_FRCH_2018$Hyst_index < 0)

table(sign(HI_FI_NO3_FRCH_2018$Beta_index))
which(HI_FI_NO3_FRCH_2018$Beta_ymin < 0 & HI_FI_NO3_FRCH_2018$Beta_ymax > 0 & HI_FI_NO3_FRCH_2018$Beta_index > 0)
which(HI_FI_NO3_FRCH_2018$Beta_ymin < 0 & HI_FI_NO3_FRCH_2018$Beta_ymax > 0 & HI_FI_NO3_FRCH_2018$Beta_index < 0)

# fDOM
HI_FI_fDOM_FRCH_2018 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_fDOM_FRCH_2018$Hyst_index))
which(HI_FI_fDOM_FRCH_2018$HI_ymin < 0 & HI_FI_fDOM_FRCH_2018$HI_ymax > 0 & HI_FI_fDOM_FRCH_2018$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2018$HI_ymin < 0 & HI_FI_fDOM_FRCH_2018$HI_ymax > 0 & HI_FI_fDOM_FRCH_2018$Hyst_index < 0)

table(sign(HI_FI_fDOM_FRCH_2018$Beta_index))
which(HI_FI_fDOM_FRCH_2018$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2018$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2018$Beta_index > 0)
which(HI_FI_fDOM_FRCH_2018$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2018$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2018$Beta_index < 0)

#SPC
HI_FI_SPC_FRCH_2018 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_SPC_FRCH_2018$Hyst_index))
which(HI_FI_SPC_FRCH_2018$HI_ymin < 0 & HI_FI_SPC_FRCH_2018$HI_ymax > 0 & HI_FI_SPC_FRCH_2018$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2018$HI_ymin < 0 & HI_FI_SPC_FRCH_2018$HI_ymax > 0 & HI_FI_SPC_FRCH_2018$Hyst_index < 0)

table(sign(HI_FI_SPC_FRCH_2018$Beta_index))
which(HI_FI_SPC_FRCH_2018$Beta_ymin < 0 & HI_FI_SPC_FRCH_2018$Beta_ymax > 0 & HI_FI_SPC_FRCH_2018$Beta_index > 0)
which(HI_FI_SPC_FRCH_2018$Beta_ymin < 0 & HI_FI_SPC_FRCH_2018$Beta_ymax > 0 & HI_FI_SPC_FRCH_2018$Beta_index < 0)

#turb
HI_FI_turb_FRCH_2018 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2018")

table(sign(HI_FI_turb_FRCH_2018$Hyst_index))
which(HI_FI_turb_FRCH_2018$HI_ymin < 0 & HI_FI_turb_FRCH_2018$HI_ymax > 0 & HI_FI_turb_FRCH_2018$Hyst_index > 0)
which(HI_FI_turb_FRCH_2018$HI_ymin < 0 & HI_FI_turb_FRCH_2018$HI_ymax > 0 & HI_FI_turb_FRCH_2018$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2018$Beta_index))
which(HI_FI_turb_FRCH_2018$Beta_ymin < 0 & HI_FI_turb_FRCH_2018$Beta_ymax > 0 & HI_FI_turb_FRCH_2018$Beta_index > 0)
which(HI_FI_turb_FRCH_2018$Beta_ymin < 0 & HI_FI_turb_FRCH_2018$Beta_ymax > 0 & HI_FI_turb_FRCH_2018$Beta_index < 0)

# MOOS ####
# NO3 
HI_FI_NO3_MOOS_2018 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_NO3_MOOS_2018$Hyst_index))
which(HI_FI_NO3_MOOS_2018$HI_ymin < 0 & HI_FI_NO3_MOOS_2018$HI_ymax > 0 & HI_FI_NO3_MOOS_2018$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2018$HI_ymin < 0 & HI_FI_NO3_MOOS_2018$HI_ymax > 0 & HI_FI_NO3_MOOS_2018$Hyst_index < 0)

table(sign(HI_FI_NO3_MOOS_2018$Beta_index))
which(HI_FI_NO3_MOOS_2018$Beta_ymin < 0 & HI_FI_NO3_MOOS_2018$Beta_ymax > 0 & HI_FI_NO3_MOOS_2018$Beta_index > 0)
which(HI_FI_NO3_MOOS_2018$Beta_ymin < 0 & HI_FI_NO3_MOOS_2018$Beta_ymax > 0 & HI_FI_NO3_MOOS_2018$Beta_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2018 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_fDOM_MOOS_2018$Hyst_index))
which(HI_FI_fDOM_MOOS_2018$HI_ymin < 0 & HI_FI_fDOM_MOOS_2018$HI_ymax > 0 & HI_FI_fDOM_MOOS_2018$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2018$HI_ymin < 0 & HI_FI_fDOM_MOOS_2018$HI_ymax > 0 & HI_FI_fDOM_MOOS_2018$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2018$Beta_index))
which(HI_FI_fDOM_MOOS_2018$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2018$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2018$Beta_index > 0)
which(HI_FI_fDOM_MOOS_2018$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2018$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2018$Beta_index < 0)

#SPC
HI_FI_SPC_MOOS_2018 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_SPC_MOOS_2018$Hyst_index))
which(HI_FI_SPC_MOOS_2018$HI_ymin < 0 & HI_FI_SPC_MOOS_2018$HI_ymax > 0 & HI_FI_SPC_MOOS_2018$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2018$HI_ymin < 0 & HI_FI_SPC_MOOS_2018$HI_ymax > 0 & HI_FI_SPC_MOOS_2018$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2018$Beta_index))
which(HI_FI_SPC_MOOS_2018$Beta_ymin < 0 & HI_FI_SPC_MOOS_2018$Beta_ymax > 0 & HI_FI_SPC_MOOS_2018$Beta_index > 0)
which(HI_FI_SPC_MOOS_2018$Beta_ymin < 0 & HI_FI_SPC_MOOS_2018$Beta_ymax > 0 & HI_FI_SPC_MOOS_2018$Beta_index < 0)

#turb
HI_FI_turb_MOOS_2018 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2018")

table(sign(HI_FI_turb_MOOS_2018$Hyst_index))
which(HI_FI_turb_MOOS_2018$HI_ymin < 0 & HI_FI_turb_MOOS_2018$HI_ymax > 0 & HI_FI_turb_MOOS_2018$Hyst_index > 0)
which(HI_FI_turb_MOOS_2018$HI_ymin < 0 & HI_FI_turb_MOOS_2018$HI_ymax > 0 & HI_FI_turb_MOOS_2018$Hyst_index < 0)

table(sign(HI_FI_turb_MOOS_2018$Beta_index))
which(HI_FI_turb_MOOS_2018$Beta_ymin < 0 & HI_FI_turb_MOOS_2018$Beta_ymax > 0 & HI_FI_turb_MOOS_2018$Beta_index > 0)
which(HI_FI_turb_MOOS_2018$Beta_ymin < 0 & HI_FI_turb_MOOS_2018$Beta_ymax > 0 & HI_FI_turb_MOOS_2018$Beta_index < 0)

# CARI ####
# NO3 
HI_FI_NO3_CARI_2018 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_NO3_CARI_2018$Hyst_index))
which(HI_FI_NO3_CARI_2018$HI_ymin < 0 & HI_FI_NO3_CARI_2018$HI_ymax > 0 & HI_FI_NO3_CARI_2018$Hyst_index > 0)
which(HI_FI_NO3_CARI_2018$HI_ymin < 0 & HI_FI_NO3_CARI_2018$HI_ymax > 0 & HI_FI_NO3_CARI_2018$Hyst_index < 0)

table(sign(HI_FI_NO3_CARI_2018$Beta_index))
which(HI_FI_NO3_CARI_2018$Beta_ymin < 0 & HI_FI_NO3_CARI_2018$Beta_ymax > 0 & HI_FI_NO3_CARI_2018$Beta_index > 0)
which(HI_FI_NO3_CARI_2018$Beta_ymin < 0 & HI_FI_NO3_CARI_2018$Beta_ymax > 0 & HI_FI_NO3_CARI_2018$Beta_index < 0)

#fDOM 
HI_FI_fDOM_CARI_2018 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_fDOM_CARI_2018$Hyst_index))
which(HI_FI_fDOM_CARI_2018$HI_ymin < 0 & HI_FI_fDOM_CARI_2018$HI_ymax > 0 & HI_FI_fDOM_CARI_2018$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2018$HI_ymin < 0 & HI_FI_fDOM_CARI_2018$HI_ymax > 0 & HI_FI_fDOM_CARI_2018$Hyst_index < 0)

table(sign(HI_FI_fDOM_CARI_2018$Beta_index))
which(HI_FI_fDOM_CARI_2018$Beta_ymin < 0 & HI_FI_fDOM_CARI_2018$Beta_ymax > 0 & HI_FI_fDOM_CARI_2018$Beta_index > 0)
which(HI_FI_fDOM_CARI_2018$Beta_ymin < 0 & HI_FI_fDOM_CARI_2018$Beta_ymax > 0 & HI_FI_fDOM_CARI_2018$Beta_index < 0)

#SPC
HI_FI_SPC_CARI_2018 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_SPC_CARI_2018$Hyst_index))
which(HI_FI_SPC_CARI_2018$HI_ymin < 0 & HI_FI_SPC_CARI_2018$HI_ymax > 0 & HI_FI_SPC_CARI_2018$Hyst_index > 0)
which(HI_FI_SPC_CARI_2018$HI_ymin < 0 & HI_FI_SPC_CARI_2018$HI_ymax > 0 & HI_FI_SPC_CARI_2018$Hyst_index < 0)

table(sign(HI_FI_SPC_CARI_2018$Beta_index))
which(HI_FI_SPC_CARI_2018$Beta_ymin < 0 & HI_FI_SPC_CARI_2018$Beta_ymax > 0 & HI_FI_SPC_CARI_2018$Beta_index > 0)
which(HI_FI_SPC_CARI_2018$Beta_ymin < 0 & HI_FI_SPC_CARI_2018$Beta_ymax > 0 & HI_FI_SPC_CARI_2018$Beta_index < 0)

#turb
HI_FI_turb_CARI_2018 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2018")

table(sign(HI_FI_turb_CARI_2018$Hyst_index))
which(HI_FI_turb_CARI_2018$HI_ymin < 0 & HI_FI_turb_CARI_2018$HI_ymax > 0 & HI_FI_turb_CARI_2018$Hyst_index > 0)
which(HI_FI_turb_CARI_2018$HI_ymin < 0 & HI_FI_turb_CARI_2018$HI_ymax > 0 & HI_FI_turb_CARI_2018$Hyst_index < 0)

table(sign(HI_FI_turb_CARI_2018$Beta_index))
which(HI_FI_turb_CARI_2018$Beta_ymin < 0 & HI_FI_turb_CARI_2018$Beta_ymax > 0 & HI_FI_turb_CARI_2018$Beta_index > 0)
which(HI_FI_turb_CARI_2018$Beta_ymin < 0 & HI_FI_turb_CARI_2018$Beta_ymax > 0 & HI_FI_turb_CARI_2018$Beta_index < 0)

#

# 2019 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2019 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_NO3_FRCH_2019$Hyst_index))
which(HI_FI_NO3_FRCH_2019$HI_ymin < 0 & HI_FI_NO3_FRCH_2019$HI_ymax > 0 & HI_FI_NO3_FRCH_2019$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2019$HI_ymin < 0 & HI_FI_NO3_FRCH_2019$HI_ymax > 0 & HI_FI_NO3_FRCH_2019$Hyst_index < 0)

table(sign(HI_FI_NO3_FRCH_2019$Beta_index))
which(HI_FI_NO3_FRCH_2019$Beta_ymin < 0 & HI_FI_NO3_FRCH_2019$Beta_ymax > 0 & HI_FI_NO3_FRCH_2019$Beta_index > 0)
which(HI_FI_NO3_FRCH_2019$Beta_ymin < 0 & HI_FI_NO3_FRCH_2019$Beta_ymax > 0 & HI_FI_NO3_FRCH_2019$Beta_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2019 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_fDOM_FRCH_2019$Hyst_index))
which(HI_FI_fDOM_FRCH_2019$HI_ymin < 0 & HI_FI_fDOM_FRCH_2019$HI_ymax > 0 & HI_FI_fDOM_FRCH_2019$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2019$HI_ymin < 0 & HI_FI_fDOM_FRCH_2019$HI_ymax > 0 & HI_FI_fDOM_FRCH_2019$Hyst_index < 0)

table(sign(HI_FI_fDOM_FRCH_2019$Beta_index))
which(HI_FI_fDOM_FRCH_2019$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2019$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2019$Beta_index > 0)
which(HI_FI_fDOM_FRCH_2019$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2019$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2019$Beta_index < 0)

#SPC
HI_FI_SPC_FRCH_2019 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_SPC_FRCH_2019$Hyst_index))
which(HI_FI_SPC_FRCH_2019$HI_ymin < 0 & HI_FI_SPC_FRCH_2019$HI_ymax > 0 & HI_FI_SPC_FRCH_2019$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2019$HI_ymin < 0 & HI_FI_SPC_FRCH_2019$HI_ymax > 0 & HI_FI_SPC_FRCH_2019$Hyst_index < 0)

table(sign(HI_FI_SPC_FRCH_2019$Beta_index))
which(HI_FI_SPC_FRCH_2019$Beta_ymin < 0 & HI_FI_SPC_FRCH_2019$Beta_ymax > 0 & HI_FI_SPC_FRCH_2019$Beta_index > 0)
which(HI_FI_SPC_FRCH_2019$Beta_ymin < 0 & HI_FI_SPC_FRCH_2019$Beta_ymax > 0 & HI_FI_SPC_FRCH_2019$Beta_index < 0)

#turb
HI_FI_turb_FRCH_2019 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2019")

table(sign(HI_FI_turb_FRCH_2019$Hyst_index))
which(HI_FI_turb_FRCH_2019$HI_ymin < 0 & HI_FI_turb_FRCH_2019$HI_ymax > 0 & HI_FI_turb_FRCH_2019$Hyst_index > 0)
which(HI_FI_turb_FRCH_2019$HI_ymin < 0 & HI_FI_turb_FRCH_2019$HI_ymax > 0 & HI_FI_turb_FRCH_2019$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2019$Beta_index))
which(HI_FI_turb_FRCH_2019$Beta_ymin < 0 & HI_FI_turb_FRCH_2019$Beta_ymax > 0 & HI_FI_turb_FRCH_2019$Beta_index > 0)
which(HI_FI_turb_FRCH_2019$Beta_ymin < 0 & HI_FI_turb_FRCH_2019$Beta_ymax > 0 & HI_FI_turb_FRCH_2019$Beta_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2019 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_NO3_MOOS_2019$Hyst_index))
which(HI_FI_NO3_MOOS_2019$HI_ymin < 0 & HI_FI_NO3_MOOS_2019$HI_ymax > 0 & HI_FI_NO3_MOOS_2019$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2019$HI_ymin < 0 & HI_FI_NO3_MOOS_2019$HI_ymax > 0 & HI_FI_NO3_MOOS_2019$Hyst_index < 0)

table(sign(HI_FI_NO3_MOOS_2019$Beta_index))
which(HI_FI_NO3_MOOS_2019$Beta_ymin < 0 & HI_FI_NO3_MOOS_2019$Beta_ymax > 0 & HI_FI_NO3_MOOS_2019$Beta_index > 0)
which(HI_FI_NO3_MOOS_2019$Beta_ymin < 0 & HI_FI_NO3_MOOS_2019$Beta_ymax > 0 & HI_FI_NO3_MOOS_2019$Beta_index < 0)

#fDOM 
HI_FI_fDOM_MOOS_2019 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_fDOM_MOOS_2019$Hyst_index))
which(HI_FI_fDOM_MOOS_2019$HI_ymin < 0 & HI_FI_fDOM_MOOS_2019$HI_ymax > 0 & HI_FI_fDOM_MOOS_2019$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2019$HI_ymin < 0 & HI_FI_fDOM_MOOS_2019$HI_ymax > 0 & HI_FI_fDOM_MOOS_2019$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2019$Beta_index))
which(HI_FI_fDOM_MOOS_2019$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2019$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2019$Beta_index > 0)
which(HI_FI_fDOM_MOOS_2019$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2019$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2019$Beta_index < 0)

#SPC
HI_FI_SPC_MOOS_2019 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_SPC_MOOS_2019$Hyst_index))
which(HI_FI_SPC_MOOS_2019$HI_ymin < 0 & HI_FI_SPC_MOOS_2019$HI_ymax > 0 & HI_FI_SPC_MOOS_2019$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2019$HI_ymin < 0 & HI_FI_SPC_MOOS_2019$HI_ymax > 0 & HI_FI_SPC_MOOS_2019$Hyst_index < 0)

table(sign(HI_FI_SPC_MOOS_2019$Beta_index))
which(HI_FI_SPC_MOOS_2019$Beta_ymin < 0 & HI_FI_SPC_MOOS_2019$Beta_ymax > 0 & HI_FI_SPC_MOOS_2019$Beta_index > 0)
which(HI_FI_SPC_MOOS_2019$Beta_ymin < 0 & HI_FI_SPC_MOOS_2019$Beta_ymax > 0 & HI_FI_SPC_MOOS_2019$Beta_index < 0)

#turb
HI_FI_turb_MOOS_2019 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2019")

table(sign(HI_FI_turb_MOOS_2019$Hyst_index))
which(HI_FI_turb_MOOS_2019$HI_ymin < 0 & HI_FI_turb_MOOS_2019$HI_ymax > 0 & HI_FI_turb_MOOS_2019$Hyst_index > 0)
which(HI_FI_turb_MOOS_2019$HI_ymin < 0 & HI_FI_turb_MOOS_2019$HI_ymax > 0 & HI_FI_turb_MOOS_2019$Hyst_index < 0)

table(sign(HI_FI_turb_MOOS_2019$Beta_index))
which(HI_FI_turb_MOOS_2019$Beta_ymin < 0 & HI_FI_turb_MOOS_2019$Beta_ymax > 0 & HI_FI_turb_MOOS_2019$Beta_index > 0)
which(HI_FI_turb_MOOS_2019$Beta_ymin < 0 & HI_FI_turb_MOOS_2019$Beta_ymax > 0 & HI_FI_turb_MOOS_2019$Beta_index < 0)

# CARI ####
HI_FI_NO3_CARI_2019 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_NO3_CARI_2019$Hyst_index))
which(HI_FI_NO3_CARI_2019$HI_ymin < 0 & HI_FI_NO3_CARI_2019$HI_ymax > 0 & HI_FI_NO3_CARI_2019$Hyst_index > 0)
which(HI_FI_NO3_CARI_2019$HI_ymin < 0 & HI_FI_NO3_CARI_2019$HI_ymax > 0 & HI_FI_NO3_CARI_2019$Hyst_index < 0)

table(sign(HI_FI_NO3_CARI_2019$Beta_index))
which(HI_FI_NO3_CARI_2019$Beta_ymin < 0 & HI_FI_NO3_CARI_2019$Beta_ymax > 0 & HI_FI_NO3_CARI_2019$Beta_index > 0)
which(HI_FI_NO3_CARI_2019$Beta_ymin < 0 & HI_FI_NO3_CARI_2019$Beta_ymax > 0 & HI_FI_NO3_CARI_2019$Beta_index < 0)

#fDOM 
HI_FI_fDOM_CARI_2019 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_fDOM_CARI_2019$Hyst_index))
which(HI_FI_fDOM_CARI_2019$HI_ymin < 0 & HI_FI_fDOM_CARI_2019$HI_ymax > 0 & HI_FI_fDOM_CARI_2019$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2019$HI_ymin < 0 & HI_FI_fDOM_CARI_2019$HI_ymax > 0 & HI_FI_fDOM_CARI_2019$Hyst_index < 0)

table(sign(HI_FI_fDOM_CARI_2019$Beta_index))
which(HI_FI_fDOM_CARI_2019$Beta_ymin < 0 & HI_FI_fDOM_CARI_2019$Beta_ymax > 0 & HI_FI_fDOM_CARI_2019$Beta_index > 0)
which(HI_FI_fDOM_CARI_2019$Beta_ymin < 0 & HI_FI_fDOM_CARI_2019$Beta_ymax > 0 & HI_FI_fDOM_CARI_2019$Beta_index < 0)

#SPC
HI_FI_SPC_CARI_2019 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_SPC_CARI_2019$Hyst_index))
which(HI_FI_SPC_CARI_2019$HI_ymin < 0 & HI_FI_SPC_CARI_2019$HI_ymax > 0 & HI_FI_SPC_CARI_2019$Hyst_index > 0)
which(HI_FI_SPC_CARI_2019$HI_ymin < 0 & HI_FI_SPC_CARI_2019$HI_ymax > 0 & HI_FI_SPC_CARI_2019$Hyst_index < 0)

table(sign(HI_FI_SPC_CARI_2019$Beta_index))
which(HI_FI_SPC_CARI_2019$Beta_ymin < 0 & HI_FI_SPC_CARI_2019$Beta_ymax > 0 & HI_FI_SPC_CARI_2019$Beta_index > 0)
which(HI_FI_SPC_CARI_2019$Beta_ymin < 0 & HI_FI_SPC_CARI_2019$Beta_ymax > 0 & HI_FI_SPC_CARI_2019$Beta_index < 0)

#turb
HI_FI_turb_CARI_2019 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2019")

table(sign(HI_FI_turb_CARI_2019$Hyst_index))
which(HI_FI_turb_CARI_2019$HI_ymin < 0 & HI_FI_turb_CARI_2019$HI_ymax > 0 & HI_FI_turb_CARI_2019$Hyst_index > 0)
which(HI_FI_turb_CARI_2019$HI_ymin < 0 & HI_FI_turb_CARI_2019$HI_ymax > 0 & HI_FI_turb_CARI_2019$Hyst_index < 0)

table(sign(HI_FI_turb_CARI_2019$Beta_index))
which(HI_FI_turb_CARI_2019$Beta_ymin < 0 & HI_FI_turb_CARI_2019$Beta_ymax > 0 & HI_FI_turb_CARI_2019$Beta_index > 0)
which(HI_FI_turb_CARI_2019$Beta_ymin < 0 & HI_FI_turb_CARI_2019$Beta_ymax > 0 & HI_FI_turb_CARI_2019$Beta_index < 0)

# POKE ####
HI_FI_NO3_POKE_2019 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_NO3_POKE_2019$Hyst_index))
which(HI_FI_NO3_POKE_2019$HI_ymin < 0 & HI_FI_NO3_POKE_2019$HI_ymax > 0 & HI_FI_NO3_POKE_2019$Hyst_index > 0)
which(HI_FI_NO3_POKE_2019$HI_ymin < 0 & HI_FI_NO3_POKE_2019$HI_ymax > 0 & HI_FI_NO3_POKE_2019$Hyst_index < 0)

table(sign(HI_FI_NO3_POKE_2019$Beta_index))
which(HI_FI_NO3_POKE_2019$Beta_ymin < 0 & HI_FI_NO3_POKE_2019$Beta_ymax > 0 & HI_FI_NO3_POKE_2019$Beta_index > 0)
which(HI_FI_NO3_POKE_2019$Beta_ymin < 0 & HI_FI_NO3_POKE_2019$Beta_ymax > 0 & HI_FI_NO3_POKE_2019$Beta_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2019 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_fDOM_POKE_2019$Hyst_index))
which(HI_FI_fDOM_POKE_2019$HI_ymin < 0 & HI_FI_fDOM_POKE_2019$HI_ymax > 0 & HI_FI_fDOM_POKE_2019$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2019$HI_ymin < 0 & HI_FI_fDOM_POKE_2019$HI_ymax > 0 & HI_FI_fDOM_POKE_2019$Hyst_index < 0)

table(sign(HI_FI_fDOM_POKE_2019$Beta_index))
which(HI_FI_fDOM_POKE_2019$Beta_ymin < 0 & HI_FI_fDOM_POKE_2019$Beta_ymax > 0 & HI_FI_fDOM_POKE_2019$Beta_index > 0)
which(HI_FI_fDOM_POKE_2019$Beta_ymin < 0 & HI_FI_fDOM_POKE_2019$Beta_ymax > 0 & HI_FI_fDOM_POKE_2019$Beta_index < 0)

#SPC
HI_FI_SPC_POKE_2019 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_SPC_POKE_2019$Hyst_index))
which(HI_FI_SPC_POKE_2019$HI_ymin < 0 & HI_FI_SPC_POKE_2019$HI_ymax > 0 & HI_FI_SPC_POKE_2019$Hyst_index > 0)
which(HI_FI_SPC_POKE_2019$HI_ymin < 0 & HI_FI_SPC_POKE_2019$HI_ymax > 0 & HI_FI_SPC_POKE_2019$Hyst_index < 0)

table(sign(HI_FI_SPC_POKE_2019$Beta_index))
which(HI_FI_SPC_POKE_2019$Beta_ymin < 0 & HI_FI_SPC_POKE_2019$Beta_ymax > 0 & HI_FI_SPC_POKE_2019$Beta_index > 0)
which(HI_FI_SPC_POKE_2019$Beta_ymin < 0 & HI_FI_SPC_POKE_2019$Beta_ymax > 0 & HI_FI_SPC_POKE_2019$Beta_index < 0)

#turb
HI_FI_turb_POKE_2019 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2019")

table(sign(HI_FI_turb_POKE_2019$Hyst_index))
which(HI_FI_turb_POKE_2019$HI_ymin < 0 & HI_FI_turb_POKE_2019$HI_ymax > 0 & HI_FI_turb_POKE_2019$Hyst_index > 0)
which(HI_FI_turb_POKE_2019$HI_ymin < 0 & HI_FI_turb_POKE_2019$HI_ymax > 0 & HI_FI_turb_POKE_2019$Hyst_index < 0)

table(sign(HI_FI_turb_POKE_2019$Beta_index))
which(HI_FI_turb_POKE_2019$Beta_ymin < 0 & HI_FI_turb_POKE_2019$Beta_ymax > 0 & HI_FI_turb_POKE_2019$Beta_index > 0)
which(HI_FI_turb_POKE_2019$Beta_ymin < 0 & HI_FI_turb_POKE_2019$Beta_ymax > 0 & HI_FI_turb_POKE_2019$Beta_index < 0)

# STRT ####
HI_FI_NO3_STRT_2019 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_NO3_STRT_2019$Hyst_index))
which(HI_FI_NO3_STRT_2019$HI_ymin < 0 & HI_FI_NO3_STRT_2019$HI_ymax > 0 & HI_FI_NO3_STRT_2019$Hyst_index > 0)
which(HI_FI_NO3_STRT_2019$HI_ymin < 0 & HI_FI_NO3_STRT_2019$HI_ymax > 0 & HI_FI_NO3_STRT_2019$Hyst_index < 0)

table(sign(HI_FI_NO3_STRT_2019$Beta_index))
which(HI_FI_NO3_STRT_2019$Beta_ymin < 0 & HI_FI_NO3_STRT_2019$Beta_ymax > 0 & HI_FI_NO3_STRT_2019$Beta_index > 0)
which(HI_FI_NO3_STRT_2019$Beta_ymin < 0 & HI_FI_NO3_STRT_2019$Beta_ymax > 0 & HI_FI_NO3_STRT_2019$Beta_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2019 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_fDOM_STRT_2019$Hyst_index))
which(HI_FI_fDOM_STRT_2019$HI_ymin < 0 & HI_FI_fDOM_STRT_2019$HI_ymax > 0 & HI_FI_fDOM_STRT_2019$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2019$HI_ymin < 0 & HI_FI_fDOM_STRT_2019$HI_ymax > 0 & HI_FI_fDOM_STRT_2019$Hyst_index < 0)

table(sign(HI_FI_fDOM_STRT_2019$Beta_index))
which(HI_FI_fDOM_STRT_2019$Beta_ymin < 0 & HI_FI_fDOM_STRT_2019$Beta_ymax > 0 & HI_FI_fDOM_STRT_2019$Beta_index > 0)
which(HI_FI_fDOM_STRT_2019$Beta_ymin < 0 & HI_FI_fDOM_STRT_2019$Beta_ymax > 0 & HI_FI_fDOM_STRT_2019$Beta_index < 0)

#SPC
HI_FI_SPC_STRT_2019 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_SPC_STRT_2019$Hyst_index))
which(HI_FI_SPC_STRT_2019$HI_ymin < 0 & HI_FI_SPC_STRT_2019$HI_ymax > 0 & HI_FI_SPC_STRT_2019$Hyst_index > 0)
which(HI_FI_SPC_STRT_2019$HI_ymin < 0 & HI_FI_SPC_STRT_2019$HI_ymax > 0 & HI_FI_SPC_STRT_2019$Hyst_index < 0)

table(sign(HI_FI_SPC_STRT_2019$Beta_index))
which(HI_FI_SPC_STRT_2019$Beta_ymin < 0 & HI_FI_SPC_STRT_2019$Beta_ymax > 0 & HI_FI_SPC_STRT_2019$Beta_index > 0)
which(HI_FI_SPC_STRT_2019$Beta_ymin < 0 & HI_FI_SPC_STRT_2019$Beta_ymax > 0 & HI_FI_SPC_STRT_2019$Beta_index < 0)

#turb
HI_FI_turb_STRT_2019 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2019")

table(sign(HI_FI_turb_STRT_2019$Hyst_index))
which(HI_FI_turb_STRT_2019$HI_ymin < 0 & HI_FI_turb_STRT_2019$HI_ymax > 0 & HI_FI_turb_STRT_2019$Hyst_index > 0)
which(HI_FI_turb_STRT_2019$HI_ymin < 0 & HI_FI_turb_STRT_2019$HI_ymax > 0 & HI_FI_turb_STRT_2019$Hyst_index < 0)

table(sign(HI_FI_turb_STRT_2019$Beta_index))
which(HI_FI_turb_STRT_2019$Beta_ymin < 0 & HI_FI_turb_STRT_2019$Beta_ymax > 0 & HI_FI_turb_STRT_2019$Beta_index > 0)
which(HI_FI_turb_STRT_2019$Beta_ymin < 0 & HI_FI_turb_STRT_2019$Beta_ymax > 0 & HI_FI_turb_STRT_2019$Beta_index < 0)

# VAUL ####
HI_FI_NO3_VAUL_2019 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_NO3_VAUL_2019$Hyst_index))
which(HI_FI_NO3_VAUL_2019$HI_ymin < 0 & HI_FI_NO3_VAUL_2019$HI_ymax > 0 & HI_FI_NO3_VAUL_2019$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2019$HI_ymin < 0 & HI_FI_NO3_VAUL_2019$HI_ymax > 0 & HI_FI_NO3_VAUL_2019$Hyst_index < 0)

table(sign(HI_FI_NO3_VAUL_2019$Beta_index))
which(HI_FI_NO3_VAUL_2019$Beta_ymin < 0 & HI_FI_NO3_VAUL_2019$Beta_ymax > 0 & HI_FI_NO3_VAUL_2019$Beta_index > 0)
which(HI_FI_NO3_VAUL_2019$Beta_ymin < 0 & HI_FI_NO3_VAUL_2019$Beta_ymax > 0 & HI_FI_NO3_VAUL_2019$Beta_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2019 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_fDOM_VAUL_2019$Hyst_index))
which(HI_FI_fDOM_VAUL_2019$HI_ymin < 0 & HI_FI_fDOM_VAUL_2019$HI_ymax > 0 & HI_FI_fDOM_VAUL_2019$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2019$HI_ymin < 0 & HI_FI_fDOM_VAUL_2019$HI_ymax > 0 & HI_FI_fDOM_VAUL_2019$Hyst_index < 0)

table(sign(HI_FI_fDOM_VAUL_2019$Beta_index))
which(HI_FI_fDOM_VAUL_2019$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2019$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2019$Beta_index > 0)
which(HI_FI_fDOM_VAUL_2019$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2019$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2019$Beta_index < 0)

#SPC
HI_FI_SPC_VAUL_2019 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_SPC_VAUL_2019$Hyst_index))
which(HI_FI_SPC_VAUL_2019$HI_ymin < 0 & HI_FI_SPC_VAUL_2019$HI_ymax > 0 & HI_FI_SPC_VAUL_2019$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2019$HI_ymin < 0 & HI_FI_SPC_VAUL_2019$HI_ymax > 0 & HI_FI_SPC_VAUL_2019$Hyst_index < 0)

table(sign(HI_FI_SPC_VAUL_2019$Beta_index))
which(HI_FI_SPC_VAUL_2019$Beta_ymin < 0 & HI_FI_SPC_VAUL_2019$Beta_ymax > 0 & HI_FI_SPC_VAUL_2019$Beta_index > 0)
which(HI_FI_SPC_VAUL_2019$Beta_ymin < 0 & HI_FI_SPC_VAUL_2019$Beta_ymax > 0 & HI_FI_SPC_VAUL_2019$Beta_index < 0)

#turb
HI_FI_turb_VAUL_2019 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2019")

table(sign(HI_FI_turb_VAUL_2019$Hyst_index))
which(HI_FI_turb_VAUL_2019$HI_ymin < 0 & HI_FI_turb_VAUL_2019$HI_ymax > 0 & HI_FI_turb_VAUL_2019$Hyst_index > 0)
which(HI_FI_turb_VAUL_2019$HI_ymin < 0 & HI_FI_turb_VAUL_2019$HI_ymax > 0 & HI_FI_turb_VAUL_2019$Hyst_index < 0)

table(sign(HI_FI_turb_VAUL_2019$Beta_index))
which(HI_FI_turb_VAUL_2019$Beta_ymin < 0 & HI_FI_turb_VAUL_2019$Beta_ymax > 0 & HI_FI_turb_VAUL_2019$Beta_index > 0)
which(HI_FI_turb_VAUL_2019$Beta_ymin < 0 & HI_FI_turb_VAUL_2019$Beta_ymax > 0 & HI_FI_turb_VAUL_2019$Beta_index < 0)


# 2020 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2020 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_NO3_FRCH_2020$Hyst_index))
which(HI_FI_NO3_FRCH_2020$HI_ymin < 0 & HI_FI_NO3_FRCH_2020$HI_ymax > 0 & HI_FI_NO3_FRCH_2020$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2020$HI_ymin < 0 & HI_FI_NO3_FRCH_2020$HI_ymax > 0 & HI_FI_NO3_FRCH_2020$Hyst_index < 0)

table(sign(HI_FI_NO3_FRCH_2020$Beta_index))
which(HI_FI_NO3_FRCH_2020$Beta_ymin < 0 & HI_FI_NO3_FRCH_2020$Beta_ymax > 0 & HI_FI_NO3_FRCH_2020$Beta_index > 0)
which(HI_FI_NO3_FRCH_2020$Beta_ymin < 0 & HI_FI_NO3_FRCH_2020$Beta_ymax > 0 & HI_FI_NO3_FRCH_2020$Beta_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2020 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_fDOM_FRCH_2020$Hyst_index))
which(HI_FI_fDOM_FRCH_2020$HI_ymin < 0 & HI_FI_fDOM_FRCH_2020$HI_ymax > 0 & HI_FI_fDOM_FRCH_2020$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2020$HI_ymin < 0 & HI_FI_fDOM_FRCH_2020$HI_ymax > 0 & HI_FI_fDOM_FRCH_2020$Hyst_index < 0)

table(sign(HI_FI_fDOM_FRCH_2020$Beta_index))
which(HI_FI_fDOM_FRCH_2020$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2020$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2020$Beta_index > 0)
which(HI_FI_fDOM_FRCH_2020$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2020$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2020$Beta_index < 0)

#SPC
HI_FI_SPC_FRCH_2020 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_SPC_FRCH_2020$Hyst_index))
which(HI_FI_SPC_FRCH_2020$HI_ymin < 0 & HI_FI_SPC_FRCH_2020$HI_ymax > 0 & HI_FI_SPC_FRCH_2020$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2020$HI_ymin < 0 & HI_FI_SPC_FRCH_2020$HI_ymax > 0 & HI_FI_SPC_FRCH_2020$Hyst_index < 0)

table(sign(HI_FI_SPC_FRCH_2020$Beta_index))
which(HI_FI_SPC_FRCH_2020$Beta_ymin < 0 & HI_FI_SPC_FRCH_2020$Beta_ymax > 0 & HI_FI_SPC_FRCH_2020$Beta_index > 0)
which(HI_FI_SPC_FRCH_2020$Beta_ymin < 0 & HI_FI_SPC_FRCH_2020$Beta_ymax > 0 & HI_FI_SPC_FRCH_2020$Beta_index < 0)

#turb
HI_FI_turb_FRCH_2020 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2020")

table(sign(HI_FI_turb_FRCH_2020$Hyst_index))
which(HI_FI_turb_FRCH_2020$HI_ymin < 0 & HI_FI_turb_FRCH_2020$HI_ymax > 0 & HI_FI_turb_FRCH_2020$Hyst_index > 0)
which(HI_FI_turb_FRCH_2020$HI_ymin < 0 & HI_FI_turb_FRCH_2020$HI_ymax > 0 & HI_FI_turb_FRCH_2020$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2020$Beta_index))
which(HI_FI_turb_FRCH_2020$Beta_ymin < 0 & HI_FI_turb_FRCH_2020$Beta_ymax > 0 & HI_FI_turb_FRCH_2020$Beta_index > 0)
which(HI_FI_turb_FRCH_2020$Beta_ymin < 0 & HI_FI_turb_FRCH_2020$Beta_ymax > 0 & HI_FI_turb_FRCH_2020$Beta_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2020 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_NO3_MOOS_2020$Hyst_index))
which(HI_FI_NO3_MOOS_2020$HI_ymin < 0 & HI_FI_NO3_MOOS_2020$HI_ymax > 0 & HI_FI_NO3_MOOS_2020$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2020$HI_ymin < 0 & HI_FI_NO3_MOOS_2020$HI_ymax > 0 & HI_FI_NO3_MOOS_2020$Hyst_index < 0)

table(sign(HI_FI_NO3_MOOS_2020$Beta_index))
which(HI_FI_NO3_MOOS_2020$Beta_ymin < 0 & HI_FI_NO3_MOOS_2020$Beta_ymax > 0 & HI_FI_NO3_MOOS_2020$Beta_index > 0)
which(HI_FI_NO3_MOOS_2020$Beta_ymin < 0 & HI_FI_NO3_MOOS_2020$Beta_ymax > 0 & HI_FI_NO3_MOOS_2020$Beta_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2020 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_fDOM_MOOS_2020$Hyst_index))
which(HI_FI_fDOM_MOOS_2020$HI_ymin < 0 & HI_FI_fDOM_MOOS_2020$HI_ymax > 0 & HI_FI_fDOM_MOOS_2020$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2020$HI_ymin < 0 & HI_FI_fDOM_MOOS_2020$HI_ymax > 0 & HI_FI_fDOM_MOOS_2020$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2020$Beta_index))
which(HI_FI_fDOM_MOOS_2020$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2020$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2020$Beta_index > 0)
which(HI_FI_fDOM_MOOS_2020$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2020$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2020$Beta_index < 0)

#SPC
HI_FI_SPC_MOOS_2020 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_SPC_MOOS_2020$Hyst_index))
which(HI_FI_SPC_MOOS_2020$HI_ymin < 0 & HI_FI_SPC_MOOS_2020$HI_ymax > 0 & HI_FI_SPC_MOOS_2020$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2020$HI_ymin < 0 & HI_FI_SPC_MOOS_2020$HI_ymax > 0 & HI_FI_SPC_MOOS_2020$Hyst_index < 0)

table(sign(HI_FI_SPC_MOOS_2020$Beta_index))
which(HI_FI_SPC_MOOS_2020$Beta_ymin < 0 & HI_FI_SPC_MOOS_2020$Beta_ymax > 0 & HI_FI_SPC_MOOS_2020$Beta_index > 0)
which(HI_FI_SPC_MOOS_2020$Beta_ymin < 0 & HI_FI_SPC_MOOS_2020$Beta_ymax > 0 & HI_FI_SPC_MOOS_2020$Beta_index < 0)

#turb
HI_FI_turb_MOOS_2020 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2020")

table(sign(HI_FI_turb_MOOS_2020$Hyst_index))
which(HI_FI_turb_MOOS_2020$HI_ymin < 0 & HI_FI_turb_MOOS_2020$HI_ymax > 0 & HI_FI_turb_MOOS_2020$Hyst_index > 0)
which(HI_FI_turb_MOOS_2020$HI_ymin < 0 & HI_FI_turb_MOOS_2020$HI_ymax > 0 & HI_FI_turb_MOOS_2020$Hyst_index < 0)

table(sign(HI_FI_turb_MOOS_2020$Beta_index))
which(HI_FI_turb_MOOS_2020$Beta_ymin < 0 & HI_FI_turb_MOOS_2020$Beta_ymax > 0 & HI_FI_turb_MOOS_2020$Beta_index > 0)
which(HI_FI_turb_MOOS_2020$Beta_ymin < 0 & HI_FI_turb_MOOS_2020$Beta_ymax > 0 & HI_FI_turb_MOOS_2020$Beta_index < 0)

# CARI ####
HI_FI_NO3_CARI_2020 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_NO3_CARI_2020$Hyst_index))
which(HI_FI_NO3_CARI_2020$HI_ymin < 0 & HI_FI_NO3_CARI_2020$HI_ymax > 0 & HI_FI_NO3_CARI_2020$Hyst_index > 0)
which(HI_FI_NO3_CARI_2020$HI_ymin < 0 & HI_FI_NO3_CARI_2020$HI_ymax > 0 & HI_FI_NO3_CARI_2020$Hyst_index < 0)

table(sign(HI_FI_NO3_CARI_2020$Beta_index))
which(HI_FI_NO3_CARI_2020$Beta_ymin < 0 & HI_FI_NO3_CARI_2020$Beta_ymax > 0 & HI_FI_NO3_CARI_2020$Beta_index > 0)
which(HI_FI_NO3_CARI_2020$Beta_ymin < 0 & HI_FI_NO3_CARI_2020$Beta_ymax > 0 & HI_FI_NO3_CARI_2020$Beta_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2020 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_fDOM_CARI_2020$Hyst_index))
which(HI_FI_fDOM_CARI_2020$HI_ymin < 0 & HI_FI_fDOM_CARI_2020$HI_ymax > 0 & HI_FI_fDOM_CARI_2020$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2020$HI_ymin < 0 & HI_FI_fDOM_CARI_2020$HI_ymax > 0 & HI_FI_fDOM_CARI_2020$Hyst_index < 0)

table(sign(HI_FI_fDOM_CARI_2020$Beta_index))
which(HI_FI_fDOM_CARI_2020$Beta_ymin < 0 & HI_FI_fDOM_CARI_2020$Beta_ymax > 0 & HI_FI_fDOM_CARI_2020$Beta_index > 0)
which(HI_FI_fDOM_CARI_2020$Beta_ymin < 0 & HI_FI_fDOM_CARI_2020$Beta_ymax > 0 & HI_FI_fDOM_CARI_2020$Beta_index < 0)

#SPC
HI_FI_SPC_CARI_2020 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_SPC_CARI_2020$Hyst_index))
which(HI_FI_SPC_CARI_2020$HI_ymin < 0 & HI_FI_SPC_CARI_2020$HI_ymax > 0 & HI_FI_SPC_CARI_2020$Hyst_index > 0)
which(HI_FI_SPC_CARI_2020$HI_ymin < 0 & HI_FI_SPC_CARI_2020$HI_ymax > 0 & HI_FI_SPC_CARI_2020$Hyst_index < 0)

table(sign(HI_FI_SPC_CARI_2020$Beta_index))
which(HI_FI_SPC_CARI_2020$Beta_ymin < 0 & HI_FI_SPC_CARI_2020$Beta_ymax > 0 & HI_FI_SPC_CARI_2020$Beta_index > 0)
which(HI_FI_SPC_CARI_2020$Beta_ymin < 0 & HI_FI_SPC_CARI_2020$Beta_ymax > 0 & HI_FI_SPC_CARI_2020$Beta_index < 0)

#turb
HI_FI_turb_CARI_2020 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2020")

table(sign(HI_FI_turb_CARI_2020$Hyst_index))
which(HI_FI_turb_CARI_2020$HI_ymin < 0 & HI_FI_turb_CARI_2020$HI_ymax > 0 & HI_FI_turb_CARI_2020$Hyst_index > 0)
which(HI_FI_turb_CARI_2020$HI_ymin < 0 & HI_FI_turb_CARI_2020$HI_ymax > 0 & HI_FI_turb_CARI_2020$Hyst_index < 0)

table(sign(HI_FI_turb_CARI_2020$Beta_index))
which(HI_FI_turb_CARI_2020$Beta_ymin < 0 & HI_FI_turb_CARI_2020$Beta_ymax > 0 & HI_FI_turb_CARI_2020$Beta_index > 0)
which(HI_FI_turb_CARI_2020$Beta_ymin < 0 & HI_FI_turb_CARI_2020$Beta_ymax > 0 & HI_FI_turb_CARI_2020$Beta_index < 0)

# POKE ####
HI_FI_NO3_POKE_2020 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_NO3_POKE_2020$Hyst_index))
which(HI_FI_NO3_POKE_2020$HI_ymin < 0 & HI_FI_NO3_POKE_2020$HI_ymax > 0 & HI_FI_NO3_POKE_2020$Hyst_index > 0)
which(HI_FI_NO3_POKE_2020$HI_ymin < 0 & HI_FI_NO3_POKE_2020$HI_ymax > 0 & HI_FI_NO3_POKE_2020$Hyst_index < 0)

table(sign(HI_FI_NO3_POKE_2020$Beta_index))
which(HI_FI_NO3_POKE_2020$Beta_ymin < 0 & HI_FI_NO3_POKE_2020$Beta_ymax > 0 & HI_FI_NO3_POKE_2020$Beta_index > 0)
which(HI_FI_NO3_POKE_2020$Beta_ymin < 0 & HI_FI_NO3_POKE_2020$Beta_ymax > 0 & HI_FI_NO3_POKE_2020$Beta_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2020 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_fDOM_POKE_2020$Hyst_index))
which(HI_FI_fDOM_POKE_2020$HI_ymin < 0 & HI_FI_fDOM_POKE_2020$HI_ymax > 0 & HI_FI_fDOM_POKE_2020$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2020$HI_ymin < 0 & HI_FI_fDOM_POKE_2020$HI_ymax > 0 & HI_FI_fDOM_POKE_2020$Hyst_index < 0)

table(sign(HI_FI_fDOM_POKE_2020$Beta_index))
which(HI_FI_fDOM_POKE_2020$Beta_ymin < 0 & HI_FI_fDOM_POKE_2020$Beta_ymax > 0 & HI_FI_fDOM_POKE_2020$Beta_index > 0)
which(HI_FI_fDOM_POKE_2020$Beta_ymin < 0 & HI_FI_fDOM_POKE_2020$Beta_ymax > 0 & HI_FI_fDOM_POKE_2020$Beta_index < 0)

#SPC
HI_FI_SPC_POKE_2020 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_SPC_POKE_2020$Hyst_index))
which(HI_FI_SPC_POKE_2020$HI_ymin < 0 & HI_FI_SPC_POKE_2020$HI_ymax > 0 & HI_FI_SPC_POKE_2020$Hyst_index > 0)
which(HI_FI_SPC_POKE_2020$HI_ymin < 0 & HI_FI_SPC_POKE_2020$HI_ymax > 0 & HI_FI_SPC_POKE_2020$Hyst_index < 0)

table(sign(HI_FI_SPC_POKE_2020$Beta_index))
which(HI_FI_SPC_POKE_2020$Beta_ymin < 0 & HI_FI_SPC_POKE_2020$Beta_ymax > 0 & HI_FI_SPC_POKE_2020$Beta_index > 0)
which(HI_FI_SPC_POKE_2020$Beta_ymin < 0 & HI_FI_SPC_POKE_2020$Beta_ymax > 0 & HI_FI_SPC_POKE_2020$Beta_index < 0)

#turb
HI_FI_turb_POKE_2020 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2020")

table(sign(HI_FI_turb_POKE_2020$Hyst_index))
which(HI_FI_turb_POKE_2020$HI_ymin < 0 & HI_FI_turb_POKE_2020$HI_ymax > 0 & HI_FI_turb_POKE_2020$Hyst_index > 0)
which(HI_FI_turb_POKE_2020$HI_ymin < 0 & HI_FI_turb_POKE_2020$HI_ymax > 0 & HI_FI_turb_POKE_2020$Hyst_index < 0)

table(sign(HI_FI_turb_POKE_2020$Beta_index))
which(HI_FI_turb_POKE_2020$Beta_ymin < 0 & HI_FI_turb_POKE_2020$Beta_ymax > 0 & HI_FI_turb_POKE_2020$Beta_index > 0)
which(HI_FI_turb_POKE_2020$Beta_ymin < 0 & HI_FI_turb_POKE_2020$Beta_ymax > 0 & HI_FI_turb_POKE_2020$Beta_index < 0)

# STRT ####
HI_FI_NO3_STRT_2020 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_NO3_STRT_2020$Hyst_index))
which(HI_FI_NO3_STRT_2020$HI_ymin < 0 & HI_FI_NO3_STRT_2020$HI_ymax > 0 & HI_FI_NO3_STRT_2020$Hyst_index > 0)
which(HI_FI_NO3_STRT_2020$HI_ymin < 0 & HI_FI_NO3_STRT_2020$HI_ymax > 0 & HI_FI_NO3_STRT_2020$Hyst_index < 0)

table(sign(HI_FI_NO3_STRT_2020$Beta_index))
which(HI_FI_NO3_STRT_2020$Beta_ymin < 0 & HI_FI_NO3_STRT_2020$Beta_ymax > 0 & HI_FI_NO3_STRT_2020$Beta_index > 0)
which(HI_FI_NO3_STRT_2020$Beta_ymin < 0 & HI_FI_NO3_STRT_2020$Beta_ymax > 0 & HI_FI_NO3_STRT_2020$Beta_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2020 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_fDOM_STRT_2020$Hyst_index))
which(HI_FI_fDOM_STRT_2020$HI_ymin < 0 & HI_FI_fDOM_STRT_2020$HI_ymax > 0 & HI_FI_fDOM_STRT_2020$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2020$HI_ymin < 0 & HI_FI_fDOM_STRT_2020$HI_ymax > 0 & HI_FI_fDOM_STRT_2020$Hyst_index < 0)

table(sign(HI_FI_fDOM_STRT_2020$Beta_index))
which(HI_FI_fDOM_STRT_2020$Beta_ymin < 0 & HI_FI_fDOM_STRT_2020$Beta_ymax > 0 & HI_FI_fDOM_STRT_2020$Beta_index > 0)
which(HI_FI_fDOM_STRT_2020$Beta_ymin < 0 & HI_FI_fDOM_STRT_2020$Beta_ymax > 0 & HI_FI_fDOM_STRT_2020$Beta_index < 0)

#SPC
HI_FI_SPC_STRT_2020 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_SPC_STRT_2020$Hyst_index))
which(HI_FI_SPC_STRT_2020$HI_ymin < 0 & HI_FI_SPC_STRT_2020$HI_ymax > 0 & HI_FI_SPC_STRT_2020$Hyst_index > 0)
which(HI_FI_SPC_STRT_2020$HI_ymin < 0 & HI_FI_SPC_STRT_2020$HI_ymax > 0 & HI_FI_SPC_STRT_2020$Hyst_index < 0)

table(sign(HI_FI_SPC_STRT_2020$Beta_index))
which(HI_FI_SPC_STRT_2020$Beta_ymin < 0 & HI_FI_SPC_STRT_2020$Beta_ymax > 0 & HI_FI_SPC_STRT_2020$Beta_index > 0)
which(HI_FI_SPC_STRT_2020$Beta_ymin < 0 & HI_FI_SPC_STRT_2020$Beta_ymax > 0 & HI_FI_SPC_STRT_2020$Beta_index < 0)

#turb
HI_FI_turb_STRT_2020 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2020")

table(sign(HI_FI_turb_STRT_2020$Hyst_index))
which(HI_FI_turb_STRT_2020$HI_ymin < 0 & HI_FI_turb_STRT_2020$HI_ymax > 0 & HI_FI_turb_STRT_2020$Hyst_index > 0)
which(HI_FI_turb_STRT_2020$HI_ymin < 0 & HI_FI_turb_STRT_2020$HI_ymax > 0 & HI_FI_turb_STRT_2020$Hyst_index < 0)

table(sign(HI_FI_turb_STRT_2020$Beta_index))
which(HI_FI_turb_STRT_2020$Beta_ymin < 0 & HI_FI_turb_STRT_2020$Beta_ymax > 0 & HI_FI_turb_STRT_2020$Beta_index > 0)
which(HI_FI_turb_STRT_2020$Beta_ymin < 0 & HI_FI_turb_STRT_2020$Beta_ymax > 0 & HI_FI_turb_STRT_2020$Beta_index < 0)


# VAUL ####
HI_FI_NO3_VAUL_2020 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_NO3_VAUL_2020$Hyst_index))
which(HI_FI_NO3_VAUL_2020$HI_ymin < 0 & HI_FI_NO3_VAUL_2020$HI_ymax > 0 & HI_FI_NO3_VAUL_2020$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2020$HI_ymin < 0 & HI_FI_NO3_VAUL_2020$HI_ymax > 0 & HI_FI_NO3_VAUL_2020$Hyst_index < 0)

table(sign(HI_FI_NO3_VAUL_2020$Beta_index))
which(HI_FI_NO3_VAUL_2020$Beta_ymin < 0 & HI_FI_NO3_VAUL_2020$Beta_ymax > 0 & HI_FI_NO3_VAUL_2020$Beta_index > 0)
which(HI_FI_NO3_VAUL_2020$Beta_ymin < 0 & HI_FI_NO3_VAUL_2020$Beta_ymax > 0 & HI_FI_NO3_VAUL_2020$Beta_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2020 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_fDOM_VAUL_2020$Hyst_index))
which(HI_FI_fDOM_VAUL_2020$HI_ymin < 0 & HI_FI_fDOM_VAUL_2020$HI_ymax > 0 & HI_FI_fDOM_VAUL_2020$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2020$HI_ymin < 0 & HI_FI_fDOM_VAUL_2020$HI_ymax > 0 & HI_FI_fDOM_VAUL_2020$Hyst_index < 0)

table(sign(HI_FI_fDOM_VAUL_2020$Beta_index))
which(HI_FI_fDOM_VAUL_2020$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2020$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2020$Beta_index > 0)
which(HI_FI_fDOM_VAUL_2020$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2020$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2020$Beta_index < 0)

#SPC
HI_FI_SPC_VAUL_2020 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_SPC_VAUL_2020$Hyst_index))
which(HI_FI_SPC_VAUL_2020$HI_ymin < 0 & HI_FI_SPC_VAUL_2020$HI_ymax > 0 & HI_FI_SPC_VAUL_2020$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2020$HI_ymin < 0 & HI_FI_SPC_VAUL_2020$HI_ymax > 0 & HI_FI_SPC_VAUL_2020$Hyst_index < 0)

table(sign(HI_FI_SPC_VAUL_2020$Beta_index))
which(HI_FI_SPC_VAUL_2020$Beta_ymin < 0 & HI_FI_SPC_VAUL_2020$Beta_ymax > 0 & HI_FI_SPC_VAUL_2020$Beta_index > 0)
which(HI_FI_SPC_VAUL_2020$Beta_ymin < 0 & HI_FI_SPC_VAUL_2020$Beta_ymax > 0 & HI_FI_SPC_VAUL_2020$Beta_index < 0)

#turb
HI_FI_turb_VAUL_2020 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2020")

table(sign(HI_FI_turb_VAUL_2020$Hyst_index))
which(HI_FI_turb_VAUL_2020$HI_ymin < 0 & HI_FI_turb_VAUL_2020$HI_ymax > 0 & HI_FI_turb_VAUL_2020$Hyst_index > 0)
which(HI_FI_turb_VAUL_2020$HI_ymin < 0 & HI_FI_turb_VAUL_2020$HI_ymax > 0 & HI_FI_turb_VAUL_2020$Hyst_index < 0)

table(sign(HI_FI_turb_VAUL_2020$Beta_index))
which(HI_FI_turb_VAUL_2020$Beta_ymin < 0 & HI_FI_turb_VAUL_2020$Beta_ymax > 0 & HI_FI_turb_VAUL_2020$Beta_index > 0)
which(HI_FI_turb_VAUL_2020$Beta_ymin < 0 & HI_FI_turb_VAUL_2020$Beta_ymax > 0 & HI_FI_turb_VAUL_2020$Beta_index < 0)


# 2021 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2021 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_NO3_FRCH_2021$Hyst_index))
which(HI_FI_NO3_FRCH_2021$HI_ymin < 0 & HI_FI_NO3_FRCH_2021$HI_ymax > 0 & HI_FI_NO3_FRCH_2021$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2021$HI_ymin < 0 & HI_FI_NO3_FRCH_2021$HI_ymax > 0 & HI_FI_NO3_FRCH_2021$Hyst_index < 0)

table(sign(HI_FI_NO3_FRCH_2021$Beta_index))
which(HI_FI_NO3_FRCH_2021$Beta_ymin < 0 & HI_FI_NO3_FRCH_2021$Beta_ymax > 0 & HI_FI_NO3_FRCH_2021$Beta_index > 0)
which(HI_FI_NO3_FRCH_2021$Beta_ymin < 0 & HI_FI_NO3_FRCH_2021$Beta_ymax > 0 & HI_FI_NO3_FRCH_2021$Beta_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2021 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_fDOM_FRCH_2021$Hyst_index))
which(HI_FI_fDOM_FRCH_2021$HI_ymin < 0 & HI_FI_fDOM_FRCH_2021$HI_ymax > 0 & HI_FI_fDOM_FRCH_2021$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2021$HI_ymin < 0 & HI_FI_fDOM_FRCH_2021$HI_ymax > 0 & HI_FI_fDOM_FRCH_2021$Hyst_index < 0)

table(sign(HI_FI_fDOM_FRCH_2021$Beta_index))
which(HI_FI_fDOM_FRCH_2021$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2021$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2021$Beta_index > 0)
which(HI_FI_fDOM_FRCH_2021$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2021$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2021$Beta_index < 0)

#SPC
HI_FI_SPC_FRCH_2021 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_SPC_FRCH_2021$Hyst_index))
which(HI_FI_SPC_FRCH_2021$HI_ymin < 0 & HI_FI_SPC_FRCH_2021$HI_ymax > 0 & HI_FI_SPC_FRCH_2021$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2021$HI_ymin < 0 & HI_FI_SPC_FRCH_2021$HI_ymax > 0 & HI_FI_SPC_FRCH_2021$Hyst_index < 0)

table(sign(HI_FI_SPC_FRCH_2021$Beta_index))
which(HI_FI_SPC_FRCH_2021$Beta_ymin < 0 & HI_FI_SPC_FRCH_2021$Beta_ymax > 0 & HI_FI_SPC_FRCH_2021$Beta_index > 0)
which(HI_FI_SPC_FRCH_2021$Beta_ymin < 0 & HI_FI_SPC_FRCH_2021$Beta_ymax > 0 & HI_FI_SPC_FRCH_2021$Beta_index < 0)

#turb
HI_FI_turb_FRCH_2021 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2021")

table(sign(HI_FI_turb_FRCH_2021$Hyst_index))
which(HI_FI_turb_FRCH_2021$HI_ymin < 0 & HI_FI_turb_FRCH_2021$HI_ymax > 0 & HI_FI_turb_FRCH_2021$Hyst_index > 0)
which(HI_FI_turb_FRCH_2021$HI_ymin < 0 & HI_FI_turb_FRCH_2021$HI_ymax > 0 & HI_FI_turb_FRCH_2021$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2021$Beta_index))
which(HI_FI_turb_FRCH_2021$Beta_ymin < 0 & HI_FI_turb_FRCH_2021$Beta_ymax > 0 & HI_FI_turb_FRCH_2021$Beta_index > 0)
which(HI_FI_turb_FRCH_2021$Beta_ymin < 0 & HI_FI_turb_FRCH_2021$Beta_ymax > 0 & HI_FI_turb_FRCH_2021$Beta_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2021 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_NO3_MOOS_2021$Hyst_index))
which(HI_FI_NO3_MOOS_2021$HI_ymin < 0 & HI_FI_NO3_MOOS_2021$HI_ymax > 0 & HI_FI_NO3_MOOS_2021$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2021$HI_ymin < 0 & HI_FI_NO3_MOOS_2021$HI_ymax > 0 & HI_FI_NO3_MOOS_2021$Hyst_index < 0)

table(sign(HI_FI_NO3_MOOS_2021$Beta_index))
which(HI_FI_NO3_MOOS_2021$Beta_ymin < 0 & HI_FI_NO3_MOOS_2021$Beta_ymax > 0 & HI_FI_NO3_MOOS_2021$Beta_index > 0)
which(HI_FI_NO3_MOOS_2021$Beta_ymin < 0 & HI_FI_NO3_MOOS_2021$Beta_ymax > 0 & HI_FI_NO3_MOOS_2021$Beta_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2021 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_fDOM_MOOS_2021$Hyst_index))
which(HI_FI_fDOM_MOOS_2021$HI_ymin < 0 & HI_FI_fDOM_MOOS_2021$HI_ymax > 0 & HI_FI_fDOM_MOOS_2021$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2021$HI_ymin < 0 & HI_FI_fDOM_MOOS_2021$HI_ymax > 0 & HI_FI_fDOM_MOOS_2021$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2021$Beta_index))
which(HI_FI_fDOM_MOOS_2021$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2021$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2021$Beta_index > 0)
which(HI_FI_fDOM_MOOS_2021$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2021$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2021$Beta_index < 0)

#SPC
HI_FI_SPC_MOOS_2021 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_SPC_MOOS_2021$Hyst_index))
which(HI_FI_SPC_MOOS_2021$HI_ymin < 0 & HI_FI_SPC_MOOS_2021$HI_ymax > 0 & HI_FI_SPC_MOOS_2021$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2021$HI_ymin < 0 & HI_FI_SPC_MOOS_2021$HI_ymax > 0 & HI_FI_SPC_MOOS_2021$Hyst_index < 0)

table(sign(HI_FI_SPC_MOOS_2021$Beta_index))
which(HI_FI_SPC_MOOS_2021$Beta_ymin < 0 & HI_FI_SPC_MOOS_2021$Beta_ymax > 0 & HI_FI_SPC_MOOS_2021$Beta_index > 0)
which(HI_FI_SPC_MOOS_2021$Beta_ymin < 0 & HI_FI_SPC_MOOS_2021$Beta_ymax > 0 & HI_FI_SPC_MOOS_2021$Beta_index < 0)

#turb
HI_FI_turb_MOOS_2021 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2021")

table(sign(HI_FI_turb_MOOS_2021$Hyst_index))
which(HI_FI_turb_MOOS_2021$HI_ymin < 0 & HI_FI_turb_MOOS_2021$HI_ymax > 0 & HI_FI_turb_MOOS_2021$Hyst_index > 0)
which(HI_FI_turb_MOOS_2021$HI_ymin < 0 & HI_FI_turb_MOOS_2021$HI_ymax > 0 & HI_FI_turb_MOOS_2021$Hyst_index < 0)

table(sign(HI_FI_turb_MOOS_2021$Beta_index))
which(HI_FI_turb_MOOS_2021$Beta_ymin < 0 & HI_FI_turb_MOOS_2021$Beta_ymax > 0 & HI_FI_turb_MOOS_2021$Beta_index > 0)
which(HI_FI_turb_MOOS_2021$Beta_ymin < 0 & HI_FI_turb_MOOS_2021$Beta_ymax > 0 & HI_FI_turb_MOOS_2021$Beta_index < 0)

# CARI ####
HI_FI_NO3_CARI_2021 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_NO3_CARI_2021$Hyst_index))
which(HI_FI_NO3_CARI_2021$HI_ymin < 0 & HI_FI_NO3_CARI_2021$HI_ymax > 0 & HI_FI_NO3_CARI_2021$Hyst_index > 0)
which(HI_FI_NO3_CARI_2021$HI_ymin < 0 & HI_FI_NO3_CARI_2021$HI_ymax > 0 & HI_FI_NO3_CARI_2021$Hyst_index < 0)

table(sign(HI_FI_NO3_CARI_2021$Beta_index))
which(HI_FI_NO3_CARI_2021$Beta_ymin < 0 & HI_FI_NO3_CARI_2021$Beta_ymax > 0 & HI_FI_NO3_CARI_2021$Beta_index > 0)
which(HI_FI_NO3_CARI_2021$Beta_ymin < 0 & HI_FI_NO3_CARI_2021$Beta_ymax > 0 & HI_FI_NO3_CARI_2021$Beta_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2021 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_fDOM_CARI_2021$Hyst_index))
which(HI_FI_fDOM_CARI_2021$HI_ymin < 0 & HI_FI_fDOM_CARI_2021$HI_ymax > 0 & HI_FI_fDOM_CARI_2021$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2021$HI_ymin < 0 & HI_FI_fDOM_CARI_2021$HI_ymax > 0 & HI_FI_fDOM_CARI_2021$Hyst_index < 0)

table(sign(HI_FI_fDOM_CARI_2021$Beta_index))
which(HI_FI_fDOM_CARI_2021$Beta_ymin < 0 & HI_FI_fDOM_CARI_2021$Beta_ymax > 0 & HI_FI_fDOM_CARI_2021$Beta_index > 0)
which(HI_FI_fDOM_CARI_2021$Beta_ymin < 0 & HI_FI_fDOM_CARI_2021$Beta_ymax > 0 & HI_FI_fDOM_CARI_2021$Beta_index < 0)

#SPC
HI_FI_SPC_CARI_2021 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_SPC_CARI_2021$Hyst_index))
which(HI_FI_SPC_CARI_2021$HI_ymin < 0 & HI_FI_SPC_CARI_2021$HI_ymax > 0 & HI_FI_SPC_CARI_2021$Hyst_index > 0)
which(HI_FI_SPC_CARI_2021$HI_ymin < 0 & HI_FI_SPC_CARI_2021$HI_ymax > 0 & HI_FI_SPC_CARI_2021$Hyst_index < 0)

table(sign(HI_FI_SPC_CARI_2021$Beta_index))
which(HI_FI_SPC_CARI_2021$Beta_ymin < 0 & HI_FI_SPC_CARI_2021$Beta_ymax > 0 & HI_FI_SPC_CARI_2021$Beta_index > 0)
which(HI_FI_SPC_CARI_2021$Beta_ymin < 0 & HI_FI_SPC_CARI_2021$Beta_ymax > 0 & HI_FI_SPC_CARI_2021$Beta_index < 0)

#turb
HI_FI_turb_CARI_2021 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2021")

table(sign(HI_FI_turb_CARI_2021$Hyst_index))
which(HI_FI_turb_CARI_2021$HI_ymin < 0 & HI_FI_turb_CARI_2021$HI_ymax > 0 & HI_FI_turb_CARI_2021$Hyst_index > 0)
which(HI_FI_turb_CARI_2021$HI_ymin < 0 & HI_FI_turb_CARI_2021$HI_ymax > 0 & HI_FI_turb_CARI_2021$Hyst_index < 0)

table(sign(HI_FI_turb_CARI_2021$Beta_index))
which(HI_FI_turb_CARI_2021$Beta_ymin < 0 & HI_FI_turb_CARI_2021$Beta_ymax > 0 & HI_FI_turb_CARI_2021$Beta_index > 0)
which(HI_FI_turb_CARI_2021$Beta_ymin < 0 & HI_FI_turb_CARI_2021$Beta_ymax > 0 & HI_FI_turb_CARI_2021$Beta_index < 0)

# POKE ####
HI_FI_NO3_POKE_2021 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_NO3_POKE_2021$Hyst_index))
which(HI_FI_NO3_POKE_2021$HI_ymin < 0 & HI_FI_NO3_POKE_2021$HI_ymax > 0 & HI_FI_NO3_POKE_2021$Hyst_index > 0)
which(HI_FI_NO3_POKE_2021$HI_ymin < 0 & HI_FI_NO3_POKE_2021$HI_ymax > 0 & HI_FI_NO3_POKE_2021$Hyst_index < 0)

table(sign(HI_FI_NO3_POKE_2021$Beta_index))
which(HI_FI_NO3_POKE_2021$Beta_ymin < 0 & HI_FI_NO3_POKE_2021$Beta_ymax > 0 & HI_FI_NO3_POKE_2021$Beta_index > 0)
which(HI_FI_NO3_POKE_2021$Beta_ymin < 0 & HI_FI_NO3_POKE_2021$Beta_ymax > 0 & HI_FI_NO3_POKE_2021$Beta_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2021 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_fDOM_POKE_2021$Hyst_index))
which(HI_FI_fDOM_POKE_2021$HI_ymin < 0 & HI_FI_fDOM_POKE_2021$HI_ymax > 0 & HI_FI_fDOM_POKE_2021$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2021$HI_ymin < 0 & HI_FI_fDOM_POKE_2021$HI_ymax > 0 & HI_FI_fDOM_POKE_2021$Hyst_index < 0)

table(sign(HI_FI_fDOM_POKE_2021$Beta_index))
which(HI_FI_fDOM_POKE_2021$Beta_ymin < 0 & HI_FI_fDOM_POKE_2021$Beta_ymax > 0 & HI_FI_fDOM_POKE_2021$Beta_index > 0)
which(HI_FI_fDOM_POKE_2021$Beta_ymin < 0 & HI_FI_fDOM_POKE_2021$Beta_ymax > 0 & HI_FI_fDOM_POKE_2021$Beta_index < 0)

#SPC
HI_FI_SPC_POKE_2021 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_SPC_POKE_2021$Hyst_index))
which(HI_FI_SPC_POKE_2021$HI_ymin < 0 & HI_FI_SPC_POKE_2021$HI_ymax > 0 & HI_FI_SPC_POKE_2021$Hyst_index > 0)
which(HI_FI_SPC_POKE_2021$HI_ymin < 0 & HI_FI_SPC_POKE_2021$HI_ymax > 0 & HI_FI_SPC_POKE_2021$Hyst_index < 0)

table(sign(HI_FI_SPC_POKE_2021$Beta_index))
which(HI_FI_SPC_POKE_2021$Beta_ymin < 0 & HI_FI_SPC_POKE_2021$Beta_ymax > 0 & HI_FI_SPC_POKE_2021$Beta_index > 0)
which(HI_FI_SPC_POKE_2021$Beta_ymin < 0 & HI_FI_SPC_POKE_2021$Beta_ymax > 0 & HI_FI_SPC_POKE_2021$Beta_index < 0)

#turb
HI_FI_turb_POKE_2021 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2021")

table(sign(HI_FI_turb_POKE_2021$Hyst_index))
which(HI_FI_turb_POKE_2021$HI_ymin < 0 & HI_FI_turb_POKE_2021$HI_ymax > 0 & HI_FI_turb_POKE_2021$Hyst_index > 0)
which(HI_FI_turb_POKE_2021$HI_ymin < 0 & HI_FI_turb_POKE_2021$HI_ymax > 0 & HI_FI_turb_POKE_2021$Hyst_index < 0)

table(sign(HI_FI_turb_POKE_2021$Beta_index))
which(HI_FI_turb_POKE_2021$Beta_ymin < 0 & HI_FI_turb_POKE_2021$Beta_ymax > 0 & HI_FI_turb_POKE_2021$Beta_index > 0)
which(HI_FI_turb_POKE_2021$Beta_ymin < 0 & HI_FI_turb_POKE_2021$Beta_ymax > 0 & HI_FI_turb_POKE_2021$Beta_index < 0)

# STRT ####
HI_FI_NO3_STRT_2021 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_NO3_STRT_2021$Hyst_index))
which(HI_FI_NO3_STRT_2021$HI_ymin < 0 & HI_FI_NO3_STRT_2021$HI_ymax > 0 & HI_FI_NO3_STRT_2021$Hyst_index > 0)
which(HI_FI_NO3_STRT_2021$HI_ymin < 0 & HI_FI_NO3_STRT_2021$HI_ymax > 0 & HI_FI_NO3_STRT_2021$Hyst_index < 0)

table(sign(HI_FI_NO3_STRT_2021$Beta_index))
which(HI_FI_NO3_STRT_2021$Beta_ymin < 0 & HI_FI_NO3_STRT_2021$Beta_ymax > 0 & HI_FI_NO3_STRT_2021$Beta_index > 0)
which(HI_FI_NO3_STRT_2021$Beta_ymin < 0 & HI_FI_NO3_STRT_2021$Beta_ymax > 0 & HI_FI_NO3_STRT_2021$Beta_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2021 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_fDOM_STRT_2021$Hyst_index))
which(HI_FI_fDOM_STRT_2021$HI_ymin < 0 & HI_FI_fDOM_STRT_2021$HI_ymax > 0 & HI_FI_fDOM_STRT_2021$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2021$HI_ymin < 0 & HI_FI_fDOM_STRT_2021$HI_ymax > 0 & HI_FI_fDOM_STRT_2021$Hyst_index < 0)

table(sign(HI_FI_fDOM_STRT_2021$Beta_index))
which(HI_FI_fDOM_STRT_2021$Beta_ymin < 0 & HI_FI_fDOM_STRT_2021$Beta_ymax > 0 & HI_FI_fDOM_STRT_2021$Beta_index > 0)
which(HI_FI_fDOM_STRT_2021$Beta_ymin < 0 & HI_FI_fDOM_STRT_2021$Beta_ymax > 0 & HI_FI_fDOM_STRT_2021$Beta_index < 0)

#SPC
HI_FI_SPC_STRT_2021 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_SPC_STRT_2021$Hyst_index))
which(HI_FI_SPC_STRT_2021$HI_ymin < 0 & HI_FI_SPC_STRT_2021$HI_ymax > 0 & HI_FI_SPC_STRT_2021$Hyst_index > 0)
which(HI_FI_SPC_STRT_2021$HI_ymin < 0 & HI_FI_SPC_STRT_2021$HI_ymax > 0 & HI_FI_SPC_STRT_2021$Hyst_index < 0)

table(sign(HI_FI_SPC_STRT_2021$Beta_index))
which(HI_FI_SPC_STRT_2021$Beta_ymin < 0 & HI_FI_SPC_STRT_2021$Beta_ymax > 0 & HI_FI_SPC_STRT_2021$Beta_index > 0)
which(HI_FI_SPC_STRT_2021$Beta_ymin < 0 & HI_FI_SPC_STRT_2021$Beta_ymax > 0 & HI_FI_SPC_STRT_2021$Beta_index < 0)

#turb
HI_FI_turb_STRT_2021 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2021")

table(sign(HI_FI_turb_STRT_2021$Hyst_index))
which(HI_FI_turb_STRT_2021$HI_ymin < 0 & HI_FI_turb_STRT_2021$HI_ymax > 0 & HI_FI_turb_STRT_2021$Hyst_index > 0)
which(HI_FI_turb_STRT_2021$HI_ymin < 0 & HI_FI_turb_STRT_2021$HI_ymax > 0 & HI_FI_turb_STRT_2021$Hyst_index < 0)

table(sign(HI_FI_turb_STRT_2021$Beta_index))
which(HI_FI_turb_STRT_2021$Beta_ymin < 0 & HI_FI_turb_STRT_2021$Beta_ymax > 0 & HI_FI_turb_STRT_2021$Beta_index > 0)
which(HI_FI_turb_STRT_2021$Beta_ymin < 0 & HI_FI_turb_STRT_2021$Beta_ymax > 0 & HI_FI_turb_STRT_2021$Beta_index < 0)


# VAUL ####
HI_FI_NO3_VAUL_2021 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_NO3_VAUL_2021$Hyst_index))
which(HI_FI_NO3_VAUL_2021$HI_ymin < 0 & HI_FI_NO3_VAUL_2021$HI_ymax > 0 & HI_FI_NO3_VAUL_2021$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2021$HI_ymin < 0 & HI_FI_NO3_VAUL_2021$HI_ymax > 0 & HI_FI_NO3_VAUL_2021$Hyst_index < 0)

table(sign(HI_FI_NO3_VAUL_2021$Beta_index))
which(HI_FI_NO3_VAUL_2021$Beta_ymin < 0 & HI_FI_NO3_VAUL_2021$Beta_ymax > 0 & HI_FI_NO3_VAUL_2021$Beta_index > 0)
which(HI_FI_NO3_VAUL_2021$Beta_ymin < 0 & HI_FI_NO3_VAUL_2021$Beta_ymax > 0 & HI_FI_NO3_VAUL_2021$Beta_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2021 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_fDOM_VAUL_2021$Hyst_index))
which(HI_FI_fDOM_VAUL_2021$HI_ymin < 0 & HI_FI_fDOM_VAUL_2021$HI_ymax > 0 & HI_FI_fDOM_VAUL_2021$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2021$HI_ymin < 0 & HI_FI_fDOM_VAUL_2021$HI_ymax > 0 & HI_FI_fDOM_VAUL_2021$Hyst_index < 0)

table(sign(HI_FI_fDOM_VAUL_2021$Beta_index))
which(HI_FI_fDOM_VAUL_2021$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2021$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2021$Beta_index > 0)
which(HI_FI_fDOM_VAUL_2021$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2021$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2021$Beta_index < 0)

#SPC
HI_FI_SPC_VAUL_2021 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_SPC_VAUL_2021$Hyst_index))
which(HI_FI_SPC_VAUL_2021$HI_ymin < 0 & HI_FI_SPC_VAUL_2021$HI_ymax > 0 & HI_FI_SPC_VAUL_2021$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2021$HI_ymin < 0 & HI_FI_SPC_VAUL_2021$HI_ymax > 0 & HI_FI_SPC_VAUL_2021$Hyst_index < 0)

table(sign(HI_FI_SPC_VAUL_2021$Beta_index))
which(HI_FI_SPC_VAUL_2021$Beta_ymin < 0 & HI_FI_SPC_VAUL_2021$Beta_ymax > 0 & HI_FI_SPC_VAUL_2021$Beta_index > 0)
which(HI_FI_SPC_VAUL_2021$Beta_ymin < 0 & HI_FI_SPC_VAUL_2021$Beta_ymax > 0 & HI_FI_SPC_VAUL_2021$Beta_index < 0)

#turb
HI_FI_turb_VAUL_2021 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2021")

table(sign(HI_FI_turb_VAUL_2021$Hyst_index))
which(HI_FI_turb_VAUL_2021$HI_ymin < 0 & HI_FI_turb_VAUL_2021$HI_ymax > 0 & HI_FI_turb_VAUL_2021$Hyst_index > 0)
which(HI_FI_turb_VAUL_2021$HI_ymin < 0 & HI_FI_turb_VAUL_2021$HI_ymax > 0 & HI_FI_turb_VAUL_2021$Hyst_index < 0)

table(sign(HI_FI_turb_VAUL_2021$Beta_index))
which(HI_FI_turb_VAUL_2021$Beta_ymin < 0 & HI_FI_turb_VAUL_2021$Beta_ymax > 0 & HI_FI_turb_VAUL_2021$Beta_index > 0)
which(HI_FI_turb_VAUL_2021$Beta_ymin < 0 & HI_FI_turb_VAUL_2021$Beta_ymax > 0 & HI_FI_turb_VAUL_2021$Beta_index < 0)



# 2022 ####
# FRCH #### 
# NO3 
HI_FI_NO3_FRCH_2022 <- subset(HI_FI_NO3, site.ID == "FRCH" & year == "2022")

table(sign(HI_FI_NO3_FRCH_2022$Hyst_index))
which(HI_FI_NO3_FRCH_2022$HI_ymin < 0 & HI_FI_NO3_FRCH_2022$HI_ymax > 0 & HI_FI_NO3_FRCH_2022$Hyst_index > 0)
which(HI_FI_NO3_FRCH_2022$HI_ymin < 0 & HI_FI_NO3_FRCH_2022$HI_ymax > 0 & HI_FI_NO3_FRCH_2022$Hyst_index < 0)

table(sign(HI_FI_NO3_FRCH_2022$Beta_index))
which(HI_FI_NO3_FRCH_2022$Beta_ymin < 0 & HI_FI_NO3_FRCH_2022$Beta_ymax > 0 & HI_FI_NO3_FRCH_2022$Beta_index > 0)
which(HI_FI_NO3_FRCH_2022$Beta_ymin < 0 & HI_FI_NO3_FRCH_2022$Beta_ymax > 0 & HI_FI_NO3_FRCH_2022$Beta_index < 0)


#fDOM 
HI_FI_fDOM_FRCH_2022 <- subset(HI_FI_fDOM, site.ID == "FRCH" & year == "2022")

table(sign(HI_FI_fDOM_FRCH_2022$Hyst_index))
which(HI_FI_fDOM_FRCH_2022$HI_ymin < 0 & HI_FI_fDOM_FRCH_2022$HI_ymax > 0 & HI_FI_fDOM_FRCH_2022$Hyst_index > 0)
which(HI_FI_fDOM_FRCH_2022$HI_ymin < 0 & HI_FI_fDOM_FRCH_2022$HI_ymax > 0 & HI_FI_fDOM_FRCH_2022$Hyst_index < 0)

table(sign(HI_FI_fDOM_FRCH_2022$Beta_index))
which(HI_FI_fDOM_FRCH_2022$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2022$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2022$Beta_index > 0)
which(HI_FI_fDOM_FRCH_2022$Beta_ymin < 0 & HI_FI_fDOM_FRCH_2022$Beta_ymax > 0 & HI_FI_fDOM_FRCH_2022$Beta_index < 0)

#SPC
HI_FI_SPC_FRCH_2022 <- subset(HI_FI_SPC, site.ID == "FRCH" & year == "2022")

table(sign(HI_FI_SPC_FRCH_2022$Hyst_index))
which(HI_FI_SPC_FRCH_2022$HI_ymin < 0 & HI_FI_SPC_FRCH_2022$HI_ymax > 0 & HI_FI_SPC_FRCH_2022$Hyst_index > 0)
which(HI_FI_SPC_FRCH_2022$HI_ymin < 0 & HI_FI_SPC_FRCH_2022$HI_ymax > 0 & HI_FI_SPC_FRCH_2022$Hyst_index < 0)

table(sign(HI_FI_SPC_FRCH_2022$Beta_index))
which(HI_FI_SPC_FRCH_2022$Beta_ymin < 0 & HI_FI_SPC_FRCH_2022$Beta_ymax > 0 & HI_FI_SPC_FRCH_2022$Beta_index > 0)
which(HI_FI_SPC_FRCH_2022$Beta_ymin < 0 & HI_FI_SPC_FRCH_2022$Beta_ymax > 0 & HI_FI_SPC_FRCH_2022$Beta_index < 0)

#turb
HI_FI_turb_FRCH_2022 <- subset(HI_FI_turb, site.ID == "FRCH" & year == "2022")

table(sign(HI_FI_turb_FRCH_2022$Hyst_index))
which(HI_FI_turb_FRCH_2022$HI_ymin < 0 & HI_FI_turb_FRCH_2022$HI_ymax > 0 & HI_FI_turb_FRCH_2022$Hyst_index > 0)
which(HI_FI_turb_FRCH_2022$HI_ymin < 0 & HI_FI_turb_FRCH_2022$HI_ymax > 0 & HI_FI_turb_FRCH_2022$Hyst_index < 0)

table(sign(HI_FI_turb_FRCH_2022$Beta_index))
which(HI_FI_turb_FRCH_2022$Beta_ymin < 0 & HI_FI_turb_FRCH_2022$Beta_ymax > 0 & HI_FI_turb_FRCH_2022$Beta_index > 0)
which(HI_FI_turb_FRCH_2022$Beta_ymin < 0 & HI_FI_turb_FRCH_2022$Beta_ymax > 0 & HI_FI_turb_FRCH_2022$Beta_index < 0)

# MOOS ####
HI_FI_NO3_MOOS_2022 <- subset(HI_FI_NO3, site.ID == "MOOS" & year == "2022")

table(sign(HI_FI_NO3_MOOS_2022$Hyst_index))
which(HI_FI_NO3_MOOS_2022$HI_ymin < 0 & HI_FI_NO3_MOOS_2022$HI_ymax > 0 & HI_FI_NO3_MOOS_2022$Hyst_index > 0)
which(HI_FI_NO3_MOOS_2022$HI_ymin < 0 & HI_FI_NO3_MOOS_2022$HI_ymax > 0 & HI_FI_NO3_MOOS_2022$Hyst_index < 0)

table(sign(HI_FI_NO3_MOOS_2022$Beta_index))
which(HI_FI_NO3_MOOS_2022$Beta_ymin < 0 & HI_FI_NO3_MOOS_2022$Beta_ymax > 0 & HI_FI_NO3_MOOS_2022$Beta_index > 0)
which(HI_FI_NO3_MOOS_2022$Beta_ymin < 0 & HI_FI_NO3_MOOS_2022$Beta_ymax > 0 & HI_FI_NO3_MOOS_2022$Beta_index < 0)


#fDOM 
HI_FI_fDOM_MOOS_2022 <- subset(HI_FI_fDOM, site.ID == "MOOS" & year == "2022")

table(sign(HI_FI_fDOM_MOOS_2022$Hyst_index))
which(HI_FI_fDOM_MOOS_2022$HI_ymin < 0 & HI_FI_fDOM_MOOS_2022$HI_ymax > 0 & HI_FI_fDOM_MOOS_2022$Hyst_index > 0)
which(HI_FI_fDOM_MOOS_2022$HI_ymin < 0 & HI_FI_fDOM_MOOS_2022$HI_ymax > 0 & HI_FI_fDOM_MOOS_2022$Hyst_index < 0)

table(sign(HI_FI_fDOM_MOOS_2022$Beta_index))
which(HI_FI_fDOM_MOOS_2022$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2022$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2022$Beta_index > 0)
which(HI_FI_fDOM_MOOS_2022$Beta_ymin < 0 & HI_FI_fDOM_MOOS_2022$Beta_ymax > 0 & HI_FI_fDOM_MOOS_2022$Beta_index < 0)

#SPC
HI_FI_SPC_MOOS_2022 <- subset(HI_FI_SPC, site.ID == "MOOS" & year == "2022")

table(sign(HI_FI_SPC_MOOS_2022$Hyst_index))
which(HI_FI_SPC_MOOS_2022$HI_ymin < 0 & HI_FI_SPC_MOOS_2022$HI_ymax > 0 & HI_FI_SPC_MOOS_2022$Hyst_index > 0)
which(HI_FI_SPC_MOOS_2022$HI_ymin < 0 & HI_FI_SPC_MOOS_2022$HI_ymax > 0 & HI_FI_SPC_MOOS_2022$Hyst_index < 0)

table(sign(HI_FI_SPC_MOOS_2022$Beta_index))
which(HI_FI_SPC_MOOS_2022$Beta_ymin < 0 & HI_FI_SPC_MOOS_2022$Beta_ymax > 0 & HI_FI_SPC_MOOS_2022$Beta_index > 0)
which(HI_FI_SPC_MOOS_2022$Beta_ymin < 0 & HI_FI_SPC_MOOS_2022$Beta_ymax > 0 & HI_FI_SPC_MOOS_2022$Beta_index < 0)

#turb
HI_FI_turb_MOOS_2022 <- subset(HI_FI_turb, site.ID == "MOOS" & year == "2022")

table(sign(HI_FI_turb_MOOS_2022$Hyst_index))
which(HI_FI_turb_MOOS_2022$HI_ymin < 0 & HI_FI_turb_MOOS_2022$HI_ymax > 0 & HI_FI_turb_MOOS_2022$Hyst_index > 0)
which(HI_FI_turb_MOOS_2022$HI_ymin < 0 & HI_FI_turb_MOOS_2022$HI_ymax > 0 & HI_FI_turb_MOOS_2022$Hyst_index < 0)

table(sign(HI_FI_turb_MOOS_2022$Beta_index))
which(HI_FI_turb_MOOS_2022$Beta_ymin < 0 & HI_FI_turb_MOOS_2022$Beta_ymax > 0 & HI_FI_turb_MOOS_2022$Beta_index > 0)
which(HI_FI_turb_MOOS_2022$Beta_ymin < 0 & HI_FI_turb_MOOS_2022$Beta_ymax > 0 & HI_FI_turb_MOOS_2022$Beta_index < 0)

# CARI ####
HI_FI_NO3_CARI_2022 <- subset(HI_FI_NO3, site.ID == "CARI" & year == "2022")

table(sign(HI_FI_NO3_CARI_2022$Hyst_index))
which(HI_FI_NO3_CARI_2022$HI_ymin < 0 & HI_FI_NO3_CARI_2022$HI_ymax > 0 & HI_FI_NO3_CARI_2022$Hyst_index > 0)
which(HI_FI_NO3_CARI_2022$HI_ymin < 0 & HI_FI_NO3_CARI_2022$HI_ymax > 0 & HI_FI_NO3_CARI_2022$Hyst_index < 0)

table(sign(HI_FI_NO3_CARI_2022$Beta_index))
which(HI_FI_NO3_CARI_2022$Beta_ymin < 0 & HI_FI_NO3_CARI_2022$Beta_ymax > 0 & HI_FI_NO3_CARI_2022$Beta_index > 0)
which(HI_FI_NO3_CARI_2022$Beta_ymin < 0 & HI_FI_NO3_CARI_2022$Beta_ymax > 0 & HI_FI_NO3_CARI_2022$Beta_index < 0)


#fDOM 
HI_FI_fDOM_CARI_2022 <- subset(HI_FI_fDOM, site.ID == "CARI" & year == "2022")

table(sign(HI_FI_fDOM_CARI_2022$Hyst_index))
which(HI_FI_fDOM_CARI_2022$HI_ymin < 0 & HI_FI_fDOM_CARI_2022$HI_ymax > 0 & HI_FI_fDOM_CARI_2022$Hyst_index > 0)
which(HI_FI_fDOM_CARI_2022$HI_ymin < 0 & HI_FI_fDOM_CARI_2022$HI_ymax > 0 & HI_FI_fDOM_CARI_2022$Hyst_index < 0)

table(sign(HI_FI_fDOM_CARI_2022$Beta_index))
which(HI_FI_fDOM_CARI_2022$Beta_ymin < 0 & HI_FI_fDOM_CARI_2022$Beta_ymax > 0 & HI_FI_fDOM_CARI_2022$Beta_index > 0)
which(HI_FI_fDOM_CARI_2022$Beta_ymin < 0 & HI_FI_fDOM_CARI_2022$Beta_ymax > 0 & HI_FI_fDOM_CARI_2022$Beta_index < 0)

#SPC
HI_FI_SPC_CARI_2022 <- subset(HI_FI_SPC, site.ID == "CARI" & year == "2022")

table(sign(HI_FI_SPC_CARI_2022$Hyst_index))
which(HI_FI_SPC_CARI_2022$HI_ymin < 0 & HI_FI_SPC_CARI_2022$HI_ymax > 0 & HI_FI_SPC_CARI_2022$Hyst_index > 0)
which(HI_FI_SPC_CARI_2022$HI_ymin < 0 & HI_FI_SPC_CARI_2022$HI_ymax > 0 & HI_FI_SPC_CARI_2022$Hyst_index < 0)

table(sign(HI_FI_SPC_CARI_2022$Beta_index))
which(HI_FI_SPC_CARI_2022$Beta_ymin < 0 & HI_FI_SPC_CARI_2022$Beta_ymax > 0 & HI_FI_SPC_CARI_2022$Beta_index > 0)
which(HI_FI_SPC_CARI_2022$Beta_ymin < 0 & HI_FI_SPC_CARI_2022$Beta_ymax > 0 & HI_FI_SPC_CARI_2022$Beta_index < 0)

#turb
HI_FI_turb_CARI_2022 <- subset(HI_FI_turb, site.ID == "CARI" & year == "2022")

table(sign(HI_FI_turb_CARI_2022$Hyst_index))
which(HI_FI_turb_CARI_2022$HI_ymin < 0 & HI_FI_turb_CARI_2022$HI_ymax > 0 & HI_FI_turb_CARI_2022$Hyst_index > 0)
which(HI_FI_turb_CARI_2022$HI_ymin < 0 & HI_FI_turb_CARI_2022$HI_ymax > 0 & HI_FI_turb_CARI_2022$Hyst_index < 0)

table(sign(HI_FI_turb_CARI_2022$Beta_index))
which(HI_FI_turb_CARI_2022$Beta_ymin < 0 & HI_FI_turb_CARI_2022$Beta_ymax > 0 & HI_FI_turb_CARI_2022$Beta_index > 0)
which(HI_FI_turb_CARI_2022$Beta_ymin < 0 & HI_FI_turb_CARI_2022$Beta_ymax > 0 & HI_FI_turb_CARI_2022$Beta_index < 0)

# POKE ####
HI_FI_NO3_POKE_2022 <- subset(HI_FI_NO3, site.ID == "POKE" & year == "2022")

table(sign(HI_FI_NO3_POKE_2022$Hyst_index))
which(HI_FI_NO3_POKE_2022$HI_ymin < 0 & HI_FI_NO3_POKE_2022$HI_ymax > 0 & HI_FI_NO3_POKE_2022$Hyst_index > 0)
which(HI_FI_NO3_POKE_2022$HI_ymin < 0 & HI_FI_NO3_POKE_2022$HI_ymax > 0 & HI_FI_NO3_POKE_2022$Hyst_index < 0)

table(sign(HI_FI_NO3_POKE_2022$Beta_index))
which(HI_FI_NO3_POKE_2022$Beta_ymin < 0 & HI_FI_NO3_POKE_2022$Beta_ymax > 0 & HI_FI_NO3_POKE_2022$Beta_index > 0)
which(HI_FI_NO3_POKE_2022$Beta_ymin < 0 & HI_FI_NO3_POKE_2022$Beta_ymax > 0 & HI_FI_NO3_POKE_2022$Beta_index < 0)


#fDOM 
HI_FI_fDOM_POKE_2022 <- subset(HI_FI_fDOM, site.ID == "POKE" & year == "2022")

table(sign(HI_FI_fDOM_POKE_2022$Hyst_index))
which(HI_FI_fDOM_POKE_2022$HI_ymin < 0 & HI_FI_fDOM_POKE_2022$HI_ymax > 0 & HI_FI_fDOM_POKE_2022$Hyst_index > 0)
which(HI_FI_fDOM_POKE_2022$HI_ymin < 0 & HI_FI_fDOM_POKE_2022$HI_ymax > 0 & HI_FI_fDOM_POKE_2022$Hyst_index < 0)

table(sign(HI_FI_fDOM_POKE_2022$Beta_index))
which(HI_FI_fDOM_POKE_2022$Beta_ymin < 0 & HI_FI_fDOM_POKE_2022$Beta_ymax > 0 & HI_FI_fDOM_POKE_2022$Beta_index > 0)
which(HI_FI_fDOM_POKE_2022$Beta_ymin < 0 & HI_FI_fDOM_POKE_2022$Beta_ymax > 0 & HI_FI_fDOM_POKE_2022$Beta_index < 0)

#SPC
HI_FI_SPC_POKE_2022 <- subset(HI_FI_SPC, site.ID == "POKE" & year == "2022")

table(sign(HI_FI_SPC_POKE_2022$Hyst_index))
which(HI_FI_SPC_POKE_2022$HI_ymin < 0 & HI_FI_SPC_POKE_2022$HI_ymax > 0 & HI_FI_SPC_POKE_2022$Hyst_index > 0)
which(HI_FI_SPC_POKE_2022$HI_ymin < 0 & HI_FI_SPC_POKE_2022$HI_ymax > 0 & HI_FI_SPC_POKE_2022$Hyst_index < 0)

table(sign(HI_FI_SPC_POKE_2022$Beta_index))
which(HI_FI_SPC_POKE_2022$Beta_ymin < 0 & HI_FI_SPC_POKE_2022$Beta_ymax > 0 & HI_FI_SPC_POKE_2022$Beta_index > 0)
which(HI_FI_SPC_POKE_2022$Beta_ymin < 0 & HI_FI_SPC_POKE_2022$Beta_ymax > 0 & HI_FI_SPC_POKE_2022$Beta_index < 0)

#turb
HI_FI_turb_POKE_2022 <- subset(HI_FI_turb, site.ID == "POKE" & year == "2022")

table(sign(HI_FI_turb_POKE_2022$Hyst_index))
which(HI_FI_turb_POKE_2022$HI_ymin < 0 & HI_FI_turb_POKE_2022$HI_ymax > 0 & HI_FI_turb_POKE_2022$Hyst_index > 0)
which(HI_FI_turb_POKE_2022$HI_ymin < 0 & HI_FI_turb_POKE_2022$HI_ymax > 0 & HI_FI_turb_POKE_2022$Hyst_index < 0)

table(sign(HI_FI_turb_POKE_2022$Beta_index))
which(HI_FI_turb_POKE_2022$Beta_ymin < 0 & HI_FI_turb_POKE_2022$Beta_ymax > 0 & HI_FI_turb_POKE_2022$Beta_index > 0)
which(HI_FI_turb_POKE_2022$Beta_ymin < 0 & HI_FI_turb_POKE_2022$Beta_ymax > 0 & HI_FI_turb_POKE_2022$Beta_index < 0)

# STRT ####
HI_FI_NO3_STRT_2022 <- subset(HI_FI_NO3, site.ID == "STRT" & year == "2022")

table(sign(HI_FI_NO3_STRT_2022$Hyst_index))
which(HI_FI_NO3_STRT_2022$HI_ymin < 0 & HI_FI_NO3_STRT_2022$HI_ymax > 0 & HI_FI_NO3_STRT_2022$Hyst_index > 0)
which(HI_FI_NO3_STRT_2022$HI_ymin < 0 & HI_FI_NO3_STRT_2022$HI_ymax > 0 & HI_FI_NO3_STRT_2022$Hyst_index < 0)

table(sign(HI_FI_NO3_STRT_2022$Beta_index))
which(HI_FI_NO3_STRT_2022$Beta_ymin < 0 & HI_FI_NO3_STRT_2022$Beta_ymax > 0 & HI_FI_NO3_STRT_2022$Beta_index > 0)
which(HI_FI_NO3_STRT_2022$Beta_ymin < 0 & HI_FI_NO3_STRT_2022$Beta_ymax > 0 & HI_FI_NO3_STRT_2022$Beta_index < 0)


#fDOM 
HI_FI_fDOM_STRT_2022 <- subset(HI_FI_fDOM, site.ID == "STRT" & year == "2022")

table(sign(HI_FI_fDOM_STRT_2022$Hyst_index))
which(HI_FI_fDOM_STRT_2022$HI_ymin < 0 & HI_FI_fDOM_STRT_2022$HI_ymax > 0 & HI_FI_fDOM_STRT_2022$Hyst_index > 0)
which(HI_FI_fDOM_STRT_2022$HI_ymin < 0 & HI_FI_fDOM_STRT_2022$HI_ymax > 0 & HI_FI_fDOM_STRT_2022$Hyst_index < 0)

table(sign(HI_FI_fDOM_STRT_2022$Beta_index))
which(HI_FI_fDOM_STRT_2022$Beta_ymin < 0 & HI_FI_fDOM_STRT_2022$Beta_ymax > 0 & HI_FI_fDOM_STRT_2022$Beta_index > 0)
which(HI_FI_fDOM_STRT_2022$Beta_ymin < 0 & HI_FI_fDOM_STRT_2022$Beta_ymax > 0 & HI_FI_fDOM_STRT_2022$Beta_index < 0)

#SPC
HI_FI_SPC_STRT_2022 <- subset(HI_FI_SPC, site.ID == "STRT" & year == "2022")

table(sign(HI_FI_SPC_STRT_2022$Hyst_index))
which(HI_FI_SPC_STRT_2022$HI_ymin < 0 & HI_FI_SPC_STRT_2022$HI_ymax > 0 & HI_FI_SPC_STRT_2022$Hyst_index > 0)
which(HI_FI_SPC_STRT_2022$HI_ymin < 0 & HI_FI_SPC_STRT_2022$HI_ymax > 0 & HI_FI_SPC_STRT_2022$Hyst_index < 0)

table(sign(HI_FI_SPC_STRT_2022$Beta_index))
which(HI_FI_SPC_STRT_2022$Beta_ymin < 0 & HI_FI_SPC_STRT_2022$Beta_ymax > 0 & HI_FI_SPC_STRT_2022$Beta_index > 0)
which(HI_FI_SPC_STRT_2022$Beta_ymin < 0 & HI_FI_SPC_STRT_2022$Beta_ymax > 0 & HI_FI_SPC_STRT_2022$Beta_index < 0)

#turb
HI_FI_turb_STRT_2022 <- subset(HI_FI_turb, site.ID == "STRT" & year == "2022")

table(sign(HI_FI_turb_STRT_2022$Hyst_index))
which(HI_FI_turb_STRT_2022$HI_ymin < 0 & HI_FI_turb_STRT_2022$HI_ymax > 0 & HI_FI_turb_STRT_2022$Hyst_index > 0)
which(HI_FI_turb_STRT_2022$HI_ymin < 0 & HI_FI_turb_STRT_2022$HI_ymax > 0 & HI_FI_turb_STRT_2022$Hyst_index < 0)

table(sign(HI_FI_turb_STRT_2022$Beta_index))
which(HI_FI_turb_STRT_2022$Beta_ymin < 0 & HI_FI_turb_STRT_2022$Beta_ymax > 0 & HI_FI_turb_STRT_2022$Beta_index > 0)
which(HI_FI_turb_STRT_2022$Beta_ymin < 0 & HI_FI_turb_STRT_2022$Beta_ymax > 0 & HI_FI_turb_STRT_2022$Beta_index < 0)


# VAUL ####
HI_FI_NO3_VAUL_2022 <- subset(HI_FI_NO3, site.ID == "VAUL" & year == "2022")

table(sign(HI_FI_NO3_VAUL_2022$Hyst_index))
which(HI_FI_NO3_VAUL_2022$HI_ymin < 0 & HI_FI_NO3_VAUL_2022$HI_ymax > 0 & HI_FI_NO3_VAUL_2022$Hyst_index > 0)
which(HI_FI_NO3_VAUL_2022$HI_ymin < 0 & HI_FI_NO3_VAUL_2022$HI_ymax > 0 & HI_FI_NO3_VAUL_2022$Hyst_index < 0)

table(sign(HI_FI_NO3_VAUL_2022$Beta_index))
which(HI_FI_NO3_VAUL_2022$Beta_ymin < 0 & HI_FI_NO3_VAUL_2022$Beta_ymax > 0 & HI_FI_NO3_VAUL_2022$Beta_index > 0)
which(HI_FI_NO3_VAUL_2022$Beta_ymin < 0 & HI_FI_NO3_VAUL_2022$Beta_ymax > 0 & HI_FI_NO3_VAUL_2022$Beta_index < 0)


#fDOM 
HI_FI_fDOM_VAUL_2022 <- subset(HI_FI_fDOM, site.ID == "VAUL" & year == "2022")

table(sign(HI_FI_fDOM_VAUL_2022$Hyst_index))
which(HI_FI_fDOM_VAUL_2022$HI_ymin < 0 & HI_FI_fDOM_VAUL_2022$HI_ymax > 0 & HI_FI_fDOM_VAUL_2022$Hyst_index > 0)
which(HI_FI_fDOM_VAUL_2022$HI_ymin < 0 & HI_FI_fDOM_VAUL_2022$HI_ymax > 0 & HI_FI_fDOM_VAUL_2022$Hyst_index < 0)

table(sign(HI_FI_fDOM_VAUL_2022$Beta_index))
which(HI_FI_fDOM_VAUL_2022$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2022$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2022$Beta_index > 0)
which(HI_FI_fDOM_VAUL_2022$Beta_ymin < 0 & HI_FI_fDOM_VAUL_2022$Beta_ymax > 0 & HI_FI_fDOM_VAUL_2022$Beta_index < 0)

#SPC
HI_FI_SPC_VAUL_2022 <- subset(HI_FI_SPC, site.ID == "VAUL" & year == "2022")

table(sign(HI_FI_SPC_VAUL_2022$Hyst_index))
which(HI_FI_SPC_VAUL_2022$HI_ymin < 0 & HI_FI_SPC_VAUL_2022$HI_ymax > 0 & HI_FI_SPC_VAUL_2022$Hyst_index > 0)
which(HI_FI_SPC_VAUL_2022$HI_ymin < 0 & HI_FI_SPC_VAUL_2022$HI_ymax > 0 & HI_FI_SPC_VAUL_2022$Hyst_index < 0)

table(sign(HI_FI_SPC_VAUL_2022$Beta_index))
which(HI_FI_SPC_VAUL_2022$Beta_ymin < 0 & HI_FI_SPC_VAUL_2022$Beta_ymax > 0 & HI_FI_SPC_VAUL_2022$Beta_index > 0)
which(HI_FI_SPC_VAUL_2022$Beta_ymin < 0 & HI_FI_SPC_VAUL_2022$Beta_ymax > 0 & HI_FI_SPC_VAUL_2022$Beta_index < 0)

#turb
HI_FI_turb_VAUL_2022 <- subset(HI_FI_turb, site.ID == "VAUL" & year == "2022")

table(sign(HI_FI_turb_VAUL_2022$Hyst_index))
which(HI_FI_turb_VAUL_2022$HI_ymin < 0 & HI_FI_turb_VAUL_2022$HI_ymax > 0 & HI_FI_turb_VAUL_2022$Hyst_index > 0)
which(HI_FI_turb_VAUL_2022$HI_ymin < 0 & HI_FI_turb_VAUL_2022$HI_ymax > 0 & HI_FI_turb_VAUL_2022$Hyst_index < 0)

table(sign(HI_FI_turb_VAUL_2022$Beta_index))
which(HI_FI_turb_VAUL_2022$Beta_ymin < 0 & HI_FI_turb_VAUL_2022$Beta_ymax > 0 & HI_FI_turb_VAUL_2022$Beta_index > 0)
which(HI_FI_turb_VAUL_2022$Beta_ymin < 0 & HI_FI_turb_VAUL_2022$Beta_ymax > 0 & HI_FI_turb_VAUL_2022$Beta_index < 0)

###############################################################
### Figuring out how many days are missing from the record  ###
###############################################################
# 2015 ####
chem.2015 <- read.csv(here("processed_sensor_data", "2015", "SUNA.EXO.int.corr.lab_2015.csv"), na.strings = "NA")

chem.2015 <- chem.2015[c("datetimeAK", "Site", "fDOM.QSU.adj.T.turb.int", "SpCond.uScm.adj",
                         "Turbidity.FNU.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]

chem.2015$datetimeAK <- ymd_hms(chem.2015$datetimeAK) # converting character to datetime
chem.2015$datetimeAK <- force_tz(chem.2015$datetimeAK, "America/Anchorage") # converting character to datetime

names(chem.2015) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")

FRCH.2015 <-  subset(chem.2015, site.ID == "FRCH")
FRCH.2015 <- FRCH.2015[-c(12519:12849), ] # removing unnecessary rows that correspond to when I merge the file the NO3 from the lab merges weird with datetimes from another section within the dataframe

MOOS.2015 <-  subset(chem.2015, site.ID == "MOOS")
MOOS.2015 <- MOOS.2015[-c(12540:12796), ] # removing unnecessary rows that correspond to when I merge the file the NO3 from the lab merges weird with datetimes from another section within the dataframe

#plot
ggplot(MOOS.2015, aes(x = datetimeAK, y = MOOS.2015$NO3)) +
  geom_point()

#  FRCH
FRCH.2015$DOY <- yday(FRCH.2015$datetimeAK)
FRCH_fDOM <- FRCH.2015[c("datetimeAK", "fDOM")]
FRCH_SPC <- FRCH.2015[c("datetimeAK", "SPC")]
FRCH_turb <- FRCH.2015[c("datetimeAK", "Turb")]
FRCH_NO3 <- FRCH.2015[c("datetimeAK", "NO3")]

#  MOOS
MOOS.2015$DOY <- yday(MOOS.2015$datetimeAK)
MOOS_fDOM <- MOOS.2015[c("datetimeAK", "fDOM")]
MOOS_SPC <- MOOS.2015[c("datetimeAK", "SPC")]
MOOS_turb <- MOOS.2015[c("datetimeAK", "Turb")]
MOOS_NO3 <- MOOS.2015[c("datetimeAK", "NO3")]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in seconds
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# 2018 ####
MOOS_2018 <- read.csv(here("processed_sensor_data", "2018", "EXO_MOOS_final_formatted.csv"))
FRCH_2018 <- read.csv(here("processed_sensor_data", "2018", "EXO_FRCH_final_formatted.csv"))
CARI_2018 <- read.csv(here("processed_sensor_data", "2018", "NEON_Q_WaterQuality2018.csv"))

# converting to datetime 
MOOS_2018$datetimeAK <- ymd_hms(MOOS_2018$datetimeAK)
FRCH_2018$datetimeAK <- ymd_hms(FRCH_2018$datetimeAK)
CARI_2018$DateTimeAK <- ymd_hms(CARI_2018$DateTimeAK)

# CARI_2018 <- CARI_2018 %>% # common window 
#   mutate(across(c(DateTimeAK), 
#                 ~ifelse(DateTimeAK >= "2018-06-27" & DateTimeAK <= "2018-10-12", NA, .)))

CARI_2018 <- CARI_2018[-c(1:3793),]  # removing not within the common window

#plot
ggplot(CARI_2018, aes(x = DateTimeAK, y = NO3)) +
  geom_point()

#  MOOS
MOOS_2018$DOY <- yday(FRCH_2018$datetimeAK)
MOOS_fDOM <- MOOS_2018[c("datetimeAK", "fDOM.QSU.mn.adj")]
MOOS_SPC <- MOOS_2018[c("datetimeAK", "SpCond.uScm.mn.adj")]
MOOS_turb <- MOOS_2018[c("datetimeAK", "Turbidity.FNU.mn.adj")]

#  FRCH
FRCH_2018$DOY <- yday(FRCH_2018$datetimeAK)
FRCH_fDOM <- FRCH_2018[c("datetimeAK", "fDOM.QSU.mn.adj")]
FRCH_SPC <- FRCH_2018[c("datetimeAK", "SpCond.uScm.mn.adj")]
FRCH_turb <- FRCH_2018[c("datetimeAK", "Turbidity.FNU.mn.adj")]

# CARI
CARI_2018$DOY <- yday(CARI_2018$DateTimeAK)
CARI_fDOM <- CARI_2018[c("DateTimeAK", "fDOM")]
CARI_SPC <- CARI_2018[c("DateTimeAK", "SPC")]
CARI_turb <- CARI_2018[c("DateTimeAK", "Turb")]
CARI_no3 <- CARI_2018[c("DateTimeAK", "NO3")]

# identifying gaps 
my_dat = data.frame(DateTimeAK = na.omit(CARI_turb$DateTimeAK))
my_dat$DateTimeAK = sort(my_dat$DateTimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, DateTimeAK[-1] - DateTimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$DateTimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate #
FRCH_SUNA <- read.csv(here("processed_sensor_data", "2018", "FRCH_SUNA_means_detailed_clean.csv"))
MOOS_SUNA <- read.csv(here("processed_sensor_data", "2018", "MOOS_SUNA_means_detailed_clean.csv"))

# converting to datetime 
FRCH_SUNA$datetimeAK <- ymd_hms(FRCH_SUNA$datetimeAK)
MOOS_SUNA$datetimeAK <- ymd_hms(MOOS_SUNA$datetimeAK)

#  FRCH
FRCH_SUNA$DOY <- yday(FRCH_SUNA$datetimeAK)
FRCH_NO3 <- FRCH_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]

#  MOOS
MOOS_SUNA$DOY <- yday(MOOS_SUNA$datetimeAK)
MOOS_NO3 <- MOOS_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(MOOS_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# 2019 ####
POKE_2019 <- read.csv(here("processed_sensor_data", "2019", "POKE_EXO_stitched_formatted.csv"))
STRT_2019 <- read.csv(here("processed_sensor_data", "2019", "STRT_EXO_stitched_formatted.csv"))
VAUL_2019 <- read.csv(here("processed_sensor_data", "2019", "VAUL_EXO_stitched_formatted.csv"))
MOOS_2019 <- read.csv(here("processed_sensor_data", "2019", "MOOS_EXO_stitched_formatted.csv"))
FRCH_2019 <- read.csv(here("processed_sensor_data", "2019", "FRCH_EXO_stitched_formatted.csv"))
CARI_2019 <- read.csv(here("processed_sensor_data", "2019", "NEON_Q_WaterQuality2019.csv"))

# converting to datetime 
POKE_2019$datetimeAK <- ymd_hms(POKE_2019$datetimeAK)
STRT_2019$datetimeAK <- ymd_hms(STRT_2019$datetimeAK)
VAUL_2019$datetimeAK <- ymd_hms(VAUL_2019$datetimeAK)
MOOS_2019$datetimeAK <- ymd_hms(MOOS_2019$datetimeAK)
FRCH_2019$datetimeAK <- ymd_hms(FRCH_2019$datetimeAK)
CARI_2019$DateTimeAK <- ymd_hms(CARI_2019$DateTimeAK)

# CARI_2019 <- CARI_2019 %>% # common window 
#   mutate(across(c(DateTimeAK), 
#                 ~ifelse(DateTimeAK >= "2019-06-16" & DateTimeAK <= "2019-10-01", NA, .)))

CARI_2019 <- CARI_2019[-c(1:3794),]  # removing not within the common window


#plot
ggplot(FRCH_2019, aes(x = datetimeAK, y = FRCH_2019$fDOM.QSU)) +
  geom_point()

# POKE
POKE_2019$DOY <- yday(POKE_2019$datetimeAK)
POKE_fDOM <- POKE_2019[c("fDOM.QSU", "datetimeAK", "DOY")]
POKE_SPC <- POKE_2019[c("SpCond.uScm", "datetimeAK", "DOY")]
POKE_turb <- POKE_2019[c("Turbidity.FNU", "datetimeAK", "DOY")]

# STRT
STRT_2019$DOY <- yday(STRT_2019$datetimeAK)
STRT_fDOM <- STRT_2019[c("fDOM.QSU", "datetimeAK", "DOY")]
STRT_SPC <- STRT_2019[c("SpCond.uScm", "datetimeAK", "DOY")]
STRT_turb <- STRT_2019[c("Turbidity.FNU", "datetimeAK", "DOY")]

# VAUL
VAUL_2019$DOY <- yday(VAUL_2019$datetimeAK)
VAUL_fDOM <- VAUL_2019[c("fDOM.QSU", "datetimeAK", "DOY")]
VAUL_SPC <- VAUL_2019[c("SpCond.uScm", "datetimeAK", "DOY")]
VAUL_turb <- VAUL_2019[c("Turbidity.FNU", "datetimeAK", "DOY")]

#  MOOS
MOOS_2019$DOY <- yday(MOOS_2019$datetimeAK)
MOOS_fDOM <- MOOS_2019[c("fDOM.QSU", "datetimeAK", "DOY")]
MOOS_SPC <- MOOS_2019[c("SpCond.uScm", "datetimeAK", "DOY")]
MOOS_turb <- MOOS_2019[c("Turbidity.FNU", "datetimeAK", "DOY")]

#  FRCH
FRCH_2019$DOY <- yday(FRCH_2019$datetimeAK)
FRCH_fDOM <- FRCH_2019[c("fDOM.QSU", "datetimeAK", "DOY")]
FRCH_SPC <- FRCH_2019[c("SpCond.uScm", "datetimeAK", "DOY")]
FRCH_turb <- FRCH_2019[c("Turbidity.FNU", "datetimeAK", "DOY")]

# CARI
CARI_2019$DOY <- yday(CARI_2019$DateTimeAK)
CARI_fDOM <- CARI_2019[c("fDOM", "DateTimeAK", "DOY")]
CARI_SPC <- CARI_2019[c("SPC", "DateTimeAK", "DOY")]
CARI_turb <- CARI_2019[c("Turb", "DateTimeAK", "DOY")]
CARI_no3 <- CARI_2019[c("NO3", "DateTimeAK", "DOY")]

#plot
ggplot(CARI_2019, aes(x = DateTimeAK, y = CARI_2019$NO3)) +
  geom_point()

# identifying gaps 
my_dat = data.frame(DateTimeAK = na.omit(CARI_no3$DateTimeAK))
my_dat$DateTimeAK = sort(my_dat$DateTimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, DateTimeAK[-1] - DateTimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$DateTimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_turb$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in seconds
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate
POKE_SUNA <- read.csv(here("processed_sensor_data", "2019", "POKE_SUNA_burst_detailed_clean.csv"))
STRT_SUNA <- read.csv(here("processed_sensor_data", "2019", "STRT_SUNA_burst_detailed_clean.csv"))
VAUL_SUNA <- read.csv(here("processed_sensor_data", "2019", "VAUL_SUNA_burst_detailed_clean.csv"))
MOOS_SUNA <- read.csv(here("processed_sensor_data", "2019", "MOOS_SUNA_burst_detailed_clean.csv"))
FRCH_SUNA <- read.csv(here("processed_sensor_data", "2019", "FRCH_SUNA_burst_detailed_clean.csv"))

# converting to datetime 
POKE_SUNA$datetimeAK <- ymd_hms(POKE_SUNA$datetimeAK)
STRT_SUNA$datetimeAK <- ymd_hms(STRT_SUNA$datetimeAK)
VAUL_SUNA$datetimeAK <- ymd_hms(VAUL_SUNA$datetimeAK)
MOOS_SUNA$datetimeAK <- ymd_hms(MOOS_SUNA$datetimeAK)
FRCH_SUNA$datetimeAK <- ymd_hms(FRCH_SUNA$datetimeAK)

#plot
ggplot(POKE_NO3, aes(x = datetimeAK, y = POKE_NO3$nitrateuM)) +
  geom_point()


# POKE
POKE_SUNA$DOY <- yday(POKE_SUNA$datetimeAK)
POKE_NO3 <- POKE_SUNA[c("nitrateuM", "datetimeAK", "DOY")]

# STRT
STRT_SUNA$DOY <- yday(STRT_SUNA$datetimeAK)
STRT_NO3 <- STRT_SUNA[c("nitrateuM", "datetimeAK", "DOY")]

# VAUL
VAUL_SUNA$DOY <- yday(VAUL_SUNA$datetimeAK)
VAUL_NO3 <- VAUL_SUNA[c("nitrateuM", "datetimeAK", "DOY")]

# MOOS
MOOS_SUNA$DOY <- yday(MOOS_SUNA$datetimeAK)
MOOS_NO3 <- MOOS_SUNA[c("nitrateuM", "datetimeAK", "DOY")]

# FRCH
FRCH_SUNA$DOY <- yday(FRCH_SUNA$datetimeAK)
FRCH_NO3 <- FRCH_SUNA[c("nitrateuM", "datetimeAK", "DOY")]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in seconds
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# 2020 ####
POKE_2020 <- read.csv(here("processed_sensor_data", "2020", "POKE.EXO.cl.csv"))
STRT_2020 <- read.csv(here("processed_sensor_data", "2020", "STRT.EXO.cl.csv"))
VAUL_2020 <- read.csv(here("processed_sensor_data", "2020", "VAUL.EXO.cl.csv"))
MOOS_2020 <- read.csv(here("processed_sensor_data", "2020", "MOOS.EXO.cl.csv"))
FRCH_2020 <- read.csv(here("processed_sensor_data", "2020", "FRCH.EXO.cl.csv"))
CARI_2020 <- read.csv(here("processed_sensor_data", "2020", "NEON_Q_WaterQuality2020.csv"))

# converting to datetime 
POKE_2020$datetimeAK <- ymd_hms(POKE_2020$datetimeAK)
STRT_2020$datetimeAK <- ymd_hms(STRT_2020$datetimeAK)
VAUL_2020$datetimeAK <- ymd_hms(VAUL_2020$datetimeAK)
MOOS_2020$datetimeAK <- ymd_hms(MOOS_2020$datetimeAK)
FRCH_2020$datetimeAK <- ymd_hms(FRCH_2020$datetimeAK)
CARI_2020$DateTimeAK <- ymd_hms(CARI_2020$DateTimeAK)

# CARI_2020 <- CARI_2020 %>% # common window 
#   mutate(across(c(DateTimeAK), 
#                 ~ifelse(DateTimeAK >= "2020-06-17" & DateTimeAK <= "2020-09-30", NA, .)))

CARI_2020 <- CARI_2020[-c(1:4461),]  # removing not within the common window



#plot
ggplot(FRCH_2020, aes(x = datetimeAK, y = FRCH_2020$fDOM.QSU.mn)) +
  geom_point()

# POKE
POKE_2020$DOY <- yday(POKE_2020$datetimeAK)
POKE_fDOM <- POKE_2020[c("fDOM.QSU.mn", "datetimeAK", "DOY")]
POKE_SPC <- POKE_2020[c("SpCond.uScm.mn", "datetimeAK", "DOY")]
POKE_turb <- POKE_2020[c("Turbidity.FNU.mn", "datetimeAK", "DOY")]

# STRT
STRT_2020$DOY <- yday(STRT_2020$datetimeAK)
STRT_fDOM <- STRT_2020[c("fDOM.QSU.mn", "datetimeAK", "DOY")]
STRT_SPC <- STRT_2020[c("SpCond.uScm.mn", "datetimeAK", "DOY")]
STRT_turb <- STRT_2020[c("Turbidity.FNU.mn", "datetimeAK", "DOY")]

# VAUL
VAUL_2020$DOY <- yday(VAUL_2020$datetimeAK)
VAUL_fDOM <- VAUL_2020[c("fDOM.QSU.mn", "datetimeAK", "DOY")]
VAUL_SPC <- VAUL_2020[c("SpCond.uScm.mn", "datetimeAK", "DOY")]
VAUL_turb <- VAUL_2020[c("Turbidity.FNU.mn", "datetimeAK", "DOY")]

#  MOOS
MOOS_2020$DOY <- yday(MOOS_2020$datetimeAK)
MOOS_fDOM <- MOOS_2020[c("fDOM.QSU.mn", "datetimeAK", "DOY")]
MOOS_SPC <- MOOS_2020[c("SpCond.uScm.mn", "datetimeAK", "DOY")]
MOOS_turb <- MOOS_2020[c("Turbidity.FNU.mn", "datetimeAK", "DOY")]

#  FRCH
FRCH_2020$DOY <- yday(FRCH_2020$datetimeAK)
FRCH_fDOM <- FRCH_2020[c("fDOM.QSU.mn", "datetimeAK", "DOY")]
FRCH_SPC <- FRCH_2020[c("SpCond.uScm.mn", "datetimeAK", "DOY")]
FRCH_turb <- FRCH_2020[c("Turbidity.FNU.mn", "datetimeAK", "DOY")]

#  CARI
CARI_2020$DOY <- yday(CARI_2020$DateTimeAK)
CARI_fDOM <- CARI_2020[c("fDOM", "DateTimeAK", "DOY")]
CARI_SPC <- CARI_2020[c("SPC", "DateTimeAK", "DOY")]
CARI_turb <- CARI_2020[c("Turb", "DateTimeAK", "DOY")]
CARI_no3 <- CARI_2020[c("NO3", "DateTimeAK", "DOY")]

ggplot(CARI_2020, aes(x = DateTimeAK, y = CARI_2020$NO3)) +
  geom_point()

# identifying gaps 
my_dat = data.frame(DateTimeAK = na.omit(CARI_no3$DateTimeAK))
my_dat$DateTimeAK = sort(my_dat$DateTimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, DateTimeAK[-1] - DateTimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$DateTimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]



# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_turb$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate 
POKE_SUNA <- read.csv(here("processed_sensor_data", "2020", "POKE_SUNA_means_detailed_clean.csv"))
STRT_SUNA <- read.csv(here("processed_sensor_data", "2020", "STRT_SUNA_means_detailed_clean.csv"))
VAUL_SUNA <- read.csv(here("processed_sensor_data", "2020", "VAUL_SUNA_means_detailed_clean.csv"))
MOOS_SUNA <- read.csv(here("processed_sensor_data", "2020", "MOOS_SUNA_means_detailed_clean.csv"))
FRCH_SUNA <- read.csv(here("processed_sensor_data", "2020", "FRCH_SUNA_means_detailed_clean.csv"))

# converting to datetime 
POKE_SUNA$datetimeAK <- ymd_hms(POKE_SUNA$datetimeAK)
STRT_SUNA$datetimeAK <- ymd_hms(STRT_SUNA$datetimeAK)
VAUL_SUNA$datetimeAK <- ymd_hms(VAUL_SUNA$datetimeAK)
MOOS_SUNA$datetimeAK <- ymd_hms(MOOS_SUNA$datetimeAK)
FRCH_SUNA$datetimeAK <- ymd_hms(FRCH_SUNA$datetimeAK)


#plot
ggplot(POKE_SUNA, aes(x = datetimeAK, y = POKE_SUNA$nitrateuM.adj.mn)) +
  geom_point()

# POKE
POKE_SUNA$DOY <- yday(POKE_SUNA$datetimeAK)
POKE_NO3 <- POKE_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]

# STRT
STRT_SUNA$DOY <- yday(STRT_SUNA$datetimeAK)
STRT_NO3 <- STRT_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]

# VAUL
VAUL_SUNA$DOY <- yday(VAUL_SUNA$datetimeAK)
VAUL_NO3 <- VAUL_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]

# MOOS
MOOS_SUNA$DOY <- yday(MOOS_SUNA$datetimeAK)
MOOS_NO3 <- MOOS_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]

# FRCH
FRCH_SUNA$DOY <- yday(FRCH_SUNA$datetimeAK)
FRCH_NO3 <- FRCH_SUNA[c("datetimeAK", "nitrateuM.mn", "DOY")]


# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]



# 2021 ####
EXO_processed <- read_csv("~/Documents/DoD_2021/EXO_data/from_internal_harddrive/processed/EXO.processed.csv")
CARI_2021 <- read.csv(here("processed_sensor_data", "2021", "NEON_Q_WaterQuality2021.csv"))

POKE_2021 <- subset(EXO_processed, site.ID == "POKE")
STRT_2021 <- subset(EXO_processed, site.ID == "STRT")
VAUL_2021 <- subset(EXO_processed, site.ID == "VAUL")
MOOS_2021 <- subset(EXO_processed, site.ID == "MOOS")
FRCH_2021 <- subset(EXO_processed, site.ID == "FRCH")

CARI_2021$DateTimeAK <- ymd_hms(CARI_2021$DateTimeAK)

# CARI_2021 <- CARI_2021 %>% # common window 
#   mutate(across(c(DateTimeAK), 
#                 ~ifelse(DateTimeAK >= "2021-06-12" & DateTimeAK <= "2021-09-27", NA, .)))

CARI_2021 <- CARI_2021[-c(1:3944),]  # removing not within the common window
CARI_2021 <- CARI_2021[-c(10329:12387),]  # removing not within the common window


#plot
ggplot(POKE_2021, aes(x = datetimeAK, y = POKE_2021$fDOM.QSU)) +
  geom_point()


# POKE
POKE_2021$DOY <- yday(POKE_2021$datetimeAK)
POKE_fDOM <- POKE_2021[,-c(1:6,8:22) ]
POKE_SPC <- POKE_2021[,-c(1:13,15:22) ]
POKE_turb <- POKE_2021[,-c(1:15, 17:22) ]

# STRT
STRT_2021$DOY <- yday(STRT_2021$datetimeAK)
STRT_fDOM <- STRT_2021[,-c(1:6,8:22) ]
STRT_SPC <- STRT_2021[,-c(1:13,15:22) ]
STRT_turb <- STRT_2021[,-c(1:15, 17:22) ]

# VAUL
VAUL_2021$DOY <- yday(VAUL_2021$datetimeAK)
VAUL_fDOM <- VAUL_2021[,-c(1:6,8:22) ]
VAUL_SPC <- VAUL_2021[,-c(1:13,15:22) ]
VAUL_turb <- VAUL_2021[,-c(1:15, 17:22) ]

#  MOOS
MOOS_2021$DOY <- yday(MOOS_2021$datetimeAK)
MOOS_fDOM <- MOOS_2021[,-c(1:6,8:22) ]
MOOS_SPC <- MOOS_2021[,-c(1:13,15:22) ]
MOOS_turb <- MOOS_2021[,-c(1:15, 17:22) ]

#  FRCH
FRCH_2021$DOY <- yday(FRCH_2021$datetimeAK)
FRCH_fDOM <- FRCH_2021[,-c(1:6,8:22) ]
FRCH_SPC <- FRCH_2021[,-c(1:13,15:22) ]
FRCH_turb <- FRCH_2021[,-c(1:15, 17:22) ]

#  CARI
CARI_2021$DOY <- yday(CARI_2021$DateTimeAK)
CARI_fDOM <- CARI_2021[c("fDOM", "DateTimeAK", "DOY")]
CARI_SPC <- CARI_2021[c("SPC", "DateTimeAK", "DOY")]
CARI_turb <- CARI_2021[c("Turb", "DateTimeAK", "DOY")]
CARI_no3 <- CARI_2021[c("NO3", "DateTimeAK", "DOY")]

ggplot(CARI_2021, aes(x = DateTimeAK, y = CARI_2021$NO3)) +
  geom_point()

# identifying gaps 
my_dat = data.frame(DateTimeAK = na.omit(CARI_no3$DateTimeAK))
my_dat$DateTimeAK = sort(my_dat$DateTimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, DateTimeAK[-1] - DateTimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$DateTimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_fDOM$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate 
SUNA_processed <- read_csv("~/Documents/DoD_2021/SUNA_data/from_internal_harddrive/processed/SUNA.processed.csv")

POKE_SUNA <- subset(SUNA_processed, site.ID == "POKE")
STRT_SUNA <- subset(SUNA_processed, site.ID == "STRT")
VAUL_SUNA <- subset(SUNA_processed, site.ID == "VAUL")
MOOS_SUNA <- subset(SUNA_processed, site.ID == "MOOS")
FRCH_SUNA <- subset(SUNA_processed, site.ID == "FRCH")

# POKE
POKE_NO3 <- POKE_SUNA[,-c(1:5,7:292) ]

# STRT
STRT_NO3 <- STRT_SUNA[,-c(1:5,7:292) ]

# VAUL
VAUL_NO3 <- VAUL_SUNA[,-c(1:5,7:292) ]

# MOOS
MOOS_NO3 <- MOOS_SUNA[,-c(1:5,7:292) ]

# FRCH
FRCH_NO3 <- FRCH_SUNA[,-c(1:5,7:292) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 43200 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# 2022 ####
EXO_processed <- read_csv("~/Documents/DoD_2022/EXO_data/from_internal_harddrive/processed/EXO.processed.csv")
CARI_2022 <- read.csv(here("processed_sensor_data", "2022", "NEON_Q_WaterQuality2022.csv"))

POKE_2022 <- subset(EXO_processed, site.ID == "POKE")
STRT_2022 <- subset(EXO_processed, site.ID == "STRT")
VAUL_2022 <- subset(EXO_processed, site.ID == "VAUL")
MOOS_2022 <- subset(EXO_processed, site.ID == "MOOS")
FRCH_2022 <- subset(EXO_processed, site.ID == "FRCH")

CARI_2022$DateTimeAK <- ymd_hms(CARI_2022$DateTimeAK)

# CARI_2021 <- CARI_2021 %>% # common window 
#   mutate(across(c(DateTimeAK), 
#                 ~ifelse(DateTimeAK >= "2021-06-12" & DateTimeAK <= "2021-09-27", NA, .)))

CARI_2022 <- CARI_2022[-c(13419:16640),]  # removing not within the common window
CARI_2022 <- CARI_2022[-c(1:3295),]  # removing not within the common window

POKE_2022$datetimeAK <- lubridate::round_date(POKE_2022$datetimeAK, "15 minutes")
POKE_2022 <- POKE_2022[!duplicated(POKE_2022$datetimeAK), ]

STRT_2022$datetimeAK <- lubridate::round_date(STRT_2022$datetimeAK, "15 minutes")
STRT_2022 <- STRT_2022[!duplicated(STRT_2022$datetimeAK), ]

VAUL_2022$datetimeAK <- lubridate::round_date(VAUL_2022$datetimeAK, "15 minutes")
VAUL_2022 <- VAUL_2022[!duplicated(VAUL_2022$datetimeAK), ]

MOOS_2022$datetimeAK <- lubridate::round_date(MOOS_2022$datetimeAK, "15 minutes")
MOOS_2022 <- MOOS_2022[!duplicated(MOOS_2022$datetimeAK), ]

FRCH_2022$datetimeAK <- lubridate::round_date(FRCH_2022$datetimeAK, "15 minutes")
FRCH_2022 <- FRCH_2022[!duplicated(FRCH_2022$datetimeAK), ]

#plot
ggplot(FRCH_2022, aes(x = datetimeAK, y = FRCH_2022$fDOM.QSU)) +
  geom_point()


# POKE
POKE_2022$DOY <- yday(POKE_2022$datetimeAK)
POKE_fDOM <- POKE_2022[,-c(1:6,8:22) ]
POKE_SPC <- POKE_2022[,-c(1:13,15:22) ]
POKE_turb <- POKE_2022[,-c(1:15, 17:22) ]

# STRT
STRT_2022$DOY <- yday(STRT_2022$datetimeAK)
STRT_fDOM <- STRT_2022[,-c(1:6,8:22) ]
STRT_SPC <- STRT_2022[,-c(1:13,15:22) ]
STRT_turb <- STRT_2022[,-c(1:15, 17:22) ]

# VAUL
VAUL_2022$DOY <- yday(VAUL_2022$datetimeAK)
VAUL_fDOM <- VAUL_2022[,-c(1:6,8:22) ]
VAUL_SPC <- VAUL_2022[,-c(1:13,15:22) ]
VAUL_turb <- VAUL_2022[,-c(1:15, 17:22) ]

#  MOOS
MOOS_2022$DOY <- yday(MOOS_2022$datetimeAK)
MOOS_fDOM <- MOOS_2022[,-c(1:6,8:22) ]
MOOS_SPC <- MOOS_2022[,-c(1:13,15:22) ]
MOOS_turb <- MOOS_2022[,-c(1:15, 17:22) ]

#  FRCH
FRCH_2022$DOY <- yday(FRCH_2022$datetimeAK)
FRCH_fDOM <- FRCH_2022[,-c(1:6,8:22) ]
FRCH_SPC <- FRCH_2022[,-c(1:13,15:22) ]
FRCH_turb <- FRCH_2022[,-c(1:15, 17:22) ]

#  CARI
CARI_2022$DOY <- yday(CARI_2022$DateTimeAK)
CARI_fDOM <- CARI_2022[c("fDOM", "DateTimeAK", "DOY")]
CARI_SPC <- CARI_2022[c("SPC", "DateTimeAK", "DOY")]
CARI_turb <- CARI_2022[c("Turb", "DateTimeAK", "DOY")]
CARI_no3 <- CARI_2022[c("NO3", "DateTimeAK", "DOY")]

ggplot(CARI_2022, aes(x = DateTimeAK, y = CARI_2022$fDOM)) +
  geom_point()

# identifying gaps 
my_dat = data.frame(DateTimeAK = na.omit(CARI_no3$DateTimeAK))
my_dat$DateTimeAK = sort(my_dat$DateTimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, DateTimeAK[-1] - DateTimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$DateTimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]


# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(FRCH_fDOM$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# nitrate 
FRCH_SUNA <- read_csv("~/Documents/DoD_2022/SUNA_processed/FRCH_SUNA_means_detailed_clean.csv")
MOOS_SUNA <- read_csv("~/Documents/DoD_2022/SUNA_processed/MOOS_SUNA_means_detailed_clean.csv")
POKE_SUNA <- read_csv("~/Documents/DoD_2022/SUNA_processed/POKE_SUNA_means_detailed_clean.csv")
STRT_SUNA <- read_csv("~/Documents/DoD_2022/SUNA_processed/STRT_SUNA_means_detailed_clean.csv")
VAUL_SUNA <- read_csv("~/Documents/DoD_2022/SUNA_processed/VAUL_SUNA_means_detailed_clean.csv")

ggplot(VAUL_SUNA, aes(x = datetimeAK, y = VAUL_SUNA$nitrateuM.mn)) +
  geom_point()


# POKE
POKE_NO3 <- POKE_SUNA[,-c(3:27) ]

# STRT
STRT_NO3 <- STRT_SUNA[,-c(3:27) ]

# VAUL
VAUL_NO3 <- VAUL_SUNA[,-c(3:27) ]

# MOOS
MOOS_NO3 <- MOOS_SUNA[,-c(3:27) ]

# FRCH
FRCH_NO3 <- FRCH_SUNA[,-c(3:27) ]

# identifying gaps 
my_dat = data.frame(datetimeAK = na.omit(VAUL_NO3$datetimeAK))
my_dat$datetimeAK = sort(my_dat$datetimeAK, decreasing = F)
my_dat$gap <- c(NA, with(my_dat, datetimeAK[-1] - datetimeAK[-nrow(my_dat)]))
gap_threshold <- 720 # 12 hours in minutes
my_dat$over_thresh <- my_dat$gap > gap_threshold
range(my_dat$datetimeAK, na.rm=T)
my_dat[which(my_dat$over_thresh==T)-1,]
my_dat[my_dat$over_thresh==T,]

# Duration 
AMC <- read.csv("Output_from_analysis", "08_Catchment_characteristics", "Antecedent_HI_BETA_Catchment.csv")

mean_duration <-  AMC %>% 
  group_by(storm.ID, site.ID, year) %>% 
  dplyr::summarise(Duration = mean(TOTAL.TIME))

mean_duration <- mean_duration %>%
  mutate(across(c(Duration), 
                ~ifelse(Duration > 500, NA, .)))
range(mean_duration$Duration, na.rm = TRUE)
mean(mean_duration$Duration, na.rm = TRUE) #65.40994
65.40994/24 # 2.725414

sd(mean_duration$Duration, na.rm = TRUE) # 53.486
53.486/24



mean_duration_site <- AMC %>% 
  group_by(site.ID) %>% 
  dplyr::summarise(Duration = mean(TOTAL.TIME, na.rm = TRUE))


##################################
### MEAN SOLUTE CONCENTRATIONS ###
##################################
chem_2015 <- read.csv(here("processed_sensor_data", "2015", "SUNA.EXO.int.corr.lab_2015.csv"))
chem_2018 <- read.csv(here("processed_sensor_data", "2018", "SUNA.EXO.int.corr.lab_2018.csv"))
chem_2019 <- read.csv(here("processed_sensor_data", "2019", "SUNA.EXO.int.corr.lab_2019.csv"))
chem_2020 <- read.csv(here("processed_sensor_data", "2020", "SUNA.EXO.int.corr.lab_2020.csv"))
chem_2021 <- read.csv(here("processed_sensor_data", "2021", "SUNA.EXO.int.corr.lab_2021.csv"))
chem_2022 <- read.csv(here("processed_sensor_data", "2022", "SUNA.EXO.int.corr.lab_2022.csv"))

#
chem_2015 <- chem_2015[c("datetimeAK", "Site", "fDOM.QSU.adj.T.turb.int", "SpCond.uScm.adj",
                         "Turbidity.FNU.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]

chem_2015$datetimeAK <- ymd_hms(chem_2015$datetimeAK) # converting character to datetime
chem_2015$datetimeAK <- force_tz(chem_2015$datetimeAK, "America/Anchorage") # converting character to datetime

names(chem_2015) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")
chem_2015$year <- format(chem_2015$datetimeAK, format = "%Y")

#
chem_2018 <- chem_2018[c("datetimeAK", "site.ID", "fDOM.QSU.mn.adj", 
                         "SpCond.uScm.mn.adj", "Turbidity.FNU.mn.adj",
                         "nitrateuM.mn", "abs254.adj.mn")] # reading in the only columns I want

chem_2018$datetimeAK <- ymd_hms(chem_2018$datetimeAK) # converting character to datetime
chem_2018$datetimeAK <- force_tz(chem_2018$datetimeAK, "America/Anchorage") # converting character to datetime

names(chem_2018) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")
chem_2018$year <- format(chem_2018$datetimeAK, format = "%Y")

#
chem_2019 <- chem_2019[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2019$datetimeAK <- ymd_hms(chem_2019$datetimeAK)
chem_2019$datetimeAK <- force_tz(chem_2019$datetimeAK, "America/Anchorage") # converting character to datetime
names(chem_2019) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")
chem_2019$year <- format(chem_2019$datetimeAK, format = "%Y")

#
chem_2020 <- chem_2020[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2020$datetimeAK <- ymd_hms(chem_2020$datetimeAK)
chem_2020$datetimeAK <- force_tz(chem_2020$datetimeAK, "America/Anchorage") # converting character to datetime
names(chem_2020) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")
chem_2020$year <- format(chem_2020$datetimeAK, format = "%Y")

#
chem_2021 <- chem_2021[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2021$datetimeAK <- ymd_hms(chem_2021$datetimeAK)
chem_2021$datetimeAK <- force_tz(chem_2021$datetimeAK, "America/Anchorage") # converting character to datetime
names(chem_2021) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")
chem_2021$year <- format(chem_2021$datetimeAK, format = "%Y")

#
chem_2022 <- chem_2022[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2022$datetimeAK <- ymd_hms(chem_2022$datetimeAK)
chem_2022$datetimeAK <- force_tz(chem_2022$datetimeAK, "America/Anchorage") # converting character to datetime
names(chem_2022) <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "ABS_254")
chem_2022$year <- format(chem_2022$datetimeAK, format = "%Y")


DOD_chem <- rbind(chem_2015, chem_2018, chem_2019, chem_2020, chem_2021, chem_2022)
DOD_chem$julian <- yday(DOD_chem$datetimeAK)
DOD_chem$day <- as.character(DOD_chem$datetimeAK)


# read in Caribou data 
CARI_2018 <- read.csv(here("processed_sensor_data", "2018", "NEON_Q_WaterQuality2018.csv"))
CARI_2018 <-  subset(CARI_2018, select=-c(site.ID.y))
names(CARI_2018)[names(CARI_2018) == 'site.ID.x'] <- 'site.ID'
CARI_2018$site.ID <- "CARI"

CARI_2019 <- read.csv(here("processed_sensor_data", "2019", "NEON_Q_WaterQuality2019.csv"))
CARI_2019 <-  subset(CARI_2019, select=-c(site.ID.y))
names(CARI_2019)[names(CARI_2019) == 'site.ID.x'] <- 'site.ID'
CARI_2019$site.ID <- "CARI"

CARI_2020 <- read.csv(here("processed_sensor_data", "2020", "NEON_Q_WaterQuality2020.csv"))
CARI_2020 <-  subset(CARI_2020, select=-c(site.ID.y))
names(CARI_2020)[names(CARI_2020) == 'site.ID.x'] <- 'site.ID'
CARI_2020$site.ID <- "CARI"

CARI_2021 <- read.csv(here("processed_sensor_data", "2021", "NEON_Q_WaterQuality2021.csv"))
CARI_2021 <-  subset(CARI_2021, select=-c(site.ID.y))
names(CARI_2021)[names(CARI_2021) == 'site.ID.x'] <- 'site.ID'
CARI_2021$site.ID <- "CARI"

CARI_2022 <- read.csv(here("processed_sensor_data", "2022", "NEON_Q_WaterQuality2022.csv"))
CARI_2022 <-  subset(CARI_2022, select=-c(site.ID.y))
names(CARI_2022)[names(CARI_2022) == 'site.ID.x'] <- 'site.ID'
CARI_2022$site.ID <- "CARI"


CARI_2018$year <- "2018"
CARI_2019$year <- "2019"
CARI_2020$year <- "2020"
CARI_2021$year <- "2021"
CARI_2022$year <- "2022"

CARI_chem <- rbind(CARI_2018, CARI_2019, CARI_2020, CARI_2021, CARI_2022)
CARI_chem$day <- as.character(CARI_chem$DateTimeAK)
CARI_chem <- CARI_chem[c("DateTimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "Discharge",
                         "day", "year")] # reorganizing column headers


colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", "NO3", "Q", "day", "year")
names(CARI_chem)<- colNames # renaming columns 


# merge CARI and DOD sites 
CARI_chem$datetimeAK <- ymd_hms(CARI_chem$datetimeAK)
DOD_chem$datetimeAK <- ymd_hms(DOD_chem$datetimeAK)

DOD_chem <- full_join(DOD_chem, CARI_chem, by = c("datetimeAK", "site.ID", "fDOM",
                                                  "SPC", "Turb", "NO3", "year",
                                                  "day"))
DOD_chem <- DOD_chem[order(DOD_chem$datetimeAK),]
DOD_chem$julian <- yday(DOD_chem$datetimeAK)

DOD_chem %>% filter(julian < 305) %>%
             ggplot(aes(x = julian, y = fDOM, group = year)) +
               geom_point(aes(color = year)) +
               facet_wrap(~site.ID, scales = "free_y")

# check for outliers
#DOD_chem <- DOD_chem %>% mutate(across(c(Turb), 
#                ~ifelse(Turb > 1250, NA, .)))

#DOD_chem <- DOD_chem %>%
#  mutate(across(c(NO3), 
#                ~ifelse(site.ID == "STRT" & year == 2019 & NO3 > 40, NA, .)))

#DOD_chem <- DOD_chem %>%
#  mutate(across(c(NO3), 
#                ~ifelse(site.ID == "VAUL" & year == 2019 & NO3 < 2, NA, .)))

DOD_chem <- DOD_chem %>%
  mutate(across(c(NO3), 
                ~ifelse(site.ID == "MOOS" & year == 2020 & NO3 > 40, NA, .)))

DOD_chem <- DOD_chem %>%
  mutate(across(c(NO3), 
                ~ifelse(site.ID == "STRT" & year == 2020 & NO3 < 10, NA, .)))

#DOD_chem <- DOD_chem %>%
#  mutate(across(c(fDOM), 
#                ~ifelse(year == 2021 & fDOM < 1, NA, .)))

#DOD_chem <- DOD_chem[-c(409859:419367), ] # removing the bottom of the df that has no datetime associated with chems

# plotting to make sure this merged properly
chem.long <- DOD_chem %>%
     pivot_longer(
       cols = fDOM:ABS_254,
       names_to = "response_var",
       values_to = "concentration",
       values_drop_na = TRUE) # converting to a long format so each response_var is within a single column

ggplot(chem.long, aes(x = julian, y = concentration, color = site.ID)) +
  geom_point(size = 0.5) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_grid(response_var~year, scales = "free") +
  theme_classic()

# Filtering by year to compare concentrations across years 

chem_2015 <- subset(DOD_chem, year == "2015")
chem_2018 <- subset(DOD_chem, year == "2018")
chem_2019 <- subset(DOD_chem, year == "2019")
chem_2020 <- subset(DOD_chem, year == "2020")
chem_2021 <- subset(DOD_chem, year == "2021")
chem_2022 <- subset(DOD_chem, year == "2022")

# start and end dates for 2022 # 
# FRCH <- subset(DOD_2022, site.ID == "FRCH")
# MOOS <- subset(DOD_2022, site.ID == "MOOS")
# POKE <- subset(DOD_2022, site.ID == "POKE")
# VAUL <- subset(DOD_2022, site.ID == "VAUL")
# STRT <- subset(DOD_2022, site.ID == "STRT")


# The common window for time since peak chena is day 35-142 so the dates are as follows:
# these dates are in the Summart_statistics csv summary file 
# 2015 TPC: 5/12
# 2018 TPC: 5/23
# 2019 TPC: 5/12
# 2020 TPC: 5/13
# 2021 TPC: 5/8
# 2022 TPC: 5/9

chem_2015 <- subset(chem_2015, datetimeAK > "2015-06-27" & datetimeAK < "2015-10-11")
chem_2018 <- subset(chem_2018, datetimeAK > "2018-06-27" & datetimeAK < "2018-10-12")
chem_2019 <- subset(chem_2019, datetimeAK > "2019-06-16" & datetimeAK < "2019-10-01")
chem_2020 <- subset(chem_2020, datetimeAK > "2020-06-17" & datetimeAK < "2020-09-30")
chem_2021 <- subset(chem_2021, datetimeAK > "2021-06-12" & datetimeAK < "2021-09-27")
chem_2022 <- subset(chem_2022, datetimeAK > "2022-06-13" & datetimeAK < "2022-09-28")

# make a julian day columnn:
chem_2015$julian <- yday(chem_2015$datetimeAK)
chem_2015$TSC <- chem_2015$julian-142 # TSC column
chem_2015$day <- as.Date(chem_2015$datetimeAK)

chem_2018$julian <- yday(chem_2018$datetimeAK)
chem_2018$TSC <- chem_2018$julian-143 # TSC column
chem_2018$day <- as.Date(chem_2018$datetimeAK)

chem_2019$julian <- yday(chem_2019$datetimeAK)
chem_2019$TSC <- chem_2019$julian-132 # TSC column
chem_2019$day <- as.Date(chem_2019$datetimeAK)

chem_2020$julian <- yday(chem_2020$datetimeAK)
chem_2020$TSC <- chem_2020$julian-134 # TSC column
chem_2020$day <- as.Date(chem_2020$datetimeAK)

chem_2021$julian <- yday(chem_2021$datetimeAK)
chem_2021$TSC <- chem_2021$julian-128 # TSC column
chem_2021$day <- as.Date(chem_2021$datetimeAK)

chem_2022$julian <- yday(chem_2022$datetimeAK)
chem_2022$TSC <- chem_2022$julian-129 # TSC column
chem_2022$day <- as.Date(chem_2022$datetimeAK)

# combine them to be able to plot it 
similar_chem_year <- rbind(chem_2015, chem_2018, chem_2019, chem_2020, chem_2021, chem_2022)

mean_daily <- similar_chem_year %>% 
  group_by(day, site.ID, year) %>% 
  summarise(dailyfDOM = mean(fDOM, na.rm = TRUE),
            dailyNO3 = mean(NO3, na.rm = TRUE),
            dailySPC = mean(SPC, na.rm = TRUE),
            dailyTurb = mean(Turb, na.rm = TRUE),
            dailyABS = mean(ABS_254, na.rm = TRUE),
            julian = as.numeric(julian))

mean_daily <- mean_daily %>%
  group_by(day, site.ID) %>%
  slice(which.min(day)) # making sure I just take the first value for the daily mean since it gives the same mean value for each 15minute interval so I just want one

mean_daily_long <- mean_daily %>%
  pivot_longer(
    cols = starts_with("daily"),
    names_to = "response_var",
    values_to = "concentration"
  ) # converting to a long format so each response_var is within a single column

ggplot(mean_daily_long, aes(x = julian, y = concentration, color = site.ID)) +
  geom_point(size = 0.35) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_grid(response_var~year, scales = "free")



CARI_year <- subset(similar_chem_year, site.ID == "CARI")
FRCH_year <- subset(similar_chem_year, site.ID == "FRCH")
MOOS_year <- subset(similar_chem_year, site.ID == "MOOS")
POKE_year <- subset(similar_chem_year, site.ID == "POKE")
STRT_year <- subset(similar_chem_year, site.ID == "STRT")
VAUL_year <- subset(similar_chem_year, site.ID == "VAUL")
# 
# similar_chem_year$day <- as.Date(similar_chem_year$datetimeAK)
# 
# similar_chem_year[, 6][similar_chem_year[, 6] < 10] <- NA # there are values of 0 for nitrate
#   # in VAUL data starting in August that are not accurate so I am setting them to NA
# # Mean daily concentration 
# mean_daily <- similar_chem_year %>% 
#   group_by(day, site.ID, year) %>% 
#   summarise(dailyfDOM = mean(fDOM, na.rm = TRUE),
#             dailyNO3 = mean(NO3, na.rm = TRUE),
#             dailySPC = mean(SPC, na.rm = TRUE),
#             dailyTurb = mean(Turb, na.rm = TRUE))
# 
# mean_daily$julian <- yday(mean_daily$day)
# mean_daily$TSC <- NA
# mean_daily[c(1:290), 9] <- mean_daily[c(1:290), 8] - 143 # 2018 
# mean_daily[c(291:928), 9] <- mean_daily[c(291:928), 8] - 132 # 2019
# mean_daily[c(929:1541), 9] <- mean_daily[c(929:1541), 8] - 134 # 2020
# mean_daily[c(1542:2074), 9] <- mean_daily[c(1542:2074), 8] - 128 # 2021

# mean_daily <- mean_daily[-2130, ] # last row
write.csv(mean_daily, here("Output_from_analysis", "08_Catchment_characteristics", "mean_daily.csv"))
# plot
# across all years ####
# NO3
ggplot(mean_daily, aes(x = site.ID, y = dailyNO3, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()
# fDOM
ggplot(mean_daily, aes(x = site.ID, y = dailyfDOM, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()

# SPC
ggplot(mean_daily, aes(x = site.ID, y = dailySPC, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()

# turb
ggplot(mean_daily, aes(x = site.ID, y = dailyTurb, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  theme_classic()

# by year 
# NO3
ggplot(mean_daily, aes(x = site.ID, y = dailyNO3, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# fDOM
ggplot(mean_daily, aes(x = site.ID, y = dailyfDOM, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# SPC
ggplot(mean_daily, aes(x = site.ID, y = dailySPC, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# turb
ggplot(mean_daily, aes(x = site.ID, y = dailyTurb, fill = site.ID)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

## Time Series ##
# NO3-
ggplot(mean_daily, aes(x = julian, y = dailyNO3, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# fDOM
ggplot(mean_daily, aes(x = julian, y = dailyfDOM, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# SPC
ggplot(mean_daily, aes(x = julian, y = dailySPC, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# turb
ggplot(mean_daily, aes(x = julian, y = dailyTurb, col = site.ID)) +
  geom_line() +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  facet_wrap(~year) +
  theme_classic()

# Output tables for summary tables #
write.table(mean_daily, file = "chem_year_similar.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.cari <- CARI_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))

write.table(year.cari, file = "cari.csv", sep = ",", col.names = NA,
            qmethod = "double")


year.frch <- FRCH_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))

write.table(year.frch, file = "frch.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.moos <- MOOS_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.moos, file = "moos.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.poke <- POKE_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.poke, file = "poke.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.vaul <- VAUL_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.vaul, file = "vaul.csv", sep = ",", col.names = NA,
            qmethod = "double")

year.strt <- STRT_year %>% 
  group_by(year) %>% 
  summarise(medianfDOM = median(fDOM, na.rm = TRUE),
            medianNO3 = median(NO3, na.rm = TRUE),
            medianSPC = median(SPC, na.rm = TRUE),
            medianTURB = median(Turb, na.rm = TRUE))
write.table(year.strt, file = "strt.csv", sep = ",", col.names = NA,
            qmethod = "double")

#########################################
### ANOVA: Comparing catchments*years ###
#########################################
###***TKH: Are we using these catchment*year comparisons? These analysis are treating each daily value as independent. Would need to account for temporal autocorrelation.

#fDOM

fDOM <- aov(dailyfDOM ~ site.ID*year, data = mean_daily)

summary(fDOM) # everything is significant 

TukeyHSD(fDOM, which = "site.ID") # this shows that STRT and CARI are the only ones not sig
    # different from each other 
plot(fDOM)

#NO3
NO3 <- aov(dailyNO3 ~ site.ID*year, data = mean_daily)

summary(NO3) # Site and year and Site*year is significant 


TukeyHSD(NO3, which = "site.ID")
# this shows that POKE and CARI are not significantly different neither is STRT/MOOS
plot(NO3)

# SPC
SPC <- aov(dailySPC ~ site.ID*year, data = mean_daily)

summary(SPC) # Site and year and Site*year is significant 


TukeyHSD(SPC, which = "site.ID")
# this shows that POKE/MOOS are not significantly different neither is STRT/MOOS
plot(SPC)

# turb
turb <- aov(dailyTurb ~ site.ID*year, data = mean_daily)

summary(turb) # Site and year and Site*year is significant 


TukeyHSD(turb, which = "site.ID")
# this shows that POKE/CARI are not significantly different neither is MOOS/FRCH, STRT/CARI,
      #VAUL/FRCH, VAUL/MOOS,STRT/POKE
plot(turb)


#mean_daily <- na.omit(mean_daily)


############################### TIME SERIES PLOT - THESIS ###############################################
chem_2015 <- read.csv(here("processed_sensor_data", "2015", "SUNA.EXO.int.corr.lab_2015.csv"))
chem_2018 <- read.csv(here("processed_sensor_data", "2018", "SUNA.EXO.int.corr.lab_2018.csv"))
chem_2019 <- read.csv(here("processed_sensor_data", "2019", "SUNA.EXO.int.corr.lab_2019.csv"))
chem_2020 <- read.csv(here("processed_sensor_data", "2020", "SUNA.EXO.int.corr.lab_2020.csv"))
chem_2021 <- read.csv(here("processed_sensor_data", "2021", "SUNA.EXO.int.corr.lab_2021.csv"))
chem_2022 <- read.csv(here("processed_sensor_data", "2022", "SUNA.EXO.int.corr.lab_2022.csv"))

chem_2018 <- chem_2018[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2018$datetimeAK <- ymd_hms(chem_2018$datetimeAK)
chem_2018$year <- format(chem_2018$datetimeAK, format = "%Y")

chem_2019 <- chem_2019[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2019$datetimeAK <- ymd_hms(chem_2019$datetimeAK)
chem_2019$year <- format(chem_2019$datetimeAK, format = "%Y")

chem_2020 <- chem_2020[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2020$datetimeAK <- ymd_hms(chem_2020$datetimeAK)
chem_2020$year <- format(chem_2020$datetimeAK, format = "%Y")

chem_2021 <- chem_2021[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2021$datetimeAK <- ymd_hms(chem_2021$datetimeAK)
chem_2021$year <- format(chem_2021$datetimeAK, format = "%Y")

chem_2022 <- chem_2022[c("datetimeAK", "site.ID", "fDOM.QSU.T.turb.col", "SpCond.uScm.mn.adj",
                         "Turbidity.FNU.mn.adj", "nitrateuM.mn.lab", "abs254.adj.mn")]
chem_2022$datetimeAK <- ymd_hms(chem_2022$datetimeAK)
chem_2022$year <- format(chem_2022$datetimeAK, format = "%Y")


DOD_chem <- rbind(chem_2018, chem_2019, chem_2020, chem_2021, chem_2022)
DOD_chem$julian <- yday(DOD_chem$datetimeAK)
DOD_chem$day <- as.Date(DOD_chem$datetimeAK)

names(DOD_chem) <- c("datetimeAK", "site.ID", "fDOM", "SPC",
                     "Turb", "NO3", "ABS_254", "year", "julian", "day")

# MERGE IN DISCHARGE # 
Q_daily_2015 <- read_csv(here("processed_sensor_data", "2015", "Q.daily.2015.csv"))
Q_daily_2015 <- Q_daily_2015 %>% 
  dplyr::select(-"...1")
Q_daily_2018 <- read_csv(here("processed_sensor_data", "2018", "Q.daily.2018.csv"))
Q_daily_2019 <- read_csv(here("processed_sensor_data", "2019", "Q.daily.2019.csv"))
Q_daily_2020 <- read_csv(here("processed_sensor_data", "2020", "Q.daily.2020.csv"))
Q_daily_2021 <- read_csv(here("processed_sensor_data", "2021", "Q.daily.2021.csv"))
Q_daily_2022 <- read_csv(here("processed_sensor_data", "2022", "Q.daily.2022.csv"))

Q_DOD <- rbind(Q_daily_2018, Q_daily_2019, Q_daily_2020, Q_daily_2021, Q_daily_2022)
names(Q_DOD) <- c("site.ID", "day", "dailyQ")

chem_Q <- full_join(Q_DOD, DOD_chem, by = c("site.ID", "day"))

chem_Q <- chem_Q[c("site.ID", "datetimeAK", "day",
                   "fDOM", "SPC", "Turb", "NO3", "ABS_254", "dailyQ",
                   "year", "julian")]

mean_daily_DOD <- chem_Q %>% 
  group_by(day, site.ID, year) %>% 
  summarise(dailyfDOM = mean(fDOM, na.rm = TRUE),
            dailyNO3 = mean(NO3, na.rm = TRUE),
            dailySPC = mean(SPC, na.rm = TRUE),
            dailyTurb = mean(Turb, na.rm = TRUE),
            dailyQ = as.numeric(dailyQ, na.rm = TRUE),
            julian = as.numeric(julian),
            year = as.character(year))

mean_daily_DOD <- mean_daily_DOD %>%
  group_by(day, site.ID) %>%
  slice(which.min(day)) # making sure I just take the first value for the daily mean since it gives the same mean value for each 15minute interval so I just want one


# CARI # 
CARI_2018 <- read.csv(here("processed_sensor_data", "2018", "NEON_Q_WaterQuality2018.csv"))
CARI_2019 <- read.csv(here("processed_sensor_data", "2019", "NEON_Q_WaterQuality2019.csv"))
CARI_2020 <- read.csv(here("processed_sensor_data", "2020", "NEON_Q_WaterQuality2020.csv"))
CARI_2021 <- read.csv(here("processed_sensor_data", "2021", "NEON_Q_WaterQuality2021.csv"))
CARI_2022 <- read.csv(here("processed_sensor_data", "2022", "NEON_Q_WaterQuality2022.csv"))

CARI_total <- rbind(CARI_2018, CARI_2019, CARI_2020, CARI_2021, CARI_2022)

CARI_total$DateTimeAK <- ymd_hms(CARI_total$DateTimeAK)
CARI_total$julian <- yday(CARI_total$DateTimeAK)
CARI_total$day <- as.Date(CARI_total$DateTimeAK)
CARI_total$year <- format(CARI_total$DateTimeAK,'%Y')

CARI_total <- CARI_total[c("site.ID.x", "DateTimeAK", "day", 
                           "fDOM", "SPC", "Turb", "NO3", "Discharge", "year", "julian")]

names(CARI_total) <- c("site.ID", "datetimeAK", "day",
                       "fDOM", "SPC", "Turb", "NO3", "dailyQ",
                       "year", "julian")


mean_daily_CARI <- CARI_total %>% 
  group_by(day, site.ID, year) %>% 
  summarise(dailyfDOM = mean(fDOM, na.rm = TRUE),
            dailyNO3 = mean(NO3, na.rm = TRUE),
            dailySPC = mean(SPC, na.rm = TRUE),
            dailyTurb = mean(Turb, na.rm = TRUE),
            dailyQ = as.numeric(dailyQ, na.rm = TRUE),
            julian = as.numeric(julian),
            year = as.character(year))

mean_daily_CARI <- mean_daily_CARI %>%
  group_by(day, site.ID) %>%
  slice(which.min(day)) # making sure I just take the first value for the daily mean since it gives the same mean value for each 15minute interval so I just want one

mean_daily_CARI$site.ID <- "CARI"

mean_daily_total <- full_join(mean_daily_DOD, mean_daily_CARI)
mean_daily_total <- mean_daily_total[order(mean_daily_total$day),]

mean_daily_total$julian <- yday(mean_daily_total$day)
mean_daily_total$year <- format(mean_daily_total$day,'%Y')


chem_total_long <- mean_daily_total %>%
  pivot_longer(
    cols = dailyfDOM:dailyQ,
    names_to = "response_var",
    values_to = "concentration",
    values_drop_na = TRUE
  ) 


chem_total_long$response_var = factor(chem_total_long$response_var, 
                                      levels=c('dailyQ', 'dailyNO3', 'dailyfDOM', 'dailySPC', 'dailyTurb'))

ggplot(chem_total_long, aes(x = julian, y = concentration, color = site.ID)) +
  geom_line(size = 0.5) +
  scale_color_manual(values=c("#3288BD", "#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A", "#3288BD")) +
  facet_grid(response_var~year, scales = "free") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        axis.text.x=element_text(size=20, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank())






###########################################################################################

############################### Storm Precip/Total Q for each storm ###############################################
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
library(wesanderson)


# Load in Antecedent moisture conditions dataframe
AMC <- read.csv(here("Output_from_analysis", "07_Combine_HI_BETA_FI", "antecedent_HI_FI_AllYears.csv"))

AMC <- AMC[c("Hyst_index","HI_ymin", "HI_ymax", "site.ID", "storm.ID", "month.x", "day.x",
             "response_var", "Flush_index","FI_ymin", "FI_ymax", "year", 
             "Parameter", "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", "t", 
             "df", "p", "precip", "temp", "precip.week", "precip.month", 
             "ThreeMonth", "temp.week", "TOTAL.TIME", "Intensity", "doy", "burn", "pf", 
             "date", "TimeSinceChena")] # selecting the columns that I want

colNames <- c("Hyst_index", "HI_ymin", "HI_ymax", "site.ID", "storm.ID", "month", 
              "day", "response_var", "Flush_index", "FI_ymin", "FI_ymax", "year", 
              "Parameter", "Beta_index", "SE", "CI", "Beta_ymin", "Beta_ymax", "t", 
              "df", "p", "StormPrecip", "StormTemp", "PrecipWeek", "PrecipMonth", 
              "ThreeMonth", "TempWeek", "Duration", "Intensity", "doy", "burn", "PF", 
              "date", "TimeSinceChena")

names(AMC)<- colNames # renaming columns

AMC <- AMC %>% 
  dplyr::mutate(across(c(PF),
                       ~ifelse(site.ID == "STRT" | site.ID == "VAUL", "High", "Moderate")))

AMC <- AMC %>% 
  dplyr::mutate(across(c(burn),
                       ~ifelse(site.ID == "STRT" | site.ID == "MOOS" | site.ID == "POKE", "Burned", "Unburned")))


# Box-plots of storm ppt by year # 
# 2019 - 2022 # 

AMC <- AMC %>% 
  subset(year == "2015" | year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
# only Poker
AMC.POKE <- AMC %>% 
  subset(site.ID == "POKE")
AMC.POKE <- AMC.POKE %>% 
  subset(month < 10 & month > 5)

ggplot(AMC.POKE, aes(x = as.factor(year), y = StormPrecip, fill = as.factor(year))) + 
  geom_boxplot() +
  xlab("") +
  ylab("Precipitation per storm event (mm)") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=30), 
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 32)) +
  annotate("text", x = 1, y=60, size = 7.5, label= "n = 11", col = "#1B9E77") +
  annotate("text", x = 2, y=60, size = 7.5, label= "n = 18", col = "#D95F02") +
  annotate("text", x = 3, y=60, size = 7.5, label= "n = 9", col = "#7570B3") +
  annotate("text", x = 4, y=60, size = 7.5, label= "n = 4", col = "#E7298A")


display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = "Dark2")

ggplot(AMC, aes(x = as.factor(year), y = StormPrecip, fill = as.factor(year))) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Precipitation per storm event (mm)") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 22)) +
  annotate("text", x = 1, y=60, size = 7.5, label= "n = 13", col = "#1B9E77") +
  annotate("text", x = 2, y=60, size = 7.5, label= "n = 34", col = "#D95F02") +
  annotate("text", x = 3, y=60, size = 7.5, label= "n = 65", col = "#7570B3") +
  annotate("text", x = 4, y=60, size = 7.5, label= "n = 80", col = "#E7298A") +
  annotate("text", x = 5, y=60, size = 7.5, label= "n = 43", col = "#66A61E") +
  annotate("text", x = 6, y=60, size = 7.5, label= "n = 26", col = "#E6AB02")

ggplot(AMC, aes(x = as.factor(year), y = StormPrecip, fill = as.factor(year))) + 
  geom_boxplot() +
  xlab("Year") +
  ylab("Precipitation per storm event (mm)") +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(trans='log10') +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=15), 
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 18))



# cumulative Q for storms each site*year # 
### 2018 ####
### FRCH ###
FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2018/All_Sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE) # reading in individual storms by site 

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 142),
                          rep("storm10", 689),
                          rep("storm11a", 91),
                          rep("storm11b", 256),
                          rep("storm2a", 208),
                          rep("storm2b", 156),
                          rep("storm3", 196),
                          rep("storm4a", 88),
                          rep("storm4b", 153),
                          rep("storm5", 331),
                          rep("storm6", 303),
                          rep("storm7", 129),
                          rep("storm8a", 79),
                          rep("storm8b", 95),
                          rep("storm9", 99)) # naming each storm by the number of storm 

FRCH_storms <- FRCH_storms[c("DateTime", "Site", "fDOM.QSU", "SpCond.uScm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]


colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(FRCH_storms)<- colNames # renaming columns

### MOOS ###
# MOOS # 
MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2018/All_Sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

MOOS_storms<-do.call("rbind", lapply(MOOSstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

MOOS_storms$storm.num = c(rep("storm1", 58),
                          rep("storm10", 432),
                          rep("storm11a", 90),
                          rep("storm11b", 9),
                          rep("storm12", 294),
                          rep("storm2a", 74),
                          rep("storm2b", 146),
                          rep("storm2c", 182),
                          rep("storm3", 198),
                          
                          rep("storm5", 282),
                          rep("storm6", 333),
                          rep("storm7", 176),
                          rep("storm8a", 78),
                          rep("storm8b", 100),
                          rep("storm9", 106))

MOOS_storms <- MOOS_storms[c("DateTime", "Site", "fDOM.QSU", "SpCond.uScm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]


colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(MOOS_storms)<- colNames # renaming columns

# merge 2018 # 
Q.sum.2018 <- rbind(FRCH_storms, MOOS_storms)
Q.sum.2018$year <- 2018


### 2019 ####
### FRCH ###
FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2019/All_Sites/", 
                                  recursive=F, 
                                  pattern="FRCH", 
                                  full.names=TRUE)

FRCH_storms<-do.call("rbind", lapply(FRCHstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

FRCH_storms$storm.num = c(rep("storm1", 993),
                          rep("storm10a", 121),
                          rep("storm10b", 95),
                          rep("storm10c", 207),
                          rep("storm11", 479),
                          rep("storm12a", 183),
                          rep("storm12b", 67),
                          rep("storm12c", 511),
                          rep("storm12d", 99),
                          rep("storm12e", 127),
                          rep("storm13", 391),
                          rep("storm14", 631),
                          rep("storm2", 165),
                          rep("storm3", 201),
                          rep("storm4", 193),
                          rep("storm5", 133),
                          rep("storm6", 289),
                          rep("storm7", 133),
                          rep("storm8", 105),
                          rep("storm9a", 61),
                          rep("storm9b", 149))

FRCH_storms <- FRCH_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(FRCH_storms)<- colNames # renaming columns

### MOOS ###
MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2019/All_Sites/", 
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
                          rep("storm6a", 114),
                          rep("storm6b", 95),
                          rep("storm6c", 223),
                          rep("storm6d", 479),
                          rep("storm7a", 166),
                          rep("storm7b", 84),
                          rep("storm7c", 430),
                          rep("storm8", 174),
                          rep("storm9", 530))

MOOS_storms <- MOOS_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(MOOS_storms)<- colNames # renaming columns

### POKE ###
POKEstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2019/All_Sites/", 
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
                          rep("storm5a", 87),
                          rep("storm5b", 239),
                          rep("storm5c", 111),
                          rep("storm5d", 99),
                          rep("storm6a", 51),
                          rep("storm6b", 231),
                          rep("storm7", 235),
                          rep("storm8", 95),
                          rep("storm9", 211))

POKE_storms <- POKE_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(POKE_storms)<- colNames # renaming columns

### VAUL ###
VAULstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2019/All_Sites/", 
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
                          rep("storm4a", 83),
                          rep("storm4b", 211),
                          rep("storm4c", 707),
                          rep("storm5", 275),
                          rep("storm6", 263),
                          rep("storm7", 107),
                          rep("storm8a", 167),
                          rep("storm8b", 223),
                          rep("storm8c", 479))

VAUL_storms <- VAUL_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(VAUL_storms)<- colNames # renaming columns

### STRT ###
STRTstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2019/All_Sites/", 
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
                          rep("storm7", 246),
                          rep("storm7b", 266),
                          rep("storm7c", 258))

STRT_storms <- STRT_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(STRT_storms)<- colNames # renaming columns

### CARI ###
CARIstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2019/All_Sites/", 
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
                          rep("storm3", 104),
                          rep("storm4", 147),
                          rep("storm5", 135),
                          rep("storm6a", 83),
                          rep("storm6b", 235),
                          rep("storm6c", 465),
                          rep("storm6d", 135),
                          rep("storm7a", 51),
                          rep("storm7b", 219),
                          rep("storm8", 267))

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "Discharge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "MeanDischarge", "storm.num")

names(CARI_storms)<- colNames # renaming columns

CARI_storms$ABS_254 <- NA

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "Discharge", "storm.num")]



# merge 2019 # 
Q.sum.2019 <- rbind(FRCH_storms, MOOS_storms,
                    POKE_storms, VAUL_storms,
                    STRT_storms, CARI_storms)

Q.sum.2019$year <- 2019



### 2020 ####
### FRCH ###
FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2020/All_Sites/", 
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
                          rep("storm10b", 439),
                          rep("storm11", 91),
                          rep("storm12", 67),
                          rep("storm13", 211),
                          rep("storm2", 123),
                          rep("storm3a", 163),
                          rep("storm3b", 435),
                          rep("storm3c", 159),
                          rep("storm4a", 187),
                          rep("storm4b", 203),
                          rep("storm5", 59),
                          rep("storm6", 103),
                          rep("storm7", 339),
                          rep("storm8", 383),
                          rep("storm9a", 139),
                          rep("storm9b", 286))

FRCH_storms <- FRCH_storms[c("datetimeAK", "site.ID", "fDOM.QSU", "SpCond.µS.cm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(FRCH_storms)<- colNames # renaming columns

### MOOS ###
MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2020/All_Sites/", 
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

MOOS_storms <- MOOS_storms[c("datetimeAK", "site.ID", "fDOM.QSU", "SpCond.µS.cm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(MOOS_storms)<- colNames # renaming columns

### POKE ###
POKEstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2020/All_Sites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 95),
                          rep("storm10", 99),
                          rep("storm11", 199),
                          rep("storm12", 307),
                          rep("storm13", 87),
                          rep("storm14", 383),
                          rep("storm15", 335),
                          rep("storm16", 95),
                          rep("storm17", 119),
                          rep("storm18", 95),
                          rep("storm19", 135),
                          rep("storm2", 87),
                          rep("storm20", 139),
                          rep("storm21", 227),
                          rep("storm22a", 107),
                          rep("storm22b", 212),
                          rep("storm3", 119),
                          rep("storm4a", 98),
                          rep("storm4b", 95),
                          rep("storm4c", 159),
                          rep("storm5", 219),
                          rep("storm6", 95),
                          rep("storm7", 127),
                          rep("storm8", 135),
                          rep("storm9", 263))

POKE_storms <- POKE_storms[c("datetimeAK", "site.ID", "fDOM.QSU", "SpCond.µS.cm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(POKE_storms)<- colNames # renaming columns

### VAUL ###
VAULstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2020/All_Sites/", 
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
                          rep("storm2", 214),
                          rep("storm3", 342),
                          rep("storm4", 318),
                          rep("storm5", 230),
                          rep("storm6a", 107),
                          rep("storm6b", 511),
                          rep("storm7", 283),
                          rep("storm8", 91),
                          rep("storm9", 91))

VAUL_storms <- VAUL_storms[c("datetimeAK", "site.ID", "fDOM.QSU", "SpCond.µS.cm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(VAUL_storms)<- colNames # renaming columns

### STRT ###
STRTstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2020/All_Sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm10", 246),
                          rep("storm1a", 103),
                          rep("storm1b", 161),
                          rep("storm1c", 105),
                          rep("storm1d", 86),
                          rep("storm1e", 476),
                          rep("storm2", 166),
                          rep("storm3", 386),
                          rep("storm4a", 140),
                          rep("storm4b", 322),
                          rep("storm5", 250),
                          rep("storm6", 122),
                          rep("storm7a", 98),
                          rep("storm7b", 95),
                          rep("storm8", 82),
                          rep("storm9a", 294),
                          rep("storm9b", 134),
                          rep("storm9c", 482))

STRT_storms <- STRT_storms[c("datetimeAK", "site.ID", "fDOM.QSU", "SpCond.µS.cm",
                             "Turbidity.FNU", "nitrateuM", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(STRT_storms)<- colNames # renaming columns

### CARI ###
CARIstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2020/All_Sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

CARI_storms<-do.call("rbind", lapply(CARIstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

CARI_storms$storm.num = c(rep("storm1", 239),
                          rep("storm2a", 103),
                          rep("storm2b", 95),
                          rep("storm2c", 155),
                          rep("storm3", 305),
                          rep("storm4", 155),
                          rep("storm5", 219),
                          rep("storm6", 183),
                          rep("storm7", 307),
                          rep("storm8a", 111),
                          rep("storm8b", 491),
                          rep("storm9", 99))

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "Discharge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "MeanDischarge", "storm.num")

names(CARI_storms)<- colNames # renaming columns

CARI_storms$ABS_254 <- NA

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]



# merge 2020 # 
Q.sum.2020 <- rbind(FRCH_storms, MOOS_storms,
                    POKE_storms, VAUL_storms,
                    STRT_storms, CARI_storms)

Q.sum.2020$year <- 2020

### 2021 ####
### FRCH ###
FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2021/All_Sites/", 
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
  rep("storm5a", 184),
  rep("storm5b", 260),
  rep("storm6a", 112),
  rep("storm6b", 312),
  rep("storm7", 140),
  rep("storm8", 468))

FRCH_storms <- FRCH_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(FRCH_storms)<- colNames # renaming columns

### MOOS ###
MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2021/All_Sites/", 
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
                          rep("storm4a", 167),
                          rep("storm4b", 247),
                          rep("storm5a", 91),
                          rep("storm5b", 191),
                          rep("storm6", 127),
                          rep("storm7", 259))

MOOS_storms <- MOOS_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(MOOS_storms)<- colNames # renaming columns

### POKE ###
POKEstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2021/All_Sites/", 
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
                          rep("storm7c", 99),
                          rep("storm7d", 147))


POKE_storms <- POKE_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(POKE_storms)<- colNames # renaming columns

### VAUL ###
VAULstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2021/All_Sites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(
  rep("storm1b", 267),
  
  rep("storm3", 667),
  rep("storm4a", 427),
  rep("storm4b", 319),
  rep("storm5a", 331),
  rep("storm5b", 383))

VAUL_storms <- VAUL_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                              "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(VAUL_storms)<- colNames # renaming columns

### STRT ###
STRTstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2021/All_Sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm1a", 191),
                          rep("storm1b", 255),
                          rep("storm2a", 95),
                          rep("storm2b", 211),
                          rep("storm3", 127))

STRT_storms <- STRT_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(STRT_storms)<- colNames # renaming columns

### CARI ###
CARIstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2021/All_Sites/", 
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

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "Discharge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "MeanDischarge", "storm.num")

names(CARI_storms)<- colNames # renaming columns

CARI_storms$ABS_254 <- NA

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]



# merge 2021 # 
Q.sum.2021 <- rbind(FRCH_storms, MOOS_storms,
                    POKE_storms, VAUL_storms,
                    STRT_storms, CARI_storms)

Q.sum.2021$year <- 2021

### 2022 ####
### FRCH ###
FRCHstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
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

FRCH_storms <- FRCH_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "Q", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(FRCH_storms)<- colNames # renaming columns

### MOOS ###
MOOSstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
                                  recursive=F, 
                                  pattern="MOOS", 
                                  full.names=TRUE)

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

MOOS_storms <- MOOS_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "Q", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(MOOS_storms)<- colNames # renaming columns

### POKE ###
POKEstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
                                  recursive=F, 
                                  pattern="POKE", 
                                  full.names=TRUE)

POKE_storms<-do.call("rbind", lapply(POKEstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

POKE_storms$storm.num = c(rep("storm1", 139),
                          rep("storm2", 119),
                          rep("storm3", 95),
                          rep("storm4", 187))


POKE_storms <- POKE_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "Q", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(POKE_storms)<- colNames # renaming columns

### VAUL ###
VAULstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
                                  recursive=F, 
                                  pattern="VAUL", 
                                  full.names=TRUE)

VAUL_storms<-do.call("rbind", lapply(VAULstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

VAUL_storms$storm.num = c(rep("storm1", 127),
                          rep("storm2", 763))

VAUL_storms <- VAUL_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "Q", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(VAUL_storms)<- colNames # renaming columns

### STRT ###
STRTstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
                                  recursive=F, 
                                  pattern="STRT", 
                                  full.names=TRUE)

STRT_storms<-do.call("rbind", lapply(STRTstorm_file_list, 
                                     read.csv, 
                                     check.names = FALSE,
                                     stringsAsFactors=FALSE, 
                                     header=T, blank.lines.skip = TRUE, fill=TRUE))

STRT_storms$storm.num = c(rep("storm1", 103),
                          rep("storm2", 191),
                          rep("storm3", 107))

STRT_storms <- STRT_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "Q", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "ABS_254", "MeanDischarge", "storm.num")

names(STRT_storms)<- colNames # renaming columns

### CARI ###
CARIstorm_file_list <- list.files(path="~/Documents/Storms_clean_repo/Storm_Events/2022/All_sites/", 
                                  recursive=F, 
                                  pattern="CARI", 
                                  full.names=TRUE)

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

CARI_storms <- CARI_storms[c("DateTimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "Discharge", "storm.num")]

colNames <- c("datetimeAK", "site.ID", "fDOM", "SPC", "Turb", 
              "NO3", "MeanDischarge", "storm.num")

names(CARI_storms)<- colNames # renaming columns

CARI_storms$ABS_254 <- NA

CARI_storms <- CARI_storms[c("datetimeAK", "site.ID", "fDOM", "SPC",
                             "Turb", "NO3", "ABS_254", "MeanDischarge", "storm.num")]



# merge 2022 # 
Q.sum.2022 <- rbind(FRCH_storms, MOOS_storms,
                    POKE_storms, VAUL_storms,
                    STRT_storms, CARI_storms)

Q.sum.2022$year <- 2022

# 
# merge all years #

Q.sum <- rbind(Q.sum.2018, Q.sum.2019,
               Q.sum.2020, Q.sum.2021,
               Q.sum.2022)

Q.sum.site.year <- Q.sum %>% 
  dplyr::group_by(site.ID, year) %>% 
  dplyr::summarise(Q = sum(MeanDischarge, na.rm = TRUE)) # totaling by year and snow/rain 

Q.sum.site.year <- na.omit(Q.sum.site.year)

ggplot(Q.sum.site.year, aes(x = site.ID, y = Q, color = as.character(site.ID))) + 
  geom_point() +
  xlab("") +
  ylab("Total Discharge per year (L/s)") +
  facet_wrap(~year) +
  scale_color_manual(values=c("#3288BD","#FF7F00","#A6761D","#6A3D9A", "#66C2A5","#E7298A"), "Site") +
  scale_y_continuous(trans='log10') +
  theme_classic() +
  theme(legend.position = "right") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1, size = 15), 
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 18))







