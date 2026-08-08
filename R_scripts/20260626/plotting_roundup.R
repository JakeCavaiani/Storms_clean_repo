# =================================== Objectives =================================
#
# finalize up figures for submission
# 
# Status: 
#
# Review status: 

# Notes: 
# These plots were rerun in June of 2026 to addres TKH comments 
# 
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 26 June 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")

rm(list=ls(all=TRUE))

library(pacman)
p_load(tidyverse,
       lubridate,
       scales, 
       fs,
       data.table,
       fuzzyjoin,
       gsheet,
       here)


### SUPPLEMENTAL FIGURE 1: Time series of chem and Q ####
# Read in daily chemistry data for DOD sites  
years <- c(2015, 2018, 2019, 2020, 2021, 2022)

mean_daily_chem <- map_dfr(years, function(yr) {
  df <- read_csv(sprintf("processed_sensor_data/%d/SUNA.EXO.int.corr.lab_%d.csv", yr, yr))
  
  # Standardize the datetime column name
  if (yr == 2015) {
    df <- df %>% rename(min = datetimeAK_rd,
                        fDOM.QSU.mn.adj = fDOM.QSU.adj,
                        SpCond.uScm.mn.adj = SpCond.uScm.adj,
                        Turbidity.FNU.mn.adj = Turbidity.FNU.adj,
                        nitrateuM.adj.mn = nitrateuM.adj.mn)
  }
  
  df %>%
    select(min, Site,
           fDOM.QSU.mn.adj, SpCond.uScm.mn.adj,
           Turbidity.FNU.mn.adj, nitrateuM.adj.mn) %>%
    mutate(day = as.Date(min))
}) %>%
  group_by(Site, day) %>%
  summarise(
    fDOM    = mean(fDOM.QSU.mn.adj,      na.rm = TRUE),
    SPC     = mean(SpCond.uScm.mn.adj,   na.rm = TRUE),
    Turb    = mean(Turbidity.FNU.mn.adj, na.rm = TRUE),
    NO3 = mean(nitrateuM.adj.mn,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(format(day, "%Y"))) %>%
  rename(site.ID = Site)

mean_daily_chem <- mean_daily_chem %>% filter(!is.na(site.ID))

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

mean_daily_chem_trimmed <- mean_daily_chem %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

# Read in Q for each year (2018, 2019, 2020, 2021, 2022) and take mean daily Q
# Build daily Q from yearly files
Q_daily <- list.files("Q/Q_chem", pattern = "^DOD\\.\\d{4}\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x) %>% select(datetimeAK, site.ID, Q)) %>%
  mutate(day = as.Date(datetimeAK, tz = "America/Anchorage")) %>%
  group_by(site.ID, day) %>%
  summarise(Q = mean(Q, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = as.numeric(format(day, "%Y")))

Q_daily <- Q_daily %>% filter(!is.na(site.ID))

# Trim to common window
Q_daily_trimmed <- Q_daily %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

# Merge into mean_daily (keep mean_daily as the main data frame)
DOD_Q_daily <- mean_daily_chem_trimmed %>%
  left_join(Q_daily_trimmed, by = c("day", "site.ID", "year"))

DOD_Q_daily <- DOD_Q_daily %>% filter(!is.na(day))

# read in NEON data
neon_years <- 2018:2022

mean_daily_neon <- map_dfr(neon_years, function(yr) {
  read_csv(sprintf("processed_sensor_data/%d/NEON_Q_WaterQuality%d.csv", yr, yr)) %>%
    select(DateTimeAK, site.ID.x,
           fDOM, SPC,
           Turb, NO3, Discharge) %>%
    mutate(day = as.Date(DateTimeAK))
}) %>%
  group_by(site.ID.x, day) %>%
  summarise(
    fDOM    = mean(fDOM,      na.rm = TRUE),
    SPC     = mean(SPC,   na.rm = TRUE),
    Turb    = mean(Turb, na.rm = TRUE),
    NO3 = mean(NO3,     na.rm = TRUE),
    Q = mean(Discharge,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(format(day, "%Y"))) %>%
  rename(site.ID = site.ID.x)

mean_daily_neon <- mean_daily_neon %>% filter(!is.na(site.ID))

# Trim to common window
mean_daily_neon_trimmed <- mean_daily_neon %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

# combine NEON and DOD chem 
mean_daily_chem_all <- bind_rows(DOD_Q_daily, mean_daily_neon_trimmed) %>%
  mutate(across(c(fDOM, SPC, Turb, NO3, Q),
                ~ ifelse(is.nan(.) | . < 0, NA, .))) %>%
  filter(if_any(c(fDOM, SPC, Turb, NO3, Q), ~ !is.na(.))) 

# Pivot long 
mean_daily_fig_data_long <- mean_daily_chem_all %>%
  pivot_longer(
    cols = c("fDOM", "SPC", "Turb", "NO3", "Q"),
    names_to = "response_var",
    names_prefix = "wk",
    values_to = "concentration") %>% 
  filter(response_var != "dailyABS")# converting to a long format so each response_var is within a single column

mean_daily_fig_data_long$response_var <- factor(mean_daily_fig_data_long$response_var, levels = c("Q", "fDOM", "NO3", "SPC", "Turb"), 
                                       labels = c("Q~(L/s)", "fDOM~(QSU)", "NO[3]^{'-'}~(mu*M)", "SPC~(mu*S/cm)", "Turbidity~(FNU)"))


# Plotting #
mean_daily_fig_data_long %>%
  ggplot(aes(x = day, y = concentration, color = site.ID)) +
  geom_line() +
  scale_color_manual(values = c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A"), 
                     guide = guide_legend(title = "Site")) +
  xlab("") +
  ylab("") +
  facet_grid(response_var~year, scales = "free", labeller = labeller(response_var = label_parsed)) +
  theme_classic() +
  theme(strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 13, angle = -45, hjust = 1),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

ggsave("DoD_2015_2022.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 9, units = "in")

ggsave("DoD_2015_2022.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 9, units = "in")


# Daily mean Q boxplot with letters for significant differences for each catchment and year of the study ####
rm(list=ls(all=TRUE))

library(pacman)
p_load(tidyverse,
       ggpattern,
       rstatix, 
       multcompView)

years <- c(2015, 2018, 2019, 2020, 2021, 2022)

mean_daily_chem <- map_dfr(years, function(yr) {
  df <- read_csv(sprintf("processed_sensor_data/%d/SUNA.EXO.int.corr.lab_%d.csv", yr, yr))
  
  # Standardize the datetime column name
  if (yr == 2015) {
    df <- df %>% rename(min = datetimeAK_rd,
                        fDOM.QSU.mn.adj = fDOM.QSU.adj,
                        SpCond.uScm.mn.adj = SpCond.uScm.adj,
                        Turbidity.FNU.mn.adj = Turbidity.FNU.adj,
                        nitrateuM.adj.mn = nitrateuM.adj.mn)
  }
  
  df %>%
    dplyr::select(min, Site,
           fDOM.QSU.mn.adj, SpCond.uScm.mn.adj,
           Turbidity.FNU.mn.adj, nitrateuM.adj.mn) %>%
    mutate(day = as.Date(min))
}) %>%
  group_by(Site, day) %>%
  summarise(
    fDOM    = mean(fDOM.QSU.mn.adj,      na.rm = TRUE),
    SPC     = mean(SpCond.uScm.mn.adj,   na.rm = TRUE),
    Turb    = mean(Turbidity.FNU.mn.adj, na.rm = TRUE),
    NO3 = mean(nitrateuM.adj.mn,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(format(day, "%Y"))) %>%
  rename(site.ID = Site)

mean_daily_chem <- mean_daily_chem %>% filter(!is.na(site.ID))

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

mean_daily_chem_trimmed <- mean_daily_chem %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

summary <- mean_daily_chem_trimmed %>% 
  group_by(site.ID, year) %>% 
  summarise(date_range_min = min(day),
            date_range_max = max(day)) %>% 
  na.omit()

# Read in Q for each year (2018, 2019, 2020, 2021, 2022) and take mean daily Q
# Build daily Q from yearly files
Q_daily <- list.files("Q/Q_chem", pattern = "^DOD\\.\\d{4}\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x) %>% select(datetimeAK, site.ID, Q)) %>%
  mutate(day = as.Date(datetimeAK, tz = "America/Anchorage")) %>%
  group_by(site.ID, day) %>%
  summarise(Q = mean(Q, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = as.numeric(format(day, "%Y")))

Q_daily <- Q_daily %>% filter(!is.na(site.ID))

# Trim to common window
Q_daily_trimmed <- Q_daily %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

# Merge into mean_daily (keep mean_daily as the main data frame)
DOD_Q_daily <- mean_daily_chem_trimmed %>%
  left_join(Q_daily_trimmed, by = c("day", "site.ID", "year"))

DOD_Q_daily <- DOD_Q_daily %>% filter(!is.na(day))

# read in NEON data
neon_years <- 2018:2022

mean_daily_neon <- map_dfr(neon_years, function(yr) {
  read_csv(sprintf("processed_sensor_data/%d/NEON_Q_WaterQuality%d.csv", yr, yr)) %>%
    select(DateTimeAK, site.ID.x,
           fDOM, SPC,
           Turb, NO3, Discharge) %>%
    mutate(day = as.Date(DateTimeAK))
}) %>%
  group_by(site.ID.x, day) %>%
  summarise(
    fDOM    = mean(fDOM,      na.rm = TRUE),
    SPC     = mean(SPC,   na.rm = TRUE),
    Turb    = mean(Turb, na.rm = TRUE),
    NO3 = mean(NO3,     na.rm = TRUE),
    Q = mean(Discharge,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(format(day, "%Y"))) %>%
  rename(site.ID = site.ID.x)

mean_daily_neon <- mean_daily_neon %>% filter(!is.na(site.ID))

# Trim to common window
mean_daily_neon_trimmed <- mean_daily_neon %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

# combine NEON and DOD chem 
mean_daily_chem_all <- bind_rows(DOD_Q_daily, mean_daily_neon_trimmed) %>%
  mutate(across(c(fDOM, SPC, Turb, NO3, Q),
                ~ ifelse(is.nan(.) | . < 0, NA, .))) %>%
  filter(if_any(c(fDOM, SPC, Turb, NO3, Q), ~ !is.na(.)))

Q_daily <- mean_daily_chem_all %>% 
  select(site.ID, day, Q, year)

Q_daily$Burn <- NA

Q_daily <- Q_daily %>% 
  mutate(across(c(Burn),
                ~ifelse(site.ID == "CARI" | site.ID == "FRCH" | site.ID == "VAUL", "unburned", "burned")))

Q_daily$PF <- NA

Q_daily <- Q_daily %>% 
  mutate(across(c(PF),
                ~ifelse(site.ID == "VAUL" | site.ID == "STRT", "High", "Moderate")))



# 1. Remove NaNs
Q_daily <- Q_daily %>% filter(!is.na(site.ID))
Q_daily_new <- Q_daily %>% filter(!is.na(Q) & !is.nan(Q))

# 2. Per-year Kruskal-Wallis + Dunn's test, then compact letter display
# Function to get CLD for one year's data
get_cld_year <- function(df, yr) {
  sites <- unique(df$site.ID)
  
  # Only one site → just label "a"
  if (length(sites) < 2) {
    return(tibble(year = yr, site.ID = sites, Label = "a"))
  }
  
  dunn <- df %>%
    dunn_test(Q ~ site.ID, p.adjust.method = "bonferroni")
  
  pvals <- setNames(dunn$p.adj, paste(dunn$group1, dunn$group2, sep = "-"))
  letters <- multcompLetters(pvals)$Letters
  
  tibble(year = yr, site.ID = names(letters), Label = unname(letters))
}

# Loop over years
years <- sort(unique(Q_daily_new$year))

cld_df <- map_dfr(years, function(yr) {
  df_yr <- Q_daily_new %>% filter(year == yr)
  get_cld_year(df_yr, yr)
})

# Add x/y positions for plotting
desired_order <- c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL")

y_pos <- Q_daily_new %>%
  group_by(year, site.ID) %>%
  summarise(y_position = max(Q, na.rm = TRUE) * 2, .groups = "drop")

annotations <- cld_df %>%
  left_join(y_pos, by = c("year", "site.ID")) %>%
  mutate(x_position = match(site.ID, desired_order))

# 4. Plot
Q <- ggplot(Q_daily_new %>% filter(year %in% 2018:2022),
            aes(x = site.ID, y = Q, pattern = Burn, fill = site.ID)) +
  geom_boxplot(width = .25, outlier.colour = NA, alpha = 0.5) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       width = .5, color = "black",
                       pattern_fill = "white",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c(burned = "stripe", unburned = "none")) +
  scale_x_discrete(limits = desired_order,
                   labels = c("POKE" = "POKE\n(25%)",
                              "CARI" = "CARI\n(29%)",
                              "STRT" = "STRT\n(30%)",
                              "FRCH" = "FRCH\n(33%)",
                              "MOOS" = "MOOS\n(38%)",
                              "VAUL" = "VAUL\n(58%)")) +
  coord_cartesian(xlim = c(1, 6.05)) +
  scale_fill_manual(values = c("#3288BD","#FF7F00","#A6761D","#6A3D9A","#66C2A5","#E7298A"),
                    guide = "none") +
  scale_y_continuous(trans = 'log10') +
  xlab("") + ylab(expression("Discharge (L s"^-1*")")) +
  facet_wrap(~year) +
  geom_text(data = annotations,
            aes(x = x_position, y = y_position, label = Label),
            inherit.aes = FALSE, size = 7) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))

Q

ggsave("Q_stats.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

ggsave("Q_stats.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")


# Figure 4. Concentration-discharge relationships during storms ####
rm(list=ls(all=TRUE))
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
                       ~ifelse(site.ID == "STRT" | site.ID == "VAUL", "High", "Moderate"))) %>% 
  dplyr::mutate(across(c(burn),
                       ~ifelse(site.ID == "CARI" | site.ID == "VAUL", "Unburned", "Burned")))  

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

AMC_trimmed <- AMC %>%
  left_join(date_ranges, by = "year") %>%
  filter(date >= start_date,
         date <= end_date) %>%
  select(-start_date, -end_date)

vn = expression(paste(N*O[3]^"-"))
##subsetting by solute 
# NO3 #
HI_FI_NO3 = subset(AMC_trimmed, response_var == "NO3")
# fDOM #
HI_FI_fDOM = subset(AMC_trimmed, response_var == "fDOM")
# SPC #
HI_FI_SPC = subset(AMC_trimmed, response_var == "SPC")
# turb #
HI_FI_turb = subset(AMC_trimmed, response_var == "turb")

#### PLOTS ####

# 2015-2022 # 
HI_FI_NO3 <- subset(HI_FI_NO3, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
HI_FI_fDOM <- subset(HI_FI_fDOM, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
HI_FI_SPC <- subset(HI_FI_SPC, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")
HI_FI_turb <- subset(HI_FI_turb, year =="2015"| year == "2018" | year == "2019" | year == "2020" | year == "2021" | year == "2022")

# plots 
# NO3
vn = expression(paste(N*O[3]^"-"))

coord_fixed(ratio = 1)

HI_BETA_NO3.p = 
  ggplot(HI_FI_NO3, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(
    aes(ymin = HI_ymin, ymax = HI_ymax),
    colour = "black", alpha = 0.5, size = .5, width = 0.05
  ) + 
  geom_errorbarh(
    aes(xmin = Beta_ymin, xmax = Beta_ymax),
    colour = "black", alpha = 0.5, size = .5, height = 0.05
  ) +
  geom_point(
    aes(colour = factor(site.ID), shape = burn),
    size = 2.5
  ) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(
    values = c(
      "#3288BD", "#FF7F00", "#A6761D",
      "#6A3D9A", "#66C2A5", "#E7298A"
    )
  ) + 
  theme_bw() +
  xlim(-1.5, 1.5) +
  ylim(-1.5, 1.5) +
  ggtitle(vn) + 
  ylab("") +
  xlab("") +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), 
    text = element_text(size = 15),
    legend.position = "none",
    aspect.ratio = 1
  ) +
  labs(
    colour = "Catchment",
    shape = "Burn"
  )

a <- ggMarginal(
  HI_BETA_NO3.p,
  groupColour = TRUE,
  groupFill = TRUE
)

a

# HI_BETA_NO3.p = 
#   ggplot(HI_FI_NO3, aes(Beta_index, Hyst_index)) + 
#   geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
#   geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
#   geom_point(aes(colour = factor(site.ID), shape = burn), size = 2.5) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
#   theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle(vn)+ 
#   ylab("") +
#   xlab("") +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"), 
#         text = element_text(size = 15),
#         legend.position = "none") +
#   labs(
#     colour = "Catchment",
#     shape = "Burn")
# 
# a <- ggMarginal(HI_BETA_NO3.p, groupColour = TRUE, groupFill = TRUE)
# a

# fDOM
# HI_BETA_fDOM.p = 
#   ggplot(HI_FI_fDOM, aes(Beta_index, Hyst_index)) + 
#   geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
#   geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
#   geom_point(aes(colour = factor(site.ID), shape = burn), size = 2.5) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
#   theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("fDOM")+ 
#   ylab("HI") +
#   xlab("") +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"), 
#         text = element_text(size = 15),
#         legend.position = "none") +
#   guides(shape=guide_legend("Permafrost Extent"),
#          col=guide_legend("Catchment"))
# 
# b <- ggMarginal(HI_BETA_fDOM.p, groupColour = TRUE, groupFill = TRUE)
# b

HI_BETA_fDOM.p = 
  ggplot(HI_FI_fDOM, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(
    aes(ymin = HI_ymin, ymax = HI_ymax),
    colour = "black", alpha = 0.5, size = .5, width = 0.05
  ) + 
  geom_errorbarh(
    aes(xmin = Beta_ymin, xmax = Beta_ymax),
    colour = "black", alpha = 0.5, size = .5, height = 0.05
  ) +
  geom_point(
    aes(colour = factor(site.ID), shape = burn),
    size = 2.5
  ) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(
    values = c(
      "#3288BD", "#FF7F00", "#A6761D",
      "#6A3D9A", "#66C2A5", "#E7298A"
    )
  ) + 
  theme_bw() +
  ylim(-1.5, 1.5) + 
  xlim(-1.5, 1.5) +
  ggtitle("fDOM") + 
  ylab("HI") +
  xlab("") +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), 
    text = element_text(size = 15),
    legend.position = "none",
    aspect.ratio = 1
  ) +
  guides(
    shape = guide_legend("Permafrost Extent"),
    col = guide_legend("Catchment")
  )

b <- ggMarginal(
  HI_BETA_fDOM.p,
  groupColour = TRUE,
  groupFill = TRUE
)

b

# SPC
# HI_BETA_SPC.p = 
#   ggplot(HI_FI_SPC, aes(Beta_index, Hyst_index)) + 
#   geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
#   geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
#   geom_point(aes(colour = factor(site.ID), shape = burn), size = 2.5) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) + 
#   theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("SPC")+ 
#   ylab("HI") +
#   xlab("ß") +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"), 
#         text = element_text(size = 15),
#         legend.position = "none") 
# 
# c <- ggMarginal(HI_BETA_SPC.p, groupColour = TRUE, groupFill = TRUE)
# # c

HI_BETA_SPC.p = 
  ggplot(HI_FI_SPC, aes(Beta_index, Hyst_index)) + 
  geom_errorbar(
    aes(ymin = HI_ymin, ymax = HI_ymax),
    colour = "black", alpha = 0.5, size = .5, width = 0.05
  ) + 
  geom_errorbarh(
    aes(xmin = Beta_ymin, xmax = Beta_ymax),
    colour = "black", alpha = 0.5, size = .5, height = 0.05
  ) +
  geom_point(
    aes(colour = factor(site.ID), shape = burn),
    size = 2.5
  ) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  scale_color_manual(
    values = c(
      "#3288BD", "#FF7F00", "#A6761D",
      "#6A3D9A", "#66C2A5", "#E7298A"
    )
  ) + 
  theme_bw() +
  ylim(-1.5, 1.5) + 
  xlim(-1.5, 1.5) +
  ggtitle("SPC") + 
  ylab("HI") +
  xlab("ß") +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"), 
    text = element_text(size = 15),
    legend.position = "none",
    aspect.ratio = 1
  )

c <- ggMarginal(
  HI_BETA_SPC.p,
  groupColour = TRUE,
  groupFill = TRUE
)

c

# turb
# HI_BETA_turb.p =
#   ggplot(HI_FI_turb, aes(Beta_index, Hyst_index)) +
#   geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+
#   geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
#   geom_point(aes(colour = factor(site.ID), shape = burn), size = 2.5) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   # quadrant labels
#   annotate("text", x = -1.5, y =  1.25, label = "Clockwise/\nDilution",         hjust = 0, size = 5.5, fontface = "bold") +
#   annotate("text", x =  1.3, y =  1.25, label = "Clockwise/\nFlushing",         hjust = 1, size = 5.5, fontface = "bold") +
#   annotate("text", x = -1.5, y = -1.00, label = "Counter-\nclockwise/\nDilution", hjust = 0, size = 5.5, fontface = "bold") +
#   annotate("text", x =  1.3, y = -1.00, label = "Counter-\nclockwise/\nFlushing", hjust = 1, size = 5.5, fontface = "bold") +
#   scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
#   theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
#   ggtitle("Turbidity") +
#   ylab("") + xlab("ß") +
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"),
#         text = element_text(size = 15),
#         legend.position = "none")
# 
# d <- ggMarginal(HI_BETA_turb.p, groupColour = TRUE, groupFill = TRUE)
# d
# OLD #
# HI_BETA_turb.p = 
#   ggplot(HI_FI_turb, aes(Beta_index, Hyst_index)) + 
#   geom_errorbar(aes(ymin = HI_ymin, ymax = HI_ymax), colour = "black", alpha = 0.5, size = .5, width = 0.05)+ 
#   geom_errorbarh(aes(xmin = Beta_ymin, xmax = Beta_ymax), colour = "black", alpha = 0.5, size = .5, height = 0.05) +
#   geom_point(aes(colour = factor(site.ID), shape = PF), size = 2.5) +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A"), "Permafrost Extent") + 
#   theme_bw() +
#   ylim(-1.5, 1.5) + 
#   xlim(-1.5, 1.5) +
#   ggtitle("Turbidity")+ 
#   ylab("") +
#   xlab("ß") +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         axis.line = element_line(colour = "black"), 
#         text = element_text(size = 15),
#         legend.position = "none") 
# 
# d <- ggMarginal(HI_BETA_turb.p, groupColour = TRUE, groupFill = TRUE)
# d
# 
# ggarrange(b,a,
#           c,d,
#           labels = c("A)", "B)",
#                      "C)", "D)"))

HI_BETA_turb.p =
  ggplot(HI_FI_turb, aes(Beta_index, Hyst_index)) +
  geom_errorbar(
    aes(ymin = HI_ymin, ymax = HI_ymax),
    colour = "black", alpha = 0.5, size = .5, width = 0.05
  ) +
  geom_errorbarh(
    aes(xmin = Beta_ymin, xmax = Beta_ymax),
    colour = "black", alpha = 0.5, size = .5, height = 0.05
  ) +
  geom_point(
    aes(colour = factor(site.ID), shape = burn),
    size = 2.5
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  # quadrant labels
  annotate(
    "text", x = -1.5, y = 1.25,
    label = "Clockwise/\nDilution",
    hjust = 0, size = 5.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 1.3, y = 1.25,
    label = "Clockwise/\nFlushing",
    hjust = 1, size = 5.5, fontface = "bold"
  ) +
  annotate(
    "text", x = -1.5, y = -1.00,
    label = "Counter-\nclockwise/\nDilution",
    hjust = 0, size = 5.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 1.3, y = -1.00,
    label = "Counter-\nclockwise/\nFlushing",
    hjust = 1, size = 5.5, fontface = "bold"
  ) +
  
  scale_color_manual(
    values = c(
      "#3288BD", "#FF7F00", "#A6761D",
      "#6A3D9A", "#66C2A5", "#E7298A"
    )
  ) +
  theme_bw() +
  ylim(-1.5, 1.5) +
  xlim(-1.5, 1.5) +
  ggtitle("Turbidity") +
  ylab("") +
  xlab("ß") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 15),
    legend.position = "none",
    aspect.ratio = 1
  )

d <- ggMarginal(
  HI_BETA_turb.p,
  groupColour = TRUE,
  groupFill = TRUE
)

d
ggarrange(b,a,
          c,d,
          labels = c("A)", "B)",
                     "C)", "D)"))
# OLD END #
library(ggpubr)
library(cowplot)

# a plot that has BOTH color and shape mappings + the legend visible
legend_plot <- ggplot(HI_FI_fDOM,
                      aes(Beta_index, Hyst_index,
                          colour = factor(site.ID), shape = burn)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#3288BD","#FF7F00","#A6761D",
                                "#6A3D9A","#66C2A5","#E7298A")) +
  labs(colour = "Catchment", shape = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 13))

shared_legend <- cowplot::get_legend(legend_plot)

# arrange the four ggMarginal panels
panels <- ggarrange(b, a,
                    c, d,
                    labels = c("A)", "B)", "C)", "D)"))

# stack panels + legend
final_fig <- ggarrange(panels, shared_legend,
                       ncol = 1, heights = c(1, 0.1))
final_fig


library(ggpubr)
library(cowplot)

# ---------------------------------------------------------
# Create plot for shared legend
# ---------------------------------------------------------

legend_plot <- ggplot(
  HI_FI_fDOM,
  aes(
    Beta_index,
    Hyst_index,
    colour = factor(site.ID),
    shape = burn
  )
) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c(
      "#3288BD",
      "#FF7F00",
      "#A6761D",
      "#6A3D9A",
      "#66C2A5",
      "#E7298A"
    )
  ) +
  labs(
    colour = "Catchment",
    shape = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    text = element_text(size = 13)
  )

shared_legend <- cowplot::get_legend(legend_plot)


# ---------------------------------------------------------
# Add a little extra space above plot titles
# ---------------------------------------------------------

a <- a +
  theme(
    plot.margin = margin(
      t = 15,
      r = 5,
      b = 5,
      l = 5
    )
  )

b <- b +
  theme(
    plot.margin = margin(
      t = 15,
      r = 5,
      b = 5,
      l = 5
    )
  )

c <- c +
  theme(
    plot.margin = margin(
      t = 15,
      r = 5,
      b = 5,
      l = 5
    )
  )

d <- d +
  theme(
    plot.margin = margin(
      t = 15,
      r = 5,
      b = 5,
      l = 5
    )
  )


# ---------------------------------------------------------
# Arrange the four panels
# ---------------------------------------------------------

panels <- ggarrange(
  b, a,
  c, d,
  labels = c("A)", "B)", "C)", "D)")
)


# ---------------------------------------------------------
# Stack panels + shared legend
# ---------------------------------------------------------

final_fig <- ggarrange(
  panels,
  shared_legend,
  ncol = 1,
  heights = c(1, 0.1)
)

# Display
final_fig

ggsave("HI_BETA.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 9, height = 9)

ggsave("HI_BETA.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 9, height = 9)


# Figure 5. Hysteresis index across all storms for fDOM (A), NO3– (B), SPC (C), and turbidity (D) ####
rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rstatix)
library(multcompView)

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

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

AMC_trimmed <- AMC %>%
  left_join(date_ranges, by = "year") %>%
  filter(date >= start_date,
         date <= end_date) %>%
  select(-start_date, -end_date)

### AMC SUMMARY STATS ###
summary_stats <- AMC_trimmed %>%
  group_by(site.ID, year) %>%
  summarise(n_storms = n_distinct(storm.ID), .groups = "drop") %>%
  group_by(site.ID) %>%
  summarise(total_storms = sum(n_storms))

AMC_trimmed %>%
  group_by(site.ID, year) %>%
  summarise(n_storms = n_distinct(storm.ID), .groups = "drop")

AMC_trimmed <- AMC_trimmed %>% 
  group_by(site.ID, response_var, year) %>% 
  dplyr::summarise(meanHI = mean(Hyst_index, na.rm = TRUE),
                   meanBETA = mean(Beta_index, na.rm = TRUE),
                   sdHI = sd(Hyst_index, na.rm = TRUE),
                   sdBETA = sd(Beta_index, na.rm = TRUE))

AMC_trimmed <- AMC_trimmed %>% 
  mutate(PF = case_when(site.ID == "STRT" | site.ID == "VAUL" ~ "High", TRUE ~ "Moderate")) %>% 
  mutate(Burn = case_when(site.ID == "CARI" | site.ID == "VAUL" ~ "Unburned", TRUE ~ "Burned"))

AMC_trimmed <- AMC_trimmed %>% ungroup() %>% 
  filter(response_var != "abs")

# ---- 1. CLD per response_var ---------------------------------------------
get_cld_var <- function(df, rv) {
  sites <- unique(df$site.ID)
  if (length(sites) < 2) {
    return(tibble(response_var = rv, site.ID = sites, Label = "a"))
  }
  
  dunn <- df %>%
    dunn_test(meanHI ~ site.ID, p.adjust.method = "bonferroni")
  
  pvals   <- setNames(dunn$p.adj, paste(dunn$group1, dunn$group2, sep = "-"))
  letters <- multcompLetters(pvals)$Letters
  
  tibble(response_var = rv,
         site.ID      = names(letters),
         Label        = unname(letters))
}

rvs <- unique(AMC_trimmed$response_var)
cld_df <- map_dfr(rvs, function(rv) {
  df_rv <- AMC_trimmed %>% filter(response_var == rv)
  get_cld_var(df_rv, rv)
})

# ---- 2. y-positions for letters ------------------------------------------
desired_order <- c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL")

y_pos <- AMC_trimmed %>%
  group_by(response_var, site.ID) %>%
  summarise(y_position = max(meanHI, na.rm = TRUE) + 0.15, .groups = "drop")

annotations <- cld_df %>%
  left_join(y_pos, by = c("response_var", "site.ID")) %>%
  mutate(x_position = match(site.ID, desired_order))

# ---- 3. Optional: pretty facet labels ------------------------------------
AMC_trimmed$response_var <- factor(AMC_trimmed$response_var,
                           levels = c("fDOM", "NO3", "SPC", "turb"),
                           labels = c("fDOM",
                                      "NO[3]^{'-'}",
                                      "SPC",
                                      "Turbidity"))
annotations$response_var <- factor(annotations$response_var,
                                   levels = c("fDOM", "NO3", "SPC", "turb"),
                                   labels = c("fDOM",
                                              "NO[3]^{'-'}",
                                              "SPC",
                                              "Turbidity"))

# ---- 4. Plot -------------------------------------------------------------
library(ggpattern)

lvls <- c("fDOM", "NO[3]^{'-'}", "SPC", "Turbidity")
labs <- c("fDOM", "Nitrate", "SPC", "Turbidity")

AMC_trimmed$response_var <- factor(AMC_trimmed$response_var, levels = lvls, labels = labs)
annotations$response_var <- factor(annotations$response_var,
                                   levels = lvls, labels = labs)

spc_labels <- data.frame(
  response_var = factor("SPC", levels = levels(AMC_trimmed$response_var)),
  x    = 0.6,           # near left edge
  y    = c(0.1, -0.1),
  lab  = c("Clockwise", "Counterclockwise")
)

HI_box <- ggplot(AMC_trimmed, aes(x = site.ID, y = meanHI,
                          fill = site.ID, pattern = Burn)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black",
                       pattern_fill    = "white",
                       pattern_angle   = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  geom_text(data = spc_labels,
            aes(x = x, y = y, label = lab),
            inherit.aes = FALSE, hjust = 0, fontface = "bold", size = 4) +
  scale_pattern_manual(values = c(Burned = "stripe", Unburned = "none"),
                       name = "") +
  scale_x_discrete(
    limits = c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL"),
    labels = c(POKE = "POKE\n(25%)",
               CARI = "CARI\n(29%)",
               STRT = "STRT\n(30%)",
               FRCH = "FRCH\n(33%)",
               MOOS = "MOOS\n(38%)",
               VAUL = "VAUL\n(58%)")
  ) +
  scale_fill_manual(values=c("#3288BD", "#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  xlab("") +
  ylab("Mean HI") +
  theme_bw() +
  facet_wrap(~response_var) +
  geom_text(data = annotations,
            aes(x = x_position, y = y_position, label = Label),
            inherit.aes = FALSE, size = 6) +
  guides(fill = "none",
         pattern = guide_legend(override.aes = list(fill = "white"))) +
  theme_classic() +
  theme(legend.position = "bottom")

HI_box

ggsave("HI_all_solutes.line.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 6, height = 6)

ggsave("HI_all_solutes.line.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 6, height = 6)

# Figure 6. Slope of the concentration-discharge relationship on the rising limb of each storm (β), across all storms ####
rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rstatix)
library(multcompView)
library(here)

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

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

AMC_trimmed <- AMC %>%
  left_join(date_ranges, by = "year") %>%
  filter(date >= start_date,
         date <= end_date) %>%
  select(-start_date, -end_date)

AMC <- AMC_trimmed

### AMC SUMMARY STATS ###
summary_stats <- AMC %>%
  group_by(site.ID, year) %>%
  summarise(n_storms = n_distinct(storm.ID), .groups = "drop") %>%
  group_by(site.ID) %>%
  summarise(total_storms = sum(n_storms))

year <- AMC %>%
  group_by(site.ID, year) %>%
  summarise(n_storms = n_distinct(storm.ID), .groups = "drop")

AMC <- AMC %>% 
  group_by(site.ID, response_var, year) %>% 
  dplyr::summarise(meanHI = mean(Hyst_index, na.rm = TRUE),
                   meanBETA = mean(Beta_index, na.rm = TRUE),
                   sdHI = sd(Hyst_index, na.rm = TRUE),
                   sdBETA = sd(Beta_index, na.rm = TRUE))

AMC <- AMC %>% 
  mutate(PF = case_when(site.ID == "STRT" | site.ID == "VAUL" ~ "High", TRUE ~ "Moderate")) %>% 
  mutate(Burn = case_when(site.ID == "CARI" | site.ID == "VAUL" ~ "Unburned", TRUE ~ "Burned"))

AMC <- AMC %>% ungroup() %>% 
  filter(response_var != "abs")

# ---- 1. CLD per response_var ---------------------------------------------
get_cld_var <- function(df, rv) {
  sites <- unique(df$site.ID)
  if (length(sites) < 2) {
    return(tibble(response_var = rv, site.ID = sites, Label = "a"))
  }
  
  dunn <- df %>%
    dunn_test(meanBETA ~ site.ID, p.adjust.method = "bonferroni")
  
  pvals   <- setNames(dunn$p.adj, paste(dunn$group1, dunn$group2, sep = "-"))
  letters <- multcompLetters(pvals)$Letters
  
  tibble(response_var = rv,
         site.ID      = names(letters),
         Label        = unname(letters))
}

rvs <- unique(AMC$response_var)
cld_df <- map_dfr(rvs, function(rv) {
  df_rv <- AMC %>% filter(response_var == rv)
  get_cld_var(df_rv, rv)
})

# ---- 2. y-positions for letters ------------------------------------------
desired_order <- c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL")

y_pos <- AMC %>%
  group_by(response_var, site.ID) %>%
  summarise(y_position = max(meanBETA, na.rm = TRUE) + 0.15, .groups = "drop")

annotations <- cld_df %>%
  left_join(y_pos, by = c("response_var", "site.ID")) %>%
  mutate(x_position = match(site.ID, desired_order))

# ---- 3. Optional: pretty facet labels ------------------------------------
AMC$response_var <- factor(AMC$response_var,
                           levels = c("fDOM", "NO3", "SPC", "turb"),
                           labels = c("fDOM",
                                      "NO[3]^{'-'}",
                                      "SPC",
                                      "Turbidity"))
annotations$response_var <- factor(annotations$response_var,
                                   levels = c("fDOM", "NO3", "SPC", "turb"),
                                   labels = c("fDOM",
                                              "NO[3]^{'-'}",
                                              "SPC",
                                              "Turbidity"))

# ---- 4. Plot -------------------------------------------------------------
library(ggpattern)

lvls <- c("fDOM", "NO[3]^{'-'}", "SPC", "Turbidity")
labs <- c("fDOM", "Nitrate", "SPC", "Turbidity")

AMC$response_var <- factor(AMC$response_var, levels = lvls, labels = labs)
annotations$response_var <- factor(annotations$response_var,
                                   levels = lvls, labels = labs)

spc_labels <- data.frame(
  response_var = factor("SPC", levels = levels(AMC$response_var)),
  x    = 0.6,           # near left edge
  y    = c(0.5, -1.0),
  lab  = c("Flushing", "Dilution")
)

BETA_box <- ggplot(AMC, aes(x = site.ID, y = meanBETA,
                          fill = site.ID, pattern = Burn)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black",
                       pattern_fill    = "white",
                       pattern_angle   = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  geom_text(data = spc_labels,
            aes(x = x, y = y, label = lab),
            inherit.aes = FALSE, hjust = 0, fontface = "bold", size = 4) +
  scale_pattern_manual(values = c(Burned = "stripe", Unburned = "none"),
                       name = "") +
  scale_x_discrete(
    limits = c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL"),
    labels = c(POKE = "POKE\n(25%)",
               CARI = "CARI\n(29%)",
               STRT = "STRT\n(30%)",
               FRCH = "FRCH\n(33%)",
               MOOS = "MOOS\n(38%)",
               VAUL = "VAUL\n(58%)")
  ) +
  scale_fill_manual(values=c("#3288BD", "#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  xlab("") +
  ylab("ß") +
  theme_bw() +
  facet_wrap(~response_var) +
  geom_text(data = annotations,
            aes(x = x_position, y = y_position, label = Label),
            inherit.aes = FALSE, size = 6) +
  guides(fill = "none",
         pattern = guide_legend(override.aes = list(fill = "white"))) +
  theme_classic() +
  theme(legend.position = "bottom")

BETA_box

ggsave("BETA_all_solutes.line.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 6, height = 6)

ggsave("BETA_all_solutes.line.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 6, height = 6)





### SUPPLEMENTAL FIGURE 2-5: Daily Mean NO3, fDOM, SPC, Turb #### ####
rm(list=ls(all=TRUE))

library(pacman)
p_load(tidyverse,
       ggpattern,
       rstatix, 
       multcompView)

years <- c(2015, 2018, 2019, 2020, 2021, 2022)

mean_daily_chem <- map_dfr(years, function(yr) {
  df <- read_csv(sprintf("processed_sensor_data/%d/SUNA.EXO.int.corr.lab_%d.csv", yr, yr))
  
  # Standardize the datetime column name
  if (yr == 2015) {
    df <- df %>% rename(min = datetimeAK_rd,
                        fDOM.QSU.mn.adj = fDOM.QSU.adj,
                        SpCond.uScm.mn.adj = SpCond.uScm.adj,
                        Turbidity.FNU.mn.adj = Turbidity.FNU.adj,
                        nitrateuM.adj.mn = nitrateuM.adj.mn)
  }
  
  df %>%
    dplyr::select(min, Site,
                  fDOM.QSU.mn.adj, SpCond.uScm.mn.adj,
                  Turbidity.FNU.mn.adj, nitrateuM.adj.mn) %>%
    mutate(day = as.Date(min))
}) %>%
  group_by(Site, day) %>%
  summarise(
    fDOM    = mean(fDOM.QSU.mn.adj,      na.rm = TRUE),
    SPC     = mean(SpCond.uScm.mn.adj,   na.rm = TRUE),
    Turb    = mean(Turbidity.FNU.mn.adj, na.rm = TRUE),
    NO3 = mean(nitrateuM.adj.mn,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(format(day, "%Y"))) %>%
  rename(site.ID = Site)

mean_daily_chem <- mean_daily_chem %>% filter(!is.na(site.ID))

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

mean_daily_chem_trimmed <- mean_daily_chem %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

summary <- mean_daily_chem_trimmed %>% 
  group_by(site.ID, year) %>% 
  summarise(date_range_min = min(day),
            date_range_max = max(day)) %>% 
  na.omit()

# read in NEON data
neon_years <- 2018:2022

mean_daily_neon <- map_dfr(neon_years, function(yr) {
  read_csv(sprintf("processed_sensor_data/%d/NEON_Q_WaterQuality%d.csv", yr, yr)) %>%
    select(DateTimeAK, site.ID.x,
           fDOM, SPC,
           Turb, NO3, Discharge) %>%
    mutate(day = as.Date(DateTimeAK))
}) %>%
  group_by(site.ID.x, day) %>%
  summarise(
    fDOM    = mean(fDOM,      na.rm = TRUE),
    SPC     = mean(SPC,   na.rm = TRUE),
    Turb    = mean(Turb, na.rm = TRUE),
    NO3 = mean(NO3,     na.rm = TRUE),
    Q = mean(Discharge,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = as.numeric(format(day, "%Y"))) %>%
  rename(site.ID = site.ID.x)

mean_daily_neon <- mean_daily_neon %>% filter(!is.na(site.ID))

# Trim to common window
mean_daily_neon_trimmed <- mean_daily_neon %>%
  left_join(date_ranges, by = "year") %>%
  filter(day >= start_date,
         day <= end_date) %>%
  select(-start_date, -end_date)

# combine NEON and DOD chem 
mean_daily_chem_all <- bind_rows(mean_daily_chem_trimmed, mean_daily_neon_trimmed) %>%
  mutate(across(c(fDOM, SPC, Turb, NO3, Q),
                ~ ifelse(is.nan(.) | . < 0, NA, .))) %>%
  filter(if_any(c(fDOM, SPC, Turb, NO3, Q), ~ !is.na(.)))

mean_daily_chem_all$Burn <- NA

mean_daily_chem_all <- mean_daily_chem_all %>% 
  mutate(across(c(Burn),
                ~ifelse(site.ID == "CARI" | site.ID == "FRCH" | site.ID == "VAUL", "unburned", "burned")))

mean_daily_chem_all$PF <- NA

mean_daily_chem_all <- mean_daily_chem_all %>% 
  mutate(across(c(PF),
                ~ifelse(site.ID == "VAUL" | site.ID == "STRT", "High", "Moderate")))


# 1. Remove NaNs
mean_daily_chem_all <- mean_daily_chem_all %>% filter(!is.na(site.ID))

library(tidyverse)
library(rstatix)
library(multcompView)

get_cld_year <- function(df, yr, analyte) {
  
  df_yr <- df %>%
    filter(year == yr) %>%
    filter(!is.na(.data[[analyte]]),
           !is.na(site.ID))
  
  sites <- unique(df_yr$site.ID)
  
  # Only one site → just label "a"
  if (length(sites) < 2) {
    return(tibble(
      year = yr,
      analyte = analyte,
      site.ID = sites,
      Label = "a"
    ))
  }
  
  # Kruskal-Wallis test
  kw <- kruskal_test(
    df_yr,
    formula = as.formula(paste(analyte, "~ site.ID"))
  )
  
  # If KW is not significant, all groups get "a"
  if (kw$p > 0.05) {
    return(tibble(
      year = yr,
      analyte = analyte,
      site.ID = sites,
      Label = "a"
    ))
  }
  
  # Dunn's test if KW is significant
  dunn <- dunn_test(
    df_yr,
    formula = as.formula(paste(analyte, "~ site.ID")),
    p.adjust.method = "bonferroni"
  )
  
  # Create named p-value vector
  pvals <- setNames(
    dunn$p.adj,
    paste(dunn$group1, dunn$group2, sep = "-")
  )
  
  # Compact letter display
  letters <- multcompLetters(pvals)$Letters
  
  tibble(
    year = yr,
    analyte = analyte,
    site.ID = names(letters),
    Label = unname(letters)
  )
}


analytes <- c("fDOM", "SPC", "Turb", "NO3")

years <- sort(unique(mean_daily_chem_all$year))

cld_df <- map_dfr(analytes, function(analyte) {
  
  map_dfr(years, function(yr) {
    get_cld_year(
      df = mean_daily_chem_all,
      yr = yr,
      analyte = analyte
    )
  })
})

chem_long <- mean_daily_chem_all %>%
  select(site.ID, day, year, fDOM, SPC, Turb, NO3, Burn, PF) %>%
  pivot_longer(
    cols = c(fDOM, SPC, Turb, NO3),
    names_to = "analyte",
    values_to = "value"
  )

plot_data <- chem_long %>%
  left_join(
    cld_df,
    by = c("year", "analyte", "site.ID")
  )

ggplot(plot_data, aes(x = site.ID, y = value)) +
  geom_boxplot() +
  geom_text(
    data = plot_data %>%
      group_by(year, analyte, site.ID, Label) %>%
      summarize(
        y = max(value, na.rm = TRUE) * 1.05,
        .groups = "drop"
      ),
    aes(y = y, label = Label),
    vjust = 0
  ) +
  facet_grid(
    analyte ~ year,
    scales = "free_y"
  ) +
  labs(
    x = "Site",
    y = NULL
  ) +
  theme_bw()

desired_order <- c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL")

y_pos <- chem_long %>%
  group_by(year, analyte, site.ID) %>%
  summarise(
    y_position = max(value, na.rm = TRUE) * 1.7,
    .groups = "drop"
  )

annotations <- cld_df %>%
  left_join(
    y_pos,
    by = c("year", "analyte", "site.ID")
  ) %>%
  mutate(
    x_position = match(site.ID, desired_order)
  )

plot_analyte <- function(analyte_name, y_label) {
  
  plot_df <- chem_long %>%
    filter(
      analyte == analyte_name,
      year %in% 2015:2022
    )
  
  annotation_df <- annotations %>%
    filter(
      analyte == analyte_name,
      year %in% 2015:2022
    )
  
  ggplot(
    plot_df,
    aes(
      x = site.ID,
      y = value,
      pattern = Burn,
      fill = site.ID
    )
  ) +
    geom_boxplot(
      width = .25,
      outlier.colour = NA,
      alpha = 0.5
    ) +
    geom_boxplot_pattern(
      position = position_dodge(preserve = "single"),
      width = .5,
      color = "black",
      pattern_fill = "white",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6
    ) +
    scale_pattern_manual(
      values = c(
        burned = "stripe",
        unburned = "none"
      )
    ) +
    scale_x_discrete(
      limits = desired_order,
      labels = c(
        "POKE" = "POKE\n(25%)",
        "CARI" = "CARI\n(29%)",
        "STRT" = "STRT\n(30%)",
        "FRCH" = "FRCH\n(33%)",
        "MOOS" = "MOOS\n(38%)",
        "VAUL" = "VAUL\n(58%)"
      )
    ) +
    coord_cartesian(xlim = c(1, 6.05)) +
    scale_fill_manual(
      values = c(
        "#3288BD",
        "#FF7F00",
        "#A6761D",
        "#6A3D9A",
        "#66C2A5",
        "#E7298A"
      ),
      guide = "none"
    ) +
    xlab("") +
    ylab(y_label) +
    facet_wrap(~year) +
    geom_text(
      data = annotation_df,
      aes(
        x = x_position,
        y = y_position,
        label = Label
      ),
      inherit.aes = FALSE,
      size = 7
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 25),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
}

NO3 <- plot_analyte("NO3", expression(NO[3]^"-" ~ "(\u00B5M)"))
NO3

ggsave("Supplemental Figure 2. Daily mean NO3– .pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

 ggsave("Supplemental Figure 2. Daily mean NO3– .png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

fDOM <- plot_analyte("fDOM", "fDOM (QSU)")
fDOM

ggsave("Supplemental Figure 3. Daily mean fDOM.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

ggsave("Supplemental Figure 3. Daily mean fDOM.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

plot_analyte <- function(analyte_name, y_label, log_y = FALSE) {
  
  plot_df <- chem_long %>%
    filter(
      analyte == analyte_name,
      year %in% 2015:2022
    )
  
  annotation_df <- annotations %>%
    filter(
      analyte == analyte_name,
      year %in% 2015:2022
    )
  
  p <- ggplot(
    plot_df,
    aes(
      x = site.ID,
      y = value,
      pattern = Burn,
      fill = site.ID
    )
  ) +
    geom_boxplot(
      width = .25,
      outlier.colour = NA,
      alpha = 0.5
    ) +
    geom_boxplot_pattern(
      position = position_dodge(preserve = "single"),
      width = .5,
      color = "black",
      pattern_fill = "white",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6
    ) +
    scale_pattern_manual(
      values = c(burned = "stripe", unburned = "none")
    ) +
    scale_x_discrete(
      limits = desired_order,
      labels = c(
        "POKE" = "POKE\n(25%)",
        "CARI" = "CARI\n(29%)",
        "STRT" = "STRT\n(30%)",
        "FRCH" = "FRCH\n(33%)",
        "MOOS" = "MOOS\n(38%)",
        "VAUL" = "VAUL\n(58%)"
      )
    ) +
    coord_cartesian(xlim = c(1, 6.05)) +
    scale_fill_manual(
      values = c(
        "#3288BD", "#FF7F00", "#A6761D",
        "#6A3D9A", "#66C2A5", "#E7298A"
      ),
      guide = "none"
    ) +
    xlab("") +
    ylab(y_label) +
    facet_wrap(~year) +
    geom_text(
      data = annotation_df,
      aes(
        x = x_position,
        y = y_position,
        label = Label
      ),
      inherit.aes = FALSE,
      size = 7
    )
  
  # Apply log scale if requested
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  p +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 25),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
}
SPC <- plot_analyte("SPC", expression("SPC (" * "\u00B5S/cm" * ")"),
  log_y = TRUE)

SPC
ggsave("Supplemental Figure 4. Daily mean SPC.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

ggsave("Supplemental Figure 4. Daily mean SPC.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

Turb <- plot_analyte("Turb", "Turbidity (FNU)",
                    log_y = TRUE)
Turb
ggsave("Supplemental Figure 5. Daily mean turbidity.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

ggsave("Supplemental Figure 5. Daily mean turbidity.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

### SUPPLEMENTAL FIGURE 6: Annual standard deviation of HI #### 
rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rstatix)
library(multcompView)

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

# Trim to common window
date_ranges <- tibble(
  year = c(2015, 2018, 2019, 2020, 2021, 2022),
  start_date = as.Date(c(
    "2015-06-26",
    "2018-06-27",
    "2019-06-16",
    "2020-06-17",
    "2021-06-12",
    "2022-06-13"
  )),
  end_date = as.Date(c(
    "2015-10-11",
    "2018-10-12",
    "2019-10-01",
    "2020-09-30",
    "2021-09-27",
    "2022-09-28"
  ))
)

AMC_trimmed <- AMC %>%
  left_join(date_ranges, by = "year") %>%
  filter(date >= start_date,
         date <= end_date) %>%
  select(-start_date, -end_date)

### AMC SUMMARY STATS ###
summary_stats <- AMC_trimmed %>%
  group_by(site.ID, year) %>%
  summarise(n_storms = n_distinct(storm.ID), .groups = "drop") %>%
  group_by(site.ID) %>%
  summarise(total_storms = sum(n_storms))

AMC_trimmed %>%
  group_by(site.ID, year) %>%
  summarise(n_storms = n_distinct(storm.ID), .groups = "drop")

AMC_trimmed <- AMC_trimmed %>% 
  group_by(site.ID, response_var, year) %>% 
  dplyr::summarise(meanHI = mean(Hyst_index, na.rm = TRUE),
                   meanBETA = mean(Beta_index, na.rm = TRUE),
                   sdHI = sd(Hyst_index, na.rm = TRUE),
                   sdBETA = sd(Beta_index, na.rm = TRUE))

AMC_trimmed <- AMC_trimmed %>% 
  mutate(PF = case_when(site.ID == "STRT" | site.ID == "VAUL" ~ "High", TRUE ~ "Moderate")) %>% 
  mutate(Burn = case_when(site.ID == "CARI" | site.ID == "VAUL" ~ "Unburned", TRUE ~ "Burned"))

AMC_trimmed <- AMC_trimmed %>% ungroup() %>% 
  filter(response_var != "abs")

# ---- 1. CLD per response_var ---------------------------------------------
get_cld_var <- function(df, rv) {
  sites <- unique(df$site.ID)
  if (length(sites) < 2) {
    return(tibble(response_var = rv, site.ID = sites, Label = "a"))
  }
  
  dunn <- df %>%
    dunn_test(meanHI ~ site.ID, p.adjust.method = "bonferroni")
  
  pvals   <- setNames(dunn$p.adj, paste(dunn$group1, dunn$group2, sep = "-"))
  letters <- multcompLetters(pvals)$Letters
  
  tibble(response_var = rv,
         site.ID      = names(letters),
         Label        = unname(letters))
}

rvs <- unique(AMC_trimmed$response_var)
cld_df <- map_dfr(rvs, function(rv) {
  df_rv <- AMC_trimmed %>% filter(response_var == rv)
  get_cld_var(df_rv, rv)
})

# ---- 2. y-positions for letters ------------------------------------------
desired_order <- c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL")

y_pos <- AMC_trimmed %>%
  group_by(response_var, site.ID) %>%
  summarise(y_position = max(meanHI, na.rm = TRUE) + 0.15, .groups = "drop")

annotations <- cld_df %>%
  left_join(y_pos, by = c("response_var", "site.ID")) %>%
  mutate(x_position = match(site.ID, desired_order))

# ---- 3. Optional: pretty facet labels ------------------------------------
AMC_trimmed$response_var <- factor(AMC_trimmed$response_var,
                                   levels = c("fDOM", "NO3", "SPC", "turb"),
                                   labels = c("fDOM",
                                              "NO[3]^{'-'}",
                                              "SPC",
                                              "Turbidity"))
annotations$response_var <- factor(annotations$response_var,
                                   levels = c("fDOM", "NO3", "SPC", "turb"),
                                   labels = c("fDOM",
                                              "NO[3]^{'-'}",
                                              "SPC",
                                              "Turbidity"))

# ---- 4. Plot -------------------------------------------------------------
library(ggpattern)

lvls <- c("fDOM", "NO[3]^{'-'}", "SPC", "Turbidity")
labs <- c("fDOM", "Nitrate", "SPC", "Turbidity")

AMC_trimmed$response_var <- factor(AMC_trimmed$response_var, levels = lvls, labels = labs)
annotations$response_var <- factor(annotations$response_var,
                                   levels = lvls, labels = labs)

spc_labels <- data.frame(
  response_var = factor("SPC", levels = levels(AMC_trimmed$response_var)),
  x    = 0.6,           # near left edge
  y    = c(0.1, -0.1),
  lab  = c("Clockwise", "Counterclockwise")
)

HI_box <- ggplot(AMC_trimmed, aes(x = site.ID, y = sdHI,
                                  fill = site.ID, pattern = Burn)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black",
                       pattern_fill    = "white",
                       pattern_angle   = 45,
                       pattern_density = 0.1,
                       pattern_spacing = 0.025,
                       pattern_key_scale_factor = 0.6) +
  geom_text(data = spc_labels,
            aes(x = x, y = y, label = lab),
            inherit.aes = FALSE, hjust = 0, fontface = "bold", size = 4) +
  scale_pattern_manual(values = c(Burned = "stripe", Unburned = "none"),
                       name = "") +
  scale_x_discrete(
    limits = c("POKE", "CARI", "STRT", "FRCH", "MOOS", "VAUL"),
    labels = c(POKE = "POKE\n(25%)",
               CARI = "CARI\n(29%)",
               STRT = "STRT\n(30%)",
               FRCH = "FRCH\n(33%)",
               MOOS = "MOOS\n(38%)",
               VAUL = "VAUL\n(58%)")
  ) +
  scale_fill_manual(values=c("#3288BD", "#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A")) +
  xlab("") +
  ylab("SD-HI") +
  theme_bw() +
  facet_wrap(~response_var) +
  geom_text(data = annotations,
            aes(x = x_position, y = y_position, label = Label),
            inherit.aes = FALSE, size = 6) +
  guides(fill = "none",
         pattern = guide_legend(override.aes = list(fill = "white"))) +
  theme_classic() +
  theme(legend.position = "bottom")

HI_box

ggsave("HI_all_solutes.line.pdf",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 6, height = 6)

ggsave("HI_all_solutes.line.png",
       path = here("Output_from_analysis", "20260626_plotting_roundup"),
       width = 6, height = 6)










