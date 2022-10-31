#### READ ME ####
# The purpose of this script is to make final plots for my thesis that may be pulled from other scripts
  # I will try to make good comments to see what needs to be loaded in before the plotting 

# load libraries
library(tidyverse)
library(stats)
library(readr)
library(ggplot2)
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

# 10_gls_models plotting ####
mean_daily <- read.csv(here("Output_from_analysis", "08_Catchment_characteristics", "mean_daily.csv"))

mean_daily$datetimeAK <- ymd(mean_daily$day)
mean_daily <- mean_daily[order(mean_daily$year, mean_daily$site.ID), ]

mean_daily$year <- as.character(mean_daily$year)

mean_daily$Burn <- NA

mean_daily <- mean_daily %>% 
  mutate(across(c(Burn),
                ~ifelse(site.ID == "CARI" | site.ID == "FRCH" | site.ID == "VAUL", "unburned", "burned")))

mean_daily$PF <- NA

mean_daily <- mean_daily %>% 
  mutate(across(c(PF),
                ~ifelse(site.ID == "VAUL" | site.ID == "STRT", "High", "Moderate")))


vn = expression(paste(N*O[3]^"-"))
# mean concentration across years for each site 
mean_daily_site <- mean_daily %>% 
  group_by(site.ID) %>% 
  dplyr::summarise(meanNO3 = mean(dailyNO3, na.rm = TRUE),
                   meanfDOM = mean(dailyfDOM, na.rm = TRUE),
                   meanSPC = mean(dailySPC, na.rm = TRUE),
                   meanTurb = mean(dailyTurb, na.rm = TRUE))


mean_daily_long <- mean_daily %>%
  pivot_longer(
    cols = starts_with("daily"),
    names_to = "response_var",
    names_prefix = "wk",
    values_to = "concentration") # converting to a long format so each response_var is within a single column


mean_daily_long$response_var <- factor(mean_daily_long$response_var, levels = c("dailyfDOM", "dailyNO3", 
                                                                                "dailySPC", "dailyTurb"), 
                  labels = c("fDOM (QSU)", "NO3- (µM)", "SPC(µS/cm)", "Turbidity (FNU)"))

# mean_daily_long$year <- factor(df$supp, levels = c("OJ", "VC"),
#                   labels = c("Orange Juice", "Vitamin C")
#                   
# 
# # New facet label names for dose variable
# dose.labs <- c("D0.5", "D1", "D2")
# names(dose.labs) <- c("0.5", "1", "2")
# 
# # New facet label names for supp variable
# supp.labs <- c("Orange Juice", "Vitamin C")
# names(supp.labs) <- c("OJ", "VC")
# 
# # Create the plot
# p + facet_grid(
#   dose ~ supp, 
#   labeller = labeller(dose = dose.labs, supp = supp.labs)
# )



ggplot(mean_daily_long, aes(x = julian, y = concentration, color = site.ID)) +
  geom_line(size = 0.5) +
  scale_color_manual(values=c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A"), "Catchment") +
  facet_grid(response_var~year, scales = "free") +
  xlab("Julian Day")  +
  ylab("Concentration") +
  theme_classic() +
  theme(axis.text.x=element_text(size=15), 
       axis.text.y = element_text(size = 15),
       axis.title.x = element_text(size = 20),
       axis.title.y = element_text(size = 20),
       legend.key.size = unit(2.0, 'cm'),
       strip.text = element_text(size = 20))

ggsave("All_years.pdf",
       path = here("plots", "Time_Series"),
       width = 12, height = 10)




# 12_Catchment Characteristics plotting ####
# You have to run 12_catchment_characteristics script to define these in the environment

# NO3
gA <- ggplotGrob(no3.hi.burn)
gB <- ggplotGrob(no3.beta.deciduous)
gC <- ggplotGrob(no3.sdhi.slope)
gD <- ggplotGrob(no3.sdbeta.burn)

grid::grid.newpage()
grid.draw(cbind(rbind(gA, gB, size = "max"),
                rbind(gC, gD, size = "max")))
fig <- arrangeGrob(cbind(rbind(gA, gB, size = "max"),
                         rbind(gC, gD, size = "max")))


# fDOM
gA <- ggplotGrob(fdom.hi.slope)
gB <- ggplotGrob(fDOM.beta.slope)
gC <- ggplotGrob(fDOM.sdhi.burn)
gD <- ggplotGrob(fDOM.sdbeta.deciduous)


grid::grid.newpage()
grid.draw(cbind(rbind(gA, gB, size = "max"),
                rbind(gC, gD, size = "max")))
fig <- arrangeGrob(cbind(rbind(gA, gB, size = "max"),
                         rbind(gC, gD, size = "max")))

# SPC
gA <- ggplotGrob(spc.hi.slope)
gB <- ggplotGrob(spc.beta.slope)
gC <- ggplotGrob(spc.sdhi.burn)
gD <- ggplotGrob(spc.sdbeta.burn)


grid::grid.newpage()
grid.draw(cbind(rbind(gA, gB, size = "max"),
                rbind(gC, gD, size = "max")))
fig <- arrangeGrob(cbind(rbind(gA, gB, size = "max"),
                         rbind(gC, gD, size = "max")))

# turb
gA <- ggplotGrob(turb.hi.burn)
gB <- ggplotGrob(turb.beta.deciduous)
gC <- ggplotGrob(turb.sdhi.deciduous)
gD <- ggplotGrob(turb.sdbeta.slope)


grid::grid.newpage()
grid.draw(cbind(rbind(gA, gB, size = "max"),
                rbind(gC, gD, size = "max")))
fig <- arrangeGrob(cbind(rbind(gA, gB, size = "max"),
                         rbind(gC, gD, size = "max")))


#














