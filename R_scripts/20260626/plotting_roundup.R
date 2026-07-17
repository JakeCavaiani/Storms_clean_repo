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
       gsheet)


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

# Read in Q for each year (2018, 2019, 2020, 2021, 2022) and take mean daily Q
# Build daily Q from yearly files
Q_daily <- list.files("Q/Q_chem", pattern = "^DOD\\.\\d{4}\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x) %>% select(datetimeAK, site.ID, Q)) %>%
  mutate(day = as.Date(datetimeAK, tz = "America/Anchorage")) %>%
  group_by(site.ID, day) %>%
  summarise(Q = mean(Q, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = as.numeric(format(day, "%Y")))

Q_daily <- Q_daily %>% filter(!is.na(site.ID))

# Merge into mean_daily (keep mean_daily as the main data frame)
DOD_Q_daily <- mean_daily_chem %>%
  left_join(Q_daily, by = c("day", "site.ID", "year"))

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

# combine NEON and DOD chem 
mean_daily_chem_all <- bind_rows(DOD_Q_daily, mean_daily_neon) %>%
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
                                       labels = c("Q (l/sec)", "fDOM (QSU)", "NO3- (µM)", "SPC(µS/cm)", "Turbidity (FNU)"))


# Plotting #
mean_daily_fig_data_long %>%
  ggplot(aes(x = day, y = concentration, color = site.ID)) +
  geom_line() +
  scale_color_manual(values = c("#3288BD","#FF7F00", "#A6761D", "#6A3D9A", "#66C2A5", "#E7298A"), 
                     guide = guide_legend(title = "Site")) +
  xlab("") +
  ylab("") +
  facet_grid(response_var~year, scales = "free") +
  theme_classic() +
  theme(strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 13, angle = -45, hjust = 1),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

ggsave("DoD_2015_2022.pdf",
       path = here("plots", "20260626_plotting_roundup"),
       width = 12, height = 8, units = "in")

ggsave("DoD_2015_2022.png",
       path = here("plots", "20260626_plotting_roundup"),
       width = 10, height = 8, units = "in")


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

# Read in Q for each year (2018, 2019, 2020, 2021, 2022) and take mean daily Q
# Build daily Q from yearly files
Q_daily <- list.files("Q/Q_chem", pattern = "^DOD\\.\\d{4}\\.csv$", full.names = TRUE) %>%
  map_dfr(~ read_csv(.x) %>% select(datetimeAK, site.ID, Q)) %>%
  mutate(day = as.Date(datetimeAK, tz = "America/Anchorage")) %>%
  group_by(site.ID, day) %>%
  summarise(Q = mean(Q, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = as.numeric(format(day, "%Y")))

Q_daily <- Q_daily %>% filter(!is.na(site.ID))

# Merge into mean_daily (keep mean_daily as the main data frame)
DOD_Q_daily <- mean_daily_chem %>%
  left_join(Q_daily, by = c("day", "site.ID", "year"))

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

# combine NEON and DOD chem 
mean_daily_chem_all <- bind_rows(DOD_Q_daily, mean_daily_neon) %>%
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

ggsave(Q, path = here("plots", "20260626_plotting_roundup"),
       file = paste0("Q_boxplot_letter_", Sys.Date(), ".pdf"),
       width = 12, height = 8, units = "in")

ggsave(Q, path = here("plots", "20260626_plotting_roundup"),
       file = paste0("Q_boxplot_letter_", Sys.Date(), ".jpg"),
       width = 12, height = 8, units = "in")




















