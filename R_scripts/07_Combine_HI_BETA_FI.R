### The purpose of this script is to combine all HI, BETA, FI into a singular dataframe so I can do my model analysis




########################################## COMBINE YEARS ################################################
# Load in 2018 #
# HI #

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
FRCH.HI.df <- read.csv("~/Documents/Storms_clean_repo/Output_from_analysis/03_HI_FI/2018/FRCH/FRCH.HI.df.csv")

storm.list = unique(FRCH.HI.df$storm.ID)
FRCH.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(FRCH.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
FRCH.HI.boot$storm.ID = storm.list

# MOOS #
MOOS.HI.df <- read.csv("~/Documents/Storms_clean_repo/Output_from_analysis/03_HI_FI/2018/MOOS/MOOS.HI.df.csv")

storm.list = unique(MOOS.HI.df$storm.ID)
MOOS.HI.boot <- do.call(rbind.data.frame,
                        lapply(storm.list, function(i){
                          dat = subset(MOOS.HI.df, storm.ID == i)
                          median_cl_boot(dat$HI)
                        }))
MOOS.HI.boot$storm.ID = storm.list

# CARI #
CARI.HI.df <- read.csv("~/Documents/Storms_clean_repo/Output_from_analysis/03_HI_FI/2018/CARI/CARI.HI.df.csv")

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

all.FI.diff.results = read.csv("~/Documents/Storms_clean_repo/Output_from_analysis/05_FI/all.FI.diff.results_2018.csv", header = T, row.names = 1)

FI = subset(all.FI.diff.results, select=c("Flushing_index", "percCI_2.5", "percCI_97.5", "ID"))
FI$ID = as.character(FI$ID)
FI = separate(FI, ID, into=c("site.ID", "storm.ID", "month", "day", "response_var", NA), sep = "_")
names(FI) = c("Flush_index", "FI_ymin", "FI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI$site.ID=NULL
HI = separate(HI, storm.ID, into=c("site.ID", "storm.ID", "month", "day", "response_var"), sep = "_")
names(HI) = c("Hyst_index", "HI_ymin", "HI_ymax","site.ID", "storm.ID", "month", "day", "response_var")

HI_FI = left_join(HI, FI, by=c("site.ID", "storm.ID", "response_var"))
HI_FI$year <- "2018"
write.csv(HI_FI, "~/Documents/Storms_clean_repo/Output_from_analysis/07_Combine_HI_BETA_FI/HI_FI.diff_results.2018.csv")

### BETA ###
beta_2018 <- read_csv("~/Documents/Storms_clean_repo/Output_from_analysis/06_BETA/beta.2018.csv")
beta_2018$year <- "2018"

beta_2018 <- beta_2018 %>% 
  filter(Parameter != "(Intercept)")

names(beta_2018) = c("X1", "site.ID", "storm.ID","Parameter",
                     "Beta_index", "SE", "CI", "Beta_ymin",
                     "Beta_ymax", "t", "df", "p", "response_var",
                     "year")

### Antecedent Conditions ###
antecedent_2018 <- read_csv("~/Documents/Storms_clean_repo/Output_from_analysis/04_Antecedent_Conditions/2018/HI.2018.csv")
antecedent_2018 <- antecedent_2018[,-c(1)]

names(antecedent_2018)[names(antecedent_2018) == "storm.num"] <- "storm.ID"
names(antecedent_2018)[names(antecedent_2018) == "response"] <- "response_var"
antecedent_2018$year <- as.character(antecedent_2018$year)
# merge #
HI_FI = left_join(HI_FI, beta_2018, by=c("site.ID", "storm.ID", "response_var", "year"))
HI_FI <- left_join(HI_FI, antecedent_2018, by = c("site.ID", "storm.ID", "response_var", "year"))

write.csv(HI_FI, "~/Documents/Storms_clean_repo/Output_from_analysis/07_Combine_HI_BETA_FI/antecedent_HI_FI_2018.csv")



# # merge #
# HI_FI_2018 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results.2018.csv")
# HI_FI_2018$year <- "2018"
# HI_FI_2019 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results_2019.csv")
# HI_FI_2019$year <- "2019"
# HI_FI_2020 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results_2020.csv")
# HI_FI_2020$year <- "2020"
# HI_FI_2021 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_FI.diff_results_2021.csv")
# HI_FI_2021$year <- "2021"
# 
# beta_2018 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/beta/beta.2018.csv")
# beta_2018$year <- "2018"
# beta_2019 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/beta/beta.2019.csv")
# beta_2019$year <- "2019"
# beta_2020 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/beta/beta.2020.csv")
# beta_2020$year <- "2020"
# beta_2021 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/beta/beta.2021.csv")
# beta_2021$year <- "2021"
# 
# antecedent_2018 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2018.csv")
# antecedent_2018 <- antecedent_2018[,-c(1:2)]
# antecedent_2019 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI.2019.csv")
# antecedent_2019 <- antecedent_2019[,-c(1:2)]
# antecedent_2020 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_antecedent_conditions/All_years/HI.2020.csv")
# antecedent_2020 <- antecedent_2020[,-c(1:2)]
# antecedent_2021 <- read.csv("~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/HI_antecedent_conditions/All_years/HI.2021.csv")
# antecedent_2021 <- antecedent_2021[,-c(1:3)]
# 
# HI_FI <- rbind(HI_FI_2018, HI_FI_2019, HI_FI_2020, HI_FI_2021)
# 
# beta_all <- rbind(beta_2018, beta_2019, beta_2020, beta_2021)
# beta_all <- beta_all %>% 
#   filter(Parameter != "(Intercept)")
# antecedent_all <- rbind(antecedent_2018, antecedent_2019, antecedent_2020, antecedent_2021)
# names(antecedent_all)[names(antecedent_all) == "storm.num"] <- "storm.ID"
# names(antecedent_all)[names(antecedent_all) == "response"] <- "response_var"
# antecedent_all$year <- as.character(antecedent_all$year)
# 
# HI_FI = left_join(HI_FI, beta_all, by=c("site.ID", "storm.ID", "response_var", "year"))
# HI_FI <- left_join(HI_FI, antecedent_all, by = c("site.ID", "storm.ID", "response_var", "year"))
# 
# write.csv(HI_FI, "~/Documents/Storms/Output_from_analysis/06_HI_fire_permafrost_script/antecedent_HI_FI_1.0.csv")
# 
# # NO3 #
# HI_FI_NO3 = subset(HI_FI, response_var == "NO3")
# HI_FI_NO3$site.ID <- factor(HI_FI_NO3$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL', 'CARI'))
# 
# HI_FI_NO3.p = 
#   ggplot(HI_FI_NO3, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID), shape = pf), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(6)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("a) NO3-")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_NO3.p
# 
# # fDOM #
# HI_FI_fDOM = subset(HI_FI, response_var == "fDOM")
# HI_FI_fDOM$site.ID <- factor(HI_FI_fDOM$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL', 'CARI'))
# 
# HI_FI_fDOM.p = 
#   ggplot(HI_FI_fDOM, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID), shape = pf), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(6)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("b) fDOM")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_fDOM.p
# 
# # SPC #
# HI_FI_SPC = subset(HI_FI, response_var == "SPC")
# HI_FI_SPC$site.ID <- factor(HI_FI_SPC$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL', 'CARI'))
# 
# HI_FI_SPC.p = 
#   ggplot(HI_FI_SPC, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID), shape = pf), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(6)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("c) SPC")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_SPC.p
# 
# # turb #
# HI_FI_turb = subset(HI_FI, response_var == "turb")
# HI_FI_turb$site.ID <- factor(HI_FI_turb$site.ID, levels = c('FRCH','MOOS','POKE','STRT','VAUL', 'CARI'))
# 
# HI_FI_turb.p = 
#   ggplot(HI_FI_turb, aes(Flush_index, Hyst_index)) + geom_point(aes(colour=factor(site.ID), shape = pf), size = 4)+
#   geom_errorbar(aes(ymin=HI_ymin, ymax=HI_ymax), colour="black", alpha=0.5, size=.5, width = 0.1)+ 
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
#   geom_errorbarh(aes(xmin=FI_ymin, xmax=FI_ymax), colour="black", alpha=0.5, size=.5, height = 0.1) +
#   scale_color_manual(values = c("orange red", viridis::viridis(6)), "Catchment")+theme_bw() +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5)+
#   ggtitle("d) Turb")+ 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 20)) 
# HI_FI_turb.p
# 
# 
# grid.arrange(HI_FI_NO3.p,HI_FI_fDOM.p,HI_FI_SPC.p,HI_FI_turb.p)
# 
# 
# ggplot(HI_FI_NO3, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
#   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
#   stat_poly_eq(formula = y~x,
#                label.y = "top", label.x = "right",
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE)
# 
# ggplot(HI_FI_fDOM, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
#   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
#   stat_poly_eq(formula = y~x,
#                label.y = "top", label.x = "right",
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE)
# 
# ggplot(HI_FI_SPC, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
#   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
#   stat_poly_eq(formula = y~x,
#                label.y = "top", label.x = "right",
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE)
# 
# ggplot(HI_FI_turb, aes(Flush_index, beta)) + geom_point(aes(colour=factor(site.ID)), size = 4) +
#   ylim(-1.5, 1.5) + xlim(-1.5, 1.5) +
#   geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE, aes(group = 1)) + 
#   stat_poly_eq(formula = y~x,
#                label.y = "top", label.x = "right",
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE)