# the script for manuscript
# import library

library(tidyverse)
library(ggmap)
library(RColorBrewer)
library(scales)
library(mapproj)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(plyr)
library(dplyr)
library(gridExtra)
library(patchwork)
library(extrafont)
loadfonts()
Sys.setenv(R.GSCMD = "C:/Program Files/gs/gs9.52/bin/gswin64c.exe")
library(ggrepel)
library(ggpubr)
library(patchwork)

# import datasets
country1 <- read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/countries and terrorities/IHME-GBD_2019_DATA-17f54449-1.csv") %>% 
  janitor::clean_names()

country2 <- read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/countries and terrorities/IHME-GBD_2019_DATA-17f54449-2.csv") %>% 
  janitor::clean_names()

country3 <- read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/countries and terrorities/IHME-GBD_2019_DATA-17f54449-3.csv") %>% 
  janitor::clean_names()

global <- read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/countries and terrorities/global.csv") %>% 
  janitor::clean_names()

country <- rbind(country1,country2,country3,global)

regions <- read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/regions.csv") %>% 
  janitor::clean_names()

super_regions <- 
  read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/super_regions.csv") %>% 
  janitor::clean_names()

ctry_abbr <- read_csv("C:/Users/zzhao/Desktop/global suicide/data/countries abbr.csv") %>% 
  janitor::clean_names()

sdi <- read_csv("C:/Users/zzhao/Desktop/global suicide/data2019/sdi.csv")

# Table 1. Number of OA suicides, ASSR per 100,000, percent-change from 1990 to 2019 with UI, and OA-NOA suicide ratios in 2019 for all regions.
detach(package:plyr)

# total dth number
table1_dthnum <- regions %>% 
  filter(measure=="Deaths" & metric=="Number" & sex=="Both" &
           year %in% c(1990,2019)) %>% 
  group_by(location,year,sex) %>% 
  mutate(wider_sd = if_else((upper-val)>(lower-val),upper,lower),
         sd = (wider_sd - val) / 1.96 * sqrt(1000),
         var = sd^2) %>% 
  dplyr::summarize(val = sum(val),
                   sum_var = sum(var)) %>% 
  mutate(upper = val + 1.96 * sqrt(sum_var) / sqrt(1000),
         lower = val - 1.96 * sqrt(sum_var) / sqrt(1000)) %>%
  select(-sum_var) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = c("val","upper","lower"))

# change the column names
colnames(table1_dthnum)[2:7] <-
  paste0(colnames(table1_dthnum)[2:7],"_all_age_deaths_number")

table1_dthnum <- table1_dthnum %>% janitor::clean_names()

# round to 1 decimal point
table1_dthnum[,2:7] <- round(table1_dthnum[,2:7],digits = 0)

# change the format
table1_dthnum <- table1_dthnum %>% 
  mutate(dth_num_1990_ui = paste0(val_1990_both_all_age_deaths_number,
                                  " (",lower_1990_both_all_age_deaths_number,
                                  ", ",upper_1990_both_all_age_deaths_number,
                                  ")"),
         dth_num_2019_ui = paste0(val_2019_both_all_age_deaths_number,
                                  " (",lower_2019_both_all_age_deaths_number,
                                  ", ",upper_2019_both_all_age_deaths_number,
                                  ")")) %>% 
  select(-c(val_1990_both_all_age_deaths_number,
            val_2019_both_all_age_deaths_number,
            upper_1990_both_all_age_deaths_number,
            lower_1990_both_all_age_deaths_number,
            upper_2019_both_all_age_deaths_number,
            lower_2019_both_all_age_deaths_number))

# ASMR
table1_asmr <- regions %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  mutate(age_grp_num = val_Deaths_Number * 10^10 / (val_Deaths_Rate/100),
         wider_sd = if_else((upper_Deaths_Number - val_Deaths_Number)>
                              (lower_Deaths_Number - val_Deaths_Number),
                            upper_Deaths_Number,lower_Deaths_Number),
         sd = (wider_sd - val_Deaths_Number) / 1.96 * sqrt(1000),
         var = sd^2) %>% 
  group_by(location,year,sex) %>% 
  dplyr::summarize(sum_age_grp_num = sum(age_grp_num),
                   val = sum((val_Deaths_Rate/100)*(age_grp_num/sum_age_grp_num)),
                   sum_var = sum(var)) %>% 
  mutate(sd = 10^10 * sqrt(sum_var) / (sum_age_grp_num * sqrt(1000)),
         upper = val + 1.96 * sd,
         lower = val - 1.96 * sd) %>% 
  select(-c(sum_age_grp_num,sum_var)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = c("val","lower","upper","sd"))

# change the column names
colnames(table1_asmr)[2:9] <- 
  paste0(colnames(table1_asmr)[2:9],"_std_dth_rate")

table1_asmr <- table1_asmr %>% janitor::clean_names()

# older to younger ASMR ratio
table1_asmr_ratio <- regions %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  mutate(age_grp_num = val_Deaths_Number * 100000 / ((val_Deaths_Rate/100) / 100000),
         age_elder = if_else(age %in% c("10-14 years","15-19 years",
                                        "20-24 years","25-29 years",
                                        "30-34 years","35-39 years",
                                        "40-44 years","45-49 years",
                                        "50-54 years","55-59 years"),
                             "10-59 years","60 plus")) %>% 
  group_by(location,year,sex,age_elder) %>% 
  mutate(sum_age_grp_num = sum(age_grp_num),
         std_prop_wt = age_grp_num / sum_age_grp_num) %>% 
  dplyr::summarize(std_dth_rate = sum((val_Deaths_Rate/100)*std_prop_wt)) %>% 
  pivot_wider(names_from = "age_elder",
              values_from = "std_dth_rate") %>% 
  mutate(std_dth_rratio = `60 plus` / `10-59 years`) %>% 
  select(-c(`10-59 years`,`60 plus`)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = "std_dth_rratio")

# change the column names
colnames(table1_asmr_ratio)[2:3] <- 
  paste0("val_",colnames(table1_asmr_ratio)[2:3],
         "_std_dth_rate_ratio")

table1_asmr_ratio <- table1_asmr_ratio %>% janitor::clean_names()

# round to 2 decimal points
table1_asmr_ratio[,2:3] <- round(table1_asmr_ratio[2:3], digits = 2)

# percent change
# get mean and sd of asmr in 1990 and 2019
mu1 <- pull(table1_asmr,val_1990_both_std_dth_rate)
mu2 <- pull(table1_asmr,val_2019_both_std_dth_rate)

sd1 <- pull(table1_asmr,sd_1990_both_std_dth_rate) 
sd2 <- pull(table1_asmr,sd_2019_both_std_dth_rate)

# simulation
lst1 <- list()
lst2 <- list()

set.seed(1)
for (i in 1:46) {
  lst1[[i]] <- sample(rnorm(1000,mean=mu1[i],sd=sd1[i]),
                      replace = FALSE)
  lst2[[i]] <- sample(rnorm(1000,mean=mu2[i],sd=sd2[i]),
                      replace=FALSE)
}

# percent change,variance and SD
pct_chg <- list()
pct_chg_mean <- rep(0,46)
pct_chg_var <- rep(0,46)
pct_chg_sd <- rep(0,46)

for (i in 1:46) {
  pct_chg[[i]] <- (lst2[[i]] - lst1[[i]]) / lst1[[i]]
  pct_chg_mean[i] <- mean(pct_chg[[i]])
  pct_chg_var[i] <- var(pct_chg[[i]])
  pct_chg_sd[i] <- sqrt(pct_chg_var[i])
}

# p-value, null=0
p_value_pct_chg_std_dth_rate <- rep(0,46)

for (i in 1:46) {
  p_value_pct_chg_std_dth_rate[i] <- t.test(x=pct_chg[[i]],mu=0,
                                            alternative = "two.sided")$p.value
}
# since the p-value is very approximate to 0, it's shown as 0
# here is an example
t.test(x=pct_chg[[1]],mu=0,alternative = "two.sided")

# create tibble for percent change and its 95% UI
val_pct_chg_std_dth_rate <- (mu2 - mu1) / mu1
upper_pct_chg_std_dth_rate <- val_pct_chg_std_dth_rate + 1.96 * 
  (pct_chg_sd / sqrt(1000))
lower_pct_chg_std_dth_rate <- val_pct_chg_std_dth_rate - 1.96 * 
  (pct_chg_sd / sqrt(1000))

val_pct_chg_std_dth_rate <- val_pct_chg_std_dth_rate * 100
upper_pct_chg_std_dth_rate <- upper_pct_chg_std_dth_rate * 100
lower_pct_chg_std_dth_rate <- lower_pct_chg_std_dth_rate * 100

location <- pull(table1_asmr,location)

table1_asmr_pctchg <- tibble(location,
                             val_pct_chg_std_dth_rate,
                             upper_pct_chg_std_dth_rate,
                             lower_pct_chg_std_dth_rate,
                             p_value_pct_chg_std_dth_rate)

# convert table1_asmr into percentage
table1_asmr[2:9] <- table1_asmr[2:9] * 100

# round asmr to 2 decimal points
table1_asmr[2:9] <- round(table1_asmr[2:9],digits = 2)

# round pct change to 3 decimal points
table1_asmr_pctchg[2:5] <- round(table1_asmr_pctchg[2:5],digits = 2)

# change the format
table1_asmr <- table1_asmr %>% 
  mutate(asmr_1990_ui = paste0(val_1990_both_std_dth_rate,
                               " (",lower_1990_both_std_dth_rate,
                               ", ", upper_1990_both_std_dth_rate,
                               ")"),
         asmr_2019_ui = paste0(val_2019_both_std_dth_rate,
                               " (",lower_2019_both_std_dth_rate,
                               ", ",upper_2019_both_std_dth_rate,
                               ")")) %>% 
  select(-c(val_1990_both_std_dth_rate,lower_1990_both_std_dth_rate,
            upper_1990_both_std_dth_rate,val_2019_both_std_dth_rate,
            lower_2019_both_std_dth_rate,upper_2019_both_std_dth_rate,
            sd_1990_both_std_dth_rate,sd_2019_both_std_dth_rate))

table1_asmr_pctchg <- table1_asmr_pctchg %>% 
  mutate(pct_chg_ui = paste0(val_pct_chg_std_dth_rate,
                             " (",lower_pct_chg_std_dth_rate,
                             ", ",upper_pct_chg_std_dth_rate,")")) %>% 
  select(-c(val_pct_chg_std_dth_rate,
            lower_pct_chg_std_dth_rate,
            upper_pct_chg_std_dth_rate))

# merge tables together
merged_tbl1 <- left_join(table1_dthnum,table1_asmr,by="location") %>% 
  left_join(.,table1_asmr_pctchg,by="location") %>% 
  left_join(.,table1_asmr_ratio,by="location") %>% 
  filter(location %in% c("Global","East Asia","Southeast Asia",
                         "Oceania","Central Asia","Central Europe",
                         "Eastern Europe","High-income Asia Pacific",
                         "Australasia","Western Europe",
                         "Southern Latin America","High-income North America",
                         "Caribbean","Andean Latin America",
                         "Central Latin America","Tropical Latin America",
                         "North Africa and Middle East",
                         "South Asia","Central Sub-Saharan Africa",
                         "Eastern Sub-Saharan Africa",
                         "Southern Sub-Saharan Africa",
                         "Western Sub-Saharan Africa"))

# write.csv(merged_tbl1,"Table1_ver2019.csv")





