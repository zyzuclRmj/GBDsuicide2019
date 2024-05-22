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

# table 2. ASSR and percent-change between 1990 and 2019 among individuals age 60 and older in 5-year age groups.
tbl3_dthrate <- regions %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  group_by(location,year,sex) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  mutate(val_Both_Deaths_Rate = val_Both_Deaths_Rate/100,
         lower_Both_Deaths_Rate = lower_Both_Deaths_Rate/100,
         upper_Both_Deaths_Rate = upper_Both_Deaths_Rate/100,
         sd = if_else((upper_Both_Deaths_Rate - val_Both_Deaths_Rate) > (val_Both_Deaths_Rate - lower_Both_Deaths_Rate), upper_Both_Deaths_Rate - val_Both_Deaths_Rate, val_Both_Deaths_Rate - lower_Both_Deaths_Rate)) %>% 
  select(-c(val_Both_Deaths_Number,upper_Both_Deaths_Number,lower_Both_Deaths_Number)) %>% 
  pivot_wider(names_from = c("year"),
              values_from = c("val_Both_Deaths_Rate","upper_Both_Deaths_Rate",
                              "lower_Both_Deaths_Rate","sd"))

# percent change
# get mean and sd of asmr in 1990 and 2019
mu1 <- pull(tbl3_dthrate,val_Both_Deaths_Rate_1990)
mu2 <- pull(tbl3_dthrate,val_Both_Deaths_Rate_2019)

sd1 <- pull(tbl3_dthrate,sd_1990) 
sd2 <- pull(tbl3_dthrate,sd_2019)

# simulation
lst1 <- list()
lst2 <- list()

set.seed(1)
for (i in 1:dim(tbl3_dthrate)[1]) {
  lst1[[i]] <- sample(rnorm(1000,mean=mu1[i],sd=sd1[i]),
                      replace = FALSE)
  lst2[[i]] <- sample(rnorm(1000,mean=mu2[i],sd=sd2[i]),
                      replace=FALSE)
}

# percent change,variance and SD
pct_chg <- list()
pct_chg_mean <- rep(0,dim(tbl3_dthrate)[1])
pct_chg_var <- rep(0,dim(tbl3_dthrate)[1])
pct_chg_sd <- rep(0,dim(tbl3_dthrate)[1])

for (i in 1:dim(tbl3_dthrate)[1]) {
  pct_chg[[i]] <- (lst2[[i]] - lst1[[i]]) / lst1[[i]]
  pct_chg_mean[i] <- mean(pct_chg[[i]])
  pct_chg_var[i] <- var(pct_chg[[i]])
  pct_chg_sd[i] <- sqrt(pct_chg_var[i])
}

# p-value, null=0
p_value_pct_chg_std_dth_rate <- rep(0,dim(tbl3_dthrate)[1])

for (i in 1:dim(tbl3_dthrate)[1]) {
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

location <- pull(tbl3_dthrate,location)
age_grp <- pull(tbl3_dthrate,age)

tbl3_dthrate_pctchg <- tibble(location,
                              age_grp,
                              val_pct_chg_std_dth_rate,
                              upper_pct_chg_std_dth_rate,
                              lower_pct_chg_std_dth_rate,
                              p_value_pct_chg_std_dth_rate)

tbl3_dthrate_pctchg[,c(3:6)] <- round(tbl3_dthrate_pctchg[,c(3:6)],digits = 2)

tbl3_dthrate_pctchg <- tbl3_dthrate_pctchg %>% 
  select(-p_value_pct_chg_std_dth_rate) %>% 
  pivot_wider(names_from = "age_grp",
              values_from = c("val_pct_chg_std_dth_rate",
                              "lower_pct_chg_std_dth_rate",
                              "upper_pct_chg_std_dth_rate")) %>% 
  janitor::clean_names()

tbl3_dthrate_pctchg <- tbl3_dthrate_pctchg %>% 
  mutate(pct_chg_10_14_ui = paste0(val_pct_chg_std_dth_rate_10_14_years,
                                   " (",lower_pct_chg_std_dth_rate_10_14_years,
                                   ", ",upper_pct_chg_std_dth_rate_10_14_years,
                                   ")"),
         pct_chg_15_19_ui = paste0(val_pct_chg_std_dth_rate_15_19_years,
                                   " (",lower_pct_chg_std_dth_rate_15_19_years,
                                   ", ",upper_pct_chg_std_dth_rate_15_19_years,
                                   ")"),
         pct_chg_20_24_ui = paste0(val_pct_chg_std_dth_rate_20_24_years,
                                   " (",lower_pct_chg_std_dth_rate_20_24_years,
                                   ", ",upper_pct_chg_std_dth_rate_20_24_years,
                                   ")"),
         pct_chg_25_29_ui = paste0(val_pct_chg_std_dth_rate_25_29_years,
                                   " (",lower_pct_chg_std_dth_rate_25_29_years,
                                   ", ",upper_pct_chg_std_dth_rate_25_29_years,
                                   ")"),
         pct_chg_30_34_ui = paste0(val_pct_chg_std_dth_rate_30_34_years,
                                   " (",lower_pct_chg_std_dth_rate_30_34_years,
                                   ", ",upper_pct_chg_std_dth_rate_30_34_years,
                                   ")"),
         pct_chg_35_39_ui = paste0(val_pct_chg_std_dth_rate_35_39_years,
                                   " (",lower_pct_chg_std_dth_rate_35_39_years,
                                   ", ",upper_pct_chg_std_dth_rate_35_39_years,
                                   ")"),
         pct_chg_40_44_ui = paste0(val_pct_chg_std_dth_rate_40_44_years,
                                   " (",lower_pct_chg_std_dth_rate_40_44_years,
                                   ", ",upper_pct_chg_std_dth_rate_40_44_years,
                                   ")"),
         pct_chg_45_49_ui = paste0(val_pct_chg_std_dth_rate_45_49_years,
                                   " (",lower_pct_chg_std_dth_rate_45_49_years,
                                   ", ",upper_pct_chg_std_dth_rate_45_49_years,
                                   ")"),
         pct_chg_50_54_ui = paste0(val_pct_chg_std_dth_rate_50_54_years,
                                   " (",lower_pct_chg_std_dth_rate_50_54_years,
                                   ", ",upper_pct_chg_std_dth_rate_50_54_years,
                                   ")"),
         pct_chg_55_59_ui = paste0(val_pct_chg_std_dth_rate_55_59_years,
                                   " (",lower_pct_chg_std_dth_rate_55_59_years,
                                   ", ",upper_pct_chg_std_dth_rate_55_59_years,
                                   ")"),
         pct_chg_60_64_ui = paste0(val_pct_chg_std_dth_rate_60_64_years,
                                   " (",lower_pct_chg_std_dth_rate_60_64_years,
                                   ", ",upper_pct_chg_std_dth_rate_60_64_years,
                                   ")"),
         pct_chg_65_69_ui = paste0(val_pct_chg_std_dth_rate_65_69_years,
                                   " (",lower_pct_chg_std_dth_rate_65_69_years,
                                   ", ",upper_pct_chg_std_dth_rate_65_69_years,
                                   ")"),
         pct_chg_70_74_ui = paste0(val_pct_chg_std_dth_rate_70_74_years,
                                   " (",lower_pct_chg_std_dth_rate_70_74_years,
                                   ", ",upper_pct_chg_std_dth_rate_70_74_years,
                                   ")"),
         pct_chg_75_79_ui = paste0(val_pct_chg_std_dth_rate_75_79_years,
                                   " (",lower_pct_chg_std_dth_rate_75_79_years,
                                   ", ",upper_pct_chg_std_dth_rate_75_79_years,
                                   ")"),
         pct_chg_80_84_ui = paste0(val_pct_chg_std_dth_rate_80_84,
                                   " (",lower_pct_chg_std_dth_rate_80_84,
                                   ", ",upper_pct_chg_std_dth_rate_80_84,
                                   ")"),
         pct_chg_85_89_ui = paste0(val_pct_chg_std_dth_rate_85_89,
                                   " (",lower_pct_chg_std_dth_rate_85_89,
                                   ", ",upper_pct_chg_std_dth_rate_85_89,
                                   ")"),
         pct_chg_90_94_ui = paste0(val_pct_chg_std_dth_rate_90_94,
                                   " (",lower_pct_chg_std_dth_rate_90_94,
                                   ", ",upper_pct_chg_std_dth_rate_90_94,
                                   ")"),
         pct_chg_95_plus_ui = paste0(val_pct_chg_std_dth_rate_95_years,
                                     " (",lower_pct_chg_std_dth_rate_95_years,
                                     ", ",upper_pct_chg_std_dth_rate_95_years,
                                     ")")) %>% 
  select(location,pct_chg_10_14_ui,pct_chg_15_19_ui,pct_chg_20_24_ui,
         pct_chg_25_29_ui,pct_chg_30_34_ui,pct_chg_35_39_ui,pct_chg_40_44_ui,
         pct_chg_45_49_ui,pct_chg_50_54_ui,pct_chg_55_59_ui,pct_chg_60_64_ui,
         pct_chg_65_69_ui,pct_chg_70_74_ui,pct_chg_75_79_ui,pct_chg_80_84_ui,
         pct_chg_85_89_ui,pct_chg_90_94_ui,pct_chg_95_plus_ui)

tbl3_dthrate2 <- regions %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  group_by(location,year,sex) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = c("val","upper","lower")) %>%
  select(-c(val_Both_Deaths_Number,upper_Both_Deaths_Number,lower_Both_Deaths_Number)) %>% 
  mutate(val_Both_Deaths_Rate = round(val_Both_Deaths_Rate,digits = 2),
         upper_Both_Deaths_Rate = round(upper_Both_Deaths_Rate,digits = 2),
         lower_Both_Deaths_Rate = round(lower_Both_Deaths_Rate,digits = 2),
         asmr_95_ui = paste0(val_Both_Deaths_Rate," (",lower_Both_Deaths_Rate,
                             ", ",upper_Both_Deaths_Rate,")")) %>% 
  select(-c(val_Both_Deaths_Rate,upper_Both_Deaths_Rate,lower_Both_Deaths_Rate)) %>% 
  pivot_wider(names_from = c("year","age"),
              values_from = c("asmr_95_ui"))

merged_tbl3 <- left_join(tbl3_dthrate2,tbl3_dthrate_pctchg,
                         by="location") %>% 
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

# write.csv(merged_tbl3,"Table 2_ver2019.csv")

# Table 3. ASSR ratio for individuals age 60 and older by  5-year groups relative to individuals ages 10 to 59 years as well as the percent change in ASSR ratios between 1990 and 2019.
tbl4_dth_rratio <- regions %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  dplyr::group_by(location,year,sex) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(age=recode(age,"10-14 years" = "60 below",
                    "15-19 years" = "60 below",
                    "20-24 years" = "60 below",
                    "25-29 years" = "60 below",
                    "30-34 years" = "60 below",
                    "35-39 years" = "60 below",
                    "40-44 years" = "60 below",
                    "45-49 years" = "60 below",
                    "50-54 years" = "60 below",
                    "55-59 years" = "60 below"),
         age_grp_num = val_Both_Deaths_Number * 10^10 / (val_Both_Deaths_Rate/100),
         wider_sd = if_else((upper_Both_Deaths_Number - val_Both_Deaths_Number)>
                              (lower_Both_Deaths_Number - val_Both_Deaths_Number),
                            upper_Both_Deaths_Number,lower_Both_Deaths_Number),
         sd = (wider_sd - val_Both_Deaths_Number)*10^5 / 1.96 * sqrt(1000),
         var = sd^2) %>% 
  dplyr::group_by(location,year,age) %>% 
  dplyr::summarize(sum_dthnum = sum(val_Both_Deaths_Number*10^5),
            sum_age_grp_num = sum(age_grp_num), 
            sum_var = sum(var)) %>% 
  dplyr::mutate(val_asmr = sum_dthnum / sum_age_grp_num,
         var_asmr = sum_var / (1000 * sum_age_grp_num^2),
         sd_asmr = sqrt(var_asmr)) %>% 
  select(-c(sum_dthnum,sum_age_grp_num,sum_var,var_asmr,sd_asmr)) %>% 
  pivot_wider(names_from = "age",
              values_from = "val_asmr") %>% 
  dplyr::mutate(val_asmr_ratio_6460 = `60-64 years` / `60 below`,
         val_asmr_ratio_6960 = `65-69 years` / `60 below`,
         val_asmr_ratio_7460 = `70-74 years` / `60 below`,
         val_asmr_ratio_7960 = `75-79 years` / `60 below`,
         val_asmr_ratio_8460 = `80-84` / `60 below`,
         val_asmr_ratio_8960 = `85-89` / `60 below`,
         val_asmr_ratio_9460 = `90-94` / `60 below`,
         val_asmr_ratio_9560 = `95+ years` / `60 below`) %>% 
  select(-c(`60 below`,`60-64 years`,`65-69 years`,`70-74 years`,`75-79 years`,
            `80-84`,`85-89`,`90-94`,`95+ years`)) %>% 
  pivot_wider(names_from = "year",
              values_from = c("val_asmr_ratio_6460",
                              "val_asmr_ratio_6960",
                              "val_asmr_ratio_7460",
                              "val_asmr_ratio_7960",
                              "val_asmr_ratio_8460",
                              "val_asmr_ratio_8960",
                              "val_asmr_ratio_9460",
                              "val_asmr_ratio_9560")) %>% 
  dplyr::mutate(val_asmr_ratio_pct_chg_6460 = (val_asmr_ratio_6460_2019 - 
                                          val_asmr_ratio_6460_1990) * 100 / val_asmr_ratio_6460_1990,
         val_asmr_ratio_pct_chg_6960 = (val_asmr_ratio_6960_2019 - 
                                          val_asmr_ratio_6960_1990) * 100 / val_asmr_ratio_6960_1990,
         val_asmr_ratio_pct_chg_7460 = (val_asmr_ratio_7460_2019 - 
                                          val_asmr_ratio_7460_1990) * 100 / val_asmr_ratio_7460_1990,
         val_asmr_ratio_pct_chg_7960 = (val_asmr_ratio_7960_2019 - 
                                          val_asmr_ratio_7960_1990) * 100 / val_asmr_ratio_7960_1990,
         val_asmr_ratio_pct_chg_8460 = (val_asmr_ratio_8460_2019 - 
                                          val_asmr_ratio_8460_1990) * 100 / val_asmr_ratio_8460_1990,
         val_asmr_ratio_pct_chg_8960 = (val_asmr_ratio_8960_2019 - 
                                          val_asmr_ratio_8960_1990) * 100 / val_asmr_ratio_8960_1990,
         val_asmr_ratio_pct_chg_9460 = (val_asmr_ratio_9460_2019 - 
                                          val_asmr_ratio_9460_1990) * 100 / val_asmr_ratio_9460_1990,
         val_asmr_ratio_pct_chg_9560 = (val_asmr_ratio_9560_2019 - 
                                          val_asmr_ratio_9560_1990) * 100 / val_asmr_ratio_9560_1990)

tbl4_dth_rratio[,c(2:25)] <- tbl4_dth_rratio[,c(2:25)] %>% 
  round(digits = 2)

location <- tbl3_dthrate2[,1]

tbl4_dth_rratio <- left_join(location,tbl4_dth_rratio,by="location")

# write.csv(tbl4_dth_rratio,"Table 3_ver2019.csv")



