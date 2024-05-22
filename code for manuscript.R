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
  dplyr::group_by(location,year,sex) %>% 
  dplyr::mutate(wider_sd = if_else((upper-val)>(lower-val),upper,lower),
         sd = (wider_sd - val) / 1.96 * sqrt(1000),
         var = sd^2) %>% 
  dplyr::summarize(val = sum(val),
                   sum_var = sum(var)) %>% 
  dplyr::mutate(upper = val + 1.96 * sqrt(sum_var) / sqrt(1000),
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
  dplyr::mutate(dth_num_1990_ui = paste0(val_1990_both_all_age_deaths_number,
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
  dplyr::mutate(age_grp_num = val_Deaths_Number * 10^10 / (val_Deaths_Rate/100),
         wider_sd = if_else((upper_Deaths_Number - val_Deaths_Number)>
                              (lower_Deaths_Number - val_Deaths_Number),
                            upper_Deaths_Number,lower_Deaths_Number),
         sd = (wider_sd - val_Deaths_Number) / 1.96 * sqrt(1000),
         var = sd^2) %>% 
  group_by(location,year,sex) %>% 
  dplyr::summarize(sum_age_grp_num = sum(age_grp_num),
                   val = sum((val_Deaths_Rate/100)*(age_grp_num/sum_age_grp_num)),
                   sum_var = sum(var)) %>% 
  dplyr::mutate(sd = 10^10 * sqrt(sum_var) / (sum_age_grp_num * sqrt(1000)),
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
  dplyr::mutate(age_grp_num = val_Deaths_Number * 100000 / ((val_Deaths_Rate/100) / 100000),
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
  dplyr::mutate(std_dth_rratio = `60 plus` / `10-59 years`) %>% 
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
  dplyr::mutate(asmr_1990_ui = paste0(val_1990_both_std_dth_rate,
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
  dplyr::mutate(pct_chg_ui = paste0(val_pct_chg_std_dth_rate,
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
  dplyr::group_by(location,year,sex) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(val_Both_Deaths_Rate = val_Both_Deaths_Rate/100,
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
  dplyr::mutate(pct_chg_10_14_ui = paste0(val_pct_chg_std_dth_rate_10_14_years,
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
  dplyr::group_by(location,year,sex) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = c("val","upper","lower")) %>%
  select(-c(val_Both_Deaths_Number,upper_Both_Deaths_Number,lower_Both_Deaths_Number)) %>% 
  dplyr::mutate(val_Both_Deaths_Rate = round(val_Both_Deaths_Rate,digits = 2),
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

# Figure 1. OA ASSR by super regions, age groups and gender in 2019.
fig5_tbl <- super_regions %>% 
  filter(measure=="Deaths" & metric=="Rate" & year==2019 & 
           age %in% c("60-64 years","65-69 years","70-74 years",
                      "75-79 years","80-84 years","85-89 years",
                      "90-94 years","95+ years") & 
           location %in% c("South Asia","Latin America and Caribbean",
                           "North Africa and Middle East",
                           "Central Europe, Eastern Europe, and Central Asia",
                           "High-income",
                           "Southeast Asia, East Asia, and Oceania",
                           "Sub-Saharan Africa",
                           "Global")) %>% 
  select(-c(cause,measure,metric,upper,lower))

wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=20),collapse=" \n ")
  return(wtext)
}

fig5_tbl$location <- plyr::llply(fig5_tbl$location, wrapit)
fig5_tbl$location <- unlist(fig5_tbl$location)

fig5_tbl <- fig5_tbl %>% 
  dplyr::mutate(location = factor(location,
                           c("Global",
                             "South Asia",
                             "Latin America and \n Caribbean",
                             "North Africa and \n Middle East",
                             "Central Europe, \n Eastern Europe, and \n Central Asia",
                             "High-income",
                             "Southeast Asia, \n East Asia, and \n Oceania",
                             "Sub-Saharan Africa")))

fig5_tbl <- fig5_tbl %>% mutate(sex = recode(sex,"Both" = "Total population",
                                             "Female" = "Women",
                                             "Male" = "Men"))
fig5_tbl$sex <- plyr::llply(fig5_tbl$sex, wrapit)
fig5_tbl$sex <- unlist(fig5_tbl$sex)

fig5_tbl <- fig5_tbl %>% 
  dplyr::mutate(sex = factor(sex,c("Total population","Women","Men")))

fig5_tbl %>%
  group_by(location) %>% 
  ggplot(aes(x=age,y=val,fill=age))+
  geom_histogram(stat = "identity")+
  facet_grid(sex ~ location)+
  scale_fill_brewer(palette = "Purples",
                    name="Age")+
  scale_y_continuous(breaks = c(0,25,50,75,100,125))+
  theme_bw()+
  ylab("Age-specific suicide rate")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#d9d9d9"),
        plot.background = element_rect(fill = "#d9d9d9"),
        strip.text.x = element_text(size = 6,
                                    face = "bold",
                                    margin = margin(1,0,1,0, "cm")),
        strip.text.y = element_text(size = 7,
                                    face = "bold",
                                    angle = 360),
        strip.background = element_rect(fill = "white"),
        legend.position = "right",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 5),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 8))

# ggsave("figure 4_2019.jpeg", width = 26, height = 18, units = "cm",dpi=300)

# Figure 2. Temporal trends of OA-NOA suicide ratios by super regions and gender from 1990 to 2019.   
fig6_tbl <- super_regions %>% 
  filter(measure=="Deaths" & 
           location %in% c("South Asia","Latin America and Caribbean",
                           "North Africa and Middle East",
                           "Central Europe, Eastern Europe, and Central Asia",
                           "High-income",
                           "Southeast Asia, East Asia, and Oceania",
                           "Sub-Saharan Africa",
                           "Global")) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  mutate(total_pop = val_Deaths_Number * 100000 / (val_Deaths_Rate / 100000),
         age_elder = if_else(age %in% c("10-14 years","15-19 years",
                                        "20-24 years","25-29 years",
                                        "30-34 years","35-39 years",
                                        "40-44 years","45-49 years",
                                        "50-54 years","55-59 years"),
                             "10-59 years","60 plus")) %>% 
  group_by(location,year,sex,age_elder) %>% 
  dplyr::mutate(sum_age_grp_num = sum(total_pop),
                std_prop_wt = total_pop / sum_age_grp_num) %>% 
  dplyr::summarize(std_dth_rate = sum(val_Deaths_Rate*std_prop_wt)) %>% 
  pivot_wider(names_from = "age_elder",
              values_from = "std_dth_rate") %>% 
  dplyr::mutate(std_dth_rratio = `60 plus` / `10-59 years`) %>% 
  select(-c(`10-59 years`,`60 plus`))

fig6_tbl$location <- plyr::llply(fig6_tbl$location, wrapit)
fig6_tbl$location <- unlist(fig6_tbl$location)

fig6_tbl <- fig6_tbl %>% 
  dplyr::mutate(location = factor(location,
                           c("Global",
                             "South Asia",
                             "Latin America and \n Caribbean",
                             "North Africa and \n Middle East",
                             "Central Europe, \n Eastern Europe, and \n Central Asia",
                             "High-income",
                             "Southeast Asia, \n East Asia, and \n Oceania",
                             "Sub-Saharan Africa")))

fig6a_tbl <- fig6_tbl %>% filter(sex=="Male")
fig6b_tbl <- fig6_tbl %>% filter(sex=="Female")
fig6c_tbl <- fig6_tbl %>% filter(sex=="Both")

fig6a <- fig6a_tbl %>% ggplot(aes(x=year,y=std_dth_rratio,shape=location))+
  geom_line(aes(linetype=location,color=location),
            position = position_dodge(0.2),
            size=1.5)+
  geom_point(size=4,
             position = position_dodge(0.2),
             aes(color=location))+
  scale_linetype()+
  scale_shape_manual(values = c(0,1,2,4,5,6,8,9))+
  scale_color_brewer(palette = "Paired")+
  scale_x_continuous(breaks = seq(from=1990,to=2019,by=2))+
  ylab("Age-standardized OA-NOA Suicide Ratio")+
  xlab("Year")+
  annotate("text",x=min(range(fig6a_tbl$year)),y=Inf,
           label="Men",vjust=1.5,size=6)+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 8),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 10),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "#f7f7f7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "#f7f7f7"),
        strip.background = element_rect(fill = "white"))

# ggsave("figure 6a_2019.jpeg", width = 20, height = 10, units = "cm",dpi=350)

fig6b <- fig6b_tbl %>% ggplot(aes(x=year,y=std_dth_rratio,shape=location))+
  geom_line(aes(linetype=location,color=location),
            position = position_dodge(0.2),
            size=1.5)+
  geom_point(size=4,
             position = position_dodge(0.2),
             aes(color=location))+
  scale_linetype()+
  scale_shape_manual(values = c(0,1,2,4,5,6,8,9))+
  scale_color_brewer(palette = "Paired")+
  scale_x_continuous(breaks = seq(from=1990,to=2019,by=2))+
  ylab("Age-standardized OA-NOA Suicide Ratio")+
  xlab("Year")+
  annotate("text",x=1991,y=Inf,
           label="Women",vjust=1.5,size=6)+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 8),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 10),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "#f7f7f7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "#f7f7f7"),
        strip.background = element_rect(fill = "white"))

# ggsave("figure 6b_2019.jpeg", width = 20, height = 10, units = "cm",dpi=350)

fig6c <- fig6c_tbl %>% ggplot(aes(x=year,y=std_dth_rratio,shape=location))+
  geom_line(aes(linetype=location,color=location),
            position = position_dodge(0.2),
            size=1.5)+
  geom_point(size=4,
             position = position_dodge(0.2),
             aes(color=location))+
  scale_linetype()+
  scale_shape_manual(values = c(0,1,2,4,5,6,8,9))+
  scale_color_brewer(palette = "Paired")+
  scale_x_continuous(breaks = seq(from=1990,to=2019,by=2))+
  ylab("Age-standardized OA-NOA Suicide Ratio")+
  xlab("Year")+
  annotate("text",x=1990,y=Inf,
           label="Total population",vjust=1.5,hjust=0.15,size=5)+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 8),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 10),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "#f7f7f7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "#f7f7f7"),
        strip.background = element_rect(fill = "white"))

# ggsave("figure 6c_2019.png", width = 20, height = 10, units = "cm",dpi=350)

fig6a / fig6b / fig6c

# ggsave("figure 6_2019.jpeg", width = 20, height = 40, units = "cm",dpi=300)

# Figure 3. Age-specific OA-NOA suicide ratio by super region, age groups, and gender in 2019.
tbl9_dth_rratio <- super_regions %>% 
  filter(measure=="Deaths" & sex %in% c("Male","Female","Both") & year==2019) %>% 
  select(-cause) %>% 
  dplyr::group_by(location,year,sex) %>% 
  pivot_wider(names_from = c("measure","metric"),
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
                age_grp_num = val_Deaths_Number * 10^10 / val_Deaths_Rate,
                wider_sd = if_else((upper_Deaths_Number - val_Deaths_Number)>
                                     (lower_Deaths_Number - val_Deaths_Number),
                                   upper_Deaths_Number,lower_Deaths_Number),
                sd = (wider_sd - val_Deaths_Number)*10^5 / 1.96 * sqrt(1000),
                var = sd^2) %>% 
  dplyr::group_by(location,year,sex,age) %>% 
  dplyr::summarize(sum_dthnum = sum(val_Deaths_Number*10^5),
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
  pivot_longer(val_asmr_ratio_6460:val_asmr_ratio_9560,
               names_to = "age_group",
               values_to = "val_asmr_ratio") %>% 
  dplyr::mutate(age_group = recode(age_group,
                                   "val_asmr_ratio_6460" = "60 to 64",
                                   "val_asmr_ratio_6960" = "65 to 69",
                                   "val_asmr_ratio_7460" = "70 to 74",
                                   "val_asmr_ratio_7960" = "75 to 79",
                                   "val_asmr_ratio_8460" = "80 to 84",
                                   "val_asmr_ratio_8960" = "85 to 89",
                                   "val_asmr_ratio_9460" = "90 to 94",
                                   "val_asmr_ratio_9560" = "95 plus")) %>% 
  filter(location %in% c("South Asia","Latin America and Caribbean",
                         "North Africa and Middle East",
                         "Central Europe, Eastern Europe, and Central Asia",
                         "High-income",
                         "Southeast Asia, East Asia, and Oceania",
                         "Sub-Saharan Africa",
                         "Global"))

tbl9_dth_rratio$location <- plyr::llply(tbl9_dth_rratio$location, wrapit)
tbl9_dth_rratio$location <- unlist(tbl9_dth_rratio$location)

tbl9_dth_rratio <- tbl9_dth_rratio %>% 
  dplyr::mutate(location = factor(location,
                           c("Global",
                             "South Asia",
                             "Latin America and \n Caribbean",
                             "North Africa and \n Middle East",
                             "Central Europe, \n Eastern Europe, and \n Central Asia",
                             "High-income",
                             "Southeast Asia, \n East Asia, and \n Oceania",
                             "Sub-Saharan Africa")))

tbl9_dth_rratio <- tbl9_dth_rratio %>% mutate(sex = recode(sex,"Both" = "Total population",
                                                           "Female" = "Women",
                                                           "Male" = "Men"),
                                              sex = factor(sex,c("Total population","Women","Men")))

tbl9_dth_rratio %>% 
  group_by(location) %>% 
  ggplot(aes(x=age_group,y=val_asmr_ratio,fill=age_group))+
  geom_histogram(stat = "identity")+
  facet_grid(sex ~ location)+
  scale_fill_brewer(palette = "Purples",
                    name="Age group")+
  scale_y_continuous(breaks = seq(from=0,to=25,by=5))+
  theme_bw()+
  ylab("Age-specific OA-NOA Suicide Ratio")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#d9d9d9"),
        plot.background = element_rect(fill = "#d9d9d9"),
        strip.text.x = element_text(size = 6,
                                    face = "bold",
                                    margin = margin(1,0,1,0, "cm")),
        strip.text.y = element_text(size = 7,
                                    face = "bold",
                                    angle = 360),
        strip.background = element_rect(fill = "white"),
        legend.position = "right",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 5),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 8))

# ggsave("figure 5_2019.jpeg", width = 26, height = 18, units = "cm",dpi=350)

# Figure 4. Age-standardized OA-NOA suicide ratio by SDI across super regions in 2019.
fig8_tbl <- country %>% 
  filter(measure=="Deaths" & sex=="Both" & year==2019) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(total_pop = val_Deaths_Number * 100000 / (val_Deaths_Rate / 100000),
         age_elder = if_else(age %in% c("10-14 years","15-19 years",
                                        "20-24 years","25-29 years",
                                        "30-34 years","35-39 years",
                                        "40-44 years","45-49 years",
                                        "50-54 years","55-59 years"),
                             "10-59 years","60 plus")) %>% 
  dplyr::group_by(location,year,sex,age_elder) %>% 
  dplyr::mutate(sum_age_grp_num = sum(total_pop),
         std_prop_wt = total_pop / sum_age_grp_num) %>% 
  dplyr::summarize(std_dth_rate = sum(val_Deaths_Rate*std_prop_wt)) %>% 
  pivot_wider(names_from = "age_elder",
              values_from = "std_dth_rate") %>% 
  dplyr::mutate(std_dth_rratio = `60 plus` / `10-59 years`) %>% 
  select(-c(`10-59 years`,`60 plus`)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = "std_dth_rratio")

fig8_tbl <- left_join(fig8_tbl,sdi_fig7,by="location") %>% 
  inner_join(.,ctry_abbr,by="location") %>% 
  dplyr::mutate(super_region = case_when(location %in% c(
    "Armenia","Azerbaijan","Georgia","Kazakhstan","Kyrgyzstan",
    "Mongolia","Tajikistan","Turkmenistan","Uzbekistan",
    "Albania","Bosnia and Herzegovina","Bulgaria","Croatia",
    "Czechia","Hungary","North Macedonia","Montenegro","Poland",
    "Romania","Serbia","Slovakia","Slovenia","Belarus","Estonia",
    "Latvia","Lithuania","Republic of Moldova","Russian Federation","Ukraine") ~ 
      "Central Europe, Eastern Europe, and Central Asia",
    location %in% c("Australia","New Zealand","Brunei Darussalam",
                    "Japan","Democratic People's Republic of Korea","Singapore",
                    "Canada","Greenland","United States of America",
                    "Argentina","Chile","Uruguay","Andorra",
                    "Austria","Belgium","Cyprus","Denmark","Monaco",
                    "Finland","France","Germany","Greece",
                    "Iceland","Ireland","Israel","Italy",
                    "Luxembourg","Malta","Netherlands","San Marino",
                    "Norway","Portugal","Spain","Sweden",
                    "Switzerland","United Kingdom") ~ "High-income",
    location %in% c("Bolivia (Plurinational State of)","Ecuador","Peru","Antigua and Barbuda",
                    "Bahamas","Barbados","Belize","Bermuda","Cuba",
                    "Dominica","Dominican Republic","Grenada","Guyana","Haiti",
                    "Jamaica","Puerto Rico","Saint Lucia",
                    "Saint Vincent and the Grenadines",
                    "Suriname","Trinidad and Tobago",
                    "United States Virgin Islands","Colombia",
                    "Costa Rica","El Salvador","Guatemala",
                    "Honduras","Mexico","Nicaragua","Saint Kitts and Nevis",
                    "Panama","Venezuela (Bolivarian Republic of)","Brazil","Paraguay") ~ 
      "Latin America and Caribbean",
    location %in% c("Afghanistan","Algeria","Bahrain","Egypt",
                    "Iran (Islamic Republic of)","Iraq","Jordan","Kuwait","Lebanon",
                    "Libya","Morocco","Palestine","Oman","Qatar",
                    "Saudi Arabia","Sudan","Syrian Arab Republic","Tunisia",
                    "Turkey","United Arab Emirates","Yemen") ~ 
      "North Africa and Middle East",
    location %in% c("Bangladesh","Bhutan","India","Nepal","Pakistan") ~ 
      "South Asia",
    location %in% c("China","Republic of Korea","Taiwan (Province of China)",
                    "American Samoa","Micronesia (Federated States of)",
                    "Fiji","Guam","Kiribati","Marshall Islands",
                    "Northern Mariana Islands","Papua New Guinea",
                    "Samoa","Solomon Islands","Tonga","Vanuatu","Tuvalu",
                    "Cambodia","Indonesia","Cook Islands","Nauru","Niue","Palau",
                    "Lao People's Democratic Republic","Malaysia","Maldives",
                    "Mauritius","Myanmar","Philippines","Sri Lanka","Seychelles",
                    "Thailand","Timor-Leste","Viet Nam","Tokelau") ~ 
      "Southeast Asia, East Asia, and Oceania",
    location %in% c("Angola","Central African Republic","Congo",
                    "Democratic Republic of the Congo",
                    "Equatorial Guinea","Gabon","Burundi",
                    "Comoros","Djibouti","Eritrea","Ethiopia",
                    "Kenya","Madagascar","Malawi","Mozambique",
                    "Rwanda","Somalia","South Sudan","United Republic of Tanzania",
                    "Uganda","Zambia","Botswana","Lesotho",
                    "Namibia","South Africa","Eswatini",
                    "Zimbabwe","Benin","Burkina Faso","Cameroon",
                    "Cabo Verde","Chad","Côte d'Ivoire","Gambia",
                    "Ghana","Guinea","Guinea-Bissau","Liberia","Mali",
                    "Mauritania","Niger","Nigeria","Sao Tome and Principe",
                    "Senegal","Sierra Leone","Togo") ~ "Sub-Saharan Africa",
    TRUE ~ "Global")) 

fig8_tbl$super_region <- plyr::llply(fig8_tbl$super_region, wrapit)
fig8_tbl$super_region <- unlist(fig8_tbl$super_region)

fig8_tbl <- fig8_tbl %>% 
  dplyr::mutate(location = factor(super_region,
                           c("South Asia",
                             "Latin America and \n Caribbean",
                             "North Africa and \n Middle East",
                             "Central Europe, \n Eastern Europe, and \n Central Asia",
                             "High-income",
                             "Southeast Asia, \n East Asia, and \n Oceania",
                             "Sub-Saharan Africa")))

fig8_cor <- cor.test(fig8_tbl$sdi,fig8_tbl$`2019_Both`,
                     alternative = "two.sided",
                     method = "pearson",
                     conf.level = 0.95)

fig8_cor$estimate
fig8_cor$p.value

fig8_tbl %>% ggplot(aes(x=sdi,y=`2019_Both`))+
  geom_point(aes(color=super_region))+
  geom_text(aes(label=abbr,color=super_region),size=2.5,
            vjust=0,alpha=0.7)+
  geom_text(aes(label=paste0("r=",-0.64,", ","p<0.05")),color="black",
            x=0.85,y=6.5,size=3,family="Gill Sans")+
  stat_smooth(method = lm,level = 0.95,se=FALSE,color="#2171b5",size=0.5)+
  scale_color_brewer(palette = "Paired",
                     name="Super Regions")+
  scale_y_continuous(breaks = seq(from = 0,to = 8,by=0.5))+
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(from = 0,to = 1, by = 0.1))+
  xlab("Socio-Demographic Index (SDI)")+
  ylab("OA-NOA ratio of suicide rates")+
  theme_bw()+
  theme(panel.background = element_rect(fill = "#f7f7f7"),
        plot.background = element_rect(fill = "#f7f7f7"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.position = "right",
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 7),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 8),
        legend.background = element_blank(),
        legend.key = element_blank())

# ggsave("figure 8_2019.png", width = 18, height = 12, units = "cm",dpi = 350)

# figure 5. Percent change of ASSR by country SDI, in five OA age-groups
sdi <- sdi[-668,]

sdi_fig10 <- sdi %>% 
  select(Location,`1990`,`1997`,`1998`,`2005`,`2006`,`2012`,`2013`,`2019`) %>%
  dplyr::mutate(`1990`= paste0(str_sub(`1990`,start = 1L, end = 1L),".",
                        str_sub(`1990`,start = 3L, end = -1L)),
         `1990` = as.numeric(`1990`),
         `1997`= paste0(str_sub(`1997`,start = 1L, end = 1L),".",
                        str_sub(`1997`,start = 3L, end = -1L)),
         `1997` = as.numeric(`1997`),
         `1998`= paste0(str_sub(`1998`,start = 1L, end = 1L),".",
                        str_sub(`1998`,start = 3L, end = -1L)),
         `1998` = as.numeric(`1998`),
         `2005`= paste0(str_sub(`2005`,start = 1L, end = 1L),".",
                        str_sub(`2005`,start = 3L, end = -1L)),
         `2005` = as.numeric(`2005`),
         `2006`= paste0(str_sub(`2006`,start = 1L, end = 1L),".",
                        str_sub(`2006`,start = 3L, end = -1L)),
         `2006` = as.numeric(`2006`),
         `2012`= paste0(str_sub(`2012`,start = 1L, end = 1L),".",
                        str_sub(`2012`,start = 3L, end = -1L)),
         `2012` = as.numeric(`2012`),
         `2013`= paste0(str_sub(`2013`,start = 1L, end = 1L),".",
                        str_sub(`2013`,start = 3L, end = -1L)),
         `2013` = as.numeric(`2013`),
         `2019` = paste0(str_sub(`2019`,start = 1L, end = 1L),".",
                         str_sub(`2019`,start = 3L, end = -1L)),
         `2019` = as.numeric(`2019`)) %>% 
  dplyr::mutate(sdi_1997 = case_when(`1997` <= quantile(`1997`,probs = 0.2) ~ 
                                "Low SDI",
                              `1997` <= quantile(`1997`,probs = 0.4) & `1997` >
                                quantile(`1997`,probs = 0.2) ~ "Low-middle SDI",
                              `1997` <= quantile(`1997`,probs = 0.6) & `1997` >
                                quantile(`1997`,probs = 0.4) ~ "Middle SDI",
                              `1997` <= quantile(`1997`,probs = 0.8) & `1997` >
                                quantile(`1997`,probs = 0.6) ~ "High-middle SDI",
                              `1997` > quantile(`1997`,probs = 0.8) ~ 
                                "High SDI"),
         sdi_2005 = case_when(`2005` <= quantile(`2005`,probs = 0.2) ~ "Low SDI",
                              `2005` <= quantile(`2005`,probs = 0.4) & `2005` >
                                quantile(`2005`,probs = 0.2) ~ "Low-middle SDI",
                              `2005` <= quantile(`2005`,probs = 0.6) & `2005` >
                                quantile(`2005`,probs = 0.4) ~ "Middle SDI",
                              `2005` <= quantile(`2005`,probs = 0.8) & `2005` >
                                quantile(`2005`,probs = 0.6) ~ "High-middle SDI",
                              `2005` > quantile(`2005`,probs = 0.8) ~ 
                                "High SDI"),
         sdi_2012 = case_when(`2012` <= quantile(`2012`,probs = 0.2) ~ "Low SDI",
                              `2012` <= quantile(`2012`,probs = 0.4) & `2012` >
                                quantile(`2012`,probs = 0.2) ~ "Low-middle SDI",
                              `2012` <= quantile(`2012`,probs = 0.6) & `2012` >
                                quantile(`2012`,probs = 0.4) ~ "Middle SDI",
                              `2012` <= quantile(`2012`,probs = 0.8) & `2012` >
                                quantile(`2012`,probs = 0.6) ~ "High-middle SDI",
                              `2012` > quantile(`2012`,probs = 0.8) ~ 
                                "High SDI"),
         sdi_2019 = case_when(`2019` <= quantile(`2019`,probs = 0.2) ~ "Low SDI",
                              `2019` <= quantile(`2019`,probs = 0.4) & `2019` >
                                quantile(`2019`,probs = 0.2) ~ "Low-middle SDI",
                              `2019` <= quantile(`2019`,probs = 0.6) & `2019` >
                                quantile(`2019`,probs = 0.4) ~ "Middle SDI",
                              `2019` <= quantile(`2019`,probs = 0.8) & `2019` >
                                quantile(`2019`,probs = 0.6) ~ "High-middle SDI",
                              `2019` > quantile(`2019`,probs = 0.8) ~ 
                                "High SDI")) %>% 
  select(-c(`1990`,`1997`,`1998`,`2005`,`2006`,`2012`,`2013`,`2019`)) %>% 
  pivot_longer(sdi_1997:sdi_2019,
               names_to = "age_change",
               values_to = "sdi_level") %>% 
  dplyr::mutate(age_change = recode(age_change,
                             "sdi_1997" = "Change between 1990 and 1997",
                             "sdi_2005" = "Change between 1998 and 2005",
                             "sdi_2012" = "Change between 2006 and 2012",
                             "sdi_2019" = "Change between 2013 and 2019"))

fig10a_tbl <- country %>% 
  filter(measure=="Deaths" & sex=="Both" &
           year %in% c(1990,1997,1998,2005,2006,2012,2013,2019) &
           location!="Global" &
           age %in% c("60-64 years","65-69 years","70-74 years","75-79 years",
                      "80-84 years","85-89 years","90-94 years","95+ years")) %>% 
  select(-c(cause,upper,lower)) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = "val") %>% 
  dplyr::mutate(age_grp_num = 10^10 * Both_Deaths_Number / Both_Deaths_Rate) %>% 
  dplyr::group_by(location,year) %>% 
  dplyr::mutate(total_age_grp_num = sum(age_grp_num),
         std_wt = age_grp_num / total_age_grp_num) %>% 
  dplyr::summarize(val_asmr = sum(std_wt*Both_Deaths_Rate)) %>% 
  pivot_wider(names_from = "year",
              values_from = "val_asmr") %>% 
  dplyr::mutate(val_pct_chg_dth_rate_9790 = (`1997` - `1990`)*100 / `1990`,
         val_pct_chg_dth_rate_0598 = (`2005` - `1998`)*100 / `1998`,
         val_pct_chg_dth_rate_1206 = (`2012` - `2006`)*100 / `2006`,
         val_pct_chg_dth_rate_1913 = (`2019` - `2013`)*100 / `2013`) %>% 
  select(-c(`1990`,`1997`,`1998`,`2005`,`2006`,`2012`,`2013`,`2019`)) %>% 
  pivot_longer(val_pct_chg_dth_rate_9790:val_pct_chg_dth_rate_1913,
               names_to = "age_change",
               values_to = "val_pct_chg_dth_rate") %>% 
  dplyr::mutate(age_change = recode(age_change,
                                    "val_pct_chg_dth_rate_9790" = 
                                      "Change between 1990 and 1997",
                                    "val_pct_chg_dth_rate_0598" = 
                                      "Change between 1998 and 2005",
                                    "val_pct_chg_dth_rate_1206" = 
                                      "Change between 2006 and 2012",
                                    "val_pct_chg_dth_rate_1913" = 
                                      "Change between 2013 and 2019"),
                age_cat = "60 above")

col_order <- c("location","age_cat","age_change","val_pct_chg_dth_rate")
fig10a_tbl <- fig10a_tbl[,col_order]

fig10b_tbl <- country %>% 
  filter(measure=="Deaths" & sex=="Both" &
           year %in% c(1990,1997,1998,2005,2006,2012,2013,2019) &
           location!="Global" &
           age %in% c("60-64 years","65-69 years","70-74 years","75-79 years",
                      "80-84 years","85-89 years","90-94 years","95+ years")) %>% 
  select(-c(cause,upper,lower)) %>% 
  pivot_wider(names_from = c("sex","measure","metric"),
              values_from = "val") %>% 
  dplyr::mutate(age_cat = recode(age,"60-64 years" = "60 to <70",
                          "65-69 years" = "60 to <70",
                          "70-74 years" = "70 to <80",
                          "75-79 years" = "70 to <80",
                          "80-84 years" = "80 above",
                          "85-89 years" = "80 above",
                          "90-94 years" = "80 above",
                          "95+ years" = "80 above"),
         age_grp_num = 10^10 * Both_Deaths_Number / Both_Deaths_Rate) %>% 
  dplyr::group_by(location,year,age_cat) %>% 
  dplyr::mutate(total_age_grp_num = sum(age_grp_num),
         std_wt = age_grp_num / total_age_grp_num) %>% 
  dplyr::summarize(val_asmr = sum(std_wt*Both_Deaths_Rate)) %>% 
  pivot_wider(names_from = "year",
              values_from = "val_asmr") %>% 
  dplyr::mutate(val_pct_chg_dth_rate_9790 = (`1997` - `1990`)*100 / `1990`,
         val_pct_chg_dth_rate_0598 = (`2005` - `1998`)*100 / `1998`,
         val_pct_chg_dth_rate_1206 = (`2012` - `2006`)*100 / `2006`,
         val_pct_chg_dth_rate_1913 = (`2019` - `2013`)*100 / `2013`) %>% 
  select(-c(`1990`,`1997`,`1998`,`2005`,`2006`,`2012`,`2013`,`2019`)) %>% 
  pivot_longer(val_pct_chg_dth_rate_9790:val_pct_chg_dth_rate_1913,
               names_to = "age_change",
               values_to = "val_pct_chg_dth_rate") %>% 
  dplyr::mutate(age_change = recode(age_change,
                             "val_pct_chg_dth_rate_9790" = 
                               "Change between 1990 and 1997",
                             "val_pct_chg_dth_rate_0598" = 
                               "Change between 1998 and 2005",
                             "val_pct_chg_dth_rate_1206" = 
                               "Change between 2006 and 2012",
                             "val_pct_chg_dth_rate_1913" = 
                               "Change between 2013 and 2019"))

fig10_tbl <- bind_rows(fig10a_tbl,fig10b_tbl)

sdi_fig10[which(sdi_fig10$Location=="The Bahamas"),1] <- "Bahamas"
sdi_fig10[which(sdi_fig10$Location=="Bolivia"),1] <- "Bolivia (Plurinational State of)"
sdi_fig10[which(sdi_fig10$Location=="Brunei"),1] <- "Brunei Darussalam"
sdi_fig10[which(sdi_fig10$Location=="Cape Verde"),1] <- "Cabo Verde"
sdi_fig10[which(sdi_fig10$Location=="Congo (Brazzaville)"),1] <- "Congo"
sdi_fig10[which(sdi_fig10$Location=="DR Congo"),1] <- "Democratic Republic of the Congo"
sdi_fig10[which(sdi_fig10$Location=="Czech Republic"),1] <- "Czechia"
sdi_fig10[which(sdi_fig10$location=="South Korea"),1] <- "Democratic People's Republic of Korea"
sdi_fig10[which(sdi_fig10$Location=="eSwatini"),1] <- "Eswatini"
sdi_fig10[which(sdi_fig10$Location=="The Gambia"),1] <- "Gambia"
sdi_fig10[which(sdi_fig10$Location=="Iran"),1] <- "Iran (Islamic Republic of)"
sdi_fig10[which(sdi_fig10$Location=="Laos"),1] <- "Lao People's Democratic Republic"
sdi_fig10[which(sdi_fig10$Location=="Federated States of Micronesia"),1] <- "Micronesia (Federated States of)"
sdi_fig10[which(sdi_fig10$Location=="North Korea"),1] <- "Republic of Korea"
sdi_fig10[which(sdi_fig10$Location=="Moldova"),1] <- "Republic of Moldova"
sdi_fig10[which(sdi_fig10$Location=="Russia"),1] <- "Russian Federation"
sdi_fig10[which(sdi_fig10$Location=="São Tomé and PrÍncipe"),1] <- "Sao Tome and Principe"
sdi_fig10[which(sdi_fig10$Location=="Syria"),1] <- "Syrian Arab Republic"
sdi_fig10[which(sdi_fig10$Location=="Taiwan (province of China)"),1] <- "Taiwan (Province of China)"
sdi_fig10[which(sdi_fig10$Location=="UK"),1] <- "United Kingdom"
sdi_fig10[which(sdi_fig10$Location=="Tanzania"),1] <- "United Republic of Tanzania"
sdi_fig10[which(sdi_fig10$Location=="USA"),1] <- "United States of America"
sdi_fig10[which(sdi_fig10$Location=="Virgin Islands"),1] <- "United States Virgin Islands"
sdi_fig10[which(sdi_fig10$Location=="Venezuela"),1] <- "Venezuela (Bolivarian Republic of)"
sdi_fig10[which(sdi_fig10$Location=="Vietnam"),1] <- "Viet Nam"

colnames(sdi_fig10)[1] <- "location"

merged_fig10_tbl <- inner_join(fig10_tbl,sdi_fig10,by=c('location','age_change'))

wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=20),collapse=" \n ")
  return(wtext)
}

merged_fig10_tbl$age_change <- plyr::llply(merged_fig10_tbl$age_change, wrapit)
merged_fig10_tbl$age_change <- unlist(merged_fig10_tbl$age_change)

merged_fig10_tbl <- merged_fig10_tbl %>% 
  dplyr::mutate(age_change = factor(age_change,
                             c("Change between 1990 \n and 1997",
                               "Change between 1998 \n and 2005",
                               "Change between 2006 \n and 2012",
                               "Change between 2013 \n and 2019")))

merged_fig10_tbl <- merged_fig10_tbl %>% 
  mutate(age_cat = paste0("OA age-group: age ",age_cat))

wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=15),collapse=" \n ")
  return(wtext)
}

merged_fig10_tbl$age_cat <- plyr::llply(merged_fig10_tbl$age_cat, wrapit)
merged_fig10_tbl$age_cat <- unlist(merged_fig10_tbl$age_cat)

merged_fig10_tbl$age_cat[merged_fig10_tbl$age_cat=="OA age-group: \n age 80 above"] <- "OA age-group: \n age 80 years and older"

merged_fig10_tbl$age_cat[merged_fig10_tbl$age_cat=="OA age-group: \n age 60 above"] <- "OA age-group: \n age 60 years and older"

merged_fig10_tbl$age_cat[merged_fig10_tbl$age_cat=="OA age-group: \n age 60 to <70"] <- "OA age-group: \n age 60 to <70 years"

merged_fig10_tbl$age_cat[merged_fig10_tbl$age_cat=="OA age-group: \n age 70 to <80"] <- "OA age-group: \n age 70 to <80 years"

merged_fig10_tbl <- merged_fig10_tbl %>% mutate(age_cat = factor(age_cat,
                                                                 c("OA age-group: \n age 60 years and older",
                                                                   "OA age-group: \n age 60 to <70 years",
                                                                   "OA age-group: \n age 70 to <80 years",
                                                                   "OA age-group: \n age 80 years and older")))

merged_fig10_tbl <- merged_fig10_tbl %>% 
  dplyr::group_by(age_change,age_cat) %>% 
  dplyr::mutate(median_pct_chg_asmr = median(val_pct_chg_dth_rate))

merged_fig10_tbl <- merged_fig10_tbl %>% 
  dplyr::mutate(sdi_level = factor(sdi_level,
                            levels=c("High SDI",
                                     "High-middle SDI",
                                     "Middle SDI",
                                     "Low-middle SDI",
                                     "Low SDI")))

merged_fig10_tbl %>% 
  ggplot(aes(x=val_pct_chg_dth_rate,fill=sdi_level))+
  geom_histogram()+
  scale_fill_manual(values = c(`High SDI` = "#54278f",
                               `High-middle SDI` = "#756bb1",
                               `Middle SDI` = "#9e9ac8",
                               `Low-middle SDI` = "#cbc9e2",
                               `Low SDI` = "#f2f0f7"),
                    name="SDI")+
  facet_grid(age_cat~age_change)+
  xlab("Percent change in age-standardized \n suicide rate by SDI quintile")+
  ylab("Number of countries")+
  scale_x_continuous(breaks = seq(from=-50, to=75,by=25),limits = c(-55,80))+
  scale_y_continuous(breaks = seq(from=0, to=60, by=10))+
  geom_vline(xintercept = 0)+
  geom_vline(aes(xintercept=median_pct_chg_asmr),color="gray",
             linetype="dashed")+
  theme_bw()+
  theme(strip.text.x = element_text(size = 8,
                                    face = "bold",
                                    margin = margin(0.1,0,0.1,0, "cm")),
        strip.text.y = element_text(size = 8,
                                    face = "bold",
                                    angle = 360),
        strip.background = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Gill Sans",color = "#444444",
                                   size = 8),
        legend.title = element_text(family = "Gill Sans",face="bold",
                                    color = "#444444",size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# ggsave("figure 10_2019.jpeg",width = 28,height = 20,units = "cm",dpi = 350)


