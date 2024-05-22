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




