# the script was created for supplementary materials.

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

# Appendix Figure S1. Age-standardized suicide-rates (ASSR) globally (all age groups), 2019.
# get the lat and long for all countries in world from ggmap 
map.world <- map_data("world")

head(country)

# get ASMR for countries from GBD
# remove the global
fig1_tbl <- country %>% 
  filter(measure=="Deaths" & sex=="Both" & year==2019) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(total_pop = val_Deaths_Number * 100000 / 
                  (val_Deaths_Rate / 100000)) %>% 
  dplyr::group_by(location,year,sex) %>% 
  mutate(sum_age_grp_num = sum(total_pop),
         std_prop_wt = total_pop / sum_age_grp_num) %>% 
  dplyr::summarize(std_dth_rate = sum(val_Deaths_Rate*std_prop_wt)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = "std_dth_rate")  %>% 
  filter(location!="Global") 

# change the column names
colnames(fig1_tbl) <- c("region","asmr")

# GBD have different names of countries from ggmap
# find countries in common
ctry_comn <- inner_join(map.world,fig1_tbl,by="region") %>% 
  pull(region) %>% unique()
# there are 176 countries in common
# 195-176=19 country names in GBD are different

# view uncommon countries in GBD and ggmap
fig1_tbl %>% filter(!(region %in% ctry_comn)) %>% 
  arrange(region) %>% 
  pull(region)
map.world %>% filter(!(region %in% ctry_comn)) %>% 
  arrange(region) %>% 
  pull(region) %>% unique()

# change the different country names in ggmap to names in GBD
map.world <- map.world %>% 
  dplyr::mutate(region = recode(region,"Antigua" = "Antigua and Barbuda",
                         "Barduba" = "Antigua and Barbuda",
                         "Bolivia" = "Bolivia (Plurinational State of)",
                         "Brunei" = "Brunei Darussalam",
                         "Cape Verde" = "Cabo Verde",
                         "Republic of Congo" = "Congo",
                         "Ivory Coast" = "Côte d'Ivoire",
                         "Czech Republic" = "Czechia",
                         "South Korea" = "Democratic People's Republic of Korea",
                         "Swaziland" = "Eswatini",
                         "Iran" = "Iran (Islamic Republic of)",
                         "Laos" = "Lao People's Democratic Republic",
                         "Micronesia" = "Micronesia (Federated States of)",
                         "North Korea" = "Republic of Korea",
                         "Moldova" = "Republic of Moldova",
                         "Russia" = "Russian Federation",
                         "Saint Kitts" = "Saint Kitts and Nevis",
                         "Nevis" = "Saint Kitts and Nevis",
                         "Saint Vincent" = 
                           "Saint Vincent and the Grenadines",
                         "Grenadines" = "Saint Vincent and the Grenadines",
                         "Syria" = "Syrian Arab Republic",
                         "Taiwan" = "Taiwan (Province of China)",
                         "Trinidad" = "Trinidad and Tobago",
                         "Tobago" = "Trinidad and Tobago",
                         "UK" = "United Kingdom",
                         "Tanzania" = "United Republic of Tanzania",
                         "USA" = "United States of America",
                         "Virgin Islands" = "United States Virgin Islands",
                         "Venezuela" = "Venezuela (Bolivarian Republic of)",
                         "Vietnam" = "Viet Nam"))

map.world_joined <- left_join(map.world,fig1_tbl,by="region")

map.world_joined <- map.world_joined %>% 
  mutate(asmr_cat = case_when(asmr>=0 & asmr<5 ~ "0 to <5",
                              asmr>=5 & asmr<10 ~ "5 to <10",
                              asmr>=10 & asmr<15 ~ "10 to <15",
                              asmr>=15 & asmr<20 ~ "15 to <20",
                              asmr>=20 & asmr<25 ~ "20 to <25",
                              asmr>=25 & asmr<30 ~ "25 to <30",
                              asmr>=30 & asmr<35 ~ "30 to <35",
                              asmr>=35 & asmr<40 ~ "35 to <40",
                              asmr>=40 ~ ">=40",
                              TRUE ~ "")) %>% 
  filter(!(asmr_cat=="")) %>% 
  dplyr::mutate(asmr_cat = factor(asmr_cat,
                           levels = c("0 to <5",
                                      "5 to <10",
                                      "10 to <15",
                                      "15 to <20",
                                      "20 to <25",
                                      "25 to <30",
                                      "30 to <35",
                                      "35 to <40",
                                      ">=40")))

map.world_joined %>% ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  coord_cartesian()+
  ggsn::scalebar(map.world_joined,dist = 1500, st.size=6, 
                 height=0.01, transform=T, model = 'International',
                 dist_unit = "km",location = "bottomleft")+
  ggsn::north(map.world_joined,location = "topright",
              scale = 0.1,symbol = 1)+
  theme(panel.background = element_rect(fill = "#efedf5")
        ,plot.background = element_rect(fill = "#efedf5")
        ,panel.grid = element_blank()
        ,plot.title = element_text(hjust = 0.5,size = 20)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.text = element_text(color = "#444444",size = 20)
        ,legend.title = element_text(face = "bold",color = "#444444",
                                     size = 30)
        ,legend.background = element_blank()
        ,legend.position = "right"
        ,legend.key = element_blank())+
  borders(database = "world",regions = ctry_vtr,colour = "white")

# ggsave("figure 1a_2019.png",width = 80,height=40,units = "cm",dpi = 300)
# There are a few nations which should be outlined.
fig1_cbn <- map.world_joined %>% 
  filter(region %in% c("Antigua and Barbuda","Bahamas","Barbados",
                       "Belize","Bermuda","Cuba","Dominica","Dominican Republic",
                       "Grenada","Guyana","Haiti","Jamaica","Puerto Rico",
                       "Saint Vincent and the Grenadines",
                       "Suriname","Trinidad and Tobago",
                       "United States Virgin Islands")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-80,y=2,label="Caribbean",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_atg <- map.world_joined %>% 
  filter(region=="Antigua and Barbuda") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-61.84,y=17.01,label="ATG",size=8) +
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_vct <- map.world_joined %>% 
  filter(region=="Saint Vincent and the Grenadines") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-61.3,y=12.8,label="VCT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_brb <- map.world_joined %>% 
  filter(region=="Barbados") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-59.6,y=13.08,label = "BRB",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_dma <- map.world_joined %>% 
  filter(region=="Dominica") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-61.4,y=15.3,label="DMA",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_grd <- map.world_joined %>% 
  filter(region=="Grenada") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-61.74,y=12.2,label="GRD",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_com <- map.world_joined %>% 
  filter(region=="Comoros") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=43.5,y=-12.2,label="COM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_mdv <- map.world_joined %>% 
  filter(region=="Maldives") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=73.42,y=4.2,label="MDV",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_mus <- map.world_joined %>% 
  filter(region=="Mauritius") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=57.45,y=-20.05,label="MUS",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_lca <- map.world_joined %>% 
  filter(region=="Saint Lucia") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-61.025,y=14.05,label="LCA",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_tto <- map.world_joined %>% 
  filter(region=="Trinidad and Tobago") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-61.5,y=11.25,label="TTO",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_tls <- map.world_joined %>% 
  filter(region=="Timor-Leste") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=124.5,y=-8.2,label="TLS",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_syc <- map.world_joined %>% 
  filter(region=="Seychelles") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=55.425,y=-4.75,label="SYC",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_waf <- map.world_joined %>% 
  filter(region %in% c("Benin","Burkina Faso","Cameroon","Cabo Verde",
                       "Chad","Côte d'Ivoire","The Gambia","Ghana","Guinea",
                       "Guinea-Baissau","Liberia","Mali","Mauritania",
                       "Niger","Nigeria","Sao Tome and Principe","Senegal",
                       "Sierra Leone","Togo")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-10,y=4,label="W Africa",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_tbl %>% filter(!(region %in% ctry_comn)) %>% 
  arrange(region) %>% 
  pull(region)
map.world %>% filter(!(region %in% ctry_comn)) %>% 
  arrange(region) %>% 
  pull(region) %>% unique()

fig1_pgf <- map.world_joined %>% 
  filter(region %in% c("Bahrain","Qatar","Oman","Iraq","Iraq","Iran (Islamic Republic of)",
                       "Saudi Arabia","Kuwait","United Arab Emirates")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=45,y=17,label="Persian Gulf",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_emd <- map.world_joined %>% 
  filter(region %in% c("Cyprus","Greece","Palestine","Lebanon",
                       "Syria","Jordan","Turkey","Egypt","Israel")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=25,y=22.5,label="E Med",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_mlt <- map.world_joined %>% 
  filter(region=="Malta") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=14.29,y=35.85,label="MLT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_sgp <- map.world_joined %>% 
  filter(region=="Singapore") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=103.75,y=1.28,label="SGP",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

map.world_joined %>% 
  filter(region %in% c("Slovenia","Croatia","Bosnia and Herzegovina",
                       "Serbia","North Macedonia","Italy","Bulgaria","Albania",
                       "Greece","Turkey","Romania","Republic of Moldova",
                       "Hungary","Montenegro","Austria")) %>% 
  arrange(region) %>% 
  pull(region) %>% 
  unique()

fig1_bpa <- map.world_joined %>% 
  filter(region %in% c("Slovenia","Croatia","Bosnia and Herzegovina",
                       "Serbia","North Macedonia","Italy","Bulgaria","Albania",
                       "Greece","Turkey","Romania","Republic of Moldova",
                       "Hungary","Montenegro","Austria")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=25,y=36,label="Balkan Penisula",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_mhl <- map.world_joined %>% 
  filter(region=="Marshall Islands") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=168,y=6.5,label="MHL",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_slb <- map.world_joined %>% 
  filter(region=="Solomon Islands") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill =  asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000")+
  annotate("text",x=157.5,y=-11,label="SLB",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_vut <- map.world_joined %>% 
  filter(region=="Vanuatu") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=167.5,y=-19.5,label="VUT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_fji <- map.world_joined %>% 
  filter(region=="Fiji") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-125,y=-21,label="FJI",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_kir <- map.world_joined %>% 
  filter(region=="Kiribati") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-125,y=-10,label="KIR",size=8) +
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Kiribati",colour = "black")

fig1_fsm <- map.world_joined %>% 
  filter(region=="Micronesia (Federated States of)") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000")+
  annotate("text",x=145,y=5.5,label="FSM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_wsm <- map.world_joined %>% 
  filter(region=="Samoa") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-172.4,y=-14,label="WSM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig1_ton <- map.world_joined %>% 
  filter(region=="Tonga") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_cat)) +
  scale_fill_manual(values = c(`0 to <5` = "#f7fbff",
                               `5 to <10` = "#deebf7",
                               `10 to <15` = "#c6dbef",
                               `15 to <20` = "#9ecae1",
                               `20 to <25` = "#6baed6",
                               `25 to <30` = "#4292c6",
                               `30 to <35` = "#2171b5",
                               `35 to <40` = "#08519c",
                               `>=40` = "#08306b"),
                    name="Deaths per 100,000") +
  annotate("text",x=-175,y=-19,label="TON",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

# bind plots
fig1_combo1 <- ggpubr::ggarrange(fig1_atg,fig1_vct,fig1_brb,fig1_com,
                                 fig1_dma,fig1_grd,fig1_mdv,fig1_mus,
                                 fig1_lca,fig1_tto,fig1_tls,fig1_syc,
                                 ncol = 4,nrow = 3)

fig1_combo2 <- ggpubr::ggarrange(fig1_mlt,fig1_sgp,
                                 ncol=1,nrow = 2)

fig1_combo2_plus <- ggpubr::ggarrange(fig1_waf,fig1_emd,
                                      fig1_pgf,fig1_combo2,
                                      ncol = 2,nrow = 2)

fig1_combo3 <- ggpubr::ggarrange(fig1_mhl,fig1_kir,
                                 fig1_slb,fig1_fsm,
                                 fig1_vut,fig1_wsm,
                                 fig1_fji,fig1_ton,
                                 ncol = 2,nrow = 4)

ggpubr::ggarrange(fig1_cbn,fig1_combo1,fig1_combo2_plus,
                  fig1_combo3,ncol = 4)

# ggsave("figure 1b_2019.png",width = 80, height = 16,units = "cm", dpi=300,
#        limitsize = F)
# Two figures are exported and combined into one through PPT.

# Appendix Figure S2. Percentage change in age-standardized suicide-rates (ASSR) globally (all age groups) between 1990 and 2019.
fig3_tbl <- country %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(total_pop = val_Deaths_Number * 100000 / (val_Deaths_Rate / 100000)) %>% 
  dplyr::group_by(location,year,sex) %>% 
  mutate(sum_age_grp_num = sum(total_pop),
         std_prop_wt = total_pop / sum_age_grp_num) %>% 
  dplyr::summarize(std_dth_rate = sum(val_Deaths_Rate*std_prop_wt)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = "std_dth_rate") %>% 
  dplyr::mutate(pct_chg_asmr = (`2019_Both` - `1990_Both`) * 100 / `1990_Both`) %>% 
  select(-c(`1990_Both`,`2019_Both`))

colnames(fig3_tbl) <- c("region","asmr_pct_chg")

map.world_joined_fig3 <- left_join(map.world,fig3_tbl,by="region")

map.world_joined_fig3 <- map.world_joined_fig3 %>% 
  mutate(asmr_pct_chg_cat = case_when(
    asmr_pct_chg>=(-80) & asmr_pct_chg<(-60) ~ "-80 to <-60",
    asmr_pct_chg>=(-60) & asmr_pct_chg<(-40) ~ "-60 to <-40",
    asmr_pct_chg>=(-40) & asmr_pct_chg<(-20) ~ "-40 to <-20",
    asmr_pct_chg>=(-20) & asmr_pct_chg<0 ~ "-20 to <0",
    asmr_pct_chg>=0 & asmr_pct_chg<20 ~ "0 to <20",
    asmr_pct_chg>=20 & asmr_pct_chg<40 ~ "20 to <40",
    asmr_pct_chg>=40 & asmr_pct_chg<60 ~ "40 to <60",
    asmr_pct_chg>=60 & asmr_pct_chg<80 ~ "60 to <80",
    asmr_pct_chg>=80 ~ ">=80",
    TRUE ~ "")) %>% 
  filter(asmr_pct_chg_cat!="") %>% 
  dplyr::mutate(asmr_pct_chg_cat = factor(asmr_pct_chg_cat,
                                   levels = c("-80 to <-60",
                                              "-60 to <-40",
                                              "-40 to <-20",
                                              "-20 to <0",
                                              "0 to <20",
                                              "20 to <40",
                                              "40 to <60",
                                              "60 to <80",
                                              ">=80")))

map.world_joined_fig3 %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  ggsn::scalebar(map.world_joined_fig3,dist = 1500,st.size=6, 
                 height=0.01, transform=T, model = 'International',
                 dist_unit = "km",location = "bottomleft")+
  ggsn::north(map.world_joined_fig3,location = "topright",
              scale = 0.1,symbol = 1)+
  theme(panel.background = element_rect(fill = "#efedf5")
        ,plot.background = element_rect(fill = "#efedf5")
        ,panel.grid = element_blank()
        ,plot.title = element_text(hjust = 0.5,size = 20)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.text = element_text(color = "#444444",size = 20)
        ,legend.title = element_text(face = "bold",color = "#444444",
                                     size = 30)
        ,legend.background = element_blank()
        ,legend.key = element_blank())+
  borders(database = "world",regions = ctry_vtr,colour = "white")

# ggsave("figure 2a_2019.png",width = 80, height = 40, units = "cm",dpi=300,
#        limitsize = F)

# Some regions and nations should be outlined.
fig3_cbn <- map.world_joined_fig3 %>% 
  filter(region %in% c("Antigua and Barbuda","Bahamas","Barbados",
                       "Belize","Bermuda","Cuba","Dominica","Dominican Republic",
                       "Grenada","Guyana","Haiti","Jamaica","Puerto Rico",
                       "Saint Vincent and the Grenadines",
                       "Suriname","Trinidad and Tobago",
                       "United States Virgin Islands")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-80,y=2,label="Caribbean",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_atg <- map.world_joined_fig3 %>% 
  filter(region=="Antigua and Barbuda") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-61.85,y=17,label="ATG",size=8) +
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_vct <- map.world_joined_fig3 %>% 
  filter(region=="Saint Vincent and the Grenadines") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-61.3,y=12.8,label="VCT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_brb <- map.world_joined_fig3 %>% 
  filter(region=="Barbados") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-59.6,y=13.09,label = "BRB",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_dma <- map.world_joined_fig3 %>% 
  filter(region=="Dominica") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-61.4,y=15.3,label="DMA",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_grd <- map.world_joined_fig3 %>% 
  filter(region=="Grenada") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-61.75,y=12.225,label="GRD",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_com <- map.world_joined_fig3 %>% 
  filter(region=="Comoros") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=43.5,y=-12.2,label="COM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_mdv <- map.world_joined_fig3 %>% 
  filter(region=="Maldives") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=73.415,y=3.375,label="MDV",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")


fig3_mus <- map.world_joined_fig3 %>% 
  filter(region=="Mauritius") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=57.4,y=-20.025,label="MUS",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_lca <- map.world_joined_fig3 %>% 
  filter(region=="Saint Lucia") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-61.05,y=14.05,label="LCA",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_tto <- map.world_joined_fig3 %>% 
  filter(region=="Trinidad and Tobago") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-61.6,y=11.25,label="TTO",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_tls <- map.world_joined_fig3 %>% 
  filter(region=="Timor-Leste") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=124.5,y=-8.3,label="TLS",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_syc <- map.world_joined_fig3 %>% 
  filter(region=="Seychelles") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=55.425,y=-4.75,label="SYC",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_waf <- map.world_joined_fig3 %>% 
  filter(region %in% c("Benin","Burkina Faso","Cameroon","Cabo Verde",
                       "Chad","Côte d'Ivoire","The Gambia","Ghana","Guinea",
                       "Guinea-Baissau","Liberia","Mali","Mauritania",
                       "Niger","Nigeria","Sao Tome and Principe","Senegal",
                       "Sierra Leone","Togo")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-15,y=4,label="W Africa",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_pgf <- map.world_joined_fig3 %>% 
  filter(region %in% c("Bahrain","Qatar","Oman","Iraq","Iraq","Iran (Islamic Republic of)",
                       "Saudi Arabia","Kuwait","United Arab Emirates")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=45,y=17,label="Persian Gulf",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_emd <- map.world_joined_fig3 %>% 
  filter(region %in% c("Cyprus","Greece","Palestine","Lebanon",
                       "Syria","Jordan","Turkey","Egypt","Israel")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=25,y=22.5,label="E Med",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_mlt <- map.world_joined_fig3 %>% 
  filter(region=="Malta") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=14.25,y=35.85,label="MLT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_sgp <- map.world_joined_fig3 %>% 
  filter(region=="Singapore") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=103.725,y=1.275,label="SGP",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_bpa <- map.world_joined_fig3 %>% 
  filter(region %in% c("Slovenia","Croatia","Bosnia and Herzegovina",
                       "Serbia","North Macedonia","Italy","Bulgaria","Albania",
                       "Greece","Turkey","Romania","Republic of Moldova",
                       "Hungary","Montenegro","Austria")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=25,y=36,label="Balkan Penisula",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_mhl <- map.world_joined_fig3 %>% 
  filter(region=="Marshall Islands") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=167.5,y=6,label="MHL",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_slb <- map.world_joined_fig3 %>% 
  filter(region=="Solomon Islands") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill =  asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=157.5,y=-11.5,label="SLB",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_vut <- map.world_joined_fig3 %>% 
  filter(region=="Vanuatu") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=167,y=-19.5,label="VUT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_fji <- map.world_joined_fig3 %>% 
  filter(region=="Fiji") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-100,y=-21,label="FJI",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_kir <- map.world_joined_fig3 %>% 
  filter(region=="Kiribati") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-125,y=-10,label="KIR",size=8) +
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Kiribati",colour = "black")

fig3_fsm <- map.world_joined_fig3 %>% 
  filter(region=="Micronesia (Federated States of)") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage")+
  annotate("text",x=140,y=5.5,label="FSM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_wsm <- map.world_joined_fig3 %>% 
  filter(region=="Samoa") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-172.5,y=-14,label="WSM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig3_ton <- map.world_joined_fig3 %>% 
  filter(region=="Tonga") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_pct_chg_cat)) +
  scale_fill_manual(values = c(`-80 to <-60` = "#f7fbff",
                               `-60 to <-40` = "#deebf7",
                               `-40 to <-20` = "#c6dbef",
                               `-20 to <0` = "#9ecae1",
                               `0 to <20` = "#6baed6",
                               `20 to <40` = "#4292c6",
                               `40 to <60` = "#2171b5",
                               `60 to <80` = "#08519c",
                               `>=80` = "#08306b"),
                    name="Percentage") +
  annotate("text",x=-175,y=-19,label="TON",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

# bind plots
fig3_combo1 <- ggpubr::ggarrange(fig3_atg,fig3_vct,fig3_brb,fig3_com,
                                 fig3_dma,fig3_grd,fig3_mdv,fig3_mus,
                                 fig3_lca,fig3_tto,fig3_tls,fig3_syc,
                                 ncol = 4,nrow = 3)

fig3_combo2 <- ggpubr::ggarrange(fig3_mlt,fig3_sgp,
                                 ncol=1,nrow = 2)

fig3_combo2_puls <- ggpubr::ggarrange(fig3_waf,fig3_emd,
                                      fig3_pgf,fig3_combo2,
                                      ncol = 2,nrow = 2)

fig3_combo3 <- ggpubr::ggarrange(fig3_mhl,fig3_kir,
                                 fig3_slb,fig3_fsm,
                                 fig3_vut,fig3_wsm,
                                 fig3_fji,fig3_ton,
                                 ncol = 2,nrow = 4)

ggpubr::ggarrange(fig3_cbn,fig3_combo1,fig3_combo2_puls,
                  fig3_combo3,ncol = 4)

# ggsave("figure 2b_2019.png",width = 80, height = 16, units = "cm",dpi=300,
#       limitsize = F)
# Two figures will be binded togather to generate a new one through PPT.

# Appendix Figure S3. Global OA-NOA suicide-rates ratio in 2019.
fig4_tbl <- country %>% 
  filter(measure=="Deaths" & sex=="Both" & year==2019) %>% 
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

colnames(fig4_tbl) <- c("region","asmr_ratio")

fig4_tbl %>% filter(asmr_ratio<1) %>% arrange(asmr_ratio)

map.world_joined_fig4 <- left_join(map.world,fig4_tbl,by="region")

map.world_joined_fig4 <- map.world_joined_fig4 %>% 
  mutate(asmr_ratio_cat = case_when(
    asmr_ratio>=0 & asmr_ratio<1 ~ "0 to <1",
    asmr_ratio>=1 & asmr_ratio<2 ~ "1 to <2",
    asmr_ratio>=2 & asmr_ratio<3 ~ "2 to <3",
    asmr_ratio>=3 & asmr_ratio<4 ~ "3 to <4",
    asmr_ratio>=4 & asmr_ratio<5 ~ "4 to <5",
    asmr_ratio>=5 & asmr_ratio<6 ~ "5 to <6",
    asmr_ratio>=6 & asmr_ratio<7 ~ "6 to <7",
    asmr_ratio>=7 & asmr_ratio<8 ~ "7 to <8",
    asmr_ratio>=8 ~ ">=8",
    TRUE ~ "")) %>% 
  filter(asmr_ratio_cat!="") %>% 
  dplyr::mutate(asmr_ratio_cat = factor(asmr_ratio_cat,
                                 levels = c("0 to <1",
                                            "1 to <2",
                                            "2 to <3",
                                            "3 to <4",
                                            "4 to <5",
                                            "5 to <6",
                                            "6 to <7",
                                            "7 to <8",
                                            ">=8")))

map.world_joined_fig4 %>% ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  ggsn::scalebar(map.world_joined_fig3,dist = 1500,st.size=6, 
                 height=0.01, transform=T, model = 'International',
                 dist_unit = "km",location = "bottomleft")+
  ggsn::north(map.world_joined_fig3,location = "topright",
              scale = 0.1,symbol = 1)+
  theme(panel.background = element_rect(fill = "#efedf5")
        ,plot.background = element_rect(fill = "#efedf5")
        ,panel.grid = element_blank()
        ,plot.title = element_text(hjust = 0.5,size = 20)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.text = element_text(color = "#444444",size = 20)
        ,legend.title = element_text(face = "bold",color = "#444444",
                                     size = 30)
        ,legend.background = element_blank()
        ,legend.key = element_blank())+
  borders(database = "world",regions = ctry_vtr,colour = "white")

# ggsave("figure 3a_2019.png",width = 80, height = 40, units = "cm",dpi=300,
#       limitsize = F) 
# Some regions and nations should be outlined.
fig4_cbn <- map.world_joined_fig4 %>% 
  filter(region %in% c("Antigua and Barbuda","Bahamas","Barbados",
                       "Belize","Bermuda","Cuba","Dominica","Dominican Republic",
                       "Grenada","Guyana","Haiti","Jamaica","Puerto Rico",
                       "Saint Vincent and the Grenadines",
                       "Suriname","Trinidad and Tobago",
                       "United States Virgin Islands")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-80,y=5,label="Caribbean",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_atg <- map.world_joined_fig4 %>% 
  filter(region=="Antigua and Barbuda") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-61.85,y=17.01,label="ATG",size=8) +
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_vct <- map.world_joined_fig4 %>% 
  filter(region=="Saint Vincent and the Grenadines") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-61.3,y=12.8,label="VCT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_brb <- map.world_joined_fig4 %>% 
  filter(region=="Barbados") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-59.6,y=13.075,label = "BRB",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_dma <- map.world_joined_fig4 %>% 
  filter(region=="Dominica") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-61.41,y=15.3,label="DMA",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_grd <- map.world_joined_fig4 %>% 
  filter(region=="Grenada") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-61.75,y=12.2,label="GRD",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_com <- map.world_joined_fig4 %>% 
  filter(region=="Comoros") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=43.5,y=-12.2,label="COM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_mdv <- map.world_joined_fig4 %>% 
  filter(region=="Maldives") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=73.42,y=3.375,label="MDV",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_mus <- map.world_joined_fig4 %>% 
  filter(region=="Mauritius") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=57.41,y=-20.04,label="MUS",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_lca <- map.world_joined_fig4 %>% 
  filter(region=="Saint Lucia") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-61.03,y=14.05,label="LCA",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_tto <- map.world_joined_fig4 %>% 
  filter(region=="Trinidad and Tobago") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-61.5,y=11.25,label="TTO",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_tls <- map.world_joined_fig4 %>% 
  filter(region=="Timor-Leste") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=124.9,y=-8.2,label="TLS",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_syc <- map.world_joined_fig4 %>% 
  filter(region=="Seychelles") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=55.425,y=-4.75,label="SYC",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_waf <- map.world_joined_fig4 %>% 
  filter(region %in% c("Benin","Burkina Faso","Cameroon","Cabo Verde",
                       "Chad","Côte d'Ivoire","The Gambia","Ghana","Guinea",
                       "Guinea-Baissau","Liberia","Mali","Mauritania",
                       "Niger","Nigeria","Sao Tome and Principe","Senegal",
                       "Sierra Leone","Togo")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-10,y=4,label="W Africa",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_pgf <- map.world_joined_fig4 %>% 
  filter(region %in% c("Bahrain","Qatar","Oman","Iraq","Iraq","Iran (Islamic Republic of)",
                       "Saudi Arabia","Kuwait","United Arab Emirates")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=45,y=17,label="Persian Gulf",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_emd <- map.world_joined_fig4 %>% 
  filter(region %in% c("Cyprus","Greece","Palestine","Lebanon",
                       "Syria","Jordan","Turkey","Egypt","Israel")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=27,y=23,label="E Med",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_mlt <- map.world_joined_fig4 %>% 
  filter(region=="Malta") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=14.28,y=35.85,label="MLT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_sgp <- map.world_joined_fig4 %>% 
  filter(region=="Singapore") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=103.74,y=1.28,label="SGP",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_bpa <- map.world_joined_fig4 %>% 
  filter(region %in% c("Slovenia","Croatia","Bosnia and Herzegovina",
                       "Serbia","North Macedonia","Italy","Bulgaria","Albania",
                       "Greece","Turkey","Romania","Republic of Moldova",
                       "Hungary","Montenegro","Austria")) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=25,y=36,label="Balkan Penisula",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_mhl <- map.world_joined_fig4 %>% 
  filter(region=="Marshall Islands") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=168,y=6.5,label="MHL",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Marshall Islands",colour = "black")

fig4_slb <- map.world_joined_fig4 %>% 
  filter(region=="Solomon Islands") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=158,y=-11.5,label="SLB",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Solomon Islands",colour = "black")

fig4_vut <- map.world_joined_fig4 %>% 
  filter(region=="Vanuatu") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=167.5,y=-19,label="VUT",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Vanuatu",colour = "black")

fig4_fji <- map.world_joined_fig4 %>% 
  filter(region=="Fiji") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-125,y=-20,label="FJI",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Fiji",colour = "black")

fig4_kir <- map.world_joined_fig4 %>% 
  filter(region=="Kiribati") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-125,y=-10,label="KIR",size=8) +
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Kiribati",colour = "black")

fig4_fsm <- map.world_joined_fig4 %>% 
  filter(region=="Micronesia (Federated States of)") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b"))+
  annotate("text",x=145,y=5.9,label="FSM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")+
  borders(database = "world" ,regions = "Micronesia",colour = "black")

fig4_wsm <- map.world_joined_fig4 %>% 
  filter(region=="Samoa") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-172.4,y=-14,label="WSM",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

fig4_ton <- map.world_joined_fig4 %>% 
  filter(region=="Tonga") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = asmr_ratio_cat)) +
  scale_fill_manual(name="Suicide age ratio",
                    values = c(`0 to <1` = "#f7fbff",
                               `1 to <2` = "#deebf7",
                               `2 to <3` = "#c6dbef",
                               `3 to <4` = "#9ecae1",
                               `4 to <5` = "#6baed6",
                               `5 to <6` = "#4292c6",
                               `6 to <7` = "#2171b5",
                               `7 to <8` = "#08519c",
                               `>=8` = "#08306b")) +
  annotate("text",x=-175,y=-21,label="TON",size=8)+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "#efedf5")
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,panel.grid = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "none")

# bind plots
fig4_combo1 <- ggpubr::ggarrange(fig4_atg,fig4_vct,fig4_brb,fig4_com,
                                 fig4_dma,fig4_grd,fig4_mdv,fig4_mus,
                                 fig4_lca,fig4_tto,fig4_tls,fig4_syc,
                                 ncol = 4,nrow = 3)

fig4_combo2 <- ggpubr::ggarrange(fig4_mlt,fig4_sgp,
                                 ncol=1,nrow = 2)

fig4_combo2_puls <- ggpubr::ggarrange(fig4_waf,fig4_emd,
                                      fig4_pgf,fig4_combo2,
                                      ncol = 2,nrow = 2)

fig4_combo3 <- ggpubr::ggarrange(fig4_mhl,fig4_kir,
                                 fig4_slb,fig4_fsm,
                                 fig4_vut,fig4_wsm,
                                 fig4_fji,fig4_ton,
                                 ncol = 2,nrow = 4)

ggpubr::ggarrange(fig4_cbn,fig4_combo1,fig4_combo2_puls,
                  fig4_combo3,ncol = 4)

# ggsave("figure 3b_2019.png",width = 80, height = 16, units = "cm",dpi=300,
#        limitsize = F)
# Two figures are exported and combined into a new one through PPT.

# Appendix Table S1. Total number of older adult (OA) suicides, OA age-standardized suicide-rates (ASSR) per 100,000 in 1990 and 2019, 
# total percent change in OA ASSR from 1990 to 2019, with uncertainty intervals (UI), and older-adult to non-older-adult (OA-NOA) suicide-rates ratios, 
# in 2019, for all countries.
# total dth number
app_tbl1_dthnum <- country %>% 
  filter(measure=="Deaths" & metric=="Number" & sex=="Both" &
           year %in% c(1990,2019)) %>% 
  group_by(location,year,sex) %>% 
  mutate(wider_sd = if_else((upper-val)>(lower-val),upper,lower),
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
colnames(app_tbl1_dthnum)[2:7] <-
  paste0(colnames(app_tbl1_dthnum)[2:7],"_all_age_deaths_number")

app_tbl1_dthnum <- app_tbl1_dthnum %>% janitor::clean_names()

# no decimal
app_tbl1_dthnum[,2:7] <- round(app_tbl1_dthnum[,2:7],digits = 0)

# change the format
app_tbl1_dthnum <- app_tbl1_dthnum %>% 
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
app_tbl1_asmr <- country %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(age_grp_num = val_Deaths_Number * 10^10 / val_Deaths_Rate,
         wider_sd = if_else((upper_Deaths_Number - val_Deaths_Number)>
                              (lower_Deaths_Number - val_Deaths_Number),
                            upper_Deaths_Number,lower_Deaths_Number),
         sd = (wider_sd - val_Deaths_Number) / 1.96 * sqrt(1000),
         var = sd^2) %>% 
  dplyr::group_by(location,year,sex) %>% 
  dplyr::summarize(sum_age_grp_num = sum(age_grp_num),
                   val = sum(val_Deaths_Rate*(age_grp_num/sum_age_grp_num)),
                   sum_var = sum(var)) %>% 
  dplyr::mutate(sd = 10^10 * sqrt(sum_var) / (sum_age_grp_num * sqrt(1000)),
         upper = val + 1.96 * sd,
         lower = val - 1.96 * sd) %>% 
  select(-c(sum_age_grp_num,sum_var)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = c("val","lower","upper","sd"))

# change the column names
colnames(app_tbl1_asmr)[2:9] <- 
  paste0(colnames(app_tbl1_asmr)[2:9],"_std_dth_rate")

app_tbl1_asmr <- app_tbl1_asmr %>% janitor::clean_names()

# older to younger ASMR ratio
app_tbl1_asmr_ratio <- country %>% 
  filter(measure=="Deaths" & sex=="Both" & year %in% c(1990,2019)) %>% 
  select(-cause) %>% 
  pivot_wider(names_from = c("measure","metric"),
              values_from = c("val","upper","lower")) %>% 
  dplyr::mutate(age_grp_num = val_Deaths_Number * 100000 / (val_Deaths_Rate / 100000),
         age_elder = if_else(age %in% c("10-14 years","15-19 years",
                                        "20-24 years","25-29 years",
                                        "30-34 years","35-39 years",
                                        "40-44 years","45-49 years",
                                        "50-54 years","55-59 years"),
                             "10-59 years","60 plus")) %>% 
  dplyr::group_by(location,year,sex,age_elder) %>% 
  dplyr::mutate(sum_age_grp_num = sum(age_grp_num),
         std_prop_wt = age_grp_num / sum_age_grp_num) %>% 
  dplyr::summarize(std_dth_rate = sum(val_Deaths_Rate*std_prop_wt)) %>% 
  pivot_wider(names_from = "age_elder",
              values_from = "std_dth_rate") %>% 
  dplyr::mutate(std_dth_rratio = `60 plus` / `10-59 years`) %>% 
  select(-c(`10-59 years`,`60 plus`)) %>% 
  pivot_wider(names_from = c("year","sex"),
              values_from = "std_dth_rratio")

# change the column names
colnames(app_tbl1_asmr_ratio)[2:3] <- 
  paste0("val_",colnames(app_tbl1_asmr_ratio)[2:3],
         "_std_dth_rate_ratio")

app_tbl1_asmr_ratio <- app_tbl1_asmr_ratio %>% janitor::clean_names()

# round to 2 decimal points
app_tbl1_asmr_ratio[,2:3] <- round(app_tbl1_asmr_ratio[2:3], digits = 2)

# percent change
# get mean and sd of asmr in 1990 and 2017
mu1 <- pull(app_tbl1_asmr,val_1990_both_std_dth_rate)
mu2 <- pull(app_tbl1_asmr,val_2019_both_std_dth_rate)

sd1 <- pull(app_tbl1_asmr,sd_1990_both_std_dth_rate) 
sd2 <- pull(app_tbl1_asmr,sd_2019_both_std_dth_rate)

# simulation
lst1 <- list()
lst2 <- list()

set.seed(1)
for (i in 1:205) {
  lst1[[i]] <- sample(rnorm(1000,mean=mu1[i],sd=sd1[i]),
                      replace = FALSE)
  lst2[[i]] <- sample(rnorm(1000,mean=mu2[i],sd=sd2[i]),
                      replace=FALSE)
}

# percent change,variance and SD
pct_chg <- list()
pct_chg_mean <- rep(0,205)
pct_chg_var <- rep(0,205)
pct_chg_sd <- rep(0,205)

for (i in 1:205) {
  pct_chg[[i]] <- (lst2[[i]] - lst1[[i]]) / lst1[[i]]
  pct_chg_mean[i] <- mean(pct_chg[[i]])
  pct_chg_var[i] <- var(pct_chg[[i]])
  pct_chg_sd[i] <- sqrt(pct_chg_var[i])
}

# p-value, null=0
p_value_pct_chg_std_dth_rate <- rep(0,205)

for (i in 1:205) {
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

location <- pull(app_tbl1_asmr,location)

app_tbl1_asmr_pctchg <- tibble(location,
                               val_pct_chg_std_dth_rate,
                               upper_pct_chg_std_dth_rate,
                               lower_pct_chg_std_dth_rate,
                               p_value_pct_chg_std_dth_rate)

# round asmr to 2 decimal points
app_tbl1_asmr[2:9] <- round(app_tbl1_asmr[2:9],digits = 2)

# round pct change to 3 decimal points
app_tbl1_asmr_pctchg[2:5] <- round(app_tbl1_asmr_pctchg[2:5],digits = 2)

# change the format
app_tbl1_asmr <- app_tbl1_asmr %>% 
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

app_tbl1_asmr_pctchg <- app_tbl1_asmr_pctchg %>% 
  dplyr::mutate(pct_chg_ui = paste0(val_pct_chg_std_dth_rate,
                             " (",lower_pct_chg_std_dth_rate,
                             ", ",upper_pct_chg_std_dth_rate,")")) %>% 
  select(-c(val_pct_chg_std_dth_rate,
            lower_pct_chg_std_dth_rate,
            upper_pct_chg_std_dth_rate))

# merge tables together
app_merged_tbl1 <- left_join(app_tbl1_dthnum,app_tbl1_asmr,by="location") %>% 
  left_join(.,app_tbl1_asmr_pctchg,by="location") %>% 
  left_join(.,app_tbl1_asmr_ratio,by="location")

# write.csv(app_merged_tbl1,"Appendix_table1_2019.csv")

