#### Making figure for germination paper with soil moisture, precipitation and geography info about sites ####

#### Load libraries ####

library(tidyverse)
library(lubridate)
library(ggmap)
library(maps)
library(mapdata)
library(grid)
library(ggspatial)
library(ggsn)
library(ggpubr)
library(ggrepel)

#### Load data ####

#SM_plot <- read.csv2("Data/SeedClim_Plot_SoilMoisture.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
#SM_site <- read.csv2("Data/SeedClim_Site_Soil_Moisture.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
#community_2016 <- read.csv2("Data/funcab_composition_2016.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)
load(file = "Data/Soilmoisture.RData")
#load(file = "Data/Precipitation.RData")

#### Cleaning soil moisture dataset ####

SoilMoisture <- soilmoisture %>% 
  filter(site %in% c("gud", "lav", "skj", "ulv")) %>% 
  ungroup() %>% 
  select(date, logger, value, site) %>% 
  filter(!is.na(value))

SoilMoisture_1 <- SoilMoisture %>%
  mutate(month = month(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(site = factor(site, levels = c("skj", "gud", "lav", "ulv"))) %>% 
  filter(month %in% c(6:9)) %>% 
  group_by(site) %>% 
  mutate(SM_mean = mean(value),
         SM_sd = sd(value)) %>% 
  mutate(value = value*100)

Overview_SM_site <- SoilMoisture_1 %>% 
  ungroup() %>% 
  select(site, SM_mean, SM_sd) %>% 
  unique()

#### Cleaning precipitation dataset ####

Precipitation <- precipitation %>% 
  filter(site %in% c("gud", "lav", "skj", "ulv")) %>% 
  filter(logger == "nedbor") %>% 
  ungroup() %>% 
  select(date, value, site) %>% 
  filter(!is.na(value))

Precipitation_1 <- Precipitation %>%
  mutate(month = month(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(site = factor(site, levels = c("skj", "gud", "lav", "ulv"))) %>% 
  #filter(month %in% c(6:9)) %>% 
  group_by(site, year, month) %>% 
  mutate(precip_month = sum(value))
  # filter(precip_year > 10) %>% 
  # filter(precip_year < 2000)

Overview_precip <- Precipitation_1 %>% 
  ungroup() %>% 
  select(site, year, month, precip_month) %>% 
  unique() %>% 
  filter(precip_month > 10) %>% 
  filter(precip_month < 2000)


#### Visualize soil moisture data ####

SoilMoisture_1 %>% 
  mutate(Moist_level = recode(site, ulv = "Very dry", lav = "Dry",  gud = "Wet", skj = "Very wet")) %>%
  ggplot(aes(x = Moist_level, y = value, fill = Moist_level)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues", direction =-1) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = NULL) +
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 25),
        axis.text = element_text(size = 25),
        panel.grid.major.x = element_line( colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("INCLINE_moisture.png", width = 31, height = 10, units = "cm")

library(ggridges)
theme_set(theme_ridges())

SoilMoisture_1 %>% 
  ggplot(aes(x = value, y = site, fill = site)) +
  geom_density_ridges() +
  scale_fill_brewer(palette = "Blues", direction =-1) +
  ggtitle("Soil moisture for june-september from 2009 to 2019")


#### Visualize precipitation data ####

Overview_precip %>% 
  ggplot(aes(x = site, y = precip_month*10)) +
  geom_boxplot()+
  labs(x = "Site", y = "Annual precipiation (mm)")




#### Making a plot for INCLINE sites ####

dat <- read.table(header = TRUE, text = "
siteID           latitude longitude Temperature Precipitation
VeryWet         60.9335    6.41504           1             4
Wet             60.8328    7.17561           1             3
Dry             60.8231    7.27596           1             2
VeryDry         61.0243    8.12343           1             1
")

dat <- dat %>% 
  mutate(siteID = recode(siteID, "VeryDry" = "Very dry", "Dry" = "Dry",  "Wet" = "Wet", "VeryWet" = "Very wet")) %>% 
  mutate(Precipitation = as.factor(Precipitation))
         
         
precipLab <- c("Very dry", "Dry", "Wet", "Very wet")

xlim <- range(dat$longitude) + c(-1, 0.5)
ylim <- range(dat$latitude) + c(-0.5, 1)


norwaymap <- map_data("world", "Norway")
norwaymapHires <- map_data("worldHires", "Norway")


Scandinaviamap <- map_data("world", c("Norway", "Sweden", "Finland"))
norwaymapHires <- map_data("worldHires", "Norway")

#function for theme on plots

maptheme <- function(...) {
  theme(
    axis.text = element_text(size = 30),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    ...
  )
}



Big_map <- ggplot() +
  geom_map(data = Scandinaviamap, aes(x = long, y = lat, map_id = region, alpha = 0.6),
           map = Scandinaviamap, colour = "black", fill = "grey60") +
  geom_rect(data = data.frame(),
            aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            colour = "black", fill = "#76B4FF", alpha = 0.8) +
  coord_map(xlim = c(4, 32), ylim = c(55, 71)) +
  labs(x = NULL, y = NULL) +
  guides(alpha = FALSE) +
  maptheme()


Small_map <- ggplot(dat, aes(x = longitude, y = latitude, fill = Precipitation, label = siteID)) +
  geom_map(aes(x = long, y = lat, map_id = region), data = norwaymapHires, map = norwaymapHires, color = "black", fill = "grey70", inherit.aes = FALSE) +
  geom_point(shape = 21, size = 12, colour = "black", show.legend = FALSE) +
  geom_text_repel(aes(label = siteID),
                  nudge_y = -0.15,
                  segment.color = "black",
                  size = 10) +
  coord_map(xlim = xlim, ylim = ylim) +
  labs(x = NULL, y = NULL, fill = "Moisture") +
  scale_fill_brewer(palette = "Blues",
                    breaks = 1:4,
                    labels = c("Very dry", "Dry", "Wet", "Very wet"))+
  maptheme()



png("INCLINE_sites2.png", width = 1285, height = 861)
 
grid.newpage()
 vp_small_map <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the zoomed in map
 vp_big_map <- viewport(width = 0.4, height = 0.4, x = 0.71, y = 0.8)  # the inset in upper left of scandinacia
 print(Small_map, vp = vp_small_map)
print(Big_map, vp = vp_big_map)
 
dev.off()

## World clim data for precip map ##

library("raster")
 
clim_precip = getData('worldclim', var='bio', res=2.5) 
 
ma.area <- extent(5, 9, 60, 62.2)
WC_precip <- crop(clim_precip, ma.area)

library("rasterVis")

gplot(WC_precip$bio12)+
  geom_raster(aes(fill = value))
