### Soil moisture plots and data ###

## Load libraries ##

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

## Load data ##

SM_plot <- read.csv2("Data/SeedClim_Plot_SoilMoisture.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
SM_site <- read.csv2("Data/SeedClim_Site_Soil_Moisture.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
community_2016 <- read.csv2("Data/funcab_composition_2016.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)
load(file = "Data/Soilmoisture.RData")

## Cleaning dataset site level ##

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
         SM_sd = sd(value))

Overview_SM_site <- SoilMoisture_1 %>% 
  ungroup() %>% 
  select(site, SM_mean, SM_sd) %>% 
  unique()

SoilMoisture_1 %>% 
  ggplot(aes(x = site, y = value)) +
  geom_boxplot()

SoilMoisture_1 %>% 
  ggplot(aes(x = value, fill = site)) +
  geom_density(alpha = 0.6) +
  scale_fill_brewer(palette = "Blues", direction =-1) +
  ggtitle("Soil moisture for june-september from 2009 to 2019")


## Cleaning dataset site ##

SM_site_1 <- SM_site %>% 
  select(-X) %>%
  filter(site %in% c("Gudmedalen", "Lavisdalen", "Ulvehaugen", "Skjellingahaugen")) %>% 
  mutate(soilMoisture = as.numeric(soilMoisture),
         date = ymd_hms(date),
         day = date(date)) %>% 
  group_by(site) %>% 
  mutate(SM_avg_total = mean(soilMoisture),
         SM_sd_total = sd(soilMoisture)) %>% 
  group_by(site, logger, day) %>% 
  mutate(SM_avg = mean(soilMoisture),
         SM_sd = sd(soilMoisture))

SM_site_average <- SM_site_1 %>% 
  ungroup() %>% 
  select(site, SM_avg_total, SM_sd_total) %>% 
  distinct()

SM_site_1 %>% 
  ggplot(aes(x = day), )

SM_site_1 %>% 
  ggplot(aes(x = site, y = soilMoisture)) +
  geom_boxplot()

## Cleaning dataset plot ##

SM_plot_1 <- SM_plot %>% 
  select(-X) %>%
  filter(site %in% c("Gudmedalen", "Lavisdalen", "Ulvehaugen", "Skjellingahaugen")) %>% 
  mutate(soil_moisture = as.numeric(soil_moisture),
         date = ymd(date)) %>%  
  group_by(site) %>% 
  mutate(SM_avg_site = mean(soil_moisture),
         SM_lower_site = mean(soil_moisture) - sd(soil_moisture),
         SM_upper_site = mean(soil_moisture) + sd(soil_moisture)) %>% 
  group_by(site, SeedClim_plotID) %>% 
  mutate(SM_avg_plot = mean(soil_moisture),
         SM_lower_plot = mean(soil_moisture) - sd(soil_moisture),
         SM_upper_plot = mean(soil_moisture) + sd(soil_moisture))

SM_plot_average <- SM_plot_1 %>% 
  ungroup() %>% 
  select(site, SM_avg_site, SM_upper_site, SM_lower_site) %>% 
  distinct()

SM_plot_1 %>% 
  ggplot(aes(x = site, y = soil_moisture)) +
  geom_boxplot()


## Cleaning community data ##

community_2016_SP_VA <- community_2016 %>% 
  select(Site, turfID, Measure, Sib.pro, Ver.alp) %>% 
  filter(Measure == "Cover") %>% 
  filter(!Site == "") %>% 
  filter(Site %in% c("Lavisdalen", "Gudmedalen", "Ulvhaugen", "Skjellingahaugen")) %>% 
  mutate(Sib.pro = as.numeric(Sib.pro),
         Ver.alp = as.numeric(Ver.alp))


## Merging community data and soil moisture plot data ##

moisture_community <- community_2016_SP_VA %>% 
  left_join(SM_plot_1, by = c("turfID" = "FunCaB_plotID"))
  
moisture_community_1 <- moisture_community %>% 
  select(Site, turfID, Sib.pro, Ver.alp, date, soil_moisture) %>% 
  pivot_longer(cols = Sib.pro:Ver.alp,
             names_to = "species",
             values_to = "cover") %>% 
  #filter(!is.na(cover)) %>%
  mutate(species = ifelse(is.na(cover), "None", species)) %>% 
  group_by(Site, species) %>% 
  mutate(SM_site_mean = mean(soil_moisture),
         SM_site_lower = mean(soil_moisture) - sd(soil_moisture),
         SM_site_upper = mean(soil_moisture) + sd(soil_moisture))

moisture_community_1 %>% 
  ggplot(aes(x = species, y = soil_moisture, fill = species)) +
  geom_boxplot() +
  geom_jitter() +
  coord_flip()

moisture_community_1 %>% 
  ggplot(aes(x = soil_moisture, fill = species)) +
  geom_density(alpha = 0.2)


## Making a plot for INCLINE sites ##

dat <- read.table(header = TRUE, text = "
siteID           latitude longitude Temperature Precipitation
Gudmedalen       60.8328    7.17561           1             3
Låvisdalen       60.8231    7.27596           1             2
Skjellingahaugen 60.9335    6.41504           1             4
Ulvehaugen       61.0243    8.12343           1             1
")
dat$Precipitation <- factor(dat$Precipitation)
precipLab <- c("Very dry", "Dry", "Wet", "Very wet")

xlim <- range(dat$longitude) + c(-1, 0.5)
ylim <- range(dat$latitude) + c(-0.5, 1)


norwaymap <- map_data("world", "Norway")
norwaymapHires <- map_data("worldHires", "Norway")


Scandinaviamap <- map_data("world", c("Norway", "Sweden", "Finland"))
norwaymapHires <- map_data("worldHires", "Norway")

a <- ggplot() +
  geom_map(data = Scandinaviamap, aes(x = long, y = lat, map_id = region, alpha = 0.6),
           map = Scandinaviamap, colour = "black", fill = "grey60") +
  geom_rect(data = data.frame(),
            aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            colour = "black", fill = "#76B4FF", alpha = 0.8) +
  coord_map(xlim = c(4, 32), ylim = c(55, 71)) +
  labs(x = NULL, y = NULL) +
  guides(alpha = FALSE) +
  theme(text = element_text(size = 34), 
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))


b <- ggplot(dat, aes(x = longitude, y = latitude, fill = Precipitation, label = siteID)) +
  geom_map(aes(x = long, y = lat, map_id = region), data = norwaymapHires, map = norwaymapHires, color = "black", fill = "grey70", inherit.aes = FALSE) +
  geom_point(shape = 21, size = 12, colour = "black") +
  geom_text_repel(aes(label = siteID),
                  nudge_y = -0.15,
                  segment.color = "black",
                  size = 7) +
  coord_map(xlim = xlim, ylim = ylim) +
  labs(x = NULL, y = NULL, fill = "Moisture") +
  scale_fill_brewer(palette = "Blues",
                    breaks = 1:4,
                    labels = c("Very dry", "Dry", "Wet", "Very wet"))+
  theme(text = element_text(size = 32))
  # annotation_north_arrow(location = "br", which_north = "true", 
  #                        pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
  #                        style = north_arrow_fancy_orienteering)
#print(b)



# 
# maptheme <- theme(
#   axis.text = element_blank(),
#   axis.ticks = element_blank(),
#   panel.grid = element_blank(),
#   panel.border = element_rect(fill = NA, colour = "black"),
#   panel.background = element_blank()
# )

# png("INCLINE_sites.png", width = 1285, height = 861)
# 
# grid.newpage()
# vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
# vp_a <- viewport(width = 0.4, height = 0.4, x = 0.64, y = 0.8)  # the inset in upper left
# print(b, vp = vp_b)
# print(a, vp = vp_a)
# 
# dev.off()

## Different way of doing this ##

a2 <- ggplot() +
  geom_map(data = Scandinaviamap, aes(x = long, y = lat, map_id = region, alpha = 0.6),
           map = Scandinaviamap, colour = "black", fill = "grey60") +
  geom_rect(data = data.frame(),
            aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            colour = "black", fill = "#76B4FF", alpha = 0.8) +
  coord_map(xlim = c(4, 32), ylim = c(55, 71)) +
  labs(x = NULL, y = NULL) +
  guides(alpha = FALSE) +
  theme(text = element_text(size = 22), 
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

b2 <- ggplot(dat, aes(x = longitude, y = latitude, fill = Precipitation, label = siteID)) +
  geom_map(aes(x = long, y = lat, map_id = region), data = norwaymapHires, map = norwaymapHires, color = "black", fill = "grey70", inherit.aes = FALSE) +
  geom_point(shape = 21, size = 8, colour = "black") +
  geom_text_repel(aes(label = siteID),
                  nudge_y = -0.15,
                  segment.color = "black",
                  size = 5) +
  coord_map(xlim = xlim, ylim = ylim) +
  labs(x = NULL, y = NULL, fill = "Moisture") +
  scale_fill_brewer(palette = "Blues",
                    breaks = 1:4,
                    labels = c("Very dry", "Dry", "Wet", "Very wet"))+
  theme(text = element_text(size = 22),
        legend.position = "none")

site_info_table <- read.table(header = TRUE, text = "
siteID            Moisture    Temperature   Precipitation  SoilMoisture
Ulvehaugen         VeryDry           6.17             596         0.282
Låvisdalen             Dry           6.45            1321         0.389
Gudmesdalen            Wet           5.87            1925         0.507
Skjellingahaugen   VeryWet           6.58            2725         0.531
")

site_info <- ggtexttable(site_info_table, 
                         rows = NULL,
                         theme = ttheme(
                           colnames.style = colnames_style(fill = "white"),
                           tbody.style = tbody_style(fill = get_palette("Blues", 4))
                         ))



ggarrange(b2,site_info,
          ncol = 2,
          widths = c(1, 3))



ggsave("INCLINE_sites_table.png", width = 10, height = 4, units = cm)
