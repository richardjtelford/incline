#### Germination data ####

## Load libraries ##

library(tidyverse)
library(lubridate)

## Load data ##

VA_germ <-read.csv2("Data/INCLINE_Germination_Seedling_Experiment_Data_2020_03_31_VA.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)
SP_germ <-read.csv2("Data/INCLINE_Germination_Seedling_Experiment_Data_2020_03_31_SP.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)


## Veronical alpina ##

VA_germ1 <- VA_germ %>% 
  filter(!Germination_date %in% c("21.02", "20.2.20")) %>% 
  mutate(Start_date = dmy(Start_date)) %>% 
  mutate(Germination_date = dmy(Germination_date)) %>% 
  mutate(Cotelydon_date = dmy(Cotelydon_date)) %>% 
  mutate(Leaf_date = dmy(Leaf_date)) %>% 
  mutate(Petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(Days_to_germination = Germination_date - Start_date,
         Days_to_cotelydon = Cotelydon_date - Start_date,
         Days_to_leaf = Leaf_date - Start_date) %>% 
  mutate(Site_WP = paste(Site, Water_potential)) %>% 
  mutate(Water_potential = as.factor(Water_potential)) %>% 
  group_by(Species, Site, Water_potential, Replicate) %>% 
  mutate(Seeds_in_dish = n())

VA_germ_analysis <- VA_germ1 %>% 
  filter(!is.na(Germination_date)) %>% 
  group_by(Species, Site, Water_potential, Replicate, Days_to_germination) %>% 
  mutate(Seeds_germinated = n()) %>% 
  mutate(Germination_percent = Seeds_germinated/Seeds_in_dish * 100) %>% 
  select(Species, Site, Water_potential, Replicate, Petri_dish, Days_to_germination, Seeds_germinated, Germination_percent) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(Petri_dish) %>% 
  arrange(Days_to_germination) %>% 
  mutate(Cum_germ_percent = cumsum(Germination_percent))

VA_cotelydon_analysis <- VA_germ1 %>% 
  filter(!is.na(Cotelydon_date)) %>% 
  group_by(Species, Site, Water_potential, Replicate, Days_to_cotelydon) %>% 
  mutate(Seeds_cotelydon = n()) %>% 
  mutate(Cotelydon_percent = Seeds_cotelydon/Seeds_in_dish * 100) %>% 
  select(Species, Site, Water_potential, Replicate, Petri_dish, Days_to_cotelydon, Seeds_cotelydon, Cotelydon_percent) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(Petri_dish) %>% 
  arrange(Days_to_cotelydon) %>% 
  mutate(Cum_cot_percent = cumsum(Cotelydon_percent))

Plot_cotelydon_graph(data = VA_cotelydon_analysis, site = "SKJ", species = "Veronica alpina")

## Sibbaldia procumbens ##

SP_germ1 <- SP_germ %>% 
  mutate(Replicate = as.factor(Replicate)) %>% 
  mutate(Start_date = dmy(Start_date)) %>% 
  mutate(Germination_date = dmy(Germination_date)) %>% 
  mutate(Cotelydon_date = dmy(Cotelydon_date)) %>% 
  mutate(Leaf_date = dmy(Leaf_date)) %>% 
  mutate(Petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(Days_to_germination = Germination_date - Start_date,
         Days_to_cotelydon = Cotelydon_date - Start_date,
         Days_to_leaf = Leaf_date - Start_date) %>% 
  mutate(Site_WP = paste(Site, Water_potential)) %>% 
  mutate(Water_potential = as.factor(Water_potential)) %>% 
  group_by(Species, Site, Water_potential, Replicate) %>% 
  mutate(Seeds_in_dish = n())

SP_germ_analysis <- SP_germ1 %>% 
  filter(!is.na(Germination_date)) %>% 
  group_by(Species, Site, Water_potential, Replicate, Days_to_germination) %>% 
  mutate(Seeds_germinated = n()) %>% 
  mutate(Germination_percent = Seeds_germinated/Seeds_in_dish * 100) %>% 
  select(Species, Site, Water_potential, Replicate, Petri_dish, Days_to_germination, Seeds_germinated, Germination_percent) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(Petri_dish) %>% 
  arrange(Days_to_germination) %>% 
  mutate(Cum_germ_percent = cumsum(Germination_percent))

SP_cotelydon_analysis <- SP_germ1 %>% 
  filter(!is.na(Cotelydon_date)) %>% 
  group_by(Species, Site, Water_potential, Replicate, Days_to_cotelydon) %>% 
  mutate(Seeds_cotelydon = n()) %>% 
  mutate(Cotelydon_percent = Seeds_cotelydon/Seeds_in_dish * 100) %>% 
  select(Species, Site, Water_potential, Replicate, Petri_dish, Days_to_cotelydon, Seeds_cotelydon, Cotelydon_percent) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(Petri_dish) %>% 
  arrange(Days_to_cotelydon) %>% 
  mutate(Cum_cot_percent = cumsum(Cotelydon_percent))





## Function for plotting data ##

## Translation for water potential levels ##

WP_names <- c(
  "1" = "WP 1 (-0.25 MPa)",
  "2" = "WP 2 (-0.33 MPa)",
  "3" = "WP 3 (-0.42 MPa)",
  "4" = "WP 4 (-0.50 MPa)",
  "5" = "WP 5 (-0.57 MPa)",
  "6" = "WP 6 (-0.70 MPa)",
  "7" = "WP 7 (-0.95 MPa)",
  "8" = "WP 8 (-1.20 MPa)",
  "9" = "WP 9 (-1.45 MPa)",
  "10" = "WP 10 (-1.70 MPa"
)

## Function for plots ##

Plot_germination_graph <- function(data, site, species){
  
  data <- data %>% 
    filter(Site == site)
  
  plot <- ggplot(aes(x = sort(as.integer(Days_to_germination)), y = Cum_germ_percent, color = Replicate), data = data) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Water_potential, labeller = as_labeller(WP_names)) +
    xlab("Days to germination") +
    ylab("Germination %") +
    ggtitle(paste(species, "from", site)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5,
                                          linetype = "solid"),
          panel.grid.major = element_line(size = 0.25,
                                          linetype = 'solid',
                                          colour = "lightgrey")) +
    scale_x_continuous(limits = c(0,42)) +
    scale_y_continuous(limits = c(0,100))
  
  return(plot)
}

Plot_cotelydon_graph <- function(data, site, species){
  
  data <- data %>% 
    filter(Site == site)
  
  plot <- ggplot(aes(x = sort(as.integer(Days_to_cotelydon)), y = Cum_cot_percent, color = Replicate), data = data) +
    geom_point() +
    geom_line() +
    facet_wrap(~ Water_potential, labeller = as_labeller(WP_names)) +
    xlab("Days to cotelydon emergance") +
    ylab("Germination %") +
    ggtitle(paste(species, "from", site)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          size = 0.5,
                                          linetype = "solid"),
          panel.grid.major = element_line(size = 0.25,
                                          linetype = 'solid',
                                          colour = "lightgrey")) +
    scale_x_continuous(limits = c(0,42)) +
    scale_y_continuous(limits = c(0,100))
  
  return(plot)
}


## Plot data ##
Plot_cotelydon_graph(data = VA_cotelydon_analysis, site = "SKJ", species = "Veronica alpina")

Plot_cotelydon_graph(data = SP_cotelydon_analysis, site = "SKJ", species = "Sibbaldia procumbens")

Plot_germination_graph(data = VA_germ_analysis, site = "SKJ", species = "Veronica alpina")
#ggsave("VA_SKJ.png", width = 15, height = 10, unit = "cm")

Plot_germination_graph(data = VA_germ_analysis, site = "GUD", species = "Veronica alpina")
#ggsave("VA_GUD.png", width = 15, height = 10, unit = "cm")

Plot_germination_graph(data = VA_germ_analysis, site = "LAV", species = "Veronica alpina")
#ggsave("VA_LAV.png", width = 15, height = 13, unit = "cm")

Plot_germination_graph(data = VA_germ_analysis, site = "ULV", species = "Veronica alpina")
#ggsave("VA_ULV.png", width = 15, height = 13, unit = "cm")


Plot_germination_graph(data = SP_germ_analysis, site = "SKJ", species = "Sibbaldia procumbens")
#ggsave("SP_SKJ.png", width = 15, height = 13, unit = "cm")

Plot_germination_graph(data = SP_germ_analysis, site = "GUD", species = "Sibbaldia procumbens")
#ggsave("SP_GUD.png", width = 15, height = 7, unit = "cm")

Plot_germination_graph(data = SP_germ_analysis, site = "LAV", species = "Sibbaldia procumbens")
#ggsave("SP_LAV.png", width = 15, height = 13, unit = "cm")

Plot_germination_graph(data = SP_germ_analysis, site = "ULV", species = "Sibbaldia procumbens")
#ggsave("SP_ULV.png", width = 15, height = 10, unit = "cm")
