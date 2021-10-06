#### Libraries ####
library("tidyverse")
library("lubridate")
library("ggpubr")

#### Load data ####

removal <-read.csv("Data/Removal_experiment_biomass_data.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)

#### Clean data ####

removal1 <- removal %>% 
  mutate(Woody = ifelse(Woody == "2,413.00000", "", Woody),
         Litter = ifelse(Litter == "N/A", "", Litter)) %>% 
  mutate(Woody = as.numeric(Woody),
         Litter = as.numeric(Litter)) %>% 
  mutate(plotID = paste0(Site, "_", Plot)) %>% 
  pivot_longer(cols = c("Graminoid", "Forb", "Bryophyte", "Woody", "Fern", "Lichen", "Litter"), names_to = "FunctionalGroup", values_to = "value") %>% 
  mutate(Site = factor(Site, levels = c("ULV", "LAV", "GUD", "SKJ")), #Ordering the sites from dry to wet
         FunctionalGroup = factor(FunctionalGroup, levels = c("Graminoid", "Forb", "Fern","Woody", "Bryophyte", "Lichen", "Litter"))) %>%  #Ordering the functional groups like I wan them in the figure
  group_by(Year, plotID, FunctionalGroup) %>% 
  mutate(value = sum(value))
  

#### Plot data ####

#### Color palette ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

#### Biomass removal plot ####

biomass_2019 <- removal1 %>% 
  filter(Year == 2019) %>% 
  ggplot(aes(y = value, x = Site, fill = FunctionalGroup))+
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(Year~Treatment, nrow = 1) +
  scale_fill_manual(values=rev(cbPalette)) +
  ggtitle("Biomass removed in the first year") +
  ylab("Biomass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

biomass_2020_2021 <- removal1 %>% 
  filter(!Year == 2019) %>% 
ggplot(aes(y = value, x = Site, fill = FunctionalGroup))+
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(Year~Treatment, nrow = 3) +
  scale_fill_manual(values=rev(cbPalette)) +
  ggtitle("Biomass removed in following years") +
  ylab("Biomass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none")

biomass_figure <- ggarrange(biomass_2019, biomass_2020_2021, nrow = 2, ncol = 1,  heights = c(2,3),  common.legend = TRUE,legend = "bottom")
