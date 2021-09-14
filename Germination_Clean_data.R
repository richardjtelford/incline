#### Germination data ####

## Load libraries ##

library(tidyverse)
library(lubridate)
#library(extrafont)
#loadfonts(device = "win")

## Load data ##

VA_germ <- read.csv2("Data/INCLINE_Germination_Seedling_Experiment_Data_VA.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)
SP_germ <-read.csv2("Data/INCLINE_Germination_Seedling_Experiment_Data_SP.csv", header=TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
 

## Veronical alpina ##

## Fixing mistakes in dataset
VA_mistakes <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                            "old new
  02.02.2020 02.03.2020
  20.2.20 20.02.2020
  20.2.2020 20.02.2020
  21.02 21.02.2020
  13.04 13.04.2020 
  21.02.2002 20.02.2020
  04.02.2020 04.03.2020
  03.07.22020 03.07.2020
  6.7.2020 06.07.2020")



VA_germ1 <- VA_germ %>%
  mutate(Comment = ifelse(Germination_date %in% c("DEAD", "dead"), "Dead", Comment),
         Germination_date = ifelse(Germination_date %in% c("DEAD", "dead", " ", ""), NA, Germination_date),
         Harvest_comment = ifelse(Harvest_date == "Brown and a bit moldy", "Brown and a bit moldy", Harvest_comment),
         Harvest_date = ifelse(Harvest_date == "Brown and a bit moldy", NA, Harvest_date),
         Comment = ifelse(dead_date == "yellow", "yellow",
                          ifelse(dead_date %in% c("seed seems dead", "Seed seems dead"), "Seed seems dead", 
                                 ifelse(dead_date == "early", "dead",
                                        ifelse(dead_date == "meant to be harvested 20.04 but no plant?", "meant to be harvested 20.04 but no plant?", Comment)))),
         dead_date = ifelse(dead_date %in% c("yellow", "Unknown", "seed seems dead", "meant to be harvested 20.04 but no plant?", "early", "Seed seems dead"), NA,
                            ifelse(dead_date == "before 18.05.2020", "18.05.2020",
                                   ifelse(dead_date == "before 17.03.2020", "17.03.2020", dead_date))),
         Dry_mass_g_root = ifelse(Dry_mass_g_root == "X", NA, Dry_mass_g_root),
         Weighing_comments = ifelse(Dry_mass_g_total == "two roots; two seedlings; second seedling: Dry_mass_g_root=0.00036, Dry_mass_g_above_ground=0.00022", "two roots; two seedlings; second seedling: Dry_mass_g_root=0.00036, Dry_mass_g_above_ground=0.00022", 
                                    ifelse(Dry_mass_g_total == "two roots", "two roots", 
                                           ifelse(Dry_mass_g_total == "3 roots", "3 roots", Weighing_comments))),
         Dry_mass_g_total = ifelse(Dry_mass_g_total %in% c("two roots; two seedlings; second seedling: Dry_mass_g_root=0.00036, Dry_mass_g_above_ground=0.00022","two roots","3 roots"), NA, Dry_mass_g_total)) %>% 
  mutate(Germination_date = plyr::mapvalues(Germination_date, from = VA_mistakes$old, to = VA_mistakes$new),
         Cotelydon_date = plyr::mapvalues(Cotelydon_date, from = VA_mistakes$old, to = VA_mistakes$new),
         Leaf_date = plyr::mapvalues(Leaf_date, from = VA_mistakes$old, to = VA_mistakes$new)) %>% 
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
  mutate(Seeds_in_dish = n()) %>% 
  rename(ID = X)

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
  mutate(Cum_germ_percent = cumsum(Germination_percent)) %>% 
  ungroup() %>% 
  group_by(Species, Site, Water_potential, Days_to_germination) %>% 
  mutate(Average = mean(Cum_germ_percent)) %>% 
  ungroup() %>% 
  mutate(Days_to_germination = as.numeric(Days_to_germination))

Last_day_germination_score <- VA_germ_analysis %>% 
  group_by(Petri_dish) %>% 
  mutate(Max_germination = max(Cum_germ_percent)) %>% 
  select(Max_germination, Petri_dish, Species, Site, Water_potential, Replicate) %>% 
  mutate(Days_to_germination = 76)

VA_germ_analysis <- VA_germ_analysis %>% 
  bind_rows(Last_day_germination_score) %>% 
  mutate(Cum_germ_percent = ifelse(is.na(Cum_germ_percent), Max_germination, Cum_germ_percent))


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

VA_leaf_analysis <- VA_germ1 %>% 
  filter(!is.na(Leaf_date)) %>% 
  group_by(Species, Site, Water_potential, Replicate, Days_to_leaf) %>% 
  mutate(Seeds_leaf = n()) %>% 
  mutate(Leaf_percent = Seeds_leaf/Seeds_in_dish * 100) %>% 
  select(Species, Site, Water_potential, Replicate, Petri_dish, Days_to_leaf, Seeds_leaf, Leaf_percent) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(Petri_dish) %>% 
  arrange(Days_to_leaf) %>% 
  mutate(Cum_leaf_percent = cumsum(Leaf_percent))

## Sibbaldia procumbens ##

#Mistakes in Sibbaldia procumbens data
SP_mistakes <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
  "old new
  02.02.2020 02.03.2020
  09.04.2020 09.03.2020") #remove after new data on the 6th of April


SP_germ1 <- SP_germ %>% 
  filter(!Germination_date %in% c("only 10 seeds")) %>% 
  mutate(Germination_date = plyr::mapvalues(Germination_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>% 
  mutate(Cotelydon_date = plyr::mapvalues(Cotelydon_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>%
  mutate(Leaf_date = plyr::mapvalues(Leaf_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>% 
  mutate(Replicate = as.factor(Replicate)) %>% 
  mutate(Start_date = dmy(Start_date)) %>% 
  mutate(Germination_date = dmy(Germination_date)) %>% 
  mutate(Cotelydon_date = dmy(Cotelydon_date)) %>% 
  mutate(Leaf_date = dmy(Leaf_date)) %>% 
  mutate(Petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(Days_to_germination = Germination_date - Start_date,
         Days_to_cotelydon = Cotelydon_date - Start_date,
         Days_to_leaf = Leaf_date - Start_date,
         Days_since_germination = today() - Germination_date,
         Days_since_cotelydon = today() - Cotelydon_date,
         Days_since_leaf = today() - Leaf_date) %>% 
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
  mutate(Cum_germ_percent = cumsum(Germination_percent)) %>% 
  ungroup() %>% 
  group_by(Species, Site, Water_potential, Days_to_germination) %>% 
  mutate(Average = mean(Cum_germ_percent)) %>% 
  ungroup() %>% 
  mutate(Days_to_germination = as.numeric(Days_to_germination))

Last_day_germination_score_SP <- SP_germ_analysis %>% 
  group_by(Petri_dish) %>% 
  mutate(Max_germination = max(Cum_germ_percent)) %>% 
  select(Max_germination, Petri_dish, Species, Site, Water_potential, Replicate) %>% 
  mutate(Days_to_germination = 76)

SP_germ_analysis <- SP_germ_analysis %>% 
  bind_rows(Last_day_germination_score_SP) %>% 
  mutate(Cum_germ_percent = ifelse(is.na(Cum_germ_percent), Max_germination, Cum_germ_percent))


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
  "10" = "WP 10 (-1.70 MPa)"
)

## Function for plots ##

Plot_germination_graph <- function(data, site, species){
  
  data <- data %>% 
    filter(Site == site)
  
  plot <- ggplot(aes(x = sort(Days_to_germination), y = Cum_germ_percent), data = data) +
    geom_point(aes(color = Replicate, alpha = 0.01)) +
    geom_line(aes(color = Replicate, alpha = 0.01)) +
    stat_smooth(method = loess, formula = x~y^2, color = "black", size = 3) +
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
    scale_x_continuous(limits = c(0,80)) +
    scale_y_continuous(limits = c(0,100))
  
  return(plot)
}

Plot_germination_mean_graph <- function(data, site, species){
  
  data <- data %>% 
    filter(Site == site)
  
  plot <- ggplot(aes(x = sort(as.integer(Days_to_germination)), y = Average), data = data) +
    geom_line() +
    geom_point(Cum_germ_percent, alpha = 0.1) +
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
    scale_x_continuous(limits = c(0,45)) +
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
    scale_x_continuous(limits = c(0,45)) +
    scale_y_continuous(limits = c(0,100))
  
  return(plot)
}

Plot_leaf_graph <- function(data, site, species){
  
  data <- data %>% 
    filter(Site == site)
  
  plot <- ggplot(aes(x = sort(as.integer(Days_to_leaf)), y = Cum_leaf_percent, color = Replicate), data = data) +
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
    scale_x_continuous(limits = c(0,45)) +
    scale_y_continuous(limits = c(0,100))
  
  return(plot)
}


## Plot data ##

# VA Germination plot #

Plot_germination_graph(data = VA_germ_analysis, site = "SKJ", species = "Veronica alpina")
#ggsave("VA_SKJ.png", width = 15, height = 10, unit = "cm")

Plot_germination_graph(data = VA_germ_analysis, site = "GUD", species = "Veronica alpina")
#ggsave("VA_GUD.png", width = 15, height = 10, unit = "cm")

Plot_germination_graph(data = VA_germ_analysis, site = "LAV", species = "Veronica alpina")
#ggsave("VA_LAV.png", width = 15, height = 13, unit = "cm")

Plot_germination_graph(data = VA_germ_analysis, site = "ULV", species = "Veronica alpina")
#ggsave("VA_ULV.png", width = 15, height = 13, unit = "cm")


# VA germination average #

Plot_germination_mean_graph(data = VA_germ_analysis, site = "SKJ", species = "Veronica alpina")
#ggsave("VA_SKJ.png", width = 15, height = 10, unit = "cm")


# VA cotelydon data #

Plot_cotelydon_graph(data = VA_cotelydon_analysis, site = "SKJ", species = "Veronica alpina")

Plot_cotelydon_graph(data = VA_cotelydon_analysis, site = "GUD", species = "Veronica alpina")

Plot_cotelydon_graph(data = VA_cotelydon_analysis, site = "LAV", species = "Veronica alpina")

Plot_cotelydon_graph(data = VA_cotelydon_analysis, site = "ULV", species = "Veronica alpina")


#VA Leaf plot #

Plot_leaf_graph(data = VA_leaf_analysis, site = "SKJ", species = "Veronica alpina")

Plot_leaf_graph(data = VA_leaf_analysis, site = "GUD", species = "Veronica alpina")

Plot_leaf_graph(data = VA_leaf_analysis, site = "LAV", species = "Veronica alpina")

Plot_leaf_graph(data = VA_leaf_analysis, site = "ULV", species = "Veronica alpina")



## Sibbaldia procumens ##

# SP germination plot #

Plot_germination_graph(data = SP_germ_analysis, site = "SKJ", species = "Sibbaldia procumbens")
#ggsave("SP_SKJ.png", width = 15, height = 13, unit = "cm")

Plot_germination_graph(data = SP_germ_analysis, site = "GUD", species = "Sibbaldia procumbens")
#ggsave("SP_GUD.png", width = 15, height = 7, unit = "cm")

Plot_germination_graph(data = SP_germ_analysis, site = "LAV", species = "Sibbaldia procumbens")
#ggsave("SP_LAV.png", width = 15, height = 13, unit = "cm")

Plot_germination_graph(data = SP_germ_analysis, site = "ULV", species = "Sibbaldia procumbens")
#ggsave("SP_ULV.png", width = 15, height = 10, unit = "cm")


# SP cotelydon plot #

Plot_cotelydon_graph(data = SP_cotelydon_analysis, site = "LAV", species = "Sibbaldia procumbens")

# SP leaf plot #




## SP biomass plots ##

Seedling_info_SP <- SP_germ1 %>%
  mutate(AGB_mass_g = Wet_mass_g_rest + Wet_mass_g_True.leaf,
         Total.wet.mass = Wet_mass_g_root + Wet_mass_g_rest + Wet_mass_g_True.leaf,
         rs_ratio = Wet_mass_g_root/AGB_mass_g) %>% 
  filter(!rs_ratio > 2,
         !Total.wet.mass >0.010)

#Mass total plot

ggplot(aes(x = Water_potential, y = Total.wet.mass), data = Seedling_info_SP) +
  geom_violin(aes(fill = Water_potential)) +
  geom_jitter(alpha = 0.3) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  xlab("Water potential") +
  ylab("Total wet mass (g)") +
  theme(text = element_text(size=20),
        legend.position = "none")

ggplot(aes(x = Water_potential, y = rs_ratio), data = Seedling_info_SP) +
  geom_violin(aes(fill = Water_potential)) +
  geom_jitter(alpha = 0.3) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  xlab("Water potential") +
  ylab("Root/shoot ratio") +
  theme(text = element_text(size=20),
        legend.position = "none")


# Germination percent plots

VA_germ_analysis %>% 
  mutate(Site = factor(Site, levels = c("SKJ", "GUD", "LAV", "ULV"))) %>% 
ggplot(aes(x = Water_potential, y = Max_germination)) +
  geom_boxplot() +
  facet_wrap(~Site, nrow = 4) + 
  xlab("Max germination percent (%)") +
  ylab("Water potential") +
  theme(text = element_text(size=20, family = "Century Schoolbook"))

SP_germ_analysis %>% 
  filter(Site == "LAV") %>% 
ggplot(aes(x = Water_potential, y = Max_germination)) +
  geom_boxplot() +
  xlab("Max germination percent (%)") +
  ylab("Water potential") +
  theme(text = element_text(size=20))


#Richards suggestion to plot, but this does not account for the fact that they have different levels of 100 % gemrination. Some seeds have not germinated yet, although everything goes to 100 %.

# VA_germ_analysis %>% 
#   ggplot(aes(as.numeric(Days_to_germination), color = Water_potential)) +
#   stat_ecdf() +
#   facet_wrap(~ Site)


#Plots and info used to decide when to sample germinants

SP_germ1 %>% 
  filter(Harvest_date == "") %>% 
  filter(!is.na(Germination_date)) %>% 
  filter(Days_since_germination > 0) %>% 
 ggplot(aes(x = Water_potential, y = Days_since_germination)) +
   geom_boxplot()
 
 ggplot(aes(x = Water_potential, y = Days_since_leaf), data = SP_germ1) +
   geom_boxplot()
 
 VA_germ1 %>% 
   filter(Harvest_date == "") %>% 
   filter(!is.na(Germination_date)) %>% 
   #filter(Days_since_leaf < 100) %>% 
   ggplot(aes(x = Water_potential, y = Days_since_cotelydon)) +
   geom_violin() +
   geom_jitter( alpha = 0.1)
 
 ggplot(aes(x = Water_potential, y = Days_since_leaf), data = SP_germ1) +
   geom_violin() +
   geom_jitter(alpha = 0.1)
 
 VA_leaf <- VA_germ1 %>% 
   filter(!is.na(Leaf_date)) %>% 
   ungroup() %>% 
   mutate(Days_since_leaf = as.numeric(Days_since_leaf)) %>% 
   group_by(Site, Water_potential, Days_since_leaf) %>%
   mutate(n = n()) %>%
   select(Site, Water_potential, Days_since_leaf, n) %>%
   distinct() %>% 
   spread(Days_since_leaf, n)
# 
# write_csv(VA_leaf, path = "VA_leaf_summary.csv")
# 
# 
# SP_germ1 %>% 
#   filter(!is.na(Leaf_date)) %>% 
#   ungroup() %>% 
#   count()
# 
# SP_leaf <- SP_germ1 %>% 
#   filter(!is.na(Leaf_date)) %>% 
#   ungroup() %>% 
#   mutate(Days_since_leaf = as.numeric(Days_since_leaf)) %>% 
#   group_by(Site, Water_potential, Replicate, Days_since_leaf) %>%
#   mutate(n = n()) %>%
#   filter(!Wet_mass_g_root == "") %>% 
#   select(Site, Water_potential, Replicate, Days_since_leaf, n) %>%
#   distinct() %>% 
#   spread(Days_since_leaf, n)
# 
# write_csv(SP_leaf, path = "SP_leaf_summary_with_traitleaves.csv")
