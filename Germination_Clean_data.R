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

## Deal with comments ##
## Entering information in the Flag column. Options are: Remove_duplicate, Dead_plant, Sick_plant, Other, Missing_date, Possible_mistakes_in_ID
VA_germ_test <- VA_germ1 %>% 
  mutate(Flag = ifelse(Flag %in% c("Duplicate_remove", "Remove_duplicate", "Remove_Duplicate", "Remove_duplicat", "Remove_unsureID", "	No germination date. 2 seeds.; seed#2 germinated with cotyledons 23.03.2020'", "Might have two versions of this, becasue this one was not crossed out (18.05.2020) Agar dried out, crossed out dish 07.05.2020"),  "Remove_duplicate",
                       ifelse(Flag == "No", NA,
                              ifelse(Flag == "Remove_rotten", "Dead_plant", Flag)))) %>% 
  mutate(Flag = ifelse(Comment %in% c("yellowing 09.04.2020", "yellowing 09.04", "yellow", "Very moldy, embryo green with cut-test", "Very moldy, but allive (cut-test)", "seems mouldy at root 17.03.2020", "seems dyin 17.03.2020"), "Sick_plant",
                       ifelse(Comment %in% c("Was yellow and dead, but also disturbed during harvest of another seedling", "Was not there on th 09.04.2020 (Seems dead on harvest day 07.04.2020)", "VERY MOLDY PLATE-17.03.2020", "Too mushy to harvest", "This whole plate is dying", "Seems to be dead", "Seems dead,re did the parafilm, 06.04.2020", "Seems dead, re did the parafilm, 06.04.2020", "Seems dead on hravest day 07.04.2020, wasnot harvested", "Seems dead", "Seed blackened 07.05.2020", "rotted seed", "Reposition due to loose agar; Seems to be dead", "moulded away 17.03.2020", "Molded", "Modly seed", "Looks dead, very yellow and lying down, did not harvest on the 07.04.2020", "Looks dead"), "Dead_plant",
                              ifelse(Comment %in% c("Two seeds?", "Two seeds at same place", "Two seedlings in the same place", "Two seedlings in this spot: there was not really any true leaves here, only two seedling with cotelydons each. That was discovered when pulling them out for harvest, so they were removed. Taking out the true leaf date, but the germintion and cotelydon date can still be used."), "Remove_duplicate",
                                     ifelse(Comment %in% c("Was scored as dead on the 30.03.2020, but was found with small pair of true leaves later", "Was pulled out with harvesting the one next to it", "Was pulled halfway out during removing another seedling", "Three cotelydons", "SG Measured in arvo, but some already had values from morning...", "SG checked this dish in arvo, but it already had values.....", "Seed at the bottom of the row", "Rooted on the 14.05.2020", "Reposition due to loose agar", "replaced parafilm 06.04.2020", "reparafilmed 09.04, some crystalization", "reparafilm, 6.4.20", "reparafilm 06.04.2020; this is not ready to harvest, but 13 is", "reparafilm 06.04.2020", "re did the parafilm, 06.04.2020", "needs parafilm 06.04.2020", "Missing seed", "Missing", "Might have two versions of this, becasue this one was not crossed out (18.05.2020) Agar dried out, crossed out dish 07.05.2020", "meant to be harvested 20.04 but no plant?", "lying on agar 09.04", "lying on agar", "Leaves but no root on the 27.02.2020"), "Other", 
                                            ifelse(Comment %in% c("Was scored for true leaf from before, put the last scoring date as the leaf date.", "Was crossed out as leafing out, but with no date, put todays date 16.04.2020", "Was crossed out with true leaves, but no date, put the last scoring date in.", "Was crossed out with cotelydons, but did not have a date on the 29.02.2020, so it happened before that. I just put in the 28.02.2020, because it defenitaly happened before that", "Was originally swtiched around with 18, might not be all correct with the dates and order of things", "Was originally swtiched around with 17, might not be all correct with the dates and order of things", "Was originally swtiched around with 14, might not be all correct with the dates and order of things", "Was originally swtiched around with 10, might not be all correct with the dates and order of things", "Was crossed out as having cotelydons but not marked in on the 29.02.2020. so at the latest it had cotelydons on the 28.02.2020", "the germination date isnt right", "Should have had an earlier date on the leaf out because it was very highly developed by then", "Should have had a leaf date that was earlier, long stem with true leaves on on the 09.04.2020", "Should have had a leaf date that was earlier, long stem with true leaves on on the 09.04.2020", "should have a germination date. harvested on the 07.07 and it had a circle", "ring from before 17.03.2020", "	ring and slash from before 17.03.2020", "Only one cotelydon - have probably been fully expanded before this date, hard to see with only one cotelydon", "On the 4th we found the cotelydons but non of the other seeds had germinated, Noted wrong place?", "not germinated 20.03.2020", "No true leaf here, date was set to 23.03.2020", "no germination record", "No germination date.", "no germination date though?", "No germination date", "No date on germination?", "No cotelydon date", "Must have had cotelydons before this since there were the pair of true leaves as well"), "Missing_wrong_date", 
                                                   ifelse(Comment %in% c("VA_SKJ_5_4 looked a lot like VA_ULV (because it had been labelled wrongly and the SKJ had almost dissaperead) on the 06.04.2020, might have been done wrong before this.", "This is gone. Possibley confused with rep 5", "Root was intertwingled with number 6, impossible to tell the difference between the two - was not harvested"), "Possible_mistakes_in_ID", 
                                                          ifelse(Comment %in% c("part of root stuck to paper it was harvested on", "no true leaves at harvest"), "Biomass_mistakes",Flag)))))))) %>% 
  mutate(Seed_viable = ifelse(Comment %in% c("Very moldy, embryo green with cut-test", "Very moldy, but allive (cut-test)"), "Yes", Seed_viable))
         
           
         #This is the last Comment I delt with: Leaves but no root on the 27.02.2020
         #Full plate info: Comment %in% c("VERY MOLDY PLATE-17.03.2020", "This whole plate is dying", "This dish contains 21 seeds. 2 seeds on nr 17.", "Only 10 seeds")
  

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
