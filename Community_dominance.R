#### Making a list of abundance of species at each site ####

## Loading library 
library(tidyverse)

## Loading data
community_2018 <- read.csv2("INCLINE_community_2018_full.csv")


## Cleaning and gathering dataset, and grouping and adding up sum of covers
community <- community_2018 %>% 
  filter(Measure == "cover") %>% 
  select(-subPlot, -Treatment, -year, -date, -recorder, -writer, -Weather, -X, -X.1, -Unknown, -Nid.seedling, -moss, -lichen, -litter, -soil, -rock, -poo, -bare, -X.2, -Veg_cover, -Veg_height_mm, -Moss_depth_mm, -X.3, -Block, -plot) %>% 
  gather(species, cover, Ach_mil:Vio_riv) %>% 
  unique() %>%
  select(-Measure) %>% 
  mutate(cover = as.numeric(cover)) %>% 
  group_by(Site, species) %>% 
  mutate(sum_cover = sum(cover, na.rm=TRUE)) %>% 
  select(-cover) %>% 
  unique()

## Making a file with this information in it

write.csv(community, file = "Dominance_2018.csv")
