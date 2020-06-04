###### NDVI ######

library(tidyverse)

NDVI <-read.csv2("Data/NDVI_2019.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

NDVI <- NDVI %>% 
  mutate(NDVI = ave(NDVI_1, NDVI_2, NDVI_3)) %>% 
  mutate(NDVI = round(NDVI, digits = 2)) %>% 
  filter(!is.na(NDVI)) %>% 
  filter(Treatment %in% c("W_C", "C_C"))

NDVI %>% 
  ggplot(aes(x = OTC, y = NDVI, fill = OTC))+
  geom_boxplot()+
  scale_fill_manual(values = c("royalblue", "firebrick"),
                    labels = c("Cold", "Warm"))
  #+
  #facet_grid(. ~ Site)
  



