library(tidyverse)
library(sedgwicktraits)
library(sedgwickspecies)

sedgwick_annuals <- 
  sedgwicktraits %>% 
  left_join(sedgwick_plants) %>% 
  filter( life_history == 'Annual')

sedgwick_annuals %>% 
  filter( !is.na(phenology)) %>% 
  ggplot(aes( x= log(seed_mass), y = phenology )) + geom_point()

sedgwick_annuals %>% 
  filter( !is.na(phenology)) %>% 
  ggplot(aes( x= max_height, y = phenology )) + geom_point()

sedgwick_annuals %>% 
  filter( !is.na(phenology)) %>% 
  ggplot(aes( x= phenology, y = turgor_loss_point)) + 
  geom_point() + 
  geom_text(aes( label = USDA_symbol), size = 2, nudge_x = 2, nudge_y = 0) 

sedgwick_annuals %>% 
  filter( !is.na(phenology)) %>% 
  ggplot(aes( x= CN_ratio, y = phenology, size = turgor_loss_point )) + 
  geom_point() + 
  geom_text(aes( label = USDA_symbol), size = 2, nudge_x = 2, nudge_y = 2)



sedgwicktraits %>% 
  left_join(sedgwick_plants) %>% 
  filter( life_history == 'Annual') %>% 
  filter( !is.na(phenology) ) %>% 
  ggplot(aes( x= seed_mass, y = phenology )) + geom_point()
