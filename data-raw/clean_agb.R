rm(list = ls())
library(tidyverse)
library(lubridate)

alias <- read_csv('data-raw/alias.csv')
agb <- read_csv('data-raw/raw_trait_data/aboveground_biomass_weights.csv') 

leaf_weights_2017 <- read_csv('data-raw/raw_trait_data/2017-trait-measurements.csv')
leaf_weights_2019 <- read_csv('data-raw/raw_trait_data/2019-trait-measurements.csv')

# -------------------------------------- # 

leaf_weights_2017$date_collected
leaf_weights_2019$date_collected

leaf_weights_2017 %>% 
  bind_rows(leaf_weights_2019) %>%  
  mutate( date_collected = ymd( date_collected)) %>% 
  rename( 'alias' = species ) %>% 
  left_join(alias, by = 'alias' ) %>% 
  select( sequence, date_collected, plot, USDA_symbol, plant_number:n_leaves, notes1:notes4) %>% View



leaf_




agb %>% 
  mutate( date_collected = mdy(date_collected)) %>% 
  rename( 'alias' = species ) %>% 
  left_join(alias , by = 'alias') %>% 
  distinct() %>% 
  filter( plot == 'non_plot' ) %>% 
  select( plot,  USDA_symbol, plant_number, tissue_type, aboveground_biomass_g, year, date_collected, notes ) %>% 
  rename( 'mass_g' = aboveground_biomass_g) %>% 
  mutate( tissue_type = ifelse( is.na(tissue_type), 'unclassified', tissue_type)) %>% 
  group_by( plot, USDA_symbol, plant_number, tissue_type, year, date_collected ) %>% 
  summarise( mass_g = sum(mass_g)) %>% 
  spread( tissue_type, mass_g , fill = 0 ) %>% 
  mutate( all_agb = leaves + petiole + stem + unclassified ) %>% 
  arrange( USDA_symbol, year, date_collected, plant_number ) %>% View






