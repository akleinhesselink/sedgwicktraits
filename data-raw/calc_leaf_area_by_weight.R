rm(list = ls())
library(tidyverse)
library(sedgwickspecies)
library(stringr)
library(lubridate)

alias <- read_csv('data-raw/alias.csv')

agb <- read_csv('data-raw/raw_trait_data/aboveground_biomass_weights.csv') %>% 
  filter(  plot == 'non_plot')  %>% 
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(-alias) 

leaf_traits <- 
  read_csv('data-raw/cleaned_trait_data/clean_leaf_traits.csv') %>% 
  mutate( year = year( date )) 

leaf_traits %>% filter( USDA_symbol == 'ERGR5') %>% View()

canopy_area1 <- readRDS('temp/direct_canopy_leaf_area.RDS')
#------------------------------------------------------ #

avg_SLA <- 
  leaf_traits %>% 
  filter( plot == 'non_plot') %>% 
  filter( !censor) %>% 
  group_by( USDA_symbol, petiole, year ) %>% 
  summarise( m_SLA = mean(SLA, na.rm = T) ) 

avg_SLA %>% View()

agb <- 
  agb %>%
  filter( plot == 'non_plot') %>% 
  select( species, plant_number, type, tissue_type, aboveground_biomass_g, year, date_collected, notes) %>% 
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(-alias) %>% 
  group_by(USDA_symbol, plant_number, tissue_type) %>% 
  rename('mass_g' = aboveground_biomass_g) %>% 
  group_by(year, USDA_symbol, type, plant_number, tissue_type) %>% 
  summarise( mass_g = sum(mass_g) ) %>% 
  mutate( tissue_type = ifelse( is.na(tissue_type), 'unclassified', tissue_type)) %>% 
  spread(tissue_type, mass_g, fill = 0) %>% 
  mutate( total = leaves + stem + unclassified) %>% 
  ungroup() %>% 
  left_join(avg_SLA) %>% 
  mutate( leaf_area_by_weight = leaves*m_SLA)



canopy_area1 %>% 
  left_join(agb, by = c('year', 'USDA_symbol', 'plant_number')) %>% 
  select( year, USDA_symbol, plant_number, canopy_leaf_area,  leaf_area_by_weight) %>% View()


