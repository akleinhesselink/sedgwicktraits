rm(list = ls())
library(tidyverse)
library(sedgwickspecies)

outfile <- 'data-raw/clean_canopy.R'

alias <- read_csv('data-raw/alias.csv')
canopy <- read_csv('data-raw/canopy_dimensions.csv')
agb <- read_csv('data-raw/aboveground_biomass_weights.csv')

leaf_area <- read_csv('data-raw/leaf_area.csv')

leaf_area <- 
  leaf_area %>% 
  filter( plot == 'non_plot') %>% 
  select( slice, USDA_symbol, plant_number, leaf_number, total_area, scan_date, notes) %>%
  mutate( leaf_number = ifelse(is.na(leaf_number), toupper(str_extract(slice, '(all)|(ALL)')), leaf_number)) %>% 
  mutate( petiole_type = str_extract(notes, '.*petiole')) %>% 
  mutate( tissue_type = NA) %>% 
  mutate( tissue_type = ifelse(str_detect(slice, 'bracts'), 'bracts', 'leaves')) %>% 
  group_by( USDA_symbol, plant_number, petiole_type, scan_date) %>% 
  summarise( leaf_area = sum(total_area))

agb <- 
  agb %>%
  filter( plot == 'non_plot') %>% 
  select( species, plant_number, type, tissue_type, aboveground_biomass_g, date_collected, notes) %>% 
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(-alias)

agb <- 
  agb %>% 
  group_by(USDA_symbol, plant_number, tissue_type) %>% 
  mutate( repeats = n())

canopy <- 
  canopy %>%  
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(-alias)

canopy %>% 
  ggplot( aes( x = USDA_symbol, y = height )) + geom_point()

canopy %>% 
  ggplot( aes( x = USDA_symbol, y = width )) + geom_point()

canopy %>% 
  ggplot( aes( x = USDA_symbol, y = length )) + geom_point()

canopy %>% 
  mutate( area = width*height ) %>% 
  ggplot(aes( x = USDA_symbol, y = area, label = plant_number)) + 
  geom_label()

agb <- 
  agb %>% 
  rename('mass_g' = aboveground_biomass_g) %>% 
  group_by( USDA_symbol, type, plant_number, tissue_type) %>% 
  summarise( mass_g = sum(mass_g) )

agb <- 
  agb %>% 
  mutate( tissue_type = ifelse( is.na(tissue_type), 'unclassified', tissue_type)) %>% 
  spread(tissue_type, mass_g, fill = 0) %>% 
  mutate( total = leaves + stem + unclassified)

canopy_stats <- 
  canopy %>% 
  ungroup() %>% 
  right_join(agb %>% ungroup, by = c('USDA_symbol', 'plant_number'))

canopy_stats <- 
  canopy_stats %>% 
  left_join(leaf_area, by = c('USDA_symbol', 'plant_number'))

canopy_stats <- 
  canopy_stats %>% 
  filter( is.na(petiole_type) | petiole_type == 'no_petiole')

agb %>% filter( USDA_symbol == 'CHGL')

canopy_stats %>% filter( USDA_symbol == 'CHGL')


