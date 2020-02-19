rm(list = ls())
library(tidyverse)
library(sedgwickspecies)
library(stringr)
library(lubridate)

outfile <- 'data-raw/cleaned_trait_data/clean_canopy.csv'

alias <- read_csv('data-raw/alias.csv')
canopy <- read_csv('data-raw/raw_trait_data/canopy_dimensions.csv')
mass <- read_csv('data-raw/raw_trait_data/aboveground_biomass_weights.csv') 
area <- read_csv('data-raw/cleaned_trait_data/clean_leaf_area.csv')
traits <- read_csv('data-raw/cleaned_trait_data/clean_leaf_traits.csv') 

heights <- read_csv('data-raw/cleaned_trait_data/clean_heights.csv')

focal_leaf_mass <- 
  traits %>% 
  mutate( tissue_type = 'focal_leaves') %>% 
  group_by(plot, USDA_symbol, date, plant, tissue_type) %>% 
  summarise( mass_g = sum(dry_mass_g))


focal_leaf_area <- 
  traits %>% 
  group_by(plot, USDA_symbol, date, plant ) %>% 
  summarise( leaf_area = sum(total_area)) 

canopy <- 
  canopy %>%  
  left_join(heights, by = c('USDA_symbol')) %>% 
  mutate( height = ifelse( is.na(height), mean_height, height)) %>% 
  mutate( date = lubridate::mdy( date ) ) %>% 
  rename( 'plant' = plant_number) %>% 
  mutate( projected_area = pi*(1/2*width*1/2*length) ) %>% 
  mutate( relative_spread = ((width + length)/2)/height )  %>% 
  select( plot, date, USDA_symbol, plant, height, projected_area, relative_spread) %>% 
  mutate( plant = ifelse( USDA_symbol == 'CAPY2' & plant == 5.1, 9, plant )) %>% 
  mutate( plant = ifelse( USDA_symbol == 'THLA3', plant - 16, plant  )) %>% 
  mutate( plant = ifelse( USDA_symbol == 'THCU', plant - 8, plant )) 


mass <- 
  mass %>%
  mutate( date = lubridate::mdy( date )) %>% 
  select( plot, USDA_symbol, plant_number, tissue_type, mass_g, date, notes) %>% 
  separate(plant_number, c('plant', 'part')) %>%
  mutate_at( vars( 'plant', 'part'), as.numeric) %>% 
  mutate( tissue_type = ifelse( is.na(tissue_type), 'unassigned', tissue_type)) %>% 
  filter( tissue_type != 'dead leaves') %>% 
  mutate( plant = ifelse( USDA_symbol == 'THLA3', plant - 16, plant  )) %>% 
  mutate( plant = ifelse( USDA_symbol == 'THCU', plant - 8, plant )) %>% 
  group_by( plot, USDA_symbol, date, plant, tissue_type ) %>% 
  summarise( mass_g = sum( mass_g)) %>% 
  bind_rows(focal_leaf_mass) %>% 
  spread( tissue_type, mass_g , fill = 0)  %>% 
  rename( 'petiole_mass' = petiole) %>% 
  ungroup() %>% 
  mutate( tissue_separated = ( leaves > 0 | stem > 0 ), T, F) %>% 
  mutate( total_leaf_mass = focal_leaves + leaves, total_agb_mass = focal_leaves + leaves + petiole_mass + stem + unassigned) %>% 
  select( plot, USDA_symbol, date, plant, tissue_separated, total_leaf_mass, total_agb_mass, focal_leaves, leaves, petiole_mass, stem, unassigned )

area <- 
  area %>% 
  group_by( plot, date, USDA_symbol, plant, petiole ) %>% 
  summarise( total_area = sum( total_area ), complete = any(type == 'all')) %>% 
  mutate( complete = ifelse( USDA_symbol == 'DOCL', T, complete))

avg_SLA <- 
  traits %>% 
  group_by( date , plot, USDA_symbol ) %>% 
  summarise( SLA = mean(SLA, na.rm = T) )

canopy_dat <- 
  bind_rows(
  traits %>% distinct( plot, date, USDA_symbol, plant),
  area %>% distinct( plot, date, USDA_symbol, plant ), 
  mass %>% distinct( plot, date, USDA_symbol, plant ), 
  canopy %>% distinct( plot, date, USDA_symbol, plant )) %>% 
  distinct %>% 
  arrange( date, plot, USDA_symbol, plant ) %>% 
  left_join( canopy ) %>% 
  left_join( area ) %>% 
  left_join( mass) %>% 
  left_join(avg_SLA) %>% 
  mutate( total_area_est = SLA*total_leaf_mass ) %>%
  filter( plot %in% c('UCLA', 'non_plot')) %>% 
  arrange( USDA_symbol, date, projected_area) %>% 
  mutate( total_area_est = ifelse( tissue_separated , total_area_est, NA)) 


#------------------------------------------------------ #

canopy_dat <- 
  canopy_dat %>% 
  mutate(total_area = ifelse(!complete, NA, total_area) ) %>% 
  mutate(total_area_est = ifelse( !tissue_separated, NA, total_area_est)) %>% 
  filter( USDA_symbol != 'DICA14') 

write_csv(canopy_dat, outfile)