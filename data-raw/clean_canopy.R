rm(list = ls())
library(tidyverse)
library(sedgwickspecies)
library(stringr)

outfile <- 'data-raw/cleaned_trait_data/clean_canopy.csv'

alias <- read_csv('data-raw/alias.csv')
canopy <- read_csv('data-raw/raw_trait_data/canopy_dimensions.csv')
agb <- read_csv('data-raw/raw_trait_data/aboveground_biomass_weights.csv')
leaf_traits <- read_csv('data-raw/cleaned_trait_data/clean_leaf_traits.csv')

leaf_area <- read_csv('data-raw/raw_trait_data/leaf_area.csv')

avg_SLA <- 
  leaf_traits %>% 
  filter( plot == 'non_plot') %>% 
  filter( !censor) %>% 
  group_by( USDA_symbol, petiole) %>% 
  summarise( m_SLA = mean(SLA) ) 

leaf_area <-
  leaf_area %>% 
  filter( plot == 'non_plot') %>% 
  filter( total_area > 0) %>% 
  select( slice, USDA_symbol, plant_number, leaf_number, total_area, scan_date, petiole, notes) %>%
  mutate( leaf_number = ifelse(is.na(leaf_number), toupper(str_extract(slice, '(all)|(ALL)')), leaf_number)) 

leaf_area <- 
  leaf_area %>% 
  mutate( tissue_type = NA) %>% 
  mutate( tissue_type = ifelse(str_detect(slice, 'bracts'), 'bracts', 'leaves')) %>%
  group_by( USDA_symbol, plant_number, leaf_number, scan_date, tissue_type, petiole) %>% 
  mutate( id = row_number())  %>% 
  select(-slice) %>% 
  spread( leaf_number, total_area, fill = 0) %>% 
  mutate( complete = ALL != 0 ) %>% 
  mutate( total  = `1` + `2` + `3` + `ALL`) %>% 
  group_by( USDA_symbol, plant_number, petiole, scan_date) %>% 
  summarise( canopy_LA = sum(total), complete = all(complete) ) %>% 
  filter( !petiole | (petiole & USDA_symbol %in% c('LUBI', 'LEBI4', 'VIPE3', 'GIOC')) )

# read in areas without petiole 
no_petiole <- read_csv('data-raw/raw_trait_data/canopy_LA_petiole_deleted.csv')

no_petiole_species <- unique( no_petiole $USDA_symbol )

leaf_area <- 
  leaf_area %>% 
  filter( !(USDA_symbol %in% no_petiole_species ))  %>%
  bind_rows(no_petiole)

# drop LUBI leaf area without petiole --------------- # 
leaf_area <- 
  leaf_area %>% 
  filter( !(USDA_symbol == 'LUBI'  & !petiole) )
#------------------------------------------------------ #

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

canopy <- 
  canopy %>%  
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(-alias)

canopy_stats <- 
  canopy %>% 
  ungroup() %>% 
  right_join(agb %>% ungroup, by = c('USDA_symbol', 'plant_number'))

canopy_stats <- 
  canopy_stats %>% 
  left_join(leaf_area, by = c('USDA_symbol', 'plant_number'))

canopy_stats <- 
  canopy_stats %>% 
  left_join(avg_SLA, by = c('USDA_symbol', 'petiole')) %>% 
  select(USDA_symbol, plant_number, petiole, height:length, canopy_LA, complete, leaves, stem, unclassified, total, m_SLA) %>% 
  mutate( canopy_LA_by_weight = m_SLA*leaves) %>%
  mutate( canopy_LA  = ifelse(complete, canopy_LA, NA)) %>% 
  mutate( canopy_LA_by_weight = ifelse( canopy_LA_by_weight == 0, NA, canopy_LA_by_weight))

canopy_stats <- 
  canopy_stats %>% 
  mutate( projected_area = pi*(1/2*width*1/2*length) ) %>% 
  mutate( relative_spread = ((width + length)/2)/height ) %>% 
  rename( 'total_agb_g' = total) %>% 
  mutate( total_LA = ifelse( is.na(canopy_LA), canopy_LA_by_weight, canopy_LA)) %>% 
  mutate( LAI = total_LA/projected_area, 
          LAR = total_LA/total_agb_g ) %>% 
  select( USDA_symbol, plant_number, petiole, height, LAI, LAR, relative_spread, projected_area)

canopy_stats %>% 
  gather( metric, value, height:projected_area) %>% 
  ggplot( aes( x = USDA_symbol, y = value )) + 
  geom_boxplot() + 
  geom_point(alpha = 0.3) + 
  facet_wrap( ~ metric, scales = 'free') + 
  coord_flip()

mean_canopy_traits <- 
  canopy_stats %>% 
  gather( metric, value, height:projected_area) %>% 
  group_by( USDA_symbol, petiole, metric) %>% 
  summarise( value = mean(value, na.rm = T)) %>% 
  spread( metric, value ) 

mean_canopy_traits %>% 
  arrange(!is.na(relative_spread), desc(petiole)) %>% 
  filter( petiole | is.na(LAI) | is.na(LAR) | is.na(projected_area) | is.na(relative_spread) ) %>% 
  write_csv( 'temp/check_canopy.csv')

write_csv(mean_canopy_traits, outfile)