rm(list = ls())

library(tidyverse)
library(ggplot2)

outfile <- 'data-raw/all_2017_traits.csv'

canopy <- read_csv('data-raw/clean_canopy.csv')
isotopes <- read_csv('data-raw/clean_isotope_data.csv')
heights <- read_csv('data-raw/clean_heights.csv')
pheno <- read_csv('data-raw/clean_phenology.csv')
seed_mass <- read_csv('data-raw/clean_seed_mass.csv')
srl <- read_csv('data-raw/specific_root_length.csv')
leaf_traits <- read_csv('data-raw/leaf_traits.csv')

leaf_traits <- 
  leaf_traits %>% 
  filter( plot == 'non_plot') %>% 
  filter( !censor) %>% 
  group_by( USDA_symbol, petiole ) %>% 
  summarise( SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T), dry_mass_g = mean(dry_mass_g, na.rm = T))

leaf_traits <- 
  leaf_traits %>% 
  group_by( USDA_symbol ) %>% 
  arrange( USDA_symbol, rev( petiole )) %>% 
  filter( row_number() == 1 ) 

srl <- 
  srl %>% 
  filter( plot == 'non_plot') %>% 
  group_by( USDA_symbol ) %>% 
  summarise( SRL = mean(SRL, na.rm = T))

seed_mass <- 
  seed_mass %>% 
  group_by( USDA_symbol ) %>% 
  arrange( USDA_symbol, dataset) %>% 
  filter( row_number() == 1) %>% 
  select(USDA_symbol, seed_mass)
  
pheno <- 
  pheno %>% 
  group_by(USDA_symbol) %>% 
  arrange( USDA_symbol, dataset) %>% 
  filter( row_number() == 1) %>% 
  select( USDA_symbol, `phenology (DOY 50% fruit)`)


all_traits <- 
  leaf_traits %>% 
  left_join(isotopes) %>% 
  left_join(canopy) %>% 
  left_join(heights) %>% 
  left_join(pheno) %>% 
  left_join(srl) %>% 
  left_join(seed_mass)


all_traits <- 
  all_traits %>% 
  mutate( notes = ifelse( petiole, 'LA with petiole', '' )) %>% 
  rename( 'leaf_size(cm2)' = LA,
          'SLA (g/cm2)' = SLA, 
          'LDMC(mg/g)' = LDMC, 
          'LAI (LA/canopy_area)' = LAI, 
          'LAR(cm2/g)' = LAR, 
          'seed_mass(g)' = seed_mass, 
          'max_height(cm)' = max_height, 
          'SRL(m/g)' = SRL, 
          'relative_spread(lateral/height)' = relative_spread, 
          'seed_mass_data_source' = dataset) 


all_traits %>% 
  select( USDA_symbol, 
          `leaf_size(cm2)`, 
          `SLA (g/cm2)`, 
          `LDMC(mg/g)`, 
          `LAI (LA/canopy_area)`, 
          `LAR(cm2/g)`, 
          `seed_mass(g)`, 
          `max_height(cm)`, 
          `SRL(m/g)`, 
          `relative_spread(lateral/height)`,
          `phenology (DOY 50% fruit)`, 
          foliar_N, 
          CN_ratio, 
          d13C, 
          d15N, 
          notes, 
          seed_mass_data_source) %>% 
  write_csv(path = outfile )


