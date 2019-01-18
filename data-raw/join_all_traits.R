rm(list = ls())

library(tidyverse)
library(ggplot2)

outfile <- 'data-raw/all_2017_traits.csv'

canopy <- read_csv('data-raw/clean_canopy.csv')
isotopes <- read_csv('data-raw/clean_isotopes.csv')
heights <- read_csv('data-raw/clean_heights.csv')
pheno <- read_csv('data-raw/clean_phenology.csv')
seed_mass <- read_csv('data-raw/clean_seed_mass.csv')
srl <- read_csv('data-raw/clean_SRL.csv')
leaf_traits <- read_csv('data-raw/clean_leaf_traits.csv')

leaf_traits <- 
  leaf_traits %>% 
  filter( plot == 'non_plot' | USDA_symbol %in% c('BRMA3', 'HOMU', 'FEMI2', 'MICA', 'CHGL')) %>% 
  filter( !censor) %>% 
  group_by( USDA_symbol, petiole ) %>% 
  summarise( SLA = mean(SLA, na.rm = T), 
             LDMC = mean(LDMC, na.rm = T), 
             LA = mean(LA, na.rm = T), 
             dry_mass_g = mean(dry_mass_g, na.rm = T))

leaf_traits <- 
  leaf_traits %>% 
  group_by( USDA_symbol ) %>% 
  arrange( USDA_symbol, rev( petiole )) %>% 
  filter( row_number() == 1 ) 

srl <- 
  srl %>% 
  filter( plot == 'non_plot' | USDA_symbol %in% c('BRMA3', 'HOMU', 'FEMI2', 'MICA', 'CHGL')) %>% 
  group_by( USDA_symbol ) %>% 
  summarise( `SRL (m/g)` = mean(`SRL (m/g)`, na.rm = T))

seed_mass <- 
  seed_mass %>% 
  group_by( USDA_symbol ) %>% 
  arrange( USDA_symbol, seed_mass_data_source) %>% 
  filter( row_number() == 1) %>% 
  select(USDA_symbol, seed_mass, seed_mass_data_source)
  
pheno <- 
  pheno %>% 
  group_by(USDA_symbol) %>% 
  arrange( USDA_symbol, dataset) %>% 
  filter( row_number() == 1) %>% 
  select( USDA_symbol, `phenology (DOY 50% fruit)`)

heights <- heights[ complete.cases(heights), ] 

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
          'SRL(m/g)' = `SRL (m/g)`, 
          'relative_spread(lateral/height)' = relative_spread) 


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
          seed_mass_data_source, 
          max_height_data_source) %>% 
  write_csv(path = outfile )


