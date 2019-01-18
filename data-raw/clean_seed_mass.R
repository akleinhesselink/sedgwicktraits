rm(list = ls())
library(tidyverse)
library(stringr)
library(BIEN)

outfile <- 'data-raw/cleaned_trait_data/clean_seed_mass.csv'
alias <- read_csv('data-raw/alias.csv')
old_mass <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
new_mass <- read_csv('data-raw/raw_trait_data/seed_weights.csv')
new_focal_mass <- read_csv('data-raw/raw_trait_data/new_seed_weights_2018.csv')
molinari <- read_csv('data-raw/old-data/molinari.csv')

old_mass <- 
  old_mass %>% 
  select( species, `seed_mass(g)` ) %>% 
  rename( 'seed_mass' = `seed_mass(g)`, 'alias' = species) %>%
  left_join( alias ) %>% 
  select( USDA_symbol, seed_mass) %>% 
  mutate( seed_mass_data_source = 'TAPIOCA') %>% 
  distinct()

new_mass <- 
  new_mass %>% 
  mutate( seed_mass = dry_weight/seed_n) %>% 
  group_by(species) %>% 
  summarise( seed_mass = mean(seed_mass)) %>% 
  rename( 'alias' = species) %>% 
  left_join( alias)  %>%
  select(USDA_symbol, seed_mass) %>% 
  mutate( seed_mass_data_source = '2017')

new_focal_mass <- 
  new_focal_mass %>% 
  rename('seed_mass_data_source' = year, 'seed_mass' = dry_weight_total) %>%  
  select(USDA_symbol, seed_mass, seed_mass_data_source)

molinari <- 
  molinari %>% 
  select( USDA_symbol, seed_mass) %>% 
  mutate( seed_mass_data_source = 'MOLINARI') %>% 
  filter( complete.cases(.)) 

avba_seed <- 
  BIEN::BIEN_trait_traitbyspecies(species = 'Avena barbata', trait = 'seed mass') %>% 
  distinct(scrubbed_species_binomial, trait_name, trait_value, unit) %>%
  group_by(scrubbed_species_binomial, trait_name, unit) %>% 
  summarise( seed_mass = mean(as.numeric(trait_value))/1000) %>% 
  mutate( USDA_symbol = 'AVBA', seed_mass_data_source = 'BIEN') %>% 
  ungroup() %>% 
  select(USDA_symbol, seed_mass, seed_mass_data_source)

CACO35_seed <- 
  BIEN::BIEN_trait_traitbygenus(genus = 'Calystegia', trait = 'seed mass') %>% 
  distinct(scrubbed_species_binomial, trait_name, trait_value, unit) %>% 
  mutate( genus = 'Calystegia' ) %>% 
  group_by( genus, trait_name, unit) %>% 
  summarise( seed_mass = mean(as.numeric(trait_value))/1000) %>% 
  mutate( USDA_symbol = 'CACO35', seed_mass_data_source = 'BIEN(avg. of 6 Calystegia sp.)') %>% 
  ungroup() %>% 
  select( USDA_symbol, seed_mass, seed_mass_data_source) 

new_mass <- rbind( avba_seed, CACO35_seed, new_mass, new_focal_mass)

write_csv( rbind(old_mass, new_mass, molinari), outfile)


