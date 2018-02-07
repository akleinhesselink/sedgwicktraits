rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/clean_seed_mass.csv'
alias <- read_csv('data-raw/alias.csv')
old_mass <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
new_mass <- read_csv('data-raw/seed_weights.csv')
molignari <- read_csv('data-raw/molignari_traits_clean.csv')

old_mass <- 
  old_mass %>% 
  select( species, `seed_mass(g)` ) %>% 
  rename( 'seed_mass' = `seed_mass(g)`, 'alias' = species) %>%
  left_join( alias ) %>% 
  select( USDA_symbol, seed_mass) %>% 
  mutate( dataset = 'tapioca') %>% 
  distinct()

new_mass <- 
  new_mass %>% 
  mutate( seed_mass = dry_weight/seed_n) %>% 
  group_by(species) %>% 
  summarise( seed_mass = mean(seed_mass)) %>% 
  rename( 'alias' = species) %>% 
  left_join( alias)  %>%
  select(USDA_symbol, seed_mass) %>% 
  mutate( dataset = '2017')


molignari <- 
  molignari %>% 
  select( USDA_symbol, seed_mass) %>% 
  mutate( dataset = 'molignari') %>% 
  filter( complete.cases(.)) 


write_csv(rbind(old_mass, new_mass, molignari), outfile)  

