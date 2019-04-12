rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/cleaned_trait_data/clean_heights.csv'
q_height <- 0.95

old_heights <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
new_heights <- read_csv('data-raw/raw_trait_data/2017-plant-heights.csv')
molinari <- read_csv('data-raw/old-data/molinari.csv')
alias <- read_csv('data-raw/alias.csv')

old_heights <- 
  old_heights %>% 
  select( species, `max_height(cm)` ) %>% 
  rename( 'max_height' = `max_height(cm)` , 'alias' = species) %>%
  left_join( alias ) %>% 
  select( USDA_symbol, max_height) %>% 
  distinct() %>% 
  mutate( max_height_data_source = 'TAPIOCA')


new_heights %>% 
  group_by( USDA_symbol ) %>% 
  summarise( n = n() ) %>% 
  arrange( n, USDA_symbol ) %>% 
  mutate( trait = 'max_height') %>% 
  write_csv('temp/check_heights.csv')


new_heights <- 
  new_heights %>% 
  group_by( USDA_symbol) %>% 
  summarise( max_height = quantile(height, q_height)) %>% 
  mutate( max_height_data_source = '2017')


molinari <- 
  molinari %>%
  select( USDA_symbol, max_height) %>% 
  mutate( max_height_data_source = 'MOLINARI') %>% 
  filter( complete.cases(.))

all_heights <- rbind(old_heights, new_heights, molinari) 

all_heights <- 
  all_heights %>% 
  arrange( USDA_symbol, max_height_data_source ) %>% 
  group_by(USDA_symbol) %>% 
  filter( row_number() == 1)

write_csv(all_heights, path = outfile)
