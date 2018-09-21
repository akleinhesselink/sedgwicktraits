rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/clean_heights.csv'
q_height <- 0.95

old_heights <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
new_heights <- read_csv('data-raw/2017-plant-heights.csv')
molinari <- read_csv('data-raw/old-data/molinari.csv')
alias <- read_csv('data-raw/alias.csv')

old_heights <- 
  old_heights %>% 
  select( species, `max_height(cm)` ) %>% 
  rename( 'max_height' = `max_height(cm)` , 'alias' = species) %>%
  left_join( alias ) %>% 
  select( USDA_symbol, max_height) %>% 
  distinct() %>% 
  mutate( dataset = 'tapioca')

new_heights <- 
  new_heights %>% 
  group_by( USDA_symbol) %>% 
  summarise( max_height = quantile(height, q_height)) %>% 
  mutate( dataset = '2017')

molinari <- 
  molinari %>%
  select( USDA_symbol, max_height) %>% 
  mutate( dataset = 'molinari')

all_heights <- rbind(old_heights, new_heights, molinari) 

all_heights <- 
  all_heights %>% 
  arrange( USDA_symbol, dataset ) %>% 
  group_by(USDA_symbol) %>% 
  filter( row_number() == 1)

write_csv(all_heights, path = outfile)
