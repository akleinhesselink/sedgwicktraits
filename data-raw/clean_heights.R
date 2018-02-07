rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/clean_heights.csv'
q_height <- 0.9

old_heights <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
new_heights <- read_csv('data-raw/2017-plant-heights.csv')
molignari <- read_csv('data-raw/clean_molignari_traits.csv')
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

molignari <- 
  molignari %>%
  select( USDA_symbol, max_height) %>% 
  mutate( dataset = 'molignari')


rbind(old_heights, new_heights, molignari) %>% 
  write_csv(outfile)
