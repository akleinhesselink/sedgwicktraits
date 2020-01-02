rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/temp/GSK_canopy_dimensions.csv'
traits <- read_csv('data-raw/raw_trait_data/2017-trait-measurements.csv')
alias <- read_csv('data-raw/alias.csv')

GSK_canopy_dims <- 
  traits %>% 
  select( sequence, 'species', 'plot', 'plant_number', 'leaf_number', starts_with('notes')) %>% 
  filter( !plot == 'non_plot') %>% 
  unite( extra, starts_with('notes'), sep = ' ') %>% 
  filter( str_detect( extra, '\\d+')) %>% 
  mutate( canopy_dims = str_extract_all(extra, '(?<![wW])[0-9\\.]+')) %>% 
  group_by( row_number() ) %>% 
  filter( length(unlist(canopy_dims)) == 3) %>% 
  mutate( canopy_dims = paste(unlist(canopy_dims), collapse = '-')) %>% 
  separate( canopy_dims, c('height', 'width', 'length'), sep = '-') %>%
  mutate( species = toupper(species)) %>% 
  filter( !(species == 'HOMU' & plot == '754')) %>% 
  ungroup() %>% 
  select( -leaf_number, -`row_number()`)

GSK_canopy_dims %>% 
  rename('alias' = species) %>% 
  left_join(alias) %>% 
  select( sequence, USDA_symbol, plot, plant_number, height, width, length, extra) %>% 
  distinct() %>% 
  write_csv(outfile)


