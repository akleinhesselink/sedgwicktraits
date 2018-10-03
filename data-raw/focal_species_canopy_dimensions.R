rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/focal_species_canopy_dimensions.csv'
traits <- read_csv('data-raw/2017-trait-measurements.csv')
alias <- read_csv('data-raw/alias.csv')

focal_canopy_dims <- 
  traits %>% 
  select( sequence, 'species', 'plot', 'plant_number', 'leaf_number', notes, starts_with('X')) %>% 
  filter( !plot == 'non_plot') %>% 
  unite( extra, c(notes, starts_with('X')), sep = ' ') %>% 
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

focal_canopy_dims %>% 
  rename('alias' = species) %>% 
  left_join(alias) %>% 
  select( sequence, USDA_symbol, plot, plant_number, height, width, length, extra) %>% 
  write_csv(outfile)


