rm(list = ls())
library(tidyverse)
library(stringr)
library(lubridate)

outfile <- 'data-raw/temp/AK_doublecheck_canopy_dimensions.csv'
traits <- read_csv('data-raw/raw_trait_data/2017-trait-measurements.csv')
alias <- read_csv('data-raw/alias.csv')


AK_canopy_dims <- 
  traits %>% 
  mutate( year = year( mdy(date_collected))) %>% 
  select( year, sequence, species, plot, plant_number, leaf_number, starts_with('notes')) %>% 
  filter( plot == 'non_plot') %>% 
  unite( extra, starts_with('notes'), sep = ' ') %>% 
  filter( str_detect( extra, '\\d+')) %>% 
  mutate( canopy_dims = str_extract_all(extra, '(?<![wW])[0-9\\.]+')) %>% 
  group_by( row_number() ) %>% 
  filter( length(unlist(canopy_dims)) == 3) %>% 
  mutate( canopy_dims = paste(unlist(canopy_dims), collapse = '-')) %>% 
  separate( canopy_dims, c('height', 'width', 'length'), sep = '-') %>%
  mutate( species = toupper(species)) %>% 
  ungroup() %>% 
  select( -leaf_number, -`row_number()`) %>% 
  rename('USDA_symbol' = species) %>% 
  select( year, sequence, USDA_symbol, plot, plant_number, height, width, length, extra) %>% 
  distinct() 

read_csv('data-raw/raw_trait_data/canopy_dimensions.csv') %>% 
  rename( 'USDA_symbol'  = species ) %>%
  left_join(AK_canopy_dims, by = c('USDA_symbol', 'year', 'plot', 'plant_number')) %>% 
  write_csv(outfile)

# 
# AK_canopy_dims %>% 
#   left_join(read_csv('data-raw/raw_trait_data/canopy_dimensions.csv') %>% 
#               rename('USDA_symbol' = species),  by = c('USDA_symbol', 'year', 'plot', 'plant_number') ) %>% 
#   mutate( width.x = as.numeric( width.x)) %>% 
#   ggplot( aes(  width.x, width.y)) + geom_point()
# 
# 
# AK_canopy_dims %>% 
#   left_join(read_csv('data-raw/raw_trait_data/canopy_dimensions.csv') %>% 
#               rename('USDA_symbol' = species),  by = c('USDA_symbol', 'year', 'plot', 'plant_number') ) %>% 
#   mutate( length.x = as.numeric( length.x)) %>% 
#   ggplot( aes(length.x, length.y)) + geom_point()
