rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/clean_SRL.csv'

alias <- read.csv('data-raw/alias.csv')
weights <- read.csv('data-raw/root_data/root_weights.csv')
lengths <- read.csv('data-raw/root_data/clean_root_lengths.csv')

alias <- 
  alias %>% 
  select(alias, USDA_symbol)

lengths$datetime <- as.POSIXct(lengths$datetime)

weights$plot[is.na(weights$plot)] <- 'non_plot'

weights %>% distinct(species)

weights <- 
  weights %>% 
  mutate( alias = toupper(str_trim(species))) %>% 
  left_join(alias, by = 'alias')


root_data <- 
  lengths %>% 
  full_join(weights, by = c('plot', 'USDA_symbol', 'plant_number'))

root_data <- 
  root_data %>% 
  mutate(`SRL (m/g)` = total_length_cm/100/dry_weight) %>% 
  select(species, USDA_symbol, plot, plant_number, `SRL (m/g)`)

root_data %>% 
  write_csv(outfile)
