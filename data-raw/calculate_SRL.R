rm(list = ls())
library(tidyverse)

my_codes <- read.csv('data-raw/my_codes.csv')
weights <- read.csv('data-raw/root_data/root_weights.csv')
lengths <- read.csv('data-raw/root_data/cleaned_root_lengths.csv')

my_codes <- my_codes %>% 
  select(species, USDA_symbol)

lengths$datetime <- as.POSIXct(lengths$datetime)

weights$plot[is.na(weights$plot)] <- 'non_plot'

weights <- 
  weights %>% 
  mutate( species = str_trim(species)) %>% 
  left_join(my_codes, by = 'species')

lengths <- 
  lengths %>% 
  mutate( species = str_trim(species)) %>% 
  left_join(my_codes, by = 'species')

root_data <- 
  lengths %>% 
  full_join(weights, by = c('plot', 'USDA_symbol', 'plant_number'))

root_data <- root_data %>% 
  mutate(SRL = total_length_cm/dry_weight)

write_csv(root_data, 'data-raw/non_plot_root_traits.csv')
