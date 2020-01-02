rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/cleaned_trait_data/clean_GSK_species_leaf_traits.csv'

leaf_traits <- read_csv('data-raw/clean_leaf_traits.csv')

focal_species_avgs <- 
  leaf_traits %>% 
  filter( plot != 'non_plot') %>% 
  group_by(USDA_symbol) %>% 
  summarise( SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T)) %>% 
  mutate( type = 'focal')

write_csv(focal_species_avgs, outfile)
