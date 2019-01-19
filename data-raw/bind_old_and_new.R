rm(list = ls())

library(tidyverse)
library(ggplot2)
library(sedgwickspecies)

outfile <- 'data/sedgwick_traits.Rdata'

new <- read_csv( 'data-raw/cleaned_trait_data/clean_all_2017_traits.csv')
old <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
alias <- read_csv('data-raw/alias.csv')
avg_tlp <- read_csv('data-raw/cleaned_trait_data/clean_tlp.csv')

new <- 
  new %>% 
  mutate( dataset = '2017')


old <- 
  old %>% 
  mutate( `phenology` = `phenology (corrected May 2016- frame shift error)`) %>% 
  mutate( seed_mass_data_source = 'TAPIOCA', 
          notes = '', 
          dataset = 'TAPIOCA', 
          max_height_data_source = 'TAPIOCA') %>% 
  rename( 'alias' = species, 
          'leaf_size' = `leaf_size(cm2)`, 
          'SLA' = `SLA (g/cm2)`, 
          'LDMC' = `LDMC(mg/g)`, 
          'LAI' = `LAI (LA/canopy_area)`, 
          'LAR' = `LAR(cm2/g)`, 
          'seed_mass' = `seed_mass(g)`, 
          'seed_size' = `seed_size (mm3)`, 
          'max_height' = `max_height(cm)`, 
          'SRL' = `SRL(m/g)`, 
          'relative_spread' = `relative_spread(lateral/height)`, 
          'rooting_depth' = `rooting_depth (oscar)`) %>% 
  left_join(alias) %>% 
  left_join(avg_tlp) %>% 
  rename('turgor_loss_point' = tlp)

sedgwicktraits <- 
  bind_rows(new, old) %>% 
  select( -alias,   - `phenology (corrected May 2016- frame shift error)`, -`phenology (DOY 50% fruit)`)

sedgwicktraits <- 
  sedgwicktraits %>% 
  left_join(sedgwick_plants, by = 'USDA_symbol') %>% 
  select( calflora_binomial, `leaf_size`:`turgor_loss_point` ) %>% 
  rename( 'species' = calflora_binomial) %>%
  distinct()

usethis::use_data(sedgwicktraits, overwrite = T)

