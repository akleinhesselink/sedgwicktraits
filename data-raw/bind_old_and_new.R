rm(list = ls())

library(tidyverse)
library(ggplot2)

outfile <- 'data/sedgwick_traits.csv'

new <- read_csv( 'data-raw/all_2017_traits.csv')
old <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
alias <- read_csv('data-raw/alias.csv')

new <- 
  new %>% 
  mutate( dataset = '2017')

old <- 
  old %>% 
  mutate( `phenology (DOY 50% fruit)` = `phenology (corrected May 2016- frame shift error)`) %>% 
  mutate( seed_mass_data_source = 'TAPIOCA', 
          notes = '', 
          dataset = 'TAPIOCA') %>% 
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(  - `phenology (corrected May 2016- frame shift error)`)

all_traits <- 
  bind_rows(new, old)

write_csv(all_traits, outfile)
