rm(list = ls())
library(tidyverse)
library(stringr)

dat <- read_csv('data-raw/2017-trait-measurements.csv')
la_dat <- read_csv('data-raw/leaf_area.csv')
alias <- read_csv('data-raw/alias.csv')

outfile <- 'data-raw/leaf_traits.csv'

la_dat <- 
  la_dat %>% 
  mutate( leaf_number = as.character( leaf_number), 
          plant_number = as.character( plant_number))

dat <- 
  dat %>% 
  mutate( LDMC =  dry_mass_g / wet_mass_g ) %>% 
  mutate( plant_number = as.character(plant_number )) %>% 
  mutate( alias = toupper(species)) %>% 
  left_join(alias, by = 'alias')

dat <- 
  dat %>% 
  mutate( alias = ifelse( alias == 'VUMA', 'VUMI', alias)) %>%
  mutate( date = lubridate::dmy( date_collected))

dat <- 
  dat %>% 
  mutate( petiole = str_detect(notes, regex('.*with.petiole.*', ignore_case = T))) %>%
  mutate( petiole = ifelse (is.na(petiole), F, petiole))



dat <- 
  dat %>% 
  mutate(leaf_number = as.character(leaf_number)) %>% 
  left_join(la_dat, by = c('USDA_symbol', 'plant_number', 'leaf_number', 'petiole')) %>% 
  mutate(SLA = total_area/dry_mass_g) %>% 
  mutate(LDMC = dry_mass_g/wet_mass_g)  %>% 
  mutate( LA = total_area/n_leaves) %>% 
  select( sequence, species, USDA_symbol, plot, plant_number, leaf_number, petiole, leaf_length_cm, leaf_width_cm, wet_mass_g, dry_mass_g, SLA, LDMC, LA)

# output -------------------------------------------------------- 

dat %>% 
  write_csv(outfile)

test <- 
  dat %>% 
  filter( is.na(USDA_symbol))
