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
  left_join(la_dat, by = c('USDA_symbol', 'plant_number', 'leaf_number', 'petiole', 'date')) %>% 
  mutate(SLA = total_area/dry_mass_g) %>% 
  mutate(LDMC = dry_mass_g/wet_mass_g)  %>% 
  mutate( LA = total_area/n_leaves) %>% 
  select(date, sequence, species, USDA_symbol, plot, plant_number, leaf_number, petiole, leaf_length_cm, leaf_width_cm, wet_mass_g, dry_mass_g, SLA, LDMC, LA)

# edit/censor records ----------------------------------------------------------------------- # 


dat <- 
  dat %>% 
  mutate( censor = F) %>% 
  mutate( censor = ifelse(USDA_symbol == 'FEMY2' & leaf_number == 3 & plant_number == 3, T, censor))
  
dat <- 
  dat %>% 
  mutate( censor = ifelse( USDA_symbol == 'AMME' & leaf_number == 1 & plant_number == 3 & plot == 'comp', T, censor))

# output -------------------------------------------------------- 

dat %>% 
  write_csv(outfile)

