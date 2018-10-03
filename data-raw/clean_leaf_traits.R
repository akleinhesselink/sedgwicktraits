rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/clean_leaf_traits.csv'

dat <- read_csv('data-raw/2017-trait-measurements.csv')
la_dat <- read_csv('data-raw/leaf_area.csv')
focal_la_dat <- read_csv('data-raw/focal_leaf_areas.csv')
alias <- read_csv('data-raw/alias.csv')


focal_la_dat <-
  focal_la_dat %>%
  rename( 'notes' = X7) %>%
  filter( !(str_detect( Name, 'lebo'))) %>%
  separate(Name, c('plot', 'alias', 'plant_number', 'leaf_number'),  '-', remove = F) %>%
  rename( 'slice' = Name) %>%
  mutate( plant_number = str_extract(plant_number, '\\d+'),
          leaf_number = str_extract(leaf_number, '\\d+')) %>%
  mutate( alias = toupper(alias)) %>%
  left_join(alias, by = 'alias') %>%
  mutate( petiole = F) %>%
  mutate( petiole = ifelse(!is.na(notes) & str_detect(notes, 'petiole'), T, petiole)) %>%
  mutate( all = F) %>%
  rename( 'total_area' = `Total Area`) %>%
  select( slice, plot, alias, USDA_symbol, plant_number, leaf_number, total_area, petiole, notes)

la_dat <- 
  la_dat %>% 
  mutate( leaf_number = as.character( leaf_number), 
          plant_number = as.character( plant_number)) %>%
  select( slice, plot, alias, USDA_symbol, plant_number, leaf_number, total_area, petiole, notes)

la_dat <- rbind(la_dat, focal_la_dat)

dat <- 
  dat %>% 
  mutate( LDMC =  dry_mass_g / wet_mass_g ) %>% 
  mutate( plant_number = as.character(plant_number )) %>% 
  mutate( alias = toupper(species)) %>% 
  left_join(alias, by = 'alias') %>% 
  mutate( date = lubridate::dmy( date_collected)) %>% 
  mutate( petiole = str_detect(notes, regex('.*with.petiole.*', ignore_case = T))) %>%
  mutate( petiole = ifelse (is.na(petiole), F, petiole))

dat <- 
  dat %>% 
  mutate(leaf_number = as.character(leaf_number)) %>% 
  left_join(la_dat, by = c('USDA_symbol', 'plot', 'plant_number', 'leaf_number', 'petiole')) %>% 
  mutate(SLA = total_area/dry_mass_g) %>% 
  mutate(LDMC = 1000*dry_mass_g/wet_mass_g)  %>% 
  mutate(LA = total_area/n_leaves) %>% 
  select(sequence, date, species, USDA_symbol, plot, plant_number, leaf_number, petiole, leaf_length_cm, leaf_width_cm, wet_mass_g, dry_mass_g, SLA, LDMC, LA)

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

