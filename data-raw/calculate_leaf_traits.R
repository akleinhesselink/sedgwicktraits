library(tidyverse)
library(stringr)

dat <- read.csv('data-raw/2017-trait-measurements.csv')
la_dat <- read.csv('data-raw/non-plot_leaf_area_data.csv')
my_codes <- read_csv('data-raw/my_codes.csv')

dat <- 
  dat %>% 
  mutate( LDMC =  dry_mass_g / wet_mass_g ) %>% 
  mutate( plant_number = as.character(plant_number )) %>% 
  mutate( species = toupper(species)) %>% 
  left_join(my_codes, by = 'species')

dat$date <- as.character( dat$date_collected)
dat$date  <- lubridate::dmy( dat$date_collected)
dat$date[is.na(dat$date)] <- lubridate::mdy( dat$date_collected[ is.na(dat$date) ])

la_dat <- 
  la_dat %>% 
  rename(plant_number = plant, species = Species, leaf_number= leaf) %>% 
  mutate( date = lubridate::as_date( date )) %>% 
  mutate( plant_number = str_extract(plant_number, '\\d+'), 
          leaf_number = str_extract(leaf_number, '\\d+')) %>%
  mutate(species = toupper(species)) %>% 
  left_join(my_codes, by = 'species')

dat <- 
  dat %>% 
  left_join(la_dat, by = c('USDA_symbol', 'plant_number', 'leaf_number', 'date')) %>% 
  mutate(SLA = Total.Area/dry_mass_g)

# output -------------------------------------------------------- 

write_csv(dat %>% filter( plot == 'non_plot'), 'data-raw/non_plot_leaf_data.csv')

