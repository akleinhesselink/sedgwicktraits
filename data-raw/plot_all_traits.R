library(tidyverse)
library(ggplot2)


source( 'data-raw/ggplots_for_leaf_traits.R')

leaf <- read_csv('data-raw/leaf_traits.csv')
root <- read_csv('data-raw/specific_root_length.csv')
heights <- read_csv('data-raw/clean_heights.csv')
seed_mass <- read_csv('data-raw/clean_seed_mass.csv')

leaf$plant_number <- as.character(leaf$plant_number)


leaf %>% filter(!censor & plot == 'non_plot') %>% ggSLA 
leaf %>% filter(!censor & plot == 'non_plot') %>% ggLDMC

leaf %>% filter(!censor) %>% ggSLA 
leaf %>% filter(!censor) %>% ggLDMC

dat <- 
  leaf %>% 
  filter( !censor) %>%
  group_by(USDA_symbol, date, petiole, plant_number) %>% 
  summarise(SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T)) %>% 
  left_join( root, by = c('USDA_symbol', 'plant_number')) 

dat %>% 
  filter( plot == 'non_plot') %>% 
  select( USDA_symbol, plant_number, LA, SLA, LDMC, SRL) %>% 
  gather(trait, value, LA, SLA, LDMC, SRL)  %>% 
  group_by( USDA_symbol, trait) %>% 
  mutate( std_value = scale(value)) %>% 
  mutate( outlier = ifelse(abs(std_value) > 2, paste( USDA_symbol, plant_number), '')) %>% 
  ggplot( aes( x = USDA_symbol, y = value)) + 
    geom_point(alpha = 0.2) + 
    geom_boxplot(alpha = 0.2) +
    geom_text( aes( label = outlier)) +
    facet_wrap(~trait, scales = 'free') + 
    coord_flip()

dat %>% 
  filter( USDA_symbol == 'AVBA')

library(sedgwickspecies)

individual_level_traits <- 
  dat %>% 
  group_by (plot,  USDA_symbol, plant_number) %>% 
  summarise( SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T), SRL = mean(SRL, na.rm = T)/100) %>% 
  left_join(sedgwick_plants) %>% 
  filter(plot == 'non_plot') 

test <- dat %>% ungroup %>% distinct(USDA_symbol)

other_traits <- 
  dat %>%
  filter( USDA_symbol %in% c('SIGA', 'CHPA8', 'ANAR')) %>% 
  group_by(USDA_symbol, plant_number) %>% 
  summarise( SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T), SRL = mean(SRL, na.rm = T)/100) %>% 
  left_join(sedgwick_plants) %>% 
  group_by(current_code) %>% 
  summarise( SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T), SRL = mean(SRL, na.rm = T)) 
  
write_csv(other_traits, 'data-raw/other_traits_for_Anmol.csv')

metadata <- data.frame(trait = c('SLA', 'LDMC', 'LA', 'SRL', 'height'), units = c('cm^2 per g', 'g dry per g wet', 'cm^2', 'm per g', 'cm'))

metadata %>% write_csv('data-raw/grass_metadata.csv')

individual_level_traits %>% 
  ungroup() %>%
  filter( family == 'POACEAE') %>% 
  select( USDA_symbol:species) %>% 
  write_csv('data-raw/grass_traits_for_Brody.csv')

individual_level_traits %>% 
  ggplot( aes( x = SLA, y = SRL)) + 
  geom_point() + 
  facet_wrap( ~ USDA_symbol, scales = 'free')

heights  %>% 
  left_join( sedgwick_plants) %>% 
  filter( family == 'POACEAE') %>% 
  select(USDA_symbol:species)

heights %>% 
  left_join(sedgwick_plants) %>% 
  filter( family == 'POACEAE', dataset == 2017) %>%
  select(USDA_symbol:species) %>% 
  write_csv('data-raw/grass_heights_for_brody.csv')

seed_mass %>% 
  left_join(sedgwick_plants, by = "USDA_symbol") %>% 
  filter( family == 'POACEAE') %>% 
  distinct(USDA_symbol, seed_mass, dataset)


  

