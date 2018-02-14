library(tidyverse)
library(ggplot2)


source( 'data-raw/ggplots_for_leaf_traits.R')

leaf <- read_csv('data-raw/leaf_traits.csv')
root <- read_csv('data-raw/specific_root_length.csv')
heights <- read_csv('data-raw/clean_heights.csv')
seed_mass <- read_csv('data-raw/clean_seed_mass.csv')

leaf$plant_number <- as.character(leaf$plant_number)


leaf %>% filter(!censor) %>% ggSLA 
leaf %>% filter(! censor) %>% ggLDMC

leaf %>% filter(!censor, USDA_symbol == 'LEBI4') %>% ggSLA + geom_text( aes( label = paste(plant_number, leaf_number, sep = ',')))
leaf %>% filter(!censor, USDA_symbol == 'AMME') %>% ggLDMC + geom_text( aes( label = paste(plot, plant_number, leaf_number, sep = ',')))


dat <- 
  leaf %>% 
  filter( !censor) %>%
  group_by(USDA_symbol, date, petiole, plant_number) %>% 
  summarise(SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(LA, na.rm = T)) %>% 
  left_join( root, by = c('USDA_symbol', 'plant_number')) 

dat %>% 
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


