rm(list = ls())
library(tidyverse)

outfile <- 'data-raw/clean_isotope_data.csv'
isotopes <- read_csv('data-raw/leaf_isotopes_2016.csv')
alias <- read_csv('data-raw/alias.csv')
old_traits <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')

isotopes <- 
  isotopes %>% 
  mutate( species = ifelse( str_detect(species, 'LASE'), 'LASE', species)) %>% 
  mutate( species = ifelse( str_detect(species, 'AVBA'), 'AVBA', species)) %>%
  rename( 'USDA_symbol' = species,  
          'd13C' = `d 13C (‰)`, 
          'd15N' = `d 15N (‰)`, 
          'foliar_N' = percent_N) %>% 
  mutate( CN_ratio = percent_C/foliar_N)

isotopes <- 
  isotopes %>% 
  left_join(alias)

isotopes %>% ggplot( aes( x = USDA_symbol, y = CN_ratio)) + geom_point() + coord_flip()

isotopes %>% ggplot( aes( x = USDA_symbol, y = d13C)) + geom_point() + coord_flip()

isotopes %>% ggplot( aes( x = USDA_symbol, y = d15N)) + geom_point() + coord_flip()

old_isotopes <- 
  old_traits %>% 
  left_join(alias, by = c('species' = 'alias')) %>%
  select( USDA_symbol, foliar_N, CN_ratio, d15N, d13C) %>%
  mutate( dataset = 'tapioca')
  
isotopes <- isotopes %>% 
  select(USDA_symbol, foliar_N, CN_ratio, d15N, d13C) %>% 
  mutate( dataset = '2017')

all_isotopes <- bind_rows(isotopes, old_isotopes)

isotopes$foliar_N
isotopes$CN_ratio

all_isotopes %>% 
  mutate( USDA_symbol = factor( USDA_symbol, levels = unique(USDA_symbol[order(CN_ratio)]), ordered = T)) %>% 
  ggplot( aes( x = USDA_symbol, y = CN_ratio, color = dataset)) + 
  geom_point() + 
  coord_flip() 

all_isotopes %>% 
  mutate( USDA_symbol = factor( USDA_symbol, levels = unique(USDA_symbol[order(d13C)]), ordered = T)) %>% 
  ggplot( aes( x = USDA_symbol, y = d13C, color = dataset)) + 
  geom_point() + 
  coord_flip()

all_isotopes %>% 
  mutate( USDA_symbol = factor( USDA_symbol, levels = unique(USDA_symbol[order(d15N)]), ordered = T)) %>% 
  ggplot( aes( x = USDA_symbol, y = d15N, color = dataset)) + 
  geom_point() + 
  coord_flip()

isotope_avgs <- 
  all_isotopes %>% 
  group_by(USDA_symbol, dataset) %>% 
  summarise( foliar_N = mean(foliar_N), 
             CN_ratio = mean(CN_ratio), 
             d15N = mean(d15N), 
             d13C = mean(d13C))

isotope_avgs %>% 
  ungroup() %>% 
  mutate( USDA_symbol = factor( USDA_symbol, levels = unique(USDA_symbol[order(CN_ratio)]), ordered = T)) %>% 
  ggplot( aes( x = USDA_symbol, y = CN_ratio, color = dataset)) + 
  geom_point() + 
  coord_flip() 


write_csv(isotope_avgs, outfile)
