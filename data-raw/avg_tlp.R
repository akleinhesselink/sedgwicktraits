rm(list = ls())
library(tidyverse)

outfile <- 'data-raw/cleaned_trait_data/avg_tlp.csv'

tlp_dat <- read_csv('data-raw/raw_trait_data/Sedgwick_TLP_2017.csv')
alias <- read_csv('data-raw/alias.csv')


tlp_dat <- 
  tlp_dat %>% 
  rename('alias' = species) %>% 
  left_join(alias) %>% 
  select(sequence, USDA_symbol, individual, leaf, osmotic_potential, tlp) 

tlp_dat %>% 
  group_by( USDA_symbol) %>% 
  mutate( mtlp = mean(tlp, na.rm = T)) %>% 
  ungroup() %>% 
  mutate( USDA_symbol = factor(USDA_symbol, levels = unique( USDA_symbol[ order(mtlp) ] ), ordered = T)) %>% 
  ggplot( aes( x = USDA_symbol, y = tlp)) + 
  geom_point() + 
  coord_flip()
  
average_tlp <- 
  tlp_dat %>% 
  group_by( USDA_symbol ) %>% 
  summarise( tlp = mean(tlp, na.rm = T))

average_tlp %>% 
  write_csv(path = outfile)
