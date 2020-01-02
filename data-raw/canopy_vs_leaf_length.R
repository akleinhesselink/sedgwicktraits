rm(list  = ls() ) 
library(tidyverse)

traits <- read_csv('data-raw/cleaned_trait_data/clean_leaf_traits.csv')
canopy <- read_csv('data-raw/raw_trait_data/canopy_dimensions.csv')
alias <- read_csv('data-raw/alias.csv')

traits <- traits %>% 
  mutate( leaf_length_cm = ifelse( USDA_symbol %in% c( 'THLA3', 'THCU') , leaf_length_cm/10, leaf_length_cm)) %>% 
  group_by( USDA_symbol, plant_number ) %>% 
  summarise( max_leaf_length = max(leaf_length_cm, na.rm = T), 
             avg_leaf_length = mean(leaf_length_cm, na.rm = T)) 

canopy <- 
  canopy %>%  
  rename( 'alias' = species) %>% 
  left_join(alias) %>% 
  select(-alias)


traits %>% 
  filter( USDA_symbol %in% c('THLA3', 'THCU', 'CHPA8', 'ERMO7')) %>% 
  left_join(canopy %>% 
              select( -height ) %>% 
              gather( dim, value, width:length ) %>% 
              group_by( USDA_symbol, plant_number ) %>% 
              summarise( dim1 =  max( value ), dim2 = min(value) ) ) %>% 
  ggplot( aes( x = max_leaf_length, y = dim1 )) + 
  geom_point() + 
  geom_smooth(se = F, method = 'lm') + 
  facet_wrap( ~ USDA_symbol)

