rm(list = ls())
library(tidyverse)
library(stringr)
library(lubridate)

outfile <- 'data-raw/cleaned_trait_data/clean_leaf_traits.csv'

areas <- read_csv('data-raw/cleaned_trait_data/clean_leaf_area.csv')
weights <- read_csv('data-raw/raw_trait_data/AK_leaf_weights.csv') %>% 
  mutate( date = mdy( date ) )

# ------------------ # 

traits <- 
  areas %>% 
  filter( !all ) %>% 
  left_join( 
    weights %>% 
      rename( 'USDA_symbol' = species ), 
    by = c('date', 'plot', 'USDA_symbol', 'plant', 'leaf', 'petiole')) %>%
  select(date, plot, USDA_symbol, plant, leaf, petiole, dry_mass_g, wet_mass_g, petiole_mass_g, dry_mass_petiole_g, total_area, n_leaves ) %>% 
  mutate( leaf_mass = dry_mass_g/n_leaves, 
          LDMC = 1000*dry_mass_g/wet_mass_g, 
          SLA = total_area/dry_mass_g, 
          LA = total_area/n_leaves ) %>% 
  mutate( censor = F) %>% 
  mutate( censor = ifelse(USDA_symbol == 'FEMY2' & leaf == 3 & plant == 3, T, censor))


traits %>% 
  group_by( USDA_symbol, petiole, plot  ) %>% 
  summarise( LDMC = mean(LDMC, na.rm = T) ) %>% 
  ggplot( aes( x = USDA_symbol, y = LDMC, color = petiole, shape = plot )) + 
  geom_point() + 
  coord_flip()

traits %>% 
  group_by( USDA_symbol, petiole, plot  ) %>% 
  summarise( SLA = mean(SLA, na.rm = T) ) %>% 
  ggplot( aes( x = USDA_symbol, y = SLA, color = petiole, shape = plot )) + 
  geom_point() + 
  coord_flip()


