rm(list = ls())
library(tidyverse)
library(stringr)
library(lubridate)

outfile <- 'data-raw/cleaned_trait_data/clean_leaf_traits.csv'
areas <- read_csv('data-raw/cleaned_trait_data/clean_leaf_area.csv')
weights <- read_csv('data-raw/raw_trait_data/AK_leaf_weights.csv') %>% 
  mutate( date = mdy( date ) ) %>% 
  rename( 'USDA_symbol' = species )

# ------------------ # 

traits <- 
  areas %>% 
  filter( !all ) %>% 
  left_join( 
    weights, 
    by = c('date', 'plot', 'USDA_symbol', 'plant', 'leaf', 'petiole')) %>%
  select(date, plot, USDA_symbol, plant, leaf, petiole, dry_mass_g, wet_mass_g, petiole_mass_g, dry_mass_petiole_g, total_area, n_leaves ) %>% 
  mutate( leaf_mass = dry_mass_g/n_leaves, 
          LDMC = 1000*dry_mass_g/wet_mass_g, 
          SLA = total_area/dry_mass_g, 
          LA = total_area/n_leaves ) 


# Check LDMC 

traits %>% 
  group_by( USDA_symbol, petiole, plot) %>% 
  select( date, USDA_symbol, plot, plant, leaf, dry_mass_g, wet_mass_g, petiole, LDMC, SLA, LA ) %>% 
  mutate( LDMC_scaled = scale(LDMC), SLA_scaled = scale(SLA), LA_scaled = scale(LA)) %>%
  select( date, USDA_symbol, petiole, plot, plant, leaf, LDMC_scaled, SLA_scaled, LA_scaled,  dry_mass_g, wet_mass_g, LDMC, SLA, LA) %>% 
  arrange( desc(abs( LDMC_scaled))) %>% 
  filter( abs( LDMC_scaled ) > 2.7 ) %>% 
  View()
  
# Check SLA 

traits %>% 
  group_by( USDA_symbol, petiole, plot) %>% 
  select( date, USDA_symbol, plot, plant, leaf, dry_mass_g, wet_mass_g, petiole, LDMC, SLA, LA ) %>% 
  mutate( LDMC_scaled = scale(LDMC), SLA_scaled = scale(SLA), LA_scaled = scale(LA), wet_SLA = scale(LA/wet_mass_g)) %>%
  select( date, USDA_symbol, petiole, plot, plant, leaf, SLA_scaled, wet_SLA, LA_scaled, LDMC_scaled,  dry_mass_g, wet_mass_g, LDMC, SLA, LA) %>% 
  arrange( desc(abs( SLA_scaled))) %>% 
  filter( abs( SLA_scaled ) > 2.7 ) %>% 
  View()

# Censor outliers for LDMC 
traits %>% 
  filter( USDA_symbol == 'FEMY2', plant == 3, leaf == 3 ) %>% View # parts might have fallen out of envelope 
traits %>% 
  filter( USDA_symbol == 'CHPO3' , plant == 4, leaf == 3 ) %>% View # something wrong with wet mass 
traits %>% 
  filter( USDA_symbol == 'STPU2', plant == 8, leaf == 2) %>% View # parts might have fallen out of envelope 

# Censor SLA 
traits %>% 
  filter( USDA_symbol == 'LEBI4', plant == 4, leaf == 2) %>% View # image mismatch:  This Leaf image is same as plant 4 leaf 1 

traits %>% 
  filter( USDA_symbol == 'CRCO34') %>% View  # Plant 2 leaf 1 -- dry sample is missing leaves, exclude 

# Censor leaves 

traits <- 
  traits %>% 
  mutate( censor = F) %>% 
  mutate( censor = ifelse(USDA_symbol == 'FEMY2' & plant == 3 & leaf == 3, T, censor), 
          censor = ifelse(USDA_symbol == 'CHPO3' & plant == 4 & leaf == 3, T, censor), 
          censor = ifelse(USDA_symbol == 'STPU2' & plant == 8 & leaf == 2, T, censor), 
          censor = ifelse(USDA_symbol == 'LEBI4' & plant == 4 & leaf == 2, T, censor), 
          censor = ifelse(USDA_symbol == 'CRCO34' & plant == 2 & leaf == 1, T, censor))


# Re-assign "petiole" VIPE, GIOC and LEBI4 don't have true petioles but were marked as such 

traits <- 
  traits %>% 
  mutate( petiole = ifelse( USDA_symbol %in% c('VIPE3', 'GIOC', 'LEBI4') & petiole, F, petiole))

# Check plots ------------------- # 

traits %>% 
  filter( ! censor ) %>% 
  filter( USDA_symbol == 'LOST4') %>% 
  ggplot( aes( x = plant, y = SLA, color = petiole )) + 
  geom_point() + 
  facet_wrap( ~ petiole + plot ) 

traits %>% 
  filter( ! censor , ! petiole ) %>% 
  filter( USDA_symbol == 'LOST4') %>% 
  ggplot( aes( x = plant, y = LA)) + 
  geom_point() 

traits %>% 
  filter( USDA_symbol == 'LOST4', ! petiole) %>% View

traits %>% 
  filter( ! censor) %>% 
  group_by( USDA_symbol, petiole, plant, plot  ) %>% 
  summarise( LDMC = mean(LDMC, na.rm = T) ) %>% 
  ggplot( aes( x = USDA_symbol, y = LDMC, color = petiole, shape = plot )) + 
  geom_point() + 
  coord_flip() + 
  facet_wrap( ~ petiole + plot )

traits %>% 
  filter( !censor) %>% 
  group_by( USDA_symbol, petiole, plant, plot  ) %>% 
  summarise( SLA = mean(SLA, na.rm = T) ) %>% 
  ggplot( aes( x = USDA_symbol, y = SLA, color = petiole, shape = plot )) + 
  geom_point() + 
  coord_flip() + 
  facet_wrap( ~ petiole + plot )

