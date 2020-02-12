rm(list = ls())
library(tidyverse)
library(stringr)
library(lubridate)

outfile <- 'data-raw/cleaned_trait_data/clean_leaf_traits.csv'
areas <- read_csv('data-raw/cleaned_trait_data/clean_leaf_area.csv')

weights <- read_csv('data-raw/raw_trait_data/AK_leaf_weights.csv') %>% 
  mutate( date = mdy( date ) ) %>% 
  rename( 'USDA_symbol' = species ) %>% 
  mutate( plant = ifelse( USDA_symbol == 'THLA3', plant - 16, plant  )) %>% 
  mutate( plant = ifelse( USDA_symbol == 'THCU', plant - 8, plant )) 

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
  filter( ! censor) %>% 
  mutate( collection = paste( USDA_symbol, plot, sep = '_' )) %>% 
  ggplot( aes( x = collection, y = LDMC, color = petiole)) + 
  geom_point() + 
  coord_flip() 

traits %>% 
  filter( ! censor) %>% 
  mutate( collection = paste( USDA_symbol, plot, sep = '_' )) %>% 
  ggplot( aes( x = collection, y = SLA, color = petiole)) + 
  geom_point() + 
  coord_flip() 

traits %>% 
  filter( str_detect(USDA_symbol, 'CHGL')) %>% View

traits %>% 
  filter( USDA_symbol == 'GAAP2') %>% 
  select( USDA_symbol, plant, leaf,  wet_mass_g, dry_mass_g, SLA, LDMC) %>% View

traits %>% 
  filter( USDA_symbol == 'STME2') %>% View

traits %>% 
  filter( USDA_symbol == 'LEPA51') %>% View

sort( unique(traits$USDA_symbol) )

# --- Check grasses 
library(sedgwickspecies)

new_grass_traits <- 
  traits %>% 
  left_join( sedgwick_plants, by = 'USDA_symbol') %>% 
  filter( family == 'POACEAE', !censor, !is.na(LA), !is.na(leaf_mass) ) %>% 
  group_by(calflora_binomial, plot, plant, year(date) ) %>% 
  summarise( LA = mean( LA ), SLA = mean(SLA), LDMC =  mean( LDMC ), leaf_mass = mean( leaf_mass )) %>% 
  select( calflora_binomial, plot, plant, LA:leaf_mass )


my_sandel_traits <- read_csv('~/Dropbox/peer_reviews/Pavlika_Sandel_Grass_Traits/grass_traits_for_Brody.csv')

my_sandel_traits %>% 
  select( calflora_binomial, plant_number, SLA, LA) %>% 
  filter( calflora_binomial == 'Festuca myuros')

compare <- 
  my_sandel_traits %>% 
  rename( 'plant' = plant_number) %>% 
  select( calflora_binomial, plant, SLA, LDMC, LA ) %>% 
  left_join( new_grass_traits, by = c('calflora_binomial', 'plant')) 

compare %>% 
  select( calflora_binomial, plant, SLA.x, SLA.y) %>% 
  mutate( SLA.y - SLA.x )  %>% 
  View()

compare %>% 
  group_by( calflora_binomial ) %>% 
  summarise( mean( SLA.x) , mean(SLA.y))


compare %>% 
  select( calflora_binomial, plant, LA.x, LA.y) %>% 
  mutate( LA.y - LA.x )  %>% 
  View()


