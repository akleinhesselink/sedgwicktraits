rm(list = ls())

library(tidyverse)
library(ggplot2)
library(sedgwickspecies)

outfile <- 'data/sedgwick_traits.Rdata'

new <- read_csv( 'data-raw/cleaned_trait_data/clean_all_2017_traits.csv')
tapioca <- read_csv('data-raw/old-data/tapioca_trait_averages.csv')
gk_2016 <- read_csv('data-raw/old-data/2016_sp_avg.csv')
alias <- read_csv('data-raw/alias.csv')
avg_tlp <- read_csv('data-raw/cleaned_trait_data/clean_tlp.csv')

tapioca_raw <- read_csv('data-raw/old-data/tapioca_raw_traits.csv')

tapioca <- 
  tapioca %>% 
  mutate(`LAI (LA/canopy_area)` = ifelse(species %in% c('LACA', 'PLER'), `LAI (LA/canopy_area)`*100, `LAI (LA/canopy_area)`)) 

tapioca_raw <- 
  tapioca_raw %>% 
  mutate(`canopy_Projected_area(cm2)` = ifelse(species %in% c('LACA', 'PLER'), `canopy_Projected_area(cm2)`/100, `canopy_Projected_area(cm2)`))

tapioca %>% 
  ggplot(aes( x = species, y = `LAI (LA/canopy_area)`)) + 
  geom_point(data = tapioca_raw, aes( x = species, y = `total_leaf_area(cm2)`/`canopy_Projected_area(cm2)` ), color = 'red')  + 
  geom_point() + coord_flip() 

canopy_area <- 
  tapioca_raw %>% 
  group_by( species ) %>% 
  summarise( projected_area_cm2 = min(`canopy_Projected_area(cm2)`, na.rm = T))

tapioca <- 
  tapioca %>% 
  left_join(canopy_area)

new <- 
  new %>% 
  mutate( dataset = '2017') %>% 
  left_join(avg_tlp) %>% 
  rename('turgor_loss_point' = tlp)

tapioca <- 
  tapioca %>% 
  mutate( `phenology` = `phenology (corrected May 2016- frame shift error)`) %>% 
  mutate( seed_mass_data_source = 'TAPIOCA', 
          notes = '', 
          dataset = 'TAPIOCA', 
          max_height_data_source = 'TAPIOCA') %>% 
  rename( 'alias' = species, 
          'leaf_size' = `leaf_size(cm2)`, 
          'SLA' = `SLA (g/cm2)`, 
          'LDMC' = `LDMC(mg/g)`, 
          'LAI' = `LAI (LA/canopy_area)`, 
          'LAR' = `LAR(cm2/g)`, 
          'seed_mass' = `seed_mass(g)`, 
          'seed_size' = `seed_size (mm3)`, 
          'max_height' = `max_height(cm)`, 
          'SRL' = `SRL(m/g)`, 
          'relative_spread' = `relative_spread(lateral/height)`,
          'projected_area' = projected_area_cm2,
          'rooting_depth' = `rooting_depth (oscar)`) %>% 
  left_join(alias) %>% 
  left_join(avg_tlp) %>% 
  rename('turgor_loss_point' = tlp)

gk_2016 <- 
  gk_2016 %>% 
  mutate( dataset = '2016', 
          notes = '', 
          seed_mass_data_source = '2016', 
          max_height_data_source = '2016', 
          LAR = NA, 
          SRL = fine_root_length_m/fine_root_dry_mass_g, 
          seed_size = NA) %>% 
  rename( 'alias' = species, 
          'leaf_size' = leaf_area_cm2, 
          'SLA' = sla_cm2_g, 
          'LDMC' = ldmc_mg_g, 
          'LAI' = lai_la_canopy, 
          'seed_mass' = seed_mass_g, 
          'relative_spread' = relative_spread_lateral_height, 
          'max_height' = height_to_infloresence_cm, 
          'rooting_depth' = rooting_depth_cm)  %>% 
  left_join(alias) %>%
  left_join(avg_tlp) %>% 
  rename('turgor_loss_point' = tlp) %>% 
  select( alias, max_height, rooting_depth, relative_spread, LAI:LDMC, d13C:turgor_loss_point)

sedgwicktraits <- 
  bind_rows(new, tapioca, gk_2016) %>% 
  select( -alias,   - `phenology (corrected May 2016- frame shift error)`, -`phenology (DOY 50% fruit)`)

sedgwicktraits <- 
  sedgwicktraits %>% 
  left_join(sedgwick_plants, by = 'USDA_symbol') %>% 
  select( USDA_symbol, leaf_size:leaf_pH ) %>% 
  distinct() %>% 
  group_by(USDA_symbol) %>% 
  mutate( turgor_loss_point = mean(turgor_loss_point, na.rm = T)) ### Correct for duplicate tlp

usethis::use_data(sedgwicktraits, overwrite = T)

