library(tidyverse)
library(ggplot2)

leaf <- read_csv('data/non_plot_leaf_data.csv')
root <- read_csv('data/non_plot_root_traits.csv')

root <- 
  root %>% 
  mutate(species = str_replace_all(species, c('LOTUS2' = 'LOST')))

dat <- 
  leaf %>% 
  group_by(species, plant_number) %>% 
  summarise(SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(Total.Area, na.rm = T)) %>% 
  left_join( root, by = c('species', 'plant_number'))

dat %>% 
  select( species, plant_number, LA, SLA, LDMC, SRL) %>% 
  gather(trait, value, LA, SLA, LDMC, SRL)  %>% 
  filter( trait != 'LA') %>% 
  ggplot( aes( x = species, y = value)) + 
    geom_point() + 
    geom_boxplot(alpha = 0.2) + 
    facet_wrap(~trait, scales = 'free') + 
    coord_flip()
