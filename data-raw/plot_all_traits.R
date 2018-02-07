library(tidyverse)
library(ggplot2)

my_codes <- read_csv('data-raw/my_codes.csv')
leaf <- read_csv('data-raw/non_plot_leaf_data.csv')
root <- read_csv('data-raw/non_plot_root_traits.csv')
heights <- read_csv('data-raw/2017-plant-heights.csv')

leaf$plant_number <- as.character(leaf$plant_number)



dat <- 
  leaf %>% 
  group_by(USDA_symbol, plant_number) %>% 
  summarise(SLA = mean(SLA, na.rm = T), LDMC = mean(LDMC, na.rm = T), LA = mean(Total.Area, na.rm = T)) %>% 
  left_join( root, by = c('USDA_symbol', 'plant_number')) 

dat %>% filter(USDA_symbol == 'LOPE', plant_number == 3)

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


