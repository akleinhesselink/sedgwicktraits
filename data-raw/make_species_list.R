library(sedgwickspecies)
library(tidyverse)

alias <- read_csv('data-raw/alias.csv')

out <- 
  alias %>% 
  left_join(sedgwick_plants, by = 'USDA_symbol') %>% 
  select(standard_binomial, alias, USDA_symbol) %>% 
  arrange( standard_binomial)

write_csv(out, '~/Desktop/species_symbols.csv')