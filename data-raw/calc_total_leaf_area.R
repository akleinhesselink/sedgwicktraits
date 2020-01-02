rm(list = ls() )

library(tidyverse)

leaf_area <- read_csv( 'data-raw/raw_trait_data/leaf_area.csv' )

canopy_leaf_area <- 
  leaf_area %>% 
  group_by( year, scan_date, plot, USDA_symbol, plant, petiole) %>%
  summarise( n_leafparts = sum(count), complete = any( all ), leaf_area = sum(total_area)) 



