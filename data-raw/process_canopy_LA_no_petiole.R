rm(list = ls())
library(tidyverse)
library(stringr)

outfile <- 'data-raw/raw_trait_data/canopy_LA_petiole_deleted.csv'

files <- dir('data-raw/canopy_LA_no_petiole', pattern = 'leaf_area_no_petiole', full.names = T)
alias <- read_csv('data-raw/alias.csv')

scan_dates <- unlist( str_extract_all(files, '[0-9]+-[0-9]+-[0-9]+'))
areas <- lapply( files, read_tsv)
areas <- do.call(bind_rows, areas )

areas %>% 
 separate(Slice, c('alias', 'plant_number', 'leaf_number'), sep = '-') %>% 
  select( alias, plant_number, leaf_number, `Total Area`) %>% 
  mutate( alias = toupper(alias)) %>% 
  mutate( leaf_number = str_remove(leaf_number, '\\.jpg$')) %>%
  mutate( plant_number = str_extract(plant_number, '\\d+')) %>% 
  mutate( scan_date = lubridate::dmy( factor( alias, labels = unique(scan_dates) ) )) %>% 
  spread( leaf_number, `Total Area`) %>% 
  mutate( canopy_LA = all + l1 + l2 + l3, 
          complete = T, 
          petiole = F) %>% 
  left_join(alias) %>% 
  select( USDA_symbol, plant_number, petiole, scan_date, canopy_LA, complete ) %>% 
  write_csv(outfile)



