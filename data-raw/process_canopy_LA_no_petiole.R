rm(list = ls())
library(tidyverse)
library(stringr)

files <- dir('~/Desktop/scans_delete_petiole', pattern = 'leaf_area_no_petiole', full.names = T)

scan_dates <- str_extract_all(files, '[0-9]+-[0-9]+-[0-9]+')
areas <- lapply( files, read_tsv)
areas <- do.call(bind_rows, areas )

areas %>% 
 separate(Slice, c('alias', 'plant_number', 'leaf_number'), sep = '-') %>% 
  select( alias, plant_number, leaf_number, `Total Area`) %>% 
  mutate( alias = toupper(alias)) %>% 
  mutate( leaf_number = str_remove(leaf_number, '\\.jpg$')) %>%
  mutate( plant_number = str_extract(plant_number, '\\d+')) %>% 
  mutate( scan_date = as.Date( factor( alias, labels = unique(scan_dates) ) )) %>% 
  spread( leaf_number, `Total Area`) %>% 
  mutate( canopy_LA = all + l1 + l2 + l3, 
          complete = T, 
          petiole = F) %>% 
  left_join(alias) %>% 
  select( USDA_symbol, plant_number, petiole, scan_date, canopy_LA, complete )


