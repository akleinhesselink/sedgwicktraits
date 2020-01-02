rm(list = ls())

library(stringr)
library(tidyverse)
library(sedgwickspecies)

alias <- read_csv('data-raw/alias.csv')
outfile <- 'data-raw/raw_trait_data/leaf_area.csv'

file_names <- dir(path = 'data-raw/all-scans/', pattern = '*.csv$', recursive = 'T', full.names = T) 

files <- data.frame(file_name = file_names)

files <- 
  files %>% 
  mutate( notes = ifelse( str_detect('with_petiole', string = file_name), 'with_petiole', '')  ) %>% 
  mutate( notes = ifelse( str_detect('no_petiole', string = file_name), 'no_petiole', notes)) %>% 
  mutate( raw_date = str_extract(string = file_names, '[0-9]{8}')) %>% 
  mutate( scan_date = as.Date( raw_date, format = '%d%m%Y'))

dat <- list()

for ( i in 1:nrow(files)){ 
  
  temp <- read.csv(as.character( files$file_name[i] ))
  temp$notes <- files$notes[i]
  temp$scan_date <- files$scan_date[i]
  temp$file <- files$file_name[i]
  dat[[i]] <- temp 
}

leaf_area <- do.call(rbind, dat)


leaf_area_test <- 
  leaf_area %>%  
  mutate( Slice = str_to_upper(Slice)) %>% 
  separate(Slice, c('species', 'plant_number', 'leaf'), sep = "_" , remove = F) %>% 
  mutate( plant_number = str_extract(plant_number, '[0-9]+')) %>% 
  mutate( leaf = str_extract(leaf, '[0-9]+')) %>%
  mutate( leaf = ifelse( is.na(leaf), 'all', leaf)) %>% 
  mutate( raw_date = str_extract(string = file, '[0-9]{8}')) %>%
  mutate( scan_date = lubridate::mdy(raw_date)) %>% 
  mutate( all = leaf == 'all') %>% 
  mutate( leaf = ifelse( leaf == 'all', NA, leaf )) %>% 
  mutate( plot = 'non_plot') %>% 
  mutate( alias = toupper(species)) %>% 
  mutate( plant_number = as.numeric(plant_number), leaf = as.numeric(leaf)) %>% 
  rename( "slice" = Slice, "leaf_number" = leaf, "total_area" = Total.Area, 'count' = Count) %>% 
  mutate( petiole = F) %>% 
  left_join(alias, by = 'alias') 

leaf_area_test <- leaf_area_test %>% select( names(read_csv(outfile)))

read_csv(outfile) %>% 
  bind_rows(leaf_area_test) %>% 
  distinct() %>% 
  write_csv(outfile)



