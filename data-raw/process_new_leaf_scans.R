rm(list = ls())

library(stringr)
library(tidyverse)
library(sedgwickspecies)


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
  separate(Slice, c('Species', 'plant', 'leaf'), sep = "_" ) %>% 
  mutate( leaf2 =  str_extract(file, '(?<=L)[0-3]')) %>% 
  mutate( leaf = ifelse( str_detect(leaf, 'lvs.jpeg'), leaf2, leaf)) %>% 
  mutate( leaf = ifelse( !str_detect( leaf, pattern = '[0-9]+'), 'all', leaf)) %>% 
  mutate( leaf = ifelse( is.na(leaf), 'all', leaf)) %>% 
  mutate( raw_date = str_extract(string = file, '[0-9]{8}')) %>% 
  mutate( date = lubridate::mdy(raw_date)) %>% 
  select( Species, plant, leaf, Total.Area, notes, scan_date, file, date)
  
