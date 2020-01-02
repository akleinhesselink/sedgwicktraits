rm(list = ls())

library(stringr)
library(tidyverse)
library(sedgwickspecies)
library(lubridate)

outfile <- 'data-raw/cleaned_trait_data/clean_leaf_area.csv'

file_names <- dir(path = 'data-raw/all-scans', 
                  pattern = '(*.xls$)|(*.csv$)', 
                  recursive = 'T', 
                  full.names = T) 

alias <- read_csv('data-raw/alias.csv')

files <- data.frame(file_name = file_names)

files <- 
  files %>% 
  mutate( notes = ifelse( str_detect('with_petiole', string = file_name), 'with_petiole', '')  ) %>% 
  mutate( notes = ifelse( str_detect('no_petiole', string = file_name), 'no_petiole', notes)) %>% 
  mutate( raw_date = str_extract(string = file_names, '[0-9]{8}')) %>% 
  mutate( date = dmy( raw_date))

dat <- list()

for ( i in 1:nrow(files)){ 
  if( str_detect(files$file_name[i], 'csv$' )){
    temp <- read_csv(as.character(files$file_name[i]))
  }else if( str_detect(as.character(files$file_name[i]), 'xls$')){
    temp <- read_tsv(as.character( files$file_name[i] ))
  }
  temp$notes <- files$notes[i]
  temp$date <- files$date[i]
  temp$file <- files$file_name[i]
  dat[[i]] <- temp 
}

leaf_area <- do.call(rbind, dat)

leaf_area$Slice <- str_replace_all(leaf_area$Slice, c('_[Pp]' = '-p', '_[Ll]' = '-l'))

# correct mislabeled slices ------------------------------------------
leaf_area <- 
  leaf_area %>% 
  mutate( Slice = ifelse( Slice == 'lomu-p3-l1', 'lomu-p1-all', Slice))  

# ---------------------------------------------------------------------

leaf_area <- 
  leaf_area %>% 
  mutate( Slice = str_to_upper(Slice)) %>% 
  mutate( Slice = str_remove(Slice, '\\.JP[E]?G$')) %>% 
  mutate( Slice = str_replace(Slice, 'LVS$', 'ALL'))  %>% 
  separate( Slice, c('alias', 'plant', 'leaf'))  %>% 
  mutate( all = str_detect( leaf, 'ALL'), petiole = str_detect(notes, 'with_petiole')) %>% 
  mutate( plant = str_extract(plant, '\\d+'), leaf = str_extract(leaf, '\\d+')) %>% 
  rename( 'count' = Count, 'total_area' = `Total Area`) %>% 
  mutate( plot = 'non_plot' ) %>% 
  mutate( plot = ifelse( date > ymd( '2019-12-01') , 'UCLA', plot )) %>% 
  left_join( alias ) %>% 
  select( plot, date, USDA_symbol, all, plant, leaf, count, total_area, petiole, file, notes)  %>% 
  distinct() 

leaf_area %>% 
  write_csv(outfile)

