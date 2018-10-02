rm(list = ls())

library(stringr)
library(tidyverse)
library(sedgwickspecies)

outfile <- 'data-raw/leaf_area.csv'

file_names <- dir(path = 'data-raw/all-scans', pattern = '*.xls$', recursive = 'T', full.names = T) 
alias <- read_csv('data-raw/alias.csv')

files <- data.frame(file_name = file_names)

files <- 
  files %>% 
  mutate( notes = ifelse( str_detect('with_petiole', string = file_name), 'with_petiole', '')  ) %>% 
  mutate( notes = ifelse( str_detect('no_petiole', string = file_name), 'no_petiole', notes)) %>% 
  mutate( raw_date = str_extract(string = file_names, '[0-9]{8}')) %>% 
  mutate( scan_date = as.Date( raw_date, format = '%d%m%Y'))

dat <- list()

for ( i in 1:nrow(files)){ 

  temp <- read.csv(as.character( files$file_name[i] ), sep = '\t')
  temp$notes <- files$notes[i]
  temp$scan_date <- files$scan_date[i]
  temp$file <- files$file_name[i]
  dat[[i]] <- temp 
}

leaf_area <- do.call(rbind, dat)

leaf_area$Slice <- str_replace_all(leaf_area$Slice, c('_[Pp]' = '-p', '_[Ll]' = '-l'))

# correct mislabeled slices ------------------------------------------
leaf_area <- 
  leaf_area %>% 
  mutate( Slice = ifelse( Slice == 'lomu-p3-l1', 'lomu-p1-all', Slice))  

leaf_area <- 
  leaf_area %>% 
  filter( !str_detect( Slice, 'avfa-p7-l3-\\d$'))

# remove slices with no area ------------------------------------------

leaf_area <- 
  leaf_area %>% 
  filter( X.Area > 0 )

# ---------------------------------------------------------------------

leaf_area$Species <- str_extract( leaf_area$Slice, '^[A-Za-z]{2,}[1-9]?(?=[\\-_]{1})')
leaf_area$plant <- tolower(str_extract( leaf_area$Slice, pattern = '[Pp][0-9][0-9]?'))
leaf_area$leaf <- tolower(str_extract( leaf_area$Slice, pattern = '[Ll][0-9]{1}+' ))

leaf_area <- 
  leaf_area %>% 
  mutate( leaf = ifelse( is.na(leaf), tolower( str_extract(file, '(?<=l)\\d+(?=.xls)')), leaf))


leaf_area <- 
  leaf_area %>% 
  mutate(all = is.na(leaf)) %>% 
  select( Slice, Species, plant, leaf, all, Count, Total.Area, scan_date, notes) %>% 
  rename( 'slice' = Slice, 'count' = Count, 'total_area' = Total.Area, 'alias' = Species) %>% 
  mutate( leaf = str_extract(leaf, '\\d+'), plant = str_extract(plant, '\\d+'))

leaf_area <- 
  leaf_area %>% 
  mutate( alias = toupper(alias)) %>% 
  left_join( alias, by = 'alias') %>% 
  mutate( petiole = ifelse(notes == 'with_petiole', T, F))

leaf_area <- 
  leaf_area %>% 
  rename(plant_number = plant, leaf_number= leaf) %>% 
  mutate( scan_date = lubridate::as_date( scan_date )) %>% 
  mutate( plant_number = str_extract(plant_number, '\\d+'), 
          leaf_number = str_extract(leaf_number, '\\d+')) %>% 
  mutate( plot = 'non_plot')

leaf_area %>% 
  write_csv(outfile)

