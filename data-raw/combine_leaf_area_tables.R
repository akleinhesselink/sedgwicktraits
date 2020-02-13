rm(list = ls())

library(tidyverse)
library(sedgwickspecies)
library(lubridate)

outfile <- 'data-raw/cleaned_trait_data/clean_leaf_area.csv'

# inputs -------------------------------------- # 

file_names <- dir(path = 'data-raw/all-scans', 
                  pattern = '(*leaf_area.csv$)|(*leaf_area.*petiole.csv$)', 
                  recursive = 'T', 
                  full.names = T) 

alias <- read_csv('data-raw/alias.csv')

files <- data.frame(file_name = file_names)

files <- 
  files %>% 
  mutate( petiole = str_detect('with_petiole', string = file_name)  ) %>% 
  mutate( petiole = ifelse( str_detect('no_petiole', string = file_name), F, petiole)) %>% 
  mutate( raw_date = str_extract(string = file_names, '[0-9]{8}')) %>% 
  mutate( date = dmy( raw_date))

dat <- list()

for ( i in 1:nrow(files)){ 
  if( str_detect(files$file_name[i], 'csv$' )){
    temp <- read_csv(as.character(files$file_name[i]))
  }else if( str_detect(as.character(files$file_name[i]), 'xls$')){
    temp <- read_tsv(as.character( files$file_name[i] ))
  }
  temp$petiole <- files$petiole[i]
  temp$date <- files$date[i]
  temp$file <- files$file_name[i]
  dat[[i]] <- temp 
}

leaf_area <- do.call(bind_rows, dat)

leaf_area <- 
  leaf_area %>% 
  mutate( Slice = str_remove(Slice, "\\'")) %>%
  #mutate( Slice = ifelse( str_detect(Slice, 'LUBI') & date == ymd('2019-04-24'), paste0( Slice, '_all'), Slice) ) %>% 
  mutate( Slice = str_replace(Slice, 'capy-p5.1', 'capy-p9')) %>% # duplicate capy plant numbers
  mutate( Slice = ifelse( Slice == 'lomu-p3-l1', 'lomu-p1-all', Slice)) %>%   
  mutate( new_slice = str_remove(Slice , '\\.(jpeg)|(jpg)$')) %>% 
  mutate( plot = str_extract(new_slice, '^(7[0-9]{2})|(comp)')) %>% 
  mutate( new_slice = str_replace(new_slice, '^(7[0-9]{2}(-?))|(comp(-?))', '')) %>%
  mutate( new_slice = str_replace(new_slice, 'vulpia_myuros', 'vumy')) %>% 
  separate( new_slice, c('alias', 'plant_raw', 'leaf_raw', 'special'), remove = F)  %>% 
  mutate( alias = str_to_upper(alias)) %>% 
  mutate( plant = as.numeric( str_extract(plant_raw, '\\d+') ), 
          leaf = as.numeric(str_extract(leaf_raw, '\\d+'))) %>% 
  mutate( type = ifelse(is.na(leaf), 'all', 'leaf')) %>%
  mutate( bracts = str_detect(special, 'bracts')) %>% 
  mutate( bracts = ifelse( is.na(bracts), F, bracts)) %>% 
  rename( 'count' = Count, 'total_area' = `Total Area`) %>% 
  mutate( plot = ifelse( is.na(plot), 'non_plot', plot )) %>%
  mutate( plot = ifelse( date > '2019-12-01', 'UCLA', plot)) %>%
  left_join( alias ) %>% 
  select( plot, date, USDA_symbol, type, plant, leaf, count, total_area, petiole, bracts, special, Slice)  


# add in information on partial canopy scans: 

partials <- 
  read_csv('data-raw/raw_trait_data/partial_canopy_scans.csv') %>% 
  mutate( date = mdy(date) )  %>% 
  mutate( plant = as.numeric(plant))


leaf_area <- 
  leaf_area %>% 
  left_join(partials %>% select( -notes) ) %>% 
  mutate( proportion = ifelse( is.na(proportion), 1, proportion)) 

# Aggregate partial scans of same plant -------------------- # 

leaf_area <- 
  leaf_area %>% 
  mutate( total_area = total_area/proportion ) %>% 
  group_by( plot, date, USDA_symbol, type, plant, leaf, petiole, bracts ) %>% 
  summarise( total_area = sum( total_area ), 
             count = sum(count) , 
             n_scans = n())

# write -------------------------------------- # 
leaf_area %>% 
  write_csv(outfile)

# correct mislabeled slices ------------------------------------------
# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = ifelse( Slice == 'lomu-p3-l1', 'lomu-p1-all', Slice))  

# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_remove(Slice, '(-big$)|(-small$)')) %>% 
#   mutate( Slice = str_remove(Slice, 'comp-')) # Fix comp-chpa 

# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_replace( Slice, 'vulpia_myuros', 'vumy' ))

# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_replace( Slice, 'all-[1-2]$', 'all')) # two part scans 

# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_remove(Slice, "\\'")) %>%
#   mutate( Slice = str_replace(Slice, 'capy-p5.1', 'capy-p9')) # duplicate capy plant numbers

# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_replace(Slice, 'AVBA.[pP]1.[allALL]{3}.*$', 'AVBA-p1-ALL')) 
# 
# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_remove(Slice, '-b$'))
# 
# leaf_area <- 
#   leaf_area %>%
#   mutate( Slice = str_remove(Slice, '_[a-b]$')) %>% 
#   mutate( Slice = ifelse( str_detect(Slice, 'LUBI') & date == ymd('2019-04-24'), paste0( Slice, '_all'), Slice) ) 
# 
# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_remove( Slice, '-((bracts)|(leaves))$')) # for CLPE bracts and leaves 
# 
# # ---------------------------------------------------------------------

# leaf_area <- 
#   leaf_area %>% 
#   mutate( Slice = str_to_upper(Slice)) %>% 
#   mutate( Slice = str_remove(Slice, '\\.JP[E]?G$')) %>% 
#   mutate( Slice = str_replace(Slice, 'LVS$', 'ALL'))  %>% 
#   mutate( plot = str_extract(Slice, '(^7[0-9]+)')) %>% 
#   mutate( plot = ifelse( is.na(plot), 'non_plot', plot)) %>% 
#   mutate( Slice = str_remove(Slice, '(^7[0-9]+)-')) %>% 
#   separate( Slice, c('alias', 'plant', 'leaf'), remove = F)  %>% 
#   mutate( all = str_detect( leaf, 'ALL'), petiole = str_detect(notes, 'with_petiole')) %>% 
#   mutate( plant = str_extract(plant, '\\d+'), leaf = str_extract(leaf, '\\d+')) %>% 
#   rename( 'count' = Count, 'total_area' = `Total Area`) %>% 
#   mutate( plot = ifelse( date > ymd( '2019-12-01') , 'UCLA', plot )) %>% 
#   left_join( alias ) %>% 
#   select( plot, date, USDA_symbol, all, plant, leaf, count, total_area, petiole, file, notes, Slice)  %>% 
#   distinct() 
# 

