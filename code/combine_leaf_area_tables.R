rm(list = ls())

library(stringr)
library(dplyr)
library(tidyr)

file_names <- dir(path = 'data', pattern = '*.xls$', recursive = 'T') 

df <- data.frame(file_name = file_names)

df <- df %>% 
  mutate( notes = ifelse( str_detect('with_petiole', string = file_name), 'with_petiole', '')  ) %>% 
  mutate( notes = ifelse( str_detect('no_petiole', string = file_name), 'no_petiole', notes)) %>% 
  mutate( raw_date = str_extract(string = file_names, '[0-9]{8}')) %>% 
  mutate( date = as.Date( raw_date, format = '%d%m%Y'))

dat <- list()

for ( i in 1:nrow(df)){ 

  temp <- read.csv(as.character( df$file_name[i] ), sep = '\t')
  temp$notes <- df$notes[i]
  temp$date <- df$date[i]
  
  dat[[i]] <- temp 
}

leaf_area_dat <- do.call(rbind, dat)

leaf_area_dat$Slice <- str_replace_all(leaf_area_dat$Slice, c('_[Pp]', '_[Ll]'), c('-p', '-l'))
leaf_area_dat$Species <- tolower(str_match( string = leaf_area_dat$Slice, pattern = '^([A-Za-z\\-_]*).[Pp][0-9]')[, 2])
leaf_area_dat$plant <- tolower(str_extract( leaf_area_dat$Slice, pattern = '[Pp][0-9][0-9]?'))
leaf_area_dat$leaf <- tolower(str_extract( leaf_area_dat$Slice, pattern = '[Ll][0-9]{1}+' ))
leaf_area_dat$all <- str_detect(leaf_area_dat$Slice, pattern = '[Aa][Ll]{2}')

scan_index <- read.csv('data/all-scans/scans-03032017/scan_index.csv')

scan_index <- scan_index[, c('scan_id', 'plant', 'species')] 
test <- leaf_area_dat %>% filter( is.na(Species)) %>% mutate( scan_id = str_extract( pattern = '[0-9]+', plant))
test <- merge( test, scan_index, by = 'scan_id')
test <- test %>% mutate( plant = paste0('p', plant.y) ) %>% dplyr::select(-scan_id, -plant.x)


write.csv(leaf_area_dat, 'data/non-plot_leaf_area_data.csv', row.names = F)
