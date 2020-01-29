rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)

setwd('~/Desktop/Sedgwick_leaf_scans/scans-03032017/')
scan_id <- read.csv('scan_index.csv')
dat <- read.csv('new_leaf_area_all_plants.csv')

dat <- dat %>% 
  separate( Slice, c('plant', 'leaf', 'Year', 'month', 'day') )

dat$scan_id <- str_extract( dat$plant, '\\d+')
dat <- merge( dat, scan_id, by = 'scan_id')
dat$plant <- dat$plant.y
dat$leaf [ dat$leaf == 'LVS' ]  <- 'ALL'

dat <- dat %>% 
  select( species, plant, leaf, Count, Total.Area, Average.Size, X.Area, Mean) %>% 
  mutate( plant = paste0( 'P', plant)) %>% 
  unite( 'Slice', c( species,  plant, leaf), sep = '_' ) %>% 
  rename( `Total Area` = Total.Area, `Average Size` = Average.Size, `%Area` = X.Area)

dat <- split( dat, as.factor( str_extract(  dat$Slice, '^[A-Z]{4}')) )

species <- names(dat)

mapply( x = dat, y = species, function(x, y) { write.csv(x, file = paste0(y,'_leaf_area.csv'), row.names = F)})
