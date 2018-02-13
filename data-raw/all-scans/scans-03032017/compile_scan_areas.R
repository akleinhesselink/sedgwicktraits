rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)

dir_out <- 'data-raw/all-scans/scans-03032017'
scan_id <- read.csv('data-raw/all-scans/scans-03032017/scan_index.csv')
files <- dir(dir_out, '*.xls', full.names = T)

dat <- lapply(files, read.csv, sep = '\t')
dat <- do.call( rbind, dat )

dat <- dat %>% separate( Slice, c('plant', 'leaf', 'Year', 'month', 'day') )
dat$scan_id <- str_extract( dat$plant, '\\d+')
dat <- merge( dat, scan_id, by = 'scan_id')
dat$plant <- dat$plant.y
dat$leaf [ dat$leaf == 'LVS' ]  <- 'ALL'

dat <- split( dat, dat$species)
species <- names(dat)

mapply( x = dat, y = species, function(x, y) { 
  write.table(x, file = file.path( dir_out, paste0(y,'_leaf_area.xls')), row.names = F, sep = '\t')})
