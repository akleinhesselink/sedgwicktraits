rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)

setwd('~/Dropbox/2017-traits/scans-03032017/')
scan_id <- read.csv('scan_index.csv')
files <- dir('.', '*.xls')

dat <- lapply(files, read.csv, sep = '\t')
dat <- do.call( rbind, dat )

dat <- dat %>% separate( Slice, c('plant', 'leaf', 'Year', 'month', 'day') )
dat$scan_id <- str_extract( dat$plant, '\\d+')
dat <- merge( dat, scan_id, by = 'scan_id')
dat$plant <- dat$plant.y
dat$leaf [ dat$leaf == 'LVS' ]  <- 'ALL'

dat <- split( dat, dat$species)
species <- names(dat)

mapply( x = dat, y = species, function(x, y) { write.csv(x, file = paste0(y,'_leaf_area.csv'), row.names = F)})
