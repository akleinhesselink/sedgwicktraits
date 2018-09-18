library(tidyverse)

isotopes <- read_csv('data-raw/leaf_isotopes_2016.csv')

isotopes %>% head

isotopes %>% ggplot( aes( x = species, y = percent_C/percent_N)) + geom_point() + coord_flip()

isotopes %>% ggplot( aes( x = species, y = `d 13C (‰)`)) + geom_point() + coord_flip()

isotopes %>% ggplot( aes( x = percent_C/percent_N, y = `d 13C (‰)`)) + geom_point()

isotopes %>% ggplot( aes( x = percent_C/percent_N, y = `d 15N (‰)`)) + geom_point()

isotopes %>% ggplot( aes( x = sample_mass, y = `d 13C (‰)`)) + geom_point()
isotopes %>% ggplot( aes( x = sample_mass, y = `d 15N (‰)`)) + geom_point()

isotopes %>% ggplot( aes( x = sample_mass, y = percent_C/percent_N)) + geom_point() 
