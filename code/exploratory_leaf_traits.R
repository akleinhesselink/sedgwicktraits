library(tidyverse)
library(stringr)
dat <- read.csv('data/2017-trait-measurements.csv')

dat <- dat %>% mutate( LDMC =  dry_mass_g / wet_mass_g )

# Plot functions----------------------------------------------- 

ggWetDry <- 
  ggplot(dat, aes( x = dry_mass_g, y = wet_mass_g)) + 
  geom_point() + 
  facet_wrap(~species,scales = 'free')

ggLDMC <- 
  ggplot(dat, aes(x = species, y = LDMC)) + 
  geom_point() +
  theme(axis.text = element_text(angle = 90))

ggSLA <- 
  ggplot(dat, aes(x = species, y = SLA)) + 
  geom_point() + 
  theme(axis.text = element_text(angle = 90))

#---------------------------------------------------------------
ggWetDry %+% (dat %>% filter( plot != 'non_plot'))

ggWetDry %+% (dat %>% filter( plot == 'non_plot'))

ggLDMC %+% (dat %>% filter( plot != 'non_plot'))

ggLDMC %+% (dat %>% filter( plot == 'non_plot'))

# 
la_dat <- read.csv('data/non-plot_leaf_area_data.csv')
la_dat <- la_dat %>% rename(plant_number = plant, species = Species, leaf_number= leaf)
la_dat$date <- as.Date(la_dat$date)
dat$date <- as.Date( dat$date_collected, '%d-%B-%y')
dat <- dat %>% mutate( plant_number = as.character(plant_number ))

la_dat <- la_dat %>% 
  mutate( plant_number = str_extract(plant_number, '\\d+'), 
          leaf_number = str_extract(leaf_number, '\\d+'))
la_dat$species <- toupper(la_dat$species)
dat$species <- toupper(dat$species)

unique(dat$species[ dat$species %in% la_dat$species ])
unique(dat$species[ !dat$species %in% la_dat$species])
unique(la_dat$species[ !la_dat$species %in% dat$species])

toupper(la_dat$species)
unique( la_dat$species )

test <- dat %>% filter( plot == 'non_plot') %>% left_join(la_dat, by = c('species', 'plant_number', 'leaf_number', 'date'))

test$SLA <- test$Total.Area/test$dry_mass_g

ggSLA %+% test + ylim(0, 1000)
test[ !is.na(test$SLA) & test$SLA > 4000 , ] 
test %>% filter( species == 'VULPIA_MYUROS')
