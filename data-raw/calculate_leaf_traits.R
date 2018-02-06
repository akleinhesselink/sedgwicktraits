library(tidyverse)
library(stringr)

dat <- read.csv('data/2017-trait-measurements.csv')
la_dat <- read.csv('data/non-plot_leaf_area_data.csv')

dat <- dat %>% 
  mutate( LDMC =  dry_mass_g / wet_mass_g ) %>% 
  mutate( plant_number = as.character(plant_number )) %>% 
  mutate( species = toupper(species))%>%
  mutate( species = str_replace_all(species, c('LINANTHUS' = 'LESP', 
                                              'VULPIA_MYUROS' = 'VUMY',
                                              'MICROSERIS' = 'MIDO',
                                              'CROTON SETIGERUS' = 'CRSE',
                                              'GILIA' = 'GIOC', 
                                              'CARDUUS PYCNOCEPHALA' = 'CAPY',
                                              'CHLOROGALLUM POMERIDIANUM' = 'CHPO',
                                              'CALYSTEGIA COLLINA' = 'CACO', 
                                              'LAGOPHYLLA RAMOSISSIMA' = 'LARA', 
                                              'AG2' = 'AGHE')))

dat$date <- as.character( dat$date_collected)
dat$date  <- lubridate::dmy( dat$date_collected)
dat$date[is.na(dat$date)] <- lubridate::mdy( dat$date_collected[ is.na(dat$date) ])

la_dat <- la_dat %>% 
  rename(plant_number = plant, species = Species, leaf_number= leaf) %>% 
  mutate( date = lubridate::as_date( date )) %>% 
  mutate( plant_number = str_extract(plant_number, '\\d+'), 
          leaf_number = str_extract(leaf_number, '\\d+')) %>%
  mutate(species = toupper(species)) %>% 
  mutate( species = str_replace_all(species, c('LEPTOSIPHON' = 'LESP', 
                                               'VULPIA_MYUROS' = 'VUMY', 
                                               'MICROSERIS' = 'MIDO', 
                                               'CROTON' = 'CRSE', 
                                               'GILIA' = 'GIOC')))

dat <- dat %>% 
  left_join(la_dat, by = c('species', 'plant_number', 'leaf_number', 'date')) %>% 
  mutate( SLA = Total.Area/test$dry_mass_g)

# output -------------------------------------------------------- 

write_csv(dat %>% filter( plot == 'non_plot'), 'data/non_plot_leaf_data.csv')



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

ggSLA %+% (dat %>% filter( plot == 'non_plot', Slice != 'lomu-p3-l1'))
# 

ggSLA %+% 
  (dat %>% 
     filter( !(Slice == 'lomu-p3-l1')) %>% 
     group_by( species, plant_number ) %>% 
     summarise( SLA = mean(SLA, na.rm = T) )) + geom_boxplot(alpha = 0.2)

