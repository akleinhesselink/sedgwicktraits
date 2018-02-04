library(tidyverse)
library(stringr)

dat <- read.csv('data/non_plot_leaf_data.csv')

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
  ggplot(non_plot_dat, aes(x = species, y = SLA)) + 
  geom_point() + 
  theme(axis.text = element_text(angle = 90))

#---------------------------------------------------------------
ggWetDry %+% dat 

ggLDMC %+% dat

ggSLA %+% (dat %>% filter( plot == 'non_plot', Slice != 'lomu-p3-l1'))
# 

ggSLA %+% 
  (dat %>% 
     filter( !(Slice == 'lomu-p3-l1')) %>% 
     group_by( species, plant_number ) %>% 
     summarise( SLA = mean(SLA, na.rm = T) )) + geom_boxplot(alpha = 0.2)

