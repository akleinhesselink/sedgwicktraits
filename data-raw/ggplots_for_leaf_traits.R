# functions for plotting leaf traits
library(tidyverse)
library(ggplot2)

ggWetDry <- 
  function(dat){ 
  ggplot(dat , aes( x = dry_mass_g, y = wet_mass_g)) + 
  geom_point() + 
  facet_wrap(~USDA_symbol,scales = 'free') 
}

ggLDMC <- 
  function(dat){ 
  ggplot(dat , aes(x = USDA_symbol, y = LDMC)) + 
  geom_point() + 
  coord_flip()
}

ggSLA <- 
  function( dat ) { 
  ggplot(dat , aes(x = USDA_symbol, y = SLA, color = petiole)) + 
  geom_point(alpha = 0.5) + 
  coord_flip()
}

