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
  ggplot(dat , aes(x = USDA_symbol, y = LDMC, shape = petiole, color = (plot == "non_plot"))) + 
  geom_point(alpha = 0.5) + 
  coord_flip()
}

ggSLA <- 
  function( dat ) { 
  ggplot(dat , aes(x = USDA_symbol, y = SLA, shape = petiole, color = (plot == "non_plot"))) + 
  geom_point(alpha = 0.5) + 
  coord_flip()
}

