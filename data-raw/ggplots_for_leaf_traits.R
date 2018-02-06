# functions for plotting leaf traits

ggWetDry <- 
  ggplot(dat, aes( x = dry_mass_g, y = wet_mass_g)) + 
  geom_point() + 
  facet_wrap(~USDA_symbol,scales = 'free')

ggLDMC <- 
  ggplot(dat, aes(x = USDA_symbol, y = LDMC)) + 
  geom_point() +
  theme(axis.text = element_text(angle = 90))

ggSLA <- 
  ggplot(dat, aes(x = USDA_symbol, y = SLA)) + 
  geom_point() + 
  theme(axis.text = element_text(angle = 90))

