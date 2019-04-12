rm(list = ls())
library(tidyverse)
library(sedgwickspecies)

new_SRL <- read_csv('data-raw/cleaned_trait_data/clean_SRL.csv')
old_SRL <- read_csv('data-raw/old-data/tapioca_raw_traits.csv')

SRL <- rbind(
    old_SRL %>% 
      left_join(sedgwick_plants, by = c('species' = 'prior_code')) %>% 
      mutate(dataset = 'TAPIOCA', plot = 'TAPIOCA') %>% 
      rename("prior_code" = species) %>% 
      select(standard_binomial, prior_code, USDA_symbol, `SRL(m/g)`, plot, dataset, form), 
    
    new_SRL %>% 
      left_join(sedgwick_plants, by = 'USDA_symbol') %>% 
      mutate( dataset = '2017') %>% 
      mutate(`SRL (m/g)` = `SRL (m/g)` ) %>% 
      rename("SRL(m/g)" = `SRL (m/g)`) %>% 
      select(standard_binomial, prior_code, USDA_symbol, `SRL(m/g)`, plot, dataset, form) %>% 
      filter( !is.na(USDA_symbol), !is.na( `SRL(m/g)`)) 
)

plot_SRL <- 
  SRL %>% 
  ggplot( aes( x = USDA_symbol, y = `SRL(m/g)`, color = dataset)) + 
  geom_boxplot(position = position_dodge(width = 1)) + 
  geom_point(position = position_dodge(width = 1)) + 
  coord_flip() + 
  scale_y_continuous(limits = c(0, 600))  + 
  theme_bw() + 
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 8)) 
  
plot_SRL

SRL %>%
  filter(`SRL(m/g)` > 500)

plot_SRL


plot_SRL %+% 
  (SRL %>% 
  group_by(USDA_symbol) %>% 
  filter( n_distinct(dataset) >  1) )

SRL %>% 
  mutate( type = plot=='non_plot') %>% 
  mutate( type = factor( type, labels = c('not_ak', 'ak'))) %>%
  unite('type', c(dataset, type, form), sep = '-') %>%
  ggplot( aes( x = type, y = `SRL(m/g)`)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0)) + 
  theme_bw() + 
  theme(panel.grid = element_blank() ) + 
  coord_flip()


