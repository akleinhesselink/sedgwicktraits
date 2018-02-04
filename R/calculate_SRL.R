rm(list = ls())
library(tidyverse)

weights <- read.csv('data/root_data/root_weights.csv')
lengths <- read.csv('data/root_data/cleaned_root_lengths.csv')

lengths$datetime <- as.POSIXct(lengths$datetime)

weights$plot[is.na(weights$plot)] <- 'non_plot'

root_data <- merge(lengths, weights, by = c('plot', 'species', 'plant_number'))

root_data <- root_data %>% mutate(SRL = total_length_cm/dry_weight)

root_data %>% 
  ggplot( aes(x = species, y = SRL/100)) + 
    geom_point() + 
    geom_boxplot(alpha = 0.2)

write_csv(root_data, 'data/non_plot_root_traits.csv')
