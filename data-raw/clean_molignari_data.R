rm(list = ls())
library(tidyverse)
library(stringr)
library(sedgwickspecies)

outfile <- 'data-raw/clean_molignari_traits.csv'

molignari <- read.csv('data-raw/Molignari_Dryad_Final.csv')

molignari <- molignari %>%
  mutate( standard_binomial = Species.Name) %>%
  mutate( standard_binomial = str_replace_all(standard_binomial, 
                                              c('Avena sp.'='Avena fatua', 
                                                'Dodecatheon clevlandii'='Dodecatheon clevelandii', 
                                                'Eremocarpus setigerus'='Croton setigerus', 
                                                'Filago gallica' = 'Logfia gallica',
                                                'Gastridium ventricosum'='Gastridium phleoides',
                                                'Lessingia filaginifolia'='Corethrogyne filaginifolia', 
                                                'Lolium multiflorum'='Lolium perenne',
                                                'Lotus purshianus' = 'Lotus americanus',
                                                'Nassella pulchra' = 'Stipa pulchra',
                                                'Navarretia jaredii'='Navarretia mitracarpa', 
                                                'Plagiobothrys nothofolvus'='Plagiobothrys nothofulvus',
                                                'Vulpia myuros' = 'Festuca myuros'))) %>% 
  left_join( sedgwick_plants %>% distinct(standard_binomial, USDA_symbol), by = 'standard_binomial') %>% 
  mutate( max_height = Height..m.*100, seed_mass = Seed.Mass..mg./1000, SLA = SLA..cm2.g.)

write_csv( molignari, outfile)
