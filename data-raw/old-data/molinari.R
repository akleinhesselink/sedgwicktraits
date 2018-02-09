rm(list = ls())
library(tidyverse)
library(stringr)
library(sedgwickspecies)

url <- 'http://datadryad.org/bitstream/handle/10255/dryad.55089/Dryad_Final.xlsx?sequence=1'
outfile <- 'data-raw/old-data/molinari.xlsx'
outclean <- 'data-raw/old-data/molinari.csv'

if( !file.exists(outfile)){ download.file(url, outfile) } 

molinari <- readxl::read_xlsx(outfile, na = 'NA')

molinari <- 
  molinari %>%
  mutate( standard_binomial = `Species Name`) %>%
  mutate( max_height = `Height (m)`, seed_mass = `Seed Mass (mg)`, SLA = `SLA (cm2/g)`) %>%
  mutate( max_height = max_height*100, seed_mass = seed_mass/1000) %>% 
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
                                                'Vulpia myuros' = 'Festuca myuros')))

molinari <- 
  molinari %>% 
  left_join( sedgwick_plants %>% 
               distinct(standard_binomial, USDA_symbol), by = 'standard_binomial')

molinari <- 
  molinari %>% 
  select( - c( `Species Name`, `SLA (cm2/g)`, `Height (m)`, `Seed Mass (mg)`))

write_csv( molinari, outclean)
