rm(list = ls())

# Run all the data processing scripts for the non-plot species
# Output dataframe saved to data/sedgwicktraits.rdata

source( "data-raw/clean_seed_mass.R"  )
source('data-raw/combine_leaf_area_tables.R')
source( "data-raw/clean_leaf_traits.R") 

source( "data-raw/clean_canopy.R") 
source( "data-raw/clean_heights.R")
source( "data-raw/clean_isotope.R")
source( "data-raw/clean_phenology.R")
source( "data-raw/clean_tlp.R")

source( "data-raw/clean_root_lengths.R")
source( "data-raw/clean_SRL.R")

source("data-raw/join_all_traits.R")
source("data-raw/bind_old_and_new.R")
