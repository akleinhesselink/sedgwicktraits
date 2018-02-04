# Read in 2017-traits.csv and leaf_areas.csv
# Export individual_level_traits.csv

library(dplyr); library(reshape2); library(stringr); library(tidyr); library(ggplot2); library(cowplot); library(tibble)

field_measurements <- read.csv("~/Dropbox/2017-traits/data/2017-trait-measurements.csv", stringsAsFactors = F)
tbl_df(field_measurements)

# Convert a few of the columns into factors
field_measurements <- field_measurements %>% mutate_at(c(2, 3, 4, 5), funs(factor))
# let's uncapitalize all of the species names ... ugh
field_measurements <- field_measurements %>% mutate(species = tolower(species))

# filter out non-plot species
field_measurements <- field_measurements %>% filter(!(plot == "non_plot"))
# Let's set aside the issue of petioles for now.
field_measurements <- field_measurements %>% mutate(wet_mass_g = rowSums(cbind(wet_mass_g, petiole_mass_g), na.rm = T)) %>% select(-c(petiole_mass_g)) %>%
  mutate(dry_mass_g = rowSums(cbind(dry_mass_g, dry_mass_petiole_g), na.rm = T)) %>% select(-dry_mass_petiole_g)

# Import leaf areas
leaf_areas <- read.csv("~/Dropbox/2017-traits/data/leaf_areas.csv", stringsAsFactors = F)
tbl_df(leaf_areas)

# Let's get rid of the cases where petiole area was done separately- for now, let's do all together.

leaf_areas <- leaf_areas %>% group_by(Name) %>% mutate(Total.Area = sum(Total.Area)) %>%
  filter(!(X == "petiole")) %>% select(-X)

# Evidently there are some non-plot species in this dataframe:

leaf_areas %>% filter(grepl("lebo", Name))

# Let's remove those columns for now

leaf_areas <- leaf_areas %>% filter(!(grepl("lebo", Name)))


# We need to split up the "Name" column of `leaf_areas` before we merge it with the field measurements document. 

leaf_areas <- separate(leaf_areas, Name, sep = "-", into = c("plot", "species", "plant_number", "leaf_number"))


leaf_areas <- leaf_areas %>%  
  select(plot:leaf_number, leaf_area_cm2 = Total.Area) # select the columns we want
leaf_areas <- leaf_areas %>% 
  mutate(plant_number = str_replace(plant_number, "p", "")) %>% # remove the p
  mutate(leaf_number = str_replace(leaf_number, "l", "")) %>% # remove the l
  mutate(leaf_number = str_replace(leaf_number, ".jpeg", "")) %>% # remove the .jpeg
  mutate(leaf_number = str_replace(leaf_number, ".jpg", "")) %>% # remove the .jpg
  mutate_at(c(1:4), funs(factor)) 

merged_measurements <- (left_join(field_measurements, leaf_areas, by = c("plot", "species", "plant_number", "leaf_number")))

merged_measurements <- merged_measurements %>% 
  mutate(ldmc_mg_g = (dry_mass_g*1000)/wet_mass_g) %>%
  mutate(sla_cm2_g = leaf_area_cm2/dry_mass_g)


individual_traits <- merged_measurements %>% group_by(plot, species, plant_number) %>% summarise(leaf_area_cm2 = mean(leaf_area_cm2), ldmc_mg_g = mean(ldmc_mg_g), sla_cm2_g = mean(sla_cm2_g))

# Subset to grasses for Brody Sandel

grasses <- individual_traits %>% filter(species %in% c("vuma", "brma", "homu"))
View(grasses)

