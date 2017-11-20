## make a list of which plants to prioritize for getting area from ImageJ

## Last updated 18 July 2017

library(dplyr); library(stringr); library(reshape2)
tr17 <- read.csv("~/Dropbox/2017-traits/data/2017-trait-measurements.csv")
leafareas17 <- read.csv("~/Dropbox/2017-traits/data/leaf_areas.csv")
# get rid of the .jpegs...
leafareas17$Name <- str_replace(leafareas17$Name, ".jpeg", "")
# split up the name column so that we can do some merging
leafareas17 <- cbind(leafareas17,colsplit(leafareas17$Name, "-", c("plot", "species", "plant_number", "leaf_number")))
leafareas17$plant_number <- as.numeric(str_replace(leafareas17$plant_number, "p", ""))
leafareas17$leaf_number  <- as.numeric(str_replace(leafareas17$leaf_number, "l", ""))
leafareas17$plot <- as.factor(leafareas17$plot)
leafareas17 <- leafareas17 %>% select(plot, species, plant_number, leaf_number, Total.Area)

str(leafareas17)
str(tr17)
tr17 <- left_join(x = tr17, y = leafareas17, by = c("plot", "species", "plant_number", "leaf_number"))
tr17 <- tr17 %>% select(-leaf_area_cm2)
tr17 <- rename(tr17, leaf_area_cm2 = Total.Area)
get_my_areas <- tr17 %>% filter(!is.na(dry_mass_g), is.na(leaf_area_cm2)) %>% select(plot, species, plant_number, leaf_number, date_collected)
View(get_my_areas)


