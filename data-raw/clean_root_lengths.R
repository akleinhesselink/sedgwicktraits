rm(list = ls())
library(tidyverse)
library(stringr)

# ------------------------------------------------------ 
out_file <- 'data-raw/root_data/clean_root_lengths.csv'

root_files <- dir('data-raw/root_data', 'root-lengths', full.names = T)
alias <- read_csv('data-raw/alias.csv')
# ------------------------------------------------------


# function definition ---------------------------------------------------------------------- # 
process_root_data <- function(file_path) { 
  require(dplyr)
  root_table <- read.table(file_path, fill = T, header = T, sep = "\t")
  root_table <- root_table[-c(1:4),]
  
  root_table %>% mutate(length_over_2 = rowSums(.[c("X2.0000000..L...2.5000000","X2.5000000..L...3.0000000",
                                                    "X3.0000000..L...3.5000000", "X3.5000000..L...4.0000000",
                                                    "X4.0000000..L...4.5000000", ".L..4.5000000")])) %>% 
    select(id = RHIZO.2016a, timestamp = Analysis.Date.Time, 
           total_length_cm = Length.cm., length_0_to_.5 = X0..L...0.5000000,
           length_.5_to_1 = X0.5000000..L...1.0000000, length_1_to_1.5 = X1.0000000..L...1.5000000,
           length_1.5_to_2 = X1.5000000..L...2.0000000, length_over_2) %>%
    mutate(confirm_additions = ifelse(round(total_length_cm) == round(rowSums(.[4:7])), 
                                      TRUE, FALSE))
}


root_data <- lapply( root_files, process_root_data )
root_data <- do.call(rbind, root_data)
extract <- str_extract_all(root_data$id, '\\w+')
plot_species_plant <- lapply( lapply( extract, rev), function(x) x[1:3])
plot_species_plant <- data.frame(do.call(rbind, plot_species_plant))
names(plot_species_plant) <- c('plant_number', 'species', 'plot')
root_data <- cbind( plot_species_plant, root_data )

root_data$plot <- as.character( root_data$plot)
root_data$plot[is.na(root_data$plot)] <- 'non_plot'
root_data$species <- toupper(root_data$species)
root_data$datetime <- as.POSIXct(as.character(root_data$timestamp), format = '%m/%d/%Y %H:%M:%S')

out <- root_data %>% 
  select(plot, species, plant_number, total_length_cm, id, datetime)

out <- 
  out %>% 
  rename('alias' = species) %>% 
  left_join( alias, by = 'alias') 

write.csv(out, out_file , row.names = F)
