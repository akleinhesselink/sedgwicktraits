## Prelim analysis of 2017 trait data
## Last updated 1 May 2017, Gaurav Kandlikar

# setup -----------
library(dplyr); library(ggplot2); library(stringr); library(reshape2)
# read in current 2017 data
tr17 <- read.csv("~/Dropbox/2017-traits/data/2017-trait-measurements.csv")

# read in 2012 trait data
tr12 <- read.csv("~/Dropbox/2017-traits/misc/old-data/tapioca_traits.csv")
tr12$species <- str_replace(tr12$species, "SACA", "SACO")
# for now, subset 2017 data to just the following species:
# PLER, LACA, AMME, MEPO, AGHE, SACO, EUPE

tr17 <- tr17 %>% 
  filter(species %in% c("pler", "laca", "amme", "mepo", "saco", "eupe", "aghe", "homu")) %>% 
  droplevels()

tr17 <- tr17 %>%
 filter(plot %in% c(740, 741, 742, 743, 744, 745, 746, 749, 753, 754, "comp")) %>% droplevels()

tr12 <- tr12 %>% 
  filter(species %in% c("PLER", "LACA", "AMME", "MEPO",  "SACO", "EUPE")) %>% 
  droplevels()

# read in leaf areas csv file and merge it in with the tr17 dataframe -----
leafareas17 <- read.csv("~/Dropbox/2017-traits/data/leaf_areas.csv")
leafareas17$leaf_number <- as.factor(leafareas17$leaf_number)
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
tr17 <- right_join(x = tr17, y = leafareas17, by = c("plot", "species", "plant_number", "leaf_number"))

# Go from raw measurements to traits -----------
# LDMC = (leaf dry area in mg)/(leaf fresh mass in g)\
tr17$ldmc <- (tr17$dry_mass_g*1000)/tr17$wet_mass_g
# SLA = cm^2/g
tr17$sla <- (tr17$Total.Area/tr17$dry_mass_g)
tr17$species <- as.factor(tr17$species)






# general exploration of trait ranges----------
# do an exploratory check of whether leaf dimensions correlate with leaf fresh mass
ggplot(tr17, aes(x = leaf_length_cm, y = wet_mass_g)) + geom_point(aes(col = species))
# check that there don't seem to be outliers per species
ggplot(tr17, aes(x = wet_mass_g)) + geom_histogram() + facet_wrap(~species, ncol = 3, scales = "free")
ggplot(tr17, aes(x = ldmc)) + geom_histogram() + facet_wrap(~species, ncol = 3, scales = "free")
ggplot(tr17, aes(x = sla)) + geom_histogram() + facet_wrap(~species, ncol = 3, scales = "free")


# check out the top weighting AMME 
tr17 %>% filter(species == "amme") %>% arrange(-wet_mass_g) %>% head()
# Those seem to be OK... AMME had big leaves in plot 741



# check that 2017 LDMCs are in similar range as old ones...
plot(density(na.exclude(tr17$ldmc)), col = "red", xlim = c(0, 850), main = "Density of leaf LDMC")
lines(density(na.exclude(tr12$ave_LDMC)))
legend("topright", lty = 1, legend = c("2012", "2017"), col = c("black", "red"))


# histograms per species of 2017 functional traits
ggplot(tr17, aes(x = ldmc))+geom_histogram()+facet_wrap(~species, ncol = 3)
ggplot(tr17, aes(x = sla))+geom_histogram()+facet_wrap(~species, ncol = 3)


## NOTE!
# LDMC of three individuals seems to be off but I am not quite sure why.
# Check these three for dry mass soon:
# Plot 744, PLER P1 L1
# Plot 762, AMME P2 L1

# Do some trait comparisons ----------------
 
# but first, do yet more data management!
# summarize 2017 traits to the individual level so that they can be combined with the 
# 2012 traits
tr17a <- tr17 %>% group_by(plot, species, plant_number) %>% 
  summarise(mean_sla = mean(sla), mean_ldmc = mean(ldmc), mean_leafarea = mean(Total.Area)) %>% ungroup()  %>% select(-plot)
tr17a$comp_dens <- "l"

tr12a <- tr12 %>% select(species, plant_number = plant, mean_sla = ave_SLA, mean_ldmc = ave_LDMC, mean_leafarea = aveLeafSize.cm2.)
tr12a$species <- tolower(tr12a$species)
tr12a$comp_dens <- "c"


combined_traits <- rbind(tr17a, tr12a)
combined_traits$species <- as.factor(combined_traits$species)
xlabs <- paste(levels(combined_traits$species),"\n(N_c=",table(tr12a$species),"\nN_l=",table(tr17a$species),")",sep="")
pp <- ggplot(data = combined_traits, aes(x = species, fill = comp_dens))

ldmc_plot <- pp+geom_boxplot(aes(y = mean_ldmc))+scale_x_discrete(labels=xlabs) + scale_y_log10()
sla_plot <- pp+geom_boxplot(aes(y = mean_sla))+scale_x_discrete(labels=xlabs) + scale_y_log10()
la_plot <- pp+geom_boxplot(aes(y = mean_leafarea))+scale_x_discrete(labels=xlabs) + scale_y_log10()
multiplot(ldmc_plot, sla_plot, la_plot, cols = 2)
source("~/grtools/multiplot.R")
