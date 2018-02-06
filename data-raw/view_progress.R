library(reshape2)
full_dat <- read.csv("/home/gsk/Dropbox/2017-traits/data/2017-trait-measurements.csv")
full_dat <- full_dat[-which(full_dat[,"plot"] == "non_plot"),]
species_counts <- full_dat[,c("plot", "species")]
species_counts <- species_counts
inds_per_plot <- dcast(species_counts, plot~species)
rownames(inds_per_plot) <- inds_per_plot$plot
inds_per_plot <- inds_per_plot[,-1]
dim(inds_per_plot)
View(inds_per_plot)

# Total
# total individuals
sum(inds_per_plot)/3

# total sp-by-plot hits
sum(colSums((inds_per_plot != 0)))

