#' Plant functional traits for annual plants at UC Sedgwick Reserve.
#'
#' A dataset containing the average functional trait values for 59 species.  
#' Species are identified using the standard USDA code. Functional traits 
#' are the average across several individual plants measured (usually 5-10). 
#' Functional traits were collected for a study comparing functional 
#' trait differences to competitive outcomes across 20 species ("TAPIOCA" data).  
#' Another set of functional traits were collected for a study of trait 
#' differences across 24 different sites from across the reserve ("2017" data). 
#'
#' @format A data frame with 59 rows and 22 variables:
#' \describe{
#'   \item{species}{Latin species binomial for each species. Using the calflora taxonomy for species names.}
#'   \item{leaf_size(cm2)}{average leaf area in cm}
#'   \item{SLA (g/cm2)}{average specific leaf area in g per cm2}
#'   \item{LDMC(mg/g)}{average leaf dry matter content in mg dry weight per g fresh weight}
#'   \item{LAI (LA/canopy_area)}{leaf area index, total leaf area divided by the projected canopy area}
#'   \item{LAR(cm2/g)}{leaf are ratio, total leaf area divided by the total aboveground dry biomass}
#'   \item{seed_mass(g)}{average seed mass in g, average comes from weighing ~100 seeds}
#'   \item{max_height(cm2)}{95 percentile of heights of over 40 measured individual plants}
#'   \item{SRL(m/g)}{specific root length in meters per gram}
#'   \item{relative_spread}{average plant diameter when viewed from above divided by plant height}
#'   \item{phenology (DOY 50\% fruit)}{day at which approximately 50\% of individual plants begin fruiting in the field}
#'   \item{foliar_N}{leaf Nitrogen as a percent of total leaf matter}
#'   \item{CN_ratio}{ratio of leaf carbon to leaf nitrogen}
#'   \item{d13C}{relative enrichment of leaf carbon with carbon-13 isotopes}
#'   \item{d15N}{relative enrichment of leaf nitrogen with nitrogen-15 isotopes}
#'   \item{notes}{notes on data collection}
#'   \item{seed_mass_data_source}{origin of the seed mass data}
#'   \item{max_height_data_source}{origin of the maximum plant height data}
#'   \item{dataset}{whether the species data was collected in 2017 or for the earlier TAPIOCA project}
#'   \item{seed_size (mm3)}{seed volume in mm3}
#'   \item{rooting_depth (oscar)}{average maximum rooting depth}
#'   \item{leaf_ph}{leaf ph}
#' }
"sedgwicktraits"
