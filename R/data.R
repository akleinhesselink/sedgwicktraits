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
#'   \item{leaf_size}{area of a single leaf (cm\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}})}
#'   \item{SLA}{specific leaf area (g per cm\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}})}
#'   \item{LDMC}{leaf dry matter content per fresh weight (mg dry per g fresh)}
#'   \item{LAI}{leaf area index, total leaf area divided by the projected canopy area (unitless)}
#'   \item{LAR}{leaf are ratio, total leaf area divided by the total aboveground dry mass (cm\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}}/g)}
#'   \item{seed_mass}{seed mass (g), average comes from weighing ~100 seeds}
#'   \item{max_height}{95 percentile of plant heights (cm), percentiles come from 40 or more individual plants measured in the field}
#'   \item{SRL}{specific root length (m per g)}
#'   \item{relative_spread}{plant diameter when viewed from above divided by plant height (unitless)}
#'   \item{phenology}{day of year at which approximately 50\% of individual plants begin fruiting in the field (doy)}
#'   \item{foliar_N}{leaf Nitrogen as a percent of total leaf mass (\%)}
#'   \item{CN_ratio}{ratio of leaf carbon to leaf nitrogen (unitless)}
#'   \item{d13C}{relative enrichment of leaf carbon with carbon-13 isotopes (\eqn{\delta} 13 C \out{&#8240})}
#'   \item{d15N}{relative enrichment of leaf nitrogen with nitrogen-15 isotopes (\eqn{\delta} 15 N \out{&#8240}) }
#'   \item{notes}{notes on data collection}
#'   \item{seed_mass_data_source}{origin of the seed mass data, some was collected in 2017, some collected for TAPIOCA and some is from reported values in Molinari et al.}
#'   \item{max_height_data_source}{origin of the maximum plant height data}
#'   \item{dataset}{whether the species data was collected in 2017 or for the earlier TAPIOCA project}
#'   \item{seed_size}{seed volume (mm\ifelse{html}{\out{<sup>3</sup>}}{\eqn{^3}})}
#'   \item{rooting_depth}{average maximum rooting depth collected by Godoy for TAPIOCA}
#'   \item{leaf_ph}{leaf pH}
#' }
"sedgwicktraits"
