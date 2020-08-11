## Preliminary analyses for North America
library(dplyr)
library(ggplot2)
library(invacost)

# Bring in invacost data
#invacost <- read.csv("/Users/robcrystalornelas/Desktop/research/INVACOST_NorthAmerica/invacost_north_america/data/invacost_database_2-1.csv", header = TRUE)
data(invacost) # Emma: I'm importing directly from the new invacost package

# Subset only the observations in North America
north_america_non_expanded <-
  invacost[invacost$Official_country %in% c(
    "Mexico",
    "Mexico/USA",
    "USA",
    "Cuba",
    "Canada",
    "Canada/USA",
    "Guam",
    "Hawaii",
    "Puerto Rico",
    "Mariana",
    "Caribbean"
  ), ]

# First, if we want to do any temporal trend analyses, we've got to separate out impacts for each year they occurred
expanded <- expandYearlyCosts(north_america_non_expanded,
                              startcolumn = "Probable_starting_year_low_margin",
                              endcolumn = "Probable_ending_year_low_margin")

# Then, only retain impacts that happened between 1960 and 2017
expanded<-expanded %>% filter(Impact_year <= "2017")
expanded<-expanded %>% filter(Impact_year >= "1960")
dim(expanded)
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]

# Now, move costs into "billions of dollars" unit
expanded$cost_bil <- (expanded$cost/1000000000)

# First, make sure we retain only "country scale" data
expanded_country <- expanded[expanded$Spatial_scale2 %in% c("Country"),]
# This is the totoal economic impact from 
sum(expanded_country$cost_bil)

# Keep only measurements that are "observed" and "high quality"
expanded_observed_and_country <- expanded_country[expanded_country$Implementation %in% c("Observed"),]
expanded_observed_and_high_and_country <- expanded_observed_and_country[expanded_observed_and_country$Method_reliability %in% c("High"),]

sum(expanded_observed_and_high_and_country$cost_bil)