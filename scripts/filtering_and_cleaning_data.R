## Preliminary analyses for North America
library(dplyr)
library(ggplot2)
library(invacost)

# Bring in invacost data
invacost <- read.csv("/Users/robcrystalornelas/Desktop/research/INVACOST_NorthAmerica/invacost_north_america/data/invacost_database_2-1.csv", header = TRUE)

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
sum(expanded$cost_bil)

# Keep only measurements that are "observed" and "high quality"
expanded_observed <- expanded[expanded$Implementation %in% c("Observed"),]
expanded_observed_and_high <- expanded_observed[expanded_observed$Method_reliability %in% c("High"),]

dim(expanded_observed_and_high)
sum(expanded_observed_and_high$cost_bil)
