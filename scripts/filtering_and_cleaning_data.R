## Preliminary analyses for North America
library(dplyr)
library(ggplot2)
library(invacost)

# Bring in invacost data
 # data(invacost) # Importing directly from the new invacost package
 # dim(invacost)
# unique(invacost$Official_country)
#data(invacost) # Importing directly from the new invacost package
invacost<-read.csv('data/usa_clean_and_other_countries_combined.csv') # Jean's cleaned USA + other data

# Subset only the observations in North America
# north_america_non_expanded <-
#   invacost[invacost$Official_country %in% c(
#     "Mexico",
#     "Mexico/USA",
#     "USA", # Possibly remove USA, because we're getting it from other paper
#     "Cuba",
#     "Canada",
#     "Canada/USA"), ]
# unique(north_america_non_expanded$Official_country)
# dim(north_america_non_expanded)
# north_america_non_expanded$Official_country
# class(north_america_non_expanded$Cost_ID)
# # Write this out as a CSV, then combine with invacost
# write.csv(north_america_non_expanded, "data/north_america_non_expanded.csv")

# Import USA & rest of north america data file
north_america_invacost_data <- read.csv("data/usa_clean_and_other_countries_combined.csv", header = TRUE)
dim(north_america_invacost_data)

# First, if we want to do any temporal trend analyses, we've got to separate out impacts for each year they occurred
expanded <- expandYearlyCosts(north_america_invacost_data,
                              startcolumn = "Probable_starting_year_low_margin",
                              endcolumn = "Probable_ending_year_low_margin")
dim(expanded)
# Then, only retain impacts that happened between 1960 and 2017
expanded<-expanded %>% filter(Impact_year <= "2017")
expanded<-expanded %>% filter(Impact_year >= "1960")
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]

# Now, move costs into "billions of dollars" unit
expanded$cost_bil <- (expanded$cost/1000000000)

# Estimates if we include only country + all other scales
sum(expanded$cost_bil)
dim(expanded)
# Retain only "country scale" data
expanded_country <- expanded[expanded$Spatial_scale2 %in% c("Country"),]
# This is the totoal economic impact from invasive species from all country-level data
sum(expanded_country$cost_bil)
dim(expanded_country)

# Keep only measurements that are "observed" and "high quality"
expanded_observed_and_country <- expanded_country[expanded_country$Implementation %in% c("Observed"),]
dim(expanded_observed_and_country)
sum(expanded_observed_and_country$cost_bil)

expanded_observed_and_high_and_country <- expanded_observed_and_country[expanded_observed_and_country$Method_reliability %in% c("High"),]
dim(expanded_observed_and_high_and_country)

# This is the amount of costs that are 
sum(expanded_observed_and_high_and_country$cost_bil)
dim(expanded_observed_and_high_and_country)

write.csv(expanded_observed_and_high_and_country, "neobiota_submission/robust_dataset.csv")
