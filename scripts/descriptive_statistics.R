library(dplyr)
library(ggplot2)

source("/Users/robcrystalornelas/Desktop/research/INVACOST_NorthAmerica/invacost_north_america/scripts/filtering_and_cleaning_data.R")

# Cost of high and low reliability data
high_reliability <- filter(expanded, Method_reliability == "High")
sum(high_reliability$cost_bil)
  
low_reliability <- filter(expanded, Method_reliability == "Low")
sum(low_reliability$cost_bil)

# Cost of observed and potential data
observed <- filter(expanded, Implementation == "Observed")
sum(observed$cost_bil)

potential <- filter(expanded, Implementation == "Potential")
sum(potential$cost_bil)
