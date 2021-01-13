## Preliminary analyses for North America
library(dplyr)
library(ggplot2)
library(invacost)

# Import the final clean north america dataset
north_america_invacost <- read.csv('data/Crystal-Ornelas_supplementary_file_v2-CD.csv', head = TRUE) 
dim(north_america_invacost) 
unique(north_america_invacost$Official_country)

# When the start/end years got imported, they were switched to factors. Need to switch them back to integers
#north_america_invacost$Probable_starting_year_low_margin <- as.integer(as.character(north_america_invacost$Probable_starting_year_low_margin))
#north_america_invacost$Probable_ending_year_low_margin <- as.integer(as.character(north_america_invacost$Probable_ending_year_low_margin))
# north_america_invacost$Probable_starting_year_low_margin

# Expand the database
expanded <- expandYearlyCosts(north_america_invacost,
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

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

# Keep only measurements that are "observed" and "high quality"
expanded_observed <- expanded[expanded$Implementation %in% c("Observed"),]
dim(expanded_observed)
sum(expanded_observed$cost_bil)

expanded_potential <- expanded[expanded$Implementation %in% c("Potential"),]
dim(expanded_potential)
sum(expanded_potential$cost_bil)

expanded_observed_and_high <- expanded_observed[expanded_observed$Method_reliability %in% c("High"),]
dim(expanded_observed_and_high)

expanded_observed_and_low <- expanded_observed[expanded_observed$Method_reliability %in% c("Low"),]
sum(expanded_observed_and_low$cost_bil)

# This is the amount of costs that are 
sum(expanded_observed_and_high$cost_bil)
dim(expanded_observed_and_high)
write.csv(expanded_observed_and_high, "robust_dataset_v3.csv")
unique(expanded_observed_and_high$Official_country)

