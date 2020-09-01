
# Set working directory
#setwd("")

# Load libraries
source("scripts/filtering_and_cleaning_data.R")
library(invacost)
library(cowplot)

# Here is the full dataset we're working with
head(expanded_observed_and_high_and_country)
unique(expanded_observed_and_high_and_country$Impacted_sector_2)
dim(expanded_observed_and_high_and_country)

# Count up how many entries are in each category
plyr::count(expanded_observed_and_high_and_country$Impacted_sector_2)

aggregated_sector_costs <- aggregate(cost_bil~Impacted_sector_2,data=expanded_observed_and_high_and_country,FUN="sum")
aggregated_sector_costs


