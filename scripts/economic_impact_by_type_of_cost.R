library(dplyr)
library(ggplot2)
library(invacost)

source("scripts/filtering_and_cleaning_data.R")

# Spending vs. genera vs. damage-loss
head(expanded_observed_and_high_and_country)
aggregate(cost_bil~Type_2,data=expanded_observed_and_high_and_country,FUN="sum")
aggregate(cost_bil~Type_2,data=expanded_observed_and_high_and_country,FUN="length")
