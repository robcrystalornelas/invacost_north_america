# Set working directory
# setwd()

# Load libraries
source("scripts/filtering_and_cleaning_data.R")
library(invacost)
library(cowplot)

# Here is the full dataset we're working with
head(expanded_observed_and_high_and_country)
dim(expanded_observed_and_high_and_country)

# Plot of average costs over time with general plot functions
raw_costs_north_america <- calculateRawAvgCosts(expanded_observed_and_high_and_country,
                                  maximum.year = 2017)

plot_north_america_over_time <- plot(raw_costs_north_america,
                              graphical.parameters = "manual")
plot_north_america_over_time <- plot_north_america_over_time + xlab("Year") +
  ylab("Average annual cost of invasions in US$ millions") +
  scale_x_continuous(breaks = raw_costs_north_america$year.breaks) + # X axis breaks
  theme_cowplot() + # Minimal theme
  scale_y_log10(breaks = 10^(-15:15), # y axis in log 10 with pretty labels
                labels = scales::comma) +
  annotation_logticks(sides = "l") +
  ggtitle("North America")+
  theme(plot.title = element_text(hjust = 0.5))

plot_north_america_over_time

## Now get costs per year by country

north_america_trend <- costTrendOverTime(
  expanded_observed_and_high_and_country,
  minimum.year = 1970, 
  maximum.year = 2017, # Last year we are considering
  incomplete.year.threshold = 2015, # We say that all years from 2015 on are incomplete 
  incomplete.year.weights = NULL)

plot(north_america_trend)
north_america_trend$model.summary
prettySummary(north_america_trend)
