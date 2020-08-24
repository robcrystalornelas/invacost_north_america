# Load libraries
source("scripts/filtering_and_cleaning_data.R")
library(invacost)
library(cowplot)

# Plot of average costs over time with general plot functions

north_america_invacost_data$Cost_estimate_per_year_2017_USD_exchange_rate
expanded_observed_and_high_and_country

raw.costs <- calculateRawAvgCosts(expanded_observed_and_high_and_country,
                                  cost.column = "cost",
                                  maximum.year = 2017)
raw.costs

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
