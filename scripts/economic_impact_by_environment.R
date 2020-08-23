library(dplyr)
library(ggplot2)
library(cowplot)

source("scripts/filtering_and_cleaning_data.R")

# Here are the data that we need: both highly reliable and directly observed measurements
head(expanded_observed_and_high_and_country)

unique(expanded_observed_and_high_and_country$Environment)

# Make a table of the  costs grouped by environment
aggregated_environment_costs <- aggregate(cost_bil~Environment,data=expanded_observed_and_high_and_country,FUN="sum")
aggregated_environment_costs
plyr::count(expanded_observed_and_high_and_country$Environment)
enviro_costs_plot <-
  ggplot(aggregated_environment_costs,
         aes(x = reorder(Environment, -cost_bil), y = cost_bil))
enviro_costs_plot <-
  enviro_costs_plot + geom_bar(stat = "identity", fill = "#29AF7FFF")
enviro_costs_plot <- enviro_costs_plot + theme_cowplot()
enviro_costs_plot <-
  enviro_costs_plot + ylab("US$ Billions (2017 value)")
enviro_costs_plot <- enviro_costs_plot + xlab("Environment")
enviro_costs_plot
enviro_costs_plot <-
  enviro_costs_plot + theme(
    axis.text = element_text(size = 25),
    # Change tick mark label size
    axis.title = element_text(size = 25, face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    strip.text = element_text(size = 25)
  )
enviro_costs_plot <-
  enviro_costs_plot + scale_y_continuous(expand = c(0, 0)) # This text stops bars from floating above x-axis
enviro_costs_plot


# Some other ways of identifying costs
aggregate(cost_bil~Environment,data=expanded_observed_and_high_and_country,FUN="sum")
aggregate(cost_bil~Environment,data=expanded_observed_and_high_and_country,FUN="length")
