library(tidyverse)
library(cowplot)
library(ggplot2)

source("scripts/filtering_and_cleaning_data.R")

# read in research effort data
research_effort <- read.csv("data/research_effort_north_america.csv", head = TRUE)
research_effort <- research_effort[,c(1,4)]
research_effort
# we are going to use the final column "research effort as expenditure in US$"

unique(expanded_observed_and_high_and_country$Official_country)

# First, sum up all the costs by country
aggregated_country_costs<-aggregate(cost_bil~Official_country,data=expanded_observed_and_high_and_country,FUN="sum")
aggregate(cost_bil~Official_country,data=expanded_observed_and_high_and_country,FUN="length")

# create dataframe using the newly scaled costs
# First, to prepare for joinin costs, and research effort dfs, need to rename
aggregated_country_costs <- rename(aggregated_country_costs, country = Official_country)

# Then, join the data frames
joined_country_and_research_effort <- left_join(aggregated_country_costs, research_effort, by = "country")
head(joined_country_and_research_effort)

# Now, use mutate to scale costs in each country by research effort in each country
costs_scaled_by_research_effort <- mutate(joined_country_and_research_effort, costs_scaled_by_effort = cost_bil / research_effort_as_expenditure_in_R_and_D_in_percent_GDP)
costs_scaled_by_research_effort

country_costs_plot <-
  ggplot(costs_scaled_by_research_effort,
         aes(x = reorder(country, -costs_scaled_by_effort), y = costs_scaled_by_effort))
country_costs_plot <-
  country_costs_plot + geom_bar(stat = "identity", fill = "#453781FF")
country_costs_plot <- country_costs_plot + theme_cowplot()
country_costs_plot <-
  country_costs_plot + ylab("US$ Billions (2017 value)")
country_costs_plot <- country_costs_plot + xlab("Country")
country_costs_plot
country_costs_plot <-
  country_costs_plot + theme(
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
country_costs_plot <-
  country_costs_plot + scale_y_continuous(expand = c(0, 0)) # This text stops bars from floating above x-axis
country_costs_plot
