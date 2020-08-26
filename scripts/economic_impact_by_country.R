library(tidyverse)
library(cowplot)
library(ggplot2)

source("scripts/filtering_and_cleaning_data.R")

### New country cost estimates
# sum of costs for each country
aggregated_country_costs <-
  aggregate(cost_bil ~ Official_country, data = expanded_observed_and_high_and_country, FUN =
              "sum")
aggregated_country_costs

# Number of cost estimates per country
aggregate(cost_bil ~ Official_country, data = expanded_observed_and_high_and_country, FUN =
            "length")

# USA data
usa_data <-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Official_country %in% c(
    "USA"),]
sum(usa_data$cost_bil)
dim(plyr::count(unique(usa_data$Reference_ID)))
sum(usa_data$cost_bil)/68 # cost is 18 billion per paper

canada_data <-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Official_country %in% c(
    "Canada"),]
sum(canada_data$cost_bil)
dim(plyr::count(unique(canada_data$Reference_ID)))
sum(canada_data$cost_bil)/8 # cost is 18 billion per paper

mexico_data<-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Official_country %in% c(
    "Mexico"),]
sum(mexico_data$cost_bil)
dim(plyr::count(unique(mexico_data$Reference_ID)))
sum(mexico_data$cost_bil)/8 # cost is 18 billion per paper

cuba_data<-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Official_country %in% c(
    "Cuba"),]
sum(cuba_data$cost_bil)
dim(plyr::count(unique(cuba_data$Reference_ID)))
sum(cuba_data$cost_bil)/8 # cost is 18 billion per paper


# read in research effort data
# research_effort <- read.csv("data/research_effort_north_america.csv", head = TRUE)
# names(research_effort)
# research_effort <- research_effort[,c(1,6)]
# research_effort
# # we are going to use the final column "research effort as expenditure in US$"
# 
# # First, sum up all the costs by country
# 
# aggregated_country_costs <-
#   aggregate(cost_bil ~ Official_country, data = expanded_observed_and_high_and_country, FUN =
#               "sum")
# aggregated_country_costs
# aggregate(cost_bil ~ Official_country, data = expanded_observed_and_high_and_country, FUN =
#             "length")
# 
# # create dataframe using the newly scaled costs
# # First, to prepare for joinin costs, and research effort dfs, need to rename
# aggregated_country_costs <- rename(aggregated_country_costs, country = Official_country)
# 
# # Then, join the data frames
# joined_country_and_research_effort <- left_join(aggregated_country_costs, research_effort, by = "country")
# head(joined_country_and_research_effort)
# 
# # Now, use mutate to scale costs in each country by research effort in each country
# head(research_effort)
# costs_scaled_by_research_effort <- mutate(joined_country_and_research_effort, costs_scaled_by_effort = cost_bil / Research_effort_as_expenditure_in_R_and_D_in_current._USD)
# costs_scaled_by_research_effort
# 
# country_costs_plot <-
#   ggplot(costs_scaled_by_research_effort,
#          aes(x = reorder(country, -costs_scaled_by_effort), y = costs_scaled_by_effort))
# country_costs_plot <-
#   country_costs_plot + geom_bar(stat = "identity", fill = "#453781FF")
# country_costs_plot <- country_costs_plot + theme_cowplot()
# country_costs_plot <-
#   country_costs_plot + ylab("Economic cost / Science research spending (2017 US$)")
# country_costs_plot <- country_costs_plot + xlab("Country")
# country_costs_plot
# country_costs_plot <-
#   country_costs_plot + theme(
#     axis.text = element_text(size = 25),
#     # Change tick mark label size
#     axis.title = element_text(size = 22, face = "bold"),
#     axis.text.x = element_text(
#       angle = 90,
#       hjust = 1,
#       vjust = 0.5
#     ),
#     strip.text = element_text(size = 25)
#   )
# country_costs_plot <-
#   country_costs_plot + scale_y_continuous(expand = c(0, 0)) # This text stops bars from floating above x-axis
# country_costs_plot

