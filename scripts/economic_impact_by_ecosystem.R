library(dplyr)
library(ggplot2)
library(cowplot)

source("/Users/robcrystalornelas/Desktop/research/INVACOST_NorthAmerica/invacost_north_america/scripts/filtering_and_cleaning_data.R")

# Here are the data that we need: both highly reliable and directly observed measurements
head(expanded_observed_and_high_and_country)

levels(expanded_observed_and_high_and_country$Environment)

class(expanded_observed_and_high_and_country$cost_bil)
grouped_environment <-
  expanded_observed_and_high_and_country %>% group_by(Environment) %>%
  summarise(cost_by_environment = sum(numeric(cost_bil)))

gg <-
  ggplot(grouped_environment,
         aes(x = Environment, y = cost_by_environment))
gg <- gg + geom_bar(stat = "identity", fill = "darkgreen")
gg <- gg + theme_cowplot()
gg <- gg + ylab("Cost (US$ billions)")
gg <- gg + xlab("Environment")
gg
gg <-
  gg + theme(
    axis.text = element_text(size = 25),
    # Change tick mark label size
    axis.title = element_text(size = 25, face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    strip.text = element_text(size = 25)
  ) # Change axis title size

gg

 
# Some other ways of identifying costs
aggregate(cost_bil~Environment,data=expanded_observed_and_high,FUN="sum")
aggregate(cost_bil~Environment,data=expanded_observed_and_high,FUN="length")
