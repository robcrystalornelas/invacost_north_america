
# Sector costs
aggregate(cost_bil~Impacted_sector_2,data=expanded_observed_and_high_and_country,FUN="sum")
aggregate(cost_bil~Impacted_sector_2,data=expanded_observed_and_high_and_country,FUN="length")
