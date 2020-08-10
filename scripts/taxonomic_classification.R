library(dplyr)
library(ggplot2)
library(invacost)

source("/Users/robcrystalornelas/Desktop/research/INVACOST_NorthAmerica/invacost_north_america/scripts/filtering_and_cleaning_data.R")

# See all the different levels of phyla within the database
levels(expanded_observed_and_high$Phylum)

# Subset inverts
inverts <-
  expanded_observed_and_high[expanded_observed_and_high$Phylum %in% c(
    "Arthropoda",
    "Annelida",
    "Ctenophora",
    "Mollusca",
    "Platyhelminthes",
    "Nematoda",
    "Mollusca/Mollusca",
    "Mollusca/Arthropoda",
    "Cnidaria",
    "Arthropoda/Chordata",
    "Nematoda/unknown"
  ),]
sum(inverts$cost_bil)

# subset plants
plants <-
  expanded_observed_and_high[expanded_observed_and_high$Phylum %in% c("Chlorophyta",
                                                                      "Tracheophyta",
                                                                      "Haptophyta",
                                                                      "Tracheophyta/Unspecified"),]
sum(plants$cost_bil)

# Subset verts
verts <-
  expanded_observed_and_high[expanded_observed_and_high$Phylum %in% c("Chordata",
                                                                      "Chordata/Diverse"),]
sum(verts$cost_bil)

# Anything else not captured by above categories
other <-
  expanded_observed_and_high[expanded_observed_and_high$Phylum %in% c(
    "Chytridiomycota",
    "Diverse/Unspecified",
    "Haptophyta",
    "Ochrophyta",
    "Oomycota",
    "Ascomycota",
    "Proteobacteria",
    "Negarnaviricota",
    " Kitrinoviricota",
    " Pisuviricota",
    "Actinobacteria",
    "Arthropoda/Chordata/Tracheophyta",
    "Basidiomycota",
    "Basidiomycota/Ascomycota ",
    "Tracheophyta/Chordata",
    "Cressdnaviricota",
    "Orthornavirae",
    "Peploviricota",
    "Picornavirales"
  ),]

sum(other$cost_bil)


