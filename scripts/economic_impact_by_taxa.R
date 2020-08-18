library(dplyr)
library(ggplot2)
library(invacost)

source("scripts/filtering_and_cleaning_data.R")

# See all the different levels of phyla within the database
unique(expanded_observed_and_high_and_country$Phylum)

# Broad taxonomic groups
# Subset inverts
inverts <-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Phylum %in% c(
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
dim(inverts)

# subset plants
plants <-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Phylum %in% c("Chlorophyta",
                                                                      "Tracheophyta",
                                                                      "Haptophyta",
                                                                      "Tracheophyta/Unspecified"),]
sum(plants$cost_bil)
dim(plants)
# Subset verts
verts <-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Phylum %in% c("Chordata",
                                                                      "Chordata/Diverse"),]
sum(verts$cost_bil)
dim(verts)
# Anything else not captured by above categories
other <-
  expanded_observed_and_high_and_country[expanded_observed_and_high_and_country$Phylum %in% c(
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
dim(other)
# More minor taxonomic groupings
cost_by_fine_taxonomic_groups <- aggregate(expanded_observed_and_high_and_country$cost_bil, by=list(Category=expanded_observed_and_high_and_country$Phylum), FUN=sum)
cost_by_fine_taxonomic_groups[order(cost_by_fine_taxonomic_groups$x),]
