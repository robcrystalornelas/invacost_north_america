# code to link invacost north america data with economic predictors and CABI variables
#written by Emma J Hudgins, Aug 11 2020

source("filtering_and_cleaning_data.R")
library(countrycode)
library(wbstats)
data<-expanded_observed_and_high_and_country

pathways<-read.csv('intro_pathways_vectors_all.csv')
colnames(pathways)[1]<-"Species"

spp_dat<-data.frame(Species=unique(data$Species))

path_dat<-merge(spp_dat, pathways, "Species", all.x=T)# cols 2-47 are causes, 48-76 are vectors
colSums(pathways[,2:47]/rowSums(pathways[,2:47], na.rm=T), na.rm=T) # assuming equal breakdown across pathways for a given species
colSums(pathways[,48:76]/rowSums(pathways[,48:76], na.rm=T), na.rm=T)
#could conceivably group into smaller set of levels, aggregate similar to origin

origins<-read.csv('intro_pathways_origins_all.csv')
colnames(origins)[1]<-"Species"
origin_dat<-merge(spp_dat, origins, all.x=T)

data$Origin<-NA
data$Origin[match(origin_dat$Species, data$Species)]<-origin_dat$origin

health_spend<-read.csv('CHE by GDP_by country.csv')
health_spend<-health_spend[2:nrow(health_spend),]

health_spend$codes2<-countrycode(health_spend$X, 'country.name', 'iso3c')
data$codes2<-countrycode(data$Official_country, 'country.name', 'iso3c')
data<-merge(data,health_spend[,c(2,20)], "codes2") # WHO % gdp spent on health in 2017 by country (could alternatively match to 'impact_year')
colnames(data)[ncol(data)]<-"health_spend"

ind_spend<-read.csv('API_NV.AGR.TOTL.CD_DS2_en_csv_v2_1121017.csv')
ind_spend$codes2<-countrycode(ind_spend$Country.Name, 'country.name', 'iso3c')
data<-merge(data,ind_spend[,c(62,65)], "codes2" ) #industry value added for agriculture, fisheries, forestries in 2017 (could alternatively match to 'impact_year')
colnames(data)[ncol(data)]<-"ind_spend"

GDP<-wb_data(country = unique(data$codes2), indicator = c( "NY.GDP.MKTP.KD"),  start_date = 2017,end_date=2017, return_wide=TRUE) # could change this to 1960-2017 and match with impact_year

data$GDP<-GDP$NY.GDP.MKTP.KD[match(data$codes2, GDP$iso3c)]

Inv_res_dev <-wb_data(country = unique(data$codes2), indicator = c( "GB.XPD.RSDV.GD.ZS"),  start_date = 2017,end_date=2017, return_wide=TRUE)#one metric used in another paper im on, % gdp expenditure on R&D
data$RD<-Inv_res_dev$GB.XPD.RSDV.GD.ZS[match(data$codes2, Inv_res_dev$iso3c)]



### Import sTwist ###
stwist<-read.table('AlienSpecies_MultipleDBs_Masterfile_vs2.3.csv', header=T)
colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$Official_country<-gsub("United States of America", "USA", stwist$Official_country)
stwist<-subset(stwist, Official_country%in%data$Official_country)
clip_spp<-merge(stwist, data, by=c("Species", "Official_country"), all=T)
codes<-countrycode(clip_spp$Official_country, 'country.name', 'iso3c')
countrydat<-readRDS('CountriesDataPop.rds')
countrydat$NAME<-gsub("United States", "USA", countrydat$NAME)
countrydat<-subset(countrydat,  NAME%in%data$Official_country)
codes2<-countrycode(countrydat$NAME, 'country.name', 'iso3c')
clip_spp$Area<-countrydat$area_sqkm[match(codes,codes2)]
#probably actually want area of region rather than country
clip_spp2<-subset(clip_spp, Species%in%data$Species)
clip_spp4<-clip_spp2%>%group_by(Species, Official_country)%>%summarise_if(is.numeric, mean)
range_prop<-range_size<-0
for (i in 1:length(unique(clip_spp4$Species)))
{
  sub<-subset(clip_spp, Species==unique(clip_spp4$Species)[i])
  range_prop[i]<-sum(sub$Area[which(is.na(sub$cost_bil)==F)], na.rm=T)/sum(sub$Area, na.rm=T)
  range_size[i]<-sum(sub$Area, na.rm=T)
  
}
range_size<-data.frame(range_size, Species=unlist(unique(clip_spp4$Species)))
range_prop<-data.frame(range_prop, Species=unlist(unique(clip_spp4$Species)))
stwist_intros<-subset(clip_spp2,establishmentMeans%in%c("introduced" ,   "introduced; uncertain"  ,"introduced; vagrant" ,"introduced; uncertain; vagrant", "introduced; NA","introduced; ; NA"  ,"NA; introduced"))
n_intro<-stwist_intros %>%group_by(Species)%>%summarise_all(length)
data$range_size<-range_size$range_size[match(data$Species, range_size$Species)]
data$range_prop<-range_prop$range_prop[match(data$Species, range_prop$Species)]
data$n_intro<-n_intro$Official_country[match(data$Species, n_intro$Species)]

data$n_intro<-n_intro$Official_country[match(data$Species, n_intro$Species)]
data_stwist<-merge(data,stwist, by=c("Species", "Official_country"), all.x=T)#not super complete, but might be worth exploring

write.csv(data, file="NAm_data_econ_origin.csv", row.names=F)
write.csv(path_dat, file="NAm_pathways.csv", row.names=F)
write.csv(data_stwist, file="NAm_data_stwist.csv", row.names=F)

