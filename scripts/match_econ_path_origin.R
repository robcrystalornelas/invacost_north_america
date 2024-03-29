# code to link invacost north america data with economic predictors and CABI variables
#written by Emma J Hudgins, Aug 11 2020

source("scripts/filtering_and_cleaning_data.R")

library(countrycode)
library(wbstats)
library(invacost)
data<-expanded_observed_and_high_and_country

data$Species<-gsub("spp.","sp.", data$Species) # 51 species + Diverse/Unspecified, several not resolved to the species level


pathways<-read.csv('scripts/intro_pathways_vectors_all.csv')
colnames(pathways)[1]<-"Species"

spp_dat<-data.frame(Species=unique(data$Species))

path_dat<-merge(spp_dat, pathways, "Species", all.x=T)# cols 2-47 are causes, 48-76 are vectors
colSums(pathways[,2:47]/rowSums(pathways[,2:47], na.rm=T), na.rm=T) # assuming equal breakdown across pathways for a given species
colSums(pathways[,48:76]/rowSums(pathways[,48:76], na.rm=T), na.rm=T)
#could conceivably group into smaller set of levels, aggregate similar to origin

origins<-read.csv('scripts/intro_pathways_origins_all.csv')
colnames(origins)[1]<-"Species"
origin_dat<-merge(spp_dat, origins, all.x=T)

data<-merge(data, origin_dat[,c(1,10)], by="Species")

health_spend<-read.csv('scripts/CHE by GDP_by country.csv')
health_spend<-health_spend[2:nrow(health_spend),]

health_spend$codes2<-countrycode(health_spend$X, 'country.name', 'iso3c')
data$codes2<-countrycode(data$Official_country, 'country.name', 'iso3c')
data<-merge(data,health_spend[,c(2,20)], "codes2") # WHO % gdp spent on health in 2017 by country (could alternatively match to 'impact_year')
colnames(data)[ncol(data)]<-"health_spend"

ind_spend<-read.csv('scripts/API_NV.AGR.TOTL.CD_DS2_en_csv_v2_1121017.csv')
ind_spend$codes2<-countrycode(ind_spend$Country.Name, 'country.name', 'iso3c')
data<-merge(data,ind_spend[,c(62,65)], "codes2" ) #industry value added for agriculture, fisheries, forestries in 2017 (could alternatively match to 'impact_year')
colnames(data)[ncol(data)]<-"ind_spend"

GDP<-wb_data(country = unique(data$codes2), indicator = c( "NY.GDP.MKTP.KD"),  start_date = 2017,end_date=2017, return_wide=TRUE) # could change this to 1960-2017 and match with impact_year

data$GDP<-GDP$NY.GDP.MKTP.KD[match(data$codes2, GDP$iso3c)]

Inv_res_dev <-wb_data(country = unique(data$codes2), indicator = c( "GB.XPD.RSDV.GD.ZS"),  start_date = 2017,end_date=2017, return_wide=TRUE)#one metric used in another paper im on, % gdp expenditure on R&D
data$RD<-Inv_res_dev$GB.XPD.RSDV.GD.ZS[match(data$codes2, Inv_res_dev$iso3c)]



### Import sTwist ###
stwist<-read.table('scripts/AlienSpecies_MultipleDBs_Masterfile_vs2.3.csv', header=T)
head(stwist)
colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$Official_country<-gsub("United States of America", "USA", stwist$Official_country)
stwist<-subset(stwist, Official_country%in%data$Official_country)

stwist$eventDate<-as.numeric(as.character(stwist$eventDate))
length(unique(stwist$Species[which(stwist$eventDate>=1970)]))
hist(subset(stwist$eventDate, stwist$eventDate>1800), xlim=c(1800,2040), breaks=20, xlab="sTwist year of first record", main=NULL) #32 before 1800
#Completeness of invacost based on stwist 14754 total species
 # 51 species + Diverse/Unspecified, several not resolved to the species level

library(vioplot)
library(viridis)
dim(stwist)
stwist_s<-subset(stwist, eventDate >=1800)

length(which(stwist$eventDate<1800))
vioplot(stwist_s$eventDate~stwist_s$Official_country, col=viridis(4), outer=T, horizontal=T) #32 before 1800
title(xlab="sTwist year of first record")
data<-subset(data, Impact_year)
vioplot(data$Impact_year~data$Official_country, col=viridis(4), outer=T,  horizontal=T) #32 before 1800
points(y=rep(2,3),x=c(data$Impact_year[which(data$codes2=="CUB")]), bg=viridis(4)[2], pch=21, col="black")
title(xlab="Invacost impact year")

table(stwist$Official_country)
data2<-aggregate(data, by=list(data$Species, data$Official_country),FUN =  unique)
table(data2$codes2)
table(stwist$eventDate)
length(which(stwist$Species%in%data$Species))/length(unique(stwist$Species))
length(unique(stwist$Species[which(is.na(stwist$eventDate)==T)]))


stwist_intros<-subset(stwist,establishmentMeans%in%c("introduced" ,   "introduced; uncertain"  ,"introduced; vagrant" ,"introduced; uncertain; vagrant", "introduced; NA","introduced; ; NA"  ,"NA; introduced"))
n_intro<-stwist_intros %>%group_by(Species)%>%summarise_all(length)


clip_spp<-merge(stwist, data, by=c("Species", "Official_country"), all=T)
codes<-countrycode(clip_spp$Official_country, 'country.name', 'iso3c')
countrydat<-readRDS('scripts/CountriesDataPop.rds')
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
  sub<-subset(clip_spp, Species==unique(clip_spp$Species)[i])
  range_prop[i]<-sum(sub$Area[which(is.na(sub$cost_bil)==F)], na.rm=T)/sum(sub$Area, na.rm=T)
  range_size[i]<-sum(sub$Area, na.rm=T)
  
}
range_size<-data.frame(range_size, Species=unlist(unique(clip_spp4$Species)))
range_prop<-data.frame(range_prop, Species=unlist(unique(clip_spp4$Species)))
data$range_size<-range_size$range_size[match(data$Species, range_size$Species)]
data$range_prop<-range_prop$range_prop[match(data$Species, range_prop$Species)]
data$n_intro<-n_intro$Official_country[match(data$Species, n_intro$Species)]

mean(range_prop[match(unique(data$Species),range_prop$Species),1]) #79.90% complete within iso3c codes when reported
which(range_size$range_size==max(range_size$range_size)) # how many species continent-wide invaders? 
range_size_stwist<-clip_spp%>%group_by(Species)%>%summarise_at("Area", sum)
which(range_size_stwist$Area==max(range_size_stwist$Area))
mean(as.numeric(unlist(n_intro[match(unique(data$Species),n_intro$Species),2])), na.rm=T) #when known, 2.22 independent introductions 
length(which(is.na(unlist(n_intro[match(unique(data$Species),n_intro$Species),2]))==T)) #16 unknown

data_stwist<-merge(data,stwist, by=c("Species", "Official_country"), all.x=T)#not super complete, but might be worth exploring

 # write.csv(data, file="NAm_data_econ_origin.csv", row.names=F)
 # write.csv(path_dat, file="NAm_pathways.csv", row.names=F)
 # write.csv(data_stwist, file="NAm_data_stwist.csv", row.names=F)


spp_path<-read.csv('../../../../Dropbox/InvaCost Workshop France/Projects/Activity Sector/intro_pathways_cabi_v3.csv')
spp_path<-spp_path[match(spp_dat$Species, spp_path$species_list),]
colnames(spp_path)[1]<-"Species"
data<-merge(data, spp_path, "Species")
data$all_other<-rep(0, nrow(data))
data$all_other[which(rowSums(data[,94:99])==0)]<-1

density<-data %>% group_by(Kingdom,Species, Impacted_sector_2, origin) %>% summarise_if(is.numeric, sum, na.rm=T)

  density<-as.data.frame(density)
  net<-(density[1,])
  net[1,]<-NA
  net$pathway<-NA
  for (i in 1:nrow(density))
  {
    names<-colnames(density[i,48:54])[which(density[i,48:54]>0)]
    for (j in 1:length(names))
    {
      net<-rbind(net, setNames(cbind(density[i,], names[j]),c(colnames(density), "pathway")))
      net[i,"cost"]<-(density[i,"cost"]*density[i, names[j]])/sum(density[i, 48:54])
    }
  }


  
 
net<-net[2:nrow(net),]
library(networkD3)


net$origin[which(is.na(net$origin))]<-"UNK"
links = data.frame(source=net$pathway,target=net$Impacted_sector_2,value=net$cost, value2=1,Kingdom=net$Kingdom, origin=net$origin)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

##toggle between commenting out these next two lines, the two colour scheme blocks, and setting 'LinkGroup' in line 151 and 157 to either Kingdom or origin
#links$group<-as.numeric(factor(paste0(links$source, links$target, links$Kingdom)))
#links<-links[order(links$Kingdom),]
links$group<-as.numeric(factor(paste0(links$source, links$target, links$origin)))
links<-links[order(links$origin),]
links<-links %>% group_by(IDsource, IDtarget, group, Kingdom,origin) %>% summarise_if(is.numeric, sum, na.rm=T)

#my_color2c <- 'd3.scaleOrdinal() .domain(["Plantae", "Animalia", "Chromista","Fungi",  "Authorities-Stakeholders" , "Environment" ,"Diverse/Unspecified", "Forestry" ,  "Agriculture","Fishery" , "Public and social welfare", "Health", "all_agri", "all_other", "all_pet", "all_for" , "all_fish"]) .range(["#b5bca0",  "#aaaaaa","#377eb8","#000000", "#e41a1c","#045a8d", "#262f09","#b4b4b4","#006d2c", "#8c8c8c", "#969696","#6effa0","#ad7274", "#a50f15", "#ff5b86", "#01dda5","#ffeb92"])' #grey animals, #green/beige plants, #blue chromista

my_color2c <- 'd3.scaleOrdinal() .domain(["As",      "Diverse", "NAm",     "EUR",     "UNK" ,    "SA",      "AF",  "Authorities-Stakeholders" , "Environment" ,"Diverse/Unspecified", "Forestry" ,  "Agriculture","Fishery" , "Public and social welfare", "Health", "all_agri", "all_other", "all_pet", "all_for" , "all_fish"]) .range(["blue","red","yellow","purple","grey","green","orange", "#e41a1c","#045a8d", "#262f09","#b4b4b4","#006d2c", "#8c8c8c", "#969696","#6effa0","#ad7274", "#a50f15", "#ff5b86", "#01dda5","#ffeb92"])' #grey 

library(htmlwidgets)
library(htmltools)

gdp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", LinkGroup='origin', colourScale = my_color2c, fontSize = 12, ) #plot by cost, scaled by country gdp
gdp_viz<-prependContent(gdp_viz, tags$div("Cost Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
gdp_viz$sizingPolicy$viewer$fill <- FALSE
gdp_viz


spp_viz<-sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value2", NodeID = "name", LinkGroup="origin", colourScale = my_color2c, fontSize = 12) #plot by species
spp_viz<-prependContent(spp_viz, tags$div("Species Flows", style=("font-family: Helvetica; font-size:12; text-align: center")) )
spp_viz$sizingPolicy$viewer$fill <- FALSE
spp_viz


     