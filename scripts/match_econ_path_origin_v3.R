# code to link invacost north america data with economic predictors and CABI variables
#written by Emma J Hudgins, Aug 11 2020

source("scripts/filtering_and_cleaning_data.R")


library(countrycode)
library(wbstats)
library(invacost)
data<-expanded_observed_and_high

data$Species<-gsub("spp.","sp.", data$Species) # 51 species + Diverse/Unspecified, several not resolved to the species level
data$Species<-gsub("Aedes.*", "Aedes aegypti albopictus", data$Species)
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
stwist<-read.table('scripts/AlienSpecies_MultipleDBs_Masterfile_vs2.3.csv', header=T, stringsAsFactors = F)
colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$Official_country<-gsub("United States of America", "USA", stwist$Official_country)
stwist<-subset(stwist, Official_country%in%data$Official_country)
library(taxize)
data$Species_gbif<-gbif_parse(data$Species)$canonicalname
stwist<-subset(stwist, grepl("established",stwist$degreeOfEstablishment))
stwist$Species_gbif<-gbif_parse(stwist$Species)$canonicalname
# stwist$eventDate<-as.numeric(as.character(stwist$eventDate))
length(unique(stwist$Species_gbif[which(stwist$eventDate>=1970)])) #114
stwist$eventDate<-as.numeric(stwist$eventDate)
hist(subset(stwist$eventDate, stwist$eventDate>1800), xlim=c(1800,2040), breaks=20, xlab="sTwist year of first record", main=NULL) #32 before 1800
#Completeness of invacost based on stwist 14754 total species
# 51 species + Diverse/Unspecified, several not resolved to the species level
library(vioplot)
library(viridis)
stwist_s<-subset(stwist, eventDate>=1800)
length(which(stwist$eventDate<1800))
comparative<-data.frame(time=c(stwist_s$eventDate,data$Impact_year), country=c(stwist_s$Official_country,data$Official_country), set=c(rep("stwist",nrow(stwist_s)),rep("invacost", nrow(data))))
vioplot(comparative$time~comparative$set:comparative$country, col=rep(viridis(5),each=2), outer=T, horizontal=T, names=NA, xaxt="n",ylim=c(1770,2020),colMed2=rep(viridis(5),each=2),pchMed=21, colMed="black")
title(xlab="Year")
text(x=(2022),y=c(1:10),labels=c(28,64,1,39,1,32,32,44,121,260),cex=0.5, col=c(rep(c("darkgrey", "black"),5)) )
legend(x=1765,y=7.5, legend=c("Canada (44%)", "Cuba (3%)", "Dominican Republic (3%)", "Mexico (16%)", "USA (47%)"),col=viridis(5), cex=0.6, pch=19, pt.cex=1.5)
points(y=rep(5,2),x=c(data$Impact_year[which(data$codes2=="DOM")]), bg=viridis(5)[3], pch=21, col="black")

table(stwist$Official_country)
data2<-aggregate(data, by=list(data$Species, data$Official_country),FUN =  unique)
table(data2$codes2)
table(stwist$eventDate)
length(which(stwist$Species%in%data$Species))/length(unique(stwist$Species))
length(unique(stwist$Species[which(is.na(stwist$eventDate)==T)]))


stwist_intros<-subset(stwist,establishmentMeans%in%c("introduced" ,   "introduced; uncertain"  ,"introduced; vagrant" ,"introduced; uncertain; vagrant", "introduced; NA","introduced; ; NA"  ,"NA; introduced"))
n_intro<-stwist_intros %>%group_by(Species_gbif)%>%summarise_all(length)

data<-subset(data, is.na(Species_gbif)==F)
clip_spp<-merge(stwist, data, by=c("Species_gbif", "Official_country"), all=T)
codes<-countrycode(clip_spp$Official_country, 'country.name', 'iso3c')
countrydat<-readRDS('scripts/CountriesDataPop.rds')
countrydat$NAME<-gsub("United States", "USA", countrydat$NAME)
countrydat$NAME<-gsub("Dominican Rep.", "Dominican Republic", countrydat$NAME)
countrydat<-subset(countrydat,  NAME%in%data$Official_country)
codes2<-countrycode(countrydat$NAME, 'country.name', 'iso3c')
clip_spp$Area<-countrydat$area_sqkm[match(codes,codes2)]
#probably actually want area of region rather than country
clip_spp2<-subset(clip_spp, Species_gbif%in%data$Species_gbif)
clip_spp4<-clip_spp2%>%group_by(Species_gbif, Official_country)%>%summarise_if(is.numeric, max)
range_prop<-range_size<-n_countries<-0
for (i in 1:length(unique(clip_spp4$Species_gbif)))
{
  sub<-subset(clip_spp4, Species_gbif==unique(clip_spp4$Species_gbif)[i])
  range_prop[i]<-sum(sub$Area[which(is.na(sub$cost_bil)==F)], na.rm=T)/sum(sub$Area, na.rm=T)
  range_size[i]<-sum(sub$Area, na.rm=T)
  n_countries[i]<-length(unique(sub$Official_country))
}
range_size<-data.frame(range_size, Species_gbif=unlist(unique(clip_spp4$Species_gbif)))
range_prop<-data.frame(range_prop, Species_gbif=unlist(unique(clip_spp4$Species_gbif)))
data$range_size<-range_size$range_size[match(data$Species_gbif, range_size$Species_gbif)]
data$range_prop<-range_prop$range_prop[match(data$Species_gbif, range_prop$Species_gbif)]
data$n_intro<-n_intro$Official_country[match(data$Species_gbif, n_intro$Species_gbif)]

mean(range_prop[match(unique(data$Species_gbif),range_prop$Species_gbif),1], na.rm=T) #71.5 complete within iso3c codes when reported
which(range_size$range_size==max(range_size$range_size)) # how many species continent-wide invaders? 
sum(data$cost_bil[which(data$Species_gbif=="Columba livia")])
sum(data$cost_bil[which(data$Species_gbif=="Columba livia")]/data$range_prop[which(data$Species_gbif=="Columba livia")])

sum(data$cost_bil/data$range_prop, na.rm=T)
sum(data$cost_bil, na.rm=T)

range_size_stwist<-clip_spp%>%group_by(Species_gbif, Official_country)%>%summarise_at(c("Area", "Impact_year"), max)
range_size_stwist<-range_size_stwist%>%group_by(Species_gbif)%>%summarise_at(c("Area"), sum)
which(range_size_stwist$Area==max(range_size$range_size))
mean(as.numeric(unlist(n_intro[match(unique(data$Species_gbif),n_intro$Species_gbif),2])), na.rm=T) #when known, 2.22 independent introductions 
length(which(is.na(unlist(n_intro[match(unique(data$Species_gbif),n_intro$Species_gbif),2]))==T)) #43 unknown
length(which(is.na(unlist(n_intro[match(unique(data$Species_gbif),n_intro$Species_gbif),2]))==F)) #11 known


data_stwist<-merge(data,stwist, by=c("Species_gbif", "Official_country"), all.x=T)#not super complete, but might be worth exploring

# write.csv(data, file="NAm_data_econ_origin.csv", row.names=F)
# write.csv(path_dat, file="NAm_pathways.csv", row.names=F)
# write.csv(data_stwist, file="NAm_data_stwist.csv", row.names=F)
#(n=30)
spp_path<-read.csv('./data/intro_pathways_cabi_v3.csv')
spp_path$species_list<-gsub("Aedes .*", "Aedes aegypti albopictus", spp_path$species_list)
spp_path$Species_gbif<-gbif_parse(spp_path$species_list)$canonicalname
spp_path<-subset(spp_path, species_list%in%unique(data$Species_gbif))
colnames(spp_path)[1]<-"Species"
data<-merge(data, spp_path, "Species")
data$all_other<-rep(0, nrow(data))
data$all_other[which(rowSums(data[,c(92:96,98)])==0)]<-1


density<-data %>% group_by(Kingdom,Species, Impacted_sector_2, origin) %>% summarise_if(is.numeric, sum, na.rm=T)


density<-as.data.frame(density)
net<-(density[1,])
net[1,]<-NA
net$pathway<-NA
for (i in 1:nrow(density))
{
  names<-colnames(density[i,38:44])[which(density[i,38:44]>0)]
  for (j in 1:length(names))
  {
    net<-rbind(net, setNames(cbind(density[i,], names[j]),c(colnames(density), "pathway")))
    net[i,"cost"]<-(density[i,"cost"]*density[i, names[j]])/sum(density[i, 38:44])
  }
}


net<-net[2:nrow(net),]
library(networkD3)
#devtools::install_github("fbreitwieser/sankeyD3")
#library(sankeyD3) # This library makes default sankey colors much easier to read

  density<-as.data.frame(density)
  net<-(density[1,])
  net[1,]<-NA
  net$pathway<-NA
  for (i in 1:nrow(density))
  {
    names<-colnames(density[i,38:44])[which(density[i,38:44]>0)]
    for (j in 1:length(names))
    {
      net<-rbind(net, setNames(cbind(density[i,], names[j]),c(colnames(density), "pathway")))
      net[i,"cost"]<-(density[i,"cost"]*density[i, names[j]])/sum(density[i, 38:44])
    }
  }

net<-net[2:nrow(net),]
library(networkD3)

net$origin[which(is.na(net$origin))]<-"UNK"
links = data.frame(source=c(net$origin, net$pathway),target=c(net$pathway,net$Impacted_sector_2),value=net$cost, value2=1,Kingdom=net$Kingdom, origin=net$origin)
links<-subset(links,target%in%c("Mixed", "Unspecified")==F)
links<-subset(links, origin%in%c("Diverse", "UNK")==F)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes=data.frame(name=c(as.character(links$source), as.character(links$target) ) %>% unique())
links$IDsource=c(match(links$source, nodes$name)-1 )
links$IDtarget=c(match(links$target, nodes$name)-1)

links<-links %>% group_by(origin,IDsource, IDtarget) %>% summarise_if(is.numeric, sum, na.rm=T)
links$origin<-mgsub(links$origin,c("AF","As","UNK","Diverse","NAm","SA","EUR") ,c("Africa", "Asia", "Unknown", "Diverse", "North America", "South America", "Europe"))


#  my_color <- 'd3.scaleOrdinal(d3.schemeCategory20)'
# 
#  
#  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
#  legend("topleft", legend =c( "As",      "Diverse", "NAm",     "EUR",     "UNK" ,    "SA",      "AF"), pch=16, pt.cex=1.5, cex=1, bty='n',
#         col = c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E" ,"#E6AB02", "#A6761D", "#1B9E77", "#D95F02" ,"#7570B3", "#E7298A", "#66A61E" ,"#E6AB02", "#A6761D"), title="Continent of Origin")
# library(htmlwidgets)
# library(htmltools)
 # nodes[1:7,1]<-c("Africa", "Asia", "Unknown", "Diverse", "North America", "South America", "Europe")
 # nodes[8:13,1]<-c('Agriculture','Pet Trade', 'Forestry', 'Other',  'Fishery', "Health")
nodes[1:5,1]<-c("Africa", "Asia", "North America", "South America", "Europe")
nodes[6:10,1]<-c('Agriculture','Pet Trade', 'Forestry','Other',  'Fishery')
gdp_viz <-
  sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value",
    NodeID = "name",
    LinkGroup="origin",
    colourScale = my_color,
    fontSize = 20,nodePadding = 20, iterations=0) #plot by cost, scaled by country gdp

gdp_viz <-
  prependContent(gdp_viz, tags$div(
    "Cost Flows",
    style = ("font-family: Helvetica; font-size:13; text-align: center")
  ))
gdp_viz


spp_viz <-
  sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value2",
    NodeID = "name",
    LinkGroup = "origin",
 colourScale = my_color,
    fontSize = 20, nodePadding=20,iterations=0
    
  ) #plot by species
spp_viz <-
  prependContent(spp_viz, tags$div(
    "Species Flows",
    style = ("font-family: Helvetica; font-size:13; text-align: center")
  ))

spp_viz

library(shiny)
##################### convert to pdf
#Run one plot at a time and save from server -> export to pdf


ui <- shinyUI(fluidPage(column(6,
                               h2("Process 1", align = "center"),
                               uiOutput("plot1.ui")
)
))

# Define server logic required to draw the sankeys
server <- shinyServer(function(input, output) {
  
  #Render plot area relative to flow height
  output$plot1.ui <- renderUI({
    sankeyNetworkOutput("Plot1", height = "600px")
  })
  
  #Render Sankeys
  output$Plot1 <- renderSankeyNetwork(spp_viz)})

# Run the application 
shinyApp(ui = ui, server = server)



##################### convert to pdf part2

ui <- shinyUI(fluidPage(
                               uiOutput("plot1.ui")


))

# Define server logic required to draw the sankeys
server <- shinyServer(function(input, output) {
  
  #Render plot area relative to flow height
  output$plot1.ui <- renderUI({
    sankeyNetworkOutput("Plot1", height = "600px")
  })
  
  #Render Sankeys
  output$Plot1 <- renderSankeyNetwork(gdp_viz)})

# Run the application 
shinyApp(ui = ui, server = server)

