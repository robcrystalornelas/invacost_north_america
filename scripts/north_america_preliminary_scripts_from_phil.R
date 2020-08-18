library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(invacost)
data(invacost)
setwd("F:/invacost")
invacost <-read.csv("NA.csv",sep=";", stringsAsFactors=TRUE)



expanded <- expandYearlyCosts(invacost,
                              startcolumn = "Probable_starting_year_low_margin",
                              endcolumn = "Probable_ending_year_low_margin")
expanded<-expanded %>% filter(Probable_starting_year_low_margin >= "1960")
expanded<-expanded %>% filter(Probable_ending_year_low_margin <= "2017")
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded<- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil)




aggregate(cost_bil~Species,data=expanded,FUN="sum")
aggregate(cost_bil~Species,data=expanded,FUN="length")

aggregate(cost_bil~Order,data=expanded,FUN="sum")
aggregate(cost_bil~Order,data=expanded,FUN="length")

aggregate(cost_bil~Family,data=expanded,FUN="sum")
aggregate(cost_bil~Family,data=expanded,FUN="length")

aggregate(cost_bil~Class,data=expanded,FUN="sum")
aggregate(cost_bil~Class,data=expanded,FUN="length")

aggregate(cost_bil~Environment,data=expanded,FUN="sum")
aggregate(cost_bil~Environment,data=expanded,FUN="length")

aggregate(cost_bil~type2,data=expanded,FUN="sum")
aggregate(cost_bil~type2,data=expanded,FUN="length")

aggregate(cost_bil~sector2,data=expanded,FUN="sum")
aggregate(cost_bil~sector2,data=expanded,FUN="length")

aggregate(cost_bil~Method_reliability,data=expanded,FUN="sum")
aggregate(cost_bil~Method_reliability,data=expanded,FUN="length")

aggregate(cost_bil~Implementation,data=expanded,FUN="sum")
aggregate(cost_bil~Implementation,data=expanded,FUN="length")


####some plots


#Type of costs across Irder
#I am making 2 plots that are then overlapped

expanded.family.type<-aggregate(cost_bil~type2+Order,data=expanded,
                                FUN=sum)

colorset4 = c('Miscellaneous'='black',
              'Spending'='green',
              'Damage-Loss'='orange',
              'Indirect costs'='purple')

svg("type1.svg")
p1<-ggplot(expanded.family.type, aes(x = reorder(Order,-cost_bil), y = cost_bil, fill=factor(type2))) +
  geom_bar(stat='identity')+theme_bw() + theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  labs(x = "Official_country", y = "US$ Billions (2017 value)", fill = "Cost type")+ 
  scale_y_continuous(expand = c(0,0), limits = c(0, 4), 
                     breaks = c(0,1,2,3))+ 
  theme(legend.position="top")+
  #annotate("text", x=0.8, y=70, label= "(b)",size=5)+
  theme(text = element_text(size=15))
p1
dev.off()

svg("type2.svg")
p1<-ggplot(expanded.family.type, aes(x = reorder(Order,-cost_bil), y = cost_bil, fill=factor(type2))) +
  geom_bar(stat='identity')+theme_bw() + theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  labs(x = "Official_country", y = "US$ Billions (2017 value)", fill = "Cost type")+ 
  scale_y_continuous(expand = c(0,0), limits = c(0, 300000000), 
                     breaks = c(0,100000000,200000000,300000000))+ 
  theme(legend.position="top")+
  #annotate("text", x=0.8, y=70, label= "(b)",size=5)+
  theme(text = element_text(size=15))
p1
dev.off()


#sector plots
expanded.family.type<-aggregate(cost_bil~sector2+Order,data=expanded,
                                FUN=sum)

colorset4 = c('Miscellaneous'='black',
              'Spending'='green',
              'Damage-Loss'='orange',
              'Indirect costs'='purple')

svg("sector1.svg")
p1<-ggplot(expanded.family.type, aes(x = reorder(Order,-cost_bil), y = cost_bil, fill=factor(sector2))) +
  geom_bar(stat='identity')+theme_bw() + theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  labs(x = "Official_country", y = "US$ Billions (2017 value)", fill = "Cost type")+ 
  scale_y_continuous(expand = c(0,0), limits = c(0, 4), 
                     breaks = c(0,1,2,3,4))+ 
  theme(legend.position="top")+
  #annotate("text", x=0.8, y=70, label= "(b)",size=5)+
  theme(text = element_text(size=15))
p1
dev.off()

svg("sector2.svg")
p1<-ggplot(expanded.family.type, aes(x = reorder(Order,-cost_bil), y = cost_bil, fill=factor(sector2))) +
  geom_bar(stat='identity')+theme_bw() + theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
  labs(x = "Official_country", y = "US$ Billions (2017 value)", fill = "Cost type")+ 
  scale_y_continuous(expand = c(0,0), limits = c(0, 300000000), 
                     breaks = c(0,100000000,200000000,300000000))+ 
  theme(legend.position="top")+
  #annotate("text", x=0.8, y=70, label= "(b)",size=5)+
  theme(text = element_text(size=15))
p1
dev.off()


#Pie chart for environments
#camambert
#cost ENTRIES
expanded_obs.na.species.sector<-aggregate(cost_bil~Environment,data=expanded,
                                          FUN=length)
options(scipen = 999)
expanded_obs.na.species.sector
#ohje_expanded<-as.data.frame(table(expanded$Official_country))
list1 <- 3
list2 <- as.data.frame(rep("expanded",length(list1)))
colnames(list2)[1] <- "Environment2"
ohje_expanded2<-cbind(expanded_obs.na.species.sector,list2)
bla1<-as.data.frame(prop.table(ohje_expanded2$cost_bil))
colnames(bla1)[1] <- "perc"
ohje_expanded3<-cbind(ohje_expanded2,bla1)

bp<- ggplot(ohje_expanded3, aes(x="", y=perc, fill=Environment))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie
pie2<- pie + scale_fill_brewer(palette="Dark2")

svg("pie_Environment.svg")
pie2
dev.off()

#cumulations
test<-calculateRawAvgCosts(
  expanded,
  cost.column = "cost",
  in.millions = FALSE,
  minimum.year = 1990,
  maximum.year = 2017,
  year.breaks = seq(1990, 2017, by = 5))
test #and it shows 1 trillion... yay^^
svg("cum.svg")
plot(test)
dev.off()
