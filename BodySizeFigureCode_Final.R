###Makes figures 2, 4-6 from the salmonid body size manuscript. Figures 1 & 3 were created using Powerpoint

###For figure 2, need the file: BodySize_WorldMap.csv available on Github

###For figure 4,  need the files: bodysizeModelresults.csv & BodySize_LatLongTable.csv

###For figure 5, need the file: bodysizeModelresults.csv

###For figure 6, need to have run BodySizeSecondaryAnalysesCode.R and have all of the FullTables loaded into the
###environment (LinfFullTable, popAvgFullTable, rangePosFullTable, chlaFullTable, LakeAvgTempFullTable, TempChangeFullTable)

########################################################################################################################
#Figure 2- Data map

#Loading in packages
library(rworldmap)
library(dplyr)
library(ggplot2)
library(geosphere)
library(gpclib)

#Loading in locations with each genus, general lats and general longs
genusPoints<-read.csv("BodySize_WorldMap.csv", header=TRUE, stringsAsFactors = FALSE)

# Setting up world map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]
worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

#Map of study sites, color by genus
colorsGenus<-c("#a6cee3","#33a02c", "#b2df8a",  "#1f78b4")
names(colorsGenus)=sort(unique(genusPoints$Genus))#adding Genus names to colors

ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), fill="gray", colour = "white") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(76, -50, 0)) +
  geom_point(data=genusPoints, aes(x=GeneralLong, y=GeneralLat, fill=Genus, shape=Genus), size=2.5, color="black")+
  scale_fill_manual(values=colorsGenus)+
  scale_shape_manual(values=c(21,22,23,24))+theme(axis.text.x=element_blank(), #remove x axis labels
                                                  axis.ticks.x=element_blank(), #remove x axis ticks
                                                  axis.text.y=element_blank(),  #remove y axis labels
                                                  axis.ticks.y=element_blank(),
                                                  panel.background = element_rect(fill = 'white'),
                                                  panel.grid.major = element_line(color = 'lightgray'),
                                                  panel.border = element_rect(color="black", fill=NA)
  )+xlab("Longitude")+ylab("Latitude")

ggsave("dataMap.pdf", dpi = 500, width=8, height=8)
########################################################################################################################
#Figure 4- Histogram of changes in length per year in panel 1, dot plot of change in length per year for every population 
#(lake/gear/species combo) in panel 2

#Loading in packages
library("forcats")
library("gridExtra")
library("cowplot")

#Reading in model results:
bodysizeModelResults<-read.csv("Desktop/BodySizeGithubFiles/bodysizeModelresults.csv", header=TRUE, stringsAsFactors = FALSE)

#Setting colors to species:
colors<-c("#fb9a99", "#e31a1c", "#cab2d6", "#6a3d9a",  "#b2df8a","#33a02c", "#fdbf6f", "#ff7f00", "#a6cee3", "#1f78b4", "#000000", "#b15928")
speciesList<-sort(unique(bodysizeModelResults$Species))
names(colors)=sort(unique(speciesList)) #adding species names to colors

#Reading in survey table to get to latitudes:
lakeLatLong<-read.csv("BodySize_LatLongTable.csv", header=TRUE, stringsAsFactors = FALSE)
lakeLats<-lakeLatLong[,2:3]

#Combining model result table with lake latitudes
lakeLatFullTable<-merge(bodysizeModelResults, lakeLats, by="Lake", all.x=F)
lakeLatFullTable<-unique(lakeLatFullTable) #for whatever reason get a few repeats, this removes them

#Creating histogram
hist<-ggplot(bodysizeModelResults)+geom_histogram(aes(x=OverallSlopeBacktransformed), color="black", fill="lightgray", binwidth = 0.01, boundary=0)+
  geom_vline(aes(xintercept = 0), colour="red")+theme_classic(base_size = 12)+theme(panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2))+
  labs(x="Average Change in Length per Year (mm)", y="Number of Populations")

#Creating dotplot
dot<-ggplot()+geom_point(aes(x=fct_reorder(lakeLatFullTable$Lake, lakeLatFullTable$Latitude, .desc=F), y=lakeLatFullTable$OverallSlopeBacktransformed, color=lakeLatFullTable$Species))+
  geom_hline(aes(yintercept=0))+theme_classic()+labs(color="Species")+labs(x="Lake (Low Latitude -> High Latitude)", y="Average Change in Length per Year (mm)")+
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=6),
        axis.ticks.x=element_blank(), panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
        axis.title.x = element_text(size=12), axis.title.y = element_text(size=12), legend.text=element_text(size=10),
        legend.key.size = unit(0.5,'cm'),
        legend.title=element_text(size=12), legend.position=c(0.5, 0.11), legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = "black"))+guides(colour = guide_legend(nrow = 2))+
  scale_color_manual(values=colors)
#latitude values were added in post-hoc

plot_grid(hist, dot, ncol = 1)
ggsave("2panelDotHist.pdf", dpi = 300, width=15, height=10)


########################################################################################################################
#Figure 5- Bar chart of average change in length per year for each species

#Setting colors to each genus
colorsGenus<-c("#a6cee3","#33a02c", "#b2df8a",  "#1f78b4")
names(colorsGenus)=sort(unique(bodysizeModelResults$Genus))#adding Genus names to colors

library(Rmisc) #loading in necessary package to use summarySE function
avgSpeciesChange<-summarySE(bodysizeModelResults, measurevar="OverallSlopeBacktransformed", groupvars="Species")
#taking the average change in length per year for each species
#NAs are produced because some species only have one population, so can't calculate a standard deviation or error for those

#Adding genus column to model results
bodysizeModelResults<- bodysizeModelResults %>% mutate(Genus =
                     case_when(Species == "Arctic Char" ~ "Salvelinus", 
                               Species == "Brook Trout" ~ "Salvelinus",
                               Species == "Lake Trout" ~ "Salvelinus",
                               Species == "Cisco" ~ "Coregonus",
                               Species == "Coregonus wartmanni" ~ "Coregonus",
                               Species == "Common Whitefish" ~ "Coregonus",
                               Species == "Lake Whitefish" ~ "Coregonus",
                               Species == "Vendace" ~ "Coregonus",
                               Species == "Hovsgol Grayling" ~ "Thymallus",
                               Species == "Arctic Grayling" ~ "Thymallus",
                               Species == "Round Whitefish" ~ "Prosopium",
                               Species == "Bonneville Whitefish" ~ "Prosopium",))

distinctSpecies<-bodysizeModelResults%>% group_by(Species, Genus) %>% #getting dataframe of each species with appropriate genus
  select(Species, Genus)%>%
  distinct()

avgSpeciesChange<-merge(avgSpeciesChange, distinctSpecies, by=c("Species")) #combining the average values to the species/genus table

#Removing the standard deviations for additional species that only have 1 population (but more than 1 gear, so sd/se was calculated)
#so that no error bars appear
avgSpeciesChange[avgSpeciesChange$Species=="Bonneville Whitefish", "sd"]<-NA
avgSpeciesChange[avgSpeciesChange$Species=="Common Whitefish", "sd"]<-NA 
avgSpeciesChange[avgSpeciesChange$Species=="Bonneville Whitefish", "se"]<-NA
avgSpeciesChange[avgSpeciesChange$Species=="Common Whitefish", "se"]<-NA 

#Creating figure
ggplot(data=avgSpeciesChange, aes(x=fct_reorder(Species, Genus), y=OverallSlopeBacktransformed, fill=Genus))+geom_bar(stat="identity")+labs(x="Species", y="Average Change in Body Size (mm/year) ")+geom_hline(yintercept=0)+
  theme_bw(base_size = 12)+theme(panel.grid.major.x = element_blank(), axis.ticks.x=element_blank()
                                 ,axis.text.x = element_text(size=12, angle = 45, hjust=1, color="black"), axis.text.y = element_text(size=12),
                                 axis.title=element_text(size=14), legend.text=element_text(size=12))+
  geom_errorbar(aes(ymin=OverallSlopeBacktransformed-se, ymax=OverallSlopeBacktransformed+se), width=.2)+
  scale_fill_manual(values=colorsGenus)

ggsave("speciesPlot.pdf", dpi = 500, width=10, height=6) #saving figure

########################################################################################################################
#Figure 6- Biplots of environmental covariates

#Setting colors to species names
colors<-c("#fb9a99", "#e31a1c", "#cab2d6", "#6a3d9a",  "#b2df8a","#33a02c", "#fdbf6f", "#ff7f00", "#a6cee3", "#1f78b4", "#000000", "#b15928")
speciesList<-sort(unique(bodysizeModelResults$Species))
names(colors)=sort(unique(speciesList))

#Creating the average body size at the species level plot; not significant in analysis, so no line
avgBodySizePlot<-ggplot(data=LinfFullTable, aes(x=Linf, y=OverallSlopeBacktransformed))+geom_point(aes(color=Species))+
  labs(x="Asymptotic Length (cm)", y="Average Change in Body Size (mm/year)")+
  ylim(-0.07, .1)+ 
  scale_color_manual(values=colors)+theme_bw(base_size = 12)
avgBodySizePlot

#Creating the average body size at the population level plot; significant in analysis, so solid line
AvgPopSizeRegressionPlot<-ggplot(popAvgFullTable, aes(x=avgPopLength, y=OverallSlopeBacktransformed))+geom_point(aes(x=avgPopLength, y=OverallSlopeBacktransformed, color=Species))+
  labs(x="Mean Length of Population (mm)", y="Average Change in Body Size (mm/year)")+
  geom_smooth(method='lm', formula= y~x)+
  ylim(-0.07, .1)+
  scale_color_manual(values=colors)+theme_bw(base_size = 12)
AvgPopSizeRegressionPlot

#Creating the range position plot; not significant in analysis, so no line
rangePositionPlot<-ggplot(data=rangePosFullTable, aes(x=rangePosition, y=OverallSlopeBacktransformed))+geom_point(aes(color=Species))+
  labs(x="Range Position", y="Average Change in Body Size (mm/year)")+
  ylim(-0.07, .1)+
  scale_color_manual(values=colors)+theme_bw(base_size = 12)
rangePositionPlot

#Creating chlorophyll plot; not significant in analysis, so no line
chlaRegressionPlot<-ggplot(data=chlaFullTable, aes(y=OverallSlopeBacktransformed, x=AvgChlorophyll))+geom_point(aes(color=Species))+
  labs(x="Mean Chlorophyll-a (microgram/L)", y="Average Change in Body Size (mm/year)")+
  ylim(-0.07, .1)+
  scale_color_manual(values=colors)+theme_gray(base_size = 12)+theme_bw(base_size = 12)
chlaRegressionPlot

#Creating temperature change plot; not significant in analysis, so no line
tempChangeRegressionPlot<-ggplot(TempChangeFullTable, aes(x=TempChange, y=OverallSlopeBacktransformed))+geom_point(aes(x=TempChange, y=OverallSlopeBacktransformed, color=Species))+
  labs(x="Temperature Change (°C)", y="Average Change in Body Size (mm/year)")+
  ylim(-0.07, .1)+
  scale_color_manual(values=colors)+theme_gray(base_size = 12)+theme_bw(base_size = 12)
tempChangeRegressionPlot

#Creating average temperature plot; significant in analysis now so line
tempAvgRegressionPlot<-ggplot(LakeAvgTempFullTable, aes(x=avgLakeTemp, y=OverallSlopeBacktransformed))+geom_point(aes(x=avgLakeTemp, y=OverallSlopeBacktransformed, color=Species))+
  labs(x="Mean Temperature (°C)", y="Average Change in Body Size (mm/year)")+
  geom_smooth(method='lm', linetype="solid",formula= y~x)+
  ylim(-0.07, .1)+
  scale_color_manual(values=colors)+theme_gray(base_size = 12)+theme_bw(base_size = 12)
tempAvgRegressionPlot

library("ggpubr")
ggarrange(avgBodySizePlot, AvgPopSizeRegressionPlot, rangePositionPlot, chlaRegressionPlot,  tempAvgRegressionPlot, tempChangeRegressionPlot, common.legend = T, legend="right")

ggsave("biplots.pdf", dpi = 300, width=13, height=8) #saves the figure
