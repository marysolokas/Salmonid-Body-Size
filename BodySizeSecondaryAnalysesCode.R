###This code runs the secondary analyses for the salmonid body size manuscript.
###Before running this code, need to read in the model results for the full dataset: bodysizeModelResults.csv

###Also need to read in the following files: BodySize_PopAvgSize.csv, BodySize_LatLongTable.csv, BodySize_LakeChlorophyllData.csv,
###BodySize_LakeTempChangeData.csv, BodySize_LakeAvgTempData.csv

###All files available on GitHub

rm(list=ls())

#Reading in necessary files and preparing them for use in analyses
bodysizeModelResults<-read.csv("bodysizeModelResults.csv", header=TRUE, stringsAsFactors = FALSE)
bodysizeModelResults$weights<-1/bodysizeModelResults$totalVar #inverse of variance is used as the weights in the weighted linear regressions

#For the body size analysis at the species level:

#Linf values for each species; average taken from each entry in fishbase that was not questionable and not from captive populations
speciesAvgSize<-read.csv("BodySize_SpeciesAvgSize.csv", header=TRUE, stringsAsFactors = FALSE)
#average body size for each species (Linf)

#For the body size analysis at the population level
popAvgSize<-read.csv("BodySize_PopAvgSize.csv", header=TRUE, stringsAsFactors = FALSE)
#average body size for each population

#For the range position analysis:
lakeLatLong<-read.csv("BodySize_LatLongTable.csv", header=TRUE, stringsAsFactors = FALSE)
lakeLats<-lakeLatLong[,2:3]
#latitude of each lake

#For the productivity analysis:
meanChla<-read.csv("BodySize_LakeChlorophyllData.csv") 
meanChla<-meanChla[,2:3]
#mean chlorophyll-a value for each lake that we have data for

#For the average temperature analysis:
LakeAvgTemp<-read.csv("BodySize_LakeAvgTempData.csv")
#average temperature for each lake/population over the body size data survey period

#For the temperature change analysis:
LakeTempChangeAllData<-read.csv("BodySize_LakeTempChangeData.csv") 
LakeTempChangeData<-LakeTempChangeAllData[,c(1:2, 6)]
#temperature change for each lake/population over the body size data survey period

#######################################################################################################################
#Running weighted linear regression of change in body size per year against Linf (asymptotic average body size)
#to address the question: Do species with larger average body sizes experience more body significant body size changes? 

#Combining model result table with the Linf values:
LinfFullTable<-merge(bodysizeModelResults, speciesAvgSize, by="Species")
LinfFullTable$Species<-as.factor(LinfFullTable$Species)

#Simple model- regression change in length per year against Linf values, weighted by inverse of variance
modLinfRegression<-lm(OverallSlope~Linf, data=LinfFullTable, weights = weights)
summary(modLinfRegression)

#Model w Species- adding species to model to see if it is a significant predictor
modLinfRegression2<-lm(OverallSlope~Linf+Species, data=LinfFullTable, weights = weights)
summary(modLinfRegression2)
anova(modLinfRegression, modLinfRegression2) #species is a significant predictor; species term important to include

#interaction model-testing model with interaction between Linf and species is not applicable here, Linf values are species specific

#modLinfRegression2 is the top model- getting results
summary(modLinfRegression2)
confint(modLinfRegression2, 'Linf', level=0.95)

#######################################################################################################################
#Running weighted linear regression of change in body size per year against average body size at the population level (data average)
#to address the question: Do populations that have larger body sizes on average experience more significant body size changes? 

#Combining model result table with average population values:
popAvgFullTable<-merge(bodysizeModelResults, popAvgSize, by=c("Lake", "Species", "Gear"))
popAvgFullTable$Species<-as.factor(popAvgFullTable$Species)

#Simple model- regressing change in length per year against average population length values, weighted by inverse of variance
popAvgDataRegression<-lm(OverallSlope~avgPopLength, data=popAvgFullTable, weights = weights)
summary(popAvgDataRegression)

#Model w Species- adding species to model to see if it is a significant predictor
popAvgDataRegression2<-lm(OverallSlope~avgPopLength+Species, data=popAvgFullTable, weights = weights)
summary(popAvgDataRegression2)
anova(popAvgDataRegression, popAvgDataRegression2) #species is a significant term; so species term important to include

#Interaction model-testing if there is an interaction between species and average population length
popAvgDataRegression3<-lm(OverallSlope~avgPopLength*Species, data=popAvgFullTable, weights = weights)
summary(popAvgDataRegression3)
anova(popAvgDataRegression2, popAvgDataRegression3) #species interaction not significant; going to go with popAvgDataRegression2

#Going to go with popAvgDataRegression2
summary(popAvgDataRegression2)
confint(popAvgDataRegression2, 'avgPopLength', level=0.95)

#######################################################################################################################
#Running weighted linear regression of change in body size per year against range position to address the question:
#are populations that are more to the south of their native ranges more likely to be increasing in body size?

#Combining model result table with lake latitudes
lakeLatFullTable<-merge(bodysizeModelResults, lakeLats, by="Lake", all.x=F)
lakeLatFullTable<-unique(lakeLatFullTable) #for whatever reason get a few repeats, this removes them

#Initializing empty dataframe to enter ranges for each species:
rangePos<-data.frame(Species=unique(bodysizeModelResults$Species), latLow=numeric(length(unique(bodysizeModelResults$Species))), 
                     latHigh=numeric(length(unique(bodysizeModelResults$Species))))
#latLow is going to be the lower boundary of the species range, latHigh is going to be the upper boundary of the species range
rangePos<-rangePos[order(rangePos$Species),] #putting species in alphabetical order

#Inputting values for upper and lower boundary of each species range from Fishbase:
rangePos[rangePos$Species=="Arctic Char","latLow"]<-41
rangePos[rangePos$Species=="Arctic Char","latHigh"]<-82
rangePos[rangePos$Species=="Arctic Grayling","latLow"]<-44
rangePos[rangePos$Species=="Arctic Grayling","latHigh"]<-71
rangePos[rangePos$Species=="Round Whitefish","latLow"]<-41
rangePos[rangePos$Species=="Round Whitefish","latHigh"]<-72
rangePos[rangePos$Species=="Brook Trout","latLow"]<-34
rangePos[rangePos$Species=="Brook Trout","latHigh"]<-61
rangePos[rangePos$Species=="Lake Trout","latLow"]<-38
rangePos[rangePos$Species=="Lake Trout","latHigh"]<-75
rangePos[rangePos$Species=="Cisco","latLow"]<-38
rangePos[rangePos$Species=="Cisco","latHigh"]<-71
rangePos[rangePos$Species=="Vendace","latLow"]<-59
rangePos[rangePos$Species=="Vendace","latHigh"]<-66
rangePos[rangePos$Species=="Lake Whitefish","latLow"]<-41
rangePos[rangePos$Species=="Lake Whitefish","latHigh"]<-72
rangePos[rangePos$Species=="Common Whitefish","latLow"]<-40
rangePos[rangePos$Species=="Common Whitefish","latHigh"]<-73
#Hovsgol Grayling, Bonneville Whitefish, & Coregonus wartmanni are endemic species; going to manually input
#their range position as 0.5 once range position has been calculated

#Combining model result table (that now has lake latitudes) to table with range positions for each species
rangePosFullTable<-merge(lakeLatFullTable, rangePos, by="Species")

#Calculating range position for each lake
rangePosFullTable$rangePosition<-(rangePosFullTable$Latitude-rangePosFullTable$latLow)/(rangePosFullTable$latHigh-rangePosFullTable$latLow)

#Endemic species get range position of 0.5
rangePosFullTable[rangePosFullTable$Lake=="Lake Hovsgol", "rangePosition"]<-0.5 #Hovsgol Grayling are endemic to lake hovsgol
rangePosFullTable[rangePosFullTable$Lake=="Lake Constance", "rangePosition"]<-0.5 #Coregonus wartmanni are endemic to Lake Constance, 
rangePosFullTable[rangePosFullTable$Lake=="Bear Lake", "rangePosition"]<-0.5 #Bonneville whiteish are endemic to Lake Constance, 

rangePosFullTable$Species<-as.factor(rangePosFullTable$Species)

#Simple model- regressing change in length per year against range position, weighted by inverse of variance
modRangeRegression<-lm(OverallSlope~rangePosition, data=rangePosFullTable, weights = weights)
summary(modRangeRegression)

#Model w Species- adding species to model to see if it is a significant predictor
modRangeRegression2<-lm(OverallSlope~rangePosition+Species, data=rangePosFullTable, weights = weights)
summary(modRangeRegression2)
anova(modRangeRegression, modRangeRegression2) #species is a significant term; so species term important to include

#Interaction model-testing if there is an interaction between species and range position
modRangeRegression3<-lm(OverallSlope~rangePosition*Species, data=rangePosFullTable, weights = weights)
summary(modRangeRegression3)
anova(modRangeRegression2, modRangeRegression3) #species interaction is not significant

#Going to go with modRangeRegression2
summary(modRangeRegression2)
confint(modRangeRegression2, 'rangePosition', level=0.95)

#######################################################################################################################
#Running weighted linear regression of change in body size per year against average chlorophyll-a values; don't have chlorophyll data for every lake
#to address the question: are high productivity lakes experiencing more positive body size changes?

#Combining model result table with chlorophyll data:
chlaFullTable<-merge(bodysizeModelResults, chlaTab, by=c("Lake"), all.x=F) 
chlaFullTable<-unique(chlaFullTable) #getting rid of repeats

chlaFullTable$Species<-as.factor(chlaFullTable$Species)

#Simple model- regressing change in length per year against average chlorophyll, weighted by inverse of variance
chlaRegression<-lm(OverallSlope~AvgChlorophyll, data=chlaFullTable, weights = weights) 
summary(chlaRegression)

#Model w Species- adding species to model to see if it is a significant predictor
chlaRegression2<-lm(OverallSlope~AvgChlorophyll+Species, data=chlaFullTable, weights = weights) 
summary(chlaRegression2)
anova(chlaRegression, chlaRegression2) #species is not a significant term

#Interaction model-testing if there is an interaction between species and range position- not necessary because species not important factor
#chlaRegression3<-lm(OverallSlope~AvgChlorophyll*Species, data=chlaFullTableb, weights = weights)
#summary(chlaRegression3)
#anova(chlaRegression2, chlaRegression3) #interaction not significant factor

#Going with simple model, chlaRegression
summary(chlaRegression)
confint(chlaRegression, 'AvgChlorophyll', level=0.95)

#######################################################################################################################
#Running weighted linear regression of change in body size per year against average water temperature for each lake;
#don't have temp data for every lake
#to address the question: are lakes that are warmer on average experiencing more declines in body size?

#Combining model result table with average temp data:
LakeAvgTempFullTable<-merge(bodysizeModelResults, LakeAvgTemp, by.x="Lake")
length(unique(LakeAvgTempFullTable$Lake)) #172 lakes with temp data

LakeAvgTempFullTable$Species<-as.factor(LakeAvgTempFullTable$Species)

#Simple model- regressing change in length per year against average water temp, weighted by inverse of variance
avgTempRegression<-lm(OverallSlope~avgLakeTemp, data=LakeAvgTempFullTable, weights = weights)
summary(avgTempRegression)

#Model w Species- adding species to model to see if it is a significant predictor
avgTempRegression2<-lm(OverallSlope~avgLakeTemp+Species, data=LakeAvgTempFullTable, weights = weights)
summary(avgTempRegression2)
anova(avgTempRegression, avgTempRegression2) #species is significant term now that hovsgol out

#Interaction model-testing if there is an interaction between species and avg water temp- not necessary because species not important factor
avgTempRegression3<-lm(OverallSlope~avgLakeTemp*Species, data=LakeAvgTempFullTable, weights = weights)
summary(avgTempRegression3)
anova(avgTempRegression2, avgTempRegression3) #species interaction is not significant

#Going with avgTempRegression2
summary(avgTempRegression2)
confint(avgTempRegression, 'avgLakeTemp', level=0.95)

#######################################################################################################################
#Running weighted linear regression of change in body size per year against water temperature change over survey period
#for each lake; don't have temp data for every lake
#to address the question: are lakes that are experiencing more warming experiencing greater body size changes?

lakesWarming<-LakeTempChangeData[LakeTempChangeData$TempChange>0,]
length(unique(lakesWarming$Lake)) #136 unique lakes; 188 populations
lakesCooling<-LakeTempChangeData[LakeTempChangeData$TempChange<0,]
length(unique(lakesCooling$Lake)) #36 lakes; 43 populations

#Combining model result table (popTable from BodySizeMixedModelCode.R) with temp change data:
TempChangeFullTable<-merge(LakeTempChangeData, bodysizeModelResults, by=c("Lake", "Species"))
length(unique(TempChangeFullTable$Lake)) #172 lakes total with temperature data
TempChangeFullTable$Species<-as.factor(TempChangeFullTable$Species)

#Simple model- regressing change in length per year against change in temperature, weighted by inverse of variance
tempChangeRegression<-lm(OverallSlope~TempChange, data=TempChangeFullTable, weights = weights)
summary(tempChangeRegression)

#Model w Species- adding species to model to see if it is a significant predictor
tempChangeRegression2<-lm(OverallSlope~TempChange+Species, data=TempChangeFullTable, weights = weights)
summary(tempChangeRegression2)
anova(tempChangeRegression, tempChangeRegression2) #species is not significant term

#Interaction model-testing if there is an interaction between species and temp change- not necessary because species not important factor
#tempChangeRegression3<-lm(OverallSlope~TempChange*Species, data=TempChangeSlopeTable, weights = weights)
#summary(tempChangeRegression3)
#anova(tempChangeRegression2, tempChangeRegression3) #species interaction not significant factor

#Going with tempChangeRegression
summary(tempChangeRegression)
confint(tempChangeRegression, 'TempChange', level=0.95)

