###This code reads in the file: filteredData.csv which is the file that contains the datasets we compiled that have more than 
###10 observations and 5+ years of data (the filtered data). It then formats the data, runs the different model structures that 
###are included in the main body of the manuscript, and calculates the change in body size per year at the population level 
###(unique lake/species/gear combination) for the most parsimonious model (lowest AIC). It also calculates the variance for each 
###population from that model, which is used for the weights in the weighted linear regression secondary analyses (code for 
###running the secondary analyses can be found in BodySizeSecondaryAnalysesCode.R). Additionally, it calculates the average body
###size at the population level, needed for the secondary analyses, and saves it to a file titled BodySize_PopAvgSize.csv.

###The model results and average body sizes at the population level for the full dataset are uploaded on github

rm(list=ls())

#Loading in necessary packages
library("lme4")
library("lmerTest")
library('MuMIn')
library("tidyverse")

#Reading in data
data<-read.csv("Desktop/BodySizeGithubFiles/filteredData.csv", header=TRUE, stringsAsFactors = FALSE) #filtered data (min 10 obs, 5 years of data per gear)

#Formatting data
data$fGear<-factor(data$Gear)
data$fLake<-factor(data$Lake)
data$zLength<-scale(data$Length) #z-score-normalized length
data$zYear<-scale(data$Year) #z-score normalized year

###########################################################################################################################
###Running the top 3 models: top model

#Most parsimonious model in terms of AIC includes year as a fixed effect, gear nested within the lake random effect, which includes
#random slopes and intercepts, and random slopes and intercepts for the species random effect:
modelNested<-lmer(zLength~zYear+(zYear|fLake/fGear)+(zYear|Species), data=data, REML=T,
                  control = lmerControl(optimizer = "bobyqa"))
#includes random intercepts and slopes for year for the random effects; bobyqa optimizer added to help with convergence
summary(modelNested) #fixed effect estimate of year is significantly different from 0 if p<0.05
#slope (fixed effect estimate of year) from model output is in units of (standard deviation of lengths)/(standard deviation of years) due to scaling
backtransformedSlope<-(coef(summary(modelNested))[2]*attr(data$zYear, "scaled:scale"))/attr(data$zLength, "scaled:scale") #backtransforming the model slope to get units of mm/year (fixed effect estimate of year on length)

#getting 95% confidence intervals
confidenceInt<-confint(modelNested, 'zYear', level=0.95) #getting 95% CI for model slope
confintLowBack<-(confidenceInt[1]*attr(data$zYear, "scaled:scale"))/attr(data$zLength, "scaled:scale") #backtransforming lower boundary of CI to units of mm/year
confintHighBack<-(confidenceInt[2]*attr(data$zYear, "scaled:scale"))/attr(data$zLength, "scaled:scale") #backtransforming upper boundary of CI to units of mm/year

#getting R^2 values:
r.squaredGLMM(modelNested)[1] #marginal R^2 value of model
r.squaredGLMM(modelNested)[2] #conditional R^2 of model

#Running the same mixed model as above but this time without the random slope for year for species.
#This model assumes that the effect of year does not significantly differ by species:
modelNestedRed<-lmer(zLength~zYear+(zYear|fLake/fGear)+(1|Species), data=data, REML=T,
                  control = lmerControl(optimizer = "bobyqa"))#bobyqa optimizer added to help with convergence
summary(modelNestedRed)
anova(modelNested, modelNestedRed) #if pvalue less than 0.05, then the full model is more representative and the
#effect of year does significantly differ by species

#Running the same mixed model structure as above, but without year included. This model assumes there is no
#trend in body size over time:
modelNestednoYear<-lmer(zLength~1+(1|Species)+(1|fLake/fGear), data=data, 
                        REML=T, control = lmerControl(optimizer = "bobyqa"))
AIC(modelNested, modelNestedRed, modelNestednoYear) #to determine which model is the most parsimonious (lowest AIC)
#The full model (modelNested) is the most parsimonious.

###########################################################################################################################
###Running the top 3 models: second best model

#Second most parsimonious model in terms of AIC includes year as a fixed effect, random slopes and intercepts for 3 separate random
#effects-lake, species, and gear:
modelSeparate<-lmer(zLength~zYear+(zYear|fLake)+(zYear|Species)+(zYear|fGear), data=data, REML=T,
                    control = lmerControl(optimizer = "bobyqa"))
#model with random intercepts and slopes for each random effect; no nesting of random effects

#Running the same mixed model as above but this time without random slope for year for species
#This reduced model assumes that the effect of year does not significantly differ by species:
modelSeparateRed<-lmer(zLength~zYear+(zYear|fLake)+(1|Species)+(zYear|fGear), data=data, REML=T,
                       control = lmerControl(optimizer = "bobyqa"))
anova(modelSeparate, modelSeparateRed) #if pvalue less than 0.05, then the full model is more representative and the
#effect of year does significantly differ by species

#Running the same mixed model structure as above, but without year included. This model assumes there is no
#trend in body size over time:
modelSeparatenoYear<-lmer(zLength~1+(1|Species)+(1|fLake)+(1|fGear), data=data, 
                          REML=T, control = lmerControl(optimizer = "bobyqa"))
AIC(modelSeparate, modelSeparateRed, modelSeparatenoYear) #to determine which model is the most parsimonious (lowest AIC)
#The full model (modelSeparate) is the most parsimonious of these 3 models.

###########################################################################################################################
###Running the top 3 models: third best model

#Third most parsimonious model in terms of AIC includes year and gear as a fixed effect interaction, random slopes and 
#intercepts for 2 separate random effects-lake and species:
modelGearFixed<-lmer(zLength~zYear*fGear+(zYear|fLake)+(zYear|Species), data=data, 
                     REML=T, control = lmerControl(optimizer = "bobyqa")) 
#model with fixed interaction term between year and gear, random intercepts and slopes for lake and species

#Running the same mixed model structure as above but this time without random slope for year for species
#This reduced model assumes that the effect of year does not significantly differ by species:
modelGearFixedRed<-lmer(zLength~zYear*fGear+(zYear|fLake)+(1|Species), data=data, 
                        REML=T, control = lmerControl(optimizer = "bobyqa")) 
anova(modelGearFixed, modelGearFixedRed) #if pvalue less than 0.05, then the full model is more representative and the
#effect of year does significantly differ by species

#Running the same mixed model structure as above, but without year included. This model assumes there is no
#trend in body size over time:
modelGearFixednoYear<-lmer(zLength~Gear+(1|Species)+(1|fLake), data=data, 
                           REML=T, control = lmerControl(optimizer = "bobyqa"))
AIC(modelGearFixed, modelGearFixedRed, modelGearFixednoYear) #to determine which model is the most parsimonious (lowest AIC)
#The full model (modelGearFixed) is the most parsimonious of these 3 models.

###########################################################################################################################
#Comparing the most parsimonious models from each of the model groups above shows that the model with gear nested within the lake
#random effect is overall the most parsimonious, and thus the model used for the rest of the analyses:
AIC(modelNested, modelSeparate, modelGearFixed)

#We can also compare all of the models and AIC values in a table:
modelStructures<-c("zLength~zYear+(zYear|fLake/fGear)+(zYear|Species)", "zLength~zYear+(zYear|fLake/fGear)+(1|Species)",
                   "zLength~1+(1|Species)+(1|fLake/fGear)", "zLength~zYear+(zYear|fLake)+(zYear|Species)+(zYear|fGear)",
                   "zLength~zYear+(zYear|fLake)+(1|Species)+(zYear|fGear)","zLength~1+(1|Species)+(1|fLake)+(1|fGear)",
                   "zLength~zYear*fGear+(zYear|fLake)+(zYear|Species)", "zLength~zYear*fGear+(zYear|fLake)+(1|Species)",
                   "zLength~Gear+(1|Species)+(1|fLake)")
AICvalues<-c(AIC(modelNested), AIC(modelNestedRed), AIC(modelNestednoYear), AIC(modelSeparate), AIC(modelSeparateRed),
             AIC(modelSeparatenoYear), AIC(modelGearFixed), AIC(modelGearFixedRed), AIC(modelGearFixednoYear))
  
AICtable<-as.data.frame(cbind(modelStructures, AICvalues))
AICtable$AICvalues<-as.numeric(AICtable$AICvalues)
AICtable$deltaAIC<-AICtable$AICvalues-min(AICtable$AICvalues)
AICtable<-AICtable[order(AICtable$AICvalues, decreasing=F),]

###########################################################################################################################
#For the secondary analyses, need to calculate the change in length per year (the model slope) for each population 
#(each unique lake/gear/species combination). It needs to be backtransformed to units of mm/year, since we standardized 
#lengths and years. Also need to extract conditional standard deviations for each population and calculate conditional variances
#for each population. The inverse of the conditional variance for each population is then used to weight the secondary analyses.

#Doing so with the most parsimonious model only, which included included gear nested within lake: 
#zLength~zYear+(zYear|fLake/fGear)+(zYear|Species). 

#To get the overall change in length per year for each population (unique lake/species/gear combo), need to add the fixed effect of year (fixed slope)
#to the random effect of species (species slopes), random effect of lake (lake slopes), and random effect of gear within lake (gear slopes). Have to do
#the same thing with conditional standard deviations.

#Using ranef gives the slope of that random effect alone. Using coef adds the fixed effect of year to that random effect, so going to use coef for 
#only one of the random effects (instead of having to add fixed effect separately afterwards), If I extracted coef for each random effect, then the fixed 
#effect would be added 3 times instead of just once.

speciesSlopes<-coef(modelNested)$Species[,"zYear"] #storing the interept and slopes for species random effect; coef includes the fixed
#effect of year as well; thus the slope values are the changes in length/year for each species
Species<-rownames(coef(modelNested)$Species) #getting species names in the same order the slopes are listed in
speciesTable<-data.frame(Species=Species,speciesSlopes=speciesSlopes) #combining the species names with their respective slopes

lakeSlopes<-ranef(modelNested)$fLake[,"zYear"] #storing the intercept and slopes for the lake random effect; 
#slopes are change in length/year occuring in each lake
Lake<-rownames(ranef(modelNested)$fLake) #getting lake names in correct order
lakeTable<-data.frame(Lake=Lake, lakeSlopes=lakeSlopes) #combining the lake names in correct order with their respective slopes

gearSlopes<-ranef(modelNested)$`fGear:fLake`[,"zYear"] #storing the intercept and slopes for the gear nested within lake random effect;
#slopes are change in length/year for each gear within lake
Gear<-rownames(ranef(modelNested)$`fGear:fLake`) #getting species names in correct oder
gearTable<-data.frame(Gear=Gear,gearSlopes=gearSlopes) #combining gear names with their respective slopes

#Now have to do the same thing, but extract the standard deviation for each level of species, lake, and gear. Then
#can calculate standard deviation for each level of each random effect, and add everything together to get 1 overall
#slope (change in length/year) and 1 overall variance for each population (unique species/lake/gear combo).

stdDevMatrices<-as.data.frame(ranef(modelNested, condVar=T)) #as.data.frame gives random effect conditional values of mean and conditional standard deviations
#without as.data.frame, gives ranef as variance-covariance matrices.
#stdDevMatrices has 5 columns: grpvar is the random effect (species, lake, or gear nested within lake), group is the level of that random effect,
#term is whether it is referring to intercept or slope, and then condval and condsd are the conditional value of the mean and the conditional standard deviation for that level

#The code below extracts the standard deviation of each level of the random effects and squares it to get variance:
cvSpecies<-stdDevMatrices[stdDevMatrices$grpvar=="Species" & stdDevMatrices$term=="zYear",c(3:5)] #extracts sd of year random effect estimate for each species
colnames(cvSpecies)[which(names(cvSpecies) == "grp")] <- "Species" #changing name of grp column to Species
cvSpecies$conditionalVarSpecies<-(cvSpecies$condsd)^2 #squaring the standard deviation to get the variance

cvLakes<-stdDevMatrices[stdDevMatrices$grpvar=="fLake" & stdDevMatrices$term=="zYear",c(3:5)] #extracts sd of year random effect estimate for each lake
colnames(cvLakes)[which(names(cvLakes) == "grp")] <- "Lake" #changing name of grp column to Lake
cvLakes$conditionalVarLake<-(cvLakes$condsd)^2 #squaring the standard deviation to get the variance

cvGears<-stdDevMatrices[stdDevMatrices$grpvar=="fGear:fLake" & stdDevMatrices$term=="zYear",c(3:5)] #extracts sd of year random effect estimate for each gear
colnames(cvGears)[which(names(cvGears) == "grp")] <- "Gear" #changing name of grp column to Gear
cvGears$conditionalVarGear<-(cvGears$condsd)^2 #squaring standard deviation to get the variance

#This extracts the variance of the fixed effect of year from the variance-covariance matrix of the model
fixedVars<-diag(vcov(modelNested))[2] #because we used ranef for variance-covariance matrix, still need to add fixed in

#Combining the variance dataframes for each random effect with their respective slope dataframes
overallSpecies<-merge(speciesTable, cvSpecies[,c(1,4)], by.x="Species")
overallLakes<-merge(lakeTable, cvLakes[,c(1,4)], by.x="Lake")
overallGear<-merge(gearTable, cvGears[,c(1,4)], by.x="Gear")

#Because gear is nested within lake, the gear column consists of both gear and lake. Need to split gear column into 2 columns, gear & lake
#in order to combine variances with their respective gear/lake/species combo
overallGearSplit<-separate(overallGear, "Gear", into=c("Gear", "lake"),sep=":", extra="merge", fill="right") #splitting gear column into gear & lake
#But : is also used to separate gillnets from their mesh sizes for some of the gears, which means some of these lake names have part of the gear with it
#Need to separate the lake column again to get the lake names on their own for those rows
overallGearSplit<-separate(overallGearSplit, "lake", into=c("gearORlake", "Lake2"),sep=":", fill="right")
#Now need to divide the data into those that have the lake in the first lake column (gearORlake) and those that have it in the second column (Lake1)
dataPart1<-overallGearSplit[is.na(overallGearSplit$Lake2),c(1:2,4:5)] #if the second column is NA, then the lake name is in the gearORlake column
#dataPart1 now has all of the data that has the lake name in the gearORlake column
names(dataPart1)[names(dataPart1) == "gearORlake"] <- "Lake"
dataPart2<-overallGearSplit[!is.na(overallGearSplit$Lake2),] #if the second column is not NA, then gearORlake contains part of the gear
#and the lake name is in Lake1; dataPart2 has all of the data that has part of the gear name in gearORlake
dataPart2$Gear<-str_c(dataPart2$Gear, ":", dataPart2$gearORlake) #putting the gear name back together (since it was split up into 2 columns)
names(dataPart2)[names(dataPart2) == "Lake2"] <- "Lake" #Lake2 column has all of the lake names for this dataset
dataPart2<-dataPart2[,c(1,3:5)] #don't need the gearORlake column anymore
overallGear<-rbind(dataPart1, dataPart2) #combines the data together again; now have correct gear and lake names in their respective columns

#To calculate overall slope for each population, need to add together slope values for each level of each random effect in each population
#To do so, make one table that merges all of the slope tables (overallLakes, overallSpecies, overallGears)- slope is change in length/year

bodysizeModelResults<-tbl_df(data) %>%  #grouping data into populations: each row is unique species/lake/gear combination
  group_by(Species, Lake, Gear)%>%
  count()%>%
  distinct(Lake, Species, Gear)
bodysizeModelResults<-merge(bodysizeModelResults, overallLakes, by.x="Lake") #merges each population with the lake random effect slope
bodysizeModelResults<-merge(bodysizeModelResults, overallSpecies, by.x="Species") #merges each population with the species slope, which includes
#species random effect and the year fixed effect
bodysizeModelResults<-merge(bodysizeModelResults, overallGear, by=c("Gear", "Lake")) #merges each population with the gear random effect slope

bodysizeModelResults$OverallSlope<-bodysizeModelResults$lakeSlopes+bodysizeModelResults$speciesSlopes+bodysizeModelResults$gearSlopes #adding the lake slope and species slope and gear slope to get overall slope for each pop
#got species slopes using coef and then lake and gear slopes using ranef, so overall slope is correct: includes fixed effect of year once plus random effect
# of year for species (coef) plus random effect of year for lake (ranef) plus random effect of year for gear (ranef)

bodysizeModelResults$totalVar<-bodysizeModelResults$conditionalVarLake+bodysizeModelResults$conditionalVarSpecies+bodysizeModelResults$conditionalVarGear+fixedVars #adding up variance for each population; here the species
#variance does not include the fixed effect variance, so need to add that in separately

bodysizeModelResults<-bodysizeModelResults[,c(1:3, 10:11)] #reducing to rows needed for secondary analyses

bodysizeModelResults$OverallSlopeBacktransformed<-((bodysizeModelResults$OverallSlope)*attr(data$zYear, "scaled:scale"))/attr(data$zLength, "scaled:scale")
#adding column with the slopes backtransformed (so that units are of mm/year)

bodysizeModelResults<-bodysizeModelResults[,c("Lake", "Gear", "Species", "OverallSlope", "OverallSlopeBacktransformed", "totalVar")]

write.csv(x=bodysizeModelResults, file="Desktop/BodySizeGithubFiles/bodysizeModelResults.csv", row.names = F)

###########################################################################################################################
#Calculating the average body size at the population level for the secondary analyses:
popAvg<-tbl_df(data) %>% 
  group_by(Species, Lake, Gear)%>%
  mutate(avgPopLength=mean(Length))%>% #mean fish size for that species caught by that lake/gear combo
  select(Lake, Gear, Species, avgPopLength)
popAvg<-unique(popAvg)

write.csv(x=popAvg, file="Desktop/BodySizeGithubFiles/BodySize_PopAvgSize.csv", row.names = F)


