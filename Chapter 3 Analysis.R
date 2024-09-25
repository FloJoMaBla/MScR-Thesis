####Effect of migration distance on niche-overlap####
DistNiche<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/R analyses/Overwintering distances n Schoeners D.csv")
str(DistNiche)
summary(DistNiche)
DistNiche$arcsin_overlap<-asin(sqrt(DistNiche$Niche_Overlap))
#looking at whether niche overlap changes with migration distance, not including nest
library(lme4)
NicheOverlapDistance<-lmer(arcsin_overlap~Furthest_Dist+(1|Gull),data=DistNiche) 
summary(NicheOverlapDistance)
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = NicheOverlapDistance)
plot(simulationOutput) #all good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
null<-lmer(arcsin_overlap~1+(1|Gull),data=DistNiche)
anova(NicheOverlapDistance,null) #no difference in niche overlap by distance
#chisq 0.09 df 1, p=0.763
null<-lmer(Urban_Use~1+(1|Gull),data=HabitatUse) #null model

####Niche overlap between urban and non-urban nesters####
DistHabitat<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/R analyses/Overwintering distances n Schoeners D.csv")
str(DistHabitat)
DistHabitat$Year=as.factor(DistHabitat$Year)
summary(DistHabitat)
DistHabitat$arcsin_overlap<-asin(sqrt(DistHabitat$Niche_Overlap))
library(lme4)
NicheOverlapNest<-lmer(arcsin_overlap~Nest+(1|Gull),data=DistHabitat) 
summary(NicheOverlapNest)
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = NicheOverlapNest)
plot(simulationOutput) 
null<-lmer(arcsin_overlap~1+(1|Gull),data=DistHabitat)
anova(NicheOverlapNest,null) #difference in niche overlap by nest = significant!
#chisq 5.07 df 1 p=0.024
null<-lmer(Urban_Use~1+(1|Gull),data=HabitatUse) #null model

HabitatUse<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/R analyses/Urban NonUrban Habitat Use.csv")
str(HabitatUse)
summary(HabitatUse)
HabitatUse$Year=as.factor(HabitatUse$Year)
#finding out if urban area use is affected by nesting and season
#arcsin square root to transform the data – arcsin square root of each proportion 
#do this for all proportional comparisons
# Identify numeric columns
numeric_columns <- sapply(HabitatUse, is.numeric)
# Apply the arcsine square root transformation only to the numeric columns as working with proportional data
HabitatUse[numeric_columns] <- lapply(HabitatUse[numeric_columns], function(x) asin(sqrt(x)))
# 'data' now contains transformed numeric columns and unchanged categorical columns
library(lme4)
UrbanUseSeasonNest<-lmer(Urban_Use~Season+Nest+(1|Gull),data=HabitatUse) 
summary(UrbanUseSeasonNest)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = UrbanUseSeasonNest)
plot(simulationOutput) #all good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
vif(UrbanUseSeasonNest) #result is season and nest 1.0
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
var.test(Urban_Use ~ Season, data = HabitatUse) #0.99
UrbanUseNest<-lmer(Urban_Use~Nest+(1|Gull),data=HabitatUse)
summary(UrbanUseNest)
anova(UrbanUseSeasonNest,UrbanUseNest) #urban areas were used nearly* significantly less in winter by the population
#chisq 3.02 df 1 p=0.083
null<-lmer(Urban_Use~1+(1|Gull),data=HabitatUse) #null model
#this anova will look at the effect of habitat only
anova(null,UrbanUseNest)
#overall use of urban areas in both seasons is not affected by nest type chisq 1.64, df 1, p=0.1998
#but what if we only look at the breeding season?

UrbanUseSeasonNest_Interact <- lmer(Urban_Use ~ Season * Nest + (1 | Gull), data = HabitatUse)
summary(UrbanUseSeasonNest_Interact)
#drop the interaction first
simulationOutput <- simulateResiduals(fittedModel = UrbanUseSeasonNest_Interact)
plot(simulationOutput) #all good
vif(UrbanUseSeasonNest_Interact) #all good <1.5
null<-lmer(Urban_Use ~ 1 + (1 | Gull), data = HabitatUse) 
anova(UrbanUseSeasonNest_Interact, null)
#effect of season and nest is significant: chisq 32.612, df 3, p<0.001
#urban use higher in urban birds than non urban birds
urban_nesters <- subset(HabitatUse, Nest == "Urban")
non_urban_nesters <- subset(HabitatUse, Nest == "Non-Urban")
model_urban <- lmer(Urban_Use ~ Season + (1 | Gull), data = urban_nesters)
summary(model_urban)
null <- lmer(Urban_Use ~ 1 + (1 | Gull), data = urban_nesters)
anova(model_urban,null)
#significant difference in urban use between seasons in urban nesters (lower in winter)
##chisq 22.13, df 1, p<0.001
model_non_urban <- lmer(Urban_Use ~ Season + (1 | Gull), data = non_urban_nesters)
summary(model_non_urban)
null <- lmer(Urban_Use ~ 1 + (1 | Gull), data = non_urban_nesters)
anova(model_non_urban,null)
#non-urban nesters do not show a significantly different proportional use of urban areas between seasons
#chisq 0.01, df 1 , p=0.926

#difference between urban and non urban nesters per season?
#subset just to the breeding season
summer_only <- subset(HabitatUse, Season == "Summer")
winter_only <- subset(HabitatUse, Season == "Winter")
UrbanUseSummer<-lmer(Urban_Use~Nest+(1|Gull),data=summer_only) 
summary(UrbanUseSummer)
null<-lmer(Urban_Use~1+(1|Gull),data=summer_only) 
anova(UrbanUseSummer, null)
#urban nesters use urban areas significantly more than non-urban nesters in summer
#chisq 12.94, df 1, p<0.001
UrbanUseWinter<-lmer(Urban_Use~Nest+(1|Gull),data=winter_only)
null<-lmer(Urban_Use~1+(1|Gull),data=winter_only) 
anova(UrbanUseWinter,null)
#but not significantly different in winter chisq 0.690 df 1 p=0.406

#proportional habitat use in winter in birds in Europe and Africa
WinterUseByContinent<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/R analyses/Habitat use by continent.csv")
str(WinterUseByContinent)
summary(WinterUseByContinent)
WinterUseByContinent$Year=as.factor(WinterUseByContinent$Year)
#finding out if urban area use is affected by nesting and season
#arcsin square root to transform the data – arcsin square root of each proportion 
#do this for all proportional comparisons
# Identify numeric columns
numeric_columns <- sapply(WinterUseByContinent, is.numeric)
# Apply the arcsine square root transformation only to the numeric columns as working with proportional data
WinterUseByContinent[numeric_columns] <- lapply(WinterUseByContinent[numeric_columns], function(x) asin(sqrt(x)))
# 'data' now contains transformed numeric columns and unchanged categorical columns
winter_only <- subset(WinterUseByContinent, Season == "Winter")
#comparing continents only in winter
#first looking only at bare ground
winter_only_bareground<-subset(winter_only, LC_cat == "Bare_ground")
HabitatUseByContinentBG<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_bareground) #bare ground used more in one continent than another?
summary(HabitatUseByContinentBG)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_bareground)
#was bare ground used significantly more in one continent than another?
anova(HabitatUseByContinentBG, null)
#yes, bare ground use was significantly higher in Africa (see summary output and excel)
#chisq 13.72 df 1 p<0.001

##now for built up habitat
winter_only_builtup<-subset(winter_only, LC_cat == "Built_area")
HabitatUseByContinentBU<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_builtup) #bare ground used more in one continent than another?
summary(HabitatUseByContinentBU)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_builtup)
#was built area (ie urban) used significantly more in one continent than another?
anova(HabitatUseByContinentBU, null)
#no, both were used similarly chisq 2.22 df 1 p=0.137

#for crops now
winter_only_crops<-subset(winter_only, LC_cat == "Crops")
HabitatUseByContinentC<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_crops) #bare ground used more in one continent than another?
summary(HabitatUseByContinentC)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_crops)
#was built area (ie urban) used significantly more in one continent than another?
anova(HabitatUseByContinentC, null)
#no, both were used similarly chisq 1.898 df 1 p=0.168

#now for Flooded vegetation
winter_only_flooded<-subset(winter_only, LC_cat == "Flooded_Vegetation")
HabitatUseByContinentFV<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_flooded) #bare ground used more in one continent than another?
summary(HabitatUseByContinentFV)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_flooded)
#was built area (ie urban) used significantly more in one continent than another?
anova(HabitatUseByContinentFV, null)
#yes, used significantly more in Europe chisq 4.27 df 1 p=0.039

#now for Rangeland
winter_only_rangeland<-subset(winter_only, LC_cat == "Rangeland")
HabitatUseByContinentR<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_rangeland) #bare ground used more in one continent than another?
summary(HabitatUseByContinentR)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_rangeland)
#was rangeland used significantly more in one continent than another?
anova(HabitatUseByContinentR, null)
#no, used simiarly between continents chisq 2.91 df 1 p=0.088

#now for Sea Water (marine)
winter_only_sea<-subset(winter_only, LC_cat == "Sea_water")
HabitatUseByContinentSW<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_sea) #bare ground used more in one continent than another?
summary(HabitatUseByContinentSW)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_sea)
#was sea water used significantly more in one continent than another?
anova(HabitatUseByContinentSW, null)
#no, used similarly between continents chisq 0.01 df 1 p=0.925

#Trees
winter_only_trees<-subset(winter_only, LC_cat == "Trees")
HabitatUseByContinentT<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_trees) #bare ground used more in one continent than another?
summary(HabitatUseByContinentT)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_trees)
#was sea water used significantly more in one continent than another?
anova(HabitatUseByContinentT, null)
#yes, trees used more in Europe than Africa, likely because of a higher presence of these
#chisq 4.09, df 1, p=0.043

#water
winter_only_water<-subset(winter_only, LC_cat == "Water")
HabitatUseByContinentW<-lmer(Prop_use~Continent+(1|Gull),data=winter_only_water) #bare ground used more in one continent than another?
summary(HabitatUseByContinentW)
null<-lmer(Prop_use~1+(1|Gull),data=winter_only_water)
#was sea water used significantly more in one continent than another?
anova(HabitatUseByContinentW, null)
#water used similarly between continents
#chisq <0.01, df 1, p=0.983

####now for the same but by season rather than by habitat####
SeasonalUse<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/R analyses/Habitat use by continent.csv")
str(SeasonalUse)
summary(SeasonalUse)
SeasonalUse$Year=as.factor(SeasonalUse$Year)
#finding out if urban area use is affected by nesting and season
#arcsin square root to transform the data – arcsin square root of each proportion 
#do this for all proportional comparisons
# Identify numeric columns
numeric_columns <- sapply(SeasonalUse, is.numeric)
# Apply the arcsine square root transformation only to the numeric columns as working with proportional data
SeasonalUse[numeric_columns] <- lapply(SeasonalUse[numeric_columns], function(x) asin(sqrt(x)))
# 'data' now contains transformed numeric columns and unchanged categorical columns
library(lme4)
bare_ground_by_season<-subset(SeasonalUse, LC_cat == "Bare_ground")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=bare_ground_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #ok
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = bare_ground_by_season) #0.016...
null<-lmer(Prop_use~1+(1|Gull),data=bare_ground_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of bare ground between seasons was significantly different (higher in winter)
#chisq 14.8, df 1, p<0.001

#now built area
builtarea_by_season<-subset(SeasonalUse, LC_cat == "Built_area")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=builtarea_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = builtarea_by_season) #1.01...
null<-lmer(Prop_use~1+(1|Gull),data=builtarea_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of built area between seasons was significantly different (higher in summer)
#chisq 7.13, df 1, p=0.008

####now crops####
crops_by_season<-subset(SeasonalUse, LC_cat == "Crops")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=crops_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = crops_by_season) #0.34...
null<-lmer(Prop_use~1+(1|Gull),data=crops_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of crops between seasons was NOT significantly different 
#chisq 0.31, df 1, p=0.579

##now flooded vegetation
flood_by_season<-subset(SeasonalUse, LC_cat == "Flooded_Vegetation")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=flood_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #ok
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = flood_by_season) #0.017...
null<-lmer(Prop_use~1+(1|Gull),data=flood_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of flooded veg between seasons was significantly different 
#chisq 10.29, df 1, p=0.001

##now rangeland
range_by_season<-subset(SeasonalUse, LC_cat == "Rangeland")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=range_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = range_by_season) #2.14...
null<-lmer(Prop_use~1+(1|Gull),data=range_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of rangeland between seasons was significantly different, more used in summer
#chisq 20.64, df 1, p<0.001

###now sea water
seawater_by_season<-subset(SeasonalUse, LC_cat == "Sea_water")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=seawater_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = seawater_by_season) #0.06...
null<-lmer(Prop_use~1+(1|Gull),data=seawater_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of sea water between seasons was technically not significantly different
#chisq 3.587, df 1, p=0.058

###now trees
trees_by_season<-subset(SeasonalUse, LC_cat == "Trees")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=trees_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = trees_by_season) #1.65...
null<-lmer(Prop_use~1+(1|Gull),data=trees_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of trees between seasons was significantly different
#chisq 32.51, df 1, p<0.001

####and finally water
water_by_season<-subset(SeasonalUse, LC_cat == "Water")
SeasonHabUse<-lmer(Prop_use~Season+(1|Gull),data=water_by_season) 
summary(SeasonHabUse)
#season is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SeasonHabUse)
plot(simulationOutput) #good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Prop_use ~ Season, data = water_by_season) #0.57...
null<-lmer(Prop_use~1+(1|Gull),data=water_by_season) #null model
#this anova will look at the effect of season
anova(null,SeasonHabUse)
#overall use of water between seasons was significantly different
#chisq 7.01, df 1, p=0.008

#now, does urban use drop significantly more in urban nesters than non urban nesters from summer to winter?
HabitatUse<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/R analyses/Urban NonUrban Habitat Use.csv")
str(HabitatUse)
summary(HabitatUse)
HabitatUse$Year=as.factor(HabitatUse$Year)
#finding out if urban area use is affected by nesting and season
#arcsin square root to transform the data – arcsin square root of each proportion 
#do this for all proportional comparisons
# Identify numeric columns
numeric_columns <- sapply(HabitatUse, is.numeric)
# Apply the arcsine square root transformation only to the numeric columns as working with proportional data
HabitatUse[numeric_columns] <- lapply(HabitatUse[numeric_columns], function(x) asin(sqrt(x)))
# 'data' now contains transformed numeric columns and unchanged categorical columns
library(lme4)
SeasonHabUse <- lmer(Urban_Use ~ Season * Nest + (1|Gull), data=HabitatUse)
summary(SeasonHabUse)
null<-lmer(Urban_Use ~ 1 + (1|Gull), data=HabitatUse)
anova(SeasonHabUse,null)
