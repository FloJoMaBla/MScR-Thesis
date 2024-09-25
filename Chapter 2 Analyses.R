####Autumn migration departure and nest habitat####
AutDepDate<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Arr_Dep_Relationships/used/Autumn Departure Dates csv.csv")
AutDepDate$Effort=as.factor(AutDepDate$Effort)
summary(AutDepDate)
str(AutDepDate)
#this script looks only at gulls which nested in at least one year
library(lme4)
AutumnDepNestEff<-lmer(Dep_Date~Nest+Effort+(1|Gull),data=AutDepDate) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
#we look at all the possible interactions then remove the one which is least
#...significant and so on
summary(AutumnDepNestEff)
#Effort is least significant
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = AutumnDepNestEff)
plot(simulationOutput) #ok - spoke to RN
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
vif(AutumnDepNestEff) #all <1.11
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
var.test(Dep_Date ~ Nest, data = AutDepDate) #1.04...
var.test(Dep_Date ~ Effort, data = AutDepDate) #0.89 (without Archie)
#successful nest still least significant. T value lowest which shows this, so remove successful nesting
AutumnDepNest<-lmer(Dep_Date~Nest+(1|Gull),data=AutDepDate)
#find out if between leaving date is affected by nesting effort
anova(AutumnDepNestEff,AutumnDepNest)
#so effort is not sig at all in only nesters. chisq 0.01, df=1, p =0.931
summary(AutumnDepNest) 
#now to look at model for habitat
null<-lmer(Dep_Date~1+(1|Gull),data=AutDepDate)
anova(AutumnDepNest,null) #effect of habitat...?
#so nesting habitat does not have a sig difference chisq 0.76, df 1, p=0.385

####Autumn migration departure and nest outcome####
AutDepNE<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Arr_Dep_Relationships/used/AutumnDepDates HighLowEffort CSV.csv")
#NE = Nesting Effort
AutDepNE$HighEffort=as.factor(AutDepNE$HighEffort)
str(AutDepNE)
summary(AutDepNE)
#this one assumes that Bobby and Luna had a high nesting effort 
#We can do it again where they do not...
#this has been done using Julian Date
library(lme4)
AutumnDepNE<-lmer(LeavingDate~HighEffort+(1|Gull),data=AutDepNE)
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
#we look at all the possible interactions then remove the one which is least
#...significant and so on
summary(AutumnDepNE)
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = AutumnDepNEYear)
plot(simulationOutput) #all fine
#vif(AutumnDepNE) model has fewer than 2 terms
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
var.test(LeavingDate ~ HighEffort, data = AutDepNE) #ratio of variances is 1.03...good! Low number.
null<-lmer(LeavingDate~1+(1|Gull),data=AutDepNE) 
anova(AutumnDepNE,null) #compares these two models to find result for effort
#so nesting effort not sig, chisq 0.001 df 1 p=0.975

####Arrival to overwintering range####
Dep_Arr_Sites<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Arr_Dep_Relationships/used/Dep_Arr_One_Two_Sites.csv")
str(Dep_Arr_Sites)
summary(Dep_Arr_Sites)
WinterArrivalNest<-lmer(First_ow_Arrival~Autumn_Departure+Nest+(1|Gull),data=Dep_Arr_Sites)
summary(WinterArrivalNest) #lowest t value is nest hab
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = Dep_Arr_Sites)
plot(simulationOutput) #ok - checked w RN
vif(WinterArrivalNest) #1.00
#var.test(First_ow_Arrival ~ No_ow_Sites, data = LengthEachStop) #can't do as no 2 levels
WinterArrival<-lmer(First_ow_Arrival~Autumn_Departure+(1|Gull),data=Dep_Arr_Sites)
summary(WinterArrival)
anova(WinterArrivalNest,WinterArrival) #difference between birds from different habitats, chisq 0.01, df 1, p=0.918
null<-lmer(First_ow_Arrival~1+(1|Gull),data=Dep_Arr_Sites) #null model, not affected by autumn departure
anova(null,WinterArrival) #looking at effect of departure on arrival at primary o/w site
#significant as chisq 29.59, df 1 and p-value <0.001

####Effect of subsequent nesting on spring departure date####
SpringDepOn<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Arr_Dep_Relationships/used/SpringDep_Nesters_Non.csv")
summary(SpringDepOn)
library(lme4)
str(SpringDepOn)
#so the following model looks at whether departure affects subsequent nesting 
SpringDepNest<-lmer(Subsequent_Nest~Spring_Departure+(1|Gull),data=SpringDepOn) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
summary(SpringDepNest)
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SpringDepNest)
plot(simulationOutput) #all good
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
null<-lmer(Subsequent_Nest~1+(1|Gull),data=SpringDepOn) 
anova(SpringDepNest, null)
#no effect of subsequent nesting chisq 0.87 df 1, p=0.351

####Effect of subsequent nesting on spring arrival date####
SpringDepOn<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Arr_Dep_Relationships/used/SpringDep_Nesters_Non.csv")
summary(SpringDepOn)
library(lme4)
str(SpringDepOn)
SpringDepOn$Subsequent_Nest=as.factor(SpringDepOn$Subsequent_Nest)
#firstly, include everything in the model
#so the following model looks at whether year and subsequent breeding attempt affect arrival date
library(DHARMa)
library(car)
SpringArrNest<-lmer(Spring_Arrival~Subsequent_Nest+(1|Gull),data=SpringDepOn) 
summary(SpringArrYear) 
simulationOutput <- simulateResiduals(fittedModel = SpringArrNest)
plot(simulationOutput) #ok
var.test(Spring_Arrival ~ Subsequent_Nest, data = SpringDepOn) 
null<-lmer(Spring_Arrival~1+(1|Gull),data=SpringDepOn) #null model, not affected by subsequent nesting behaviour
anova(null,SpringArrNest) #this shows the difference by subsequent nesting, chisq 4.44 df 1 p=0.035

####Number of days on migration####
LengthStopovers<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Travel Days/used/Length_Stopovers_Season d2.csv")
summary(LengthStopovers)
library(lme4)
str(LengthStopovers)
SpeedSeasonGull<-lmer(No_Days_journey_final~Season+(1|Gull),data=LengthStopovers) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SpeedSeasonGull)
plot(simulationOutput) #all good
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
var.test(No_Days_journey_final ~ Season, data = LengthStopovers) #9.5 result... too high? Sorted
#var.test(No.Stops ~ Year, data = NumberStopovers) can't do for year as must have 2 levels
#season spring still least significant from summary output. T value lowest which shows this, so remove year
null<-lmer(No_Days_journey_final~1+(1|Gull),data=LengthStopovers) #null model, not affected by season
anova(SpeedSeasonGull,null) #all models should only differ by one variable 
#season significant chisq 3.97 df 1, p=0.046
library(MuMIn)
r.squaredGLMM(SpeedSeasonGull)

####Average number of travel days by season####
SeasonalTravel<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Travel Days/used/TravelDays_OW.csv")
str(SeasonalTravel)
summary(SeasonalTravel)
library(lme4)
TravelDaysSeason<-lmer(Number_Travel_Days_PnS~Season+(1|Gull),data=SeasonalTravel)
summary(TravelDaysSeason) 
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = TravelDaysSeason)
plot(simulationOutput) #all good
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
var.test(Number_Travel_Days_PnS~Season, data = SeasonalTravel) #1.1
null<-lmer(Number_Travel_Days_PnS~1+(1|Gull),data=SeasonalTravel)
anova(TravelDaysSeason,null)
#no difference in travel days chisq 0.01, df 1, p=0.936
r.squaredGLMM(TravelDaysSeason)

####The difference in the number of stopovers per season####
NumberStopovers<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Travel Days/used/Number_Stopovers_Season.csv")
summary(NumberStopovers)
library(lme4)
str(NumberStopovers)
#firstly, include everything in the model
StopsSeasonGull<-lmer(No.Stops~Season+(1|Gull),data=NumberStopovers) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
summary(StopsSeasonGull)
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = StopsSeasonGull)
plot(simulationOutput) #all good
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
var.test(No.Stops ~ Season, data = NumberStopovers) #0.8...
null<-lmer(No.Stops~1+(1|Gull),data=NumberStopovers) #null model, not affected by year season
anova(null,StopsSeasonGull) #all models should only differ by one variable
#chisq 0.83, df 1, p=0.361
r.squaredGLMM(StopsSeasonGull)

####Average stopover length per season####
LengthEachStop<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Travel Days/used/Av length of each stopover.csv")
str(LengthEachStop)
summary(LengthEachStop)
library(lme4)
LengthSeason<-lmer(Length_of_Stop~Season+(1|Gull),data=LengthEachStop) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
summary(LengthSeason) 
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = LengthSeason)
plot(simulationOutput) #fine w year
#vif(LengthSeason) #fewer than 2 terms
#tests correlation - high vif = high correlation = should be low (<5), eg body weight and body mass would correlate
#F-tests to compare variances (that is in base R)
var.test(Length_of_Stop ~ Season, data = LengthEachStop) #0.627 result
#var.test(No.Stops ~ Year, data = NumberStopovers) can't do for year as must have 2 levels
#season spring still least significant from summary output. T value lowest which shows this, so remove year
null<-lmer(Length_of_Stop~1+(1|Gull),data=LengthEachStop) #relationship between stopovers and season
anova(null,LengthSeason) #all models should only differ by one variable
#season insignificant chisq 1.37 df 1 p=0.241
r.squaredGLMM(LengthSeason)

####Number of days stopped over between seasons####
LengthStopovers<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Travel Days/used/Length_Stopovers_Season d2.csv")
summary(LengthStopovers)
library(lme4)
str(LengthStopovers)
#firstly, include everything in the model EXCEPT for number of days of travel
LengthSeasonGull<-lmer(No_Days_Stopped_Final~Season+(1|Gull),data=LengthStopovers) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
summary(LengthSeasonGull) #lowest t value is seasonspring
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = LengthSeasonGull)
plot(simulationOutput) #all good
vif(LengthSeasonYearGull) #vif low <1.2
var.test(No_Days_Stopped_Final ~ Season, data = LengthStopovers) #3.29
#var.test(No.Stops ~ Year, data = NumberStopovers) can't do for year as must have 2 levels
#season spring still least significant from summary output. T value lowest which shows this, so remove year
null<-lmer(No_Days_Stopped_Final~1+(1|Gull),data=LengthStopovers) #null model, not affected by season
anova(LengthSeasonGull,null) #all models should only differ by one variable
#season not sig, chisq 0.23, df 3, p=0.628
r.squaredGLMM(LengthSeasonGull)

####Autumn arrival and departure####


####Spring arrival and departure####
SpringDepArr1<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Arr_Dep_Relationships/used/Spring_dep_arr.csv")
summary(SpringDepArr1)
library(lme4)
str(SpringDepArr1)
#firstly, include everything in the model
#so the following model looks at whether departure date affects arrival date in spring
SpringDepArr<-lmer(Spring_Arrival~Spring_Departure+(1|Gull),data=SpringDepArr1) 
#this 1|Gull sign means that it takes into account the fact that some of the data is within the same gull
summary(SpringDepArr)
#Year has lowest t value
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = SpringDepArr)
plot(simulationOutput) #all good
vif(SpringDepArr) 
#tests correlation - vif < 1.5
#var.test(No.Stops ~ Year, data = NumberStopovers) can't do for year as must have 2 levels
#year still least significant from summary output. T value lowest which shows this, so remove year
null<-lmer(Spring_Arrival~1+(1|Gull),data=SpringDepArr1) #null model, not affected by departure date
anova(null,SpringDepArr) #this shows the difference by arrival only, non sig at chi 0.0879 df 1 p=0.767
#now to do a correlation matrix for autumn and spring departure and arrival
#first I need to make four vectors of dates
autumn_departure_vector<-c(212,210,204,252,244,334,193,211,216,187,284,294,249,220,242,233,212)
autumn_arrival_vector<-c(215,213,225,295,288,344,196,212,217,330,291,300,263,224,295,245,253)
spring_departure_vector<-c(88,78,98,60,105,75,79,91,87,79,54)
spring_arrival_vector<-c(94,94,107,122,109,121,104,140,106,102,96)
autumn_matrix <- data.frame(autumn_departure_vector,autumn_arrival_vector)
spring_matrix<-data.frame(spring_departure_vector,spring_arrival_vector)
correlation_autumn <- cor(autumn_matrix)
print(correlation_autumn)
correlation_spring <- cor(spring_matrix)
print(correlation_spring)

###Effect of nesting effort on distance travelled
WinterDistance<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Distance between ow and stopovers/overwintering distances csv.csv")
WinterDistance$Year=as.factor(WinterDistance$Year)
summary(WinterDistance)
library(lme4)
WintDistEffort<-lmer(Distance_from_nest_km~Nest_Effort+(1|Gull),data=WinterDistance) 
summary(WintDistEffort)
#drop year
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = WintDistEffort)
plot(simulationOutput) #all good
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
var.test(Distance_from_nest_km ~ Nest_Effort, data = WinterDistance) #low 0.74
null<-lmer(Distance_from_nest_km~1+(1|Gull),data=WinterDistance) 
anova(WintDistEffort, null)
#no difference by effort chisq 3.67, df 1 p=0.055

####Effect of overwintering distance on spring arrival####
Distance_Wintering<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Chapter 2 Analyses/Distance between ow and stopovers/overwintering distances csv.csv")
summary(Distance_Wintering)
library(lme4)
WintDistSpring<-lmer(Spring_Arrival~Distance_from_nest_km+(1|Gull),data=Distance_Wintering) 
summary(WintDistSpring)
#drop year
library(DHARMa)
library(car)
#applying DHARMa
simulationOutput <- simulateResiduals(fittedModel = WintDistSpring)
plot(simulationOutput) #all good - discussed
#if error, make sure factors are input as factors (eg year, successful nesting)
#do this using str(DatasetName) then DatasetName$ColumnToChange=as.factor(DatasetName$ColumnToChange)
#var.test(Spring_Arrival ~ Nest_Effort, data = Distance_Wintering) #low 0.6 - removed year
WintDistSpring<-lmer(Spring_Arrival~Distance_from_nest_km+(1|Gull),data=Distance_Wintering) 
null<-lmer(Spring_Arrival~1+(1|Gull),data=Distance_Wintering) 
anova(WintDistSpring, null)
#spring arrival not affected by distance, chisq 0.838 df 1 p=0.360
