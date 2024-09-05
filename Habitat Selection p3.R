library(mgcv)
library(ggeffects)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)

HabSel<-st_drop_geometry(HSdf) 

# Recode "Wintering 2" into "Wintering"
HabSel$Season<-HabSel$Season
HabSel$Season<-recode(HabSel$Season,
                      "Wintering 2"= "Wintering")

# Ensure LC_cat, Nest, and SEASON are factors
HabSel$LC_cat <- as.factor(HabSel$LC_cat)
HabSel$Nest <- as.factor(HabSel$Nest)
HabSel$Season <- as.factor(HabSel$Season)
HabSel$ID<-as.factor(HabSel$ID)
levels(HabSel$Season) 

# first look at simple effect of LC on presence
#independent of season, what is selected for...
model_0.gam <- bam(Pres ~ LC_cat  + s(ID, bs = "re"), data = HabSel, family = binomial)
# Summary of the model
summary(model_0.gam)
# Generate and plot effects for the interaction terms
effects_0.gam <- ggpredict(model_0.gam, terms = c("LC_cat"))
plot(effects_0.gam)
plot(effects_0.gam) +
  labs(x = "Landcover Category", y = "Selection")+ylim(0,1)

# This model will help determine if birds as a population select for the same habitat between seasons.


model_1.gam <- bam(Pres ~ LC_cat  + LC_cat:Season + s(ID, bs = "re"), data = HabSel, family = binomial)
# Summary of the model
summary(model_1.gam)
# Generate and plot effects for the interaction terms
effects_1.gam <- ggpredict(model_1.gam, terms = c("LC_cat", "Season"),
                           type="fixed")
plot(effects_1.gam)+labs(x = "Landcover Category", y = "Selection")+ylim(0,1)

# look at differences by individuals
#this looks at all gulls selection in both summer and winter for overall selection
model_2.gam <- bam(Pres ~ 
                     LC_cat  +           # effect of land cover
                     LC_cat:Season +     # effect of land cover by season
                     LC_cat:ID+          # effect of land cover by individual
                     s(ID, bs = "re"), data = HabSel, family = binomial)

#I looked at effect regardless of season as well by excluding the Season line of code

# Summary of the model
summary(model_2.gam)
# Generate and plot effects for the interaction terms
effects_2.gam <- ggpredict(model_2.gam, terms = c( "ID", "LC_cat"))
plot(effects_2.gam) 

model_2.gamMMMM <- bam(Pres ~ 
                         ID+
                         LC_cat  +           # effect of land cover
                         LC_cat:Season +     # effect of land cover by season
                         LC_cat:ID:year,          # effect of land cover by individual
                       #  s(ID, bs = "re"),
                       data = HabSel, family = binomial)

# Summary of the model
summary(model_2.gamMMMM)
# Generate and plot effects for the interaction terms
effects_2.gamMMMM <- ggpredict(model_2.gamMMMM, terms = c( "ID", "LC_cat", "year"))
plot(effects_2.gamMMMM) 

effects_2.gamMM <- ggpredict(model_2.gamMMMM, terms = c( "ID", "year", "LC_cat"))
plot(effects_2.gamMMMM) 
#looking at individuals by year ^

# Determine Habitat Selection in the Breeding Season
# First, analyze the habitat selection during the breeding season to classify birds as urban or non-urban selectors.

# Subset data for the breeding season
HabSel_breeding <- subset(HabSel,HabSel$Season == "Breeding")

# Fit a model for the breeding season
breeding_model <- bam(Pres ~ LC_cat + s(ID, bs = "re"), data = HabSel_breeding, family = binomial)
# Summary of the model
summary(breeding_model)
# Generate and plot effects for the breeding season
breeding_effects <- ggpredict(breeding_model, terms = c("LC_cat"))
plot(breeding_effects)

#this is the same as above


# with interaction with ID (this can also be used for nesters and non nesters)
breeding_model_ID <- bam(Pres ~
                           LC_cat +
                           LC_cat:ID +
                           s(ID, bs = "re"), data = HabSel_breeding, family = binomial)
# Summary of the model
summary(breeding_model_ID)
# Generate and plot effects for the breeding season
breeding_effects_ID <- ggpredict(breeding_model_ID, terms = c("ID", "LC_cat"))
plot(breeding_effects_ID)

breeding_effects_ID

# example: check if this changes between years (for one individual only)
#remove year to look at hab selection in the breeding season by individual, such as when comparing nesters
HabSel_breeding_Eric <- subset(HabSel, 
                               HabSel$Season == "Breeding" & HabSel$ID=="Eric")

HabSel_breeding_Eric
HabSel_breeding_Eric$year
HabSel_breeding_Eric$year<-as.factor(HabSel_breeding_Eric$year)
levels(HabSel_breeding_Eric$year)

breeding_model_Eric <- bam(Pres ~
                             LC_cat +
                             LC_cat:year ,
                           data = HabSel_breeding_Eric, family = binomial)

summary(breeding_model_Eric)
# Generate and plot effects for the breeding season
#remove the part after 'terms' if plotting for birds only tagged for one year
#view 'breeding_effects_Gull' to see selection
breeding_effects_Eric<-ggpredict(breeding_model_Eric,terms = c("year", "LC_cat"))
plot(breeding_effects_Eric)
#breeding_effects_Gull shows the selection in the breeding season by year
#breeding_effects_Gary

#change the axes to make it easier to compare each landcover type
breeding_effects_Gary<- ggpredict(breeding_model_Gary, terms = c("LC_cat", "year"))
plot(breeding_effects_Gary)

#this is interesting and I can use the plots for gulls followed for >1 year, but I need to use Schoener's D to make a formal comparison between seasons

####now do the same to compare gull winters to one another####
# Subset data for the breeding season
HabSel_wintering <- subset(HabSel,HabSel$Season == "Wintering")
# Fit a model for the wintering season
wintering_model <- bam(Pres ~ LC_cat + s(ID, bs = "re"), data = HabSel_wintering, family = binomial)
# Summary of the model
summary(wintering_model)
# Generate and plot effects for the wintering season
wintering_effects <- ggpredict(wintering_model, terms = c("LC_cat"))
plot(wintering_effects)


# with interaction with ID
wintering_model_ID <- bam(Pres ~
                            LC_cat +
                            LC_cat:ID +
                            s(ID, bs = "re"), data = HabSel_wintering, family = binomial)
# Summary of the model
summary(wintering_model_ID)
# Generate and plot effects for the wintering season
wintering_effects_ID <- ggpredict(wintering_model_ID, terms = c("ID", "LC_cat"))
plot(wintering_effects_ID)

wintering_effects_ID

# example: check if this changes between years (for one individual only)
HabSel_wintering_Eric <- subset(HabSel, 
                                HabSel$Season == "Wintering" & HabSel$ID=="Eric")

HabSel_wintering_Eric
HabSel_wintering_Eric$year
HabSel_wintering_Eric$year<-as.factor(HabSel_wintering_Eric$year)
levels(HabSel_wintering_Eric$year)

wintering_model_Eric <- bam(Pres ~
                              LC_cat +
                              LC_cat:year ,
                            data = HabSel_wintering_Eric, family = binomial)

summary(wintering_model_Eric)
# Generate and plot effects for the wintering season
wintering_effects_Eric<- ggpredict(wintering_model_Eric, terms = c("year", "LC_cat"))
plot(wintering_effects_Eric)

wintering_effects_Eric<- ggpredict(wintering_model_Eric, terms = c("LC_cat", "year"))
plot(wintering_effects_Eric)



# question 2
# Fit the GAM to assess individual-level niche-tracking behavior
model_individual <- bam(Pres ~ LC_cat * ID* SEASON*year, data = HabSel, family = binomial)

# Summary of the model
summary(model_individual)

# Generate and plot individual-level effects
effects_individual <- ggpredict(model_individual, terms = c("LC_cat", "SEASON", "ID","year"))
plot(effects_individual)

plot(effects_individual)+
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 60, hjust = 1, size = 4))




#plot one gull only at a time
effects_Gary <- effects_individual %>% filter(facet == "Gary")

plot(effects_Gary)+
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 60, hjust = 1, size = 6))

summary(effects_Gary) 
# this shows 4 years but he was only tagged for 2.... 


HabSel_Stuart<-subset(HabSel, HabSel$ID=="Stuart")
model_stuart <- bam(Pres ~ LC_cat * Season*year, data = HabSel_Stuart, family = binomial)
table(HabSel_Stuart$Pres, HabSel_Stuart$LC_cat,HabSel_Stuart$year)
# Summary of the model
summary(model_stuart)

# Generate and plot individual-level effects
effects_Stuart <- ggpredict(model_stuart, terms = c("LC_cat", "Season","year"))
plot(effects_Stuart)

plot(effects_individual)+
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 60, hjust = 1, size = 6))

#plot one gull only at a time
effects_Stuart <- effects_individual %>% filter(facet == "Stuart")

plot(effects_Stuart)+
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 60, hjust = 1, size = 6))

summary(effects_Stuart) 
#can ignore years where he is not tagged as this is what would be predicted
#based on previous years