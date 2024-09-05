library(geosphere)
library(lme4)
library(RInSp)
library(adehabitatHR)
library(lubridate)
library(sf)
library(ggplot2)
library(mapview)

df<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Raw Data/raw_df.csv")
df<-df[which(df$flt.switch==0),]
df$date<-as.Date(df$timestamp, format="%d/%m/%Y %H:%M")
df$YEAR<-year(df$date)

####Archie 2017####
#Breeding season 7th July - 29th Nov - day of tagging to day before migration commencement
#Wintering Site - 11th Dec - 14th Apr - day after arrival to day before departure

#time spent at the breeding site
Archie_Breeding17<-df[df$individual.local.identifier=="Archie" &
                        df$date>="2017-07-07" &
                        df$date<="2017-11-29" , ]

Archie_Breeding17<-st_as_sf(Archie_Breeding17,
                            coords=c("location.long", "location.lat"),
                            crs=4326)
plot(Archie_Breeding17)
# kernel density
Archie_Breeding17_k95<-kernelUD(as(Archie_Breeding17, "Spatial"),
                                h="href",
                                grid=1000)

Archie_Breeding17_k95<-st_as_sf(getverticeshr.estUD(Archie_Breeding17_k95, percent=95))
mapview(Archie_Breeding17_k95)

# generate random points
ratio<-1 #trying out 1:1 ratio to start with
set.seed(1)
Archie_Breeding17_rand<-st_sample(x=Archie_Breeding17_k95,
                                  size=nrow(Archie_Breeding17)*ratio,
                                  type="random")


Archie_Breeding17_rand<-st_as_sf(Archie_Breeding17_rand)


# merge with presence point
Archie_Breeding17_rand$ID<-"Archie"
Archie_Breeding17_rand$Season<-"Breeding"
Archie_Breeding17_rand$year<-2017
Archie_Breeding17_rand$Pres<-0
Archie_Breeding17_rand$timestamp<-Archie_Breeding17$timestamp

Archie_Breeding17_pres<-st_as_sf(Archie_Breeding17$geometry)
Archie_Breeding17_pres$ID<-"Archie"
Archie_Breeding17_pres$Season<-"Breeding"
Archie_Breeding17_pres$year<-2017
Archie_Breeding17_pres$Pres<-1 
Archie_Breeding17_pres$timestamp<-Archie_Breeding17$timestamp


Archie_Breeding17_PA<-rbind(Archie_Breeding17_pres,
                            Archie_Breeding17_rand)

Archie_Breeding17_PA
mapview(Archie_Breeding17_PA, zcol="Pres")


# do this for every combination of bird, year and season
#Wintering 2017

Archie_Wintering17<-df[df$individual.local.identifier=="Archie" &
                         df$date>="2017-12-11" &
                         df$date<="2018-04-14" , ] 

Archie_Wintering17<-st_as_sf(Archie_Wintering17,
                             coords=c("location.long", "location.lat"),
                             crs=4326)
plot(Archie_Wintering17)
# kernel density
Archie_Wintering17_k95<-kernelUD(as(Archie_Wintering17, "Spatial"),
                                 h="href",
                                 grid=1000)

Archie_Wintering17_k95<-st_as_sf(getverticeshr.estUD(Archie_Wintering17_k95, percent=95))
mapview(Archie_Wintering17_k95)

# generate random points
ratio<-1
Archie_Wintering17_k95<-st_make_valid(Archie_Wintering17_k95)
set.seed(1)
Archie_Wintering17_rand<-st_sample(x=Archie_Wintering17_k95,
                                   size=nrow(Archie_Wintering17)*ratio,
                                   type="random")


Archie_Wintering17_rand<-st_as_sf(Archie_Wintering17_rand)


# merge with presence point
Archie_Wintering17_rand$ID<-"Archie"
Archie_Wintering17_rand$Season<-"Wintering"
Archie_Wintering17_rand$year<-2017
Archie_Wintering17_rand$Pres<-0
Archie_Wintering17_rand$timestamp<-Archie_Wintering17$timestamp

Archie_Wintering17_pres<-st_as_sf(Archie_Wintering17$geometry)
Archie_Wintering17_pres$ID<-"Archie"
Archie_Wintering17_pres$Season<-"Wintering"
Archie_Wintering17_pres$year<-2017
Archie_Wintering17_pres$Pres<-1 
Archie_Wintering17_pres$timestamp<-Archie_Wintering17$timestamp


Archie_Wintering17_PA<-rbind(Archie_Wintering17_pres,
                             Archie_Wintering17_rand)

Archie_Wintering17_PA
mapview(Archie_Wintering17_PA, zcol="Pres")

