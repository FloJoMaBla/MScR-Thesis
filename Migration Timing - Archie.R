library(spatstat)
library(spatstat.geom)
library(terra)
library(units)
library(sf)
library(lubridate)
library(mapview)
library(plotly)
library(gapminder)

df<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Raw Data/raw_df.csv")
df$date<-as.Date(df$timestamp, format="%d/%m/%Y %H:%M")
df$YEAR<-year(df$date)

head(df)

#nrow(df) to find out the number of rows in the dataframe
# 123098

# filter our data
#  keep only fltdata 0
df<-df[which(df$flt.switch==0),]
#nrow(df)
# 104585 rows left 

# remove missing values in coordinates (don't need to do for others if using df)
summary(df$location.lat)
summary(df$location.long)

df<-df[-which(is.na(df$location.long)),]
 summary(df$location.lat)
 summary(df$location.long)

# filter by date 
#nesting period for gulls, based on general nesting rather than specific to each gull

Archie2017<-df[df$individual.local.identifier=="Archie" &
                 df$date>="2017-04-25" &  #but not tagged until early July, so this object runs to end of July
                 df$date<="2017-07-31" , ]

# turn it into a spatial object
Archie2017<-st_as_sf(Archie2017, coords = c("location.long", "location.lat"),
                     crs=4326)

mapview(Archie2017)
#find their central occurrence point
W<-as.owin(st_bbox(Archie2017))
Archie2017_ppp<-as.ppp(st_coordinates(Archie2017), W=W)
denst<-rast(density.ppp(Archie2017_ppp, sigma=0.01))
plot(denst)
Archie2017_centr<-xyFromCell(denst, which.max(values(denst)))
Archie2017_centr<-as.data.frame(Archie2017_centr)
Archie2017_centr<-st_as_sf(Archie2017_centr, coords=c("x", "y"), crs=4326)
mapview(Archie2017) + mapview(Archie2017_centr, col.regions="red")
#calculate distance with centre
Archie2017$D_centr_m<-round(drop_units(st_distance(Archie2017, Archie2017_centr)),0)
Archie2017
mapview(Archie2017) + mapview(Archie2017_centr, col.regions="red") 

### Archie when migrating
Archie_migrating<-df[df$individual.local.identifier=="Archie" &
                       df$date>="2017-08-01" & #after nesting, before leaving Scotland
                       df$date<="2018-08-03" , ] #one year on (ish) to capture full migration

Archie_migrating<-st_as_sf(Archie_migrating, coords = c("location.long", "location.lat"),
                           crs=4326)
mapview(Archie_migrating) + mapview(Archie2017_centr, col.regions="red") 

# calculate distance from central occurrence point
#pick centre for every gull
Archie_migrating$D_centr_m<-round(drop_units(st_distance(Archie_migrating, Archie2017_centr)),0)

Archie_migrating

#this plot shows distance from central occurrence point in metres
plot(Archie_migrating$date, Archie_migrating$D_centr_m)

library(ggplot2)
Archie_plot<-ggplot(Archie_migrating, aes(x=date, y=D_centr_m))+
  geom_point()

Archie_plot

ggplotly(Archie_plot)
#####now 2018####
Archie2018<-df[df$individual.local.identifier=="Archie" &
                 df$date>="2018-04-25" &
                 df$date<="2018-06-29" , ] #earlier as not a tagging year

# turn it into a spatial object
Archie2018<-st_as_sf(Archie2018, coords = c("location.long", "location.lat"),
                     crs=4326)

mapview(Archie2018)
W<-as.owin(st_bbox(Archie2018))
Archie2018_ppp<-as.ppp(st_coordinates(Archie2018), W=W)
denst<-rast(density.ppp(Archie2018_ppp, sigma=0.01))
plot(denst)
Archie2018_centr<-xyFromCell(denst, which.max(values(denst)))
Archie2018_centr<-as.data.frame(Archie2018_centr)
Archie2018_centr<-st_as_sf(Archie2018_centr, coords=c("x", "y"), crs=4326)
mapview(Archie2018) + mapview(Archie2018_centr, col.regions="red")
#calculate distance with centre
Archie2018$D_centr_m<-round(drop_units(st_distance(Archie2018, Archie2018_centr)),0)
Archie2018
mapview(Archie2018) + mapview(Archie2018_centr, col.regions="red") 

### Archie when migrating
Archie_migrating<-df[df$individual.local.identifier=="Archie" &
                       df$date>="2018-06-01" &
                       df$date<="2018-08-03" , ] #tag ran out Jul 2018

Archie_migrating<-st_as_sf(Archie_migrating, coords = c("location.long", "location.lat"),
                           crs=4326)
mapview(Archie_migrating) + mapview(Archie2018_centr, col.regions="red") #+ mapview(Archie2018_nest, col.regions="green")

# calculate distance from centre (top line) or nest (lower line)
#pick centre for every gull
Archie_migrating$D_centr_m<-round(drop_units(st_distance(Archie_migrating, Archie2018_centr)),0)

Archie_migrating

#plot(Archie_migrating$date, Archie_migrating$D_nest_m)
#this plot shows distance from nest. We want to do it distance from KDE, so D_centr_m
plot(Archie_migrating$date, Archie_migrating$D_centr_m)

library(ggplot2)
Archie_plot<-ggplot(Archie_migrating, aes(x=date, y=D_centr_m))+
  geom_point()

Archie_plot

ggplotly(Archie_plot)

