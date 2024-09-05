library(nestR)
library(dplyr)
library(lubridate)
df<-read.csv("C:/Users/floey/OneDrive - University of Glasgow/MScR/Raw Data/raw_df.csv")
df$date<-as.Date(df$timestamp, format="%d/%m/%Y %H:%M")
df$YEAR<-year(df$date)
unique(df$YEAR)
Archie.2017<-df[df$individual.local.identifier=="Archie" & df$YEAR==2017 , ]
Archie.2017$burst<-1 #burst won't work if df$date and df$YEAR have not been done
Archie.2017$date<-as.Date(Archie.2017$timestamp,format="%d/%m/%Y%H:%M")
Archie.2017$date
Archie.2017$long<-Archie.2017$location.long
Archie.2017$lat<-Archie.2017$location.lat
Archie.2017<-Archie.2017[!is.na(Archie.2017$long),]
Archie.2017<-Archie.2017[!is.na(Archie.2017$lat),]
Archie2017_nest<-find_nests(Archie.2017,
                            sea_start=as.character("04-25"), 
                            sea_end=as.character("09-01"), 
                            buffer=200,  
                            min_pts=10, 
                            min_d_fix=8,
                            min_consec=5,
                            nest_cycle = 15,   #minimum number of days' nesting cycle for late tagged gulls (after 29th June)
                            min_days_att = 50, #similar to Picardi's kestrels example
                            min_top_att=10)    #10 in later-tagged birds
Archie2017_nest$nests
#no clear attempt found, likely due to late tagging
library(sf) #package used to represent spatial vector data including points, polygons and lines and their information
Archie2017.sp<-st_as_sf(Archie.2017, coords=c("long", "lat"), crs=4326)
Archie2017_nest.sp<-st_as_sf(Archie2017_nest$nests, coords=c("long", "lat"), crs=4326)
library(mapview)
mapview(Archie2017.sp)+mapview(Archie2017_nest.sp,color="green")
#now for 2018
Archie.2018<-df[df$individual.local.identifier=="Archie" & df$YEAR==2018 , ]
Archie.2018$burst<-2 #burst won't work if df$date and df$YEAR have not been done
Archie.2018$date<-as.Date(Archie.2018$timestamp,format="%d/%m/%Y%H:%M")
Archie.2018$date
Archie.2018$long<-Archie.2018$location.long
Archie.2018$lat<-Archie.2018$location.lat
Archie.2018<-Archie.2018[!is.na(Archie.2018$long),]
Archie.2018<-Archie.2018[!is.na(Archie.2018$lat),]
Archie2018_nest<-find_nests(Archie.2018,
                            sea_start=as.character("04-19"), #put forward to 19/04 as nesting had begun by 25th
                            sea_end=as.character("09-01"), 
                            buffer=200,  
                            min_pts=10, 
                            min_d_fix=8,
                            min_consec=5,
                            nest_cycle = 66,   #assumed length of nesting, tagged for a full season in 2018
                            min_days_att = 60, 
                            min_top_att=30)
Archie2018_nest$nests
#failed nest on Little Cumbrae
library(sf) #package used to represent spatial vector data including points, polygons and lines and their information
Archie2018.sp<-st_as_sf(Archie.2018, coords=c("long", "lat"), crs=4326)
Archie2018_nest.sp<-st_as_sf(Archie2018_nest$nests, coords=c("long", "lat"), crs=4326)
library(mapview)
mapview(Archie2018.sp)+mapview(Archie2018_nest.sp,color="green")
#now to take it further and plot results
Archie18_attempt<-format_attempts(nest_info=Archie2018_nest,nest_cycle=66) 
Archie18_outcomes<-estimate_outcomes(fixes=Archie18_attempt$fixes, visits=Archie18_attempt$visits,model="p_time")
Archie18_outcomes<-estimate_outcomes(fixes=Archie18_attempt$fixes, visits=Archie18_attempt$visits,model="null")
str(Archie18_outcomes)
library(ggplot2)
#plot daily detection probability (expected to go down)
Archie18_DailyDet<-plot_detection(Archie18_outcomes)
Archie18_DailyDet
#low at only about 37%
library(plotly)
ggplotly(Archie18_DailyDet)
summarize_outcomes(Archie18_outcomes,ci=0.95)
#suggests failed 
#nest survival plot
Archie18_NeSurv<-plot_nest_surv(Archie18_outcomes,who=1)
ggplotly(Archie18_NeSurv)
#failed around day 39 (ie late May - last visit confirmed 27th May which ties in with a 19th Apr start)
#nestR confirmed what I suspected, from mapview exploration and what the find_nests output suggested