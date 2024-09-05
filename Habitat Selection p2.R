library(geosphere)
library(lme4) #not used any more
library(RInSp)
library(adehabitatHR)
library(lubridate)
library(sf)
library(ggplot2)
library(mapview)
library(raster)
#Each gull's habitat selection, first part in Part 1 Folder
Gary_Breeding17_PA$Nest<-"Urban" #do this for all depending on nest/use points
Gary_Wintering17_PA$Nest<-"Urban"
Gary_WinteringS217_PA$Nest<-"Urban"
Gary_Breeding18_PA$Nest<-"Urban"
Gary_Wintering18_PA$Nest<-"Urban"
Gary_WinteringS218_PA$Nest<-"Urban"
Clyde_Breeding17_PA$Nest<-"Urban"
Clyde_Wintering17_PA$Nest<-"Urban"
Flyback_Breeding17_PA$Nest<-"Non-Urban"
Flyback_Wintering17_PA$Nest<-"Non-Urban"
Flyback_WinteringS217_PA$Nest<-"Non-Urban"
Stuart_Breeding17_PA$Nest<-"Non-Urban"
Stuart_Wintering17_PA$Nest<-"Non-Urban"
Stuart_Breeding18_PA$Nest<-"Non-Urban"
Stuart_Wintering18_PA$Nest<-"Non-Urban"
Archie_Breeding17_PA$Nest<-"Non-Urban"
Archie_Wintering17_PA$Nest<-"Non-Urban"
Jose_Breeding18_PA$Nest<-"Non-Urban"
Jose_Wintering18_PA$Nest<-"Non-Urban"
Jose_WinteringS218_PA$Nest<-"Non-Urban"
Jose_Breeding19_PA$Nest<-"Non-Urban"
Jose_Wintering19_PA$Nest<-"Non-Urban"
Jose_WinteringS219_PA$Nest<-"Non-Urban"
Happy_Breeding18_PA$Nest<-"Non-Urban"
Happy_Wintering18_PA$Nest<-"Non-Urban"
Happy_WinteringS218_PA$Nest<-"Non-Urban"
Eric_Breeding18_PA$Nest<-"Non-Urban"
Eric_Wintering18_PA$Nest<-"Non-Urban"
Eric_Breeding19_PA$Nest<-"Non-Urban"
Eric_Wintering19_PA$Nest<-"Non-Urban"
Eric_Breeding20_PA$Nest<-"Non-Urban"
Eric_Wintering20_PA$Nest<-"Non-Urban"
Stephen_Breeding17_PA$Nest<-"Non-Urban"
Stephen_Wintering17_PA$Nest<-"Non-Urban"
Roland_Breeding17_PA$Nest<-"Non-Urban"
Roland_Wintering17_PA$Nest<-"Non-Urban"
Roger_Breeding17_PA$Nest<-"Non-Breeding" #non breeding also includes unknowns, but I am unlikely to use this anyway
Roger_Wintering17_PA$Nest<-"Non-Breeding"
Ollie_Breeding18_PA$Nest<-"Non-Breeding"
Ollie_Wintering18_PA$Nest<-"Non-Breeding"
Ollie_Breeding19_PA$Nest<-"Non-Breeding"
Ollie_Wintering19_PA$Nest<-"Non-Breeding"
Luke_Breeding18_PA$Nest<-"Non-Breeding"
Luke_Wintering18_PA$Nest<-"Non-Breeding"
Kingpin_Breeding18_PA$Nest<-"Non-Breeding"
Kingpin_Wintering18_PA$Nest<-"Non-Breeding"
Gully_Breeding17_PA$Nest<-"Non-Breeding"
Gully_Wintering17_PA$Nest<-"Non-Breeding"
Rufus_Breeding19_PA$Nest<-"Non-Urban"
Rufus_Wintering19_PA$Nest<-"Non-Urban"

HSdf<-rbind(Gary_Breeding17_PA,
            Gary_Wintering17_PA,
            Gary_WinteringS217_PA,
            Gary_Breeding18_PA,
            Gary_Wintering18_PA,
            Gary_WinteringS218_PA,
            Clyde_Breeding17_PA,
            Clyde_Wintering17_PA,
            Flyback_Breeding17_PA,
            Flyback_Wintering17_PA,
            Flyback_WinteringS217_PA,
            Stuart_Breeding17_PA,
            Stuart_Wintering17_PA,
            Stuart_Breeding18_PA,
            Stuart_Wintering18_PA,
            Archie_Breeding17_PA,
            Archie_Wintering17_PA,
            Jose_Breeding18_PA,
            Jose_Wintering18_PA,
            Jose_WinteringS218_PA,
            Jose_Breeding19_PA,
            Jose_Wintering19_PA,
            Jose_WinteringS219_PA,
            Happy_Breeding18_PA,
            Happy_Wintering18_PA,
            Happy_WinteringS218_PA,
            Eric_Breeding18_PA,
            Eric_Wintering18_PA,
            Eric_Breeding19_PA,
            Eric_Wintering19_PA,
            Eric_Breeding20_PA,
            Eric_Wintering20_PA,
            Stephen_Breeding17_PA,
            Stephen_Wintering17_PA,
            Roger_Breeding17_PA,
            Roger_Wintering17_PA,
            Ollie_Breeding18_PA,
            Ollie_Wintering18_PA,
            Ollie_Breeding19_PA,
            Ollie_Wintering19_PA,
            Luke_Breeding18_PA,
            Luke_Wintering18_PA,
            Kingpin_Breeding18_PA,
            Kingpin_Wintering18_PA,
            Gully_Breeding17_PA,
            Gully_Wintering17_PA,
            Roland_Breeding17_PA,
            Roland_Wintering17_PA,
            Rufus_Breeding19_PA,
            Rufus_Wintering19_PA)

library(terra)

LC_1<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/28Q_20190101-20200101.tif")
LC_2<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/28R_20190101-20200101.tif")
LC_3<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/29Q_20190101-20200101.tif")
LC_4<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/29R_20190101-20200101.tif")
LC_5<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/29S_20190101-20200101.tif")
LC_6<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/30V_20190101-20200101.tif")
LC_7<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/29T_20190101-20200101.tif")
LC_8<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/29U_20190101-20200101.tif")
LC_9<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/29V_20190101-20200101.tif")
LC_10<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/30S_20190101-20200101.tif")
LC_11<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/30T_20190101-20200101.tif")
LC_12<-rast("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/ARC GIS Maps/30U_20190101-20200101.tif")


HSdf$LC_code<-NA
HSdf$LC_code<-extract(LC_1, HSdf)[,2] #if this doesn't work then re-read in the LC files above
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_2, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_3, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_4, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_5, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_7, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_8, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_9, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_10, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_11, HSdf)[which(is.na(HSdf$LC_code)),2]
HSdf$LC_code[is.na(HSdf$LC_code)]<-extract(LC_12, HSdf)[which(is.na(HSdf$LC_code)),2]
table(HSdf$LC_code)


HSdf$LC_cat<-NA
HSdf$LC_cat[HSdf$LC_code==1]<-"Water"
HSdf$LC_cat[HSdf$LC_code==2]<-"Trees"
HSdf$LC_cat[HSdf$LC_code==4]<-"Flooded_vegetation"
HSdf$LC_cat[HSdf$LC_code==5]<-"Crops"
HSdf$LC_cat[HSdf$LC_code==7]<-"Built_area"
HSdf$LC_cat[HSdf$LC_code==8]<-"Bare_ground"
HSdf$LC_cat[HSdf$LC_code==10]<-"Clouds"
HSdf$LC_cat[HSdf$LC_code==11]<-"Rangeland"
HSdf$LC_cat[is.na(HSdf$LC_code)]<-"Water" 

table(HSdf$LC_cat)

# remove clouds
HSdf<-subset(HSdf, HSdf$LC_cat!="Clouds")

# import world borders (water outside of this = sea water)
library(sf)
library(terra)
countries<-st_read("C:/Users/floey/OneDrive - University of Glasgow/MScR/Data_Analysis/Habitat Selection/Part 2 - Habitat Selection Function/natural_earth_vector/10m_cultural/ne_10m_admin_0_countries.shp")
countries<-st_make_valid(countries)

water_points <- HSdf[HSdf$LC_cat == "Water",]
inside_points <- st_within(water_points, countries, sparse = FALSE)
inside_points <- apply(inside_points, 1, any) # Logical vector indicating if points are inside any country

# Points that are "Water" but not inside any country
outside_water_points <- water_points[!inside_points, ]


nrow(outside_water_points)
# Update LC_cat for points outside the borders to "Sea water"
library(dplyr)
HSdf$LC_cat[HSdf$x %in% outside_water_points$x] <- "Sea water"
table(HSdf$LC_cat)
#st_write(HSdf, "updated_points.shp")
