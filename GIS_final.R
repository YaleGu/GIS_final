##first library a few packages that we will use during the analysis
install.packages("spatstat")
install.packages("here")
install.packages("sp")
install.packages("rgeos")
install.packages("maptools")
install.packages("GISTools")
install.packages("tmap")
install.packages("sf")
install.packages("tmaptools")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tidyverse")
install.packages("rgdal")
install.packages("raster")
install.packages("shp2graph")
install.packages("classInt")
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(tmaptools)
library(ggplot2)
library(stringr)
library(tidyverse)
library(dplyr)
library(rgdal)
library(tmaptools)
library(raster)
library(shp2graph)
library(igraph)
library(classInt)


##plot accessiblevalue(300m)
AccessValue_300m <- st_read(here::here("data","LSOAwithAccessibleValue.shp"))
tm_shape(AccessValue) +
  tm_polygons("area.popul",
              alpha=1,
              style="cont",
              palette='Reds',
              midpoint=NA,
              title="Accessible score in London")


breaks_qt <- classIntervals(var=c(0, AccessValue_300m$area.popul),
                            n=8, 
                            style = "quantile")

AccessValue_300m <- mutate(AccessValue_300m,  AccessiblityScore = cut(AccessValue_300m$area.popul,breaks_qt$brks)) 
ggplot(AccessValue_300m) + 
  geom_sf(aes(fill=AccessiblityScore)) +
  scale_fill_brewer(palette = "OrRd") 


##plot accessiblevalue(500m)
AccessValue_500m <- st_read(here::here('data','500mLSOAwithAccessibleValue.shp'))
breaks_500m <- classIntervals(var=c(0, AccessValue_500m$Rj_sum),
                            n=8, 
                            style = "quantile")

AccessValue_500m <- mutate(AccessValue_500m,  AccessiblityScore = cut(AccessValue_500m$Rj_sum,breaks_500m$brks)) 
ggplot(AccessValue_500m) + 
  geom_sf(aes(fill=AccessiblityScore)) +
  scale_fill_brewer(palette = "OrRd") 


#set up histogram for shorest_distance
Shortest_distance <- st_read(here::here("data","shortest_distance.xlsx"))
# set up the basic histogram(Frequency)
gghist <- ggplot(Shortest_distance, 
                 aes(x=Distance)) + 
  geom_histogram(bins=30,
                 orientation=0,
                 color="white", 
                 fill="light blue")+
  labs(title="distance to the nearest green space", 
       x="distance(m)", 
       y="Frequency")
plot(gghist)


# add two vertical lines to the hisogram
gghist + geom_vline(aes(xintercept=300),
                    color="black", 
                    linetype="dashed", 
                    size=1)+
  geom_vline(aes(xintercept=500),
                                       color="black", 
                                       linetype="dashed", 
                                       size=1)+
  theme(plot.title = element_text(hjust = 0.5))

breaks <- classIntervals(var=c(0, Shortest_distance$Distance),
                            n=30, 
                            style = "equal")
Shortest_distance <- mutate(Shortest_distance, DistanceBreak = cut(Shortest_distance$Distance,breaks$brks)) 
Shortest_distance <- mutate(Shortest_distance, Group=max(as.numeric(as.character(Shortest_distance$DistanceBreak))))

# set up the 2nd histogram(population)
gghist2 <- ggplot(Shortest_distance, 
                 aes(x=DistanceBreak,y=Shortest_distance$All.Ages))+ 
  geom_bar(stat='identity',
           position='stack',
                 color="light blue", 
                 fill="light blue")+
  labs(title="distance to the nearest green space(Population)", 
       x="distance(m)", 
       y="Population")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x= element_text(angle=90))

#make a scatterplot for the service area of each green space
ServiceArea_300m <- st_read(here::here("data","300m_servicearea.xlsx"))
Scatterplot <- ggplot(ServiceArea_300m, aes(x=ServiceArea_300m$Area, y=ServiceArea_300m$Actual_Are))+
  geom_point(alpha=2, colour = "blue")+
  labs(x = "Area(m^2)", 
       y = "Service area per capita")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
plot(Scatterplot)

ServiceArea_500m <- st_read(here::here("data","500mServiceArea.xlsx"))
Scatterplot2 <- ggplot(ServiceArea_500m, aes(x=ServiceArea_500m$Area, y=ServiceArea_500m$ServiceArea))+
  geom_point(alpha=2, colour = "orange")+
  labs(x = "Area(m^2)", 
       y = "Service area per capita")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
plot(Scatterplot2)

Combine_scatterplot <- ggplot()+geom_point(aes(x=ServiceArea_300m$Area,y=ServiceArea_300m$Actual_Are),alpha=2, colour = "light blue")+
  geom_point(aes(x=ServiceArea_500m$Area, y=ServiceArea_500m$ServiceArea),alpha=2, colour = "orange")+
  labs(x = "Area(m^2)", 
       y = "Service area per capita")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
plot(Combine_scatterplot)
