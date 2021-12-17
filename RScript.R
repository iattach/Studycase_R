library("dplyr")
library(ggplot2)
library(data.table)
library(purrr)
library(scales)
library(maps)
library(stringr)
library(RColorBrewer)
require(viridis)
#change format to en_us
Sys.setlocale(locale = "en_US.UTF-8")

#read file 
getwd()
setwd("/home/hou/Bureau/Studycase_R")
readfile <- read.csv("Space_Corrected.csv",sep = ',')

# show types 
str(readfile$Datum)

#NA all the lost cost
readfile$Rocket[readfile$Rocket==""] <- NA

#shape datum into time format
readfile$Datum<-sapply(readfile$Datum,function(x) if(nchar(x)<=20) 
        as.POSIXct(x, format = "%a %b %d, %Y", origin = "1970-01-01", tz = "UTC") 
        else as.POSIXct(x, format = "%a %b %d, %Y %H:%M UTC", origin = "1970-01-01", tz = "UTC"))
readfile$Datum=as.POSIXct(readfile$Datum, format="%Y",origin = "1970-01-01", tz = "UTC")

#extract countries
world_map <- map_data("world")
all_countries <- str_c(unique(world_map$region), collapse = "|")
#all_countries <- str_c(unique(world.cities$country.etc), collapse = "|")
readfile$location_country <- sapply(str_extract_all(readfile$Location, all_countries), toString)
readfile$country <- sapply(str_extract_all(readfile$Location, all_countries), toString)

#special case
readfile$country[readfile$country=="Kazakhstan"]="Russia"
readfile$country[readfile$country=="Algeria, France"]="France"
readfile$country[readfile$country=="French Guiana, France"]="France"
readfile$country[readfile$country=="Marshall Islands, USA"]="USA"
readfile$country[readfile$country=="Kenya"]="Italy"

#NA all the lost country
readfile$country[readfile$country==""] <- NA
readfile$location_country[readfile$location_country==""] <- NA

#convert string to int
readfile$Rocket <- as.double(readfile$Rocket)

str(readfile)
########################################################################

#Nombre de lancement en fonction du temps
readfile %>%
  group_by(year=format(Datum,"%Y")) %>%
  summarise(launch= n()) %>%
  ggplot(aes(x=as.Date(year,format="%Y"),y=launch,group=1))+geom_line()+
  labs(title="Launches by year",x="Years", y="Launches")
#  scale_x_date(breaks =(date_breaks = '100 years'),date_labels = "%Y")
#  ggtitle("Temporal Outliers of Node 25 ") + 

#Nombre de lancement en fonction du pays
launch_country <- readfile
launch_country <- launch_country[!(is.na(launch_country$country)),]

launch_country %>%
  group_by(country) %>%
  summarise(launch= n()) %>%
  arrange(desc(launch), .by_group=TRUE ) %>%
  ggplot(aes(x=launch,y= reorder(country,launch),group=1))+geom_bar(stat="identity")+
  labs(title="Total launches by country",x="Launches", y="Country")

#Nombre de lancement en fonction du pays by year
launch_country <- readfile
launch_country <- launch_country[!(is.na(launch_country$country)),]

launch_country %>%
  group_by(country) %>%
  summarise(launch= n()) %>%
  arrange(desc(launch), .by_group=TRUE ) %>%
  ggplot(aes(x=launch,y= reorder(country,launch),group=1))+geom_bar(stat="identity")+
  labs(title="Total launches by country",x="Launches", y="Country")

#Nombre de lancement en fonction du temps
readfile %>%
  group_by(year=format(Datum,"%Y")) %>%
  summarise(launch= n()) %>%
  ggplot(aes(x=as.Date(year,format="%Y"),y=launch,group=1))+geom_line()+
  labs(title="Launches by year",x="Years", y="Launches")

#Prix en fonction du temps et pays
countryCount = length(unique(readfile$country))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

readfile %>%
  group_by(year=format(Datum,"%Y"),country) %>%
  summarise(price=sum(Rocket,na.rm=TRUE)) %>%
  ggplot(aes(x=as.Date(year,format="%Y"),y=price,color=country))+geom_line()+
  labs(title="Total prices by year",x="Years", y="Total prices")+
  scale_fill_brewer(getPalette(countryCount))+
  theme(legend.position = "bottom")

#Rapport prix/nombre de lancement en fonction du temps
readfile %>%
  group_by(year=format(Datum,"%Y")) %>%
  summarise(price=sum(Rocket,na.rm=TRUE),launch= n(),average=price/launch) %>%
  ggplot(aes(x=as.Date(year,format="%Y"),y=average))+geom_col()+
  labs(title="Average price by year",x="Years", y="Average price")

#Launches by country
launchcs <- readfile %>%
  group_by(location_country) %>%
  summarise(launch= n()) %>%
  select(region=location_country,launch) 
  #remove the NA  
launchcs$region[launchcs$region=="Algeria, France"]="Algeria"
launchcs$region[launchcs$region=="French Guiana, France"]="French Guiana"
launchcs$region[launchcs$region=="Marshall Islands, USA"]="Marshall Islands"

launchcs <- head(launchcs, -1)
world_map <- map_data("world")

right_join(launchcs,world_map,by="region") %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = launch), colour = "white")+
  scale_fill_gradient(low = "red", high = "green",name = "Total Launches")+
  labs(title="Worldwide total launches by country")
