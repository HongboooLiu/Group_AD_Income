library(readr)
library(ggplot2)
library(ggmap)
library(tmap)
library(sp)
library(DT)
library(devtools)
library(geojsonio)
library(leaflet)
library(usmap)
library(stringr)
library(raster)
library(tidyverse)
rinc= read_csv("C:/Users/22151/Desktop/Final project/10.csv")
rinc<- rinc[rinc$own_code==0,]
area<- read_csv("C:/Users/22151/Desktop/Final project/area-titles-csv.csv")
coor<- read_csv("C:/Users/22151/Desktop/Final project/uscounties.csv")
rinc$area_fips<- sub("^0+", "", rinc$area_fips)
allinc<- merge (area,rinc,all.y = TRUE)
allinc$county<- allinc$area_title
allinc[c("county","states")]<- str_split_fixed(allinc$county,',',2)
allinc$county<- sub("County",'',allinc$county)
coor<- coor[,c(1,6,7,8)]
colnames(coor)[2]<- "states"
allinc$states<- sub(" ",'',allinc$states)
allinc$county<- trimws(allinc$county,which = c("both"))
K<- merge(allinc,coor)
K$fips=K$area_fips
p1<- plot_usmap(regions= "counties",data=K,values="avg_wkly_wage")+
  scale_fill_continuous(
    low = "yellow", high = "red", name = "average weekly wage", label = scales::comma
  ) + theme(legend.position = "right")
p2<- plot_usmap(regions= "counties",data=K,values="oty_avg_wkly_wage_chg")+
  scale_fill_continuous(
    low = "yellow", high = "red", name = "over the year change in average weekly wage", label = scales::comma
  ) + theme(legend.position = "right")
p3<- plot_usmap(regions= "counties",data=K,values="qtrly_estabs")+
  scale_fill_continuous(
    low = "yellow", high = "red", name = "Count of establishments for a given quarter", label = scales::comma
  ) + theme(legend.position = "right")

p4<- plot_usmap(regions= "counties",data=K,values="oty_qtrly_estabs_chg")+
  scale_fill_continuous(
    low = "yellow", high = "red", name = "Over-the-year change in the count of establishments", label = scales::comma
  ) + theme(legend.position = "right")

p1
p2
p3
p4

USA <- getData("GADM", country = "usa", level = 2)
USA$NAME_2
mydata <- data.frame(county= K$county,
                     state= K$states,
                     avg_wkly_wage = K$avg_wkly_wage,
                     oty_avg_wkly_wage_chg=K$oty_avg_wkly_wage_chg,
                     qtrly_estabs= K$qtrly_estabs,
                     oty_qtrly_estabs_chg= K$oty_qtrly_estabs_chg,
                     stringsAsFactors = FALSE)

temp <- merge(USA, mydata,
              by.x = c("NAME_1", "NAME_2"), by.y = c("state", "county"),
              all.x = TRUE,duplicateGeoms = TRUE)


mypal <- colorNumeric(palette = "viridis", domain = temp$avg_wkly_wage)
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
  addPolygons(data = USA, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
              fillColor = ~mypal(temp$avg_wkly_wage),
              popup = paste("Region: ", temp$NAME_2, "<br>",
                            "average weekly wage: ", temp$avg_wkly_wage, "<br>",
                            "over the year change in average weekly wage:",temp$oty_avg_wkly_wage_chg, "<br>",
                            "Count of establishments for a given quarter",temp$qtrly_estabs, "<br>"
                            )) %>%
  addLegend(position = "bottomleft", pal = mypal, values = temp$avg_wkly_wage,
            title = "average weekly wage",
            opacity = 1)
