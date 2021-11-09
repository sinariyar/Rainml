library(leaflet)
library(jsonlite)
library(dplyr)
#導入資料(政府資料開放平台:自動雨量站-雨量觀測資料)(json檔)
RainmlData <- fromJSON("https://opendata.cwb.gov.tw/fileapi/v1/opendataapi/O-A0002-001?Authorization=rdec-key-123-45678-011121314&format=JSON")
str(RainmlData)
#List of 1
#$ cwbopendata:List of 10
#..$ @xmlns    : chr "urn:cwb:gov:tw:cwbcommon:0.1"
#..$ identifier: chr "e23b7c48-3ee1-11ec-9e2d-d89d67177004"
#..$ sender    : chr "weather@cwb.gov.tw"
#..$ sent      : chr "2021-11-06T17:14:22+08:00"
#..$ status    : chr "Actual"
#..$ msgType   : chr "Issue"
#..$ dataid    : chr "CWB_A0002"
#..$ scope     : chr "Public"
#..$ dataset   : NULL
#..$ location  :'data.frame':	1143 obs. of  7 variables:
#  .. ..$ lat           : chr [1:1143] "23.9637" "25.1351" "25.1667" "24.9994" ...
#.. ..$ lon           : chr [1:1143] "120.8369" "121.7323" "121.4407" "121.4338" ...
#.. ..$ locationName  : chr [1:1143] "九份二山" "基隆" "淡水" "板橋" ...

#資料整理
#抽取各地降雨資料
for(i in c(1:length(RainmlData[["cwbopendata"]][["location"]][["lon"]]))) #取座標(經度)個數為長度
{ 
  a <- RainmlData[["cwbopendata"]][["location"]][["weatherElement"]][[i]][["elementValue"]][["value"]][[8]]
  a <- as.numeric(a)
  if (a<=0){a <- 0}else{a <- a}#將沒有降雨的值(-988)歸零
  RainmlData[["cwbopendata"]][["location"]][["rain"]]<- c(a)
  i <- i+1}
#取出觀測站的經緯度與降雨量
lng<-as.numeric(RainmlData[["cwbopendata"]][["location"]][["lon"]])
lat<-as.numeric(RainmlData[["cwbopendata"]][["location"]][["lat"]])
rain<- as.numeric(RainmlData[["cwbopendata"]][["location"]][["rain"]])
#結合成檔案並挑選出有降雨的地區
Rainml <- data.frame(cbind(lng,lat,rain))#存成dataframe讓leaflet能夠讀取
Rainmlf <- filter(Rainml, rain >= 1 )
Rainmlf
rRainmlf <- nrow(Rainmlf)#沒有降雨rRainmlf為0

#經緯度的平均
mlng <- sum(lng)/length(lng)
mlat <- sum(lat)/length(lat)
loc <- data.frame(cbind(mlng,mlat))

#繪製地圖
if ( rRainmlf==0){leaflet(loc)%>%
    addTiles()%>%setView(lng = mlng,lat = mlat , zoom = 7) %>%
    addMarkers(lng = ~mlng,lat = ~mlat , popup = "全台無降雨" )}else{leaflet(Rainmlf)%>%
      addTiles()%>%
      addMarkers(lng = ~lng,lat = ~lat,popup = ~paste("降雨量:",as.character(rain)))}





