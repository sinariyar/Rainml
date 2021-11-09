library(leaflet)
library(jsonlite)
library(dplyr)
#�ɤJ���(�F����ƶ}�񥭥x:�۰ʫB�q��-�B�q�[�����)(json��)
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
#.. ..$ locationName  : chr [1:1143] "�E���G�s" "��" "�H��" "�O��" ...

#��ƾ�z
#����U�a���B���
for(i in c(1:length(RainmlData[["cwbopendata"]][["location"]][["lon"]]))) #���y��(�g��)�ӼƬ�����
{ 
  a <- RainmlData[["cwbopendata"]][["location"]][["weatherElement"]][[i]][["elementValue"]][["value"]][[8]]
  a <- as.numeric(a)
  if (a<=0){a <- 0}else{a <- a}#�N�S�����B����(-988)�k�s
  RainmlData[["cwbopendata"]][["location"]][["rain"]]<- c(a)
  i <- i+1}
#���X�[�������g�n�׻P���B�q
lng<-as.numeric(RainmlData[["cwbopendata"]][["location"]][["lon"]])
lat<-as.numeric(RainmlData[["cwbopendata"]][["location"]][["lat"]])
rain<- as.numeric(RainmlData[["cwbopendata"]][["location"]][["rain"]])
#���X���ɮרìD��X�����B���a��
Rainml <- data.frame(cbind(lng,lat,rain))#�s��dataframe��leaflet���Ū��
Rainmlf <- filter(Rainml, rain >= 1 )
Rainmlf
rRainmlf <- nrow(Rainmlf)#�S�����BrRainmlf��0

#�g�n�ת�����
mlng <- sum(lng)/length(lng)
mlat <- sum(lat)/length(lat)
loc <- data.frame(cbind(mlng,mlat))

#ø�s�a��
if ( rRainmlf==0){leaflet(loc)%>%
    addTiles()%>%setView(lng = mlng,lat = mlat , zoom = 7) %>%
    addMarkers(lng = ~mlng,lat = ~mlat , popup = "���x�L���B" )}else{leaflet(Rainmlf)%>%
      addTiles()%>%
      addMarkers(lng = ~lng,lat = ~lat,popup = ~paste("���B�q:",as.character(rain)))}




