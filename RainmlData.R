library(jsonlite)
library(ggmap)
library(ggplot2)
RainmlData <- fromJSON("https://opendata.cwb.gov.tw/fileapi/v1/opendataapi/O-A0002-001?Authorization=rdec-key-123-45678-011121314&format=JSON")


for(i in c(1:length(RainmlData[["cwbopendata"]][["location"]][["lon"]]))){ 
  a <- RainmlData[["cwbopendata"]][["location"]][["weatherElement"]][[i]][["elementValue"]][["value"]][[8]]
  a <- as.numeric(a)
  if (a<=0){a <- 0}else{a <- a}
  RainmlData[["cwbopendata"]][["location"]][["rain"]]<- c(a)
  i <- i+1}


RainmlDataFrame <- RainmlData[["cwbopendata"]][["location"]]        
         

RainmlDataFrame$longitude<-as.numeric(RainmlData[["cwbopendata"]][["location"]][["lon"]])
RainmlDataFrame$latitude<-as.numeric(RainmlData[["cwbopendata"]][["location"]][["lat"]])
RainmlDataFrame$rain<- as.numeric(RainmlData[["cwbopendata"]][["location"]][["rain"]])
TaiwanMap = get_map(location = c(119.00,22.03,122.10,25.50), zoom = 11, maptype = 'roadmap')
TaiwanMapO = ggmap(TaiwanMap)+ geom_point( data=subset(RainmlDataFrame,rain>=0), 
                                           aes(x=longitude, y=latitude,color=rain,size=3.5))+ 
  scale_color_continuous(low = "blue",high = "purple")+ guides(size="none")
TaiwanMapO
