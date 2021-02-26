library(jsonlite)
library(data.table)
library(dplyr)

while(TRUE){
  
  all_data=fread("zip_codes_states.csv")# downloaded from https://www.gaslampmedia.com/download-zip-code-latitude-longitude-city-state-county-csv/
  all_data$city=tolower(all_data$city)
  
  all_data=filter(all_data, state%in%c("MD","VA","NY","NJ","MA","CT","DE","ME","NH","PA","RI","WV"))
  
  cities_of_interest=c("anapolis","Virginia Beach","Washington","philadelphia","hartford","dover","Augusta","Albany","Harrisburg","Syracuse","Buffalo","newark","oneonta","erie","toms river","Pittsburgh")
  
  cities_of_interest=tolower(cities_of_interest)
  data_I_want=filter(all_data, city%in%cities_of_interest)
  data_I_want=distinct(data_I_want,city,.keep_all = TRUE)
  key <- "&mykey"  # This is a private key that is obtained by registering in the website
  
  collected_data= c()
  for(i in 1:nrow(data_I_want)){
    
    url <- paste0("http://api.openweathermap.org/data/2.5/forecast/daily?lat=",data_I_want$latitude[i],"&lon=",data_I_want$longitude[i],"&cnt=16")
    
    req <- fromJSON(paste0(url,key))
    city=req$city$name
    city_other=data_I_want$city[i]
    temp=req$list
    date = as.POSIXct(as.numeric(as.character(temp$dt)),origin="1970-01-01",tz="GMT")
    rain=temp$rain
    clouds=temp$clouds
    snow=temp$snow
    pressure=temp$pressure
    humidty=temp$humidity
    wind_speed=temp$speed
    z=temp$temp
    day_temperature=z$day   
    min_temperature =z$min 
    max_temperature =z$max 
    night_temperature=z$night
    eve_temperature =z$eve 
    morn_temperature=z$morn 
    zz=temp$weather
    
    condition = c()
    for(i in 1:length(zz)){
      x=zz[[i]]
      x=x$main
      show(x)
      condition= c(condition,x)
    }
    
    this_data=data.frame(date=date,city=city,city_other=city_other,
                         condition=tolower(condition), 
                         rain=rain, 
                         clouds  = clouds,
                         snow=snow,
                         pressure =pressure,
                         humidty=humidty,
                         wind_speed=wind_speed,
                         day_temperature=day_temperature,
                         min_temperature =min_temperature,
                         max_temperature=max_temperature,
                         night_temperature=night_temperature,
                         eve_temperature=eve_temperature,
                         morn_temperature=morn_temperature)
    collected_data=rbind(collected_data, this_data)
  }
  
  collected_data= merge(collected_data,data_I_want,by.x="city_other",by.y="city")
  save(collected_data,file="data/collected_data.Rdata")
  Sys.sleep(12*60*60)  # collect data every 12 hrs
  
}