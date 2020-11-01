library(tidyverse)
library(bomrang)

# get all stations and sorted by distance from MEL
station_ids = sweep_for_stations(latlon = c(-37.8136, 144.9631))

# filter VIC stations
VIC_station_ids = station_ids %>%
  filter(state == 'VIC')
rm(station_ids)


get_comprehensive_historical_weather <- function(station_id){
  
# requst weather data
rain_record = try(get_historical(station_id, type = "rain"))
min_record = try(get_historical(station_id, type = "min"))
max_record = try(get_historical(station_id, type = "max"))
solar_record = try(get_historical(station_id, type = "solar"))

# keep useful information
if (class(rain_record) != "try-error"){
  rain_record = select(rain_record, -quality, -product_code)
  }

if (class(min_record) != "try-error"){
  min_record = select(min_record, -quality, -product_code)
}

if (class(max_record) != "try-error"){
  max_record = select(max_record, -quality, -product_code)
}

if (class(solar_record) != "try-error"){
  solar_record = select(solar_record, -product_code)
}

temp = list()
temp[[1]] = rain_record
temp[[2]] = min_record
temp[[3]] = max_record
temp[[4]] = solar_record

result = NULL
  
for (i in 1:4){
  if (class(temp[[i]]) != "try-error") {
    if (is.null(result)){
      result = temp[[i]]
    } else {
      result = full_join(result, temp[[i]], by = c('year','month','day','station_number'))
    }
    
  } 
}

return(result)
}


weather_record = list()
for (i in seq(1,nrow(VIC_station_ids))){
  weather_ex = get_comprehensive_historical_weather(VIC_station_ids$site[i])
  weather_record[[i]] = weather_ex
}

weather_combine = bind_rows(weather_record)
write.csv(weather_combine, 'weather.csv')
