# combine hotspots with  <- , vegetation, geometry information

# Load libraries
library(sf)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(bomrang)
library(lubridate)

hotspots <- read_csv("data/VIC_hotspots_raw.csv")
memberships <- read_csv("data/VIC_hotspots_after_clustering.csv")

hotspots$fire_id <- memberships$fire_id
rm(memberships)

# select the ignition points

temp <- hotspots %>%
  group_by(fire_id) %>%
  summarise(hour_id = min(hour_id)) %>%
  ungroup()

hotspots <- left_join(temp, hotspots)
rm(temp)

hotspots <- hotspots %>%
  group_by(fire_id) %>%
  summarise(lon = mean(lon), lat = mean(lat), time = mean(`#obstime`)) %>%
  ungroup()

# transform time to datetime
hotspots$time <- as.Date(as.POSIXct(hotspots$time, origin = '1970-01-01', tz = "GMT"), tz = "GMT")

# rename fire_id to id
hotspots <- rename(hotspots, id = fire_id)


# Load Australia map and Victoria map
au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

# Join vegetation information

# read in forest 2018
forest <- raster('data/aus_for18_publish/aus_for18_publish/aus_for18/z001001.adf')

# project to EPSG:3577
fire_points <- SpatialPoints(select(hotspots, lon, lat), proj4string=CRS("+init=epsg:4326")) %>%
  spTransform(projection(forest))

# extract ID of rectangles
fire_forest_index <- extract(forest, fire_points)


# access attributes in forest 2018
fire_forest_info <- levels(forest) %>%
  as.data.frame()

fire_forest_info <- fire_forest_info %>%
  .[fire_forest_index,] %>%
  as_tibble()

# join back to hotspots
for (variable in names(fire_forest_info)){
  hotspots[[variable]] <- fire_forest_info[[variable]]
}

rm(forest)
rm(fire_forest_info)
rm(fire_points)
rm(fire_forest_index)

# Join <- data

hotspots <- hotspots %>%
  rowwise() %>%
  mutate(ns = list(filter(sweep_for_stations(latlon = c(lat, lon)), state == "VIC")[1:10,1]))

# unlist the 10 stations
hotspots <- hotspots %>%
  mutate(ns_1 = unlist(ns[1]),
         ns_2 = unlist(ns[2]),
         ns_3 = unlist(ns[3]),
         ns_4 = unlist(ns[4]),
         ns_5 = unlist(ns[5]),
         ns_6 = unlist(ns[6]),
         ns_7 = unlist(ns[7]),
         ns_8 = unlist(ns[8]),
         ns_9 = unlist(ns[9]),
         ns_10 = unlist(ns[10])) %>%
  select(-ns)

hotspots <- hotspots %>%
  mutate(ns_1 = as.numeric(ns_1),
         ns_2 = as.numeric(ns_2),
         ns_3 = as.numeric(ns_3),
         ns_4 =as.numeric(ns_4),
         ns_5 = as.numeric(ns_5),
         ns_6 = as.numeric(ns_6),
         ns_7 = as.numeric(ns_7),
         ns_8 = as.numeric(ns_8),
         ns_9 =as.numeric(ns_9),
         ns_10 = as.numeric(ns_10))

# Join rainfall

if (nrow(hotspots)%%10 == 0){
  cut_off <- seq(0, nrow(hotspots), as.integer(nrow(hotspots)/10))
} else {
  cut_off <- c(seq(0, nrow(hotspots), as.integer(nrow(hotspots)/10)), nrow(hotspots))
}


mean_weather <- function(date, values){
  values <- values[order(date)]
  results <- paste0(values[1])
  for (i in c(7, 14, 28, 60, 120, 180, 360, 720)){
    results <- paste0(results, ',', mean(values[1:i], na.rm = TRUE))
  }
  return(results)
}

store_result <- c()

# read in weather data
weather <- read_csv("data/weather.csv")
weather <- weather[,-1]

for (k in 2:12){

  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]

  # select date and stations number
  trim_hotspots <- select(hotspots[start_cut:end_cut, ],
                          time,
                          id,
                          ns_1:ns_10)

  # extend date
  retrieve_date <- c()
  id <- rep(trim_hotspots$id, 720)
  station_numbers <- list()

  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_hotspots[[paste0('ns_', i)]], 720)
  }

  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_hotspots$time - i)
  }

  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")

  trim_hotspots <- data.frame(date = retrieve_date, id = id)

  for (i in 1:10){
    trim_hotspots[[paste0('ns_', i)]] <- station_numbers[[i]]
  }

  rm(station_numbers)
  rm(retrieve_date)

  trim_hotspots <- trim_hotspots %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)

  temp_weather <- weather
  temp_weather$date <- make_date(year = temp_weather$year,
                                 month = temp_weather$month,
                                 day = temp_weather$day)

  temp_weather <- select(temp_weather,
                         station_number,
                         date,
                         rainfall)

  trim_hotspots <- left_join(trim_hotspots,
                             temp_weather,
                             by = c("station_id" = "station_number",
                                    "date" = "date"))

  rm(temp_weather)
  rm(id)

  trim_hotspots <- select(trim_hotspots, -station_id)

  trim_hotspots <- spread(trim_hotspots, key = ns, value = rainfall)

  trim_hotspots <- select(trim_hotspots,
                          date,
                          id,
                          ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)

  trim_hotspots$rainfall <- apply(select(trim_hotspots, -date, -id),
                                1,
                                FUN = function(x){
                                  temp <- which(!is.na(x))
                                  return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                })

  trim_hotspots <- select(trim_hotspots, id, date, rainfall)

  trim_hotspots <- trim_hotspots %>%
    group_by(id) %>%
    summarise(outputs = mean_weather(date, rainfall)) %>%
    ungroup()

  trim_hotspots <- trim_hotspots %>%
    separate(outputs,
             into = c("rf" ,
                      "arf7",
                      "arf14",
                      "arf28",
                      "arf60",
                      "arf90",
                      "arf180",
                      "arf360",
                      "arf720"),
             sep = ",",
             convert = TRUE)

  if (length(store_result) == 0){
    store_result <- trim_hotspots
    rm(trim_hotspots)
  } else {
    store_result <- bind_rows(store_result, trim_hotspots)
  }
}

hotspots <- left_join(hotspots, store_result, by = "id")
rm(store_result)

# Join solar exposure

store_result <- c()

for (k in 2:12){

  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]

  # select date and stations number
  trim_hotspots <- select(hotspots[start_cut:end_cut, ],
                          time,
                          id,
                          ns_1:ns_10)

  # extend date
  retrieve_date <- c()
  id <- rep(trim_hotspots$id, 720)
  station_numbers <- list()

  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_hotspots[[paste0('ns_', i)]], 720)
  }

  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_hotspots$time - i)
  }

  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")

  trim_hotspots <- data.frame(date = retrieve_date, id = id)

  for (i in 1:10){
    trim_hotspots[[paste0('ns_', i)]] <- station_numbers[[i]]
  }

  rm(station_numbers)
  rm(retrieve_date)

  trim_hotspots <- trim_hotspots %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)

  # read in weather data
  temp_weather <- weather
  temp_weather$date <- make_date(year = temp_weather$year,
                                 month = temp_weather$month,
                                 day = temp_weather$day)
  temp_weather <- select(temp_weather,
                         station_number,
                         date,
                         solar_exposure)

  trim_hotspots <- left_join(trim_hotspots,
                             temp_weather,
                             by = c("station_id" = "station_number",
                                    "date" = "date"))

  rm(temp_weather)
  rm(id)

  trim_hotspots <- select(trim_hotspots, -station_id)

  trim_hotspots <- spread(trim_hotspots, key = ns, value = solar_exposure)

  trim_hotspots <- select(trim_hotspots,
                          date,
                          id,
                          ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)

  trim_hotspots$solar_exposure <- apply(select(trim_hotspots, -date, -id),
                                      1,
                                      FUN = function(x){
                                        temp <- which(!is.na(x))
                                        return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                      })

  trim_hotspots <- select(trim_hotspots, id, date, solar_exposure)

  trim_hotspots <- trim_hotspots %>%
    group_by(id) %>%
    summarise(outputs = mean_weather(date, solar_exposure)) %>%
    ungroup()

  trim_hotspots <- trim_hotspots %>%
    separate(outputs,
             into = c("se" ,
                      "ase7",
                      "ase14",
                      "ase28",
                      "ase60",
                      "ase90",
                      "ase180",
                      "ase360",
                      "ase720"),
             sep = ",",
             convert = TRUE)

  if (length(store_result) == 0){
    store_result <- trim_hotspots
    rm(trim_hotspots)
  } else {
    store_result <- bind_rows(store_result, trim_hotspots)
  }


}

hotspots <- left_join(hotspots, store_result, by = "id")
rm(store_result)

# Join maximum temp

store_result <- c()

for (k in 2:12){

  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]

  # select date and stations number
  trim_hotspots <- select(hotspots[start_cut:end_cut, ],
                          time,
                          id,
                          ns_1:ns_10)

  # extend date
  retrieve_date <- c()
  id <- rep(trim_hotspots$id, 720)
  station_numbers <- list()

  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_hotspots[[paste0('ns_', i)]], 720)
  }

  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_hotspots$time - i)
  }

  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")

  trim_hotspots <- data.frame(date = retrieve_date, id = id)

  for (i in 1:10){
    trim_hotspots[[paste0('ns_', i)]] <- station_numbers[[i]]
  }

  rm(station_numbers)
  rm(retrieve_date)

  trim_hotspots <- trim_hotspots %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)

  # read in weather data
  temp_weather <- weather
  temp_weather$date <- make_date(year = temp_weather$year,
                                 month = temp_weather$month,
                                 day = temp_weather$day)
  temp_weather <- select(temp_weather,
                    station_number,
                    date,
                    max_temperature)

  trim_hotspots <- left_join(trim_hotspots,
                             temp_weather,
                             by = c("station_id" = "station_number",
                                    "date" = "date"))

  rm(temp_weather)
  rm(id)

  trim_hotspots <- select(trim_hotspots, -station_id)

  trim_hotspots <- spread(trim_hotspots,
                          key = ns,
                          value = max_temperature)

  trim_hotspots <- select(trim_hotspots,
                          date,
                          id,
                          ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)

  trim_hotspots$max_temperature <- apply(select(trim_hotspots, -date, -id),
                                       1,
                                       FUN = function(x){
                                         temp <- which(!is.na(x))
                                         return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                       })

  trim_hotspots <- select(trim_hotspots, id, date, max_temperature)

  trim_hotspots <- trim_hotspots %>%
    group_by(id) %>%
    summarise(outputs = mean_weather(date, max_temperature)) %>%
    ungroup()

  trim_hotspots <- trim_hotspots %>%
    separate(outputs,
             into = c("maxt" ,
                      "amaxt7",
                      "amaxt14",
                      "amaxt28",
                      "amaxt60",
                      "amaxt90",
                      "amaxt180",
                      "amaxt360",
                      "amaxt720"),
             sep = ",",
             convert = TRUE)

  if (length(store_result) == 0){
    store_result <- trim_hotspots
    rm(trim_hotspots)
  } else {
    store_result <- bind_rows(store_result, trim_hotspots)
  }


}

hotspots <- left_join(hotspots, store_result, by = "id")
rm(store_result)

# Join minimum temp

store_result <- c()

for (k in 2:12){

  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]

  # select date and stations number
  trim_hotspots <- select(hotspots[start_cut:end_cut, ],
                          time,
                          id,
                          ns_1:ns_10)

  # extend date
  retrieve_date <- c()
  id <- rep(trim_hotspots$id, 720)
  station_numbers <- list()

  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_hotspots[[paste0('ns_', i)]], 720)
  }

  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_hotspots$time - i)
  }

  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")

  trim_hotspots <- data.frame(date = retrieve_date, id = id)

  for (i in 1:10){
    trim_hotspots[[paste0('ns_', i)]] <- station_numbers[[i]]
  }

  rm(station_numbers)
  rm(retrieve_date)

  trim_hotspots <- trim_hotspots %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)

  # read in weather data
  temp_weather <- weather
  temp_weather$date <- make_date(year = temp_weather$year,
                                 month = temp_weather$month,
                                 day = temp_weather$day)
  temp_weather <- select(temp_weather,
                         station_number,
                         date,
                         min_temperature)

  trim_hotspots <- left_join(trim_hotspots,
                             temp_weather,
                             by = c("station_id" = "station_number",
                                    "date" = "date"))

  rm(temp_weather)
  rm(id)

  trim_hotspots <- select(trim_hotspots, -station_id)

  trim_hotspots <- spread(trim_hotspots, key = ns, value = min_temperature)

  trim_hotspots <- select(trim_hotspots,
                          date,
                          id,
                          ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)

  trim_hotspots$min_temperature <- apply(select(trim_hotspots, -date, -id),
                                       1,
                                       FUN = function(x){
                                         temp <- which(!is.na(x))
                                         return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                       })

  trim_hotspots <- select(trim_hotspots, id, date, min_temperature)

  trim_hotspots <- trim_hotspots %>%
    group_by(id) %>%
    summarise(outputs = mean_weather(date, min_temperature)) %>%
    ungroup()

  trim_hotspots <- trim_hotspots %>%
    separate(outputs,
             into = c("mint" ,
                      "amint7",
                      "amint14",
                      "amint28",
                      "amint60",
                      "amint90",
                      "amint180",
                      "amint360",
                      "amint720"),
             sep = ",",
             convert = TRUE)

  if (length(store_result) == 0){
    store_result <- trim_hotspots
    rm(trim_hotspots)
  } else {
    store_result <- bind_rows(store_result, trim_hotspots)
  }


}

hotspots <- left_join(hotspots, store_result, by = "id")
rm(store_result)

rm(weather)

# Join CFA station
cfa <- st_read("data/SDM697752/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer/geomark_point.shp")
cfa <- cfa %>% filter(FEATSUBTYP == "fire station")
cfa <- st_transform(cfa, crs = 4326)

# Find the nearest CFA station
hotspots_temp <- st_as_sf(hotspots, coords = c("lon", "lat"), crs = 4326)
nearest_index <- st_nearest_feature(hotspots_temp$geometry, cfa$geometry)
temp <- geodist::geodist(select(hotspots, lon, lat),
                         select(as_tibble(st_coordinates(cfa$geometry[nearest_index])),
                                lon = X, lat = Y),
                         paired = TRUE)
hotspots$dist_cfa <- as.numeric(temp)
rm(cfa)
rm(temp)
rm(hotspots_temp)

# Join recreation site
camp <- st_read("data/SDM698290/ll_gda94/sde_shape/whole/VIC/FORESTS/layer/recweb_site.shp")

camp <- st_transform(camp, crs = 4326)
hotspots_temp <- st_as_sf(hotspots, coords = c("lon", "lat"), crs = 4326)
nearest_index <- st_nearest_feature(hotspots_temp$geometry, camp$geometry)
temp <- geodist::geodist(select(hotspots, lon, lat),
                         select(as_tibble(st_coordinates(camp$geometry[nearest_index])),
                                lon = X, lat = Y),
                         paired = TRUE)
hotspots$dist_camp <- as.numeric(temp)
rm(camp)
rm(temp)
rm(hotspots_temp)

# Join wind data

wind <- read_csv("data/asos.txt")
wind$valid <- as.Date(wind$valid)
# Convert mph to m/s
wind$sped <- wind$sped/2.237
wind_st <- select(wind, station, lon, lat)
wind_st <- wind_st[!duplicated(wind_st),]

ws <- c()
aws_m0 <- c()
aws_m1 <- c()
aws_m3 <- c()
aws_m6 <- c()
aws_m12 <- c()
aws_m24 <- c()

for (i in 1:nrow(hotspots)){
  time <- hotspots$time[i]
  lon <- hotspots$lon[i]
  lat <- hotspots$lat[i]

  s_order <- data.frame(lon = lon, lat = lat) %>%
    geodist::geodist(select(wind_st, lon, lat)) %>%
    order()

  time <- time - (-60):800

  temp_wind_st <- wind_st
  temp_wind_st$order <- s_order

  temp <- crossing(time, station = temp_wind_st$station)

  temp <- left_join(temp, wind, by = c("time" = "valid", "station" = "station"))

  temp <- temp %>%
    group_by(time, station) %>%
    summarise(sped = mean(sped, na.rm = TRUE)) %>%
    ungroup()

  temp <- left_join(temp, select(temp_wind_st, station, order), by = "station")

  temp <- temp %>%
    filter(!is.na(sped)) %>%
    arrange(time, order) %>%
    group_by(time) %>%
    summarise(sped = first(sped)) %>%
    ungroup()

  time <- hotspots$time[i]

  ws <- c(ws,
          temp$sped[temp$time == time])
  aws_m0 <- c(aws_m0,
              mean(temp$sped[month(temp$time) ==  month(time)&year(temp$time) == year(time)], na.rm = TRUE))
  aws_m1 <- c(aws_m1,
              mean(temp$sped[month(temp$time) ==  month(time %m+% months(-1))&year(temp$time) == year(time %m+% months(-1))],
                   na.rm = TRUE))

  aws_m3 <- c(aws_m3,
              mean(temp$sped[month(temp$time) %in%  month(time %m+% months(c(-(1:3))))&year(temp$time) %in% year(time %m+% months(c(-(1:3))))],
                   na.rm = TRUE))
  aws_m6 <- c(aws_m6,
              mean(temp$sped[month(temp$time) %in%  month(time %m+% months(c(-(1:6))))&year(temp$time) %in% year(time %m+% months(c(-(1:6))))],
                   na.rm = TRUE))
  aws_m12 <- c(aws_m12,
              mean(temp$sped[month(temp$time) %in%  month(time %m+% months(c(-(1:12))))&year(temp$time) %in% year(time %m+% months(c(-(1:12))))],
                   na.rm = TRUE))
  aws_m24 <- c(aws_m24,
              mean(temp$sped[month(temp$time) %in%  month(time %m+% months(c(-(1:24))))&year(temp$time) %in% year(time %m+% months(c(-(1:24))))],
                   na.rm = TRUE))
}

hotspots$ws <- ws
hotspots$aws_m0 <- aws_m0
hotspots$aws_m1 <- aws_m1
hotspots$aws_m3 <- aws_m3
hotspots$aws_m6 <- aws_m6
hotspots$aws_m12 <- aws_m12
hotspots$aws_m24 <- aws_m24

rm(temp, temp_wind_st, wind, wind_st, ws, aws_m0, aws_m1, aws_m3, aws_m6, aws_m12, aws_m24)

# Join road distance

road <- st_read("data/australia-latest-free.shp/gis_osm_roads_free_1.shp")
roads_in_vic <- st_intersects(vic_map$geometry, road$geometry)
road <- road[roads_in_vic[[1]],]
rm(roads_in_vic)

hotspots_temp <- st_as_sf(hotspots, coords = c("lon", "lat"), crs = 4326)
nearest_index <- st_nearest_feature(hotspots_temp$geometry, road$geometry)
temp <- st_distance(hotspots_temp$geometry, road$geometry[nearest_index], by_element = TRUE)
hotspots$dist_road <- as.numeric(temp)
rm(road, hotspots_temp)

# rename fields

hotspots <- select(hotspots,
                 -ID,
                 -COUNT,
                 -STATE,
                 -FOR_SOURCE,
                 -ns_1,
                 -ns_2,
                 -ns_3,
                 -ns_4,
                 -ns_5,
                 -ns_6,
                 -ns_7,
                 -ns_8,
                 -ns_9,
                 -ns_10)


write_csv(hotspots, "data/predict_x.csv")









