# Combine fire origins (historical bushfire record) with weather, vegetation, geometry information

# Load libraries
library(sf)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(bomrang)
library(lubridate)

# Load Australia map and Victoria map
au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

# get fire origins
fire_origins_dir <- 'data/SDM714629/ll_gda94/sde_shape/whole/VIC/FIRE/layer/fire_history_origin.shp'

fire_o <- st_read(fire_origins_dir)

# filter fire from 2000
fire_o <- fire_o[fire_o$FIRE_START > "2000-01-01",]
fire_o <- st_transform(fire_o, crs = 4326)
temp <- st_coordinates(fire_o)
fire_o$lon <- temp[,1]
fire_o$lat <- temp[,2]
rm(temp)

# Join vegetation information

# read in forest 2018
forest <- raster('data/aus_for18_publish/aus_for18_publish/aus_for18/z001001.adf')

# extract coordinates information
fire_points <- st_coordinates(fire_o) %>%
  as.data.frame()

# project to EPSG:3577
fire_points <- SpatialPoints(fire_points, proj4string=CRS("+init=epsg:4326")) %>%
  spTransform(projection(forest))

# extract ID of rectangles
fire_forest_index <- extract(forest, fire_points)

# access attributes in forest 2018
fire_forest_info <- levels(forest) %>% 
  as.data.frame()

fire_forest_info <- fire_forest_info %>%
  .[fire_forest_index,] %>%
  as_tibble()

# join back to fire_o
for (variable in names(fire_forest_info)){
  fire_o[[variable]] <- fire_forest_info[[variable]]
}

rm(forest)
rm(fire_forest_info)
rm(fire_points)
rm(fire_forest_index)

# Join weather data


# get the 10 nearest stations
fire_o <- fire_o %>%
  rowwise() %>%
  mutate(ns = list(filter(sweep_for_stations(latlon = c(lat, lon)), state == "VIC")[1:10,1]))

# unlist the 10 stations
fire_o <- fire_o %>%
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

fire_o <- fire_o %>%
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

# assign unique id to each row
fire_o$id <- 1:nrow(fire_o)

# Join rainfall

cut_off <- c(seq(0, nrow(fire_o), as.integer(nrow(fire_o)/10)), nrow(fire_o))

ss <- function(date, values){
  values <- values[order(date)]
  results <- paste0(values[1])
  for (i in c(7, 14, 28, 60, 120, 180, 360, 720)){
    results <- paste0(results, ',', mean(values[1:i], na.rm = TRUE))
  }
  return(results)
}

store_result <- c()

for (k in 2:12){
  
  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]
  
  # select date and stations number
  trim_fire_o <- select(fire_o[start_cut:end_cut, ], FIRE_START, id, ns_1:ns_10)
  
  # extend date
  retrieve_date <- c()
  id <- rep(trim_fire_o$id, 720)
  station_numbers <- list()
  
  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_fire_o[[paste0('ns_', i)]], 720)
  } 
  
  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_fire_o$FIRE_START - i)
  }
  
  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")
  
  trim_fire_o <- data.frame(date = retrieve_date, id = id)
  
  for (i in 1:10){
    trim_fire_o[[paste0('ns_', i)]] <- station_numbers[[i]]
  }
  
  rm(station_numbers)
  rm(retrieve_date)
  
  trim_fire_o <- trim_fire_o %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)
  
  # read in weather data
  weather <- read_csv("data/weather.csv")
  weather <- weather[,-1]
  weather$date <- make_date(year = weather$year, month = weather$month, day = weather$day)
  weather <- select(weather, station_number, date, rainfall)

  trim_fire_o <- left_join(trim_fire_o, weather, by = c("station_id" = "station_number", "date" = "date"))
  
  rm(weather)
  rm(id)
  
  trim_fire_o <- select(trim_fire_o, -station_id)
  
  trim_fire_o <- spread(trim_fire_o, key = ns, value = rainfall)
  
  trim_fire_o <- select(trim_fire_o, date, id, ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)
  
  trim_fire_o$rainfall <- apply(select(trim_fire_o, -date, -id), 
        1, 
        FUN = function(x){
          temp <- which(!is.na(x))
          return(ifelse(length(temp) > 0, x[min(temp)], NA))
        })
  
  trim_fire_o <- select(trim_fire_o, id, date, rainfall)
  
  trim_fire_o <- trim_fire_o %>%
    group_by(id) %>%
    summarise(outputs = ss(date, rainfall)) %>%
    ungroup()
  
  trim_fire_o <- trim_fire_o %>%
    separate(outputs, 
             into = c("rf" ,"arf7", "arf14", "arf28", "arf60", "arf90", "arf180", "arf360", "arf720"),
             sep = ",",
             convert = TRUE)
  
  if (length(store_result) == 0){
    store_result <- trim_fire_o
    rm(trim_fire_o)
  } else {
    store_result <- bind_rows(store_result, trim_fire_o)
  }
    
  
}

fire_o <- left_join(fire_o, store_result, by = "id")
rm(store_result)

# Join solar exposure

store_result <- c()

for (k in 2:12){
  
  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]
  
  # select date and stations number
  trim_fire_o <- select(fire_o[start_cut:end_cut, ], FIRE_START, id, ns_1:ns_10)
  
  # extend date
  retrieve_date <- c()
  id <- rep(trim_fire_o$id, 720)
  station_numbers <- list()
  
  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_fire_o[[paste0('ns_', i)]], 720)
  } 
  
  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_fire_o$FIRE_START - i)
  }
  
  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")
  
  trim_fire_o <- data.frame(date = retrieve_date, id = id)
  
  for (i in 1:10){
    trim_fire_o[[paste0('ns_', i)]] <- station_numbers[[i]]
  }
  
  rm(station_numbers)
  rm(retrieve_date)
  
  trim_fire_o <- trim_fire_o %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)
  
  # read in weather data
  weather <- read_csv("Data/weather.csv")
  weather <- weather[,-1]
  weather$date <- make_date(year = weather$year, month = weather$month, day = weather$day)
  weather <- select(weather, station_number, date, solar_exposure)
  
  trim_fire_o <- left_join(trim_fire_o, weather, by = c("station_id" = "station_number", "date" = "date"))
  
  rm(weather)
  rm(id)
  
  trim_fire_o <- select(trim_fire_o, -station_id)
  
  trim_fire_o <- spread(trim_fire_o, key = ns, value = solar_exposure)
  
  trim_fire_o <- select(trim_fire_o, date, id, ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)
  
  trim_fire_o$solar_exposure <- apply(select(trim_fire_o, -date, -id), 
                                1, 
                                FUN = function(x){
                                  temp <- which(!is.na(x))
                                  return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                })
  
  trim_fire_o <- select(trim_fire_o, id, date, solar_exposure)
  
  trim_fire_o <- trim_fire_o %>%
    group_by(id) %>%
    summarise(outputs = ss(date, solar_exposure)) %>%
    ungroup()
  
  trim_fire_o <- trim_fire_o %>%
    separate(outputs, 
             into = c("se" ,"ase7", "ase14", "ase28", "ase60", "ase90", "ase180", "ase360", "ase720"),
             sep = ",",
             convert = TRUE)
  
  if (length(store_result) == 0){
    store_result <- trim_fire_o
    rm(trim_fire_o)
  } else {
    store_result <- bind_rows(store_result, trim_fire_o)
  }
  
  
}

fire_o <- left_join(fire_o, store_result, by = "id")
rm(store_result)

# Join maximum temp

store_result <- c()

for (k in 2:12){
  
  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]
  
  # select date and stations number
  trim_fire_o <- select(fire_o[start_cut:end_cut, ], FIRE_START, id, ns_1:ns_10)
  
  # extend date
  retrieve_date <- c()
  id <- rep(trim_fire_o$id, 720)
  station_numbers <- list()
  
  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_fire_o[[paste0('ns_', i)]], 720)
  } 
  
  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_fire_o$FIRE_START - i)
  }
  
  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")
  
  trim_fire_o <- data.frame(date = retrieve_date, id = id)
  
  for (i in 1:10){
    trim_fire_o[[paste0('ns_', i)]] <- station_numbers[[i]]
  }
  
  rm(station_numbers)
  rm(retrieve_date)
  
  trim_fire_o <- trim_fire_o %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)
  
  # read in weather data
  weather <- read_csv("Data/weather.csv")
  weather <- weather[,-1]
  weather$date <- make_date(year = weather$year, month = weather$month, day = weather$day)
  weather <- select(weather, station_number, date, max_temperature)
  
  trim_fire_o <- left_join(trim_fire_o, weather, by = c("station_id" = "station_number", "date" = "date"))
  
  rm(weather)
  rm(id)
  
  trim_fire_o <- select(trim_fire_o, -station_id)
  
  trim_fire_o <- spread(trim_fire_o, key = ns, value = max_temperature)
  
  trim_fire_o <- select(trim_fire_o, date, id, ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)
  
  trim_fire_o$max_temperature <- apply(select(trim_fire_o, -date, -id), 
                                      1, 
                                      FUN = function(x){
                                        temp <- which(!is.na(x))
                                        return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                      })
  
  trim_fire_o <- select(trim_fire_o, id, date, max_temperature)
  
  trim_fire_o <- trim_fire_o %>%
    group_by(id) %>%
    summarise(outputs = ss(date, max_temperature)) %>%
    ungroup()
  
  trim_fire_o <- trim_fire_o %>%
    separate(outputs, 
             into = c("maxt" ,"amaxt7", "amaxt14", "amaxt28", "amaxt60", "amaxt90", "amaxt180", "amaxt360", "amaxt720"),
             sep = ",",
             convert = TRUE)
  
  if (length(store_result) == 0){
    store_result <- trim_fire_o
    rm(trim_fire_o)
  } else {
    store_result <- bind_rows(store_result, trim_fire_o)
  }
  
  
}

fire_o <- left_join(fire_o, store_result, by = "id")
rm(store_result)

# Join minimum temp

store_result <- c()

for (k in 2:12){
  
  start_cut <- cut_off[k-1] + 1
  end_cut <- cut_off[k]
  
  # select date and stations number
  trim_fire_o <- select(fire_o[start_cut:end_cut, ], FIRE_START, id, ns_1:ns_10)
  
  # extend date
  retrieve_date <- c()
  id <- rep(trim_fire_o$id, 720)
  station_numbers <- list()
  
  for (i in 1:10){
    station_numbers[[i]] <- rep(trim_fire_o[[paste0('ns_', i)]], 720)
  } 
  
  for (i in 1:720){
    retrieve_date <- c(retrieve_date, trim_fire_o$FIRE_START - i)
  }
  
  retrieve_date <- as.Date(retrieve_date, origin = "1970-01-01")
  
  trim_fire_o <- data.frame(date = retrieve_date, id = id)
  
  for (i in 1:10){
    trim_fire_o[[paste0('ns_', i)]] <- station_numbers[[i]]
  }
  
  rm(station_numbers)
  rm(retrieve_date)
  
  trim_fire_o <- trim_fire_o %>%
    gather(key = "ns", value = "station_id", ns_1:ns_10)
  
  # read in weather data
  weather <- read_csv("Data/weather.csv")
  weather <- weather[,-1]
  weather$date <- make_date(year = weather$year, month = weather$month, day = weather$day)
  weather <- select(weather, station_number, date, min_temperature)
  
  trim_fire_o <- left_join(trim_fire_o, weather, by = c("station_id" = "station_number", "date" = "date"))
  
  rm(weather)
  rm(id)
  
  trim_fire_o <- select(trim_fire_o, -station_id)
  
  trim_fire_o <- spread(trim_fire_o, key = ns, value = min_temperature)
  
  trim_fire_o <- select(trim_fire_o, date, id, ns_1, ns_2, ns_3, ns_4, ns_5, ns_6, ns_7, ns_8, ns_9, ns_10)
  
  trim_fire_o$min_temperature <- apply(select(trim_fire_o, -date, -id), 
                                       1, 
                                       FUN = function(x){
                                         temp <- which(!is.na(x))
                                         return(ifelse(length(temp) > 0, x[min(temp)], NA))
                                       })
  
  trim_fire_o <- select(trim_fire_o, id, date, min_temperature)
  
  trim_fire_o <- trim_fire_o %>%
    group_by(id) %>%
    summarise(outputs = ss(date, min_temperature)) %>%
    ungroup()
  
  trim_fire_o <- trim_fire_o %>%
    separate(outputs, 
             into = c("mint" ,"amint7", "amint14", "amint28", "amint60", "amint90", "amint180", "amint360", "amint720"),
             sep = ",",
             convert = TRUE)
  
  if (length(store_result) == 0){
    store_result <- trim_fire_o
    rm(trim_fire_o)
  } else {
    store_result <- bind_rows(store_result, trim_fire_o)
  }
  
  
}

fire_o <- left_join(fire_o, store_result, by = "id")
rm(store_result)

# Join CFA station
cfa <- st_read("data/SDM697752/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer/geomark_point.shp")
cfa <- cfa %>% filter(FEATSUBTYP == "fire station")
cfa <- st_transform(cfa, crs = 4326)

# Find the nearest CFA station
nearest_index <- st_nearest_feature(fire_o$geometry, cfa$geometry)
temp <- geodist::geodist(select(fire_o, lon, lat), 
                         select(as_tibble(st_coordinates(cfa$geometry[nearest_index])),
                                lon = X, lat = Y),
                         paired = TRUE)
fire_o$dist_cfa <- as.numeric(temp)
rm(cfa)
rm(temp)

# Join recreation site
camp <- st_read("Data/SDM698290/ll_gda94/sde_shape/whole/VIC/FORESTS/layer/recweb_site.shp")

camp <- st_transform(camp, crs = 4326)
nearest_index <- st_nearest_feature(fire_o$geometry, camp$geometry)
temp <- geodist::geodist(select(fire_o, lon, lat), 
                         select(as_tibble(st_coordinates(camp$geometry[nearest_index])),
                                lon = X, lat = Y),
                         paired = TRUE)
fire_o$dist_camp = as.numeric(temp)
rm(camp)
rm(temp)


# Join wind data


# unzip all files
for (i in 2000:2018){
  unzip(paste0("data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/Aust_Wind_2deg_", i, "_V2.zip"),
        exdir = "data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip")
}

for (i in 2011:2018){
  if (i == 2013){
    next
  }
  base_file <- paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/', i)
  files <- list.files(base_file)
  for (j in files){
    file.copy(from = paste0(base_file, '/', j),
              to = paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/', j))
  }
  
  unlink(paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/', i), recursive = TRUE)
}

base_file <- 'data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/Aust_Wind_2deg_2009_V2'
files <- list.files(base_file)
for (j in files){
  file.copy(from = paste0(base_file, '/', j),
            to = paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/', j))
}
unlink(base_file, recursive = TRUE)

base_file <- 'data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/Aust_Wind_2deg_2010_V2'
files <- list.files(base_file)
for (j in files){
  file.copy(from = paste0(base_file, '/', j),
            to = paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/', j))
}
unlink(base_file, recursive = TRUE)

# rename files
date_13_16 <- seq(as.numeric(as.Date("2013-01-01")), as.numeric(as.Date("2016-12-31")))
for (i in date_13_16){

  before_name <- paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/', 
                        format(as.Date(i, origin = "1970-01-01"), "%Y%m%d"),
                        '_Aus_2d.flt')
  after_name <- paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/Aust_Wind_2deg_', 
                       format(as.Date(i, origin = "1970-01-01"), "%Y%m%d"),
                       '_V2.flt')
  file.rename(from = before_name, after_name)
}



# copy hdr file
base_file <- 'data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/Aust_Wind_Float_2deg_Envi_header.hdr'

date_00_18 <- seq(as.numeric(as.Date("2000-01-01")), as.numeric(as.Date("2018-12-31")))

for (i in date_00_18){
  copy_to_name <- paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/Aust_Wind_2deg_', 
                         format(as.Date(i, origin = "1970-01-01"), "%Y%m%d"), 
                         '_V2.hdr')
  file.copy(from = base_file, to = copy_to_name, overwrite = TRUE)
}

# trim fire_o
trim_fire_o <- select(fire_o, FIRE_START, lon, lat)

trim_fire_o$FIRE_START <- paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip/Aust_Wind_2deg_', 
                                 format(trim_fire_o$FIRE_START, "%Y%m%d"), 
                                 '_V2.flt')

trim_fire_o$FIRE_START <- ifelse(fire_o$FIRE_START > "2018-12-31", NA, trim_fire_o$FIRE_START)


# extract coordinates information
fire_points <- select(trim_fire_o, lon ,lat) %>%
  as.data.frame()

# project to EPSG:3577
fire_points <- SpatialPoints(fire_points, proj4string=CRS("+init=epsg:4326"))

temp <- rep(0, nrow(trim_fire_o))
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = nrow(fire_o))


# get daily wind speed 
for (i in 1:nrow(trim_fire_o)){
  
  pb$tick(1)
  
  if (is.na(trim_fire_o$FIRE_START[i])){
    temp[i] <- NA
    next
  }
  
  x <- raster(trim_fire_o$FIRE_START[i])
  temp[i] <- extract(x, fire_points[i])
  rm(x)
  
}

fire_o$ws <- temp

# monthly wind speed
trim_fire_o <- select(fire_o, FIRE_START, lon, lat)

unzip("data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/Monthly_Aust_Wind_2deg_1975_2018.zip",
      exdir = "data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip_m")

base_file <- 'data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/Aust_Wind_Float_2deg_Envi_header.hdr'
for (i in 1995:2018){
  for (j in 1:12){
    file.copy(from = base_file, 
              to = paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip_m/MTH/', 
                          i,
                          '/Aust_Wind_2deg_', 
                          i,
                          sprintf("%02d", j),
                          '_V2.hdr'),
              overwrite = TRUE)
  }
}

for (i in 2015:2016){
  for (j in 1:12){
    file.rename(from = paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip_m/MTH/', 
                              i,
                              '/Aust_Wind_2deg_', 
                              i,
                              sprintf("%02d", j),
                              '_V2'),
                to = paste0('data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip_m/MTH/', 
                            i,
                            '/Aust_Wind_2deg_', 
                            i,
                            sprintf("%02d", j),
                            '_V2.flt'))
  }
}

wind <- list()
k <- 0

for (i in 1998:2018){
  for (j in 1:12){
    k <- k + 1
    wind[[k]] <- raster(paste0("data/Near-surface_Wind_Speed/mcvicar_etal_grl2008/2deg/unzip_m/MTH/",
                               i,
                               '/Aust_Wind_2deg_', 
                               i,
                               sprintf("%02d", j), 
                               '_V2.flt'))
  }
}

trim_fire_o$year <- year(trim_fire_o$FIRE_START)
trim_fire_o$month <- month(trim_fire_o$FIRE_START)
trim_fire_o$k <- (trim_fire_o$year - 1998) * 12 + (trim_fire_o$month - 1) + 1

wind_record <- matrix(999, nrow = length(wind), ncol = nrow(trim_fire_o))

for (i in 1:length(wind)){
  wind_record[i,] <- extract(wind[[i]], fire_points)
}

rm(wind)

month0 <- rep(0, nrow(trim_fire_o))
month1 <- rep(0, nrow(trim_fire_o))
month3 <- rep(0, nrow(trim_fire_o))
month6 <- rep(0, nrow(trim_fire_o))
month12 <- rep(0, nrow(trim_fire_o))
month24 <- rep(0, nrow(trim_fire_o))

wind_record[wind_record<0] <- NA

# process monthly wind speed for this month, last month, last 3 month, last 6 month, last year and last 2 years

for (i in 1:nrow(trim_fire_o)){
  k <- trim_fire_o$k[i]
  if (k > dim(wind_record)[1]){
    next
  }
  temp <- wind_record[(k-24):k,i]
  month0[i] <- temp[25]
  month1[i] <- temp[24]
  month3[i] <- mean(temp[22:24], na.rm = TRUE)
  month6[i] <- mean(temp[19:24], na.rm = TRUE)
  month12[i] <- mean(temp[13:24], na.rm = TRUE)
  month24[i] <- mean(temp[1:24], na.rm = TRUE)
}

fire_o$aws_m0 <- month0
fire_o$aws_m1 <- month1
fire_o$aws_m3 <- month3
fire_o$aws_m6 <- month6
fire_o$aws_m12 <- month12
fire_o$aws_m24 <- month24

fire_o$aws_m0 <- ifelse(fire_o$aws_m0 == 0, NA, fire_o$aws_m0)
fire_o$aws_m1 <- ifelse(fire_o$aws_m1 == 0, NA, fire_o$aws_m1)
fire_o$aws_m3 <- ifelse(fire_o$aws_m3 == 0, NA, fire_o$aws_m3)
fire_o$aws_m6 <- ifelse(fire_o$aws_m6 == 0, NA, fire_o$aws_m6)
fire_o$aws_m12 <- ifelse(fire_o$aws_m12 == 0, NA, fire_o$aws_m12)
fire_o$aws_m24 <- ifelse(fire_o$aws_m24 == 0, NA, fire_o$aws_m24)

rm(trim_fire_o)

# Join road distance

road <- st_read("data/australia-latest-free.shp/gis_osm_roads_free_1.shp")
roads_in_vic <- st_intersects(vic_map$geometry, road$geometry)
road <- road[roads_in_vic[[1]],]
rm(roads_in_vic)
nearest_index <- st_nearest_feature(fire_o$geometry, road$geometry)
temp <- st_distance(fire_o$geometry, road$geometry[nearest_index], by_element = TRUE)
fire_o$dist_road <- as.numeric(temp)
rm(road)

# rename fields
# filter duplicated cases
fire_o <- fire_o[!(duplicated(fire_o$EVENTID) & is.na(fire_o$FIRE_STAT)),]
id_sets <- fire_o$EVENTID[duplicated(fire_o$EVENTID)]
fire_o <- fire_o[!(fire_o$EVENTID %in% id_sets & is.na(fire_o$FIRE_STAT)),]
fire_o <- select(fire_o, 
                 -geometry, 
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


fire_o <- fire_o %>%
  mutate(new_cause = 'other') %>%
  mutate(new_cause = ifelse(CAUSE == "LIGHTNING", "lightning", new_cause)) %>%
  mutate(new_cause = ifelse(CAUSE == "DELIBERATE LIGHTING (MALICIOUS)", "arson", new_cause)) %>%
  mutate(new_cause = ifelse(CAUSE %in% c('BURNING OFF (DEPARTMENTAL PRESCRIBED)',
                                         'BURNING OFF, STUBBLE, GRASS, SCRUB',
                                         'BURNING OFF, WINDROW, HEAP',
                                         'WASTE DISPOSAL, DOMESTIC',
                                         'WASTE DISPOSAL, INDUSTRIAL, SAWMILL, TIP'), 'burning_off_human', new_cause)) %>%
  mutate(new_cause = ifelse(CAUSE %in% c("CAMPFIRE, BARBEQUE",
                                         "FIREWORKS",
                                         "UNATTENDED CAMPFIRE - CONTAINED WITHIN BOUNDARY",
                                         'BURNING BUILDING',
                                         'BURNING HOUSE, STOVE, FLUE',
                                         'BURNING VEHICLE, MACHINE',
                                         'EXHAUST, CHAINSAW',
                                         'EXHAUST, OTHER',
                                         'PIPE, CIGARETTE, MATCH',
                                         'SNIGGING, HAULING'), "acidental_human", new_cause)) %>%
  mutate(new_cause = ifelse(CAUSE %in% c('RELIGHT - BURNING OFF',
                                         'RELIGHT - PRESCRIBED FIRE',
                                         'RELIGHT - WILDFIRE'), 'relight', new_cause))


fire_o$new_cause[which(is.na(fire_o$new_cause))] <- 'other'

write.csv(fire_o, "training.csv", row.names = FALSE)



