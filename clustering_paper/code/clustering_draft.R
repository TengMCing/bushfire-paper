# Clustering

# Load libraries
library(tidyverse)
library(sf)
library(furrr)
library(lubridate)
library(rnaturalearth)
library(igraph)
library(here)

# Hyperparameters

# Start date and end date of hotspots data (yyyymmdd)
start_date <-  "20191001"
end_date <-  "20200331"

# Hotspots data location
hotspots_data_dir <-  paste0(here(),'/data/Himawari-8')

# Firepower filtering point
left_truncation_firepower <-  100

# Target area
target_area <-  c('Victoria',
                  'New South Wales',
                  'Northern Territory',
                  'South Australia',
                  'Western Australia',
                  'Queensland',
                  'Tasmania',
                  'Australian Capital Territory')

target_area = target_area[1:8]

# Clustering distance(m)
cl_dist <-  3000

# Clustering method
cl_method <-  c("mean", 'min', 'max', 'null')
cl_method <-  cl_method[4]

# Output file names
output_hotspots_name <-  "clustered_hotspots.csv"
output_fire_name <-  "fire_mov.csv"



# Read in hotspots data
read_hotspots <- function(directory = "data/Himawari-8", start = '20190501', end = '20190701'){
  
  # Read in hotspots data from 'start' date to 'end' date
  
  fls_full <-  list.files(directory, full.names = TRUE)
  fls_short <-  list.files(directory, full.names = FALSE)
  
  fls_filter <-  (fls_short > paste('H08', start, sep = '_')) & (fls_short < paste('H08', end, sep = '_'))
  fls_full <-  fls_full[fls_filter]
  
  all <-  map(fls_full, read_csv, quote="'")
  d <-  bind_rows(all)
  
  return(d)
  
}



# Prepare hotspots data
data_preparation <- function(start_date = start_date,
                             end_date = end_date,
                             hotspots_data_dir = hotspots_data_dir,
                             left_truncation_firepower = left_truncation_firepower,
                             target_area = target_area){
  
  # read in data
  print('1/5 Read in hotspots data.')
  hotspots <-  read_hotspots(start = start_date, end = end_date, directory = hotspots_data_dir)
  
  # Select hotspots in Australia
  print('2/5 Select hotspots in Australia.')
  hotspots <-  hotspots %>%
    filter(between(lon, 112, 155)) %>% 
    filter(between(lat, -44, -10))
  
  
  
  # Load map of australia
  au_map <-  ne_states(country = 'Australia', returnclass = 'sf')
  
  # Filtering hotspots by firepower
  print('3/5 Filtering hotspots by firepower.')
  hotspots <-  hotspots %>%
    filter(firepower > left_truncation_firepower)
  
  
  
  # Transform hotspots to sf object
  hotspots <- st_as_sf(x = hotspots, coords = c('lon','lat'), crs = 4326)
  
  # Filtering hotspots by target states
  print('4/5 Filtering hotspots by target states.')
  states <-  st_intersects(au_map$geometry, hotspots$geometry)
  hotspots$state <-  ''
  
  for (i in seq(1,nrow(au_map))){
    
    hotspots$state[states[[i]]] <-  au_map$name[i]
    
  }
  
  hotspots <- hotspots %>%
    filter(state %in% target_area)
  
  # Convert hotspots data back to data frame
  coords <- st_coordinates(hotspots)
  st_geometry(hotspots) <- NULL
  hotspots$lon = coords[,1]
  hotspots$lat = coords[,2]
  
  # Extract time features and assign hour id
  print('5/5 Assign hour id to hotspots data.')
  hotspots$year <-  year(hotspots$`#obstime`)
  hotspots$month <-  month(hotspots$`#obstime`)
  hotspots$day <-  day(hotspots$`#obstime`)
  hotspots$week <-  week(hotspots$`#obstime`)
  hotspots$hour <-  hour(hotspots$`#obstime`)
  hotspots <- arrange(hotspots, `#obstime`)
  
  hotspots$hour_id <- hotspots %>%
    group_indices(year,month,day,hour)
  
  
  print('Hotspots data preprocessing finished!')
  
  return(hotspots)
  
}


# preprocess hotspots data
hotspots <- data_preparation(start_date = start_date,
                             end_date = end_date,
                             hotspots_data_dir = hotspots_data_dir,
                             left_truncation_firepower = left_truncation_firepower,
                             target_area = target_area)

# Save enitre hotspots data in database
my_db_file <- "data/hotspots.sqlite"
if (file.exists(my_db_file)) file.remove(my_db_file)
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(my_db, "HOTSPOTS", hotspots)
RSQLite::dbDisconnect(my_db)


# Select coordinates and hour_id
hotspots <- hotspots %>%
  mutate(timestamp_id = hour_id) %>%
  mutate(id = 1:nrow(hotspots)) %>%
  select(id, lon, lat, timestamp_id)

# Save trimmed hotspots data in database
my_db_file <- "data/hotspots-trimmed.sqlite"
if (file.exists(my_db_file)) file.remove(my_db_file) 
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(my_db, "HOTSPOTS", hotspots)
RSQLite::dbDisconnect(my_db)
rm(hotspots)

# Reconnect to the database
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)

DBI::dbDisconnect(my_db)

#****************************************************************************************#
# Clustering Algorithm for remote sensing hotspots data                                  #
#                                                                                        #
# Default:                                                                               #
#                                                                                        #
#   hotspots_clustering(x, method = "null", adj_distance = 3e3, active_time = 24)        #
#                                                                                        #
# Arguments:                                                                             #
#                                                                                        #
#   con:          a dbConnect() (SQLite) object represents the hotspots data with four   #
#                 columns, id, lon, lat and timestamp_id.                                #
#   table_name:   a table name of where the hotspots data stored in the database.        #
#   method:       clustering method. Either "null", "mean", or 'max'.                    #
#   adj_distance: distance (0 to 1e5 m) for two vertices to be considered as adjacent.   #                                                                               #
#   active_time:  units of time a clusters remain active.                                #
#****************************************************************************************#

# Function to calculate mean distance to the centroid
calc_mean_r <- function(lon, lat){
  cen_lon <- mean(lon)
  cen_lat <- mean(lat)
  centroid <- data.frame(lon = cen_lon, lat = cen_lat)
  points <- data.frame(lon = lon, lat = lat)
  mean_dist <- geodist::geodist(points, centroid) %>%
    mean() %>%
  
    return(mean_dist)
}

# Function to calculate max distance to the centroid
calc_max_r <- function(lon, lat){
  cen_lon <- mean(lon)
  cen_lat <- mean(lat)
  centroid <- data.frame(lon = cen_lon, lat = cen_lat)
  points <- data.frame(lon = lon, lat = lat)
  mean_dist <- geodist::geodist(points, centroid) %>%
    max() %>%
    
    return(mean_dist)
}

hotspots_clustering <- function(con,
                                table_name = "",
                                method = "null",
                                adj_distance = 3e3,
                                active_time = 24){
  
  if (table == "") stop("Argument table_name is missing")
  
  if (!DBI::dbExistsTable(con, table_name)){
    stop(paste0(table_name, "does not exist"))
  }
  
  if (DBI::dbListFields(con, table_name) != c("id", "lon", "lat", "timestamp_id")){
    stop("fields name do not match with 'lon, lat, timestamp_id'")
  }
  
  # Get the max timestamp
  max_time <- tbl(con, sql(paste0("SELECT MAX(timestamp_id) FROM ", table_name))) %>%
    collect() %>%
    .[[1]]
  
  # Get number of rows
  max_obs <- tbl(con, sql(paste0("SELECT COUNT(timestamp_id) FROM ", table_name))) %>%
    collect() %>%
    .[[1]]
  
  print(paste0("1/", max_obs))
  
  # Initialize hotspots memberships
  fire_id <- vevtor(length = max_obs)
  
  # Algorithm start from the first hour
  # Select the first hour data
  current_hour_data <- tbl(con, tabl_name) %>% 
    filter(timestamp_id == 1) %>%
    collect()
  
  # Compute the distance matrix
  dist_matrix <- current_hour_data %>%
    select(lon, lat) %>%
    geodist::geodist()
  
  # Create graph from adjacency matrix
  current_graph <- graph.adjacency(dist_matrix <= adj_distance, mode = 'undirected')
  
  # Compute clusters
  current_clusters = clusters(current_graph)
  
  # Assign membership to each point
  current_hour_data$fire_id <- current_clusters$membership
  
  # Compute the centroid of each group
  fire_mov <- current_hour_data %>%
    group_by(fire_id) %>%
    summarise(lon = mean(lon), 
              lat = mean(lat), 
              mean_r = calc_mean_r(lon, lat), 
              max_r = calc_max_r(lon, lat)) %>%
    mutate(active = 0) %>%
    arrange(fire_id) %>%
    ungroup()
  
  # Store fire movement into database
  fire_mov_file <- "data/fire_movement.sqlite"
  if (file.exists(fire_mov_file)) file.remove(fire_mov_file) 
  my_fire_mov <- DBI::dbConnect(RSQLite::SQLite(), fire_mov_file)
  DBI::dbWriteTable(my_fire_mov, "FIRE_MOV", fire_mov)
  RSQLite::dbDisconnect(my_db)
  
  # Reconnect to the database
  my_fire_mov <- DBI::dbConnect(RSQLite::SQLite(), fire_mov_file)
  
  
  for (i in 2:max_time){
    
    print(paste0(i, "/", max_obs))
    
    # Copy the groups infomation from previous hour
    current_fire_mov <- fire_mov
    
    # Let the `active` minus 1
    current_fire_mov$active <- current_fire_mov$active - 1
    
    # Get the current hour hotspots data
    current_hour_data <- tbl(con, tabl_name) %>% 
      filter(timestamp_id == i) %>%
      collect()
    
    # Get the active fire center
    active_group <- filter(current_fire_mov, active > -active_time)
    
    # Append the groups geometry to the hotspots geometry
    if (nrow(active_group)>0){
      geom_info <- current_hour_data %>%
        select(lon, lat) %>%
        bind_rows(select(active_group, lon, lat))
    } else {
      geom_info <- current_hour_data %>%
        select(lon, lat)
    }
    
    # Compute the dist matrix (roughly compute to save time)
    dist_matrix <- dist(geom_info) * 100 * 1000 %>%
      as.matrix()
    
    # Create adjacency matrix
    adj_matrix <- dist_matrix <= adj_distance
    
    # 
    
    # Create graph from adjacency matrix
    current_graph <- graph.adjacency(dist_matrix <= adj_distance, mode = 'undirected') 
    
    
  }
  
  
  return(1)
}

#hotspots %>%
#  .$hour_id %>%
#  plyr::count() %>%
#  arrange(desc(freq))
