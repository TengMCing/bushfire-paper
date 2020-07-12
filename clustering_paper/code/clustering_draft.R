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

target_area = target_area[1:1]

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
  mutate(time_id = hour_id) %>%
  mutate(id = 1:nrow(hotspots)) %>%
  select(id, lon, lat, time_id)

# Save trimmed hotspots data in database
my_db_file <- "data/hotspots-trimmed.sqlite"
if (file.exists(my_db_file)) file.remove(my_db_file) 
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(my_db, "HOTSPOTS", hotspots)
RSQLite::dbDisconnect(my_db)
rm(hotspots)

# Reconnect to the database
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)


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
#   method:       clustering method. Either "null", "mean_r", or 'max_r'.                #
#   adj_distance: distance (0 to 1e5 m) for two vertices to be considered as adjacent.   #                                                                               #
#   active_time:  units of time a clusters remain active.                                #
#****************************************************************************************#

# Function to calculate mean and max distance to the centroid
calc_r <- function(lon, lat){
  cen_lon <- mean(lon)
  cen_lat <- mean(lat)
  centroid <- data.frame(lon = cen_lon, lat = cen_lat)
  points <- data.frame(lon = lon, lat = lat)
  dist_vector <- geodist::geodist(points, centroid)
  
  paste(format(mean(dist_vector), digits = 3), 
        format(max(dist_vector), digits = 3),
        sep = ",") %>%
    return()
}



hotspots_clustering <- function(con,
                                table_name = "",
                                method = "null",
                                adj_distance = 3e3,
                                active_time = 24){
  
  if (table_name == "") stop("Argument table_name is missing")
  
  if (!DBI::dbExistsTable(con, table_name)){
    stop(paste0(table_name, "does not exist"))
  }
  
  if (any(DBI::dbListFields(con, table_name) != c("id", "lon", "lat", "time_id"))){
    stop("fields name do not match with 'id, lon, lat, time_id'")
  }
  
  if (!method %in% c("null", "mean_r", "max_r")){
    stop(paste0("invaild method ", method))
  }
  
  # Get the max timestamp
  max_time <- tbl(con, sql(paste0("SELECT MAX(time_id) FROM ", table_name))) %>%
    collect() %>%
    .[[1]]
  
  if (!((is.atomic(max_time)) & (length(max_time == 1)))) stop("max_time contains more than 1 value")
  
  # Get number of rows
  max_obs <- tbl(con, sql(paste0("SELECT COUNT(time_id) FROM ", table_name))) %>%
    collect() %>%
    .[[1]]
  
  if (!((is.atomic(max_obs)) & (length(max_obs == 1)))) stop("max_obs contains more than 1 value")
  
  print(paste0("1/", max_time))
  
  # Initialize hotspots memberships
  fire_id <- c()
  id <- c()
  
  # Algorithm start from the first timestamp
  # Select the first timestamp data
  current_time_data <- tbl(con, table_name) %>% 
    filter(time_id == 1) %>%
    collect()
  
  if (nrow(current_time_data) == 0) stop("There is no data in the table")
  
  # Compute the distance matrix
  dist_matrix <- current_time_data %>%
    select(lon, lat) %>%
    geodist::geodist()
  
  if (!is.matrix(dist_matrix)) stop("distance matrix is not a matrix")
  
  # Create adjacency matrix
  adj_matrix <- dist_matrix <= adj_distance
  
  # Create graph from adjacency matrix
  current_graph <- graph.adjacency(adj_matrix, mode = 'undirected')
  
  # Compute clusters
  current_clusters = clusters(current_graph)
  
  # Assign membership to each point
  current_time_data$fire_id <- current_clusters$membership
  
  # Assign the fire_id to a vector
  fire_id <- c(fire_id, current_time_data$fire_id)
  id <- c(id, current_time_data$id)
  fire_move_ls <- vector(mode = "list", length = max_time)
  
  # Compute the centroid of each group
  fire_mov <- current_time_data %>%
    group_by(fire_id) %>%
    summarise(r = calc_r(lon, lat),
              lon = mean(lon), 
              lat = mean(lat),
              mean_r = as.numeric(stringr::str_split(r, ",")[[1]][1]),
              max_r = as.numeric(stringr::str_split(r, ",")[[1]][2])) %>%
    ungroup() %>%
    mutate(active = 0) %>%
    mutate(time_id = 1) %>%
    arrange(fire_id) %>%
    select(-r)
    
  
  # Store fire movement in list
  fire_move_ls[[1]] <- fire_mov
  
  
  for (i in 2:max_time){
    
    print(paste0(i, "/", max_time))
    
    # Let the `active` minus 1
    fire_mov$active <- fire_mov$active - 1
    
    # Get the current timestamp hotspots data
    current_time_data <- tbl(con, table_name) %>% 
      filter(time_id == i) %>%
      collect()
    
    if (nrow(current_time_data) == 0) stop(paste0("There is no data in timestamp", i))
    
    # Get the active fire center
    active_group <- filter(fire_mov, active > -active_time)
    
    # Get row number
    total_current_obs <- nrow(current_time_data)
    total_active_group <- nrow(active_group)
    total_current_points <- total_current_obs + total_active_group
    
    print(paste0('points: ', total_current_points))
    
    # Append the groups geometry to the hotspots geometry
    if (total_active_group > 0){
      geom_info <- current_time_data %>%
        select(lon, lat) %>%
        bind_rows(select(active_group, lon, lat))
    } else {
      geom_info <- current_time_data %>%
        select(lon, lat)
    }
    
    # Compute the dist matrix
    dist_matrix <- geodist::geodist(geom_info)
    
    if (!is.matrix(dist_matrix)) stop("distance matrix is not a matrix")
    
    # # Create adjacency matrix
    # adj_matrix <- dist_matrix <= adj_distance
    # 
    # 
    # # Apply algorithm using nominated method
    # if ((total_active_group > 0) & (method %in% c("mean_r", "max_r"))){
    #   addition_metric <- active_group[[method]]
    #   
    #   # Extract distance matrix between hotspots and active groups
    #   partialA <- dist_matrix[1:total_current_obs,
    #                           (total_current_obs + 1):total_current_points,
    #                           drop = FALSE]
    #   
    #   # Extract radius from active groups
    #   partialB <- matrix(rep(addition_metric, total_current_obs),
    #                      nrow = total_current_obs,
    #                      byrow = TRUE)
    #   
    #   if (all(dim(partialA) != dim(partialB))) stop("unmatched matrices")
    #   
    #   # Decide the adjacency partial matrix
    #   partialC <- (partialA <= partialB) + (partialA <= adj_distance)
    #   partialC <- partialC > 0
    #   
    #   # Reassign the adjacency matrix
    #   adj_matrix[1:total_current_obs,
    #              (total_current_obs + 1):total_current_points] <- partialC
    #   
    # }
    
    if ((total_active_group > 0) & (method %in% c("mean_r", "max_r"))){
      
      temp_matrix = matrix(0L, nrow = dim(dist_matrix)[1], ncol = dim(dist_matrix)[2])
      partial_1 = matrix(rep(active_group[[method]], dim(dist_matrix)[1]), nrow = dim(dist_matrix)[2], byrow = TRUE)
      partial_2 = matrix(rep(active_group[[method]], dim(dist_matrix)[2]), ncol = dim(dist_matrix)[2], byrow = FALSE)
      temp_matrix[(total_current_obs + 1):dim(dist_matrix)[1],] = partial_2
      temp_matrix[,(total_current_obs + 1):dim(dist_matrix)[2]] = partial_1
      
      neighbors = ((dist_matrix <= adj_distance) + (dist_matrix <= temp_matrix)) > 0
      
    } else {
      neighbors = (dist_matrix <= adj_distance)
    }
    
    # Create graph from adjacency matrix
    current_graph <- graph.adjacency(dist_matrix <= adj_distance, mode = 'undirected') 
    
    # Compute clusters
    current_clusters = clusters(current_graph)
    
    # Assign membership to each point
    current_time_data$fire_id <- current_clusters$membership[1:total_current_obs]
    
    # Adjust fire_id for hotspots
    if (total_active_group > 0){
      active_group$new_fire_id <- current_clusters$membership[(total_current_obs + 1):total_current_points]
    
      # Expand all combination between memberships of hotspots and active groups
      combination_tbl <- expand.grid(current_time_data$fire_id, active_group$new_fire_id)
      
      # A vector to represent matched membership
      combination_tbl <- combination_tbl$Var1 == combination_tbl$Var2
      
      # Turn this to a matrix
      mat_a <- matrix(combination_tbl, nrow = total_current_obs)
      
      # Extract distance matrix
      mat_b <- dist_matrix[1:total_current_obs,(total_current_obs + 1):total_current_points]
      if (is.vector(mat_b)) {mat_b <- matrix(mat_b, nrow = total_current_obs) }
      
      if (all(dim(mat_a) != dim(mat_b))) stop("unmatched matrices")
      
      # Combine membership matrix and distance matrix
      mat_c <- mat_a * mat_b
      
      if (sum(mat_c < 0) > 0) stop("mat_c contains negative value")
      if (anyNA(mat_c)) stop("mat_c contains NA")
      
      # Find positive min for each row
      nearest_active_group <- apply(mat_c,
                                    1,
                                    FUN = function(x){
                                      ifelse(sum(x) == 0,
                                             0,
                                             which(x == min(x[x > 0]))[1])
                                      })
      
      if (anyNA(nearest_active_group)) stop("nearest_active_group contians NA")
      if (!is.atomic(nearest_active_group)) stop("nearest_active_group is not a vector")
      
      # Assign nearest active group fire_id to hotspots
      if (sum(nearest_active_group > 0) > 0){
        
        hotspots_index <- which(nearest_active_group > 0)
        active_group_index <- nearest_active_group[hotspots_index]
        
        current_time_data$fire_id[hotspots_index] <- active_group$fire_id[active_group_index]
        }
  
      }
    
    
    # Adjust membership label
    if (total_active_group > 0){
      index <- -hotspots_index
    } else {
      index <- 1:total_current_obs
    }
    
    temp_membership <- current_time_data$fire_id[index]
    
    # Let membership start from 1 and common difference equal to 1
    if (length(temp_membership) > 0) {
      temp_membership <- data.frame(temp_membership = temp_membership) %>%
        group_indices(temp_membership)
      
      # Add the total known group
      temp_membership <- temp_membership + max(fire_mov$fire_id)
      
      # Assign them back to hotspots
      current_time_data$fire_id[index] <- temp_membership
    }
    
    # Process fire movement
    current_fire_mov <- current_time_data %>%
      group_by(fire_id) %>%
      summarise(r = calc_r(lon, lat),
                lon = mean(lon), 
                lat = mean(lat),
                mean_r = as.numeric(stringr::str_split(r, ",")[[1]][1]),
                max_r = as.numeric(stringr::str_split(r, ",")[[1]][2])) %>%
      ungroup() %>%
      mutate(active = 0) %>%
      mutate(time_id = i) %>%
      arrange(fire_id) %>%
      select(-r)
    
    if (any(duplicated(current_fire_mov$fire_id))) stop("duplicated fire_id found")
    
    # Update information in fire_mov
    fire_mov <- fire_mov %>%
      filter(!(fire_id %in% current_fire_mov$fire_id))
    fire_mov <- bind_rows(fire_mov, current_fire_mov)
    fire_mov <- arrange(fire_mov, fire_id)
    fire_mov <- mutate(fire_mov, time_id = i)
    
    # Store fire movement in list
    fire_move_ls[[i]] <- fire_mov

    # Assign the fire_id to a vector
    fire_id <- c(fire_id, current_time_data$fire_id)
    id <- c(id, current_time_data$id)
  }

  return(list(fire_id, fire_move_ls))
}




###############################################

test <- hotspots_clustering(con = my_db,
                    table_name = "HOTSPOTS",
                    method = "max_r",
                    adj_distance = 3e3,
                    active_time = 24)

test1 <- hotspots_clustering(con = my_db,
                             table_name = "HOTSPOTS",
                             method = "null",
                             adj_distance = 3e3,
                             active_time = 24)
