# Load libraries
library(tidyverse)
library(sf)
library(furrr)
library(lubridate)
library(rnaturalearth)
library(igraph)

# Read hotspots data
# Note data directory is one up from paper directory, for now
read_hotspot <- function(directory = "data/Himawari-8", from = '20190501', to = '20190701'){
  ### include from, exclude to
  
  fls = list.files(directory, full.names = TRUE)
  fls_short = list.files("data/Himawari-8", full.names = FALSE)
  fls_filter = (fls_short > paste('H08', from, sep = '_')) & (fls_short < paste('H08', to, sep = '_'))
  fls = fls[fls_filter]
  all = map(fls, read_csv, quote="'")
  d = bind_rows(all)
  return(d)
}
# hotspots = read_hotspot(from = "20191001", to ="20200331")
# Small set of data to test it
hotspots = read_hotspot(from = "20200201", to ="20200203")

# Data pre-processing
# filter au
hotspots = hotspots %>%
  filter(between(lon, 112, 155)) %>% 
  filter(between(lat, -44, -10))

# Make a map
au_map = ne_states(country = 'Australia', returnclass = 'sf') #get au map
ggplot(au_map) + geom_sf()

vic_map <- au_map %>% filter(name == "Victoria")
ggplot(vic_map) + geom_sf()

# Only keep most likely to be bushfires
hotspots = hotspots %>%
  filter(firepower > 100)

# Extract hotspots for Australia, and label with state name
hotspots = st_as_sf(x = hotspots, coords = c('lon','lat'))
st_crs(hotspots) = 4326
tab = st_intersects(au_map$geometry, hotspots$geometry)
hotspots$state = ''
for (i in seq(1,nrow(au_map))) {
  hotspots$state[tab[[i]]] = au_map$name[i]
}

unidentified_point = hotspots[hotspots$state == '',]
tab2 = st_nearest_feature(unidentified_point$geometry, au_map$geometry)
hotspots$state[hotspots$state == ''] = au_map$name[tab2]

hotspots$year = year(hotspots$`#obstime`)
hotspots$month = month(hotspots$`#obstime`)
hotspots$day = day(hotspots$`#obstime`)
hotspots$week = week(hotspots$`#obstime`)
hotspots$hour = hour(hotspots$`#obstime`)

# filter VIC
hotspots_VIC = hotspots %>%
  filter(state == "Victoria")
ggplot() + geom_sf(data=vic_map) + geom_sf(data=hotspots_VIC)

# assign hour index for data
hotspots_VIC$hour_id = hotspots_VIC %>%
  group_indices(year, month, day, hour)

hotspots_VIC$obs_id = 1:nrow(hotspots_VIC)
temp_fire_id = c()
temp_obs_id = c()

first_hour = filter(hotspots_VIC, hour_id == 1) %>%
  select(obs_id, year, month, day, hour_id, geometry)

ggplot() + geom_sf(data=vic_map) + geom_sf(data=first_hour)

# calculate spatial distance matrix of first hour, units in metres
dist_matrix = matrix(as.numeric(st_distance(first_hour$geometry, first_hour$geometry)),
                     ncol=nrow(first_hour))

# if the distance between two points is less than 3 km, create an edge between them
neighbors = (dist_matrix < 3000) # creates logical matrix

# create graph by adjacency matrix
first_graph = graph.adjacency(neighbors, mode = 'undirected')

# Plot it on a map
plot(first_graph)

# compute clusters
cluster_result = clusters(first_graph)

# assign membership to each point
first_hour$fire_id = cluster_result$membership

# record centers for each hour
group_centers = list()

# compute the centroid of each group
group_centers[[1]] = as.data.frame(st_coordinates(first_hour)) %>%
  mutate(fire_id = first_hour$fire_id) %>%
  group_by(fire_id) %>%
  summarise(lon = mean(X), lat = mean(Y))

group_centers[[1]] = st_as_sf(x = group_centers[[1]], coords = c("lon","lat"), crs = 4326)
group_centers[[1]]$active = 0

# assign the fire id back to the master data set
temp_fire_id = c(temp_fire_id, first_hour$fire_id)
temp_obs_id = c(temp_obs_id, first_hour$obs_id)

# match current_fire_id with previous_center_current_id
# if find mutiple previous_center, assign the nearest one
match_muti_fire = function(current_fire_id, previous_center_current_id, dist_matrix){
  
  # all pairs of two vectors
  tb = expand.grid(current_fire_id, previous_center_current_id)
  
  # mark pairs that have same group id
  tb$var3 = tb$Var1 == tb$Var2
  tb = tb$var3
  
  # turn this comparison into a matrix
  mat_a = matrix(tb, nrow = length(current_fire_id))
  
  # get the corresponding distrance matrix
  mat_b = dist_matrix[1:length(current_fire_id),(length(current_fire_id)+1):dim(dist_matrix)[2]]
  
  if (is.vector(mat_b)) {mat_b = matrix(mat_b, nrow = length(current_fire_id)) }
  
  # safe check
  if (all(dim(mat_a) != dim(mat_b))) {stop("different dim between dist_matrix and membership comparison matrix")}
  
  # combine two matrices
  mat_c = mat_a * mat_b
  
  # function to find row positive min
  find_positive_min = function(x){
    if (sum(x) == 0){
      return(NA)
    }
    return(which(x==min(x[x>0]))[1])
  }
  
  # for each row, find the positive min
  result = apply(mat_c, 1, FUN = find_positive_min)
  
  # safe check
  if (is.vector(result) == FALSE) {
    print(result)
    stop("result is not a vector")
    
  }
  
  # return the index vector
  return(result)
}

max_hour = max(hotspots_VIC$hour_id)
for (i in seq(2, max_hour)){
  
  # copy the centers info from last hour
  group_centers[[i]] = group_centers[[i-1]]
  # let the active minus 1
  group_centers[[i]]$active = group_centers[[i]]$active - 1
  
  
  # get the current hour hotspots data
  current_hour = filter(hotspots_VIC, hour_id == i) %>%
    select(obs_id, geometry)
  
  # get the active fire center
  active_group = filter(group_centers[[i]], active > -24)
  
  # append the center geometry to the hotspots geometry vector
  if (nrow(active_group)>0){
    geom_info = c(current_hour$geometry, active_group$geometry)
  } else {
    geom_info = current_hour$geometry
  }
  
  
  # compute the dist matrix
  dist_matrix = matrix(as.numeric(st_distance(geom_info, geom_info)),ncol =nrow(current_hour)+nrow(active_group))
  
  # connect two points if they have less than 3000m distance
  neighbors = (dist_matrix<3000) 
  
  # use the adjancency to create a graph
  current_graph = graph.adjacency(neighbors, mode = 'undirected')
  
  # find the components in the graph
  cluster_result = clusters(current_graph)
  
  # get the membership for the hotspots
  current_fire_id = cluster_result$membership[1:nrow(current_hour)]
  
  current_hour$fire_id = NA
  
  # get the membership for the active centers
  if (nrow(active_group)>0){
    previous_center_current_id = cluster_result$membership[(nrow(current_hour)+1):length(cluster_result$membership)]
    
    # if the hotspots share the same group as active centers, assign those hotspots a previous fire_id
    assign_ids = match_muti_fire(current_fire_id, previous_center_current_id, dist_matrix)
    if (all(is.na(assign_ids))) {
      current_hour$fire_id = NA
    } else {
      current_hour$fire_id =  active_group$fire_id[assign_ids]
    }
    
  }
  
  # hotspots that doesn't in the same group as active centers will have a missing value, we need to assign them new fire id
  
  # we first get the membership of those points
  temp_membership = current_fire_id[is.na(current_hour$fire_id)]
  
  # we need to slightly adjust the membership to make it start from 1 and common difference equal to 1
  temp_membership = data.frame(temp_membership = temp_membership) %>%
    group_indices(temp_membership)
  # and add the total number of fire we knew
  temp_membership = temp_membership + nrow(group_centers[[i]])
  
  
  # assign them back to those missing entries
  current_hour$fire_id[is.na(current_hour$fire_id)] = temp_membership
  
  
  # now all the current hour points has a fire id, we can recompute the centers
  current_centers = as.data.frame(st_coordinates(current_hour)) %>%
    mutate(fire_id = current_hour$fire_id) %>%
    group_by(fire_id) %>%
    summarise(lon = mean(X), lat = mean(Y))
  
  # mark those fire as active
  current_centers$active = 0
  current_centers = st_as_sf(x = current_centers, coords = c("lon","lat"), crs = 4326)
  
  # replace the center info
  group_centers[[i]] = group_centers[[i]] %>%
    filter(!(fire_id %in% current_centers$fire_id))
  group_centers[[i]] = rbind(group_centers[[i]], current_centers)
  group_centers[[i]] = arrange(group_centers[[i]], fire_id)
  
  # assign the fire id back to master data set
  temp_fire_id = c(temp_fire_id, current_hour$fire_id)
  temp_obs_id = c(temp_obs_id, current_hour$obs_id)
  
}

myfun = function(x,y){
  temp = as.data.frame(st_coordinates(x)) %>%
    mutate(hour_id = y)
  temp = temp %>%
    mutate(fire_id = 1:nrow(temp))
  temp
}

center_move = imap(group_centers, ~myfun(.x,.y)) %>%
  reduce(rbind)

hotspots_VIC$fire_id_3km = temp_fire_id
output_3km = as.data.frame(hotspots_VIC) %>%
  select(`#obstime`, firepower, state, year, month, day, week, hour, hour_id, obs_id, fire_id_3km)
temp = as.data.frame(st_coordinates(hotspots_VIC))
output_3km$lon = temp$X
output_3km$lat = temp$Y

# Plot it - small subset in time
ggplot() + geom_sf(data=vic_map) +
  geom_point(data=output_3km %>% filter(hour_id < 5), 
             aes(x=lon, y=lat, colour=factor(fire_id_3km))) +
  facet_wrap(~hour_id)

# Now over time, choose a prevalent cluster first
output_3km %>% count(fire_id_3km, sort=TRUE)
output_3km %>% filter(fire_id_3km == 12) %>%
  ggplot(aes(x=hour_id, y=1)) + geom_point()

# Over space, on local scale
output_3km %>% filter(fire_id_3km == 12) %>%
  ggplot(aes(x=lon, y=lat)) + geom_point() + 
  facet_wrap(~hour_id)

# Show two, and more
# 1, 8, 9 different location
output_3km %>% filter(fire_id_3km %in% c(2, 4, 5, 7, 12)) %>%
  ggplot(aes(x=lon, y=lat, colour=factor(fire_id_3km))) + geom_point() + 
  facet_wrap(~hour_id)

save(output_3km, file="clustering_paper/data/output_3km.rda")
save(vic_map, file="clustering_paper/data/vic_map.rda")
save(hotspots_VIC, file="clustering_paper/data/hotspots_VIC.rda")

write.csv(output_3km, 'data/hotspots_3km.csv', row.names = FALSE)
write.csv(center_move, 'data/fire_3km.csv', row.names = FALSE)







