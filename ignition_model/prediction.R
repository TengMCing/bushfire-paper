# RF

library(tidyverse)
library(caret)
library(randomForest)
library(lubridate)
library(rnaturalearth)
library(plotly)
library(sf)

set.seed(10086)

training <- read_csv("training.csv")
predict_x <- read_csv("predict_x.csv")
predict_x_grid <- read_csv("predict_x_grid.csv")

# unique(training$CAUSE)
# [1] "LIGHTNING"                                      
# [2] "BURNING OFF, STUBBLE, GRASS, SCRUB"             
# X [3] "BURNING VEHICLE, MACHINE"                       
# [4] "CAMPFIRE, BARBEQUE"                             
# [5] "BURNING OFF, WINDROW, HEAP"                     
# x [6] "OTHER"                                          
# [7] "DELIBERATE LIGHTING (MALICIOUS)"                
# x [8] "UNKNOWN"                                        
# x [9] "POWER TRANSMISSION"                             
# x [10] NA                                               
# x [11] "RELIGHT - BURNING OFF"                          
# [12] "EXHAUST, OTHER"                                 
# [13] "PIPE, CIGARETTE, MATCH"                         
# X [14] "WASTE DISPOSAL, DOMESTIC"                       
# [15] "BURNING OFF (DEPARTMENTAL PRESCRIBED)"          
# [16] "FIREWORKS"                                      
# x [17] "RELIGHT - WILDFIRE"                             
# [18] "SNIGGING, HAULING"                              
# X [19] "WASTE DISPOSAL, INDUSTRIAL, SAWMILL, TIP"       
# X [20] "BURNING HOUSE, STOVE, FLUE"                     
# [21] "EXHAUST, CHAINSAW"                              
# x [22] "RELIGHT - PRESCRIBED FIRE"                      
# X [23] "BURNING BUILDING"                               
# x [24] "TRAIN"                                          
# x [25] "NULL"                                           
# [26] "UNATTENDED CAMPFIRE - CONTAINED WITHIN BOUNDARY"

training <- training %>%
  filter(!CAUSE %in% c("BURNING BUILDING", 
                       "WASTE DISPOSAL, INDUSTRIAL, SAWMILL, TIP", 
                       "WASTE DISPOSAL, DOMESTIC", 
                       "BURNING VEHICLE, MACHINE",
                       "BURNING BUILDING")) %>%
  filter(new_cause != "other") %>%
  filter(new_cause != "relight")


training <- select(training, -c(EVENTID:FIRE_NUM), -id, -CAUSE, -FOREST, -FOR_CODE, -FOR_CAT)
predict_x <- select(predict_x, -id, -FOREST, -FOR_CODE, -FOR_CAT)
predict_x_grid <- select(predict_x_grid, -id, -FOREST, -FOR_CODE, -FOR_CAT)


training <- mutate(training, 
                   month = factor(month(FIRE_START), levels = c(1:12)),
                   day = factor(day(FIRE_START), levels = c(1:31)),
                   wod = factor(wday(FIRE_START), levels = c(1:7)))

training <- mutate(training, 
                   COVER = factor(COVER, levels = c(1,2,3,4,5,6)),
                   HEIGHT = factor(HEIGHT, levels = c(1,2,3,4,5,6)))

predict_x <- mutate(predict_x, 
                   month = factor(month(time), levels = c(1:12)),
                   day = factor(day(time), levels = c(1:31)),
                   wod = factor(wday(time), levels = c(1:7)))

predict_x <- mutate(predict_x, 
                   COVER = factor(COVER, levels = c(1,2,3,4,5,6)),
                   HEIGHT = factor(HEIGHT, levels = c(1,2,3,4,5,6)))

predict_x_grid <- mutate(predict_x_grid, 
                    month = factor(month(time), levels = c(1:12)),
                    day = factor(day(time), levels = c(1:31)),
                    wod = factor(wday(time), levels = c(1:7)))

predict_x_grid <- mutate(predict_x_grid, 
                    COVER = factor(COVER, levels = c(1,2,3,4,5,6)),
                    HEIGHT = factor(HEIGHT, levels = c(1,2,3,4,5,6)))

training <- select(training, -FIRE_START)
predict_x <- select(predict_x, -time)
predict_x_grid <- select(predict_x_grid, -time, -year)


training <- filter(training, month %in% c(10,11,12,1,2,3))


training <- na.omit(training)
predict_x <- na.omit(predict_x)
predict_x_grid <- na.omit(predict_x_grid)

training <- mutate(training, new_cause = ifelse(new_cause == "acidental_human", "accident", new_cause)) %>%
  mutate(new_cause = ifelse(new_cause == "burning_off_human", "burning_off", new_cause)) %>%
  mutate(new_cause = factor(new_cause)) %>%
  mutate(FOR_TYPE = factor(FOR_TYPE))

predict_x <- predict_x %>%
  mutate(FOR_TYPE = factor(FOR_TYPE, levels = levels(training$FOR_TYPE)))

predict_x_grid <- predict_x_grid %>%
  mutate(FOR_TYPE = factor(FOR_TYPE, levels = levels(training$FOR_TYPE)))

training <- na.omit(training)
predict_x <- na.omit(predict_x)
predict_x_grid <- na.omit(predict_x_grid)

# train and test split

inTraining <- createDataPartition(training$new_cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

rf_model <- randomForest(new_cause~., data = train_set, ntree = 1000)

confusionMatrix(predict(rf_model), train_set$new_cause)

confusionMatrix(predict(rf_model, newdata = test_set), test_set$new_cause)

varImp(rf_model) %>% arrange(desc(Overall))

# prediction

predict_x$new_cause <- predict(rf_model, newdata = predict_x)
predict_x_grid$new_cause <- predict(rf_model, newdata = predict_x_grid)

write_csv(predict_x, "prediction_2019-2020.csv")
write_csv(predict_x_grid, "prediction_2019-2020_simulation.csv")

grid_prob <- predict(rf_model, newdata = predict_x_grid, type = "prob")

# plot the prediction

au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

ggplot(data = predict_x) +
  geom_sf(data = vic_map) +
  geom_point(aes(x = lon, y = lat, col = new_cause), alpha = 0.5) + 
  facet_wrap(~month)

ggplot(data = predict_x_grid) +
  geom_sf(data = vic_map) +
  geom_point(aes(lon, lat, col = new_cause), alpha = 0.5) +
  facet_wrap(~month)

ggplot(data = predict_x_grid) +
  geom_sf(data = vic_map) +
  geom_point(aes(lon, lat, col = grid_prob[,2]), alpha = 0.5) +
  facet_wrap(~month)

lat <- seq(34, 39, 0.2)
lat <- -lat
lon <- seq(141, 150, 0.2)

grids <- expand.grid(lat, lon)


rect <- function(x){
  
  # left top
  lat1 <- x[1]
  lon1 <- x[2]
  
  # right top
  lat2 <- x[1]
  lon2 <- x[2]+0.2
  
  # right bottom
  lat3 <- x[1]-0.2
  lon3 <- x[2]+0.2
  
  # left bottom
  lat4 <- x[1]-0.2
  lon4 <- x[2]
  
  st_sfc(st_polygon(list(matrix(c(lon1,lat1,lon2,lat2,lon3,lat3,lon4,lat4,lon1,lat1), ncol =2, byrow = TRUE))))
  
}

rect_list <- apply(grids[1:nrow(grids),],1,rect)

rect_list <- do.call(c, rect_list)

st_crs(rect_list) <- 4326

indexes <- st_intersects(vic_map$geometry, rect_list)[[1]]
rect_list <- rect_list[indexes]

ggplot(data = predict_x_grid) +
  geom_sf(data = vic_map) +
  geom_sf(data = rect_list, aes(geometry = geometry), fill = NA) +
  geom_point(aes(lon, lat, col = new_cause == "lightning")) +
  facet_wrap(~month)

grid_points <- st_as_sf(predict_x_grid, coords = c("lon", "lat"), crs = 4326) 

predict_x_grid$rect <- 0

indexes <- st_intersects(rect_list, grid_points)
for (i in 1:length(indexes)){
  predict_x_grid$rect[indexes[[i]]] <- i
}



rect_list <- as.data.frame(rect_list)
rect_list <- mutate(rect_list, rect = 1:nrow(rect_list))

temp <- predict_x_grid %>%
  group_by(rect, month, new_cause) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(new_cause, count)
  
temp[is.na(temp)] <- 0



rect_list <- temp %>%
  right_join(rect_list, by = c("rect"))


ggplot(data = predict_x_grid) +
  geom_sf(data = na.omit(rect_list), aes(geometry = geometry, fill = lightning/10)) +
  geom_sf(data = vic_map, fill = NA, color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  ggtitle("Likelihood of a bushfire caused by lightning") +
  facet_wrap(~month)

ggplot(data = predict_x_grid) +
  geom_sf(data = na.omit(rect_list), aes(geometry = geometry, fill = arson/10)) +
  geom_sf(data = vic_map, fill = NA, color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  ggtitle("Likelihood of a bushfire caused by arson") +
  facet_wrap(~month)

ggplot(data = predict_x_grid) +
  geom_sf(data = na.omit(rect_list), aes(geometry = geometry, fill = accident/10)) +
  geom_sf(data = vic_map, fill = NA, color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  ggtitle("Likelihood of a bushfire caused by accident") +
  facet_wrap(~month)

ggplot(data = predict_x_grid) +
  geom_sf(data = na.omit(rect_list), aes(geometry = geometry, fill = burning_off/10)) +
  geom_sf(data = vic_map, fill = NA, color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA) +
  ggtitle("Likelihood of a bushfire caused by burning_off") +
  facet_wrap(~month)
