library(tidyverse)
library(caret)
library(randomForest)
library(lubridate)
library(rnaturalearth)
library(ggthemes)

final_model <- readRDS("data/Final_model.rds")

# Read in predict data
predict_x <- read_csv("data/predict_x.csv")

predict_x <- mutate(predict_x,
                    log_dist_cfa = log(dist_cfa),
                    log_dist_camp = log(dist_camp),
                    log_dist_road = log(dist_road))

predict_x$cause <- predict(final_model, newdata = predict_x)

write_csv(predict_x, "data/prediction_2019-2020.csv")

predict_x$month <- factor(month(predict_x$time), levels = c(10, 11, 12, 1, 2, 3))

au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

p <- ggplot() +
  geom_sf(data = vic_map) +
  geom_point(data = predict_x, aes(lon, lat, col = cause), alpha = 0.8) +
  facet_wrap(~month)

ggsave(paste0("figures/", "2019-2020_prediction", ".jpeg"), plot = p, width = 14, height = 7)
