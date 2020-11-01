library(tidyverse)
library(caret)
library(randomForest)
library(lubridate)

final_model <- readRDS("data/Final_model.rds")

# Read in simulation data
predict_x_grid <- read_csv("data/predict_x_grid.csv")

predict_x_grid <- mutate(predict_x_grid,
                    log_dist_cfa = log(dist_cfa),
                    log_dist_camp = log(dist_camp),
                    log_dist_road = log(dist_road))

predict_x_grid$cause <- predict(final_model, newdata = predict_x_grid)

write_csv(predict_x_grid, "data/prediction_2019-2020_simulation.csv")
