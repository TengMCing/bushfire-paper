# XGB

library(tidyverse)
library(caret)
library(lubridate)

set.seed(10086)

# Read in training data
training <- read_csv("data/training.csv")

training <- training %>%
  filter(!CAUSE %in% c("BURNING BUILDING",
                       "WASTE DISPOSAL, INDUSTRIAL, SAWMILL, TIP",
                       "WASTE DISPOSAL, DOMESTIC",
                       "BURNING VEHICLE, MACHINE",
                       "BURNING BUILDING")) %>%
  filter(new_cause != "other") %>%
  filter(new_cause != "relight")


training <- select(training, -c(EVENTID:FIRE_NUM), -id, -CAUSE, -FOREST, -FOR_CODE, -FOR_CAT)

training <- mutate(training,
                   year = factor(year(FIRE_START)),
                   month = factor(month(FIRE_START), levels = c(10,11,12,1,2,3)),
                   day = factor(day(FIRE_START), levels = c(1:31)),
                   wod = factor(wday(FIRE_START), levels = c(1:7)))

training <- filter(training, month %in% c(10,11,12,1,2,3))

training <- na.omit(training)

training <- mutate(training, new_cause = ifelse(new_cause == "accidental_human", "accident", new_cause)) %>%
  mutate(new_cause = ifelse(new_cause == "burning_off_human", "burning_off", new_cause)) %>%
  mutate(new_cause = factor(new_cause)) %>%
  mutate(FOR_TYPE = factor(FOR_TYPE))

training <- na.omit(training)

training <- mutate(training,
                   log_dist_cfa = log(dist_cfa),
                   log_dist_camp = log(dist_camp),
                   log_dist_road = log(dist_road),
                   COVER = factor(COVER),
                   HEIGHT = factor(HEIGHT))

training <- rename(training, cause = new_cause)
training <- mutate(training,
                   cause = fct_relevel(cause,
                                       "lightning",
                                       "accident",
                                       "arson",
                                       "burning_off"))

training <- na.omit(training)

training <- select(training, -year, -dist_road, -dist_cfa, -dist_camp, -FIRE_START)


# train xgb model (find best 'max_depth' and 'min_child_weight' using 3-fold CV)

inTraining <- createDataPartition(training$cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

xgbGrid <-  expand.grid(max_depth = seq(3, 9, 2),
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.8,
                        gamma = 0,
                        colsample_bytree = 0.8,
                        min_child_weight = seq(1, 7, 2))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                  data = train_set,
                  method = "xgbTree",
                  tuneGrid = xgbGrid,
                  trControl = fitControl,
                  verbose = TRUE)

plot(xgb_model)

# max_depth = 7 and min_child_weight = 3 is the best
# train model around these two values

xgbGrid <-  expand.grid(max_depth = c(6, 7, 8),
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.8,
                        gamma = 0,
                        colsample_bytree = 0.8,
                        min_child_weight = c(2, 3, 4))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# max_depth = 6 and min_child_weight = 2 is the best
# tune gamma

xgbGrid <-  expand.grid(max_depth = 6,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.8,
                        gamma = seq(0, 1, 0.2),
                        colsample_bytree = 0.8,
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# gamma = 0 is the best
# tune subsample and colsample_bytree

xgbGrid <-  expand.grid(max_depth = 6,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = seq(0.5, 0.9, 0.1),
                        gamma = 0,
                        colsample_bytree = seq(0.5, 0.9, 0.1),
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# subsample = 0.9, colsample_bytree = 0.6 is the best
# tune around these values

xgbGrid <-  expand.grid(max_depth = 6,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = c(0.85, 0.9, 0.95),
                        gamma = 0,
                        colsample_bytree = c(0.55, 0.6, 0.65),
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# subsample = 0.85 and colsample_bytree = 0.65

xgbGrid <-  expand.grid(max_depth = 6,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.85,
                        gamma = 0,
                        colsample_bytree = 0.65,
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# reduce learning rate

xgbGrid <-  expand.grid(max_depth = 6,
                        nrounds = seq(50, 10000, 50),
                        eta = c(0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.00625),
                        subsample = 0.85,
                        gamma = 0,
                        colsample_bytree = 0.65,
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# eta = 0.025, nrounds = 2800 is the best

confusionMatrix(predict(xgb_model), train_set$cause)

# ACC: 0.9999

confusionMatrix(predict(xgb_model, newdata = test_set), test_set$cause)

# ACC: 0.7719

set.seed(123456)

lime_sample <- sample(1:length(test_set$cause), 100)

library(lime)

explainer <- lime(train_set, xgb_model)

explaination <- explain(test_set[lime_sample, ],
                        explainer,
                        n_labels = 4,
                        n_features = 10)

explaination %>%
  group_by(feature) %>%
  count() %>%
  arrange(-n) %>%
  head(10) -> xgb_best_features

xgb_best_features

# Train 10 variables xgb model

training2 <- select(training, !!xgb_best_features$feature, cause)
train_set <- training2[inTraining,]
test_set  <- training2[-inTraining,]

# tune 'max_depth' and 'min_child_weight'

xgbGrid <-  expand.grid(max_depth = seq(3, 9, 2),
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.8,
                        gamma = 0,
                        colsample_bytree = 0.8,
                        min_child_weight = seq(1, 7, 2))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# max_depth = 5 and min_child_weight = 1 is the best
# tune around values

xgbGrid <-  expand.grid(max_depth = c(4, 5, 6),
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.8,
                        gamma = 0,
                        colsample_bytree = 0.8,
                        min_child_weight = c(1, 2))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# max_depth = 5 and min_child_weight = 2 is the best
# tune gamma

xgbGrid <-  expand.grid(max_depth = 5,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = 0.8,
                        gamma = seq(0, 1, 0.2),
                        colsample_bytree = 0.8,
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# gamma = 0.8 is the best
# tune subsample and colsample_bytree

xgbGrid <-  expand.grid(max_depth = 5,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = seq(0.5, 0.9, 0.1),
                        gamma = 0.8,
                        colsample_bytree = seq(0.5, 0.9, 0.1),
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# subsample = 0.9 and colsample_bytree = 0.6 is the best
# tune values around

xgbGrid <-  expand.grid(max_depth = 5,
                        nrounds = seq(50, 1000, 50),
                        eta = 0.3,
                        subsample = c(0.85, 0.9, 0.95),
                        gamma = 0.8,
                        colsample_bytree = c(0.55, 0.6, 0.65),
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# subsample = 0.85 and colsample_bytree = 0.55 is the best
# reduce learning rate

xgbGrid <-  expand.grid(max_depth = 5,
                        nrounds = seq(50, 10000, 50),
                        eta = c(0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.00625),
                        subsample = 0.85,
                        gamma = 0.8,
                        colsample_bytree = 0.55,
                        min_child_weight = 2)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

xgb_model <- train(cause~.,
                   data = train_set,
                   method = "xgbTree",
                   tuneGrid = xgbGrid,
                   trControl = fitControl,
                   verbose = TRUE)

plot(xgb_model)

# eta = 0.025 and nrounds = 4800 is the best

confusionMatrix(predict(xgb_model), train_set$cause)

# ACC: 0.9933

confusionMatrix(predict(xgb_model, newdata = test_set), test_set$cause)

# ACC: 0.7388

# Reference
# Prediction    lightning accident arson burning_off
# lightning         695       87    42          36
# accident           53      465    85          38
# arson              22       72   183          22
# burning_off         7       10    15          40

saveRDS(xgb_best_features, "data/XGB_best_features.rds")
saveRDS(xgb_model, "data/XGB_model.rds")
