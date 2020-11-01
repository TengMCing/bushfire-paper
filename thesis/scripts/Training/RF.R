# RF

library(tidyverse)
library(caret)
library(randomForest)
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


# train randomforest model (find best 'mtry' using 3-fold CV)

inTraining <- createDataPartition(training$cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

rfGrid <-  expand.grid(mtry = seq(3, 21, 3))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

rf_model <- train(cause~.,
                  data = train_set,
                  method = "rf",
                  tuneGrid = rfGrid,
                  trControl = fitControl,
                  ntree = 1000)

plot(rf_model)

# mtry = 9 is the best

confusionMatrix(predict(rf_model), train_set$cause)

# ACC: 0.9999

confusionMatrix(predict(rf_model, newdata = test_set), test_set$cause)

# ACC: 0.7564

set.seed(123456)

lime_sample <- sample(1:length(test_set$cause), 100)

library(lime)

explainer <- lime(train_set, rf_model)

explaination <- explain(test_set[lime_sample, ],
                        explainer,
                        n_labels = 4,
                        n_features = 10)

explaination %>%
  group_by(feature) %>%
  count() %>%
  arrange(-n) %>%
  head(10) -> rf_best_features

rf_best_features

# Train 10 variables rf model

training2 <- select(training, !!rf_best_features$feature, cause)
train_set <- training2[inTraining,]
test_set  <- training2[-inTraining,]

rfGrid <-  expand.grid(mtry = seq(1, 10, 1))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

rf_model <- train(cause~.,
                  data = train_set,
                  method = "rf",
                  tuneGrid = rfGrid,
                  trControl = fitControl,
                  ntree = 1000)

plot(rf_model)

# mtry = 1 is the best

confusionMatrix(predict(rf_model), train_set$cause)

# ACC: 0.9999

confusionMatrix(predict(rf_model, newdata = test_set), test_set$cause)

# ACC: 0.7495

# Reference
# Prediction    lightning accident arson burning_off
# lightning         703       77    51          44
# accident           51      494    89          38
# arson              18       55   174          22
# burning_off         5        8    11          32

saveRDS(rf_best_features, "data/RF_best_features.rds")
saveRDS(rf_model, "data/RF_model.rds")
