# MNL

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

# train MNL model (find best 'decay' using 3-fold CV)

inTraining <- createDataPartition(training$cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

mnlGrid <-  expand.grid(decay = seq(0, 2, 0.2))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

mnl_model <- train(cause~.,
                  data = train_set,
                  method = "multinom",
                  tuneGrid = mnlGrid,
                  trControl = fitControl,
                  maxit = 1000)

plot(mnl_model)

# decay = 0 is the best

confusionMatrix(predict(mnl_model), train_set$cause)

# ACC: 0.6721

confusionMatrix(predict(mnl_model, newdata = test_set), test_set$cause)

# ACC: 0.6629

set.seed(123456)

lime_sample <- sample(1:length(test_set$cause), 100)

library(lime)

explainer <- lime(train_set, mnl_model)

explaination <- explain(test_set[lime_sample, ],
                        explainer,
                        n_labels = 4,
                        n_features = 10)

explaination %>%
  group_by(feature) %>%
  count() %>%
  arrange(-n) %>%
  head(10) -> mnl_best_features

mnl_best_features

# Train 10 variables mnl model

training2 <- select(training, !!mnl_best_features$feature, cause)
train_set <- training2[inTraining,]
test_set  <- training2[-inTraining,]

mnlGrid <-  expand.grid(decay = seq(0, 2, 0.2))

fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

mnl_model <- train(cause~.,
                   data = train_set,
                   method = "multinom",
                   tuneGrid = mnlGrid,
                   trControl = fitControl,
                   maxit = 1000)

plot(mnl_model)

# decay = 0.2 is the best

confusionMatrix(predict(mnl_model), train_set$cause)

# ACC: 0.5342

confusionMatrix(predict(mnl_model, newdata = test_set), test_set$cause)

# ACC: 0.5272

# Reference
# Prediction    lightning accident arson burning_off
# lightning         568      259    74          33
# accident          182      277   120          51
# arson              22       77   120          30
# burning_off         5       21    11          22

saveRDS(mnl_best_features, "data/MNL_best_features.rds")
saveRDS(mnl_model, "data/MNL_model.rds")
