library(pROC)
library(tidyverse)
library(caret)
library(randomForest)
library(lubridate)

rf_model <- readRDS("data/RF_model.rds")
mnl_model <- readRDS("data/MNL_model.rds")
gam_model <- readRDS("data/GAM_model.rds")
xgb_model <- readRDS("data/XGB_model.rds")

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

inTraining <- createDataPartition(training$cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]


pred_mat <- predict(rf_model, newdata = test_set, type = "prob") %>% as.matrix()

rf_roc <- multiclass.roc(test_set$cause, pred_mat)

pred_mat <- predict(mnl_model, newdata = test_set, type = "prob") %>% as.matrix()

mnl_roc <- multiclass.roc(test_set$cause, pred_mat)

pred_mat <- predict(gam_model, newdata = test_set, type = "response")

colnames(pred_mat) <- c("lightning", "accident", "arson", "burning_off")

gam_roc <- multiclass.roc(test_set$cause, pred_mat)

pred_mat <- predict(xgb_model, newdata = test_set, type = "prob") %>% as.matrix()

xgb_roc <- multiclass.roc(test_set$cause, pred_mat)

rf_roc$auc
mnl_roc$auc
gam_roc$auc
xgb_roc$auc

# Random Forest is the best one

# Fit rf model on full training data

rf_best_features <- read_rds("data/RF_best_features.rds")

training2 <- select(training, !!rf_best_features$feature, cause)

rfGrid <-  expand.grid(mtry = 1)

fitControl <- trainControl(method = "none",
                           verboseIter = TRUE)

final_model <- train(cause~.,
                  data = training2,
                  method = "rf",
                  tuneGrid = rfGrid,
                  trControl = fitControl,
                  ntree = 1000)

saveRDS(final_model, "data/Final_model.rds")
