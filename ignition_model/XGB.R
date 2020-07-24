# Xgboost


library(tidyverse)
library(caret)
library(xgboost)

set.seed(10086)

# Read in data
training <- read_csv("training.csv")
training <- select(training, -c(EVENTID:CAUSE), -id, -FOREST, -FOR_TYPE)

training <- mutate(training, 
                   new_cause = ifelse(new_cause %in% c("relight"), 
                                      "other", 
                                      new_cause))

training <- mutate(training, 
                   log_dist_cfa = log(dist_cfa),
                   log_dist_camp = log(dist_camp),
                   log_dist_road = log(dist_road),
                   COVER = factor(COVER),
                   HEIGHT = factor(HEIGHT),
                   FOR_CODE = factor(FOR_CODE),
                   new_cause = factor(new_cause),
                   FOR_CAT = factor(FOR_CAT))




training <- na.omit(training)

training <- mutate(training, new_cause = as.numeric(new_cause)-1)

# train model

inTraining <- createDataPartition(training$new_cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

training2 <- model.matrix(new_cause~., data = train_set)


model <- xgb.cv(data = training2, 
        label = train_set$new_cause, 
        nrounds = 300,
        early_stopping_rounds = 20,
        params = list(objective = "multi:softmax",
                      lamdba = 0.0003, 
                      alpha = 0.0003,
                      num_class = 5,
                      eval_metric = "mlogloss"),
        metrics = "merror",
        nfold = 10,
        prediction = TRUE)

# 66 iters

model <- xgboost(data = training2, 
                   label = train_set$new_cause, 
                   nrounds = 66,
                   params = list(objective = "multi:softmax",
                                 lamdba = 0.0003, 
                                 alpha = 0.0003,
                                 num_class = 5,
                                 eval_metric = "mlogloss"))

confusionMatrix(factor(predict(model, newdata = model.matrix(new_cause~., data = test_set))), 
                factor(test_set$new_cause))

# Accuracy : 0.6312

# Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
# Sensitivity 0.7858  0.43320  0.22059   0.8456  0.15615

# Use caret to search for optimal parameters

training <- read_csv("training.csv")
training <- select(training, -c(EVENTID:CAUSE), -id, -FOREST, -FOR_TYPE)

training <- mutate(training, 
                   new_cause = ifelse(new_cause %in% c("relight"), 
                                      "other", 
                                      new_cause))

training <- mutate(training, 
                   log_dist_cfa = log(dist_cfa),
                   log_dist_camp = log(dist_camp),
                   log_dist_road = log(dist_road),
                   COVER = factor(COVER),
                   HEIGHT = factor(HEIGHT),
                   FOR_CODE = factor(FOR_CODE),
                   new_cause = factor(new_cause),
                   FOR_CAT = factor(FOR_CAT))




training <- na.omit(training)

inTraining <- createDataPartition(training$new_cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

tune_grid <- expand.grid(max_depth = c(4,6,8), 
                         nrounds = c(50, 100, 200, 250, 300),
                         eta = 0.15,
                         subsample = c(0.6, 0.8),
                         gamma = 1,
                         colsample_bytree = 1,
                         min_child_weight = 1)



fitControl <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

model <- train(new_cause ~ ., 
               data = train_set, 
               method = "xgbTree", 
               trControl = fitControl,
               verbose = TRUE,
               tuneGrid = tune_grid)

confusionMatrix(predict(model, newdata = test_set), test_set$new_cause)

# 0.6415 

# Class: acidental_human Class: arson Class: burning_off_human Class: lightning Class: other
# Sensitivity 0.7922      0.44867                  0.24812           0.8606      0.16611


# best: 250 nrounds, 6 max_depth, 0.15 eta, 0.8 subsample 

# train all data

tune_grid <- expand.grid(max_depth = c(6), 
                         nrounds = c(250),
                         eta = 0.15,
                         subsample = c(0.8),
                         gamma = 1,
                         colsample_bytree = 1,
                         min_child_weight = 1)

fitControl <- trainControl(method = "cv", number = 10)


model <- train(new_cause ~ ., 
               data = training, 
               method = "xgbTree", 
               trControl = fitControl,
               verbose = TRUE,
               tuneGrid = tune_grid)

model$resample

# Accuracy 0.649187

varImp(model)$importance %>% arrange(desc(Overall))

# lon                  1.000000e+02
# dist_camp            9.879970e+01
# dist_road            9.724888e+01
# dist_cfa             7.595989e+01
# lat                  4.257817e+01
# amint720             3.714147e+01
# ase180               3.494849e+01
# ws                   3.363603e+01
# aws_m1               2.985454e+01
# aws_m0               2.773958e+01
# aws_m24              2.692893e+01
# ase720               2.659020e+01
# ase360               2.592527e+01
# amint14              2.561738e+01
# amaxt720             2.534746e+01
# se                   2.403347e+01
# amaxt360             2.367271e+01
# aws_m12              2.275324e+01
# ase7                 2.208457e+01
# arf60                2.203987e+01
# amaxt14              2.078057e+01
# mint                 2.055585e+01
# amint28              2.038698e+01
# aws_m3               2.038285e+01
# ase14                2.034814e+01
# maxt                 1.983803e+01
# ase28                1.962508e+01
# amint7               1.962501e+01
# ase90                1.958100e+01
# arf28                1.944167e+01
# arf720               1.879123e+01
# arf90                1.856373e+01
# arf360               1.838555e+01
# arf14                1.830618e+01
# ase60                1.818725e+01
# amint360             1.735118e+01
# amint180             1.724407e+01
# amaxt28              1.720145e+01
# amaxt7               1.715891e+01
# arf7                 1.714120e+01
# amint60              1.678515e+01
# aws_m6               1.637842e+01
# arf180               1.611885e+01
# amaxt180             1.609676e+01
# amaxt90              1.269774e+01
# amaxt60              1.262797e+01
# amint90              1.241303e+01
# rf                   6.802084e+00
# FOR_CODE34           2.685525e+00
# HEIGHT3              2.655881e+00
# FOR_CODE41           2.498824e+00
# FOR_CODE45           2.367211e+00
# FOR_CATNative forest 1.771890e+00
# FOR_CODE44           1.644204e+00
# COVER2               1.552674e+00
# HEIGHT2              1.503735e+00
# FOR_CODE28           8.038244e-01
# FOR_CODE47           8.033100e-01
# COVER4               4.594617e-01
# COVER5               4.327842e-01
# FOR_CODE5            4.132454e-01
# COVER3               4.010501e-01
# FOR_CODE102          3.960086e-01
# FOR_CODE91           3.652447e-01
# FOR_CODE92           2.491134e-01
# FOR_CODE83           1.975870e-01
# FOR_CODE11           1.623836e-01
# FOR_CODE82           1.534989e-01
# FOR_CODE42           1.486892e-01
# FOR_CODE48           1.407872e-01
# FOR_CODE46           7.346647e-02
# FOR_CODE16           6.977653e-02
# FOR_CODE2            6.951692e-02
# FOR_CODE101          6.031139e-02
# FOR_CODE53           3.982798e-02
# FOR_CODE40           2.275683e-02
# FOR_CODE10           8.298872e-03
# FOR_CODE25           0.000000e+00
# FOR_CODE35           0.000000e+00
# FOR_CODE37           0.000000e+00
# FOR_CODE43           0.000000e+00
# FOR_CODE49           0.000000e+00
# FOR_CODE50           0.000000e+00
# FOR_CODE52           0.000000e+00
# FOR_CODE64           0.000000e+00
# FOR_CODE65           0.000000e+00
# FOR_CODE68           0.000000e+00
# FOR_CODE84           0.000000e+00
# FOR_CODE93           0.000000e+00
# FOR_CODE97           0.000000e+00
# FOR_CODE98           0.000000e+00
# FOR_CATOther forest  0.000000e+00
# HEIGHT4              0.000000e+00
# HEIGHT5              0.000000e+00
# log_dist_cfa         0.000000e+00
# log_dist_camp        0.000000e+00
# log_dist_road        0.000000e+00
