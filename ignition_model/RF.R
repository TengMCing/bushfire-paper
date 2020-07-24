# RF

library(tidyverse)
library(caret)
library(randomForest)

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

# train randomforest model

inTraining <- createDataPartition(training$new_cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

fitControl <- trainControl(method = 'cv',
                           number = 10)

rf_model <- train(new_cause~., data = train_set,
                  method = 'rf',
                  trControl = fitControl,
                  ntree = 1000)

confusionMatrix(predict(rf_model), train_set$new_cause)

# Accuracy : 0.9994

confusionMatrix(predict(rf_model, newdata = test_set), test_set$new_cause)

# Accuracy : 0.6479 

# Sensitivity
# Class: acidental_human     0.8147590
# Class: arson               0.4106464
# Class: burning_off_human   0.2330827
# Class: lightning           0.8800600
# Class: other               0.1627907

varImp(rf_model) %>% arrange(desc(Overall))


# lon                  1.000000e+02
# dist_road            7.364299e+01
# log_dist_road        7.119689e+01
# dist_camp            6.257879e+01
# log_dist_camp        6.105311e+01
# amaxt720             4.776163e+01
# amint720             4.381889e+01
# dist_cfa             4.223859e+01
# lat                  4.222100e+01
# log_dist_cfa         4.138724e+01
# ws                   3.715494e+01
# aws_m0               3.446531e+01
# ase180               3.332881e+01
# amaxt360             3.290298e+01
# ase720               3.156887e+01
# aws_m1               3.059923e+01
# amint14              3.057492e+01
# aws_m24              2.949868e+01
# ase60                2.913722e+01
# se                   2.880089e+01
# ase360               2.735772e+01
# amint28              2.711499e+01
# arf60                2.673019e+01
# mint                 2.664710e+01
# ase7                 2.608265e+01
# arf720               2.583135e+01
# amaxt14              2.535475e+01
# arf28                2.532424e+01
# maxt                 2.461361e+01
# amint360             2.448375e+01
# amint7               2.415944e+01
# aws_m12              2.352685e+01
# ase28                2.341466e+01
# aws_m3               2.331534e+01
# arf7                 2.326601e+01
# arf14                2.294975e+01
# arf360               2.288393e+01
# aws_m6               2.287021e+01
# arf90                2.256326e+01
# amint60              2.197662e+01
# arf180               2.163695e+01
# ase14                2.124746e+01
# amaxt180             2.118403e+01
# amint180             2.115383e+01
# amaxt7               2.101576e+01
# amaxt28              2.039774e+01
# ase90                2.006219e+01
# amint90              1.738003e+01
# amaxt60              1.671149e+01
# amaxt90              1.523954e+01
# rf                   1.160120e+01
# FOR_CODE34           9.918115e+00
# FOR_CODE41           4.766326e+00
# FOR_CODE44           3.661607e+00
# COVER2               3.401834e+00
# HEIGHT2              2.997240e+00
# HEIGHT3              2.703476e+00
# FOR_CODE45           2.660190e+00
# FOR_CATNative forest 1.839426e+00
# FOR_CODE47           1.331710e+00
# FOR_CODE48           1.011984e+00
# COVER3               9.938716e-01
# FOR_CODE42           8.223977e-01
# FOR_CODE11           8.142837e-01
# COVER4               7.609352e-01
# FOR_CODE92           7.408786e-01
# HEIGHT4              7.136064e-01
# FOR_CODE82           6.619537e-01
# FOR_CODE43           6.362460e-01
# FOR_CODE91           5.845815e-01
# COVER5               5.482003e-01
# FOR_CODE40           5.358286e-01
# HEIGHT5              5.352837e-01
# FOR_CODE102          4.975818e-01
# FOR_CATOther forest  4.835361e-01
# FOR_CODE28           4.367768e-01
# FOR_CODE98           3.751303e-01
# FOR_CODE83           3.553478e-01
# FOR_CODE5            1.764637e-01
# FOR_CODE16           1.455607e-01
# FOR_CODE35           1.433687e-01
# FOR_CODE10           1.146561e-01
# FOR_CODE2            1.116743e-01
# FOR_CODE53           9.051037e-02
# FOR_CODE52           8.867573e-02
# FOR_CODE101          8.327082e-02
# FOR_CODE68           7.338153e-02
# FOR_CODE46           6.600507e-02
# FOR_CODE97           6.047938e-02
# FOR_CODE65           3.281765e-02
# FOR_CODE50           2.923390e-02
# FOR_CODE64           2.411870e-02
# FOR_CODE49           2.225512e-02
# FOR_CODE37           1.860758e-02
# FOR_CODE25           1.223643e-02
# FOR_CODE93           1.662613e-03
# FOR_CODE84           0.000000e+00

# Train model using all training data

rf_model <- randomForest(x = select(train_set, -new_cause), 
                         y = train_set$new_cause,
                         ntree = 1000)

confusionMatrix(predict(rf_model, newdata = test_set), test_set$new_cause)

# Accuracy : 0.6519

# Sensitivity
# Class: acidental_human     0.8313253
# Class: arson               0.4068441
# Class: burning_off_human   0.1804511
# Class: lightning           0.8875562
# Class: other               0.1495017

# Train model using all data

rf_model <- randomForest(x = select(training, -new_cause), 
                         y = training$new_cause,
                         ntree = 1000)

confusionMatrix(predict(rf_model), 
                training$new_cause)

# Accuracy : 0.6437 

# Sensitivity
# Class: acidental_human     0.8130644
# Class: arson               0.4197719
# Class: burning_off_human   0.2180451
# Class: lightning           0.8830585
# Class: other               0.1240053