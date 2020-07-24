# MNL

# Load library

library(tidyverse)
library(nnet)
library(caret)
library(lmtest)
library(glmnet)

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

#######################################################################################

# Select models without test set, drop variables using p-value

model <- multinom(new_cause~., data = training, maxit = 100, trace = FALSE)




pvalues <- c(1:(dim(training)[2]-1))
train_set <- training

while (sum(pvalues>0.05)>0){
  
  model <- multinom(new_cause~., data = train_set, maxit = 100, trace = FALSE)
  pvalues <- c()
  
  for (each_var in names(select(train_set, -new_cause))){
    temp <- lrtest(model, each_var)[[5]][2]
    pvalues <- c(pvalues, temp)
    print(paste(each_var, "--", temp))
  }
  
  if (sum(pvalues>0.05)>0){
    var_name <- names(select(train_set, -new_cause))[which.max(pvalues)]
    train_set <- select(train_set, -all_of(var_name))
    message(paste0(var_name, ' dropped.'))
    message(paste0(dim(train_set)[2], ' variables left.'))
  }
  
  
}

# final model FOR_CODE, FOR_CAT, COVER, rf, ase90, ase180, ase720, 
# mint, amint7, amint60, amint180, amint360, amint720, dist_camp, aws_m0, aws_m3, aws_m12, aws_m24,
# log_dist_cfa, log_dist_camp, log_dist_road

# 10-fold CV using these variables

training2 <- select(training,
                    FOR_CODE, FOR_CAT, COVER, rf, ase90, ase180, ase720,
                    mint, amint7, amint60, amint180, amint360, amint720, 
                    dist_camp, aws_m0, aws_m3, aws_m12, aws_m24, 
                    log_dist_cfa, log_dist_camp, log_dist_road,
                    new_cause)

folds <- createFolds(training2$new_cause, k = 10, list = FALSE)
train_con <- list()
val_con <- list()

for (i in 1:10){
  message(paste0('Training ', i, '-fold'))
  val_set <- training2[which(folds == i),]
  train_set <- training2[which(folds != i),]
  model <- multinom(new_cause~., data = train_set, maxit = 1000, trace = FALSE)
  train_con[[i]] <- confusionMatrix(predict(model, newdata = train_set), train_set$new_cause)
  val_con[[i]] <- confusionMatrix(predict(model, newdata = val_set), val_set$new_cause)
}

overall_acc <- 0

for (i in 1:10) overall_acc <- overall_acc + val_con[[i]]$overall[1]

overall_acc <- overall_acc / 10

overall_acc

# 0.5484351 

class_table <- val_con[[1]]$byClass

for (i in 2:10) class_table <- class_table + val_con[[i]]$byClass

class_table <- class_table / 10

class_table

# Sensitivity
# Class: acidental_human    0.70650621
# Class: arson              0.23037243
# Class: burning_off_human  0.12322479
# Class: lightning          0.83416051
# Class: other              0.03314349

# train model with these variables using all data

model <- multinom(new_cause~., data = training2, maxit = 1000, trace = FALSE)

confusionMatrix(predict(model), training2$new_cause)

# 0.5532

# Sensitivity
# Class: acidental_human    0.71041541 
# Class: arson              0.23802281
# Class: burning_off_human  0.12932331
# Class: lightning          0.83988006
# Class: other              0.03448276

########################################################################################

# select models without test set, add variables using AIC

var_set <- select(training, -new_cause) %>%
  names()

selected_var <- c()
best_aic <- 9999999


while (length(var_set)>0) {
  
  aic_vector <- c()
  
  for (i in 1:length(var_set)){
    selected_var <- c(selected_var, var_set[i])
    model <- multinom(as.formula(paste0("new_cause~", paste0(selected_var, collapse = '+'))),
                      data = training,
                      maxit = 1000,
                      trace = FALSE)
    print(paste0(var_set[i], ' -- ', model$AIC))
    aic_vector <- c(aic_vector, model$AIC)
    selected_var <- selected_var[-length(selected_var)]
  }
  
  if (best_aic > min(aic_vector)){
    best_aic <- min(aic_vector)
  } else {
    break
  }
  best_var <- var_set[which.min(aic_vector)]
  var_set <- var_set[-which.min(aic_vector)]
  message(paste0(best_var, ' added.'))
  message(paste0('best AIC: ', best_aic))
  selected_var <- c(selected_var, best_var)
  message(paste0("current variables: ", paste0(selected_var, collapse = ", "), '\n'))
}

# final model log_dist_road, log_dist_cfa, amint28, dist_camp, log_dist_camp, FOR_CODE, amint180
# ase7, aws_m0, ase720, lon, lat, aws_m24, aws_m3, ase180, ase90, amint90, ws, ase14, amint7
# aws_m12, aws_m6, amint60, arf14, aws_m1, ase360, mint, arf360, arf720, dist_road, rf, dist_cfa
# amaxt7, amaxt60, amaxt720, amaxt180, maxt, amaxt90

# 10-fold CV using these variables

training2 <- select(training,
                    log_dist_road, log_dist_cfa, amint28, dist_camp, log_dist_camp, FOR_CODE, amint180,
                    ase7, aws_m0, ase720, lon, lat, aws_m24, aws_m3, ase180, ase90, amint90, ws, ase14, amint7, 
                    aws_m12, aws_m6, amint60, arf14, aws_m1, ase360, mint, arf360, arf720, dist_road, rf, dist_cfa, 
                    amaxt7, amaxt60, amaxt720, amaxt180, maxt, amaxt90,
                    new_cause)

folds <- createFolds(training2$new_cause, k = 10, list = FALSE)
train_con <- list()
val_con <- list()

for (i in 1:10){
  message(paste0('Training ', i, '-fold'))
  val_set <- training2[which(folds == i),]
  train_set <- training2[which(folds != i),]
  model <- multinom(new_cause~., data = train_set, maxit = 1000, trace = FALSE)
  train_con[[i]] <- confusionMatrix(predict(model, newdata = train_set), train_set$new_cause)
  val_con[[i]] <- confusionMatrix(predict(model, newdata = val_set), val_set$new_cause)
}

overall_acc <- 0

for (i in 1:10) overall_acc <- overall_acc + val_con[[i]]$overall[1]

overall_acc <- overall_acc / 10

overall_acc

#0.5588972

class_table <- val_con[[1]]$byClass

for (i in 2:10) class_table <- class_table + val_con[[i]]$byClass

class_table <- class_table / 10

class_table

# Sensitivity
# Class: acidental_human    0.71493270
# Class: arson              0.27978834
# Class: burning_off_human  0.14737675
# Class: lightning          0.82608746
# Class: other              0.04906843

# train model with these variables using all data

model <- multinom(new_cause~.,
                  data = training2,
                  maxit = 1000,
                  trace = FALSE)

confusionMatrix(predict(model), training2$new_cause)

# 0.5723

# Sensitivity 
# Class: acidental_human    0.72456352
# Class: arson              0.30266160
# Class: burning_off_human  0.17142857
# Class: lightning          0.83628186
# Class: other              0.06498674

#######################################################################################


# 10-fold CV using all variables

folds <- createFolds(training$new_cause, k = 10, list = FALSE)
train_con <- list()
val_con <- list()

for (i in 1:10){
  message(paste0('Training ', i, '-fold'))
  val_set <- training[which(folds == i),]
  train_set <- training[which(folds != i),]
  model <- multinom(new_cause~., data = train_set, maxit = 1000, trace = FALSE)
  train_con[[i]] <- confusionMatrix(predict(model, newdata = train_set), train_set$new_cause)
  val_con[[i]] <- confusionMatrix(predict(model, newdata = val_set), val_set$new_cause)
}

overall_acc <- 0

for (i in 1:10) overall_acc <- overall_acc + val_con[[i]]$overall[1]

overall_acc <- overall_acc / 10

overall_acc

# 0.5603692

class_table <- val_con[[1]]$byClass

for (i in 2:10) class_table <- class_table + val_con[[i]]$byClass

class_table <- class_table / 10

class_table

# Sensitivity
# Class: acidental_human    0.71462788
# Class: arson              0.29127342
# Class: burning_off_human  0.15332429
# Class: lightning          0.82158566
# Class: other              0.05701987

# train this model using all data

model <- multinom(new_cause~.,
                  data = training,
                  maxit = 1000,
                  trace = FALSE)

confusionMatrix(predict(model), training$new_cause)

# 0.5728

# Sensitivity
# Class: acidental_human    0.72546659
# Class: arson              0.30722433
# Class: burning_off_human  0.17593985
# Class: lightning          0.83328336
# Class: other              0.06697613

# variable importance

varImp(model) %>% arrange(desc(Overall))

# FOR_CATNative forest 7.655643e+01
# FOR_CODE64           3.871376e+01
# FOR_CODE25           3.632636e+01
# COVER4               3.237144e+01
# HEIGHT4              3.237144e+01
# FOR_CODE37           2.382287e+01
# FOR_CODE2            2.372842e+01
# FOR_CODE102          2.339538e+01
# FOR_CATOther forest  2.339538e+01
# FOR_CODE49           2.197124e+01
# FOR_CODE68           1.984992e+01
# FOR_CODE50           1.958782e+01
# FOR_CODE16           1.899145e+01
# HEIGHT5              1.823866e+01
# COVER5               1.823866e+01
# FOR_CODE34           1.816434e+01
# FOR_CODE91           1.754805e+01
# FOR_CODE92           1.707168e+01
# FOR_CODE43           1.633972e+01
# FOR_CODE10           1.486355e+01
# FOR_CODE53           1.406865e+01
# FOR_CODE46           1.358433e+01
# aws_m24              1.304950e+01
# FOR_CODE52           1.195956e+01
# COVER2               1.193996e+01
# FOR_CODE40           1.113891e+01
# FOR_CODE93           1.097177e+01
# FOR_CODE101          1.096756e+01
# FOR_CODE44           1.095505e+01
# FOR_CODE65           1.008802e+01
# FOR_CODE82           9.899371e+00
# HEIGHT2              9.299290e+00
# FOR_CODE83           8.990552e+00
# FOR_CODE5            8.907309e+00
# FOR_CODE97           8.843795e+00
# FOR_CODE28           8.517192e+00
# FOR_CODE42           7.898983e+00
# COVER3               7.679561e+00
# FOR_CODE47           7.186296e+00
# FOR_CODE98           7.082054e+00
# HEIGHT3              6.943138e+00
# aws_m12              6.623033e+00
# FOR_CODE45           6.504399e+00
# FOR_CODE35           6.131926e+00
# FOR_CODE48           5.454450e+00
# aws_m6               5.204771e+00
# FOR_CODE41           4.313938e+00
# aws_m0               3.592680e+00
# FOR_CODE11           3.235744e+00
# aws_m3               3.092622e+00
# lat                  2.917191e+00
# ase180               2.247255e+00
# log_dist_cfa         2.182962e+00
# log_dist_camp        2.063327e+00
# amaxt180             1.800325e+00
# ase90                1.643831e+00
# arf720               1.504616e+00
# amint360             1.412464e+00
# ase360               1.407384e+00
# amaxt90              1.380267e+00
# aws_m1               1.246040e+00
# amint60              1.209877e+00
# amint180             1.207113e+00
# ase720               1.106180e+00
# arf360               1.033819e+00
# amint90              9.931894e-01
# ws                   9.301006e-01
# log_dist_road        9.242436e-01
# amint720             8.098435e-01
# arf180               7.218339e-01
# amint28              6.989804e-01
# amaxt360             6.700848e-01
# arf90                5.354476e-01
# amint14              4.615539e-01
# lon                  4.505106e-01
# amaxt60              4.411577e-01
# amaxt720             4.098277e-01
# ase14                3.386049e-01
# amaxt14              3.189254e-01
# ase28                3.112671e-01
# arf60                3.100446e-01
# amaxt7               2.653042e-01
# ase60                2.445730e-01
# amint7               2.348098e-01
# amaxt28              2.210807e-01
# ase7                 1.934132e-01
# arf14                1.594502e-01
# arf28                1.407640e-01
# arf7                 1.351105e-01
# maxt                 9.213844e-02
# mint                 4.969313e-02
# rf                   2.513629e-02
# se                   2.172540e-02
# dist_road            7.688253e-04
# dist_camp            9.959911e-05
# dist_cfa             8.135560e-05
# FOR_CODE84           0.000000e+00

##########################################################################################

# Lasso MNL

training2 <- model.matrix(new_cause~., data = training)

lasso_model <- cv.glmnet(x = training2,
          y = training$new_cause,
          family="multinomial", 
          type.multinomial = "grouped")


confusionMatrix(factor(predict(lasso_model, newx = training2, s = "lambda.min", type = "class")), 
                training$new_cause)

# 0.5697 

# Sensitivity
# Class: acidental_human    0.72366045
# Class: arson              0.30266160
# Class: burning_off_human  0.15789474
# Class: lightning          0.83628186
# Class: other              0.05570292
