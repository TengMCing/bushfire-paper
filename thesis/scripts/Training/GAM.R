# GAM

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

backup_FOR_TYPE_1 <- training$FOR_TYPE
backup_FOR_TYPE_2 <- as.numeric(training$FOR_TYPE)

training <- training %>%
  mutate(FOR_TYPE = factor(as.numeric(FOR_TYPE)))

training <- training %>%
  mutate(cause = as.numeric(cause)-1)

mnl_best <- read_rds("data/MNL_best_features.rds")
rf_best <- read_rds("data/RF_best_features.rds")

varslist <- full_join(mnl_best, rf_best, by = c("feature")) %>% .[['feature']]

training <- training %>%
  select(!!varslist, cause)


training <- fastDummies::dummy_cols(training, remove_selected_columns = TRUE)



inTraining <- createDataPartition(training$cause, p = .8, list = FALSE)[,1]
train_set <- training[inTraining,]
test_set  <- training[-inTraining,]

formula1 <- "cause ~ "
formula2 <- "~ "

for (i in names(select(training, -cause))){
  if (grepl("FOR_TYPE", i, fixed = TRUE) | grepl("HEIGHT", i, fixed = TRUE) | grepl("COVER", i, fixed = TRUE)){
    # formula1 <- paste0(formula1, i, " + ")
    # formula2 <- paste0(formula2, i, " + ")
  } else {
    formula1 <- paste0(formula1, "s(", i, ") + ")
    formula2 <- paste0(formula2, "s(", i, ") + ")
  }
}

formula1 <- substr(formula1, 1, nchar(formula1)-2)
formula2 <- substr(formula2, 1, nchar(formula2)-2)

library(mgcv)

gam_model <- gam(list(as.formula(formula1),
                      as.formula(formula2),
                      as.formula(formula2)),
    family = multinom(K = 3),
    data = train_set)

confusionMatrix(factor(apply(predict(gam_model, type = "response"), 1, function(x) which(max(x) == x))-1), factor(train_set$cause))

# ACC: 0.6966

confusionMatrix(factor(apply(predict(gam_model, newdata = test_set, type = "response"), 1, function(x) which(max(x) == x))-1), factor(test_set$cause))

# ACC: 0.6903

train_set <- select(train_set, ase90:cause)
test_set <- select(test_set, ase90:cause)

set.seed(123456)

lime_sample <- sample(1:length(test_set$cause), 100)

library(lime)

explainer <- lime(train_set, gam_model)

model_type.gam <- function(x, ...) 'classification'

predict_model.gam <- function(x, newdata = newdata, type, ...){
  res <- predict(x, newdata = newdata, type = "response")
  temp <- apply(res, 1, function(x) which(max(x) == x))-1
  switch(
    type,
    raw = data.frame(Response = temp, stringsAsFactors = FALSE),
    prob = data.frame(`0` = res[,1], `1` = res[,2], `2` = res[,3], `3` = res[,4])
  )

}

explaination <- explain(test_set[lime_sample, ],
                        explainer,
                        n_labels = 4,
                        n_features = 10)

explaination %>%
  group_by(feature) %>%
  count() %>%
  arrange(-n) %>%
  head(10) -> gam_best_features

gam_best_features

# Train 10 variables gam model

training2 <- select(training, !!gam_best_features$feature, cause)
train_set <- training2[inTraining,]
test_set  <- training2[-inTraining,]

formula1 <- "cause ~ s(ase180) + s(ase90) + s(aws_m12) + s(log_dist_camp) + s(amaxt180) + s(aws_m24) + s(log_dist_road) + s(amaxt90) + s(lon) + s(amaxt720)"
formula2 <- "~ s(ase180) + s(ase90) + s(aws_m12) + s(log_dist_camp) + s(amaxt180) + s(aws_m24) + s(log_dist_road) + s(amaxt90) + s(lon) + s(amaxt720)"

gam_model2 <- gam(list(as.formula(formula1),
                      as.formula(formula2),
                      as.formula(formula2)),
                 family = multinom(K = 3),
                 data = train_set)


confusionMatrix(factor(apply(predict(gam_model2, type = "response"), 1, function(x) which(max(x) == x))-1), factor(train_set$cause))

# ACC: 0.6675

confusionMatrix(factor(apply(predict(gam_model2, newdata = test_set, type = "response"), 1, function(x) which(max(x) == x))-1), factor(test_set$cause))

# ACC: 0.6779


# Reference
# Prediction    lightning accident arson burning_off
# lightning         663      114    64          45
# accident           74      434   106          33
# arson              31       72   144          30
# burning_off         9       14    11          28

saveRDS(gam_best_features, "data/GAM_best_features.rds")
saveRDS(gam_model2, "data/GAM_model.rds")












