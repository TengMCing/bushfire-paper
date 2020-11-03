# Using Remote Sensing Data to Understand Fire Ignitions in Victoria During the 2019-2020 Australian Bushfire Season

**A thesis submitted for the degree of Bachelor of Commerce (Honours) by Weihao Li**

## Abstract

## Table of Content

1. Introduction
	1. 2019-2020 Australia bushfires
	2. Remote sensing data
	3. Research objectives and contribution
	4. Scope of this thesis
2. Review of literature
 	1. Spatio-temporal clustering
 	2. Bushfire modelling
3. Data
 	1. Data sources
 	2. Data processing for historical bushfire ignitions
 	3. Compiled data
 	4. Exploratory data analysis of historical bushfire ignitions
4. Detecting bushfire ignitions from hotspots data
 	1. Overview of the hotspot data
 	2. Outline of the algorithm
 	3. Results
 	4. Data integration for ignition points in 2019-2020
5. Classification of ignition causes
 	1. Model description
 	2. Feature selection
 	3. Hyperparameter tuning and candidate model selection
 	4. Results
 	5. Predicting ignition cause for 2019-2020 season
6. Discussion
 	1. Policy implications
 	2. Limitation
 	3. Future work
7. Conclusion

# Instructions

Before you try to reproduce this research, we need to inform you of several tips: 

- Please use `Rstudio` to open the R project `thesis.Rproj` to preserve the relative paths. 

- Code for reproduciblity are provided in `index.Rmd`. Minor changes need to be made to enable reevaluating the code.

- Please check the [Dependencies](#Dependencies) and keep your environment as similar as possible.

- Please run the `index.Rmd` chunk by chunk. Most chunks depend on the results from one or more previous chunks.

- It may takes more than 30 hours to reproduce this entire research with a mid-range laptop. We will notify the approximate time needs for each chunk in `index.Rmd`.

## Data Collection

Check the `data` folder and download all the data as required. The detail about how to download the data is in the `data` folder. 

## `index.Rmd`

All the scripts required for reproducibility have been linked by the `index.Rmd`. Subsitial changes need to be made to activate the reevaluation functions. You need to set the condition to `TRUE` for each `if` statement in `index.Rmd` to prevent R skipping it. Then you can run the script chunk by chunk.

### 1. Code chunk: Prepare-Hotspots-For-Clustering

**Runtime: ~5 minutes**

**Input: hotspot data**

**Output: `VIC_hotspots_before_clustering.csv`, `VIC_hotspots_raw.csv`**

This chunk preprocesses the raw hotspot data by calling `Clustering-Python-setup.R`. The outcomes include two csv files `VIC_hotspots_before_clustering.csv` and `VIC_hotspots_raw.csv`. `VIC_hotspots_before_clustering.csv` only contains observed time and location of hostpots, while `VIC_hotspots_raw.csv` contains full information.

### 2. Code chunk: Clustering-Parameters-Tuning

**Runtime: ~8-10 hours**

**Input: `VIC_hotspots_before_clustering.csv`**

**Output: `clustering_grid.csv`**

This chunk runs the clustering algorithm using a grid of parameter values by calling `clustering_tune.py`. It provides the results of different attempts in `clustering_grid.csv`.

### 3. Code chunk: Choose-Optimal-Parameters-For-Clustering

**Runtime: ~20 seconds**

**Input: `clustering_grid.csv`**

**Output: `setting.txt, clustering_tuning_1.jpeg, clustering_tuning_2.jpeg`**

This chunk chooses the optimal parameters for the clustering algorithm by calling `clustering_tune_vis.R`. The result is saved in `setting.txt`.

### 4. Code chunk: Hotspots-Clustering

**Runtime: ~6 minutes**

**Input: `VIC_hotspots_before_clustering.csv`**

**Output: `VIC_hotspots_after_clustering.csv, summary.txt`**

This chunk runs the clustering algorithm on the optimal parameters by calling `main.py`.  The outcomes are the final clustering result `VIC_hotspots_after_clustering.csv` and a brief summary of the result `summary.txt`. 

### 5. Code chunk: Bushfires-Data-Integration-Hotspots

**Runtime: ~20 minutes**

**Input: `VIC_hotspots_raw.csv`, `VIC_hotspots_after_clustering.csv`, forest data, weather data, camping site data, fire stations data, road map**

**Output: `predict_x.csv`**

This chunk performs data integration on the clustering result by calling `combine_current.R`. The outome is a dataset contains fire ignitions along with 54 predictors provided in `predict_x.csv`.

### 6. Code chunk: Bushfires-Data-Integration-Simulation

**Runtime: ~3 hours**

**Input: forest data, weather data, camping site data, fire stations data, road map**

**Output: `predict_x_grid.csv`**

This chunks performs data integration on the uniformly simulated fire ignitions by calling `combine_grid.R`.  The outcome is a dataset contains simulated fire ignitions with 54 predictors provided in `predict_x_grid.csv`.

### 7. Code chunk: Bushfires-Data-Integration-History

**Runtime: ~3 hours**

**Input: fire origins, forest data, weather data, camping site, CFA stations, road map**

**Output: `training.csv`**

This chunks performs data integration on historical fire ignitions by calling `combine_his.R`. The outcome is a the training data contains the cause of the ignition and 54 predictors provided in `training.csv`.

### 8. Code chunk: EDA

**Runtime: ~3 minutes**

**Input: `training.csv`, `VIC_hotspots_raw.csv`, `VIC_hotspots_after_clustering.csv`**

**Output: multiple plots**

This chunks produces plots for exploratory data analysis of the training data.

### 9. Code chunks: Random-Forest-Model, MNL-Model, GAM-Model and XGB-Model

**Runtime: ~1 hour, 1 hour, 6 hours, 6 hours**

**Input: `training.csv`**

**Output: RF_best_features.rds, RF_model.rds, MNL_best_features.rds, MNL_model.rds, GAM_best_features.rds, GAM_model.rds, XGB_best_features.rds, XGB_model.rds**

These 4 code chunks fit classification models on the training data and find the most important features by calling `RF.R`, `MNL.R`, `GAM.R` and `XGB.R`. The final models are stored in `RF_model.rds`, `MNL_model.rds`, `GAM_model.rds` and `XGB_model.rds`. Best features are stored in `RF_best_features.rds`, `MNL_best_features.rds`, `GAM_best_features.rds` and `XGB_best_features.rds`.

### 10. Code chunk: Final-Model

**Runtime: ~2 minutes**

**Input: `RF_model.rds`, `MNL_model.rds`, `GAM_model.rds`, `XGB_model.rds`, `training.csv`, `RF_best_features.rds`**

**Output: `Final_model.rds`**

This chunk compares different models and fit the best model on the full training data by calling `Comparison.R`. The outcome is stored in `Final_model.rds`.

### 11. Code chunk: Prediction

**Runtime: ~2 minutes**

**Input: `Final_model.rds`, `predict_x.csv`, `predict_x_grid.csv`**

**Output: `prediction_2019-2020.csv`, `2019-2020_prediction.jpeg`, `prediction_2019-2020_simulation.csv`**

This final chunk predicts the cause of ignitions for the 2019-2020 season data and the simulation data by calling `2019-2020_prediction.R` and `2019-2020_simulation_prediction.R`. Results are stored in `prediction_2019-2020.csv` and `prediction_2019-2020_simulation.csv`.

## Thesis

With all the preliminary results, the entire thesis can be reproduced by knitting the `index.Rmd`.


# Dependencies

1. `R`>=3.6.0
	- `tidyverse` >= 1.3.0
	- `sf` >= 0.9.5
	- `lubridate` >= 1.7.9
	- `rnaturalearth` >= 0.1.0
	- `here` >= 0.1
	- `raster` >= 3.3.13
	- `bomrang` >= 0.7.0
	- `geodist` >= 0.0.4
	- `progress` >= 1.2.2
	- `GGally` >= 2.0.0
	- `naniar` >= 0.5.2
	- `ggridges` >= 0.5.2
	- `ggthemes` >= 4.2.0
	- `caret` >= 6.0.86
	- `lime` >= 0.5.1
	- `nnet` >= 7.3.12
	- `randomForest` >= 4.6.14
	- `mgcv` >= 1.8.31
	- `fastDummies` >= 1.6.2
	- `xgboost` >= 1.1.1.1
	- `plyr` >= 1.8.6
	- `pROC` >= 1.16.2
	
2. `Python`>=3.7.0
	- `tqdm` >= 4.48.2
	- `numpy` >= 1.19.1
	











