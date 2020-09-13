# Instructions

Before you try to reproduce this research, we need to inform you of several tips: 

- When you run R scripts, please use `Rstudio` to open the R project `reproducible.Rproj`. 

- Every time you finish running a script, please enitrely shutdown the kernel. That means you need to shutdown your R, Rsutido, Python, etc. We are assuming you are working in a clean environment.

- Please check the [Denpendencies] and keep your environment as similar as possible. We do not guarantee the result will be the same under different environments.

- Please strictly follow the instructions step by step. Most steps depend on the results from one or more previous steps.

- It may takes more than 18 hours to reproduce this entire research with a mid-range laptop. We will notify the approximate time needs for each computational intensive step.

## Data Collection

1. Check the `data` folder and download all the data as required. The detail about how to download the data is in the `data` folder. 

## Data Pre-processing

2. Run the script `Clustering-Python-setup.R`. This will preprocess the raw hotspots data for clustering. The processed hotspots data will be stored in `data/VIC_hotspots_before_clustering.csv` and `data/VIC_hotspots_raw.csv`. (Time: 5 mins)

3. To reproduce the parameters tuning process of the clustering algorithm, run the script `clustering_tune.py`. The tuning process will be recorded in `data/clustering_grid.csv`. (Time: 700 mins)

4. To visualize the tuning process, run the script `clustering_tune_vis.R`. The optimal setting will be recorded in `setting.txt`. Meanwhile, the plots will be exported to the folder `plots`.

5. Run the python script `main.py`. This will cluster the hotspots into bushfires. You will find a file `summary.txt` provides you with the summary of the clustering results. (Time: 6 mins)

6. Run the script `combine_his.R` to integrate fire history. The result will be stored in `data/training.csv`. (Time: )

7. Run the script `combine_current.R` to integrate bushfires data from 2019 to 2020. The result will be stored in `data/predict_x.csv`. (Time: )

8. Run the script `combine_grid.R` to integrate simulation bushfires data. The result will be stored in `data/predict_x_grid.csv`. (Time: )

9. Run the script `EDA.R` to perform data visulization on fire history and bushfires from 2019 to 2020. Plots will be exported to the folder `plots`. (Time: )

10. Run the script `MNL.R` to preform training, variable selection and testing on the multinomial logistic regression. The performance of the model will be recorded in `data/MNL.txt`. (Time: )

11. Run the script `GAM.R` to perform training, variable selection and testing on the generalized additive model. The performance of the model will be recorded in `data/GAM.txt`. (Time: )

12. Run the script `RF.R` to perform training, variable selection, hyperparameters tuning and testing on the random forest model. The performance of the model will be recorded in `data/RF.txt`. (Time: )

13. Run the script `XGB.R` to perform training, variable selection, hyperparameters tuning and testing on the XGBoost model. The performance of the model will be recorded in `data/XGB.txt`. (Time: )

14. Run the python script `NN.py` to perform training, variable selection, hyperparameters tuning and testing on the neural network model. The performance of the model will be recorded in `data/NN.txt`. (Time: )

15. Run the script `prediction.R` to predict the ignition cause of bushfires from 2019 to 2020. Plots will be stored in the folder `plots` and the predictions will be stored in `data/prediction.csv`. (Time: )

16. For how to reproduce the thesis of this research, please check `link`.



# Dependencies

1. `R`>=3.6.0
	a. `tidyverse` >= 1.0.0
	b. `sf` 
2. `Python`>=3.7.0
	a. `numpy`











