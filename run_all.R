################################################################################
################################################################################
############                       Master code                      ############
################################################################################
################################################################################

## Library
library(rstudioapi)      # Sets working directory to the script’s folder


## Setting a seed for reproducibility
set.seed(123)  


### Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set the folder path where you saved the downloaded .sav files.
# It only needs be changed here — it will be reused throughout the script.
data_dir <- "C:/Users/PC/Documents/MAE4191/data/"

# Load  functions
source("00_functions.R")

# Stage 1: Prepare & clean raw data
source("01_data_prep.R")

# Stage 2: Feature engineering
source("02_feature_engineering.R")

# Stage 3: Descriptive stats
source("03_descriptive_analysis.R")

# Stage 4: Train XGBoost (categorical outcomes)
source("04_modeling_categorical.R")

# Stage 5: Train XGBoost (continuous outcomes)
source("05_modeling_continuous.R")

# Stage 6: Aggregate & export results
source("06_aggregate_and_export.R")

# Stage 7: Generate plots & compile report
source("07_plots_and_report.R")

# Stage 8: Cross-country generalization analyses
source("08_cross_country_generalization.R")
