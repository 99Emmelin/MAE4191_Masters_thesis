# R guide for Master's Thesis Analysis
This repository contains R scripts used to analyze educational data from the TIMSS 2019 international assessment. 

The Norwegian and Malaysian datasets are publicly available through the [IEA TIMSS 2019 Data Manager](https://timss2019.org/international-database/).
Users are required to download the files before running the scripts.

The workflow consists of nine core scripts that must be run in strict numerical order (00–08), as each step builds upon the previous. To automate this process, use run_all.R as the main entry point.         
   **Note!** At the top of this script, there is a line that sets the folder path using `data_dir <- "your/path/here"` — update this to the location where you saved the downloaded .sav files.
   No other script paths need to be changed.   
This script sets the seed, defines the working directory, and sequentially sources the following scripts:

1. **00_functions.R** loads required packages and defines all custom functions.

2. **01_data_prep.R** imports and cleans the TIMSS student, teacher, and school data.

3. **02_feature_engineering.R** constructs categorical and continuous resilience variables.

4. **03_descriptive_analysis.R** performs summary statistics and visual diagnostics.

5. **04_modeling_categorical.R** defines and trains nested CV XGBoost classifiers.

6. **05_modeling_continuous.R** defines and trains nested CV XGBoost regressors.

7. **06_aggregate_and_export.R** compiles metrics and exports results.

8. **07_plots_and_report.R** generates all final model-related visualizations.

9. **08_cross_country_generalization.R** tests model generalizability across countries.
