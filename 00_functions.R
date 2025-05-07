################################################################################
################################################################################
############                Setup & Functions                       ############
################################################################################
################################################################################

# Purpose:
# Central collection of functions for the TIMSS resilience analysis pipeline.
# Organized into: setup, descriptive stats, aggregation & comparison,
# upsampling & balancing, modeling metrics, summary utilities, feature importance,
# and plotting helpers.

################################################################################
############ SETUP
################################################################################

## Vector of packages used
pkgs <- c(
  "foreign", "readstata13", "tidyverse", "reshape2", "xgboost",
  "pROC", "corrplot", "gridExtra", "caret", "patchwork", "rstudioapi"
)

## Install any that are missing, then load them
to_get <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_get)) install.packages(to_get, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))


## Library
library(rstudioapi)      # Sets working directory to the script’s folder
library(foreign)         # Read SPSS (.sav) files
library(readstata13)     # Alternative SPSS/Stata import
library(tidyverse)       # Includes dplyr, tidyr, ggplot2, stringr, tibble
library(reshape2)        # For correlation plot
library(xgboost)         # Gradient boosting models
library(pROC)            # ROC/AUC evaluation
library(corrplot)        # Correlation matrix for predictors
library(gridExtra)       # For combining plots
library(caret)           # ConfusionMatrix and CV folds
library(patchwork)       # For combining plots

set.seed(123)  # Setting a seed for reproducibility

## Set working dictionary
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Create models directory if missing
if (!dir.exists("models")) dir.create("models")

################################################################################
############ Descriptive Stats functions
################################################################################

## Descriptive Stats functions used in 03_descriptive_analysis.R

# avg_corr(): Correlation between one predictor and first categorical resilience vars
avg_corr <- function(df, var) {
  cor(df[[var]], df[[first_rv_cat]], use = "pairwise.complete.obs") 
}


# avg_corr_cont(): Pearson r between one predictor and first continuous resilience vars
avg_corr_cont <- function(df, var) {
  cor(df[[var]], df[[first_rv_cont]], use = "pairwise.complete.obs")
}


# avg_pval_cat(): p‐value between one predictor and first categorical resilience vars
avg_pval_cat <- function(df, var) {
  cor.test(df[[var]], df[[first_rv_cat]], use = "complete.obs")$p.value
}


# avg_pval_cont(): p‐value between one predictor and first continuous resilience vars
avg_pval_cont <- function(df, var) {
  cor.test(df[[var]], df[[first_rv_cont]], use = "complete.obs")$p.value
}


# sig(): Helper to turn p's into stars
sig <- function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","")))


# generate_resilience_table(): Summarize counts & % of resilient vs non-resilient for each resilience vars
generate_resilience_table <- function(data, resilience_vars) {
  table_list <- list()
  
  for (var in resilience_vars) {
    count_table <- table(data[[var]])
    prop_table <- prop.table(count_table) * 100  # Convert proportions to percentages
    
    table_list[[var]] <- data.frame(
      Resilience_Variable = var,
      Resilient = count_table["1"],
      Non_Resilient = count_table["0"],
      Resilient_Percentage = round(prop_table["1"], 2),  
      Non_Resilient_Percentage = round(prop_table["0"], 2)  
    )
  }
  # Combine into a single data frame
  resilience_summary <- bind_rows(table_list)
  return(resilience_summary)
}


# summarize_missingness(): Compute missing count & percent for each column
summarize_missingness <- function(data) {
  missing_summary <- data %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
    mutate(
      Total_Observations = nrow(data),
      Missing_Percentage = round((Missing_Count / Total_Observations) * 100, 2),
    ) %>%
    arrange(desc(Missing_Percentage))
  return(missing_summary)
}


# replace_with_descriptive_names(): Replace variable names with descriptive names
replace_with_descriptive_names <- function(missing_summary, descriptive_names) {
  missing_summary %>%
    mutate(Variable = ifelse(Variable %in% names(descriptive_names), 
                             descriptive_names[Variable], Variable))
}


###############################################################################
#### Aggregation & Comparison functions
###############################################################################

## Aggregation & Comparison functions used in 03_descriptive_analysis.R

# apply_descriptive_names(): Rename Variable column
apply_descriptive_names <- function(df) {
  df %>%
    mutate(Variable = ifelse(Variable %in% names(descriptive_names), 
                             # uses global descriptive_names vector (set in 01_data_prep.R)
                             descriptive_names[Variable], 
                             Variable))  # Keep original if not in the mapping
}


# compare_categorical_resilience_aggregated(): For binary resilience,
# compute group means, SDs, t-test and Cohen's d, then aggregate across vars 
# for resilient and nonresilient students
compare_categorical_resilience_aggregated <- function(data, resilience_vars, predictors) {
  results_list <- list() # List of results for each resilience variable
  for (rv in resilience_vars) {
    # Compute group means and SDs 
    group_means <- data %>%
      group_by(!!sym(rv)) %>%
      summarise(
        across(
          all_of(predictors),
          list(
            mean = ~ round(mean(.x, na.rm = TRUE), 2),
            sd   = ~ round(sd(.x, na.rm = TRUE), 2)
          )
        ),
        .groups = "drop"
      ) %>%
      # Pivot to long format 
      pivot_longer(
        cols = -!!sym(rv),
        names_to  = c("Variable", ".value"),
        names_sep = "_"
      )
    
    # For each predictor, run a two-sample t-test (comparing resilient vs. non-resilient)
    # and calculate Cohen's d
    t_tests <- lapply(predictors, function(var) {
      group0 <- data[[var]][data[[rv]] == 0]
      group1 <- data[[var]][data[[rv]] == 1]
      
      t_test <- t.test(group1, group0, var.equal = TRUE)
      
      n0 <- length(na.omit(group0))
      n1 <- length(na.omit(group1))
      sd0 <- sd(group0, na.rm = TRUE)
      sd1 <- sd(group1, na.rm = TRUE)
      
      # Mean difference: (resilient group) minus (non-resilient group)
      mean_diff <- mean(group1, na.rm = TRUE) - mean(group0, na.rm = TRUE)
      
      # Pooled standard deviation for Cohen's d
      pooled_sd <- sqrt(((n0 - 1) * sd0^2 + (n1 - 1) * sd1^2) / (n0 + n1 - 2))
      cohen_d <- mean_diff / pooled_sd
      
      tibble(
        Variable        = var,
        Mean_Difference = round(mean_diff, 2),
        p_value         = round(t_test$p.value, 3),
        Cohen_d         = round(cohen_d, 2)
      )
    }) %>% bind_rows()
    
    # Merge the group means and t-test results
    rv_results <- group_means %>%
      pivot_wider(
        names_from  = !!sym(rv),
        values_from = c(mean, sd)
      ) %>%
      # Rename columns for clarity
      rename(
        Non_Resilient_Mean = `mean_0`,
        Resilient_Mean     = `mean_1`,
        Non_Resilient_SD   = `sd_0`,
        Resilient_SD       = `sd_1`
      ) %>%
      left_join(t_tests, by = "Variable") %>%
      mutate(Resilience_Variable = rv)
    
    results_list[[rv]] <- rv_results
  }
  
  # Combine results from all resilience variables 
  combined_results <- bind_rows(results_list)
  
  # Aggregate results by predictor 
  aggregated_results <- combined_results %>%
    group_by(Variable) %>%
    summarise(
      Non_Resilient_Mean = round(mean(Non_Resilient_Mean, na.rm = TRUE), 2),
      Resilient_Mean     = round(mean(Resilient_Mean, na.rm = TRUE), 2),
      Non_Resilient_SD   = round(mean(Non_Resilient_SD, na.rm = TRUE), 2),
      Resilient_SD       = round(mean(Resilient_SD, na.rm = TRUE), 2),
      Mean_Difference    = round(mean(Mean_Difference, na.rm = TRUE), 2),
      p_value            = round(mean(p_value, na.rm = TRUE), 3),
      Cohen_d            = round(mean(Cohen_d, na.rm = TRUE), 2),
      N_ResilienceVars   = n()
    ) %>%
    ungroup()
  
  return(aggregated_results)
}


#############################################################################
#### Functions to Class Balancing & Cross-Validation Folds
#############################################################################

##  Functions to Class Balancing & Cross-Validation Folds used in 
##  04_modeling_categorical.R and 05_modeling_continuous.R

# balance_training_data(): Upsample resilient = 1 to match non-resilient count
balance_training_data <- function(train_data, resilience_var) {
  # Identify non-resilient and resilient students
  idNR <- train_data %>% filter(!!sym(resilience_var) == 0)
  idR <- train_data %>% filter(!!sym(resilience_var) == 1)
  
  # Upsample resilient students to match non-resilient count
  idR_us <- idR[sample(nrow(idR), nrow(idNR), replace = TRUE), ]
  
  # Combine non-resilient and upsampled resilient
  balanced_data <- bind_rows(idNR, idR_us)
  
  return(balanced_data)
}


# create_folds(): Stratified k-fold indices for binary target
create_folds <- function(data, stratify_var = "resilience_01", k = 5) {
  folds <- createFolds(data[[stratify_var]], k = k, returnTrain = FALSE)
  return(folds)
}


# create_folds_reg(): Random k-fold indices for regression (non‑stratified)
create_folds_reg <- function(data, k = 5) {
  set.seed(123)  # For reproducibility 
  fold_indices <- sample(rep(1:k, length.out = nrow(data)))
  folds <- vector("list", k)
  for(i in 1:k) {
    folds[[i]] <- which(fold_indices == i)
  }
  return(folds)
}


#############################################################################
#### Modeling & performance Metrics
#############################################################################

## Modeling & performance Metrics used in 04_modeling_categorical.R

# Compute_classification_metrics(): From actual vs predicted_prob, 
compute_classification_metrics <- function(actual, predicted_prob, threshold = 0.5) { # get confusion matrix & classification metrics
  predicted_class <- ifelse(predicted_prob >= threshold, 1, 0)   # Convert predicted probabilities into binary predictions
  
  # Create confusion matrix
  cm <- caret::confusionMatrix(factor(predicted_class), factor(actual), positive = "1")
  
  # Extract metrics
  accuracy    <- as.numeric(cm$overall["Accuracy"])
  sensitivity <- as.numeric(cm$byClass["Sensitivity"])  
  specificity <- as.numeric(cm$byClass["Specificity"])
  precision   <- as.numeric(cm$byClass["Pos Pred Value"])
  # Compute F1 score manually (to avoid division by zero)
  if ((precision + sensitivity) > 0) {
    f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  } else {
    f1 <- NA
  }
  
  list(
    confusion_matrix = cm$table,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = sensitivity,  # Recall is sensitivity
    f1 = f1
  )
}


#############################################################################
#### Categorical & Continuous Summaries functions for XGBoost Final models
#############################################################################

# Categorical & Continuous Summaries functions for XGBoost Final models used 
# in 04_modeling_categorical.R and 05_modeling_continuous.R

# avg_fold(): for averaging across folds, rounded to 3 decimals
avg_fold      <- function(folds, name)           round(mean(sapply(folds, `[[`, name)), 3)


# avg_fold_met(): Mean of metrics[[metric]] across folds, rounded to 3 decimals
avg_fold_met  <- function(folds, metric)         round(mean(sapply(folds, function(x) x$metrics[[metric]])), 3)


# summarize_categorical(): Build DF of average AUC, accuracy, etc.
summarize_categorical <- function(models, vars) {
  data.frame(
    ResilienceVar   = vars,
    MeanAUC         = sapply(models, avg_fold,     "auc"),
    MeanAccuracy    = sapply(models, avg_fold_met, "accuracy"),
    MeanSensitivity = sapply(models, avg_fold_met, "sensitivity"),
    MeanSpecificity = sapply(models, avg_fold_met, "specificity"),
    MeanPrecision   = sapply(models, avg_fold_met, "precision"),
    MeanF1          = sapply(models, avg_fold_met, "f1")
  )
}


# summarize_continuous(): Build DF of average RMSE, R2, etc.
summarize_continuous <- function(models, vars) {
  data.frame(
    ResilienceVar = vars,
    MeanRMSE      = sapply(models, avg_fold, "rmse"),
    UpperRMSE     = sapply(models, avg_fold, "upper_rmse"),
    DumbRMSE      = sapply(models, avg_fold, "dumb_resilient_rmse"),  # reuse avg_fold here
    MeanR2        = sapply(models, avg_fold, "r2"),
    UpperR2       = sapply(models, avg_fold, "upper_r2")
  )
}


# pool_cm(): Sum confusion matrices across 5 folds
pool_cm <- function(models_i) {
  # Extract the 5 confusion matrices and sum them
  Reduce(`+`, lapply(models_i, function(f) f$metrics$confusion_matrix))
}


#############################################################################
#### Feature Importance Summary function
#############################################################################

## Feature Importance Summary function used in 06_aggregate_and_export.R

# aggregate_importance(): Summarize feature importance by mean & total gain
aggregate_importance <- function(importance_data) {
  importance_data %>%
    group_by(Feature) %>%
    summarize(
      Mean_Gain   = mean(Gain, na.rm = TRUE),
      Total_Gain  = sum(Gain, na.rm = TRUE),
      N_Folds_Used = n()
    ) %>%
    arrange(desc(Mean_Gain))
}


#############################################################################
#### Functions for Plots
#############################################################################

# Functions for Plots used in 07_plots_and_report.R

# merge_factor_group(): Merge Factor Grouping with Feature Importance Data
merge_factor_group <- function(importance_data) {
  importance_data %>%
    # factor_group data-frame built in 06_aggregate_and_export.R
    left_join(factor_group, by = "Feature")
}


# g_legend(): Extract legend from ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# create_grouped_importance_plot(): Plot top-n feature importance bars
create_grouped_importance_plot <- function(data, title, max_y) {
  data_top10 <- data %>% slice_max(order_by = Mean_Gain, n = 10) # top 10 features by Mean_Gain
  
  # Wrap long feature names to fit in multiple lines
  data_top10$Feature <- str_wrap(data_top10$Feature, width = 30)
  
  ggplot(data_top10, aes(x = reorder(Feature, Mean_Gain), y = Mean_Gain, fill = Factor)) +
    geom_bar(stat = "identity", color = "black", width = 0.6) +
    coord_flip(clip = "off") +  # Allow labels to extend beyond plot region
    scale_fill_manual(values = factor_colors) + # factor_colors palette defined in 07_plots_and_report.R
    labs(
      title = title,
      x = NULL,
      y = "Feature Importance (Mean Gain)",
      fill = "Factor Group"
    ) +
    ylim(0, max_y) +
    theme_classic() +
    theme(
      plot.title       = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title.x     = element_text(size = 16),
      axis.title.y     = element_text(size = 14),
      axis.text.x      = element_text(size = 16),
      axis.text.y      = element_text(size = 16),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(size = 16),
      legend.text      = element_text(size = 16),
      plot.margin      = margin(t = 10, r = 80, b = 10, l = 10)
    )
}


# plot_roc_per_resilience(): ROC curves for each resilience var across folds
plot_roc_per_resilience <- function(models_list, data, folds_list, predictors, resilience_vars,
                                    main_title = "", save_path = NULL, line_color = "blue", show_legend = F) {
  # Initialize empty ROC plot
  plot(NULL, 
       xlim = c(0, 1), 
       ylim = c(0, 1),
       xlab = "False Positive Rate", 
       ylab = "True Positive Rate",
       main = main_title,
       cex.lab = 1.2, 
       cex.axis = 1.1, 
       font.lab = 2)
  # Add diagonal “no-skill” reference line
  abline(a = 0, b = 1, lty = 2, col = "gray")
  # (We omit grid() intentionally to keep it clean)
  
  legend_labels <- c() # Prepare storage for legend entries
  
  # Loop over each resilience variable
  for (i in seq_along(resilience_vars)) {
    var_name <- resilience_vars[i]
    all_preds <- c()
    all_actuals <- c()
    
    # Within each var, loop over folds to gather test predictions
    for (fold_idx in seq_along(models_list[[i]])) {
      test_idx <- folds_list[[i]][[fold_idx]]
      test_data <- data[test_idx, ]
      final_model <- models_list[[i]][[fold_idx]]$model
      
      # Convert predictors to DMatrix, run predict
      dtest <- xgb.DMatrix(as.matrix(test_data[, predictors]))
      preds <- predict(final_model, dtest)
      
      # Accumulate across folds
      all_preds <- c(all_preds, preds)
      all_actuals <- c(all_actuals, test_data[[var_name]])
    }
    
    # Compute ROC curve object and AUC
    roc_obj <- pROC::roc(all_actuals, all_preds)
    auc_val <- pROC::auc(roc_obj)
    lines(roc_obj, col = line_color, lwd = 2) # Add the ROC line to the existing plot
    legend_labels[i] <- paste0(var_name, " (AUC=", round(auc_val, 3), ")") # Store legend text with variable name + AUC
  }
  
  # Draw legend with all labels in the same color
  if (show_legend) {
    legend("bottomright", legend = legend_labels,
           col = rep(line_color, length(legend_labels)),
           lty = 1, cex = .8, bty = "n")
  }
  
  # Save the plot to a file
  if (!is.null(save_path)) {
    dev.copy(png, filename = save_path, width = 800, height = 600)
    dev.off()
  }
}


# plot_rec_curves(): REC curves for continuous predictions across folds
plot_rec_curves <- function(models_list, data, folds_list, predictors, target_vars,
                            main_title = "", save_path = NULL, line_color = "blue", show_legend = F) {
  plot(NULL, 
       xlim = c(0, 1), 
       ylim = c(0, 1),
       xlab = "Normalized Error Threshold", 
       ylab = "Fraction Within Threshold",
       main = main_title,  
       cex.lab = 1.2, cex.axis = 1.1, font.lab = 2)
  # Do not call grid() so no background lines are drawn
  
  legend_labels <- c()
  
  for (i in seq_along(target_vars)) {
    var_name <- target_vars[i]
    all_preds <- c()
    all_actuals <- c()
    
    # Combine predictions from all folds for the current target variable
    for (fold_idx in seq_along(models_list[[var_name]])) {
      test_idx <- folds_list[[fold_idx]]
      test_data <- data[test_idx, ]
      final_model <- models_list[[var_name]][[fold_idx]]$model
      
      dtest <- xgb.DMatrix(as.matrix(test_data[, predictors]))
      preds <- predict(final_model, dtest)
      
      all_preds <- c(all_preds, preds)
      all_actuals <- c(all_actuals, test_data[[var_name]])
    }
    
    # Compute absolute errors and define thresholds
    errors <- abs(all_actuals - all_preds)
    max_error <- max(errors, na.rm = TRUE)
    thresholds <- seq(0, max_error, length.out = 100) 
    proportions <- sapply(thresholds, function(thr) mean(errors <= thr)) # For each threshold, compute fraction of observations whose error ≤ threshold
    thresholds_norm <- thresholds / max_error # Normalize thresholds so the X-axis runs from 0 to 1
    
    # Add REC curve to the plot
    lines(thresholds_norm, proportions, col = line_color, lwd = 2)
    legend_labels[i] <- var_name # Store label
  }
  
  # Draw legend with all labels in the same color
  if (show_legend)
    legend("bottomright", legend = legend_labels,
           col = rep(line_color, length(legend_labels)),
           lty = 1, cex = 0.8, bty = "n")
  
  # Save the current plot
  if (!is.null(save_path)) {
    dev.copy(png, filename = save_path, width = 800, height = 600)
    dev.off()
  }
}
