################################################################################
################################################################################
##########               Cross-Country Generalization                  #########
################################################################################
################################################################################

# Purpose:
#   Final XGBoost (20-predictor) models trained in one country generalized to
#   the other, for both categorical and continuous resilience outcomes.

# Required objects from 02_feature_engineering.R:
#   mys_cat_data,  nor_cat_data, mys_cont_data, nor_cont_data,
#   resilience_vars, resilience_vars_cont, resilience_labels, predictors
#
# Required objects from 04_modeling_categorical.R / 05_modeling_continuous.R
#   mys_cat_models,  nor_cat_models, mys_cont_models, nor_cont_models

################################################################################
#### Cross-Country Generalization: Categorical Resilience (20 Predictors)  
################################################################################

# (Reusing trained models stored in mys_cat_models and nor_cat_models)

# Prepare an output data frame
cross_country_auc <- data.frame(
  Resilience_Variable = resilience_vars,
  AUC_MYS_to_NOR      = NA,  # Models trained on Malaysia, applied to Norway
  AUC_NOR_to_MYS      = NA   # Models trained on Norway, applied to Malaysia
)

# Loop over each resilience variable
for (i in seq_along(resilience_vars)) {
  res_var <- resilience_vars[i]
  
  # Use Malaysia models to predict on Norway
  # Each element of mys_cat_models[[i]] is one CV model (from one fold)
  preds_list <- lapply(mys_cat_models[[i]], function(fold_model) {
    # Create a DMatrix for the entire Norway categorical dataset using the 20 predictors
    dtest <- xgb.DMatrix(data = as.matrix(nor_cat_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  # Average predictions across all folds
  preds_agg <- Reduce("+", preds_list) / length(preds_list)
  
  # Compute AUC on Norway's actual binary resilience values for the current variable
  auc_val <- pROC::auc(pROC::roc(nor_cat_data[[res_var]], preds_agg))
  cross_country_auc$AUC_MYS_to_NOR[i] <- round(as.numeric(auc_val), 3)
  
  # Use Norway models to predict on Malaysia 
  preds_list <- lapply(nor_cat_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(mys_cat_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_list) / length(preds_list)
  
  auc_val <- pROC::auc(pROC::roc(mys_cat_data[[res_var]], preds_agg))
  cross_country_auc$AUC_NOR_to_MYS[i] <- round(as.numeric(auc_val), 3)
}

# Improved Column Names
colnames(cross_country_auc) <- c(
  "Resilience Variable",
  "Malaysia Model Applied to Norway (AUC)",
  "Norway Model Applied to Malaysia (AUC)"
)

cross_country_auc$`Resilience Variable` <- resilience_labels[cross_country_auc$`Resilience Variable`]

# save the cross-country AUC results
write.csv(cross_country_auc, "Cross_Country_AUC_20predictors.csv", row.names = FALSE)


################################################################################
#### Cross-Country Generalization: Continuous Resilience (20 Predictors)  
################################################################################

# (Reusing trained models stored in mys_cont_models and nor_cont_models)

# Prepare an output data frame
cross_country_rmse <- data.frame(
  Resilience_Variable = resilience_vars_cont,
  RMSE_MYS_to_NOR     = NA,  # Models trained on Malaysia, applied to Norway
  RMSE_NOR_to_MYS     = NA   # Models trained on Norway, applied to Malaysia
)

# Loop over each continuous resilience variable
for (i in seq_along(resilience_vars_cont)) {
  res_var <- resilience_vars_cont[i]
  
  # Use Malaysia continuous models to predict on Norway
  preds_list <- lapply(mys_cont_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(nor_cont_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_list) / length(preds_list)
  
  rmse_val <- sqrt(mean((nor_cont_data[[res_var]] - preds_agg)^2, na.rm = TRUE))
  cross_country_rmse$RMSE_MYS_to_NOR[i] <- round(rmse_val, 3)
  
  # Use Norway continuous models to predict on Malaysia
  preds_list <- lapply(nor_cont_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(mys_cont_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_list) / length(preds_list)
  
  rmse_val <- sqrt(mean((mys_cont_data[[res_var]] - preds_agg)^2, na.rm = TRUE))
  cross_country_rmse$RMSE_NOR_to_MYS[i] <- round(rmse_val, 3)
}

colnames(cross_country_rmse) <- c(
  "Resilience Variable",
  "Malaysia Model Applied to Norway (RMSE)",
  "Norway Model Applied to Malaysia (RMSE)"
)

cross_country_rmse$`Resilience Variable` <- resilience_labels[cross_country_rmse$`Resilience Variable`]

# Save the cross-country RMSE results
write.csv(cross_country_rmse, "Cross_Country_RMSE_20predictors.csv", row.names = FALSE)


################################################################################
#### Extended Metrics & Aggregation 
################################################################################

# Prepare output data frames with extended metrics
cross_country_cat_metrics <- data.frame(
  Resilience_Variable = resilience_vars,
  AUC_MYS_to_NOR = NA,
  Accuracy_MYS_to_NOR = NA,
  Sensitivity_MYS_to_NOR = NA,
  Specificity_MYS_to_NOR = NA,
  Precision_MYS_to_NOR = NA,
  F1_MYS_to_NOR = NA,
  
  AUC_NOR_to_MYS = NA,
  Accuracy_NOR_to_MYS = NA,
  Sensitivity_NOR_to_MYS = NA,
  Specificity_NOR_to_MYS = NA,
  Precision_NOR_to_MYS = NA,
  F1_NOR_to_MYS = NA
)

cross_country_cont_metrics <- data.frame(
  Resilience_Variable = resilience_vars_cont,
  RMSE_MYS_to_NOR = NA,
  R2_MYS_to_NOR = NA,
  Upper_RMSE_MYS_to_NOR = NA,
  Naive_RMSE_MYS_to_NOR = NA,
  Upper_R2_MYS_to_NOR = NA,
  
  RMSE_NOR_to_MYS = NA,
  R2_NOR_to_MYS = NA,
  Upper_RMSE_NOR_to_MYS = NA,
  Naive_RMSE_NOR_to_MYS = NA,
  Upper_R2_NOR_to_MYS = NA
)

##### Calculate categorical metrics for cross-country predictions ################

for (i in seq_along(resilience_vars)) {
  res_var <- resilience_vars[i]
  
  # Malaysia to Norway
  preds_list <- lapply(mys_cat_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(nor_cat_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_list) / length(preds_list)
  
  roc_obj <- pROC::roc(nor_cat_data[[res_var]], preds_agg)
  cross_country_cat_metrics$AUC_MYS_to_NOR[i] <- round(as.numeric(pROC::auc(roc_obj)), 3)
  
  pred_class <- as.integer(preds_agg > 0.5) # using 0.5 threshold
  conf_mat <- caret::confusionMatrix(factor(pred_class), factor(nor_cat_data[[res_var]]), positive = "1")
  
  cross_country_cat_metrics[i, c("Accuracy_MYS_to_NOR", "Sensitivity_MYS_to_NOR",
                                 "Specificity_MYS_to_NOR", "Precision_MYS_to_NOR",
                                 "F1_MYS_to_NOR")] <- round(
                                   c(conf_mat$overall['Accuracy'], conf_mat$byClass['Sensitivity'],
                                     conf_mat$byClass['Specificity'], conf_mat$byClass['Precision'],
                                     conf_mat$byClass['F1']), 3)
  
  # Norway to Malaysia
  preds_list <- lapply(nor_cat_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(mys_cat_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_list) / length(preds_list)
  
  roc_obj <- pROC::roc(mys_cat_data[[res_var]], preds_agg)
  cross_country_cat_metrics$AUC_NOR_to_MYS[i] <- round(as.numeric(pROC::auc(roc_obj)), 3)
  
  pred_class <- as.integer(preds_agg > 0.5)
  conf_mat <- caret::confusionMatrix(factor(pred_class), factor(mys_cat_data[[res_var]]), positive = "1")
  
  cross_country_cat_metrics[i, c("Accuracy_NOR_to_MYS", "Sensitivity_NOR_to_MYS",
                                 "Specificity_NOR_to_MYS", "Precision_NOR_to_MYS",
                                 "F1_NOR_to_MYS")] <- round(
                                   c(conf_mat$overall['Accuracy'], conf_mat$byClass['Sensitivity'],
                                     conf_mat$byClass['Specificity'], conf_mat$byClass['Precision'],
                                     conf_mat$byClass['F1']), 3)
}

##### Calculate continuous metrics for cross-country predictions ################

for (i in seq_along(resilience_vars_cont)) {
  res_var <- resilience_vars_cont[i]
  
  # Malaysia to Norway 
  preds_folds <- lapply(mys_cont_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(nor_cont_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_folds) / length(preds_folds)
  y_true <- nor_cont_data[[res_var]]
  
  # overall RMSE/R²
  overall_rmse <- sqrt(mean((y_true - preds_agg)^2, na.rm = TRUE))
  overall_r2 <- 1 - sum((y_true - preds_agg)^2, na.rm = TRUE) / 
    sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
  
  # upper-quartile metrics
  upper_threshold <- quantile(y_true, 0.75, na.rm = TRUE)
  upper_indices <- which(y_true >= upper_threshold)
  upper_rmse <- sqrt(mean((y_true[upper_indices] - preds_agg[upper_indices])^2, na.rm = TRUE))
  upper_r2   <- 1 - sum((y_true[upper_indices] - preds_agg[upper_indices])^2, na.rm = TRUE) / 
    sum((y_true[upper_indices] - mean(y_true[upper_indices], na.rm = TRUE))^2, na.rm = TRUE)
  
  cross_country_cont_metrics$RMSE_MYS_to_NOR[i] <- round(overall_rmse, 3)
  cross_country_cont_metrics$R2_MYS_to_NOR[i] <- round(overall_r2, 3)
  cross_country_cont_metrics$Upper_RMSE_MYS_to_NOR[i] <- round(upper_rmse, 3)
  cross_country_cont_metrics$Upper_R2_MYS_to_NOR[i] <- round(upper_r2, 3)
  cross_country_cont_metrics$Naive_RMSE_MYS_to_NOR[i] <- round(sqrt(mean((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)), 3)
  
  
  # Norway to Malaysia 
  preds_folds <- lapply(nor_cont_models[[i]], function(fold_model) {
    dtest <- xgb.DMatrix(data = as.matrix(mys_cont_data[, predictors]))
    predict(fold_model$model, dtest)
  })
  preds_agg <- Reduce("+", preds_folds) / length(preds_folds)
  y_true <- mys_cont_data[[res_var]]
  
  overall_rmse <- sqrt(mean((y_true - preds_agg)^2, na.rm = TRUE))
  overall_r2 <- 1 - sum((y_true - preds_agg)^2, na.rm = TRUE) / 
    sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
  
  # Compute upper metrics consistently by subsetting the upper 25% of y_true
  upper_threshold <- quantile(y_true, 0.75, na.rm = TRUE)
  upper_indices <- which(y_true >= upper_threshold)
  upper_rmse <- sqrt(mean((y_true[upper_indices] - preds_agg[upper_indices])^2, na.rm = TRUE))
  upper_r2   <- 1 - sum((y_true[upper_indices] - preds_agg[upper_indices])^2, na.rm = TRUE) / 
    sum((y_true[upper_indices] - mean(y_true[upper_indices], na.rm = TRUE))^2, na.rm = TRUE)
  
  cross_country_cont_metrics$RMSE_NOR_to_MYS[i] <- round(overall_rmse, 3)
  cross_country_cont_metrics$R2_NOR_to_MYS[i] <- round(overall_r2, 3)
  cross_country_cont_metrics$Upper_RMSE_NOR_to_MYS[i] <- round(upper_rmse, 3)
  cross_country_cont_metrics$Upper_R2_NOR_to_MYS[i] <- round(upper_r2, 3)
  cross_country_cont_metrics$Naive_RMSE_NOR_to_MYS[i] <- round(sqrt(mean((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)), 3)
}

# For categorical metrics (averaged across resilience variables)
cross_country_cat_means <- data.frame(
  Metric = c("AUC_MYS_to_NOR", "Accuracy_MYS_to_NOR", "Sensitivity_MYS_to_NOR",
             "Specificity_MYS_to_NOR", "Precision_MYS_to_NOR", "F1_MYS_to_NOR",
             "AUC_NOR_to_MYS", "Accuracy_NOR_to_MYS", "Sensitivity_NOR_to_MYS",
             "Specificity_NOR_to_MYS", "Precision_NOR_to_MYS", "F1_NOR_to_MYS"),
  Mean_Value = sapply(cross_country_cat_metrics[, -1], function(col) round(mean(col, na.rm = TRUE), 3))
)

# For continuous metrics (averaged across resilience variables)
cross_country_cont_means <- data.frame(
  Metric = names(cross_country_cont_metrics)[-1],
  Mean_Value = sapply(cross_country_cont_metrics[, -1], function(col) round(mean(col, na.rm = TRUE), 3))
)

# Save performance table 
write.csv(cross_country_cat_means, "cross_country_cat_metrics_means.csv", row.names = FALSE)
write.csv(cross_country_cont_means, "cross_country_cont_metrics_means.csv", row.names = FALSE)
