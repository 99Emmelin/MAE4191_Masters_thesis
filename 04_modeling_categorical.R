################################################################################
################################################################################
############      XGBoost Categorical Models with Nested CV         ############
################################################################################
################################################################################

# Purpose:
#   Fit binary XGBoost models for the five categorical resilience outcomes
#   (resilience_01 … resilience_05) in Malaysia and Norway using
#   5-fold outer CV + 3-fold inner CV hyper-parameter search.

# required objects coming from 01_data_prep.R:
#   predictors, resilience_vars
#
# required objects coming from 02_feature_engineering.R:
#   mys_cat_data, nor_cat_data

#############################################################################
#### XGBoost Categorical Models function (20 predictors) + Training
#############################################################################

# Build lists of 5 folds (one per resilience variable) for Malaysia and Norway
folds_mys <- lapply(resilience_vars, function(res_var) create_folds(mys_cat_data))
folds_nor <- lapply(resilience_vars, function(res_var) create_folds(nor_cat_data))

# Define grid of hyperparameters to try during inner CV tuning
hyper_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(3, 6, 9),
  nrounds = c(50, 100, 150)
)

# Nested Cross-Validation Training Function for binary XGBoost
train_xgb_categorical <- function(data, folds, predictors, target_var,country) {
  fold_results <- list()
  
  for (fold_idx in seq_along(folds)) {
    # Outer split: training and testing sets
    test_idx <- folds[[fold_idx]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    
    outer_train_data <- data[train_idx, ]
    outer_test_data <- data[test_idx, ]
    
    # Upsample minority class in training set for balance
    balanced_outer_train_data <- balance_training_data(outer_train_data, target_var)
    
    # Check the distribution of balanced data
    cat(sprintf("\nFold %d - Balanced Training Data Distribution for %s:\n", fold_idx, target_var))
    print(table(balanced_outer_train_data[[target_var]]))
    
    # Inner 3‑fold CV for hyperparameter tuning
    inner_folds <- create_folds(balanced_outer_train_data, k = 3)
    best_auc <- 0
    best_params <- NULL
    
    for (params_idx in 1:nrow(hyper_grid)) {
      inner_auc_list <- c()
      
      for (inner_fold_idx in seq_along(inner_folds)) {
        # Split inner train/test
        inner_test_idx <- inner_folds[[inner_fold_idx]]
        inner_train_idx <- setdiff(seq_len(nrow(balanced_outer_train_data)), inner_test_idx)
        
        inner_train_data <- balanced_outer_train_data[inner_train_idx, ]
        inner_test_data <- balanced_outer_train_data[inner_test_idx, ]
        
        # Train with early stopping
        params <- list(
          objective = "binary:logistic",
          eval_metric = "auc",
          eta = hyper_grid$eta[params_idx],
          max_depth = hyper_grid$max_depth[params_idx]
        )
        
        # Build DMatrices
        dtrain <- xgb.DMatrix(data = as.matrix(inner_train_data[, predictors]), label = inner_train_data[[target_var]])
        
        dtest <- xgb.DMatrix(data = as.matrix(inner_test_data[, predictors]), label = inner_test_data[[target_var]])
        
        # Train the model
        model <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = hyper_grid$nrounds[params_idx],
          watchlist = list(eval = dtest),
          early_stopping_rounds = 10,
          verbose = 0
        )
        
        # Evaluate AUC on inner test fold
        predictions <- predict(model, dtest)
        auc <- pROC::auc(pROC::roc(inner_test_data[[target_var]], predictions))
        inner_auc_list <- c(inner_auc_list, auc)
      }
      
      # Track the best parameters by average AUC
      avg_auc <- mean(inner_auc_list)
      
      # Update the best hyperparameters
      if (avg_auc > best_auc) {
        best_auc <- avg_auc
        best_params <- hyper_grid[params_idx, ]
      }
    }
    
    # Train final model on the outer training set with best hyperparameters
    final_dtrain <- xgb.DMatrix(data = as.matrix(balanced_outer_train_data[, predictors]), label = balanced_outer_train_data[[target_var]])
    
    final_dtest <- xgb.DMatrix(data = as.matrix(outer_test_data[, predictors]), label = outer_test_data[[target_var]])
    
    final_model <- xgb.train(
      params = list(
        objective = "binary:logistic",
        eval_metric = "auc",
        eta = best_params$eta,
        max_depth = best_params$max_depth
      ),
      data = final_dtrain,
      nrounds = best_params$nrounds,
      watchlist = list(train = final_dtrain, eval = final_dtest),
      early_stopping_rounds = 10,
      print_every_n = 10
    )
    
    # Evaluate on outer test set
    final_predictions <- predict(final_model, final_dtest)
    final_auc <- pROC::auc(pROC::roc(outer_test_data[[target_var]], final_predictions))
    metrics <- compute_classification_metrics(
      actual = outer_test_data[[target_var]],
      predicted_prob = final_predictions,
      threshold = 0.5  
    )
    
    # Save final model and results
    fold_results[[fold_idx]] <- list(model = final_model, auc = final_auc, metrics = metrics, best_params = best_params)
    
    # Save the final model for this fold
    model_filename <- paste0("models/final_model_", country, target_var, "_fold_", fold_idx, ".model")
    xgb.save(final_model, model_filename)
    
    # Save the fold results (including AUC) after each fold
    fold_results_filename <- paste0("models/fold_results_", country, target_var, ".rds")
    saveRDS(fold_results, file = fold_results_filename)
    
    # Confirmation message
    cat(sprintf("Saved model and results for %s (Fold %d, %s).\n", target_var, fold_idx, country))
    
  }
  return(fold_results)
}

# Train models for Malaysia
mys_cat_models <- lapply(seq_along(resilience_vars), function(i) {
  train_xgb_categorical(mys_cat_data, folds_mys[[i]], predictors, resilience_vars[i], "MYS")
})

# Train models for Norway
nor_cat_models <- lapply(seq_along(resilience_vars), function(i) {
  train_xgb_categorical(nor_cat_data, folds_nor[[i]], predictors, resilience_vars[i], "NOR")
})
