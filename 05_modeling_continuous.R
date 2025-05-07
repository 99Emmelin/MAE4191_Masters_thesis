################################################################################
################################################################################
############      XGBoost Continuous Models with Nested CV          ############
################################################################################
################################################################################

# Purpose:
# Train regression XGBoost models for the five continuous resilience outcomes
# (Resilience_BSSSCI01 … Resilience_BSSSCI05) in Malaysia and Norway by means of
# 5-fold outer CV and 3-fold inner CV hyper-parameter search.

# Required objects coming from 01_data_prep.R:
#   predictors, resilience_vars_cont
#
# Required objects coming from 02_feature_engineering.R
#   mys_cont_data and nor_cont_data

#############################################################################
#### XGBoost Continuous Models function (20 predictors) + Training
#############################################################################

# Define hyperparameter grid for regression models (same η and depth values, but RMSE metric)
hyper_grid_reg <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(3, 6, 9),
  nrounds = c(50, 100, 150)
)

# Nested CV training function for continuous targets
train_xgb_continuous <- function(data, folds, predictors, target_var, country) {
  # List storing the model and performance for each outer fold.
  fold_results <- list()
  
  # Loop over the outer folds.
  for (fold_idx in seq_along(folds)) {
    cat(sprintf("\nProcessing outer fold %d for target %s\n", fold_idx, target_var))
    
    # Split into outer training and test sets by these indices
    test_idx <- folds[[fold_idx]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    outer_train_data <- data[train_idx, ]
    outer_test_data <- data[test_idx, ]
    
    # Inner CV: create 3 folds on outer training for hyperparameter tuning
    inner_folds <- create_folds_reg(outer_train_data, k = 3)
    
    best_rmse <- Inf  # start with infinity.
    best_params <- NULL
    
    for (params_idx in 1:nrow(hyper_grid_reg)) {
      inner_rmse_list <- c()
      
      # Loop through each inner fold to compute RMSE for this parameter set
      for (inner_fold_idx in seq_along(inner_folds)) {
        inner_test_idx <- inner_folds[[inner_fold_idx]]
        inner_train_idx <- setdiff(seq_len(nrow(outer_train_data)), inner_test_idx)
        
        inner_train_data <- outer_train_data[inner_train_idx, ]
        inner_test_data <- outer_train_data[inner_test_idx, ]
        
        # Train with early stopping on RMSE.
        params <- list(
          objective = "reg:squarederror",  # regression objective
          eval_metric = "rmse",             # root-mean-square error
          eta = hyper_grid_reg$eta[params_idx],
          max_depth = hyper_grid_reg$max_depth[params_idx]
        )
        
        # Prepare the training and test matrices.
        dtrain_inner <- xgb.DMatrix(data = as.matrix(inner_train_data[, predictors]), 
                                    label = inner_train_data[[target_var]])
        dtest_inner <- xgb.DMatrix(data = as.matrix(inner_test_data[, predictors]), 
                                   label = inner_test_data[[target_var]])
        
        # Train the model using the current hyperparameters.
        model_inner <- xgb.train(
          params = params,
          data = dtrain_inner,
          nrounds = hyper_grid_reg$nrounds[params_idx],
          watchlist = list(eval = dtest_inner),
          early_stopping_rounds = 10,
          verbose = 0
        )
        
        # Evaluate RMSE on inner test fold
        preds_inner <- predict(model_inner, dtest_inner)
        actual_inner <- inner_test_data[[target_var]]
        rmse_inner <- sqrt(mean((actual_inner - preds_inner)^2))
        inner_rmse_list <- c(inner_rmse_list, rmse_inner)
      }  # End inner folds loop
      
      # Track best parameters by lowest average inner RMSE
      avg_rmse <- mean(inner_rmse_list)
      if (avg_rmse < best_rmse) {
        best_rmse <- avg_rmse
        best_params <- hyper_grid_reg[params_idx, ]
      }
    }  # End hyperparameter grid loop
    
    cat(sprintf("Best hyperparameters for outer fold %d: eta = %f, max_depth = %d, nrounds = %d with avg RMSE = %.4f\n", 
                fold_idx, best_params$eta, best_params$max_depth, best_params$nrounds, best_rmse))
    
    # Train final model on full outer training set with best hyperparameters
    dtrain_outer <- xgb.DMatrix(data = as.matrix(outer_train_data[, predictors]), 
                                label = outer_train_data[[target_var]])
    dtest_outer <- xgb.DMatrix(data = as.matrix(outer_test_data[, predictors]), 
                               label = outer_test_data[[target_var]])
    
    final_model <- xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        eta = best_params$eta,
        max_depth = best_params$max_depth
      ),
      data = dtrain_outer,
      nrounds = best_params$nrounds,
      watchlist = list(train = dtrain_outer, eval = dtest_outer),
      early_stopping_rounds = 10,
      print_every_n = 10,
      verbose = 0
    )
    
    # Evaluate on outer test set: compute overall RMSE
    final_preds <- predict(final_model, dtest_outer)
    actual_outer <- outer_test_data[[target_var]]
    final_rmse <- sqrt(mean((actual_outer - final_preds)^2))
    
    # Calculate RMSE for the Upper 25% (Upper Quintile) of Actual Values
    upper_threshold <- quantile(actual_outer, 0.75, na.rm = TRUE)
    upper_indices <- which(actual_outer >= upper_threshold)
    upper_rmse <- sqrt(mean((actual_outer[upper_indices] - final_preds[upper_indices])^2))
    
    # Predict the mean resilience score for all students
    dumb_prediction <- mean(actual_outer, na.rm = TRUE)
    
    # Compute RMSE if we just used the mean for high-resilient students
    dumb_resilient_rmse <- sqrt(mean((actual_outer[upper_indices] - dumb_prediction)^2))
    
    # Compute R² (Coefficient of Determination)
    ss_res <- sum((actual_outer - final_preds)^2)
    ss_tot <- sum((actual_outer - mean(actual_outer))^2)
    r2 <- 1 - (ss_res / ss_tot)
    
    # Compute UpperR2 on the upper quantile subset
    upper_actual <- actual_outer[upper_indices]
    upper_preds  <- final_preds[upper_indices]
    upper_r2 <- 1 - (sum((upper_actual - upper_preds)^2) / sum((upper_actual - mean(upper_actual))^2))
    
    # Save the model and the performance for this fold.
    fold_results[[fold_idx]] <- list(
      model = final_model,
      rmse = final_rmse,
      upper_rmse = upper_rmse,
      dumb_resilient_rmse = dumb_resilient_rmse,
      r2 = r2,
      upper_r2 = upper_r2,
      best_params = best_params
    )
    # Save the model with 'cont_' prefix
    model_filename <- sprintf("models/cont_model_%s_%s_fold_%d.model", country, target_var, fold_idx)
    xgb.save(final_model, model_filename)
    
    # Save the fold results incrementally with 'cont_' prefix
    results_filename <- sprintf("models/cont_fold_results_%s_%s.rds",country, target_var)
    saveRDS(fold_results, file = results_filename)
    
    cat(sprintf("Saved model and results for %s (Fold %d, %s).\n", target_var, fold_idx,  country))
  }  
  
  return(fold_results)
}


################################################################################
#### Running the Continuous XGBoost Models for Malaysia and Norway
################################################################################

# Create 5 outer folds for each dataset.
folds_mys_cont <- create_folds_reg(mys_cont_data, k = 5)
folds_nor_cont <- create_folds_reg(nor_cont_data, k = 5)

# Train continuous models for each of the five resilience variables for Malaysia.
mys_cont_models <- list()
for (i in seq_along(resilience_vars_cont)) {
  target_var <- resilience_vars_cont[i]  
  cat(sprintf("\nTraining XGBoost continuous model for Malaysia: %s\n", target_var))
  models_result <- train_xgb_continuous(mys_cont_data, folds_mys_cont, predictors, target_var, "MYS")
  mys_cont_models[[target_var]] <- models_result
}

# Train continuous models for each of the five resilience variables for Norway.
nor_cont_models <- list()
for (i in seq_along(resilience_vars_cont)) {
  target_var <- resilience_vars_cont[i]
  cat(sprintf("\nTraining XGBoost continuous model for Norway: %s\n", target_var))
  models_result <- train_xgb_continuous(nor_cont_data, folds_nor_cont, predictors, target_var, "NOR")
  nor_cont_models[[target_var]] <- models_result
}
