################################################################################
################################################################################
############   XGBoost results summary & feature importance         ############
################################################################################
################################################################################

# Purpose:
# Categorical & Continuous Summaries for XGBoost Final models (performance metrics, 
# pooled Confusion Metrics, importance) for Norway and Malaysia

# Required objects from 01_data_prep.R:
#   descriptive_names, resilience_vars, resilience_vars_cont                  
#
# Required objects from 04_modeling_categorical.R:
#   mys_cat_models, nor_cat_models      
#
# Required objects from 05_modeling_continuous.R:
#   mys_cont_models, nor_cont_models

#############################################################################
#### Categorical & Continuous Summaries for XGBoost Final models
#############################################################################

# Generate summaries for Malaysia and Norway
mys_cat_summary <- summarize_categorical(mys_cat_models, resilience_vars)
nor_cat_summary <- summarize_categorical(nor_cat_models, resilience_vars)

mys_cont_summary <- summarize_continuous(mys_cont_models, resilience_vars_cont)
nor_cont_summary <- summarize_continuous(nor_cont_models, resilience_vars_cont)

# Merge side‑by‑side and save
cat_comparison <- merge(
  mys_cat_summary, nor_cat_summary,
  by = "ResilienceVar",
  suffixes = c("_MYS", "_NOR")
)
cont_comparison <- merge(
  mys_cont_summary, nor_cont_summary,
  by = "ResilienceVar",
  suffixes = c("_MYS", "_NOR")
)


# Compute and append an “Average” row across resilience vars for categorical metrics
cat_avg_row <- data.frame(
  ResilienceVar = "Average",
  t(round(colMeans(cat_comparison[ , -1], na.rm = TRUE), 3)),
  stringsAsFactors = FALSE
)
cat_comparison <- rbind(cat_comparison, cat_avg_row)

# Compute and append an “Average” row across resilience vars for continuous metrics 
cont_avg_row <- data.frame(
  ResilienceVar = "Average",
  t(round(colMeans(cont_comparison[ , -1], na.rm = TRUE), 3)),
  stringsAsFactors = FALSE
)
cont_comparison <- rbind(cont_comparison, cont_avg_row)

# Build each country’s continuous summary, tag with Country, stack them
cont_all <- bind_rows(
  Malaysia = summarize_continuous(mys_cont_models, resilience_vars_cont)[-1],
  Norway   = summarize_continuous(nor_cont_models, resilience_vars_cont)[-1],
  .id      = "Country"
)

# nNw group and take means (rounded to 3 dp) of all columns except Country
final_country_summary <- cont_all %>%
  group_by(Country) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm=TRUE), 3))) %>%
  ungroup()

# Stack Malaysia and Norway cat summaries, dropping the ResilienceVar column
cat_all <- bind_rows(
  Malaysia = summarize_categorical(mys_cat_models, resilience_vars)[-1],
  Norway   = summarize_categorical(nor_cat_models, resilience_vars)[-1],
  .id      = "Country"
)

# For each country, average every metric column (rounding to 3 dp)
final_cat_summary <- cat_all %>%
  group_by(Country) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 3))) %>%
  ungroup()

# Write out with the new summary row
write.csv(final_country_summary, "final_country_summary.csv", row.names = FALSE)
write.csv(final_cat_summary, "final_country_cat_summary.csv", row.names = FALSE)


#############################################################################
#### Confusion matrices
#############################################################################

# Pool confusion matrices across folds for each categorical model (Malaysia)
pooled_cms_mys <- setNames(
  lapply(mys_cat_models, pool_cm),
  resilience_vars
)

# Pool confusion matrices across folds for each categorical model (Norway)
pooled_cms_nor <- setNames(
  lapply(nor_cat_models, pool_cm),
  resilience_vars
)

# Print pooled confusion matrices for each resilience variable
for (var in resilience_vars) {
  cat("\n=== Variable:", var, "===\n")
  cat(" Malaysia:\n"); print(pooled_cms_mys[[var]])
  cat(" Norway:  \n"); print(pooled_cms_nor[[var]])
}

# Randomly select one resilience variable (for downstream use or illustration) 
set.seed(42)                  
chosen_var <- sample(resilience_vars, 1)

# Extract the two pooled CMs
cm_mys <- pooled_cms_mys[[chosen_var]]
cm_nor <- pooled_cms_nor[[chosen_var]]

# Build per‑country tables
csv_mys <- data.frame(
  country = "Malaysia",
  TN = cm_mys["0","0"],
  FP = cm_mys["0","1"],
  FN = cm_mys["1","0"],
  TP = cm_mys["1","1"]
)

csv_nor <- data.frame(
  country = "Norway",
  TN = cm_nor["0","0"],
  FP = cm_nor["0","1"],
  FN = cm_nor["1","0"],
  TP = cm_nor["1","1"]
)

# Write each to its own CSV
write.csv(csv_mys, "confusion_matrix_Malaysia.csv", row.names = FALSE)
write.csv(csv_nor, "confusion_matrix_Norway.csv", row.names = FALSE)

#############################################################################
#### XGBoost Final models feature importance
#############################################################################

##### Final models feature importance - Norway Categorical ################

importance_nor_cat <- lapply(seq_along(nor_cat_models), function(i) {
  fold_importances <- lapply(nor_cat_models[[i]], function(one_fold) {
    importance_df <- xgb.importance(model = one_fold$model)
    importance_df$Resilience_Var <- resilience_vars[i]
    return(importance_df)
  })
  # Combine importances across the 5 folds for the same resilience variable
  do.call(rbind, fold_importances)
})

# Combine all resilience variables into one data frame
importance_nor_cat_combined <- do.call(rbind, importance_nor_cat)


##### Final models feature importance - Malaysia Categorical ################

importance_mys_cat <- lapply(seq_along(mys_cat_models), function(i) {
  fold_importances <- lapply(mys_cat_models[[i]], function(one_fold) {
    importance_df <- xgb.importance(model = one_fold$model)
    importance_df$Resilience_Var <- resilience_vars[i]
    importance_df
  })
  do.call(rbind, fold_importances)
})

importance_mys_cat_combined <- do.call(rbind, importance_mys_cat)

##### Final models feature importance - Malaysia Continuous ################

importance_mys_cont <- lapply(seq_along(mys_cont_models), function(i) {
  fold_importances <- lapply(mys_cont_models[[i]], function(one_fold) {
    importance_df <- xgb.importance(model = one_fold$model)
    importance_df$Resilience_Var <- resilience_vars_cont[i]
    importance_df
  })
  do.call(rbind, fold_importances)
})

importance_mys_cont_combined <- do.call(rbind, importance_mys_cont)

##### Final models feature importance - Norway Continuous ################

importance_nor_cont <- lapply(seq_along(nor_cont_models), function(i) {
  fold_importances <- lapply(nor_cont_models[[i]], function(one_fold) {
    importance_df <- xgb.importance(model = one_fold$model)
    importance_df$Resilience_Var <- resilience_vars_cont[i]
    importance_df
  })
  do.call(rbind, fold_importances)
})

importance_nor_cont_combined <- do.call(rbind, importance_nor_cont)

##### Summarize Feature Importance ################

# Aggregated results for Malaysia and Norway
aggregated_importance_mys_cat <- aggregate_importance(importance_mys_cat_combined)
aggregated_importance_mys_cont <- aggregate_importance(importance_mys_cont_combined)
aggregated_importance_nor_cat  <- aggregate_importance(importance_nor_cat_combined)
aggregated_importance_nor_cont <- aggregate_importance(importance_nor_cont_combined)

# Ensure descriptive names are applied
aggregated_importance_mys_cat$Feature <- descriptive_names[aggregated_importance_mys_cat$Feature]
aggregated_importance_nor_cat$Feature <- descriptive_names[aggregated_importance_nor_cat$Feature]
aggregated_importance_mys_cont$Feature <- descriptive_names[aggregated_importance_mys_cont$Feature]
aggregated_importance_nor_cont$Feature <- descriptive_names[aggregated_importance_nor_cont$Feature]
