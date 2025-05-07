################################################################################
################################################################################
##########    XGBoost models feature-importance & performance plots    #########
################################################################################
################################################################################

# Purpose:
#   draw grouped-bar importance plots, pooled ROC (categorical) and
#   REC (continuous) curves for Malaysia vs Norway.

# Required objects from 01_data_prep.R:
#   predictors , short_names
#
# Required objects from 04_modeling_categorical.R / 05_modeling_continuous.R
#   aggregated_importance_* , mys_*_models , nor_*_models,
#                     folds_* , mys_cat_data / nor_cat_data , mys_cont_data /
#                     nor_cont_data,  resilience_vars / resilience_vars_cont

#############################################################################
#### XGBoost Final Models Feature Importance Visualization preparations
#############################################################################

##### Grouped Feature Importance ################

# Define Factor Grouping
factor_group <- data.frame(
  Feature = c(
    "Instructional time in science", "Percentage of students taught biology topics", 
    "Percentage of students taught physical sci topics", "Percentage of students taught earth science topics", 
    "Instructional clarity in science lessons", "Teachers emphasis on science investigation", 
    "Experiments in science lessons", "Instruction affected by science resource shortages", 
    "School emphasis on academic success", "School discipline", 
    "Teaching limited by students not ready for instruction",
    "Students like learning science", "Students confident in science", 
    "Students value science", "Students sense of school belonging", 
    "Student absenteeism", "Socioeconomic background of the student body",
    "Student speak the language of test", "Students native language is the language of the test", 
    "Gender"
  ),
  Factor = c(rep("Opportunity", 11), rep("Propensity", 5), rep("Antecedent", 4))
)

# Apply Grouping to All Datasets
aggregated_importance_mys_cat <- merge_factor_group(aggregated_importance_mys_cat)
aggregated_importance_nor_cat <- merge_factor_group(aggregated_importance_nor_cat)
aggregated_importance_mys_cont <- merge_factor_group(aggregated_importance_mys_cont)
aggregated_importance_nor_cont <- merge_factor_group(aggregated_importance_nor_cont)

# Define Professional, Muted Colors for Each Group (used for both types)
factor_colors <- c(
  "Opportunity" = "#0072B2",  # blue
  "Propensity"  = "#D55E00",  # vermillion
  "Antecedent"  = "#009E73"   # bluish green
)

# Find max Mean_Gain for categorical and continuous plots
max_gain_cat <- max(
  c(aggregated_importance_mys_cat$Mean_Gain,
    aggregated_importance_nor_cat$Mean_Gain),
  na.rm = TRUE
)

max_gain_cont <- max(
  c(aggregated_importance_mys_cont$Mean_Gain,
    aggregated_importance_nor_cont$Mean_Gain),
  na.rm = TRUE
)

#############################################################################
# Generate Plots for Categorical 
#############################################################################

plot_cat_mys <- create_grouped_importance_plot(
  aggregated_importance_mys_cat,
  "Malaysia: Categorical Feature Importance",
  max_gain_cat
)

plot_cat_nor <- create_grouped_importance_plot(
  aggregated_importance_nor_cat,
  "Norway: Categorical Feature Importance",
  max_gain_cat
)

#############################################################################
# Generate Plots for Continuous 
#############################################################################

plot_cont_mys <- create_grouped_importance_plot(
  aggregated_importance_mys_cont,
  "Malaysia: Continuous Feature Importance",
  max_gain_cont
)

plot_cont_nor <- create_grouped_importance_plot(
  aggregated_importance_nor_cont,
  "Norway: Continuous Feature Importance",
  max_gain_cont
)

#############################################################################
# Combine & Save Categorical Plots Side-by-Side with Single Legend
#############################################################################

legend_cat <- g_legend(plot_cat_mys)
plot_cat_mys_no_leg <- plot_cat_mys + theme(legend.position = "none")
plot_cat_nor_no_leg <- plot_cat_nor + theme(legend.position = "none")

combined_cat <- arrangeGrob(
  arrangeGrob(plot_cat_mys_no_leg, plot_cat_nor_no_leg, ncol = 2),
  legend_cat,
  ncol = 1,
  heights = c(10, 1)
)

ggsave(
  filename = "feature_importance_categorical_gray.png",
  plot     = combined_cat,
  width    = 18,
  height   = 12,
  dpi      = 300,
  limitsize = FALSE
)

#############################################################################
# Combine & Save Continuous Plots Side-by-Side with Single Legend
#############################################################################

legend_cont <- g_legend(plot_cont_mys)
plot_cont_mys_no_leg <- plot_cont_mys + theme(legend.position = "none")
plot_cont_nor_no_leg <- plot_cont_nor + theme(legend.position = "none")

combined_cont <- arrangeGrob(
  arrangeGrob(plot_cont_mys_no_leg, plot_cont_nor_no_leg, ncol = 2),
  legend_cont,
  ncol = 1,
  heights = c(10, 1)
)

ggsave(
  filename = "feature_importance_continuous_gray.png",
  plot     = combined_cont,
  width    = 18,
  height   = 12,
  dpi      = 300,
  limitsize = FALSE
)

################################################################################
####### ROC Curve Plotting 
################################################################################

##### Malaysia (Categorical) ROC plot ################

plot_roc_per_resilience(
  models_list   = mys_cat_models,   
  data          = mys_cat_data,     
  folds_list    = folds_mys,        
  predictors    = predictors,
  resilience_vars = resilience_vars, 
  main_title    ="ROC Curve for Malaysia (Categorical)",
  save_path     = "mys_categorical_roc.png",
  line_color    = "#0072B2"         
)

##### Norway (Categorical) ROC plot ################
plot_roc_per_resilience(
  models_list = nor_cat_models, 
  data = nor_cat_data,
  folds_list = folds_nor, 
  predictors = predictors,
  resilience_vars = resilience_vars,
  main_title = "ROC Curves for Norway (Categorical)",
  save_path = "nor_categorical_roc.png",
  line_color = "#D55E00"
)


################################################################################
####### REC Curve Plotting
################################################################################

##### Malaysia (Continuous) REC plot ################

plot_rec_curves(
  models_list = mys_cont_models, 
  data = mys_cont_data,
  folds_list = folds_mys_cont, 
  predictors = predictors,
  target_vars = resilience_vars_cont,
  main_title = "REC Curves for Malaysia (Continuous)",
  save_path = "mys_continuous_rec.png",
  line_color = "#0072B2"
)

##### Norway (Continuous) REC plot ################

plot_rec_curves(
  models_list = nor_cont_models,    
  data        = nor_cont_data,       
  folds_list  = folds_nor_cont,      
  predictors  = predictors,
  target_vars = resilience_vars_cont,
  main_title  = "REC Curves for Norway (Continuous)",
  save_path   = "nor_continuous_rec.png",
  line_color  = "#D55E00"          
)
