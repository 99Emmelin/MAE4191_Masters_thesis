################################################################################
################################################################################
############                Descriptive analysis                    ############
################################################################################
################################################################################

# Purpose:
# Performs descriptive stats, plots, correlations, group comparisons, 
# and missing data summaries for Malaysia vs. Norway.

# required objects coming from 01_data_prep.R:
#   variables, predictors, short_names, descriptive_names 
#
# required objects coming from 02_feature_engineering.R:  
#   all_data_*_with_resilience, combined_data_*_final, mys_cat_data / nor_cat_data,  
#   science_vars, resilience_vars (cat) & resilience_vars_cont (cont)

################################################################################
############ univariate analysis
################################################################################

##### Malaysia: Summary statistics for resilience ################

for (science_var in science_vars) {
  resilience_var <- paste0("Resilience_", science_var)
  print(paste("Summary for", resilience_var))
  print(summary(all_data_mys_with_resilience[[resilience_var]]))
}

##### Norway: Summary statistics for resilience ################

for (science_var in science_vars) {
  resilience_var <- paste0("Resilience_", science_var)
  print(paste("Summary for", resilience_var))
  print(summary(all_data_nor_with_resilience[[resilience_var]]))
}

##### Malaysia Gender distribution ################

table(combined_data_mys_final$ITSEX) # Count for each gender
prop.table(table(combined_data_mys_final$ITSEX)) # Proportion for each gender

##### Norway Gender distribution ################

table(combined_data_nor_final$ITSEX)  
prop.table(table(combined_data_nor_final$ITSEX))

##### Distribution of resilient vs. non-resilient students ################

# Generate tables for Malaysia and Norway
resilience_table_mys <- generate_resilience_table(all_data_mys_with_resilience, resilience_vars)
resilience_table_nor <- generate_resilience_table(all_data_nor_with_resilience, resilience_vars)

##### Combined descriptive tables for Malaysia and Norway ################

# Subset the Norwegian and Malaysian datasets to relevant variables
subset_data_mys <- all_data_mys_with_resilience[, variables]

subset_data_nor <- all_data_nor_with_resilience[, variables]

# Combine descriptive statistics in a combined table for Malaysia and Norway
combined_stats <- data.frame(
  Variable = variables,
  Mean_MYS = round(sapply(subset_data_mys, mean, na.rm = TRUE), 2),
  Mean_NOR = round(sapply(subset_data_nor, mean, na.rm = TRUE), 2),
  Median_MYS = round(sapply(subset_data_mys, median, na.rm = TRUE), 2),
  Median_NOR = round(sapply(subset_data_nor, median, na.rm = TRUE), 2),
  Std_MYS = round(sapply(subset_data_mys, sd, na.rm = TRUE), 2),
  Std_NOR = round(sapply(subset_data_nor, sd, na.rm = TRUE), 2)
)

first_rv_cat  <- resilience_vars[1]        # "resilience_01"
first_rv_cont <- resilience_vars_cont[1]   # "Resilience_BSSSCI01"

# categorical correlations for Malaysia (bivariate)
corr_mys_cat <- sapply(predictors, function(v) avg_corr(all_data_mys_with_resilience, v))
# categorical correlations for Norway
corr_nor_cat <- sapply(predictors, function(v) avg_corr(all_data_nor_with_resilience, v))

# categorical p-values (bivariate)
p_mys_cat <- sapply(predictors, function(v) 
  avg_pval_cat(all_data_mys_with_resilience, v))
p_nor_cat <- sapply(predictors, function(v) 
  avg_pval_cat(all_data_nor_with_resilience, v))

# continuous correlations for Malaysia (bivariate)
corr_mys_cont <- sapply(predictors, function(v) avg_corr_cont(all_data_mys_with_resilience, v))
# continuous correlations for Norway
corr_nor_cont <- sapply(predictors, function(v) 
  avg_corr_cont(all_data_nor_with_resilience, v))

# continuous p-values
p_mys_cont <- sapply(predictors, function(v) 
  avg_pval_cont(all_data_mys_with_resilience, v))
p_nor_cont <- sapply(predictors, function(v) 
  avg_pval_cont(all_data_nor_with_resilience, v))

# Adds correlation results (value + significance) for selected predictors 
# across categorical and continuous resilience models in Malaysia and Norway
combined_stats$Corr_Cat_MYS  <- ifelse(
  combined_stats$Variable %in% predictors,
  paste0(round(corr_mys_cat,  2), sig(p_mys_cat)),
  NA_character_
)
combined_stats$Corr_Cat_NOR  <- ifelse(
  combined_stats$Variable %in% predictors,
  paste0(round(corr_nor_cat,  2), sig(p_nor_cat)),
  NA_character_
)
combined_stats$Corr_Cont_MYS <- ifelse(
  combined_stats$Variable %in% predictors,
  paste0(round(corr_mys_cont, 2), sig(p_mys_cont)),
  NA_character_
)
combined_stats$Corr_Cont_NOR <- ifelse(
  combined_stats$Variable %in% predictors,
  paste0(round(corr_nor_cont, 2), sig(p_nor_cont)),
  NA_character_
)

# Give variables readable names
combined_stats$Variable <- descriptive_names[combined_stats$Variable]

# Save the table as CSV
write.csv(combined_stats, "combined_descriptive_stats.csv", row.names = FALSE)


###########################################################################################
############ Malaysia and Norway: Histogram - Overall continuous resilience distribution
###########################################################################################

# Identify all plausible-value resilience columns and compute each student's 
# overall continuous resilience in Malasyia
mys_resilience_cols <- grep("^Resilience_BSSSCI", names(all_data_mys_with_resilience), value = TRUE)
all_data_mys_with_resilience$overall_continuous_resilience <- rowMeans(
  all_data_mys_with_resilience[, mys_resilience_cols],
  na.rm = TRUE
)

# Identify all plausible-value resilience columns and compute each student's
# overall continuous resilience in Norway
nor_resilience_cols <- grep("^Resilience_BSSSCI", names(all_data_nor_with_resilience), value = TRUE)
all_data_nor_with_resilience$overall_continuous_resilience <- rowMeans(
  all_data_nor_with_resilience[, nor_resilience_cols],
  na.rm = TRUE
)

# Determine common x-axis limits (and define own breaks)
x_range <- range(c(
  all_data_mys_with_resilience$overall_continuous_resilience,
  all_data_nor_with_resilience$overall_continuous_resilience
), na.rm = TRUE)

x_breaks <- seq(0, 1, 0.1)  

# Compute maximum frequency (y-axis limit)
common_breaks <- seq(x_range[1], x_range[2], length.out = 15)
hist_mys <- hist(all_data_mys_with_resilience$overall_continuous_resilience,
                 breaks = common_breaks, plot = FALSE)

hist_nor <- hist(all_data_nor_with_resilience$overall_continuous_resilience,
                 breaks = common_breaks, plot = FALSE)

y_max <- max(c(hist_mys$counts, hist_nor$counts))
y_breaks <- seq(0, y_max, by = 200)

# Create the ggplot histograms with custom scales and APA-style theme
p_mys <- ggplot(all_data_mys_with_resilience, aes(x = overall_continuous_resilience)) +
  geom_histogram(breaks = common_breaks, fill = "grey", color = "black") +
  scale_x_continuous(limits = c(min(x_breaks), max(x_breaks)), breaks = x_breaks) +
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks) +
  labs(title = "Overall Continuous Resilience in Malaysia",
       x = "Resilience Score",
       y = "Frequency") +
  theme_classic(base_size = 14)   

p_nor <- ggplot(all_data_nor_with_resilience, aes(x = overall_continuous_resilience)) +
  geom_histogram(breaks = common_breaks, fill = "grey", color = "black") +
  scale_x_continuous(limits = c(min(x_breaks), max(x_breaks)), breaks = x_breaks) +
  scale_y_continuous(limits = c(0, y_max), breaks = y_breaks) +
  labs(title = "Overall Continuous Resilience in Norway",
       x = "Resilience Score",
       y = "Frequency") +
  theme_classic(base_size = 14)   

# Arrange plots side by side
combined_plot <- grid.arrange(p_mys, p_nor, ncol = 2)

# Save the combined plot
ggsave("combined_resilience_histograms_ggplot.png", combined_plot,
       width = 12, height = 6, dpi = 300)


###############################################################################
#### Applying the Aggregated Comparison Functions to Data               
###############################################################################

##### Malaysia (Categorical) Comparison (all 20 predictors) ################
aggregated_cat_results_mys <- compare_categorical_resilience_aggregated(
  data = mys_cat_data,
  resilience_vars = resilience_vars,  
  predictors = predictors              
)

##### Norway (Categorical) Comparison (all 20 predictors) ################
aggregated_cat_results_nor <- compare_categorical_resilience_aggregated(
  data = nor_cat_data,
  resilience_vars = resilience_vars,
  predictors = predictors
)

# Apply Descriptive Names to Each Output Table
aggregated_cat_results_mys <- apply_descriptive_names(aggregated_cat_results_mys)
aggregated_cat_results_nor <- apply_descriptive_names(aggregated_cat_results_nor)

#### Save the Aggregated Comparison Results to CSV
write.csv(aggregated_cat_results_mys,  "aggregated_comparison_results_mys_cat.csv",  row.names = FALSE)
write.csv(aggregated_cat_results_nor,  "aggregated_comparison_results_nor_cat.csv",  row.names = FALSE)


###############################################################################
############ Bivariate analysis: Create correlation matrix 
###############################################################################

# Malaysia – correlation matrix for the 20 predictors
predictors_data <- all_data_mys_with_resilience[, predictors]
predictors_data <- as.data.frame(lapply(predictors_data, function(x) as.numeric(as.character(x))))
cor_matrix <- cor(predictors_data, use = "pairwise.complete.obs")

# Replace row and column names with SHORTER names
rownames(cor_matrix) <- short_names[rownames(cor_matrix)]
colnames(cor_matrix) <- short_names[colnames(cor_matrix)]

# Remove the upper triangle (set them to NA)
cor_matrix[upper.tri(cor_matrix, diag = FALSE)] <- NA

# Convert the matrix to a data frame and round the values
cor_df <- as.data.frame(round(cor_matrix, 2))

# Include the variable names as a column
cor_df$Variable <- rownames(cor_df)

# Place the Variable column at the front:
cor_df <- cor_df[, c(ncol(cor_df), 1:(ncol(cor_df)-1))]

# Convert the correlation matrix to a long-format data frame,
# keeping only the non-NA (lower triangle) values.
cor_long <- melt(cor_matrix, na.rm = TRUE)

# Create the ggplot heatmap with a neutral color palette
p <- ggplot(cor_long, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  # Use neutral grays: adjust hex codes as desired for lighter/darker tones
  scale_fill_gradient2(low = "red", mid = "white", high = "#2b8cbe",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix (Malaysia)", x = "", y = "")

# Save the plot to a file
ggsave("correlation_matrix_plot_MYS.png", plot = p, width = 8, height = 6, dpi = 300)

# Norway – correlation matrix for the 20 predictors
# Build a clean numeric predictor frame
predictors_data_nor <- all_data_nor_with_resilience[, predictors] |>
  as.data.frame() |>
  lapply(function(x) as.numeric(as.character(x))) |>
  as.data.frame()

# Correlations (pairwise NAs)
cor_matrix_nor <- cor(predictors_data_nor, use = "pairwise.complete.obs")

# Apply shorter row/col labels
rownames(cor_matrix_nor) <- short_names[rownames(cor_matrix_nor)]
colnames(cor_matrix_nor) <- short_names[colnames(cor_matrix_nor)]

# Keep only lower triangle for display
cor_matrix_nor[upper.tri(cor_matrix_nor, diag = FALSE)] <- NA

# Long format for ggplot
cor_long_nor <- reshape2::melt(cor_matrix_nor, na.rm = TRUE)

# Heat-map
p_cor_nor <- ggplot(cor_long_nor, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "#2b8cbe",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), colour = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Correlation Matrix (Norway)", x = "", y = "")

# Save
ggsave("correlation_matrix_plot_NOR.png",
       plot   = p_cor_nor,
       width  = 8, height = 6, dpi = 300)


################################################################################
############ Missing data diagnostics in Malaysia and Norway
################################################################################

# Missing data before excluding SES and science achievement observation containing NA for inspection
missing_data_mys <- summarize_missingness(combined_data_mys_final)
missing_data_nor <- summarize_missingness(combined_data_nor_final)

# Apply to Malaysia and Norway datasets after excluding SES and science achievement 
# observation containing NA
missing_summary_mys <- summarize_missingness(all_data_mys_with_resilience)
missing_summary_nor <- summarize_missingness(all_data_nor_with_resilience)

# Apply descriptive names to summaries
missing_data_mys <- replace_with_descriptive_names(missing_data_mys, descriptive_names)
missing_data_nor <- replace_with_descriptive_names(missing_data_nor, descriptive_names)

# Save the summaries as CSV files
write.csv(missing_data_mys, "missing_summary_malaysia.csv", row.names = FALSE)
write.csv(missing_data_nor, "missing_summary_norway.csv", row.names = FALSE)
