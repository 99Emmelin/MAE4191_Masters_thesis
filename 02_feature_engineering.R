################################################################################
################################################################################
############                Feature engineering                     ############
################################################################################
################################################################################

# Purpose:
# Builds all resilience outcomes (5 categorical + 5 continuous), coerces factors 
# to numeric, normalizes SES/achievement, and exports the ready‐to-model data 
# frames for Malaysia and Norway.

# required objects coming from 01_data_prep.R:
#   combined_data_mys_final / combined_data_nor_final, science_vars, predictors,
#   cols_to_convert, resilience_vars, resilience_vars_cont

################################################################################
############ Create Categorical defined resilience:
############ Lowest 25th percentile SES and top 25th percentile of Achievement
################################################################################

##### Convert variables Malaysia and Norway for resilience definition ################

# Malaysia: Convert BSBGHER (SES) variable from character to numeric
combined_data_mys_final$BSBGHER <- as.numeric(as.character(combined_data_mys_final$BSBGHER))

# Norway: Convert BSBGHER (SES) variable from character to numeric
combined_data_nor_final$BSBGHER <- as.numeric(as.character(combined_data_nor_final$BSBGHER))

# Malaysia: Convert all science achievement variables from character to numeric
combined_data_mys_final <- combined_data_mys_final %>% 
  mutate(across(all_of(science_vars), ~ as.numeric(as.character(.)), .names = "{col}")) 

# Norway: Convert all science achievement variables from character to numeric
combined_data_nor_final <- combined_data_nor_final %>% 
  mutate(across(all_of(science_vars), ~ as.numeric(as.character(.)), .names = "{col}"))


##### Malaysia: Categorical defined resilience ################

# SES 25 percentile lowest students in Malaysia by SES variable
ses_threshold_mys <- quantile(combined_data_mys_final$BSBGHER, 0.25, na.rm = TRUE)

# Select students below the 25th percentile of SES
bottom_25_mys_ses <- combined_data_mys_final %>%
  filter(BSBGHER <= ses_threshold_mys) %>%
  select(IDSTUD, BSBGHER)

# Create resilience variables for each plausible achievement
resilience_variables_mys <- list()

# Loop through each plausible achievement variable (BSSSCI01 to BSSSCI05)
for (i in 1:5) {
  # Dynamically generate the name for the achievement variable
  achievement_var <- paste("BSSSCI", sprintf("%02d", i), sep = "")
  
  # Calculate the 75th percentile threshold for science achievement
  science_threshold_mys <- quantile(combined_data_mys_final[[achievement_var]], 0.75, na.rm = TRUE)
  
  # Select students above the 75th percentile of the current science achievement variable
  top_25_science_mys <- combined_data_mys_final %>%
    filter(combined_data_mys_final[[achievement_var]] >= science_threshold_mys) %>%
    select(IDSTUD, !!sym(achievement_var))
  
  # Inner join to find resilient students based on this achievement variable
  resilient_students_mys_P <- inner_join(top_25_science_mys, bottom_25_mys_ses, by = "IDSTUD")
  
  # Label resilient students
  resilient_students_mys_P$resilience <- 1
  
  # Identify non-resilient students among the low SES group
  non_resilient_students_mys_p <- bottom_25_mys_ses %>%
    filter(!IDSTUD %in% resilient_students_mys_P$IDSTUD)
  non_resilient_students_mys_p$resilience <- 0
  
  # Combine resilient and non-resilient data
  combined_data_mys_p <- bind_rows(resilient_students_mys_P, non_resilient_students_mys_p)
  
  # Add the resilience column to the main data
  all_data_mys_with_resilience <- combined_data_mys_final %>%
    left_join(combined_data_mys_p %>% select(IDSTUD, resilience), by = "IDSTUD")
  
  # Store the resilience variable for each plausible achievement
  resilience_variables_mys[[achievement_var]] <- all_data_mys_with_resilience$resilience
}

# Assign the resilience variables for each plausible achievement to the main data
for (i in 1:5) {
  # Dynamically generate the name for each resilience variable
  resilience_var_name <- paste("resilience_", sprintf("%02d", i), sep = "")
  all_data_mys_with_resilience[[resilience_var_name]] <- resilience_variables_mys[[paste("BSSSCI", sprintf("%02d", i), sep = "")]]
}


##### Norway: Categorical defined resilience ################

# SES 25 percentile threshold for SES
ses_threshold_nor <- quantile(combined_data_nor_final$BSBGHER, 0.25, na.rm = TRUE)

# Select students below the 25th percentile of SES
bottom_25_nor_ses <- combined_data_nor_final %>%
  filter(BSBGHER <= ses_threshold_nor) %>%
  select(IDSTUD, BSBGHER)

# Create resilience variables for each plausible achievement
resilience_variables_nor <- list()

# Loop through each plausible achievement variable (BSSSCI01 to BSSSCI05)
for (i in 1:5) {
  # Dynamically generate the name for the achievement variable
  achievement_var <- paste("BSSSCI", sprintf("%02d", i), sep = "")
  
  # Calculate the 75th percentile threshold for science achievement
  science_threshold_nor <- quantile(combined_data_nor_final[[achievement_var]], 0.75, na.rm = TRUE)
  
  # Select students above the 75th percentile of the current science achievement variable
  top_25_science_nor <- combined_data_nor_final %>%
    filter(combined_data_nor_final[[achievement_var]] >= science_threshold_nor) %>%
    select(IDSTUD, !!sym(achievement_var))
  
  # Inner join to find resilient students based on this achievement variable
  resilient_students_nor_P <- inner_join(top_25_science_nor, bottom_25_nor_ses, by = "IDSTUD")
  
  # Label resilient students
  resilient_students_nor_P$resilience <- 1
  
  # Identify non-resilient students among the low SES group
  non_resilient_students_nor_p <- bottom_25_nor_ses %>%
    filter(!IDSTUD %in% resilient_students_nor_P$IDSTUD)
  non_resilient_students_nor_p$resilience <- 0
  
  # Combine resilient and non-resilient data
  combined_data_nor_p <- bind_rows(resilient_students_nor_P, non_resilient_students_nor_p)
  
  # Add the resilience column to the main data
  all_data_nor_with_resilience <- combined_data_nor_final %>%
    left_join(combined_data_nor_p %>% select(IDSTUD, resilience), by = "IDSTUD")
  
  # Store the resilience variable for each plausible achievement
  resilience_variables_nor[[achievement_var]] <- all_data_nor_with_resilience$resilience
}

# Assign the resilience variables for each plausible achievement to the main data
for (i in 1:5) {
  # Dynamically generate the name for each resilience variable
  resilience_var_name <- paste("resilience_", sprintf("%02d", i), sep = "")
  all_data_nor_with_resilience[[resilience_var_name]] <- resilience_variables_nor[[paste("BSSSCI", sprintf("%02d", i), sep = "")]]
}


##### Change NAs in resilience to 0 (non-resilient) ################
all_data_mys_with_resilience <- all_data_mys_with_resilience %>%
  mutate(across(starts_with("resilience_"), 
                ~ ifelse(is.na(.) & !is.na(BSBGHER) & BSBGHER > ses_threshold_mys, 0, .)))

all_data_nor_with_resilience <- all_data_nor_with_resilience %>%
  mutate(across(starts_with("resilience_"), 
                ~ ifelse(is.na(.) & !is.na(BSBGHER) & BSBGHER > ses_threshold_nor, 0, .)))

all_data_mys_with_resilience <- all_data_mys_with_resilience %>%
  filter(complete.cases(select(., starts_with("resilience_"))))

all_data_nor_with_resilience <- all_data_nor_with_resilience %>%
  filter(complete.cases(select(., starts_with("resilience_"))))


################################################################################
############ Create continuous defined resilience: 
############ Using normalized SES and Science achievement
################################################################################

##### Malaysia: continuous defined resilience ################

# Normalize and invert SES (BSBGHER) so that low SES becomes a high adversity value
all_data_mys_with_resilience$Normalized_SES <- 1 - (
  (all_data_mys_with_resilience$BSBGHER - min(all_data_mys_with_resilience$BSBGHER, na.rm = TRUE)) /
    (max(all_data_mys_with_resilience$BSBGHER, na.rm = TRUE) - min(all_data_mys_with_resilience$BSBGHER, na.rm = TRUE))
)

# Loop through each plausible value and calculate resilience
for (i in seq_along(science_vars)) { 
  # Get the current plausible value
  science_var <- science_vars[i]
  
  # Normalize the current plausible value
  all_data_mys_with_resilience[[paste0("Normalized_", science_var)]] <- 
    (all_data_mys_with_resilience[[science_var]] - min(all_data_mys_with_resilience[[science_var]], na.rm = TRUE)) / 
    (max(all_data_mys_with_resilience[[science_var]], na.rm = TRUE) - min(all_data_mys_with_resilience[[science_var]], na.rm = TRUE))
  
  # Calculate resilience for the current plausible value
  resilience_var <- paste0("Resilience_", science_var)
  all_data_mys_with_resilience[[resilience_var]] <- 
    all_data_mys_with_resilience$Normalized_SES * all_data_mys_with_resilience[[paste0("Normalized_", science_var)]]
}


##### Norway: continuous defined resilience ################

# Normalize and invert SES (BSBGHER) for Norway
all_data_nor_with_resilience$Normalized_SES <- 1 - (
  (all_data_nor_with_resilience$BSBGHER - min(all_data_nor_with_resilience$BSBGHER, na.rm = TRUE)) /
    (max(all_data_nor_with_resilience$BSBGHER, na.rm = TRUE) - min(all_data_nor_with_resilience$BSBGHER, na.rm = TRUE))
)

# Loop through each plausible value and calculate resilience
for (i in seq_along(science_vars)) {
  science_var <- science_vars[i]
  
  # Normalize the current plausible value
  all_data_nor_with_resilience[[paste0("Normalized_", science_var)]] <- 
    (all_data_nor_with_resilience[[science_var]] - min(all_data_nor_with_resilience[[science_var]], na.rm = TRUE)) / 
    (max(all_data_nor_with_resilience[[science_var]], na.rm = TRUE) - min(all_data_nor_with_resilience[[science_var]], na.rm = TRUE))
  
  # Calculate resilience for the current plausible value
  resilience_var <- paste0("Resilience_", science_var)
  all_data_nor_with_resilience[[resilience_var]] <- 
    all_data_nor_with_resilience$Normalized_SES * all_data_nor_with_resilience[[paste0("Normalized_", science_var)]]
}


################################################################################
############ Prepare the variables for XGBoost - Malaysia Data
################################################################################

# Convert the listed factor/character columns to numeric 
all_data_mys_with_resilience[cols_to_convert] <- lapply(all_data_mys_with_resilience[cols_to_convert], function(x) as.numeric(as.character(x)))


# Map "Female" to 0 and "Male" to 1 in ITSEX
all_data_mys_with_resilience$ITSEX <- as.numeric(factor(all_data_mys_with_resilience$ITSEX, levels = c("Female", "Male"), labels = c(0, 1)))

# BCBG04
all_data_mys_with_resilience$BCBG04 <- as.numeric(factor(
  all_data_mys_with_resilience$BCBG04,
  levels = c("25% or less", "26 to 50%", "51 to 75%", "76 to 90%", "More than 90%"),
  labels = c(1, 2, 3, 4, 5)
))

# BCDGSBC
all_data_mys_with_resilience$BCDGSBC <- as.numeric(factor(
  all_data_mys_with_resilience$BCDGSBC,
  levels = c("More Disadvantaged", "Neither More Affluent nor More Disadvantaged", "More Affluent"),
  labels = c(1, 2, 3)
))

# BSBG10
all_data_mys_with_resilience$BSBG10 <- as.numeric(factor(
  all_data_mys_with_resilience$BSBG10,
  levels = c("Once a week", "Once every two weeks", "Once a month", "Once every two month", "Never or almost never"),
  labels = c(5, 4, 3, 2, 1)
))

# BSBS21
all_data_mys_with_resilience$BSBS21 <- as.numeric(factor(
  all_data_mys_with_resilience$BSBS21,
  levels = c("At least once a week", "Once or twice a month", "A few times a year", "Never"),
  labels = c(4, 3, 2, 1)
))

# BSBG03
all_data_mys_with_resilience$BSBG03 <- as.numeric(factor(
  all_data_mys_with_resilience$BSBG03,
  levels = c("Always", "Almost always", "Sometimes", "Never"),
  labels = c(4, 3, 2, 1)
))


#############################################################################
#### Prepare the variables for XGBoost - Norway Data
#############################################################################

# Convert the listed factor/character columns to numeric
all_data_nor_with_resilience[cols_to_convert] <- lapply(all_data_nor_with_resilience[cols_to_convert], function(x) as.numeric(as.character(x)))

# Map "Female" to 0 and "Male" to 1 in ITSEX
all_data_nor_with_resilience$ITSEX <- as.numeric(factor(all_data_nor_with_resilience$ITSEX, levels = c("Female", "Male"), labels = c(0, 1)))

# BCBG04
all_data_nor_with_resilience$BCBG04 <- as.numeric(factor(
  all_data_nor_with_resilience$BCBG04,
  levels = c("25% or less", "26 to 50%", "51 to 75%", "76 to 90%", "More than 90%"),
  labels = c(1, 2, 3, 4, 5)
))

# BCDGSBC
all_data_nor_with_resilience$BCDGSBC <- as.numeric(factor(
  all_data_nor_with_resilience$BCDGSBC,
  levels = c("More Disadvantaged", "Neither More Affluent nor More Disadvantaged", "More Affluent"),
  labels = c(1, 2, 3)
))

# BSBG10
all_data_nor_with_resilience$BSBG10 <- as.numeric(factor(
  all_data_nor_with_resilience$BSBG10,
  levels = c("Once a week", "Once every two weeks", "Once a month", "Once every two month", "Never or almost never"),
  labels = c(5, 4, 3, 2, 1)
))

# BSBS21
all_data_nor_with_resilience$BSBS21 <- as.numeric(factor(
  all_data_nor_with_resilience$BSBS21,
  levels = c("At least once a week", "Once or twice a month", "A few times a year", "Never"),
  labels = c(4, 3, 2, 1)
))

# BSBG03
all_data_nor_with_resilience$BSBG03 <- as.numeric(factor(
  all_data_nor_with_resilience$BSBG03,
  levels = c("Always", "Almost always", "Sometimes", "Never"),
  labels = c(4, 3, 2, 1)
))

#############################################################################
#### Create Categorical datasets 
#############################################################################

# Subset predictors and target variable Malaysia 
mys_cat_data <- all_data_mys_with_resilience[, c("IDSTUD", predictors, 
                                                 resilience_vars)]

# Subset predictors and target variable Norway 
nor_cat_data <- all_data_nor_with_resilience[, c("IDSTUD", predictors, 
                                                 resilience_vars)]


#############################################################################
#### Create Continuous datasets
#############################################################################

# Subset predictors and target variable Malaysia 
mys_cont_data <- all_data_mys_with_resilience[, c("IDSTUD", predictors, 
                                                  resilience_vars_cont)]

# Subset predictors and target variable Norway
nor_cont_data <- all_data_nor_with_resilience[, c("IDSTUD", predictors, 
                                                  resilience_vars_cont)]
