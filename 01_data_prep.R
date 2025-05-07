################################################################################
################################################################################
############                Preparing data                          ############
################################################################################
################################################################################

# Purpose:
# Imports and merges TIMSS 2019 SPSS files for Malaysia and Norway, 
# selects relevant variables, and sets up required vectors.

################################################################################
############ Load data
################################################################################

# TIMSS 2019 data is publicly available at:
# https://timss2019.org/international-database/


# Set the folder path where you saved the downloaded .sav files.
# It only needs be changed here — it will be reused throughout the script.
#data_dir <- "C:/Users/PC/Documents/MAE4191/data/"

##### Load Malaysia TIMSS 2019 data ################

# Student data
timss_data_mys_stud <- read.spss(file.path(data_dir, "M7MYS", "bsgmysm7.sav"),
                                 to.data.frame = TRUE)

# School data
timss_data_mys_sch  <- read.spss(file.path(data_dir, "M7MYS", "bcgmysm7.sav"),
                                 to.data.frame = TRUE)

# Teacher data
timss_data_mys_teac <- read.spss(file.path(data_dir, "M7MYS", "btsmysm7.sav"),
                                 to.data.frame = TRUE)


##### Load Norway TIMSS 2019 data ################

# Student data
timss_data_nor_stud <- read.spss(file.path(data_dir, "M7NOR", "bsgnorm7.sav"),
                                 to.data.frame = TRUE)

# School data
timss_data_nor_sch  <- read.spss(file.path(data_dir, "M7NOR", "bcgnorm7.sav"),
                                 to.data.frame = TRUE)

# Teacher data
timss_data_nor_teac <- read.spss(file.path(data_dir, "M7NOR", "btsnorm7.sav"),
                                 to.data.frame = TRUE)


################################################################################
############ Select relevant varibles
################################################################################

##### Malaysia relevant variables selection ################

## Get variables (20 factors + ID indicators) Malaysia 
selected_vars_mys_stud <- timss_data_mys_stud[,c("IDBOOK","IDSCHOOL", "IDCLASS", "IDSTUD", "ITSEX", "BSBGICS", "BSBS21", "BSBGSLS", 
                                                 "BSBGSCS", "BSBGSVS", "BSBGSSB", 
                                                 "BSBG10", "BSBGHER", "BSBG03",
                                                 "BSSSCI01","BSSSCI02", "BSSSCI03", "BSSSCI04","BSSSCI05")]

selected_vars_mys_sch <- timss_data_mys_sch[,c("IDSCHOOL","BCBGSRS","BCDGSBC", "BCBGEAS","BCBGDAS", "BCBG04")]

selected_vars_mys_teac <- timss_data_mys_teac[,c("IDSCHOOL","IDTEACH", "BTBS14","BTDSBIO", "BTDSPHY", "BTDSEAR","BTBSESI","BTBGLSN")]


##### Norway relevant variables selection ################

# Get variables (20 factors + ID indicators) Norway
selected_vars_nor_stud <- timss_data_nor_stud[,c("IDBOOK","IDSCHOOL", "IDCLASS", "IDSTUD","ITSEX", "BSBGICS", "BSBS21", "BSBGSLS", 
                                                 "BSBGSCS", "BSBGSVS", "BSBGSSB", 
                                                 "BSBG10", "BSBGHER", "BSBG03",
                                                 "BSSSCI01","BSSSCI02", "BSSSCI03", "BSSSCI04","BSSSCI05")]

selected_vars_nor_sch <- timss_data_nor_sch[,c("IDSCHOOL", "BCBGSRS","BCDGSBC", "BCBGEAS","BCBGDAS", "BCBG04")]

selected_vars_nor_teac <- timss_data_nor_teac[,c("IDSCHOOL","IDTEACH","BTBS14","BTDSBIO", "BTDSPHY", "BTDSEAR","BTBSESI","BTBGLSN")]


################################################################################
############ Merging datasets
################################################################################

##### Malaysia merging datasets ################

# Merge student data (selected_vars_mys_stud) with school data (selected_vars_mys_sch) using the common key 'IDSCHOOL'
combined_data_mys <- merge(selected_vars_mys_stud, selected_vars_mys_sch, by = "IDSCHOOL", all.x = TRUE)

# Merging combined_data_nor with teacher data
combined_data_mys_teac <- merge(combined_data_mys, selected_vars_mys_teac, by = "IDSCHOOL", all.x = TRUE)

# Randomly select one teacher per student in cases where multiple teachers exist using the common key 'IDSCHOOL'
combined_data_mys_final <- combined_data_mys_teac %>%
  group_by(IDSTUD) %>%
  slice_sample(n = 1) %>%  # Randomly selects one teacher per student
  ungroup()


##### Norway merging datasets ################

# Merge student data (selected_vars_nor_stud) with school data (selected_vars_nor_sch) using the common key 'IDSCHOOL'
combined_data_nor <- merge(selected_vars_nor_stud, selected_vars_nor_sch, by = "IDSCHOOL", all.x = TRUE)

# Merging combined_data_nor with teacher data using the common key 'IDSCHOOL'
combined_data_nor_teac <- merge(combined_data_nor, selected_vars_nor_teac, by = "IDSCHOOL", all.x = TRUE)

#Randomly select one teacher per student in cases where multiple teachers exist 
combined_data_nor_final <- combined_data_nor_teac %>%
  group_by(IDSTUD) %>%
  slice_sample(n = 1) %>%  # Randomly selects one teacher per student
  ungroup()


################################################################################
############ Variable mappings for readability
################################################################################

#  Define a Mapping Vector for all predictors ans science achievment variables
descriptive_names <- c(
  "BTBS14"   = "Instructional time in science",
  "BTDSBIO"  = "Percentage of students taught biology topics",
  "BTDSPHY"  = "Percentage of students taught physical sci topics",
  "BTDSEAR"  = "Percentage of students taught earth science topics",
  "BSBGICS"  = "Instructional clarity in science lessons",
  "BTBSESI"  = "Teachers emphasis on science investigation",
  "BSBS21"   = "Experiments in science lessons",
  "BCBGSRS"  = "Instruction affected by science resource shortages",
  "BCBGEAS"  = "School emphasis on academic success",
  "BCBGDAS"  = "School discipline",
  "BTBGLSN"  = "Teaching limited by students not ready for instruction",
  "BSBGSLS"  = "Students like learning science",
  "BSBGSCS"  = "Students confident in science",
  "BSBGSVS"  = "Students value science",
  "BSBGSSB"  = "Students sense of school belonging",
  "BSBG10"   = "Student absenteeism",
  "BSBGHER"  = "Home resources",
  "BCDGSBC"  = "Socioeconomic background of the student body",
  "BSBG03"   = "Student speak the language of test",
  "BCBG04"   = "Students native language is the language of the test",
  "ITSEX"    = "Gender",
  "BSSSCI01" = "Science achievement 1",  
  "BSSSCI02" = "Science achievement 2",
  "BSSSCI03" = "Science achievement 3",
  "BSSSCI04" = "Science achievement 4",
  "BSSSCI05" = "Science achievement 5"
)

#  Define a Mapping Vector with short names (for correlation matrix)
short_names <- c(
  "BTBS14"   = "Instr_Time_Sci",
  "BTDSBIO"  = "Bio_Taught",
  "BTDSPHY"  = "PhysSci_Taught",
  "BTDSEAR"  = "EarthSci_Taught",
  "BSBGICS"  = "Instr_Clarity",
  "BTBSESI"  = "Sci_Invest",
  "BSBS21"   = "Exp_Sci_Lessons",
  "BCBGSRS"  = "Res_Shortages",
  "BCBGEAS"  = "Acad_Success",
  "BCBGDAS"  = "School_Disc",
  "BTBGLSN"  = "Teach_Limited",
  "BSBGSLS"  = "Like_Sci",
  "BSBGSCS"  = "Conf_Sci",
  "BSBGSVS"  = "Value_Sci",
  "BSBGSSB"  = "Sch_Belong",
  "BSBG10"   = "Absenteeism",
  "BSBGHER"  = "Home_Res",
  "BCDGSBC"  = "SES_School",
  "BSBG03"   = "Lang_Test",
  "BCBG04"   = "Native_Lang",
  "ITSEX"    = "Gender",
  "BSSSCI01" = "Sci_Ach_1",  
  "BSSSCI02" = "Sci_Ach_2",
  "BSSSCI03" = "Sci_Ach_3",
  "BSSSCI04" = "Sci_Ach_4",
  "BSSSCI05" = "Sci_Ach_5"
)

# Resilience Variables (Categorical)
resilience_vars <- paste0("resilience_", sprintf("%02d", 1:5))

# Resilience Variables (Continuous)
resilience_vars_cont <- paste0("Resilience_BSSSCI0", 1:5)

# Rename Resilience Variables for Tables
resilience_labels <- c(
  "Resilience_BSSSCI01" = "Resilience (Continuous 1)",
  "Resilience_BSSSCI02" = "Resilience (Continuous 2)",
  "Resilience_BSSSCI03" = "Resilience (Continuous 3)",
  "Resilience_BSSSCI04" = "Resilience (Continuous 4)",
  "Resilience_BSSSCI05" = "Resilience (Continuous 5)",
  "resilience_01" = "Resilience (Categorical 1)",
  "resilience_02" = "Resilience (Categorical 2)",
  "resilience_03" = "Resilience (Categorical 3)",
  "resilience_04" = "Resilience (Categorical 4)",
  "resilience_05" = "Resilience (Categorical 5)"
)

################################################################################
############ Variable lists for further analyses
################################################################################

# List of 26-variable set used in descriptive analysis
variables <- c("BTBS14", "BTDSBIO", "BTDSPHY", "BTDSEAR", "BSBGICS", "BTBSESI", "BSBS21", 
               "BCBGSRS", "BCBGEAS", "BCBGDAS", "BTBGLSN", "BSBGSLS", "BSBGSCS", "BSBGSVS", 
               "BSBGSSB", "BSBG10", "BSBGHER", "BCDGSBC", "BSBG03", "BCBG04", "ITSEX",
               "BSSSCI01","BSSSCI02", "BSSSCI03", "BSSSCI04","BSSSCI05")

# List of 20 predictors
predictors <- c("BTBS14", "BTDSBIO", "BTDSPHY", "BTDSEAR", "BSBGICS", "BTBSESI", "BSBS21", 
                "BCBGSRS", "BCBGEAS", "BCBGDAS", "BTBGLSN", "BSBGSLS", "BSBGSCS", "BSBGSVS", 
                "BSBGSSB", "BSBG10", "BCDGSBC", "BSBG03", "BCBG04", "ITSEX")

# List of Factor/character columns that must be numeric before modelling
cols_to_convert <- c("IDSCHOOL", "IDCLASS", "IDSTUD", "IDTEACH", "BSBGICS",
                     "BSBGSLS", "BSBGSCS", "BSBGSVS", "BSBGSSB", "BCBGSRS", "BCBGEAS",
                     "BCBGDAS", "BTBS14", "BTDSBIO", "BTDSPHY", "BTDSEAR",
                     "BTBSESI", "BTBGLSN")

# List of Five plausible-value science achievement scores
science_vars <- c("BSSSCI01", "BSSSCI02", "BSSSCI03", "BSSSCI04", "BSSSCI05")
