# ASSESS SURVEY DATA IN TERMS OF PATTERNS OF RESPONSE AND MISSINGNESS, AND PERFORM DESCRIPTIVE STATISTICS



# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "gtsummary", "flextable", "gridExtra")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
n_total_cohort <- 5654
n_imputations <- 100



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
cat('-------- end\n\n')



# LOAD DATA FROM output_location/rds/survey-data-raw.rds
cat('LOAD DATA\n')
cat('-------- start\n')
# raw data
raw_combined_data <- readRDS(paste0(output_location, "rds/", "combined-survey-and-medication-data-raw.rds"))
data.table::setDT(raw_combined_data)
raw_combined_data_dim <- paste0(dim(raw_combined_data), collapse = ',')
print(paste0('Dimensions of raw data: ', raw_combined_data_dim))
pooled_combined_data <- copy(raw_combined_data)
# imputed data
imputed_datasets <- list()
for (i in 1:n_imputations) {
    imputed_dataset <- readRDS(paste0(output_location, "rds/imputed-data-", i, ".rds"))
    data.table::setDT(imputed_dataset)
    imputed_datasets[[i]] <- imputed_dataset
}
cat('-------- end\n\n')



# pool variables
cat('POOL VARIABLES\n')
cat('-------- start\n')
# pool categorical variables through majority-voting
variables_to_pool <- c("mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema", "sex", "mother_education", "mother_smoking_pregnancy", "c_section", "birth_order", "daycare_6m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m")
for (variable in variables_to_pool) {
    # get the pooled values
    pooled_values <- data.table(majority_vote = rep(NA, n_total_cohort))
    # add one column for each imputed dataset in pooled_values
    for (i in 1:n_imputations) {
        dataset_column_name <- paste0('dataset', i)
        imputed_dataset <- imputed_datasets[[i]]
        # populate the column paste0('dataset', i) in pooled_values with the variable in question
        pooled_values[, (dataset_column_name) := imputed_dataset[[variable]]]
    }
    pooled_values[, majority_vote := apply(pooled_values, 1, function(x) {
        names(table(x))[which.max(table(x))]
    })]
    # replace the original values in pooled_combined_data with the pooled values
    pooled_combined_data[, (variable) := pooled_values$majority_vote]
    print(paste('Finished pooling', variable))
}
# pool continuous variables by taking the mean
variables_to_pool <- c("mother_age", "gestational_age", "birth_weight")
for (variable in variables_to_pool) {
    # get the pooled values
    pooled_values <- data.table(mean_value = rep(NA, n_total_cohort))
    # add one column for each imputed dataset in pooled_values
    for (i in 1:n_imputations) {
        dataset_column_name <- paste0('dataset', i)
        imputed_dataset <- imputed_datasets[[i]]
        # populate the column paste0('dataset', i) in pooled_values with the variable in question
        pooled_values[, (dataset_column_name) := imputed_dataset[[variable]]
        ]
    }
    pooled_values[, mean_value := rowMeans(pooled_values, na.rm = TRUE)]
    # replace the original values in pooled_combined_data with the pooled values
    pooled_combined_data[, (variable) := pooled_values$mean_value]
    print(paste('Finished pooling', variable))
}
# save the pooled data to an .Rds file
saveRDS(pooled_combined_data, paste0(output_location, "rds/", "pooled-imputed-survey-and-medication-data.rds"))
cat('-------- end\n\n')



# summarize data
cat('SUMMARIZE DATA\n')
cat('-------- start\n')

# any_family_member_asthma_allergy composite variable
pooled_combined_data$any_family_member_asthma_allergy <- ifelse(
    pooled_combined_data$mother_asthma == 1 | pooled_combined_data$father_asthma == 1 | pooled_combined_data$siblings_asthma == 1 | pooled_combined_data$mother_allergic_rhinitis == 1 | pooled_combined_data$father_allergic_rhinitis == 1 | pooled_combined_data$siblings_allergic_rhinitis == 1 | pooled_combined_data$mother_eczema == 1 | pooled_combined_data$father_eczema == 1 | pooled_combined_data$siblings_eczema == 1,
    1,
    0
)
# trajectory-defining variables
# age 1 year
pooled_combined_data$survey_asthma_1 <- ifelse(
    pooled_combined_data$asthma_diagnosis_1y_ever == 1 & (pooled_combined_data$asthma_treatment_1y_12m == 1 | pooled_combined_data$wheeezing_1y_12m == 1),
    1,
    0
)
pooled_combined_data$survey_eczema_1 <- ifelse(
    pooled_combined_data$eczema_1y_ever == 1,
    1,
    0
)
# age 4.5 years
pooled_combined_data$survey_asthma_2 <- ifelse(
    pooled_combined_data$asthma_diagnosis_45y_ever == 1 & (pooled_combined_data$asthma_treatment_45y_12m == 1 | pooled_combined_data$wheezing_45y_12m == 2 | pooled_combined_data$wheezing_45y_12m == 3),
    1,
    0
)
pooled_combined_data$survey_eczema_2 <- ifelse(
    pooled_combined_data$eczema_45y_12m == 1 | pooled_combined_data$eczema_treatment_45y_12m == 1,
    1,
    0
)
pooled_combined_data$survey_rhinitis_2 <- ifelse(
    pooled_combined_data$rhinitis_diagnosis_45y_ever == 1 & pooled_combined_data$rhinitis_symptoms_45y_12m == 1,
    1,
    0
)
# age 8 years
pooled_combined_data$survey_asthma_3 <- ifelse(
    pooled_combined_data$asthma_diagnosis_8y_ever == 1 & (pooled_combined_data$asthma_treatment_8y_12m == 1 | pooled_combined_data$wheezing_8y_12m == 1),
    1,
    0
)
pooled_combined_data$survey_eczema_3 <- ifelse(
    pooled_combined_data$eczema_diagnosis_8y_ever == 1 & (pooled_combined_data$eczema_treatment_8y_12m == 1 | pooled_combined_data$eczema_8y_12m == 1),
    1,
    0
)
pooled_combined_data$survey_rhinitis_3 <- ifelse(
    pooled_combined_data$rhinitis_diagnosis_8y_ever == 1 & (pooled_combined_data$rhinitis_treatment_8y_12m == 1 | pooled_combined_data$rhinitis_symptoms_8y_12m == 1),
    1,
    0
)
# age 12 years
pooled_combined_data$survey_asthma_4 <- ifelse(
    pooled_combined_data$asthma_diagnosis_12y_ever == 1 & (pooled_combined_data$asthma_treatment_12y_12m == 1 | pooled_combined_data$wheezing_12y_12m == 1),
    1,
    0
)
pooled_combined_data$survey_eczema_4 <- ifelse(
    pooled_combined_data$eczema_diagnosis_12y_ever == 1 & (pooled_combined_data$eczema_treatment_12y_12m == 1 | pooled_combined_data$eczema_12y_12m == 1),
    1,
    0
)
pooled_combined_data$survey_rhinitis_4 <- ifelse(
    pooled_combined_data$rhinitis_diagnosis_12y_ever == 1 & (pooled_combined_data$rhinitis_treatment_12y_12m == 1 | pooled_combined_data$rhinitis_symptoms_12y_12m == 1),
    1,
    0
)

# add a column "pooled" to denote if the data is pooled or not
pooled_combined_data$pooled <- 'pooled'
raw_combined_data$pooled <- 'raw'
# combine the raw and pooled data
all_data <- rbind(raw_combined_data, pooled_combined_data)

# make birth weight a three-level ordinal
all_data$birth_weight <- ifelse(
    all_data$birth_weight < 2500,
    "Low",
    ifelse(
        all_data$birth_weight >= 2500,
        "Not low",
        NA
    )
)
all_data$birth_weight <- factor(all_data$birth_weight, levels = c("Low", "Not low"))
# make gestational age a three-level ordinal
all_data$gestational_age <- ifelse(
    all_data$gestational_age < 37,
    "Preterm",
    ifelse(
        all_data$gestational_age >= 37,
        "Term",
        NA
    )
)



# set order of levels for pooled
all_data$pooled <- factor(all_data$pooled, levels = c("raw", "pooled"))
# summarize the data
table <- all_data %>%
    tbl_summary(
        by = pooled
        # , statistic = list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{p}")
    ) %>%
    # add_overall() %>%
    add_p() %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(output_location, 'docx/', 'characeristics-raw-and-pooled.docx'))