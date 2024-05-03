# PURPOSE: IMPUTE SURVEY DATA



# LOAD PACKAGES
packages_all = c("data.table", "haven", "ggcorrplot", "dplyr", "mice", "ggplot2", "ggmice")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
n_imputed_datasets <- 100
n_iterations <- 20



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
print(paste('Number of imputed datasets:', n_imputed_datasets))
print(paste('Number of iterations:', n_iterations))
print('cart = classification and regression trees')
print('pred function = quickpred')
cat('-------- end\n\n')



# LOAD DATA FROM output_location/rds/survey-data-raw.rds
cat('LOAD DATA\n')
cat('-------- start\n')
# load from .Rds
input_data <- readRDS(paste0(output_location, "rds/", "combined-survey-and-medication-data-raw.rds"))
print('Loaded data')
# convert to data.table
data.table::setDT(input_data)
print(paste('is a data.table:', is.data.table(input_data)))
# get dimensions
input_data_dim <- paste0(dim(input_data), collapse = ',')
print(paste0('Dimensions of survey data: ', input_data_dim))
n_total_cohort <- nrow(input_data)
print('Columns')
print(colnames(input_data))
# # get type of all columns in input_data
# cat('GET TYPE OF ALL COLUMNS IN input_data\n')
# cat('-------- start\n')
# input_data_types <- sapply(input_data, class)
# print(input_data_types)
# cat('-------- end\n\n')
# quit()
cat('-------- end\n\n')


# get the subjects without missing value in "survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4"
cat('GET THE SUBJECTS WITHOUT MISSING VALUE IN "survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4"\n')
cat('-------- start\n')
# get the subjects without missing value in "survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4"
subjects_without_missing_values <- input_data[complete.cases(input_data[, c("survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4")]), ]
print(paste('Number of subjects without missing values in trajectory-defining variables:', nrow(subjects_without_missing_values)))
print(table(subjects_without_missing_values$survey_asthma_4, useNA = "always"))
print(table(subjects_without_missing_values$survey_eczema_4, useNA = "always"))
print(table(subjects_without_missing_values$survey_rhinitis_4, useNA = "always"))
print(dim(subjects_without_missing_values))
# save this dataset to an .Rds file
saveRDS(subjects_without_missing_values, paste0(output_location, "rds/", "non-imputed-complete-data.rds"))
cat('-------- end\n\n')



# MAKE A CORRLATION MATRIX FOR THE TRAJECTORY-DEFINING SURVEY VARIABLES
cat('MAKE A CORRELATION MATRIX FOR THE TRAJECTORY-DEFINING SURVEY VARIABLES\n')
cat('-------- start\n')
# select trajectory-defining survey variables
suspected_correlated_variables <- c("asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_1y_ever", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m", "asthma_medication1", "asthma_medication2", "asthma_medication3", "asthma_medication4", "asthma_medication5", "asthma_medication6", "rhinitis_medication1", "rhinitis_medication2", "rhinitis_medication3", "rhinitis_medication4", "rhinitis_medication5", "rhinitis_medication6", "eczema_medication1", "eczema_medication2", "eczema_medication3", "eczema_medication4", "eczema_medication5", "eczema_medication6")
suspected_correlated_data <- input_data[, suspected_correlated_variables, with = FALSE]
suspected_correlated_data[, names(suspected_correlated_data) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = names(suspected_correlated_data)]
# make the correlation matrix
correlation_matrix <- cor(suspected_correlated_data, method = 'pearson', use = "complete.obs")
# plot the correlation matrix
correlation_matrix_plot <- ggcorrplot::ggcorrplot(
    correlation_matrix,
    type = "lower",
    lab = TRUE,
    show.diag = FALSE,
    lab_size = 4.1,
    tl.cex = 10,
    colors = c("#49609A", "#FFFFFF", "#9A4949")
) +
scale_y_discrete(position='right') +
theme(legend.position = "none")
# save the correlation plot
ggsave(paste0(output_location, "svg/", "correlation-plot.svg"), plot = correlation_matrix_plot, device = "svg", dpi = 300, width = 20, height = 20)
print('Done with making the correlation matrix for the disease variables')
cat('-------- end\n\n')



# run MICE on the data
cat('RUN MICE ON THE DATA\n')
cat('-------- start\n')

# imputation_data <- input_data[, -c("lopnr", "asthma_medication1", "asthma_medication2", "asthma_medication3", "asthma_medication4", "asthma_medication5", "asthma_medication6", "rhinitis_medication1", "rhinitis_medication2", "rhinitis_medication3", "rhinitis_medication4", "rhinitis_medication5", "rhinitis_medication6", "eczema_medication1", "eczema_medication2", "eczema_medication3", "eczema_medication4", "eczema_medication5", "eczema_medication6")]
imputation_data <- input_data[, -c("lopnr")]
imputation_data <- imputation_data[, -c("survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4")]
print(dim(imputation_data))
print(colnames(imputation_data))

mice_data <- mice::mice(
    imputation_data,
    pred = quickpred(imputation_data),
    defaultMethod = c("pmm", "cart", "cart", "cart"),
    m = n_imputed_datasets,
    maxit = n_iterations,
    seed = 1 # for reproducibility
)
print(summary(mice_data))
plot(mice_data)
print(tail(mice_data$loggedEvents, 200))
# # loop each variable to create a convergence plot for each
# for (i in 1:ncol(input_data)) {
#     # get name of variable
#     var_name <- eval(colnames(input_data)[[i]])
#     print(var_name)
#     convergence_plot <- ggmice::plot_trace(mice_data, var_name)
#     ggsave(paste0(output_location, "svg/", "mice-convergence-plot-", var_name, ".svg"), plot = convergence_plot, device = "svg", dpi = 300)
# }

# loop each imputed dataset, set composite variables (TODO! non-trajetory defining variables) and save
for (i in 1:n_imputed_datasets) {
    # get imputed dataset
    imputed <- mice::complete(mice_data, i)
    # add on lopnr
    imputed$lopnr <- input_data$lopnr
    # set composite variables
    # any_family_member_asthma_allergy
    imputed$any_family_member_asthma_allergy <- ifelse(
        imputed$mother_asthma == 1 | imputed$father_asthma == 1 | imputed$siblings_asthma == 1 | imputed$mother_allergic_rhinitis == 1 | imputed$father_allergic_rhinitis == 1 | imputed$siblings_allergic_rhinitis == 1 | imputed$mother_eczema == 1 | imputed$father_eczema == 1 | imputed$siblings_eczema == 1,
        1,
        0
    )
    # trajectory-defining variables
    # age 1 year
    imputed$survey_asthma_1 <- ifelse(
        imputed$asthma_diagnosis_1y_ever == 1 & (imputed$asthma_treatment_1y_12m == 1 | imputed$wheeezing_1y_12m == 1),
        1,
        0
    )
    imputed$survey_eczema_1 <- ifelse(
        imputed$eczema_1y_ever == 1,
        1,
        0
    )
    # age 4.5 years
    imputed$survey_asthma_2 <- ifelse(
        imputed$asthma_diagnosis_45y_ever == 1 & (imputed$asthma_treatment_45y_12m == 1 | imputed$wheezing_45y_12m == 2 | imputed$wheezing_45y_12m == 3),
        1,
        0
    )
    imputed$survey_eczema_2 <- ifelse(
        imputed$eczema_45y_12m == 1 | imputed$eczema_treatment_45y_12m == 1,
        1,
        0
    )
    imputed$survey_rhinitis_2 <- ifelse(
        imputed$rhinitis_diagnosis_45y_ever == 1 & imputed$rhinitis_symptoms_45y_12m == 1,
        1,
        0
    )
    # age 8 years
    imputed$survey_asthma_3 <- ifelse(
        imputed$asthma_diagnosis_8y_ever == 1 & (imputed$asthma_treatment_8y_12m == 1 | imputed$wheezing_8y_12m == 1),
        1,
        0
    )
    imputed$survey_eczema_3 <- ifelse(
        imputed$eczema_diagnosis_8y_ever == 1 & (imputed$eczema_treatment_8y_12m == 1 | imputed$eczema_8y_12m == 1),
        1,
        0
    )
    imputed$survey_rhinitis_3 <- ifelse(
        imputed$rhinitis_diagnosis_8y_ever == 1 & (imputed$rhinitis_treatment_8y_12m == 1 | imputed$rhinitis_symptoms_8y_12m == 1),
        1,
        0
    )
    # age 12 years
    imputed$survey_asthma_4 <- ifelse(
        imputed$asthma_diagnosis_12y_ever == 1 & (imputed$asthma_treatment_12y_12m == 1 | imputed$wheezing_12y_12m == 1),
        1,
        0
    )
    imputed$survey_eczema_4 <- ifelse(
        imputed$eczema_diagnosis_12y_ever == 1 & (imputed$eczema_treatment_12y_12m == 1 | imputed$eczema_12y_12m == 1),
        1,
        0
    )
    imputed$survey_rhinitis_4 <- ifelse(
        imputed$rhinitis_diagnosis_12y_ever == 1 & (imputed$rhinitis_treatment_12y_12m == 1 | imputed$rhinitis_symptoms_12y_12m == 1),
        1,
        0
    )
    # save imputed dataset
    saveRDS(imputed, paste0(output_location, "rds/", "imputed-data-", i, ".rds"))
}