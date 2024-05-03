# TODO! Add descriptions



# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "visdat", "naniar", "mice", "ggmice")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
cat('-------- end\n\n')



# LOAD DATA FROM output_location/rds/survey-data-raw.rds
cat('LOAD DATA\n')
cat('-------- start\n')
# load from .Rds
input_data <- readRDS(paste0(output_location, "rds/", "combined-survey-and-medication-data-raw.rds"))
print('Loaded survey data')
# convert to data.table
data.table::setDT(input_data)
print(paste('is a data.table:', is.data.table(input_data)))
# get dimensions
input_data_dim <- paste0(dim(input_data), collapse = ',')
print(paste0('Dimensions of survey data: ', input_data_dim))
n_total_cohort <- nrow(input_data)
print('Columns')
print(colnames(input_data))
cat('-------- end\n\n')



# ASSESSMENT OF PATTERNS OF MISSINGNESS
cat('ASSESSMENT OF PATTERNS OF MISSINGNESS\n')
cat('-------- start\n')

# plot missingness across variables
source_variable_data_without_meds <- input_data %>% select("mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema", "sex", "mother_education", "mother_smoking_pregnancy", "mother_age", "c_section", "gestational_age", "birth_weight", "birth_order", "daycare_6m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m")
overall_missingness_plot <- visdat::vis_miss(source_variable_data_without_meds, cluster = TRUE, sort_miss = TRUE)
print('Done with assessing missingness on variable basis')
ggsave(paste0(output_location, "svg/", "missingness-plot-all-variables.svg"), plot = overall_missingness_plot, device = "svg", dpi = 300)

# plot missingness across subjects
subject_missingness_plot <- naniar::gg_miss_case(source_variable_data_without_meds, order_cases = TRUE) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print('Done with assessing missingness on subject basis')
ggsave(paste0(output_location, "svg/", "missingness-plot-all-subjects.svg"), plot = subject_missingness_plot, device = "svg", dpi = 300)

# get percentage of subjects with any missing data
overall_missingness_data <- source_variable_data_without_meds[!complete.cases(source_variable_data_without_meds), ]
overall_missingness_n <- nrow(overall_missingness_data)
overall_missingness_perc <- round(overall_missingness_n / n_total_cohort * 100, 1)
print(paste0(overall_missingness_n, ' (', overall_missingness_perc, '%) with missingness in the disease variables'))

cat('-------- end\n\n')



# make flux plot
# exclude composite variables
cat('MAKE FLUX PLOT\n')
cat('-------- start\n')
imputation_data <- input_data %>% select("mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema", "sex", "mother_education", "mother_smoking_pregnancy", "mother_age", "c_section", "gestational_age", "birth_weight", "birth_order", "daycare_6m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m")
imputation_data_dim <- paste0(dim(imputation_data), collapse = ',')
print(paste0('Dimensions of survey data: ', input_data_dim))
print(colnames(input_data))

# MICE dry run
mice_ini <- mice::mice(imputation_data, maxit = 0)
print('Initial missingness (top row: number of missing values, bottom row: number of variables with such number of missing values)')
print(table(mice_ini$nmis))
mice_flux <- ggmice::plot_flux(imputation_data) #+ geom_text_repel()
# save mice_flux as svg
ggsave(paste0(output_location, "svg/", "mice-flux-plot.svg"), plot = mice_flux, device = "svg", dpi = 300)

# find suitable set of predictors
mice_pred <- mice::quickpred(imputation_data)
print('Predictors for asthma_diagnosis_12y_ever')
print(rowSums(mice_pred[c("asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m"), ]))


cat('-------- end\n\n')