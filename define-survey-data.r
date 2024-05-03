# PURPOSE: TO DEFINE SURVEY DATA VARIABLES



# LOAD PACKAGES
packages_all = c("data.table", "haven")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
survey_filename <- paste0(input_location, "SOS_grundfil_LOPNR_181106.sav") # path and filename of input data (survey data)



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
cat('-------- end\n\n')



# LOAD AND PREPROCESS SURVEY DATA
survey_data <- haven::read_sav(survey_filename, encoding = "latin1")
cat('LOADING SURVEY DATA AND FIRST CHECKS\n')
cat('-------- start\n')
print('Loaded survey data')
# convert to data.table
data.table::setDT(survey_data)
print(paste('is a data.table:', is.data.table(survey_data)))
# dimensions of data
survey_data_dim <- paste0(dim(survey_data), collapse = ',')
print(paste0('Dimensions of survey data: ', survey_data_dim))
# get some basic stats about löpnr and assign correctly to those who do not have a unique löpnr
unique_lopnr_num_prior <- length(unique(survey_data$lopnr))
most_common_lopnr <- as.integer(names(which.max(table(survey_data$lopnr))))
most_common_lopnr_num <- max(table(survey_data$lopnr))
lowest_lopnr_prior <- min(survey_data$lopnr)
highest_lopnr_prior <- max(survey_data$lopnr)
print(paste0('Number of unique lopnr values prior to preprocessing: ', unique_lopnr_num_prior))
print(paste0('Lowest lopnr value prior to preprocessing: ', lowest_lopnr_prior))
print(paste0('Highest lopnr value prior to preprocessing: ', highest_lopnr_prior))
print(paste0('Most common lopnr value prior to preprocessing: ', most_common_lopnr))
print(paste0('Number of times the most common lopnr value number occurs: ', most_common_lopnr_num))
# replace most common lopnr sequentially
survey_data$lopnr[survey_data$lopnr == most_common_lopnr] <- seq(highest_lopnr_prior + 1, highest_lopnr_prior + most_common_lopnr_num)
# set the highest lopnr to most_common_lopnr (so as to not have any gaps)
survey_data$lopnr[survey_data$lopnr == max(survey_data$lopnr)] <- as.integer(most_common_lopnr)
print('Preprocessing of lopnr done')
unique_lopnr_num_post <- length(unique(survey_data$lopnr))
lowest_lopnr_post <- min(survey_data$lopnr)
highest_lopnr_post <- max(survey_data$lopnr)
print(paste0('Number of unique lopnr values after preprocessing: ', unique_lopnr_num_post))
print(paste0('Lowest lopnr value after preprocessing: ', lowest_lopnr_post))
print(paste0('Highest lopnr value after preprocessing: ', highest_lopnr_post))
cat('-------- end\n\n')



# GET HEREDITARY DATA
cat('GET HEREDITARY DATA\n')
cat('-------- start\n')
# define all source variables
survey_data$mother_asthma <- survey_data$V19
survey_data$mother_allergic_rhinitis <- survey_data$V24
survey_data$mother_eczema <- survey_data$V29
survey_data$father_asthma <- survey_data$V20
survey_data$father_allergic_rhinitis <- survey_data$V25
survey_data$father_eczema <- survey_data$V30
survey_data$siblings_asthma <- survey_data$V21
survey_data$siblings_allergic_rhinitis <- survey_data$V26
survey_data$siblings_eczema <- survey_data$V31
survey_data$sex_mbr <- survey_data$KON - 1
survey_data$sex_survey <- ifelse(survey_data$V217 == 2, 0, survey_data$V217)
# recode source variables
yes_no_variables_hereditary <- c("mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema")
survey_data[, (yes_no_variables_hereditary) := lapply(.SD, function(x) ifelse(x == 2, 0, x)), .SDcols = yes_no_variables_hereditary]
survey_data[, (yes_no_variables_hereditary) := lapply(.SD, factor), .SDcols = yes_no_variables_hereditary]
# construct composite variables
survey_data[, 
    sex := ifelse(
        is.na(sex_survey),
        sex_mbr,
        ifelse(
            is.na(sex_mbr),
            sex_survey,
            ifelse(
                sex_survey == sex_mbr,
                sex_survey,
                NA
            )
        )
    )
]
survey_data$mother_asthma_allergy <- ifelse(
    survey_data$mother_asthma == 1 | survey_data$mother_allergic_rhinitis == 1 | survey_data$mother_eczema == 1,
    1,
    0
)
survey_data$father_asthma_allergy <- ifelse(
    survey_data$father_asthma == 1 | survey_data$father_allergic_rhinitis == 1 | survey_data$father_eczema == 1,
    1,
    0
)
survey_data$siblings_asthma_allergy <- ifelse(
    survey_data$siblings_asthma == 1 | survey_data$siblings_allergic_rhinitis == 1 | survey_data$siblings_eczema == 1,
    1,
    0
)
# survey_data$mother_only_asthma_allergy <- ifelse(
#     survey_data$mother_asthma_allergy == 1 & survey_data$father_asthma_allergy == 0,
#     1,
#     0
# )
# survey_data$father_only_asthma_allergy <- ifelse(
#     survey_data$mother_asthma_allergy == 0 & survey_data$father_asthma_allergy == 1,
#     1,
#     0
# )
# survey_data$mother_or_father_asthma_allergy <- ifelse(
#     survey_data$mother_asthma_allergy == 1 | survey_data$father_asthma_allergy == 1,
#     1,
#     0
# )
# survey_data$mother_and_father_asthma_allergy <- ifelse(
#     survey_data$mother_asthma_allergy == 1 & survey_data$father_asthma_allergy == 1,
#     1,
#     0
# )
survey_data$any_family_member_asthma_allergy <- ifelse(
    survey_data$mother_asthma_allergy == 1 | survey_data$father_asthma_allergy == 1 | survey_data$siblings_asthma_allergy == 1,
    1,
    0
)
cat('-------- end\n\n')



# GET ANTENATAL/PREGNANCY-RELATED DATA
cat('GET ANTENATAL/PREGNANCY-RELATED DATA\n')
cat('-------- start\n')
# TODO! is mother_smoking_pregnancy a source or composite variable??
# define all source variables
survey_data$mother_education <- ifelse(
    survey_data$V41 == 1 | survey_data$V41 == 2,
    1,
    ifelse(
        survey_data$V41 == 3 | survey_data$V41 == 4,
        2,
        ifelse(
            survey_data$V41 == 5 | survey_data$V41 == 6,
            3,
            NA
        )
    )
)
survey_data$mother_smoking_pregnancy <- ifelse(
    survey_data$V67 == 2 & is.na(survey_data$V68),
    2,
    survey_data$V68
)
# # since V69-V71 are follow-up questions, NAs are considered to be 0
# survey_data$mother_smoking_trimester_1 <- ifelse(
#     survey_data$V69 >= 2 & !is.na(survey_data$V69),
#     1,
#     0
# )
# survey_data$mother_smoking_trimester_2 <- ifelse(
#     survey_data$V70 >= 2 & !is.na(survey_data$V70),
#     1,
#     0
# )
# survey_data$mother_smoking_trimester_3 <- ifelse(
#     survey_data$V71 >= 2 & !is.na(survey_data$V71),
#     1,
#     0
# )
# survey_data$father_smoking_pregnancy <- survey_data$V75
survey_data$mother_age <- survey_data$MALDER
# survey_data$mother_prepregnancy_height <- survey_data$MLANGD
# survey_data$mother_prepregnancy_weight <- survey_data$MVIKT
survey_data$c_section <- survey_data$SECMARK
survey_data$gestational_age <- survey_data$GRVBS
survey_data$birth_weight <- survey_data$BVIKTBS
# recode source variables
yes_no_variables_antenatal_pregnancy <- c("mother_smoking_pregnancy") #, "father_smoking_pregnancy")
survey_data[, (yes_no_variables_antenatal_pregnancy) := lapply(.SD, function(x) ifelse(x == 2, 0, x)), .SDcols = yes_no_variables_antenatal_pregnancy]
survey_data[, (yes_no_variables_antenatal_pregnancy) := lapply(.SD, factor), .SDcols = yes_no_variables_antenatal_pregnancy]
# # define composite variables
# survey_data$mother_only_smoking_pregnancy <- ifelse(survey_data$mother_smoking_pregnancy == 1 & survey_data$father_smoking_pregnancy == 0, 1, 0)
# survey_data$father_only_smoking_pregnancy <- ifelse(survey_data$mother_smoking_pregnancy == 0 & survey_data$father_smoking_pregnancy == 1, 1, 0)
# survey_data$mother_or_father_smoking_pregnancy <- ifelse(survey_data$mother_smoking_pregnancy == 1 | survey_data$father_smoking_pregnancy == 1, 1, 0)
# survey_data$mother_and_father_smoking_pregnancy <- ifelse(survey_data$mother_smoking_pregnancy == 1 & survey_data$father_smoking_pregnancy == 1, 1, 0)
# survey_data$mother_prepregnancy_bmi <- survey_data$mother_prepregnancy_weight/((survey_data$mother_prepregnancy_height/100)^2)
cat('-------- end\n\n')



# GET EARLY-LIFE EXPOSURE AND EVENTS DATA
cat('GET EARLY-LIFE EXPOSURE AND EVENTS DATA\n')
cat('-------- start\n')
# TODO! is antibiotics_infancy_neonatal a source or composite variable??
# define all source variables
survey_data$birth_order <- survey_data$PARABS
survey_data$birth_order[survey_data$birth_order > 3] <- 3 # set values above 3 to 3 (interpretability, convergence of imputation etc)
# # antibiotics
# survey_data$antibiotics_neonatal <- ifelse(
#     survey_data$V61 == 3, # "vet ej" ("don't know")
#     NA,
#     ifelse(
#         survey_data$V61 == 2 | is.na(survey_data$V61), # "nej" ("no") or NA (follow-up question, so set to 0)
#         0,
#         1
#     )
# )
# # question asks about months but some appear to have answered in weeks (convert those to months)
# survey_data$V272 <- ifelse(survey_data$V272 > 12, survey_data$V272/4, survey_data$V272)
# # as V272 (age at first antibiotics treatment outside of neonatal care) is a follow-up question, NAs are considered to not be missing (i.e., 0)
# survey_data$antibiotics_infancy_neonatal_3m <- ifelse(
#     !is.na(survey_data$V272) & survey_data$V272 <= 3,
#     1,
#     0
# )
# survey_data$antibiotics_infancy_3m_6m <- ifelse(
#     !is.na(survey_data$V272) & survey_data$V272 > 3 & survey_data$V272 <= 6,
#     1,
#     0
# )
# survey_data$antibiotics_infancy_6m_12m <- ifelse(
#     !is.na(survey_data$V272) & survey_data$V272 > 6 & survey_data$V272 <= 12,
#     1,
#     0
# )
# survey_data$breastfeeding_stopped <- survey_data$V239
# survey_data$milk_supplement_introduction <- survey_data$V241
# # breastfeeding_exclusive is set to the minimum of the two variables above
# survey_data$breastfeeding_exclusive <- pmin(survey_data$breastfeeding_stopped, survey_data$milk_supplement_introduction)
survey_data$daycare_6m <- survey_data$V431
# survey_data$pets_dog <- survey_data$V228
# survey_data$pets_cat <- survey_data$V229
# survey_data$pets_bird <- survey_data$V230
# survey_data$pets_rabbit <- survey_data$V231
# survey_data$farm_living_with_animals <- survey_data$V430
# survey_data$mold_home <- survey_data$V423
# recode source variables
yes_no_variables_early_life_exposure <- c("daycare_6m") #, "pets_dog", "pets_cat", "pets_bird", "pets_rabbit", "farm_living_with_animals", "mold_home")
survey_data[, (yes_no_variables_early_life_exposure) := lapply(.SD, function(x) ifelse(x == 2, 0, x)), .SDcols = yes_no_variables_early_life_exposure]
survey_data[, (yes_no_variables_early_life_exposure) := lapply(.SD, factor), .SDcols = yes_no_variables_early_life_exposure]
# # define composite variables
# survey_data$pets_any_12m <- ifelse(survey_data$pets_dog == 1 | survey_data$pets_cat == 1 | survey_data$pets_bird == 1 | survey_data$pets_rabbit == 1, 1, 0)
# survey_data$antibiotics_infancy_0m_12m <- ifelse(survey_data$antibiotics_infancy_neonatal_3m == 1 | survey_data$antibiotics_infancy_3m_6m == 1 | survey_data$antibiotics_infancy_6m_12m == 1, 1, 0)
cat('-------- end\n\n')



# DEFINE TRAJECTORY-DEFINING SURVEY VARIABLES
cat('DEFINE TRAJECTORY-DEFINING SURVEY VARIABLES\n')
cat('-------- start\n')
# 1-year data
# asthma
survey_data$asthma_diagnosis_1y_ever <- ifelse(survey_data$V285 == 2, 0, survey_data$V285)
survey_data$asthma_treatment_1y_12m <- ifelse(survey_data$V286 == 2, 0, survey_data$V286)
survey_data$wheeezing_1y_12m <- ifelse(survey_data$V279 == 2, 0, survey_data$V279)
# eczema
survey_data$eczema_1y_ever <- ifelse(survey_data$V290 == 2, 0, survey_data$V290)
# rhinitis
# no data for it from the 1-year data
print('Done with variable definitions for 1-year data')
# 4.5-years data
# asthma
survey_data$asthma_diagnosis_45y_ever <- ifelse(survey_data$V477 == 2, 0, survey_data$V477)
survey_data$asthma_treatment_45y_12m <- ifelse(survey_data$V478 == 2, 0, survey_data$V478)
survey_data$wheezing_45y_12m <- ifelse(
    survey_data$V467 >= 2,
    1,
    ifelse(
        !is.na(survey_data$V467),
        0,
        NA
    )
)
# eczema
survey_data$eczema_45y_12m <- ifelse(survey_data$V494 == 2, 0, survey_data$V494)
survey_data$eczema_diagnosis_45y_1_45y <- ifelse(survey_data$V493 == 2, 0, survey_data$V493)
survey_data$eczema_treatment_45y_12m <- ifelse(survey_data$V495 == 2, 0, survey_data$V495)
# rhinitis
survey_data$rhinitis_diagnosis_45y_ever <- ifelse(survey_data$V485 == 2, 0, survey_data$V485)
survey_data$rhinitis_symptoms_45y_12m <- ifelse(survey_data$V492 == 2, 0, survey_data$V492)
print('Done with variable definitions for 4.5-years data')
# 8-years data
# asthma
survey_data$asthma_diagnosis_8y_ever <- ifelse(survey_data$V740 == 2, 0, survey_data$V740)
survey_data$asthma_treatment_8y_12m <- ifelse(survey_data$V744 == 2, 0, survey_data$V744)
survey_data$wheezing_8y_12m <- ifelse(survey_data$V728 == 2, 0, survey_data$V728)
# eczema
survey_data$eczema_8y_12m <- ifelse(survey_data$V763 == 2, 0, survey_data$V763)
survey_data$eczema_diagnosis_8y_ever <- ifelse(survey_data$V764 == 2, 0, survey_data$V764)
survey_data$eczema_treatment_8y_12m <- ifelse(survey_data$V765 == 2, 0, survey_data$V765)
# rhinitis
survey_data$rhinitis_diagnosis_8y_ever <- ifelse(survey_data$V758 == 2, 0, survey_data$V758)
survey_data$rhinitis_treatment_8y_12m <- ifelse(survey_data$V759 == 2, 0, survey_data$V759)
survey_data$rhinitis_symptoms_8y_12m <- ifelse(survey_data$V750 == 2, 0, survey_data$V750)
print('Done with variable definitions for 8-years data')
# 12-years data
# asthma
survey_data$asthma_diagnosis_12y_ever <- ifelse(survey_data$V1002 == 2, 0, survey_data$V1002)
survey_data$asthma_treatment_12y_12m <- ifelse(survey_data$V1003 == 2, 0, survey_data$V1003)
survey_data$wheezing_12y_12m <- ifelse(survey_data$V1001 == 2, 0, survey_data$V1001)
# eczema
survey_data$eczema_12y_12m <- ifelse(survey_data$V1042 == 2, 0, survey_data$V1042)
survey_data$eczema_diagnosis_12y_ever <- ifelse(survey_data$V1043 == 2, 0, survey_data$V1043)
survey_data$eczema_treatment_12y_12m <- ifelse(survey_data$V1044 == 2, 0, survey_data$V1044)
# rhinitis
survey_data$rhinitis_diagnosis_12y_ever <- ifelse(survey_data$V1030 == 2, 0, survey_data$V1030)
survey_data$rhinitis_treatment_12y_12m <- ifelse(survey_data$V1031 == 2, 0, survey_data$V1031)
survey_data$rhinitis_symptoms_12y_12m <- ifelse(survey_data$V1029 == 2, 0, survey_data$V1029)
print('Done with variable definitions for 12-years data')
# in case ever diagnosis has been reported, set all subsequent diagnosis variables to 1
# asthma
survey_data$asthma_diagnosis_45y_ever[survey_data$asthma_diagnosis_1y_ever == 1 & is.na(survey_data$asthma_diagnosis_45y_ever)] <- 1
survey_data$asthma_diagnosis_8y_ever[(survey_data$asthma_diagnosis_1y_ever == 1 | survey_data$asthma_diagnosis_45y_ever == 1) & is.na(survey_data$asthma_diagnosis_8y_ever)] <- 1
survey_data$asthma_diagnosis_12y_ever[(survey_data$asthma_diagnosis_1y_ever == 1 | survey_data$asthma_diagnosis_45y_ever == 1 | survey_data$asthma_diagnosis_8y_ever == 1) & is.na(survey_data$asthma_diagnosis_12y_ever)] <- 1
# eczema
survey_data$eczema_diagnosis_8y_ever[survey_data$eczema_diagnosis_45y_1_45y == 1 & is.na(survey_data$eczema_diagnosis_8y_ever)] <- 1
survey_data$eczema_diagnosis_12y_ever[(survey_data$eczema_diagnosis_45y_1_45y == 1 | survey_data$eczema_diagnosis_8y_ever == 1) & is.na(survey_data$eczema_diagnosis_12y_ever)] <- 1
# rhinitis
survey_data$rhinitis_diagnosis_8y_ever[survey_data$rhinitis_diagnosis_45y_ever == 1 & is.na(survey_data$rhinitis_diagnosis_8y_ever)] <- 1
survey_data$rhinitis_diagnosis_12y_ever[(survey_data$rhinitis_diagnosis_45y_ever == 1 | survey_data$rhinitis_diagnosis_8y_ever == 1) & is.na(survey_data$rhinitis_diagnosis_12y_ever)] <- 1
print('Done with setting subsequent diagnosis variables to 1 if ever diagnosis has been reported (previously)')
# set composite variables
# asthma
survey_data$survey_asthma_1 <- ifelse(survey_data$asthma_diagnosis_1y_ever == 1 & (survey_data$asthma_treatment_1y_12m == 1 | survey_data$wheeezing_1y_12m == 1), 1, 0)
survey_data$survey_asthma_2 <- ifelse(survey_data$asthma_diagnosis_45y_ever == 1 & (survey_data$asthma_treatment_45y_12m == 1 | survey_data$wheezing_45y_12m == 2 | survey_data$wheezing_45y_12m == 3), 1, 0)
survey_data$survey_asthma_3 <- ifelse(survey_data$asthma_diagnosis_8y_ever == 1 & (survey_data$asthma_treatment_8y_12m == 1 | survey_data$wheezing_8y_12m == 1), 1, 0)
survey_data$survey_asthma_4 <- ifelse(survey_data$asthma_diagnosis_12y_ever == 1 & (survey_data$asthma_treatment_12y_12m == 1 | survey_data$wheezing_12y_12m == 1), 1, 0)
# eczema
survey_data$survey_eczema_1 <- ifelse(survey_data$eczema_1y_ever == 1, 1, 0)
survey_data$survey_eczema_2 <- ifelse(survey_data$eczema_45y_12m == 1 | survey_data$eczema_treatment_45y_12m == 1, 1, 0)
survey_data$survey_eczema_3 <- ifelse(survey_data$eczema_diagnosis_8y_ever == 1 & (survey_data$eczema_treatment_8y_12m == 1 | survey_data$eczema_8y_12m == 1), 1, 0)
survey_data$survey_eczema_4 <- ifelse(survey_data$eczema_diagnosis_12y_ever == 1 & (survey_data$eczema_treatment_12y_12m == 1 | survey_data$eczema_12y_12m == 1), 1, 0)
# rhinitis
survey_data$survey_rhinitis_2 <- ifelse(survey_data$rhinitis_diagnosis_45y_ever == 1 & survey_data$rhinitis_symptoms_45y_12m == 1, 1, 0)
survey_data$survey_rhinitis_3 <- ifelse(survey_data$rhinitis_diagnosis_8y_ever == 1 & (survey_data$rhinitis_treatment_8y_12m == 1 | survey_data$rhinitis_symptoms_8y_12m == 1), 1, 0)
survey_data$survey_rhinitis_4 <- ifelse(survey_data$rhinitis_diagnosis_12y_ever == 1 & (survey_data$rhinitis_treatment_12y_12m == 1 | survey_data$rhinitis_symptoms_12y_12m == 1), 1, 0)
print('Done with setting composite variables')
cat('-------- end\n\n')



# SELECT VARIABLES OF INTEREST AND SAVE WORKING SURVEY DATASET TO .Rds
cat('SELECT VARIABLES OF INTEREST AND SAVE WORKING SURVEY DATASET TO .Rds\n')
cat('-------- start\n')
# convert to factors
# variables_to_factor <- c("sex", "c_section", "antibiotics_neonatal", "antibiotics_infancy_3m_6m", "antibiotics_infancy_6m_12m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m")
variables_to_factor <- c("mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema", "sex", "any_family_member_asthma_allergy", "mother_smoking_pregnancy", "daycare_6m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m", "survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4")
survey_data[, (variables_to_factor) := lapply(.SD, factor), .SDcols = variables_to_factor]
# convert birth_order and mother_education to ordered factor
survey_data$birth_order <- factor(survey_data$birth_order, ordered = TRUE)
survey_data$mother_education <- factor(survey_data$mother_education, ordered = TRUE)
# select all variables that have been defined
# output_data <- survey_data[, c("lopnr", "mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema", "sex_mbr", "sex_survey", "sex", "mother_asthma_allergy", "father_asthma_allergy", "siblings_asthma_allergy", "mother_only_asthma_allergy", "father_only_asthma_allergy", "mother_or_father_asthma_allergy", "mother_and_father_asthma_allergy", "any_family_member_asthma_allergy", "mother_smoking_pregnancy", "mother_smoking_trimester_1", "mother_smoking_trimester_2", "mother_smoking_trimester_3", "father_smoking_pregnancy", "mother_age", "mother_prepregnancy_height", "mother_prepregnancy_weight", "c_section", "gestational_age", "birth_weight", "mother_only_smoking_pregnancy", "father_only_smoking_pregnancy", "mother_or_father_smoking_pregnancy", "mother_and_father_smoking_pregnancy", "mother_prepregnancy_bmi", "birth_order", "antibiotics_neonatal", "antibiotics_infancy_neonatal_3m", "antibiotics_infancy_3m_6m", "antibiotics_infancy_6m_12m", "breastfeeding_stopped", "milk_supplement_introduction", "breastfeeding_exclusive", "daycare_6m", "pets_dog", "pets_cat", "pets_bird", "pets_rabbit", "farm_living_with_animals", "mold_home", "pets_any_12m", "antibiotics_infancy_0m_12m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m", "survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4")]
output_data <- survey_data[, c("lopnr", "mother_asthma", "mother_allergic_rhinitis", "mother_eczema", "father_asthma", "father_allergic_rhinitis", "father_eczema", "siblings_asthma", "siblings_allergic_rhinitis", "siblings_eczema", "sex", "any_family_member_asthma_allergy", "mother_education", "mother_smoking_pregnancy", "mother_age", "c_section", "gestational_age", "birth_weight", "birth_order", "daycare_6m", "asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "eczema_1y_ever", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m", "survey_asthma_1", "survey_asthma_2", "survey_asthma_3", "survey_asthma_4", "survey_eczema_1", "survey_eczema_2", "survey_eczema_3", "survey_eczema_4", "survey_rhinitis_2", "survey_rhinitis_3", "survey_rhinitis_4")]
print('Selected variables of interest')
print(colnames(output_data))
final_dims <- paste0(dim(output_data), collapse = ',')
print(paste0('Dimensions of final dataset: ', final_dims))
# save to .Rds
output_data_filename <- paste0(output_location, "rds/", "survey-data-raw.rds")
saveRDS(output_data, output_data_filename)
print(paste0('Saved working survey dataset to .Rds: ', output_data_filename))
cat('-------- end\n\n')