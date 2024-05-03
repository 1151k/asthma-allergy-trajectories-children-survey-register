# PURPOSE: TO COMBINE SURVEY AND MEDICATION DATA



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
include_all_years <- FALSE



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
cat('-------- end\n\n')



# LOAD DATA
cat('LOAD DATA\n')
cat('-------- start\n')
# survey data
survey_data <- readRDS(paste0(output_location, "rds/", "survey-data-raw.rds"))
survey_data_dims <- paste(dim(survey_data), collapse = ',')
print('Loaded survey data')
print(paste('Dimnesions of survey data:', survey_data_dims))
# medication data
if (include_all_years) {
    medication_data <- readRDS(paste0(output_location, "rds/npdr-data-6-time-points-wide.rds"))
} else {
    medication_data <- readRDS(paste0(output_location, "rds/npdr-data-6-time-points-wide.rds"))
}
medication_data_dims <- paste(dim(medication_data), collapse = ',')
print('Loaded medication data')
print(paste('Dimnesions of medication data:', medication_data_dims))
print(colnames(medication_data))
cat('-------- end\n\n')



# MERGE DATA 
# merge by lopnr, and keep all rows from survey_data
cat('MERGE DATA\n')
cat('-------- start\n')
merged_data <- merge(survey_data, medication_data, by = 'lopnr')
print('Merged data')
print(dim(merged_data))
# print first 1000 rows
print(head(merged_data, 1000))
# save to .Rds
saveRDS(merged_data, paste0(output_location, "rds/", "combined-survey-and-medication-data-raw.rds"))
cat('-------- end\n\n')
