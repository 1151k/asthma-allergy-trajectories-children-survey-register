# LOAD PACKAGES
packages_all = c("data.table", "haven", "ggplot2", "lubridate", "dplyr", "foreign")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
NPDR_filename <- paste0(input_location, "SOS_grundfil_LKM20032016_181107.sav") # path and filename of input data (NPDR data)
survey_filename <- paste0(input_location, "SOS_grundfil_LOPNR_181106.sav") # path and filename of input data (survey data)
source("ATC_CODES.r") # load ATC codes
n_total_cohort <- 5654 # total number of subjects in CWS
all_years <- FALSE



# "generate" name of output file
if (all_years) {
    output_filename <- paste0(output_location, "rds/npdr-data-all-years-long")
} else {
    output_filename <- paste0(output_location, "rds/npdr-data-6-time-points-long")
}



# LOAD AND PREPROCESS SURVEY DATA
survey_data <- read.spss(survey_filename, to.data.frame=TRUE)
data.table::setDT(survey_data) # convert to data.table
print('Survey data loaded')
# get number of rows in dataset
print('Number of rows')
num_rows <- nrow(survey_data)
print(num_rows)
# get number of unique lopnr values
print('Number of unique lopnr values')
num_unique_lopnr_prior <- length(unique(survey_data$lopnr))
print(num_unique_lopnr_prior)
# get the most common lopnr value
print('Most common lopnr value')
most_common_lopnr <- as.integer(names(which.max(table(survey_data$lopnr))))
print(most_common_lopnr)
# get the most common lopnr value number of occurrences
print('Number of times the most common lopnr value number occurs')
num_same_lopnr <- max(table(survey_data$lopnr))
print(num_same_lopnr)
# get highest lopnr
print('Highest lopnr')
highest_lopnr_prior <- max(survey_data$lopnr)
print(highest_lopnr_prior)
# get lowest lopnr
print('Lowest lopnr')
lowest_lopnr_prior <- min(survey_data$lopnr)
print(lowest_lopnr_prior)
# for the 619 subjets with lopnr 2671, set unique lopnr values, starting at highest_lopnr_prior + 1
survey_data$lopnr[survey_data$lopnr == most_common_lopnr] <- seq(highest_lopnr_prior + 1, highest_lopnr_prior + num_same_lopnr)
# set the highest lopnr to most_common_lopnr (so as to not have any gaps in the lopnr values)
survey_data$lopnr[survey_data$lopnr == max(survey_data$lopnr)] <- as.integer(most_common_lopnr)
print('Finished setting lopnr values for the 619 subjets with lopnr 2671')
print('Number of unique lopnr values')
print(length(unique(survey_data$lopnr)))
# get highest lopnr
print('Highest lopnr')
print(max(survey_data$lopnr))
# get lowest lopnr
print('Lowest lopnr')
print(min(survey_data$lopnr))



# CREATE input_data DATASET
# create input_data with all lopnr values and years from 2005 to 2015
input_data <- expand.grid(lopnr = unique(survey_data$lopnr), year = 2005:2016)
# Convert to data.table
setDT(input_data)
# order input_data by lopnr and year
input_data <- input_data[order(input_data$lopnr, input_data$year), ]
# add column asthma_medication, eczema_medication, rhinitis_medication
input_data$asthma_medication <- 0
input_data$eczema_medication <- 0
input_data$rhinitis_medication <- 0



# LOAD AND PREPROCESS NPDR DATA
NPDR_data <- read_sav(NPDR_filename)
# # exclude data from year 2016 (based on YYYY-MM-DD column EDATUM)
# NPDR_data <- NPDR_data[format(as.Date(NPDR_data$EDATUM, format="%Y-%m-%d"), "%Y") != "2016",]
data.table::setDT(NPDR_data) # convert to data.table
print('NPDR data loaded')
print('Earliest prescription fill date')
print(min(NPDR_data$EDATUM))
print('Latest prescription fill date')
print(max(NPDR_data$EDATUM))
print('Number of unique subjects')
print(length(unique(NPDR_data$lopnr)))
print('Years with data')
print(unique(format(as.Date(NPDR_data$EDATUM, format="%Y-%m-%d"), "%Y")))
print('Number of years with data')
print(length(unique(format(as.Date(NPDR_data$EDATUM, format="%Y-%m-%d"), "%Y"))))



# Convert EDATUM to Date, extract the year, and convert to integer
NPDR_data$year <- as.integer(format(as.Date(NPDR_data$EDATUM, format="%Y-%m-%d"), "%Y"))
# set year to numeric in input_data
input_data$year <- as.numeric(input_data$year)

# keep only the columns lopnr, year, atc
NPDR_data <- NPDR_data[, .(lopnr, year, atc)]
# print first 10 rows of NPDR_data
print('Head of NPDR_data')
print(head(NPDR_data, 10))
# check the data type of each column in input_data
print('Data type of each column in input_data')
print(sapply(input_data, class))
# set year to numeric in NPDR_data
NPDR_data$year <- as.numeric(NPDR_data$year)
# check the data type of each column in NPDR_data
print('Data type of each column in NPDR_data')
print(sapply(NPDR_data, class))

# ASTHMA
# Create a temporary column in NPDR_data that indicates if there is any asthma medication for that lopnr and year
NPDR_data[, asthma_medication := 0]  # Initialize the column with 0
for (code in asthma_atc_codes) {
  NPDR_data[grepl(paste0("^", code), atc), asthma_medication := 1]
}
print('NPDR asthma_medication table')
print(table(NPDR_data$asthma_medication))
# Join input_data and NPDR_data on lopnr and year, and update asthma_medication in input_data
input_data[NPDR_data, on = .(lopnr, year), asthma_medication := pmax(asthma_medication, i.asthma_medication, na.rm = TRUE)]
# Remove the temporary column in NPDR_data
NPDR_data[, asthma_medication := NULL]
# ECZEMA
# Create a temporary column in NPDR_data that indicates if there is any eczema medication for that lopnr and year
NPDR_data[, eczema_medication := 0]   # Initialize the column with 0
for (code in eczema_atc_codes) {
  NPDR_data[grepl(paste0("^", code), atc), eczema_medication := 1]
}
print('NPDR eczema_medication table')
print(table(NPDR_data$eczema_medication))
# Join input_data and NPDR_data on lopnr and year, and update eczema_medication in input_data
input_data[NPDR_data, on = .(lopnr, year), eczema_medication := pmax(eczema_medication, i.eczema_medication, na.rm = TRUE)]
# Remove the temporary column in NPDR_data
NPDR_data[, eczema_medication := NULL]
# RHINITIS
# Create a temporary column in NPDR_data that indicates if there is any rhinitis medication for that lopnr and year
NPDR_data[, rhinitis_medication := 0]  # Initialize the column with 0
for (code in rhinitis_atc_codes) {
  NPDR_data[grepl(paste0("^", code), atc), rhinitis_medication := 1]
}
print('NPDR rhinitis_medication table')
print(table(NPDR_data$rhinitis_medication))
# Join input_data and NPDR_data on lopnr and year, and update rhinitis_medication in input_data
input_data[NPDR_data, on = .(lopnr, year), rhinitis_medication := pmax(rhinitis_medication, i.rhinitis_medication, na.rm = TRUE)]
# Remove the temporary column in NPDR_data
NPDR_data[, rhinitis_medication := NULL]
# AGE
# create age varible and exchange the year variable with it
input_data <- input_data %>%
    mutate(age = year - 2003)
input_data$year <- NULL



# # print first 1000 rows of input_data
# print(head(input_data, 1000), n = 1000)
print('table for asthma_medication')
print(table(input_data$asthma_medication))
print('table for eczema_medication')
print(table(input_data$eczema_medication))
print('table for rhinitis_medication')
print(table(input_data$rhinitis_medication))



# loop each unique lopnr and check if all asthma_medication, eczema_medication, and rhinitis_medication are 0
lopnr_with_zeros <- input_data %>%
  group_by(lopnr) %>%
  filter(all(asthma_medication == 0 & eczema_medication == 0 & rhinitis_medication == 0)) %>%
  distinct(lopnr)
# print results
print('lopnr with only zeros in the medication columns')
print(lopnr_with_zeros)
# # get the input_data rows with lopnr in lopnr_with_zeros, print out the data for the first 10 lopnr without truncation
# print('input_data rows with lopnr in lopnr_with_zeros')
# options(datatable.print.nrows = 10000)
# head(input_data[input_data$lopnr %in% lopnr_with_zeros$lopnr, ], n = 100*11)
# get length of lopnr_with_zeros
print('Length of lopnr_with_zeros')
print(length(lopnr_with_zeros$lopnr))
# get length of unique lopnr in lopnr_with_zeros
print('Unique lopnr in lopnr_with_zeros')
print(length(unique(lopnr_with_zeros$lopnr)))
# get length of unique lopnr in input_data
print('Unique lopnr in input_data')
print(length(unique(input_data$lopnr)))
# print('Dimensions of input_data before removing lopnr with only zeros in the medication columns')
# print(dim(input_data))
# # remove lopnr_with_zeros from input_data
# input_data <- input_data %>%
#   filter(!lopnr %in% lopnr_with_zeros$lopnr)
# print('Dimensions of input_data after removing lopnr with only zeros in the medication columns')
# print(dim(input_data))
# get length of unique lopnr in input_data after removing lopnr_with_zeros
# print('Unique lopnr in input_data after removing lopnr with only zeros in the medication columns')
# print(length(unique(input_data$lopnr)))



# print table of asthma_medication for age = 13
print('table for asthma_medication for age = 13')
print(table(input_data$asthma_medication[input_data$age == 13]))
# print table of eczema_medication for age = 13
print('table for eczema_medication for age = 13')
print(table(input_data$eczema_medication[input_data$age == 13]))
# print table of rhinitis_medication for age = 13
print('table for rhinitis_medication for age = 13')
print(table(input_data$rhinitis_medication[input_data$age == 13]))
options(datatable.print.nrows = 20000)
print(head(input_data, 700))



# resize data to 2-year strata, 2-3 years, 4-5 years etc
if (!all_years) {
    # resize the yearly measures
    # update asthma_medication in input_data where age == 2 or age == 3 to be the sum of asthma_medication in input_data where age == 2 and age == 3
    input_data <- input_data %>%
        group_by(lopnr) %>%
        # year 2
        mutate(asthma_medication = ifelse(age == 2, sum(asthma_medication[age %in% c(2, 3)]), asthma_medication)) %>%
        mutate(eczema_medication = ifelse(age == 2, sum(eczema_medication[age %in% c(2, 3)]), eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(age == 2, sum(rhinitis_medication[age %in% c(2, 3)]), rhinitis_medication)) %>%
        # year 4
        mutate(asthma_medication = ifelse(age == 4, sum(asthma_medication[age %in% c(4, 5)]), asthma_medication)) %>%
        mutate(eczema_medication = ifelse(age == 4, sum(eczema_medication[age %in% c(4, 5)]), eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(age == 4, sum(rhinitis_medication[age %in% c(4, 5)]), rhinitis_medication)) %>%
        # year 6
        mutate(asthma_medication = ifelse(age == 6, sum(asthma_medication[age %in% c(6, 7)]), asthma_medication)) %>%
        mutate(eczema_medication = ifelse(age == 6, sum(eczema_medication[age %in% c(6, 7)]), eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(age == 6, sum(rhinitis_medication[age %in% c(6, 7)]), rhinitis_medication)) %>%
        # year 8
        mutate(asthma_medication = ifelse(age == 8, sum(asthma_medication[age %in% c(8, 9)]), asthma_medication)) %>%
        mutate(eczema_medication = ifelse(age == 8, sum(eczema_medication[age %in% c(8, 9)]), eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(age == 8, sum(rhinitis_medication[age %in% c(8, 9)]), rhinitis_medication)) %>%
        # year 10
        mutate(asthma_medication = ifelse(age == 10, sum(asthma_medication[age %in% c(10, 11)]), asthma_medication)) %>%
        mutate(eczema_medication = ifelse(age == 10, sum(eczema_medication[age %in% c(10, 11)]), eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(age == 10, sum(rhinitis_medication[age %in% c(10, 11)]), rhinitis_medication)) %>%
        # year 12
        mutate(asthma_medication = ifelse(age == 12, sum(asthma_medication[age %in% c(12, 13)]), asthma_medication)) %>%
        mutate(eczema_medication = ifelse(age == 12, sum(eczema_medication[age %in% c(12, 13)]), eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(age == 12, sum(rhinitis_medication[age %in% c(12, 13)]), rhinitis_medication)) %>%
        ungroup()
    setDT(input_data)
    # set asthma_medication to 1 if it's 2, same with eczema_medication and rhinitis_medication
    input_data <- input_data %>%
        mutate(asthma_medication = ifelse(asthma_medication == 2, 1, asthma_medication)) %>%
        mutate(eczema_medication = ifelse(eczema_medication == 2, 1, eczema_medication)) %>%
        mutate(rhinitis_medication = ifelse(rhinitis_medication == 2, 1, rhinitis_medication))
    # remove rows where age == 3, 5, 7, 9, 11
    input_data <- input_data[!age %in% c(3, 5, 7, 9, 11, 13), ]
    print(head(input_data, 200))
    print('Finished resizing the yearly measures')
    print(dim(input_data))
    # set age to 1 if age is 2
    input_data$age[input_data$age == 2] <- 1
    # set age to 2 if age is 4
    input_data$age[input_data$age == 4] <- 2
    # set age to 3 if age is 6
    input_data$age[input_data$age == 6] <- 3
    # set age to 4 if age is 8
    input_data$age[input_data$age == 8] <- 4
    # set age to 5 if age is 10
    input_data$age[input_data$age == 10] <- 5
    # set age to 6 if age is 12
    input_data$age[input_data$age == 12] <- 6
    print(table(input_data$age))
}


# save to .rds
print('dimensions of medication data before saving')
print(dim(input_data))
print(colnames(input_data))
saveRDS(input_data, paste0(output_filename, ".rds"))