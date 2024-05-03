# LOAD PACKAGES
packages_all = c("data.table", "haven", "ggplot2", "lubridate", "dplyr", "foreign")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/" # path to output data
all_years <- FALSE



# generate output file name depending on all_years
if (all_years) {
    input_data <- readRDS(paste0(output_location, "output/rds/npdr-data-all-years-long.rds"))
} else {
    input_data <- readRDS(paste0(output_location, "output/rds/npdr-data-6-time-points-long.rds"))
}
unique_lopnr <- unique(input_data$lopnr)



print('Dimensions of input_data')
print(dim(input_data))
print('Ages')
print(colnames(input_data))
print(table(input_data$age))



if (all_years) {
    # get each unique lopnr in a variable
    # make an empty data.table with columns lopnr, asthma_medication2, asthma_medication3, ..., asthma_medication12, rhinitis_medication2, rhinitis_medication3, ..., rhinitis_medication12, eczema_medication2, eczema_medication3, ..., eczema_medication12
    wide_output <- data.table(
        # set lopnr to the column lopnr in input_data,
        lopnr = unique_lopnr,
        asthma_medication2 = integer(),
        asthma_medication3 = integer(),
        asthma_medication4 = integer(),
        asthma_medication5 = integer(),
        asthma_medication6 = integer(),
        asthma_medication7 = integer(),
        asthma_medication8 = integer(),
        asthma_medication9 = integer(),
        asthma_medication10 = integer(),
        asthma_medication11 = integer(),
        asthma_medication12 = integer(),
        asthma_medication13 = integer(),
        rhinitis_medication2 = integer(),
        rhinitis_medication3 = integer(),
        rhinitis_medication4 = integer(),
        rhinitis_medication5 = integer(),
        rhinitis_medication6 = integer(),
        rhinitis_medication7 = integer(),
        rhinitis_medication8 = integer(),
        rhinitis_medication9 = integer(),
        rhinitis_medication10 = integer(),
        rhinitis_medication11 = integer(),
        rhinitis_medication12 = integer(),
        rhinitis_medication13 = integer(),
        eczema_medication2 = integer(),
        eczema_medication3 = integer(),
        eczema_medication4 = integer(),
        eczema_medication5 = integer(),
        eczema_medication6 = integer(),
        eczema_medication7 = integer(),
        eczema_medication8 = integer(),
        eczema_medication9 = integer(),
        eczema_medication10 = integer(),
        eczema_medication11 = integer(),
        eczema_medication12 = integer(),
        eczema_medication13 = integer()
    )
    print('dims of input before wide format a')
    print(dim(wide_output))
    print(colnames(wide_output))

    # loop through each unique lopnr
    for (subject in unique_lopnr) {
        # get the rows for the current lopnr
        rows <- input_data[lopnr == subject]
        # asthma
        wide_output[wide_output$lopnr == subject, asthma_medication2 := rows[rows$age == 2, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication3 := rows[rows$age == 3, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication4 := rows[rows$age == 4, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication5 := rows[rows$age == 5, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication6 := rows[rows$age == 6, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication7 := rows[rows$age == 7, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication8 := rows[rows$age == 8, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication9 := rows[rows$age == 9, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication10 := rows[rows$age == 10, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication11 := rows[rows$age == 11, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication12 := rows[rows$age == 12, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication13 := rows[rows$age == 13, asthma_medication]]
        # rhinitis
        wide_output[wide_output$lopnr == subject, rhinitis_medication2 := rows[rows$age == 2, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication3 := rows[rows$age == 3, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication4 := rows[rows$age == 4, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication5 := rows[rows$age == 5, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication6 := rows[rows$age == 6, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication7 := rows[rows$age == 7, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication8 := rows[rows$age == 8, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication9 := rows[rows$age == 9, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication10 := rows[rows$age == 10, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication11 := rows[rows$age == 11, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication12 := rows[rows$age == 12, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication13 := rows[rows$age == 13, rhinitis_medication]]
        # eczema
        wide_output[wide_output$lopnr == subject, eczema_medication2 := rows[rows$age == 2, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication3 := rows[rows$age == 3, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication4 := rows[rows$age == 4, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication5 := rows[rows$age == 5, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication6 := rows[rows$age == 6, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication7 := rows[rows$age == 7, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication8 := rows[rows$age == 8, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication9 := rows[rows$age == 9, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication10 := rows[rows$age == 10, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication11 := rows[rows$age == 11, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication12 := rows[rows$age == 12, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication13 := rows[rows$age == 13, eczema_medication]]
    }
    print('Done with filler loop of wide data')
    # # add a column age2, age3, ..., age12
    # wide_output[, age2 := 2]
    # wide_output[, age3 := 3]
    # wide_output[, age4 := 4]
    # wide_output[, age5 := 5]
    # wide_output[, age6 := 6]
    # wide_output[, age7 := 7]
    # wide_output[, age8 := 8]
    # wide_output[, age9 := 9]
    # wide_output[, age10 := 10]
    # wide_output[, age11 := 11]
    # wide_output[, age12 := 12]
    # wide_output[, age13 := 13]
    print('done with adding age columns to wide data')
    # print dimensions of input after wide format
    print('dims of input after wide format')
    print(dim(wide_output))
    # save wide format to dta
    write.dta(wide_output, "stata-input-all-years.dta")
} else {
    # get each unique lopnr in a variable
    # make an empty data.table with columns lopnr, asthma_medication2, asthma_medication3, ..., asthma_medication12, rhinitis_medication2, rhinitis_medication3, ..., rhinitis_medication12, eczema_medication2, eczema_medication3, ..., eczema_medication12
    wide_output <- data.table(
        # set lopnr to the column lopnr in input_data,
        lopnr = unique_lopnr,
        asthma_medication1 = integer(),
        asthma_medication2 = integer(),
        asthma_medication3 = integer(),
        asthma_medication4 = integer(),
        asthma_medication5 = integer(),
        asthma_medication6 = integer(),
        rhinitis_medication1 = integer(),
        rhinitis_medication2 = integer(),
        rhinitis_medication3 = integer(),
        rhinitis_medication4 = integer(),
        rhinitis_medication5 = integer(),
        rhinitis_medication6 = integer(),
        eczema_medication1 = integer(),
        eczema_medication2 = integer(),
        eczema_medication3 = integer(),
        eczema_medication4 = integer(),
        eczema_medication5 = integer(),
        eczema_medication6 = integer()
    )
    print('dims of input before wide format b')
    print(dim(wide_output))
    print(colnames(wide_output))

    # loop through each unique lopnr
    for (subject in unique_lopnr) {
        # get the rows for the current lopnr
        rows <- input_data[lopnr == subject]
        # asthma
        wide_output[wide_output$lopnr == subject, asthma_medication1 := rows[rows$age == 1, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication2 := rows[rows$age == 2, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication3 := rows[rows$age == 3, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication4 := rows[rows$age == 4, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication5 := rows[rows$age == 5, asthma_medication]]
        wide_output[wide_output$lopnr == subject, asthma_medication6 := rows[rows$age == 6, asthma_medication]]
        # rhinitis
        wide_output[wide_output$lopnr == subject, rhinitis_medication1 := rows[rows$age == 1, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication2 := rows[rows$age == 2, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication3 := rows[rows$age == 3, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication4 := rows[rows$age == 4, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication5 := rows[rows$age == 5, rhinitis_medication]]
        wide_output[wide_output$lopnr == subject, rhinitis_medication6 := rows[rows$age == 6, rhinitis_medication]]
        # eczema
        wide_output[wide_output$lopnr == subject, eczema_medication1 := rows[rows$age == 1, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication2 := rows[rows$age == 2, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication3 := rows[rows$age == 3, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication4 := rows[rows$age == 4, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication5 := rows[rows$age == 5, eczema_medication]]
        wide_output[wide_output$lopnr == subject, eczema_medication6 := rows[rows$age == 6, eczema_medication]]
    }
    print('Done with filler loop of wide data')
    # # add a column age2, age3, ..., age12
    # wide_output[, age1 := 2]
    # wide_output[, age2 := 4]
    # wide_output[, age3 := 6]
    # wide_output[, age4 := 8]
    # wide_output[, age5 := 10]
    # wide_output[, age6 := 12]
    print('done with adding age columns to wide data')
    # print dimensions of input after wide format
    print('dims of input after wide format')
    print(dim(wide_output)) 
}   

if (all_years) {
    saveRDS(wide_output, paste0(output_location, "output/rds/npdr-data-all-years-wide.rds"))
} else {
    saveRDS(wide_output, paste0(output_location, "output/rds/npdr-data-6-time-points-wide.rds"))
}