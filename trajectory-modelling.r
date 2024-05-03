# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "poLCA")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
imputed_i <- 1:100
data_source <- 'combination'
time_points <- 6
ng <- 1:12
n_iterations <- 5000
n_starts <- 100
only_positive_cases <- FALSE # whether to only include those with at least one positive outcome
stratified <- '' # if not empty, stratify by a) sex or b) heredity for asthma/allergy



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
print(paste('Imputed datasets to be analyzed:', paste(imputed_i, collapse = ', ')))
print(paste('Data source:', data_source))
print(paste('Time points:', time_points))
print(paste('Number of classes to be tested:', paste(ng, collapse = ', ')))
print(paste('Number of iterations:', n_iterations))
print(paste('Number of starts:', n_starts))
print(paste('Only positive cases:', only_positive_cases))
print(paste('Stratified by:', stratified))
cat('-------- end\n\n')




for (i in imputed_i) {
    cat(paste('IMPUTED DATASET', i, '\n'))
    cat('-------- start\n')

    # load input data
    if (data_source == 'survey') {
        input_data <- readRDS(paste0(output_location, "rds/", "survey-data-imputed-", i, ".rds"))
    } else if (data_source == 'medication') {
        if (time_points == 6) {
            input_data <- readRDS(paste0(output_location, "rds/", "wide-output-6-time-points.rds"))
        } else {
            input_data <- readRDS(paste0(output_location, "rds/", "wide-output-all-years.rds"))
        }
    } else if (data_source == 'combination') {
        input_data <- readRDS(paste0(output_location, "rds/imputed-data-", i, ".rds"))
        # TEMP!
        # input_data <- readRDS(paste0(output_location, "rds/non-imputed-complete-data.rds"))
    }
    # convert to data.table
    data.table::setDT(input_data)
    # print('Loaded dataset')
    # print(dim(input_data))

    # if stratified, stratify
    if (stratified != '') {
        if (stratified == 'boys' | stratified == 'girls') {
            if (stratified == 'boys') {
                input_data <- input_data[sex == 0, ]
            } else {
                input_data <- input_data[sex == 1, ]
            }
        } else if (stratified == 'heredity' | stratified == 'no-heredity') {
            if (stratified == 'heredity') {
                input_data <- input_data[any_family_member_asthma_allergy == 1, ]
            } else {
                input_data <- input_data[any_family_member_asthma_allergy == 0, ]
            }
        }
    }
    
    # preprocess
    # keep only relevant variables/columns
    if (data_source == 'survey') {
        input_data <- input_data[, .(
            survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4,
            survey_eczema_1, survey_eczema_2, survey_eczema_3, survey_eczema_4,
            survey_rhinitis_2, survey_rhinitis_3, survey_rhinitis_4
        )]
    } else if (data_source == 'medication') {
        input_data <- input_data[, .(
            asthma_medication1, asthma_medication2, asthma_medication3, asthma_medication4, asthma_medication5, asthma_medication6, eczema_medication1, eczema_medication2, eczema_medication3, eczema_medication4, eczema_medication5, eczema_medication6, rhinitis_medication1, rhinitis_medication2, rhinitis_medication3, rhinitis_medication4, rhinitis_medication5, rhinitis_medication6
        )]
    } else if (data_source == 'combination') {
        input_data <- input_data[, .(
            survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4,
            survey_eczema_1, survey_eczema_2, survey_eczema_3, survey_eczema_4,
            survey_rhinitis_2, survey_rhinitis_3, survey_rhinitis_4,
            asthma_medication1, asthma_medication2, asthma_medication3, asthma_medication4, asthma_medication5, asthma_medication6, eczema_medication1, eczema_medication2, eczema_medication3, eczema_medication4, eczema_medication5, eczema_medication6, rhinitis_medication1, rhinitis_medication2, rhinitis_medication3, rhinitis_medication4, rhinitis_medication5, rhinitis_medication6
        )]
    }
    # remove completely healthy subjects if needed
    if (only_positive_cases) {
        # keep only subjects that have at least one column with value 1
        input_data <- input_data[rowSums(input_data) > 0]
    }
    # define formula for LCA
    if (data_source == 'survey') {
        lca_formula <- with(input_data, cbind(survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4, survey_eczema_1, survey_eczema_2, survey_eczema_3, survey_eczema_4, survey_rhinitis_2, survey_rhinitis_3, survey_rhinitis_4)~1)
    } else if (data_source == 'medication') {
        lca_formula <- with(input_data, cbind(asthma_medication1, asthma_medication2, asthma_medication3, asthma_medication4, asthma_medication5, asthma_medication6, eczema_medication1, eczema_medication2, eczema_medication3, eczema_medication4, eczema_medication5, eczema_medication6, rhinitis_medication1, rhinitis_medication2, rhinitis_medication3, rhinitis_medication4, rhinitis_medication5, rhinitis_medication6)~1)
    } else if (data_source == 'combination') {
        lca_formula <- with(input_data, cbind(survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4, survey_eczema_1, survey_eczema_2, survey_eczema_3, survey_eczema_4, survey_rhinitis_2, survey_rhinitis_3, survey_rhinitis_4, asthma_medication1, asthma_medication2, asthma_medication3, asthma_medication4, asthma_medication5, asthma_medication6, eczema_medication1, eczema_medication2, eczema_medication3, eczema_medication4, eczema_medication5, eczema_medication6, rhinitis_medication1, rhinitis_medication2, rhinitis_medication3, rhinitis_medication4, rhinitis_medication5, rhinitis_medication6)~1)
    }
    # TEMP!
    # set all columns to numeric
    input_data <- as.data.table(lapply(input_data, as.numeric))
    # get dimensions of input data
    input_data_dims <- paste0(dim(input_data), collapse = ',')
    print(paste('Dimensions of input data:', input_data_dims))
    # add one to each value to avoid 0s
    input_data <- input_data + 1

    # loop through number of classes
    for (k in ng) {
        # for reproducibility
        set.seed(1)
        # run LCA
        lca_instance <- poLCA::poLCA(
            lca_formula,
            data = input_data,
            nclass = k,
            nrep = n_starts,
            maxiter = n_iterations,
            verbose = FALSE
        )
        print(paste('LCA with', k, 'classes done'))
        # print(lca_instance)
        # get labels
        class_labels <- lca_instance$predclass
        class_sizes <- table(class_labels)
        # # get percentage of each class of the full study population
        # class_percentages <- round((class_sizes/nrow(input_data))*100, 2)
        # posterior probabilities
        posterior_probabilities <- lca_instance$posterior
        probs <- lca_instance$probs
        probs_se <- lca_instance$probs.se
        print('class_labels')
        print(class_labels)
        # loop each values in class_labels; the value is the index (column to look at the corresponding row in the posterior column). Create one vector for each class, and then add the corresponding values from the posterior column to the vector
        class_probabilities <- lapply(1:k, function(x) {
            class_indices <- which(class_labels == x)
            class_posterior_probabilities <- posterior_probabilities[class_indices, x]
            return(class_posterior_probabilities)
        })
        # get average probability for each class
        mean_class_probabilities <- lapply(class_probabilities, function(x) {
            # get the percentage of subjects with x < 0.8
            # low_values <- sum(x < 0.8)/length(x)
            print(paste('<0.8 for class:', sum(x < 0.8)/length(x)))
            print(paste('<0.7 for class:', sum(x < 0.7)/length(x)))
            print(mean(x))
            print('--')
            return(mean(x))
        })
        print('mean_class_probabilities')
        print(mean_class_probabilities)
        # fit metrics and general statistics
        n_iter <- lca_instance$numiter
        bic <- lca_instance$bic
        aic <- lca_instance$aic
        print(paste('Number of iterations until convergence:', n_iter))
        print(paste('BIC:', bic))
        print(paste('AIC:', aic))
        # print(paste('Class sizes:'))
        # print(class_sizes)
        # print(paste('Class percentages:'))
        # print(class_percentages)
        # save .Rds (one for labels and one for posterior probabilities), with descriptive file names (containing source_data, i, ng, n_iterations, n_starts, and only_positive_cases in the file name)
        file_name_labels <- paste0(
            output_location,
            "rds/",
            "class-labels-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        file_name_probs <- paste0(
            output_location,
            "rds/",
            "probs-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        file_name_probs_se <- paste0(
            output_location,
            "rds/",
            "probs-se-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        file_name_n_iter <- paste0(
            output_location,
            "rds/",
            "n-iter-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        file_name_lca_instance <- paste0(
            output_location,
            "rds/",
            "lca-instance-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        saveRDS(class_labels, file_name_labels)
        saveRDS(probs, file_name_probs)
        saveRDS(probs_se, file_name_probs_se)
        saveRDS(n_iter, file_name_n_iter)
        saveRDS(lca_instance, file_name_lca_instance)
        # save mean class probabilities
        file_name_mean_class_probabilities <- paste0(
            output_location,
            "rds/",
            "mean-class-probabilities-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        saveRDS(mean_class_probabilities, file_name_mean_class_probabilities)
        # save class labels and class sizes
        file_name_class_labels <- paste0(
            output_location,
            "rds/",
            "class-labels-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        saveRDS(class_labels, file_name_class_labels)
        file_name_class_sizes <- paste0(
            output_location,
            "rds/",
            "class-sizes-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        saveRDS(class_sizes, file_name_class_sizes)
        print('--------')
        # save BIC and AIC
        file_name_bic <- paste0(
            output_location,
            "rds/",
            "bic-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        saveRDS(bic, file_name_bic)
        file_name_aic <- paste0(
            output_location,
            "rds/",
            "aic-",
            data_source,
            "-dataset-",
            i,
            "-ng-",
            k,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".rds"
        )
        saveRDS(aic, file_name_aic)
    }
    cat('-------- end\n\n')
}



