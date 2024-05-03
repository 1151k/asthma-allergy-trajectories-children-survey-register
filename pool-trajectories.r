# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "gridExtra", "gtsummary", "flextable", "haven")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
n_datasets <- 100
imputed_i <- 1:n_datasets
data_source <- 'combination'
time_points <- 6
ng <- 9
n_iterations <- 5000
n_starts <- 100
only_positive_cases <- FALSE # whether to only include those with at least one positive outcome
stratified <- ''
n_total_cohort <- 5654



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
print(paste('Datasets:', paste(imputed_i, collapse = ', ')))
print(paste('Data source:', data_source))
print(paste('Time points:', time_points))
print(paste('Classes:', paste(ng, collapse = ', ')))
print(paste('Iterations:', n_iterations))
print(paste('Starts:', n_starts))
print(paste('Only positive cases:', only_positive_cases))
print(paste('Stratified by:', stratified))
cat('-------- end\n\n')



# PREPARE NECESSARY VARIABLES
# data.table to store posterior probabilities (class-level)
posterior_dt <- data.table(
    dataset = numeric(),
    class = numeric(),
    survey_asthma_1 = numeric(),
    survey_asthma_2 = numeric(),
    survey_asthma_3 = numeric(),
    survey_asthma_4 = numeric(),
    survey_eczema_1 = numeric(),
    survey_eczema_2 = numeric(),
    survey_eczema_3 = numeric(),
    survey_eczema_4 = numeric(),
    survey_rhinitis_2 = numeric(),
    survey_rhinitis_3 = numeric(),
    survey_rhinitis_4 = numeric(),
    asthma_medication1 = numeric(),
    asthma_medication2 = numeric(),
    asthma_medication3 = numeric(),
    asthma_medication4 = numeric(),
    asthma_medication5 = numeric(),
    asthma_medication6 = numeric(),
    eczema_medication1 = numeric(),
    eczema_medication2 = numeric(),
    eczema_medication3 = numeric(),
    eczema_medication4 = numeric(),
    eczema_medication5 = numeric(),
    eczema_medication6 = numeric(),
    rhinitis_medication1 = numeric(),
    rhinitis_medication2 = numeric(),
    rhinitis_medication3 = numeric(),
    rhinitis_medication4 = numeric(),
    rhinitis_medication5 = numeric(),
    rhinitis_medication6 = numeric()
)
# data.table to store posterior probability SEs (class-level)
probs_se_dt <- data.table(
    dataset = numeric(),
    class = numeric(),
    survey_asthma_1 = numeric(),
    survey_asthma_2 = numeric(),
    survey_asthma_3 = numeric(),
    survey_asthma_4 = numeric(),
    survey_eczema_1 = numeric(),
    survey_eczema_2 = numeric(),
    survey_eczema_3 = numeric(),
    survey_eczema_4 = numeric(),
    survey_rhinitis_2 = numeric(),
    survey_rhinitis_3 = numeric(),
    survey_rhinitis_4 = numeric(),
    asthma_medication1 = numeric(),
    asthma_medication2 = numeric(),
    asthma_medication3 = numeric(),
    asthma_medication4 = numeric(),
    asthma_medication5 = numeric(),
    asthma_medication6 = numeric(),
    eczema_medication1 = numeric(),
    eczema_medication2 = numeric(),
    eczema_medication3 = numeric(),
    eczema_medication4 = numeric(),
    eczema_medication5 = numeric(),
    eczema_medication6 = numeric(),
    rhinitis_medication1 = numeric(),
    rhinitis_medication2 = numeric(),
    rhinitis_medication3 = numeric(),
    rhinitis_medication4 = numeric(),
    rhinitis_medication5 = numeric(),
    rhinitis_medication6 = numeric()
)
# list to store class labels (subject-level)
class_label_list <- list()
# data.table to store class labels for majority vote (subject-level)
label_dt <- data.table(
    majority_vote = numeric(n_total_cohort),
    majority_count = numeric(n_total_cohort),
    dataset1 = numeric(n_total_cohort),
    dataset2 = numeric(n_total_cohort),
    dataset3 = numeric(n_total_cohort),
    dataset4 = numeric(n_total_cohort),
    dataset5 = numeric(n_total_cohort),
    dataset6 = numeric(n_total_cohort),
    dataset7 = numeric(n_total_cohort),
    dataset8 = numeric(n_total_cohort),
    dataset9 = numeric(n_total_cohort),
    dataset10 = numeric(n_total_cohort),
    dataset11 = numeric(n_total_cohort),
    dataset12 = numeric(n_total_cohort),
    dataset13 = numeric(n_total_cohort),
    dataset14 = numeric(n_total_cohort),
    dataset15 = numeric(n_total_cohort),
    dataset16 = numeric(n_total_cohort),
    dataset17 = numeric(n_total_cohort),
    dataset18 = numeric(n_total_cohort),
    dataset19 = numeric(n_total_cohort),
    dataset20 = numeric(n_total_cohort),
    dataset21 = numeric(n_total_cohort),
    dataset22 = numeric(n_total_cohort),
    dataset23 = numeric(n_total_cohort),
    dataset24 = numeric(n_total_cohort),
    dataset25 = numeric(n_total_cohort),
    dataset26 = numeric(n_total_cohort),
    dataset27 = numeric(n_total_cohort),
    dataset28 = numeric(n_total_cohort),
    dataset29 = numeric(n_total_cohort),
    dataset30 = numeric(n_total_cohort),
    dataset31 = numeric(n_total_cohort),
    dataset32 = numeric(n_total_cohort),
    dataset33 = numeric(n_total_cohort),
    dataset34 = numeric(n_total_cohort),
    dataset35 = numeric(n_total_cohort),
    dataset36 = numeric(n_total_cohort),
    dataset37 = numeric(n_total_cohort),
    dataset38 = numeric(n_total_cohort),
    dataset39 = numeric(n_total_cohort),
    dataset40 = numeric(n_total_cohort),
    dataset41 = numeric(n_total_cohort),
    dataset42 = numeric(n_total_cohort),
    dataset43 = numeric(n_total_cohort),
    dataset44 = numeric(n_total_cohort),
    dataset45 = numeric(n_total_cohort),
    dataset46 = numeric(n_total_cohort),
    dataset47 = numeric(n_total_cohort),
    dataset48 = numeric(n_total_cohort),
    dataset49 = numeric(n_total_cohort),
    dataset50 = numeric(n_total_cohort),
    dataset51 = numeric(n_total_cohort),
    dataset52 = numeric(n_total_cohort),
    dataset53 = numeric(n_total_cohort),
    dataset54 = numeric(n_total_cohort),
    dataset55 = numeric(n_total_cohort),
    dataset56 = numeric(n_total_cohort),
    dataset57 = numeric(n_total_cohort),
    dataset58 = numeric(n_total_cohort),
    dataset59 = numeric(n_total_cohort),
    dataset60 = numeric(n_total_cohort),
    dataset61 = numeric(n_total_cohort),
    dataset62 = numeric(n_total_cohort),
    dataset63 = numeric(n_total_cohort),
    dataset64 = numeric(n_total_cohort),
    dataset65 = numeric(n_total_cohort),
    dataset66 = numeric(n_total_cohort),
    dataset67 = numeric(n_total_cohort),
    dataset68 = numeric(n_total_cohort),
    dataset69 = numeric(n_total_cohort),
    dataset70 = numeric(n_total_cohort),
    dataset71 = numeric(n_total_cohort),
    dataset72 = numeric(n_total_cohort),
    dataset73 = numeric(n_total_cohort),
    dataset74 = numeric(n_total_cohort),
    dataset75 = numeric(n_total_cohort),
    dataset76 = numeric(n_total_cohort),
    dataset77 = numeric(n_total_cohort),
    dataset78 = numeric(n_total_cohort),
    dataset79 = numeric(n_total_cohort),
    dataset80 = numeric(n_total_cohort),
    dataset81 = numeric(n_total_cohort),
    dataset82 = numeric(n_total_cohort),
    dataset83 = numeric(n_total_cohort),
    dataset84 = numeric(n_total_cohort),
    dataset85 = numeric(n_total_cohort),
    dataset86 = numeric(n_total_cohort),
    dataset87 = numeric(n_total_cohort),
    dataset88 = numeric(n_total_cohort),
    dataset89 = numeric(n_total_cohort),
    dataset90 = numeric(n_total_cohort),
    dataset91 = numeric(n_total_cohort),
    dataset92 = numeric(n_total_cohort),
    dataset93 = numeric(n_total_cohort),
    dataset94 = numeric(n_total_cohort),
    dataset95 = numeric(n_total_cohort),
    dataset96 = numeric(n_total_cohort),
    dataset97 = numeric(n_total_cohort),
    dataset98 = numeric(n_total_cohort),
    dataset99 = numeric(n_total_cohort),
    dataset100 = numeric(n_total_cohort)
)



# GET POSTERIOR PROBABILITIES AND CLASS LABELS
# loop all values of imputed_i (datasets)
cat('GET POSTERIOR PROBABILITIES AND CLASS LABELS\n')
cat('-------- start\n')
for (i in imputed_i) {
    # class labels
    # get class labels for this model (a flat list with the most likely class for each individual)
    class_labels <- readRDS(paste0(output_location, "rds/", "class-labels-", data_source, "-dataset-", i, "-ng-", ng, "-n_iterations-", n_iterations, "-n_starts-", n_starts, "-only_positive_cases-", only_positive_cases, stratified, ".rds"))
    # add class labels to class_label_list 
    class_label_list <- append(class_label_list, list(class_labels))
    # posterior probabilities
    # get posterior probabilities for this model (probability for not having (first column) and having (second column) the outcome for each variable for each class)
    probs <- readRDS(paste0(output_location, "rds/", "probs-", data_source, "-dataset-", i, "-ng-", ng, "-n_iterations-", n_iterations, "-n_starts-", n_starts, "-only_positive_cases-", only_positive_cases, stratified, ".rds"))
    # posterior probability SEs
    # get posterior probability SEs for this model
    probs_se <- readRDS(paste0(output_location, "rds/", "probs-se-", data_source, "-dataset-", i, "-ng-", ng, "-n_iterations-", n_iterations, "-n_starts-", n_starts, "-only_positive_cases-", only_positive_cases, stratified, ".rds"))
    # add posterior probabilities and corresponding SEs to posterior_dt and probs_se_dt, respectively
    for (class in 1:ng) {
        # make new row for posterior_dt and add it
        new_row <- list(
            i,
            class,
            probs$survey_asthma_1[class,2],
            probs$survey_asthma_2[class,2],
            probs$survey_asthma_3[class,2],
            probs$survey_asthma_4[class,2],
            probs$survey_eczema_1[class,2],
            probs$survey_eczema_2[class,2],
            probs$survey_eczema_3[class,2],
            probs$survey_eczema_4[class,2],
            probs$survey_rhinitis_2[class,2],
            probs$survey_rhinitis_3[class,2],
            probs$survey_rhinitis_4[class,2],
            probs$asthma_medication1[class,2],
            probs$asthma_medication2[class,2],
            probs$asthma_medication3[class,2],
            probs$asthma_medication4[class,2],
            probs$asthma_medication5[class,2],
            probs$asthma_medication6[class,2],
            probs$eczema_medication1[class,2],
            probs$eczema_medication2[class,2],
            probs$eczema_medication3[class,2],
            probs$eczema_medication4[class,2],
            probs$eczema_medication5[class,2],
            probs$eczema_medication6[class,2],
            probs$rhinitis_medication1[class,2],
            probs$rhinitis_medication2[class,2],
            probs$rhinitis_medication3[class,2],
            probs$rhinitis_medication4[class,2],
            probs$rhinitis_medication5[class,2],
            probs$rhinitis_medication6[class,2]
        )
        posterior_dt <- rbindlist(list(posterior_dt, new_row), fill = TRUE)
        # make new row for probs_se_dt and add it
        new_row_se <- list(
            i,
            class,
            probs_se$survey_asthma_1[class,2],
            probs_se$survey_asthma_2[class,2],
            probs_se$survey_asthma_3[class,2],
            probs_se$survey_asthma_4[class,2],
            probs_se$survey_eczema_1[class,2],
            probs_se$survey_eczema_2[class,2],
            probs_se$survey_eczema_3[class,2],
            probs_se$survey_eczema_4[class,2],
            probs_se$survey_rhinitis_2[class,2],
            probs_se$survey_rhinitis_3[class,2],
            probs_se$survey_rhinitis_4[class,2],
            probs_se$asthma_medication1[class,2],
            probs_se$asthma_medication2[class,2],
            probs_se$asthma_medication3[class,2],
            probs_se$asthma_medication4[class,2],
            probs_se$asthma_medication5[class,2],
            probs_se$asthma_medication6[class,2],
            probs_se$eczema_medication1[class,2],
            probs_se$eczema_medication2[class,2],
            probs_se$eczema_medication3[class,2],
            probs_se$eczema_medication4[class,2],
            probs_se$eczema_medication5[class,2],
            probs_se$eczema_medication6[class,2],
            probs_se$rhinitis_medication1[class,2],
            probs_se$rhinitis_medication2[class,2],
            probs_se$rhinitis_medication3[class,2],
            probs_se$rhinitis_medication4[class,2],
            probs_se$rhinitis_medication5[class,2],
            probs_se$rhinitis_medication6[class,2]
        )
        probs_se_dt <- rbindlist(list(probs_se_dt, new_row_se), fill = TRUE)
    }
    print(paste('Dataset', i, 'done'))
}
# multiply all values in class_label_list by 100 to be able to switch classes more easily
class_label_list <- lapply(class_label_list, function(x) x * 100)
# multiply all values in posterior_dt by 100 to be able to switch classes more easily
posterior_dt[, class := class * 100]
# done!
cat('-------- end\n\n')



# CHANGE LABELS
cat('CHANGE LABELS\n')
cat('-------- start\n')
change_labels <- function(input_dataset, input_label_list) {
    # set new class values in posterior_dt
    working_dt <- copy(posterior_dt)
    working_dt <- working_dt[dataset == input_dataset]
    working_dt$class <- ifelse(
        working_dt$class == 100, input_label_list[1],
        ifelse(working_dt$class == 200, input_label_list[2],
        ifelse(working_dt$class == 300, input_label_list[3],
        ifelse(working_dt$class == 400, input_label_list[4],
        ifelse(working_dt$class == 500, input_label_list[5],
        ifelse(working_dt$class == 600, input_label_list[6],
        ifelse(working_dt$class == 700, input_label_list[7],
        ifelse(working_dt$class == 800, input_label_list[8],
        ifelse(working_dt$class == 900, input_label_list[9], working_dt$class))))))))
    )
    posterior_dt[dataset == input_dataset, class := working_dt$class]
    # set new class values in probs_se_dt
    working_se_dt <- copy(probs_se_dt)
    working_se_dt <- working_se_dt[dataset == input_dataset]
    working_se_dt$class <- ifelse(
        working_se_dt$class == 100, input_label_list[1],
        ifelse(working_se_dt$class == 200, input_label_list[2],
        ifelse(working_se_dt$class == 300, input_label_list[3],
        ifelse(working_se_dt$class == 400, input_label_list[4],
        ifelse(working_se_dt$class == 500, input_label_list[5],
        ifelse(working_se_dt$class == 600, input_label_list[6],
        ifelse(working_se_dt$class == 700, input_label_list[7],
        ifelse(working_se_dt$class == 800, input_label_list[8],
        ifelse(working_se_dt$class == 900, input_label_list[9], working_se_dt$class))))))))
    )
    probs_se_dt[dataset == input_dataset, class := working_se_dt$class]
    # set new class values in class_label_list
    class_label_list[[input_dataset]] <<-
        ifelse(class_label_list[[input_dataset]] == 100, input_label_list[1],
        ifelse(class_label_list[[input_dataset]] == 200, input_label_list[2],
        ifelse(class_label_list[[input_dataset]] == 300, input_label_list[3],
        ifelse(class_label_list[[input_dataset]] == 400, input_label_list[4],
        ifelse(class_label_list[[input_dataset]] == 500, input_label_list[5],
        ifelse(class_label_list[[input_dataset]] == 600, input_label_list[6],
        ifelse(class_label_list[[input_dataset]] == 700, input_label_list[7],
        ifelse(class_label_list[[input_dataset]] == 800, input_label_list[8],
        ifelse(class_label_list[[input_dataset]] == 900, input_label_list[9], class_label_list[[input_dataset]])))))))))
}
# switch labels
change_labels(1, c(1,2,3,4,5,6,7,8,9))
change_labels(2, c(7,2,3,4,5,6,1,8,9))
change_labels(3, c(1,4,6,3,2,9,7,8,5))
change_labels(4, c(5,1,8,9,3,2,6,7,4))
change_labels(5, c(6,9,2,3,7,1,4,8,5))
change_labels(6, c(9,5,1,6,8,3,4,7,2))
change_labels(7, c(6,4,8,5,3,1,7,9,2))
change_labels(8, c(6,5,7,9,8,4,3,1,2))
change_labels(9, c(6,9,2,3,7,1,4,8,5))
change_labels(10, c(7,2,9,4,1,6,3,8,5))
change_labels(11, c(6,9,2,3,7,1,4,8,5))
change_labels(12, c(5,6,9,8,3,7,1,4,2))
change_labels(13, c(1,8,9,5,4,2,7,3,6))
change_labels(14, c(6,9,2,3,7,1,4,8,5))
change_labels(15, c(1,4,6,9,3,2,7,5,8))
change_labels(16, c(6,9,2,3,7,1,4,8,5))
change_labels(17, c(9,5,1,6,8,3,4,7,2))
change_labels(18, c(3,4,9,1,2,5,6,8,7))
change_labels(19, c(8,2,9,7,1,5,4,3,6))
change_labels(20, c(1,2,3,4,5,6,7,8,9))
change_labels(21, c(1,2,3,4,5,6,7,8,9))
change_labels(22, c(8,9,1,3,4,2,6,5,7))
change_labels(23, c(3,1,8,4,2,5,6,9,7))
change_labels(24, c(6,8,5,3,2,1,4,9,7))
change_labels(25, c(8,9,1,3,4,2,6,5,7))
change_labels(26, c(1,3,5,7,4,9,6,8,2))
change_labels(27, c(7,2,5,4,1,9,3,8,6))
change_labels(28, c(9,5,1,6,8,3,4,7,2))
change_labels(29, c(6,9,2,3,7,1,4,8,5))
change_labels(30, c(8,4,7,9,5,2,6,1,3))
change_labels(31, c(9,2,3,4,6,8,5,7,1))
change_labels(32, c(3,9,5,1,6,8,4,2,7))
change_labels(33, c(6,5,4,9,8,2,3,1,7))
change_labels(34, c(1,7,6,3,2,9,4,8,5))
change_labels(35, c(6,1,3,8,2,9,4,7,5))
change_labels(36, c(6,9,1,8,5,7,4,3,2))
change_labels(37, c(3,6,1,9,2,4,7,5,8))
change_labels(38, c(6,9,2,3,7,1,4,8,5))
change_labels(39, c(3,8,9,5,4,2,7,1,6))
change_labels(40, c(6,9,2,3,7,1,4,8,5))
change_labels(41, c(1,4,6,3,5,2,7,9,8))
change_labels(42, c(5,7,8,9,6,2,1,3,4))
change_labels(43, c(2,6,7,1,8,4,3,5,9))
change_labels(44, c(6,9,2,3,7,1,4,8,5))
change_labels(45, c(4,6,7,1,5,2,9,8,3))
change_labels(46, c(6,9,1,8,5,7,4,3,2))
change_labels(47, c(4,5,8,7,3,6,2,1,9))
change_labels(48, c(1,7,6,3,2,9,4,8,5))
change_labels(49, c(1,4,6,3,2,9,7,8,5))
change_labels(50, c(7,2,5,4,1,9,3,8,6))
change_labels(51, c(9,3,1,4,2,8,5,6,7))
change_labels(52, c(1,5,8,3,4,9,7,6,2))
change_labels(53, c(9,6,3,2,5,7,1,8,4))
change_labels(54, c(5,1,8,9,3,2,6,7,4))
change_labels(55, c(6,5,1,7,2,4,8,3,9))
change_labels(56, c(6,2,3,8,9,4,5,7,1))
change_labels(57, c(6,5,4,9,8,2,3,1,7))
change_labels(58, c(9,6,3,2,5,7,1,8,4))
change_labels(59, c(6,5,4,9,8,2,3,1,7))
change_labels(60, c(3,4,6,7,2,9,8,5,1))
change_labels(61, c(1,7,6,3,2,9,4,8,5))
change_labels(62, c(1,2,3,4,5,6,7,8,9))
change_labels(63, c(6,9,2,3,7,1,4,8,5))
change_labels(64, c(9,5,1,6,8,3,4,7,2))
change_labels(65, c(1,7,6,3,2,9,4,8,5))
change_labels(66, c(6,9,1,8,5,7,4,3,2))
change_labels(67, c(8,4,1,5,2,7,3,6,9))
change_labels(68, c(8,3,2,9,6,5,4,1,7))
change_labels(69, c(6,9,1,8,5,7,4,3,2))
change_labels(70, c(1,7,6,3,2,9,4,8,5))
change_labels(71, c(3,9,5,1,6,8,4,2,7))
change_labels(72, c(3,4,6,7,2,9,8,5,1))
change_labels(73, c(7,2,5,4,1,9,3,8,6))
change_labels(74, c(1,4,6,3,2,9,7,8,5))
change_labels(75, c(3,8,7,1,2,9,4,5,6))
change_labels(76, c(6,9,2,3,7,1,4,8,5))
change_labels(77, c(9,6,3,2,5,7,1,8,4))
change_labels(78, c(6,5,4,9,8,2,3,1,7))
change_labels(79, c(6,9,1,8,5,7,4,3,2))
change_labels(80, c(5,3,8,9,6,2,1,7,4))
change_labels(81, c(9,7,5,2,6,8,4,1,3))
change_labels(82, c(6,9,1,8,5,7,4,3,2))
change_labels(83, c(6,9,2,3,7,1,4,8,5))
change_labels(84, c(3,1,8,6,7,5,9,4,2))
change_labels(85, c(9,5,1,6,8,3,4,7,2))
change_labels(86, c(9,7,5,2,6,8,4,1,3))
change_labels(87, c(4,6,7,2,1,5,3,9,8))
change_labels(88, c(1,4,6,3,2,9,7,8,5))
change_labels(89, c(2,6,7,1,8,4,3,5,9))
change_labels(90, c(6,9,1,8,5,7,4,3,2))
change_labels(91, c(1,4,6,3,2,9,7,8,5))
change_labels(92, c(6,9,2,3,7,1,4,8,5))
change_labels(93, c(8,1,3,9,6,4,7,5,2))
change_labels(94, c(3,4,6,7,2,9,8,5,1))
change_labels(95, c(7,2,9,5,3,6,1,4,8))
change_labels(96, c(1,2,3,4,5,6,7,8,9))
change_labels(97, c(1,7,6,3,2,9,4,8,5))
change_labels(98, c(6,9,2,3,7,1,4,8,5))
change_labels(99, c(6,9,2,3,7,1,4,8,5))
change_labels(100, c(6,5,4,9,8,2,3,1,7))
# done!
cat('-------- end\n\n')



# DETERMINE CLASS LABEL FOR INDIVIDUALS
cat('DETERMINE CLASS LABEL FOR INDIVIDUALS\n')
cat('-------- start\n')
# for each cell (corresponding to each dataset's majority vote for each subject) in class_label_list, populate the corresponding column in label_dt
for (i in 1:n_datasets) {
    label_dt[, paste0('dataset', i) := class_label_list[[i]]]
}
# set majority_vote to the most commonly occuring class label (across the imputed datasets) for each subject
label_dt[, majority_vote := apply(label_dt, 1, function(x) {
    names(table(x))[which.max(table(x))]
})]
# set majority_count to the number of times the majority_vote class label occurs for each subject
label_dt[, majority_count := apply(label_dt, 1, function(x) {
    table(x)[which.max(table(x))] - 1
})]
# save label_dt
saveRDS(label_dt, paste0(output_location, "rds/", "label-dt-", data_source, "-ng-", ng, "-n_iterations-", n_iterations, "-n_starts-", n_starts, "-only_positive_cases-", only_positive_cases, stratified, ".rds"))
# summarize variables on classification confidence
majority_count_table <- table(label_dt$majority_count)
print(majority_count_table)
# get the number of subjects with majority_count_table <50
n_majority_count_lt_50 <- nrow(label_dt[majority_count < 50, ])
print(n_majority_count_lt_50)
# get the number of subjects with majority_count_table >90
n_majority_count_gt_90 <- nrow(label_dt[majority_count > 90, ])
print(n_majority_count_gt_90)
majority_count_values <- as.numeric(names(majority_count_table))
majority_count_frequencies <- as.vector(majority_count_table)
# make a histogram of the above variables
p <- ggplot(data = data.frame(majority_count_values, majority_count_frequencies), aes(x = majority_count_values, y = majority_count_frequencies)) +
    geom_bar(stat = "identity", fill = "#333333") +
    # geom_text(aes(label=paste0(round(majority_count_frequencies/n_total_cohort*100,1),"%")), vjust=-0.9, size=6.5) +
    labs(
        x = "Datasets in which the most common class label was assigned",
        y = "Number of subjects"
    ) +
    scale_x_continuous(breaks = seq(from = min(majority_count_values), to = max(majority_count_values), by = 3), limits = c(min(majority_count_values)-1, max(majority_count_values)+1), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(from = 0, to = max(majority_count_frequencies), by = 500), limits = c(0, max(majority_count_frequencies)+1), expand = c(0, 0)) +
    theme(
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(color='black'),
        axis.title.x = element_text(margin = margin(t = 12), size = 24),
        axis.title.y = element_text(margin = margin(r = 12), size = 24),
        axis.text.x = element_text(size = 23, color = "black", margin = margin(t = 6)),
        axis.text.y = element_text(size = 23, color = "black", margin = margin(r = 6)),
        axis.ticks = element_line(), 
        axis.ticks.length = unit(0.2, "cm"),
        # plot.title = element_text(size = 26.5, hjust = 0.5, margin = margin(b = 12)),
        plot.margin = margin(t = 50, l = 50)
    )
ggsave(
    filename = paste0(
        output_location,
        "svg/",
        "majority-vote-confidence-",
        ng,
        stratified,
        ".svg"
    ),
    plot = p,
    device = "svg",
    dpi = 300,
    height = 11.5,
    width = 21
)
# done!
cat('-------- end\n\n')



# PLOT TRAJECTORIES
get_confidence_interval <- function(average, means, SE) {
    variances <- SE^2
    within_variance <- mean(variances)
    between_variance <- ( sum((means-average)^2) / (n_datasets-1))
    total_variance <- within_variance + between_variance + (between_variance / n_datasets)
    rm <- (between_variance+(between_variance/n_datasets))/within_variance
    degrees_of_freedom <- (n_datasets-1)*((1+1/(rm))^2)
    quantile <- qt(0.975, df = degrees_of_freedom, lower.tail = TRUE)
    lower <- average - quantile * sqrt(total_variance)
    upper <- average + quantile * sqrt(total_variance)
    return(list(lower, upper))
}
cat('PLOT TRAJECTORIES\n')
cat('-------- start\n')
p_list <- list()
# for each class, plot the column-average of the posterior probabilities
for (k in 1:ng) {
    # get class size (n) and percentage
    class_n <- nrow(label_dt[majority_vote == k, ])
    class_perc <- round(class_n / n_total_cohort * 100, 1)
    # posterior probabilities
    # get the column-average of the posterior probabilities
    class_probs <- posterior_dt[class == k, ]
    # pool averages
    survey_asthma_1 <- mean(class_probs$survey_asthma_1)
    survey_asthma_2 <- mean(class_probs$survey_asthma_2)
    survey_asthma_3 <- mean(class_probs$survey_asthma_3)
    survey_asthma_4 <- mean(class_probs$survey_asthma_4)
    survey_asthma_values <- c(survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4)
    survey_eczema_1 <- mean(class_probs$survey_eczema_1)
    survey_eczema_2 <- mean(class_probs$survey_eczema_2)
    survey_eczema_3 <- mean(class_probs$survey_eczema_3)
    survey_eczema_4 <- mean(class_probs$survey_eczema_4)
    survey_eczema_values <- c(survey_eczema_1, survey_eczema_2, survey_eczema_3, survey_eczema_4)
    survey_rhinitis_2 <- mean(class_probs$survey_rhinitis_2)
    survey_rhinitis_3 <- mean(class_probs$survey_rhinitis_3)
    survey_rhinitis_4 <- mean(class_probs$survey_rhinitis_4)
    survey_rhinitis_values <- c(NA, survey_rhinitis_2, survey_rhinitis_3, survey_rhinitis_4)
    medication_asthma_1 <- mean(class_probs$asthma_medication1)
    medication_asthma_2 <- mean(class_probs$asthma_medication2)
    medication_asthma_3 <- mean(class_probs$asthma_medication3)
    medication_asthma_4 <- mean(class_probs$asthma_medication4)
    medication_asthma_5 <- mean(class_probs$asthma_medication5)
    medication_asthma_6 <- mean(class_probs$asthma_medication6)
    medication_asthma_values <- c(medication_asthma_1, medication_asthma_2, medication_asthma_3, medication_asthma_4, medication_asthma_5, medication_asthma_6)
    medication_eczema_1 <- mean(class_probs$eczema_medication1)
    medication_eczema_2 <- mean(class_probs$eczema_medication2)
    medication_eczema_3 <- mean(class_probs$eczema_medication3)
    medication_eczema_4 <- mean(class_probs$eczema_medication4)
    medication_eczema_5 <- mean(class_probs$eczema_medication5)
    medication_eczema_6 <- mean(class_probs$eczema_medication6)
    medication_eczema_values <- c(medication_eczema_1, medication_eczema_2, medication_eczema_3, medication_eczema_4, medication_eczema_5, medication_eczema_6)
    medication_rhinitis_1 <- mean(class_probs$rhinitis_medication1)
    medication_rhinitis_2 <- mean(class_probs$rhinitis_medication2)
    medication_rhinitis_3 <- mean(class_probs$rhinitis_medication3)
    medication_rhinitis_4 <- mean(class_probs$rhinitis_medication4)
    medication_rhinitis_5 <- mean(class_probs$rhinitis_medication5)
    medication_rhinitis_6 <- mean(class_probs$rhinitis_medication6)
    medication_rhinitis_values <- c(medication_rhinitis_1, medication_rhinitis_2, medication_rhinitis_3, medication_rhinitis_4, medication_rhinitis_5, medication_rhinitis_6)

    # posterior probabilty SEs
    # get the column-average of the posterior probability SEs
    class_probs_se <- probs_se_dt[class == k, ]
    # survey asthma SEs
    survey_asthma_1_se_ci <- get_confidence_interval(survey_asthma_1, class_probs$survey_asthma_1, class_probs_se$survey_asthma_1)
    survey_asthma_1_se_li <- survey_asthma_1_se_ci[[1]]
    survey_asthma_1_se_ui <- survey_asthma_1_se_ci[[2]]
    survey_asthma_2_se_ci <- get_confidence_interval(survey_asthma_2, class_probs$survey_asthma_2, class_probs_se$survey_asthma_2)
    survey_asthma_2_se_li <- survey_asthma_2_se_ci[[1]]
    survey_asthma_2_se_ui <- survey_asthma_2_se_ci[[2]]
    survey_asthma_3_se_ci <- get_confidence_interval(survey_asthma_3, class_probs$survey_asthma_3, class_probs_se$survey_asthma_3)
    survey_asthma_3_se_li <- survey_asthma_3_se_ci[[1]]
    survey_asthma_3_se_ui <- survey_asthma_3_se_ci[[2]]
    survey_asthma_4_se_ci <- get_confidence_interval(survey_asthma_4, class_probs$survey_asthma_4, class_probs_se$survey_asthma_4)
    survey_asthma_4_se_li <- survey_asthma_4_se_ci[[1]]
    survey_asthma_4_se_ui <- survey_asthma_4_se_ci[[2]]
    survey_asthma_se_li <- c(survey_asthma_1_se_li, survey_asthma_2_se_li, survey_asthma_3_se_li, survey_asthma_4_se_li)
    survey_asthma_se_ui <- c(survey_asthma_1_se_ui, survey_asthma_2_se_ui, survey_asthma_3_se_ui, survey_asthma_4_se_ui)
    # survey eczema SEs
    survey_eczema_1_se_ci <- get_confidence_interval(survey_eczema_1, class_probs$survey_eczema_1, class_probs_se$survey_eczema_1)
    survey_eczema_1_se_li <- survey_eczema_1_se_ci[[1]]
    survey_eczema_1_se_ui <- survey_eczema_1_se_ci[[2]]
    survey_eczema_2_se_ci <- get_confidence_interval(survey_eczema_2, class_probs$survey_eczema_2, class_probs_se$survey_eczema_2)
    survey_eczema_2_se_li <- survey_eczema_2_se_ci[[1]]
    survey_eczema_2_se_ui <- survey_eczema_2_se_ci[[2]]
    survey_eczema_3_se_ci <- get_confidence_interval(survey_eczema_3, class_probs$survey_eczema_3, class_probs_se$survey_eczema_3)
    survey_eczema_3_se_li <- survey_eczema_3_se_ci[[1]]
    survey_eczema_3_se_ui <- survey_eczema_3_se_ci[[2]]
    survey_eczema_4_se_ci <- get_confidence_interval(survey_eczema_4, class_probs$survey_eczema_4, class_probs_se$survey_eczema_4)
    survey_eczema_4_se_li <- survey_eczema_4_se_ci[[1]]
    survey_eczema_4_se_ui <- survey_eczema_4_se_ci[[2]]
    survey_eczema_se_li <- c(survey_eczema_1_se_li, survey_eczema_2_se_li, survey_eczema_3_se_li, survey_eczema_4_se_li)
    survey_eczema_se_ui <- c(survey_eczema_1_se_ui, survey_eczema_2_se_ui, survey_eczema_3_se_ui, survey_eczema_4_se_ui)
    # survey rhinitis SEs
    survey_rhinitis_2_se_ci <- get_confidence_interval(survey_rhinitis_2, class_probs$survey_rhinitis_2, class_probs_se$survey_rhinitis_2)
    survey_rhinitis_2_se_li <- survey_rhinitis_2_se_ci[[1]]
    survey_rhinitis_2_se_ui <- survey_rhinitis_2_se_ci[[2]]
    survey_rhinitis_3_se_ci <- get_confidence_interval(survey_rhinitis_3, class_probs$survey_rhinitis_3, class_probs_se$survey_rhinitis_3)
    survey_rhinitis_3_se_li <- survey_rhinitis_3_se_ci[[1]]
    survey_rhinitis_3_se_ui <- survey_rhinitis_3_se_ci[[2]]
    survey_rhinitis_4_se_ci <- get_confidence_interval(survey_rhinitis_4, class_probs$survey_rhinitis_4, class_probs_se$survey_rhinitis_4)
    survey_rhinitis_4_se_li <- survey_rhinitis_4_se_ci[[1]]
    survey_rhinitis_4_se_ui <- survey_rhinitis_4_se_ci[[2]]
    survey_rhinitis_se_li <- c(NA, survey_rhinitis_2_se_li, survey_rhinitis_3_se_li, survey_rhinitis_4_se_li)
    survey_rhinitis_se_ui <- c(NA, survey_rhinitis_2_se_ui, survey_rhinitis_3_se_ui, survey_rhinitis_4_se_ui)
    # medication asthma SEs
    medication_asthma_1_se_ci <- get_confidence_interval(medication_asthma_1, class_probs$asthma_medication1, class_probs_se$asthma_medication1)
    medication_asthma_1_se_li <- medication_asthma_1_se_ci[[1]]
    medication_asthma_1_se_ui <- medication_asthma_1_se_ci[[2]]
    medication_asthma_2_se_ci <- get_confidence_interval(medication_asthma_2, class_probs$asthma_medication2, class_probs_se$asthma_medication2)
    medication_asthma_2_se_li <- medication_asthma_2_se_ci[[1]]
    medication_asthma_2_se_ui <- medication_asthma_2_se_ci[[2]]
    medication_asthma_3_se_ci <- get_confidence_interval(medication_asthma_3, class_probs$asthma_medication3, class_probs_se$asthma_medication3)
    medication_asthma_3_se_li <- medication_asthma_3_se_ci[[1]]
    medication_asthma_3_se_ui <- medication_asthma_3_se_ci[[2]]
    medication_asthma_4_se_ci <- get_confidence_interval(medication_asthma_4, class_probs$asthma_medication4, class_probs_se$asthma_medication4)
    medication_asthma_4_se_li <- medication_asthma_4_se_ci[[1]]
    medication_asthma_4_se_ui <- medication_asthma_4_se_ci[[2]]
    medication_asthma_5_se_ci <- get_confidence_interval(medication_asthma_5, class_probs$asthma_medication5, class_probs_se$asthma_medication5)
    medication_asthma_5_se_li <- medication_asthma_5_se_ci[[1]]
    medication_asthma_5_se_ui <- medication_asthma_5_se_ci[[2]]
    medication_asthma_6_se_ci <- get_confidence_interval(medication_asthma_6, class_probs$asthma_medication6, class_probs_se$asthma_medication6)
    medication_asthma_6_se_li <- medication_asthma_6_se_ci[[1]]
    medication_asthma_6_se_ui <- medication_asthma_6_se_ci[[2]]
    medication_asthma_se_li <- c(medication_asthma_1_se_li, medication_asthma_2_se_li, medication_asthma_3_se_li, medication_asthma_4_se_li, medication_asthma_5_se_li, medication_asthma_6_se_li)
    medication_asthma_se_ui <- c(medication_asthma_1_se_ui, medication_asthma_2_se_ui, medication_asthma_3_se_ui, medication_asthma_4_se_ui, medication_asthma_5_se_ui, medication_asthma_6_se_ui)
    # medication eczema SEs
    medication_eczema_1_se_ci <- get_confidence_interval(medication_eczema_1, class_probs$eczema_medication1, class_probs_se$eczema_medication1)
    medication_eczema_1_se_li <- medication_eczema_1_se_ci[[1]]
    medication_eczema_1_se_ui <- medication_eczema_1_se_ci[[2]]
    medication_eczema_2_se_ci <- get_confidence_interval(medication_eczema_2, class_probs$eczema_medication2, class_probs_se$eczema_medication2)
    medication_eczema_2_se_li <- medication_eczema_2_se_ci[[1]]
    medication_eczema_2_se_ui <- medication_eczema_2_se_ci[[2]]
    medication_eczema_3_se_ci <- get_confidence_interval(medication_eczema_3, class_probs$eczema_medication3, class_probs_se$eczema_medication3)
    medication_eczema_3_se_li <- medication_eczema_3_se_ci[[1]]
    medication_eczema_3_se_ui <- medication_eczema_3_se_ci[[2]]
    medication_eczema_4_se_ci <- get_confidence_interval(medication_eczema_4, class_probs$eczema_medication4, class_probs_se$eczema_medication4)
    medication_eczema_4_se_li <- medication_eczema_4_se_ci[[1]]
    medication_eczema_4_se_ui <- medication_eczema_4_se_ci[[2]]
    medication_eczema_5_se_ci <- get_confidence_interval(medication_eczema_5, class_probs$eczema_medication5, class_probs_se$eczema_medication5)
    medication_eczema_5_se_li <- medication_eczema_5_se_ci[[1]]
    medication_eczema_5_se_ui <- medication_eczema_5_se_ci[[2]]
    medication_eczema_6_se_ci <- get_confidence_interval(medication_eczema_6, class_probs$eczema_medication6, class_probs_se$eczema_medication6)
    medication_eczema_6_se_li <- medication_eczema_6_se_ci[[1]]
    medication_eczema_6_se_ui <- medication_eczema_6_se_ci[[2]]
    medication_eczema_se_li <- c(medication_eczema_1_se_li, medication_eczema_2_se_li, medication_eczema_3_se_li, medication_eczema_4_se_li, medication_eczema_5_se_li, medication_eczema_6_se_li)
    medication_eczema_se_ui <- c(medication_eczema_1_se_ui, medication_eczema_2_se_ui, medication_eczema_3_se_ui, medication_eczema_4_se_ui, medication_eczema_5_se_ui, medication_eczema_6_se_ui)
    # medication rhinitis SEs
    medication_rhinitis_1_se_ci <- get_confidence_interval(medication_rhinitis_1, class_probs$rhinitis_medication1, class_probs_se$rhinitis_medication1)
    medication_rhinitis_1_se_li <- medication_rhinitis_1_se_ci[[1]]
    medication_rhinitis_1_se_ui <- medication_rhinitis_1_se_ci[[2]]
    medication_rhinitis_2_se_ci <- get_confidence_interval(medication_rhinitis_2, class_probs$rhinitis_medication2, class_probs_se$rhinitis_medication2)
    medication_rhinitis_2_se_li <- medication_rhinitis_2_se_ci[[1]]
    medication_rhinitis_2_se_ui <- medication_rhinitis_2_se_ci[[2]]
    medication_rhinitis_3_se_ci <- get_confidence_interval(medication_rhinitis_3, class_probs$rhinitis_medication3, class_probs_se$rhinitis_medication3)
    medication_rhinitis_3_se_li <- medication_rhinitis_3_se_ci[[1]]
    medication_rhinitis_3_se_ui <- medication_rhinitis_3_se_ci[[2]]
    medication_rhinitis_4_se_ci <- get_confidence_interval(medication_rhinitis_4, class_probs$rhinitis_medication4, class_probs_se$rhinitis_medication4)
    medication_rhinitis_4_se_li <- medication_rhinitis_4_se_ci[[1]]
    medication_rhinitis_4_se_ui <- medication_rhinitis_4_se_ci[[2]]
    medication_rhinitis_5_se_ci <- get_confidence_interval(medication_rhinitis_5, class_probs$rhinitis_medication5, class_probs_se$rhinitis_medication5)
    medication_rhinitis_5_se_li <- medication_rhinitis_5_se_ci[[1]]
    medication_rhinitis_5_se_ui <- medication_rhinitis_5_se_ci[[2]]
    medication_rhinitis_6_se_ci <- get_confidence_interval(medication_rhinitis_6, class_probs$rhinitis_medication6, class_probs_se$rhinitis_medication6)
    medication_rhinitis_6_se_li <- medication_rhinitis_6_se_ci[[1]]
    medication_rhinitis_6_se_ui <- medication_rhinitis_6_se_ci[[2]]
    medication_rhinitis_se_li <- c(medication_rhinitis_1_se_li, medication_rhinitis_2_se_li, medication_rhinitis_3_se_li, medication_rhinitis_4_se_li, medication_rhinitis_5_se_li, medication_rhinitis_6_se_li)
    medication_rhinitis_se_ui <- c(medication_rhinitis_1_se_ui, medication_rhinitis_2_se_ui, medication_rhinitis_3_se_ui, medication_rhinitis_4_se_ui, medication_rhinitis_5_se_ui, medication_rhinitis_6_se_ui)

    # create data frames for plotting
    survey_data <- data.frame(x = c(1, 4.5, 8, 12),
        asthma = survey_asthma_values,
        eczema = survey_eczema_values,
        rhinitis = survey_rhinitis_values)
    medication_data <- data.frame(x = c(3, 5, 7, 9, 11, 13),
        asthma = medication_asthma_values,
        eczema = medication_eczema_values,
        rhinitis = medication_rhinitis_values)
    survey_data_se <- data.frame(x = c(1, 4.5, 8, 12),
        asthma_li = survey_asthma_se_li,
        asthma_ui = survey_asthma_se_ui,
        eczema_li = survey_eczema_se_li,
        eczema_ui = survey_eczema_se_ui,
        rhinitis_li = survey_rhinitis_se_li,
        rhinitis_ui = survey_rhinitis_se_ui)
    medication_data_se <- data.frame(x = c(3, 5, 7, 9, 11, 13),
        asthma_li = medication_asthma_se_li,
        asthma_ui = medication_asthma_se_ui,
        eczema_li = medication_eczema_se_li,
        eczema_ui = medication_eczema_se_ui,
        rhinitis_li = medication_rhinitis_se_li,
        rhinitis_ui = medication_rhinitis_se_ui)
                        
    # plot
    plot_line_width <- 1.5
    ci_opacity <- 0.06
    p <- ggplot() +
        # survey CIs
        geom_ribbon(data = survey_data_se, aes(x = x, ymin = pmax(0, asthma_li - 0.1), ymax = pmin(1,asthma_ui + 0.1)), fill = alpha("#5e8ac4", ci_opacity)) +
        geom_ribbon(data = survey_data_se, aes(x = x, ymin = pmax(0, eczema_li - 0.1), ymax = pmin(1,eczema_ui + 0.1)), fill = alpha("#C25D5D", ci_opacity)) +
        geom_ribbon(data = survey_data_se, aes(x = x, ymin = pmax(0, rhinitis_li - 0.1), ymax = pmin(1,rhinitis_ui + 0.1)), fill = alpha("#5DC273", ci_opacity)) +
        # medication CIs
        geom_ribbon(data = medication_data_se, aes(x = x, ymin = pmax(0, asthma_li - 0.1), ymax = pmin(1,asthma_ui + 0.1)), fill = alpha("#5e8ac4", ci_opacity)) +
        geom_ribbon(data = medication_data_se, aes(x = x, ymin = pmax(0, eczema_li - 0.1), ymax = pmin(1,eczema_ui + 0.1)), fill = alpha("#C25D5D", ci_opacity)) +
        geom_ribbon(data = medication_data_se, aes(x = x, ymin = pmax(0, rhinitis_li - 0.1), ymax = pmin(1,rhinitis_ui + 0.1)), fill = alpha("#5DC273", ci_opacity)) +
        # survey
        geom_line(data = survey_data, aes(x = x, y = asthma, color = "Asthma, parental-reported"), lty = 2, linewidth = plot_line_width) +
        geom_line(data = survey_data, aes(x = x, y = eczema, color = "Eczema, parental-reported"), lty = 2, linewidth = plot_line_width) +
        geom_line(data = survey_data, aes(x = x, y = rhinitis, color = "Allergic rhinitis, parental-reported"), lty = 2, linewidth = plot_line_width) +
        # medication
        geom_line(data = medication_data, aes(x = x, y = asthma, color = "Asthma, dispensed medication"), lty = 1, linewidth = plot_line_width) +
        geom_line(data = medication_data, aes(x = x, y = eczema, color = "Eczema, dispensed medication"), lty = 1, linewidth = plot_line_width) +
        geom_line(data = medication_data, aes(x = x, y = rhinitis, color = "Allergic rhinitis, dispensed medication"), lty = 1, linewidth = plot_line_width) +
        labs(x = "Age (years)", y = "Probability of outcome") +
        ggtitle(paste0("Class ", k, " (n = ", format(class_n, big.mark = ","), "; ", class_perc, "%)")) +
        scale_y_continuous(breaks = seq(from = 0, to = 0.95, by = 0.20), limits = c(0, 1), expand = c(0, 0)) +
        scale_x_continuous(breaks = seq(from = 1, to = max(medication_data$x), by = 2), limits = c(1, max(medication_data$x)), expand = c(0, 0)) +
        scale_color_manual(values = c("Asthma, parental-reported" = "#5e8ac4", "Asthma, dispensed medication" = "#5e8ac4", "Eczema, parental-reported" = "#C25D5D", "Eczema, dispensed medication" = "#C25D5D", "Allergic rhinitis, parental-reported" = "#5DC273", "Allergic rhinitis, dispensed medication" = "#5DC273")) +
        theme(
            panel.background = element_rect(fill = "#ffffff"),
            axis.line = element_line(color='black'),
            axis.title.x = element_text(margin = margin(t = 12), size = 22),
            axis.title.y = element_text(margin = margin(r = 12), size = 22),
            axis.text.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
            axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
            axis.ticks = element_line(), 
            axis.ticks.length = unit(0.2, "cm"),
            # legend.position = "top",
            legend.position = "none",
            legend.box = "vertical",
            legend.key=element_blank(),
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 21),
            plot.title = element_text(size = 26.5, hjust = 0.5, margin = margin(b = 12)),
            plot.margin = margin(t = 50, l = 50)
        )
    p_list[[k]] <- p
}
# save all plots in one file
grid <- gridExtra::grid.arrange(grobs = p_list[c(2,4,7,1,6,5,3,9,8)], ncol = 3)
ggsave(
    filename = paste0(
        output_location,
        "svg/",
        "pooled-trajectories-",
        ng,
        stratified,
        ".svg"
    ),
    plot = grid,
    device = "svg",
    dpi = 300,
    height = 16.5,
    width = 21
)
print('Plotted all trajectories')
# asthma plots separately
asthma_grid <- gridExtra::grid.arrange(grobs = p_list[c(2,4,7)], ncol = 3)
ggsave(
    filename = paste0(
        output_location,
        "svg/",
        "pooled-trajectories-asthma-",
        ng,
        stratified,
        ".svg"
    ),
    plot = asthma_grid,
    device = "svg",
    dpi = 300,
    height = 5.5,
    width = 33
)
print('Plotted asthma trajectories')
# eczema plots separately
eczema_grid <- gridExtra::grid.arrange(grobs = p_list[c(1,6)], ncol = 2)
ggsave(
    filename = paste0(
        output_location,
        "svg/",
        "pooled-trajectories-eczema-",
        ng,
        stratified,
        ".svg"
    ),
    plot = eczema_grid,
    device = "svg",
    dpi = 300,
    height = 5.5,
    width = 14
)
print('Plotted eczema trajectories')
# rhinitis plots separately
rhinitis_grid <- gridExtra::grid.arrange(grobs = p_list[c(5)], ncol = 1)
ggsave(
    filename = paste0(
        output_location,
        "svg/",
        "pooled-trajectories-rhinitis-",
        ng,
        stratified,
        ".svg"
    ),
    plot = rhinitis_grid,
    device = "svg",
    dpi = 300,
    height = 5.5,
    width = 7
)
print('Plotted rhinitis trajectories')
# mixed plots separately
mixed_and_low_grid <- gridExtra::grid.arrange(grobs = p_list[c(3,9,8)], ncol = 3)
ggsave(
    filename = paste0(
        output_location,
        "svg/",
        "pooled-trajectories-mixed-and-low-",
        ng,
        stratified,
        ".svg"
    ),
    plot = mixed_and_low_grid,
    device = "svg",
    dpi = 300,
    height = 5.5,
    width = 14
)
print('Plotted mixed and low trajectories')
# done!
cat('-------- end\n\n')



# MAKE SUMMARY TABLE BY CLASS
cat('MAKE SUMMARY TABLE BY CLASS\n')
cat('-------- start\n')

# load data
# load pooled survey and medication data
raw_combined_data <- readRDS(paste0(output_location, "rds/", "pooled-imputed-survey-and-medication-data.rds"))
# add majority_vote to each subject in raw_combined_data
raw_combined_data$majority_vote <- label_dt$majority_vote
print('Loaded raw_combined_data')
# load ATC codes (medication data)
source("ATC_CODES.r") # load ATC codes
npdr_filename <- paste0(input_location, "SOS_grundfil_LKM20032016_181107.sav")
npdr_data <- haven::read_sav(npdr_filename)
data.table::setDT(npdr_data)
print('Loaded npdr_data')

# preprocess background characteristic variables
# set any_family_member_asthma_allergy
raw_combined_data$any_family_member_asthma_allergy <- ifelse(
    raw_combined_data$mother_asthma == 1 | raw_combined_data$father_asthma == 1 | raw_combined_data$siblings_asthma == 1 | raw_combined_data$mother_allergic_rhinitis == 1 | raw_combined_data$father_allergic_rhinitis == 1 | raw_combined_data$siblings_allergic_rhinitis == 1 | raw_combined_data$mother_eczema == 1 | raw_combined_data$father_eczema == 1 | raw_combined_data$siblings_eczema == 1,
    1,
    0
)
# make birth weight a three-level ordinal
raw_combined_data$birth_weight <- ifelse(
    raw_combined_data$birth_weight < 2500,
    "Low",
    ifelse(
        raw_combined_data$birth_weight >= 2500 & raw_combined_data$birth_weight < 4000,
        "Normal",
        ifelse(
            raw_combined_data$birth_weight >= 4000,
            "High",
            NA
        )
    )
)
raw_combined_data$birth_weight <- factor(raw_combined_data$birth_weight, levels = c("Low", "Normal", "High"))
# make gestational age a three-level ordinal
raw_combined_data$gestational_age <- ifelse(
    raw_combined_data$gestational_age < 37,
    "Preterm",
    ifelse(
        raw_combined_data$gestational_age >= 37,
        "Term",
        NA
    )
)

# preprocess medication data
# eczema
# basic ointment
for (code in atc_codes_basic_ointments) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), basic_ointment := 1]
}
basic_ointment_dt <- npdr_data[basic_ointment == 1, .(lopnr = unique(lopnr), basic_ointment = 1)]
raw_combined_data <- merge(raw_combined_data, basic_ointment_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$basic_ointment <- ifelse(is.na(raw_combined_data$basic_ointment), 0, raw_combined_data$basic_ointment)
# basic ointment count
basic_ointment_count_dt <- npdr_data[basic_ointment == 1, .(basic_ointment_count = sum(basic_ointment)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, basic_ointment_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$basic_ointment_count <- ifelse(is.na(raw_combined_data$basic_ointment_count), 0, raw_combined_data$basic_ointment_count)
# group i corticosteroids
for (code in atc_codes_group_i_corticosteroids) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), group_i_corticosteroids := 1]
}
group_i_corticosteroids_dt <- npdr_data[group_i_corticosteroids == 1, .(lopnr = unique(lopnr), group_i_corticosteroids = 1)]
raw_combined_data <- merge(raw_combined_data, group_i_corticosteroids_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$group_i_corticosteroids <- ifelse(is.na(raw_combined_data$group_i_corticosteroids), 0, raw_combined_data$group_i_corticosteroids)
# group i corticosteroids count
group_i_corticosteroids_count_dt <- npdr_data[group_i_corticosteroids == 1, .(group_i_corticosteroids_count = sum(group_i_corticosteroids)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, group_i_corticosteroids_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$group_i_corticosteroids_count <- ifelse(is.na(raw_combined_data$group_i_corticosteroids_count), 0, raw_combined_data$group_i_corticosteroids_count)
# moderate strong corticosteroids
for (code in atc_codes_moderate_strong_corticosteroids) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), moderate_strong_corticosteroids := 1]
}
moderate_strong_corticosteroids_dt <- npdr_data[moderate_strong_corticosteroids == 1, .(lopnr = unique(lopnr), moderate_strong_corticosteroids = 1)]
raw_combined_data <- merge(raw_combined_data, moderate_strong_corticosteroids_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$moderate_strong_corticosteroids <- ifelse(is.na(raw_combined_data$moderate_strong_corticosteroids), 0, raw_combined_data$moderate_strong_corticosteroids)
# moderate strong corticosteroids count
moderate_strong_corticosteroids_count_dt <- npdr_data[moderate_strong_corticosteroids == 1, .(moderate_strong_corticosteroids_count = sum(moderate_strong_corticosteroids)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, moderate_strong_corticosteroids_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$moderate_strong_corticosteroids_count <- ifelse(is.na(raw_combined_data$moderate_strong_corticosteroids_count), 0, raw_combined_data$moderate_strong_corticosteroids_count)
# dermatologic immunosuppresants
for (code in atc_codes_dermatologic_immunosuppresants) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), dermatologic_immunosuppresants := 1]
}
dermatologic_immunosuppresants_dt <- npdr_data[dermatologic_immunosuppresants == 1, .(lopnr = unique(lopnr), dermatologic_immunosuppresants = 1)]
raw_combined_data <- merge(raw_combined_data, dermatologic_immunosuppresants_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$dermatologic_immunosuppresants <- ifelse(is.na(raw_combined_data$dermatologic_immunosuppresants), 0, raw_combined_data$dermatologic_immunosuppresants)
# dermatologic immunosuppresants count
dermatologic_immunosuppresants_count_dt <- npdr_data[dermatologic_immunosuppresants == 1, .(dermatologic_immunosuppresants_count = sum(dermatologic_immunosuppresants)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, dermatologic_immunosuppresants_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$dermatologic_immunosuppresants_count <- ifelse(is.na(raw_combined_data$dermatologic_immunosuppresants_count), 0, raw_combined_data$dermatologic_immunosuppresants_count)
# asthma
# saba
for (code in atc_codes_saba) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), saba := 1]
}
saba_dt <- npdr_data[saba == 1, .(lopnr = unique(lopnr), saba = 1)]
raw_combined_data <- merge(raw_combined_data, saba_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$saba <- ifelse(is.na(raw_combined_data$saba), 0, raw_combined_data$saba)
# saba count
saba_count_dt <- npdr_data[saba == 1, .(saba_count = sum(saba)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, saba_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$saba_count <- ifelse(is.na(raw_combined_data$saba_count), 0, raw_combined_data$saba_count)
# ics
for (code in atc_codes_ics) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), ics := 1]
}
ics_dt <- npdr_data[ics == 1, .(lopnr = unique(lopnr), ics = 1)]
raw_combined_data <- merge(raw_combined_data, ics_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$ics <- ifelse(is.na(raw_combined_data$ics), 0, raw_combined_data$ics)
# ics count
ics_count_dt <- npdr_data[ics == 1, .(ics_count = sum(ics)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, ics_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$ics_count <- ifelse(is.na(raw_combined_data$ics_count), 0, raw_combined_data$ics_count)
# laba, ltra, lama
for (code in atc_codes_laba) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), laba := 1]
}
for (code in atc_codes_ics_laba) {
    npdr_data[grepl(paste0("^", code), npdr_data$atc), laba := 1]
}
for (code in atc_codes_ltra) {
    npdr_data[grepl(paste0("^", code), npdr_data$atc), laba := 1]
}
for (code in atc_codes_lama) {
    npdr_data[grepl(paste0("^", code), npdr_data$atc), laba := 1]
}
laba_dt <- npdr_data[laba == 1, .(lopnr = unique(lopnr), laba = 1)]
raw_combined_data <- merge(raw_combined_data, laba_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$laba <- ifelse(is.na(raw_combined_data$laba), 0, raw_combined_data$laba)
# laba count
laba_count_dt <- npdr_data[laba == 1, .(laba_count = sum(laba)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, laba_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$laba_count <- ifelse(is.na(raw_combined_data$laba_count), 0, raw_combined_data$laba_count)
# get subjects with both ics and laba in the same age
selected_subjects_ics_laba <- npdr_data %>%
  group_by(lopnr, ALDER) %>%
  summarise(has_ics = any(ics == 1),
            has_laba = any(laba == 1)) %>%
  filter(has_ics & has_laba) %>%
  pull(lopnr) %>%
  unique()
# set ics_and_laba to 1 for subjects in selected_subjects
raw_combined_data$ics_and_laba <- ifelse(
    raw_combined_data$lopnr %in% selected_subjects_ics_laba,
    1,
    0
)
# # ics AND laba
# raw_combined_data$ics_and_laba <- ifelse(
#     raw_combined_data$ics == 1 & raw_combined_data$laba == 1,
#     1,
#     0
# )
# rhinitis
# nasal steroids
for (code in atc_codes_nasal_steroids) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), nasal_steroids := 1]
}
nasal_steroids_dt <- npdr_data[nasal_steroids == 1, .(lopnr = unique(lopnr), nasal_steroids = 1)]
raw_combined_data <- merge(raw_combined_data, nasal_steroids_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$nasal_steroids <- ifelse(is.na(raw_combined_data$nasal_steroids), 0, raw_combined_data$nasal_steroids)
# nasal steroids count
nasal_steroids_count_dt <- npdr_data[nasal_steroids == 1, .(nasal_steroids_count = sum(nasal_steroids)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, nasal_steroids_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$nasal_steroids_count <- ifelse(is.na(raw_combined_data$nasal_steroids_count), 0, raw_combined_data$nasal_steroids_count)
# antihistamines and mast cell stabilizers
for (code in atc_codes_antihistamines_and_mast_cell_stabilizers) {
  npdr_data[grepl(paste0("^", code), npdr_data$atc), antihistamines_mast_cell_stabilizers := 1]
}
antihistamines_mast_cell_stabilizers_dt <- npdr_data[antihistamines_mast_cell_stabilizers == 1, .(lopnr = unique(lopnr), antihistamines_mast_cell_stabilizers = 1)]
raw_combined_data <- merge(raw_combined_data, antihistamines_mast_cell_stabilizers_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$antihistamines_mast_cell_stabilizers <- ifelse(is.na(raw_combined_data$antihistamines_mast_cell_stabilizers), 0, raw_combined_data$antihistamines_mast_cell_stabilizers)
# antihistamines and mast cell stabilizers count
antihistamines_mast_cell_stabilizers_count_dt <- npdr_data[antihistamines_mast_cell_stabilizers == 1, .(antihistamines_mast_cell_stabilizers_count = sum(antihistamines_mast_cell_stabilizers)), by = "lopnr"]
raw_combined_data <- merge(raw_combined_data, antihistamines_mast_cell_stabilizers_count_dt, by = "lopnr", all.x = TRUE)
raw_combined_data$antihistamines_mast_cell_stabilizers_count <- ifelse(is.na(raw_combined_data$antihistamines_mast_cell_stabilizers_count), 0, raw_combined_data$antihistamines_mast_cell_stabilizers_count)
# get subjects with both nasal steroids and antihistamines and mast cell stabilizers in the same age
selected_subjects_nasal_steroids_antihistamines <- npdr_data %>%
  group_by(lopnr, ALDER) %>%
  summarise(has_antihistamines = any(antihistamines_mast_cell_stabilizers == 1),
            has_nasal_steroids = any(nasal_steroids == 1)) %>%
  filter(has_antihistamines & has_nasal_steroids) %>%
  pull(lopnr) %>%
  unique()
# set simultaneous_antihistamines_and_nasal_steroids to 1 for subjects in selected_subjects
raw_combined_data$nasal_steroids_and_antihistamines <- ifelse(
    raw_combined_data$lopnr %in% selected_subjects_nasal_steroids_antihistamines,
    1,
    0
)
# # nasal steroids AND antihistamines and mast cell stabilizers
# raw_combined_data$nasal_steroids_and_antihistamines <- ifelse(
#     raw_combined_data$nasal_steroids == 1 & raw_combined_data$antihistamines_mast_cell_stabilizers == 1,
#     1,
#     0
# )

# select columns for the summary table
raw_combined_data <- raw_combined_data[, c("majority_vote", "sex", "any_family_member_asthma_allergy", "mother_education", "mother_smoking_pregnancy", "mother_age", "c_section", "gestational_age", "birth_weight", "birth_order", "daycare_6m", "basic_ointment", "basic_ointment_count", "group_i_corticosteroids", "group_i_corticosteroids_count", "moderate_strong_corticosteroids", "moderate_strong_corticosteroids_count", "dermatologic_immunosuppresants", "dermatologic_immunosuppresants_count", "saba", "saba_count", "ics", "ics_count", "laba", "laba_count", "ics_and_laba", "nasal_steroids", "nasal_steroids_count", "antihistamines_mast_cell_stabilizers", "antihistamines_mast_cell_stabilizers_count", "nasal_steroids_and_antihistamines")]

# recode the _count variables to levels 0, 1-2, and 3+, and convert to factor
raw_combined_data$basic_ointment_count <- ifelse(
    raw_combined_data$basic_ointment_count == 0,
    "0",
    ifelse(
        raw_combined_data$basic_ointment_count >= 1 & raw_combined_data$basic_ointment_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$basic_ointment_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$basic_ointment_count <- factor(raw_combined_data$basic_ointment_count, levels = c("0", "1-2", "3+"))
raw_combined_data$group_i_corticosteroids_count <- ifelse(
    raw_combined_data$group_i_corticosteroids_count == 0,
    "0",
    ifelse(
        raw_combined_data$group_i_corticosteroids_count >= 1 & raw_combined_data$group_i_corticosteroids_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$group_i_corticosteroids_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$group_i_corticosteroids_count <- factor(raw_combined_data$group_i_corticosteroids_count, levels = c("0", "1-2", "3+"))
raw_combined_data$moderate_strong_corticosteroids_count <- ifelse(
    raw_combined_data$moderate_strong_corticosteroids_count == 0,
    "0",
    ifelse(
        raw_combined_data$moderate_strong_corticosteroids_count >= 1 & raw_combined_data$moderate_strong_corticosteroids_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$moderate_strong_corticosteroids_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$moderate_strong_corticosteroids_count <- factor(raw_combined_data$moderate_strong_corticosteroids_count, levels = c("0", "1-2", "3+"))
raw_combined_data$dermatologic_immunosuppresants_count <- ifelse(
    raw_combined_data$dermatologic_immunosuppresants_count == 0,
    "0",
    ifelse(
        raw_combined_data$dermatologic_immunosuppresants_count >= 1 & raw_combined_data$dermatologic_immunosuppresants_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$dermatologic_immunosuppresants_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$dermatologic_immunosuppresants_count <- factor(raw_combined_data$dermatologic_immunosuppresants_count, levels = c("0", "1-2", "3+"))
raw_combined_data$saba_count <- ifelse(
    raw_combined_data$saba_count == 0,
    "0",
    ifelse(
        raw_combined_data$saba_count >= 1 & raw_combined_data$saba_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$saba_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$saba_count <- factor(raw_combined_data$saba_count, levels = c("0", "1-2", "3+"))
raw_combined_data$ics_count <- ifelse(
    raw_combined_data$ics_count == 0,
    "0",
    ifelse(
        raw_combined_data$ics_count >= 1 & raw_combined_data$ics_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$ics_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$ics_count <- factor(raw_combined_data$ics_count, levels = c("0", "1-2", "3+"))
raw_combined_data$laba_count <- ifelse(
    raw_combined_data$laba_count == 0,
    "0",
    ifelse(
        raw_combined_data$laba_count >= 1 & raw_combined_data$laba_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$laba_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$laba_count <- factor(raw_combined_data$laba_count, levels = c("0", "1-2", "3+"))
raw_combined_data$nasal_steroids_count <- ifelse(
    raw_combined_data$nasal_steroids_count == 0,
    "0",
    ifelse(
        raw_combined_data$nasal_steroids_count >= 1 & raw_combined_data$nasal_steroids_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$nasal_steroids_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$nasal_steroids_count <- factor(raw_combined_data$nasal_steroids_count, levels = c("0", "1-2", "3+"))
raw_combined_data$antihistamines_mast_cell_stabilizers_count <- ifelse(
    raw_combined_data$antihistamines_mast_cell_stabilizers_count == 0,
    "0",
    ifelse(
        raw_combined_data$antihistamines_mast_cell_stabilizers_count >= 1 & raw_combined_data$antihistamines_mast_cell_stabilizers_count <= 2,
        "1-2",
        ifelse(
            raw_combined_data$antihistamines_mast_cell_stabilizers_count >= 3,
            "3+",
            NA
        )
    )
)
raw_combined_data$antihistamines_mast_cell_stabilizers_count <- factor(raw_combined_data$antihistamines_mast_cell_stabilizers_count, levels = c("0", "1-2", "3+"))
print('Preprocessed raw_combined_data')



# print the lowest and highest value for mother_age
print(paste0("Lowest mother_age: ", min(raw_combined_data$mother_age)))
print(paste0("Highest mother_age: ", max(raw_combined_data$mother_age)))



# set order of majority_vote column (c(2,4,7,1,6,5,3,9,8))
raw_combined_data$majority_vote <- factor(raw_combined_data$majority_vote, levels = c(2,4,7,1,6,5,3,9,8))
# summarize data above into table
table <- raw_combined_data %>%
    tbl_summary(
        by = majority_vote
        # , statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}")
    ) %>%
    # add_overall() %>%
    # add_p() %>%
    # add_p(test.args = all_categorical("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
    add_p(test.args = all_categorical("fisher.test") ~ list(simulate.p.value = TRUE)) %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(output_location, 'docx/', 'trajectories-characteristics-n-imputations-', i, '.docx'))
print('Saved table')
print('-------- end\n\n')