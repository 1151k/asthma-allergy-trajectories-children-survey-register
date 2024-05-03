# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "gridExtra")
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
ng <- 1:12 # set to 9:9 when checking average posterior probabilities in the final model
n_iterations <- 5000
n_starts <- 100
only_positive_cases <- FALSE # whether to only include those with at least one positive outcome
stratified <- '' # if not empty, stratify by a) sex or b) heredity for asthma/allergy
checking_average_posterior_probabilities <- FALSE # set to TRUE when checking average posterior probabilities in the final model



# variables needed for the plotting
# for BIC plot
bic_values <- data.table(
    dataset = integer(),
    ng = integer(),
    bic = numeric()
)
low_posterior_probabilities <- data.table(
    dataset = integer(),
    ng = integer(),
    low_values = numeric()
)
class_posterior_probabilities <- data.table(
    dataset = integer(),
    class = integer(),
    posterior_probability = numeric()
)
class_orders <- list(c(1,2,3,4,5,6,7,8,9), c(7,2,3,4,5,6,1,8,9), c(1,4,6,3,2,9,7,8,5), c(5,1,8,9,3,2,6,7,4), c(6,9,2,3,7,1,4,8,5), c(9,5,1,6,8,3,4,7,2), c(6,4,8,5,3,1,7,9,2), c(6,5,7,9,8,4,3,1,2), c(6,9,2,3,7,1,4,8,5), c(7,2,9,4,1,6,3,8,5), c(6,9,2,3,7,1,4,8,5), c(5,6,9,8,3,7,1,4,2), c(1,8,9,5,4,2,7,3,6), c(6,9,2,3,7,1,4,8,5), c(1,4,6,9,3,2,7,5,8), c(6,9,2,3,7,1,4,8,5), c(9,5,1,6,8,3,4,7,2), c(3,4,9,1,2,5,6,8,7), c(8,2,9,7,1,5,4,3,6), c(1,2,3,4,5,6,7,8,9), c(1,2,3,4,5,6,7,8,9), c(8,9,1,3,4,2,6,5,7), c(3,1,8,4,2,5,6,9,7), c(6,8,5,3,2,1,4,9,7), c(8,9,1,3,4,2,6,5,7), c(1,3,5,7,4,9,6,8,2), c(7,2,5,4,1,9,3,8,6), c(9,5,1,6,8,3,4,7,2), c(6,9,2,3,7,1,4,8,5), c(8,4,7,9,5,2,6,1,3), c(9,2,3,4,6,8,5,7,1), c(3,9,5,1,6,8,4,2,7), c(6,5,4,9,8,2,3,1,7), c(1,7,6,3,2,9,4,8,5), c(6,1,3,8,2,9,4,7,5), c(6,9,1,8,5,7,4,3,2), c(3,6,1,9,2,4,7,5,8), c(6,9,2,3,7,1,4,8,5), c(3,8,9,5,4,2,7,1,6), c(6,9,2,3,7,1,4,8,5), c(1,4,6,3,5,2,7,9,8), c(5,7,8,9,6,2,1,3,4), c(2,6,7,1,8,4,3,5,9), c(6,9,2,3,7,1,4,8,5), c(4,6,7,1,5,2,9,8,3), c(6,9,1,8,5,7,4,3,2), c(4,5,8,7,3,6,2,1,9), c(1,7,6,3,2,9,4,8,5), c(1,4,6,3,2,9,7,8,5), c(7,2,5,4,1,9,3,8,6), c(9,3,1,4,2,8,5,6,7), c(1,5,8,3,4,9,7,6,2), c(9,6,3,2,5,7,1,8,4), c(5,1,8,9,3,2,6,7,4), c(6,5,1,7,2,4,8,3,9), c(6,2,3,8,9,4,5,7,1), c(6,5,4,9,8,2,3,1,7), c(9,6,3,2,5,7,1,8,4), c(6,5,4,9,8,2,3,1,7), c(3,4,6,7,2,9,8,5,1), c(1,7,6,3,2,9,4,8,5), c(1,2,3,4,5,6,7,8,9), c(6,9,2,3,7,1,4,8,5), c(9,5,1,6,8,3,4,7,2), c(1,7,6,3,2,9,4,8,5), c(6,9,1,8,5,7,4,3,2), c(8,4,1,5,2,7,3,6,9), c(8,3,2,9,6,5,4,1,7), c(6,9,1,8,5,7,4,3,2), c(1,7,6,3,2,9,4,8,5), c(3,9,5,1,6,8,4,2,7), c(3,4,6,7,2,9,8,5,1), c(7,2,5,4,1,9,3,8,6), c(1,4,6,3,2,9,7,8,5), c(3,8,7,1,2,9,4,5,6), c(6,9,2,3,7,1,4,8,5), c(9,6,3,2,5,7,1,8,4), c(6,5,4,9,8,2,3,1,7), c(6,9,1,8,5,7,4,3,2), c(5,3,8,9,6,2,1,7,4), c(9,7,5,2,6,8,4,1,3), c(6,9,1,8,5,7,4,3,2), c(6,9,2,3,7,1,4,8,5), c(3,1,8,6,7,5,9,4,2), c(9,5,1,6,8,3,4,7,2), c(9,7,5,2,6,8,4,1,3), c(4,6,7,2,1,5,3,9,8), c(1,4,6,3,2,9,7,8,5), c(2,6,7,1,8,4,3,5,9), c(6,9,1,8,5,7,4,3,2), c(1,4,6,3,2,9,7,8,5), c(6,9,2,3,7,1,4,8,5), c(8,1,3,9,6,4,7,5,2), c(3,4,6,7,2,9,8,5,1), c(7,2,9,5,3,6,1,4,8), c(1,2,3,4,5,6,7,8,9), c(1,7,6,3,2,9,4,8,5), c(6,9,2,3,7,1,4,8,5), c(6,9,2,3,7,1,4,8,5), c(6,5,4,9,8,2,3,1,7))



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



# loop each imputed dataset
for (i in imputed_i) {
    cat(paste('Imputed dataset:', i, '\n'))
    # for checking the lowest BIC value in each dataset
    dataset_bics <- data.table(
        ng = integer(),
        bic = numeric()
    )
    # loop each solution (number of classes)
    for (k in ng) {
        print(paste('Number of classes:', k))
        # AVERAGE POSTERIOR PROBABILITIES
        # read mean posterior probabilities
        mean_posterior_probabilities <- readRDS(paste0(output_location,"rds/","mean-class-probabilities-",data_source,"-dataset-",i,"-ng-",k,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".rds"))
        # copy mean_posterior_probabilities to rds/for-ofs
        saveRDS(mean_posterior_probabilities, paste0(output_location,"rds/for-ofs/","mean-class-probabilities-",data_source,"-dataset-",i,"-ng-",k,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".rds"))
        # unlist and round to 2 decimal places 
        mean_posterior_probabilities <- unlist(mean_posterior_probabilities)
        mean_posterior_probabilities <- round(mean_posterior_probabilities, 2)
        print('mean posterior probabilities')
        print(mean_posterior_probabilities)
        print('---')
        # check if any values are below or equal to 0.8 in the mean posterior probabilities
        if (any(mean_posterior_probabilities < 0.8)) {
            print('LOW VALUES')
        }
        low_posterior_probabilities <- rbind(low_posterior_probabilities, data.table(dataset = i, ng = k, low_values = any(mean_posterior_probabilities < 0.8)))
        # check average posterior probability for each class in the final model
        if (checking_average_posterior_probabilities) {
            reordered_class_posterior_probabilities <- numeric(9)
            reordered_class_posterior_probabilities[class_orders[[i]]] <- mean_posterior_probabilities
            # now, populate class_posterior_probabilities data.table
            class_posterior_probabilities <- rbind(class_posterior_probabilities, data.table(dataset = i, class = 1:9, posterior_probability = reordered_class_posterior_probabilities))
        }
        # BIC
        # read BIC
        bic <- readRDS(paste0(output_location,"rds/","bic-",data_source,"-dataset-",i,"-ng-",k,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".rds"))
        # copy bic to rds/for-ofs
        saveRDS(bic, paste0(output_location,"rds/for-ofs/","bic-",data_source,"-dataset-",i,"-ng-",k,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".rds"))
        print(bic)
        bic_values <- rbind(bic_values, data.table(dataset = i, ng = k, bic = bic))
        dataset_bics <- rbind(dataset_bics, data.table(ng = k, bic = bic))
    }
    # get the lowest BIC value in the dataset and which ng it is
    lowest_bic <- dataset_bics[which.min(bic)]
    print(paste('Lowest BIC value:', lowest_bic$bic, 'at ng =', lowest_bic$ng))
    print('--------')
}



if (!checking_average_posterior_probabilities) { # only run this when NOT checking average posterior probabilities in the final model
    # plot BIC values
    p <- ggplot(bic_values, aes(x = ng, y = bic, group = dataset)) +
        geom_line(color = '#333333') +
        # geom_point(aes(color = '#333333')) +
        labs(
            x = "Number of classes (trajectories) in model",
            y = "Bayesian information criterion (BIC)"
        ) +
        scale_x_continuous(breaks = seq(from = 1, to = tail(ng, n = 1), by = 1), limits = c(0.9, tail(ng, n = 1)+0.1), expand = c(0, 0)) +
        # scale_y_continuous(breaks = seq(from = 0, to = max(bic_values$bic), limits = c(0.9, max(big_values$bic)+0.1), expand = c(0, 0)) +
        theme(
            panel.background = element_rect(fill = "#ffffff"),
            axis.line = element_line(color='black'),
            axis.title.x = element_text(margin = margin(t = 12), size = 24),
            axis.title.y = element_text(margin = margin(r = 12), size = 24),
            axis.text.x = element_text(size = 23, color = "black", margin = margin(t = 6)),
            axis.text.y = element_text(size = 23, color = "black", margin = margin(r = 6)),
            axis.ticks.length = unit(0.2, "cm"),
            plot.margin = margin(t = 50, l = 50)
        )
    ggsave(paste0(output_location,"svg/","bic-",data_source,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".svg"), p, width = 16, height = 11, dpi = 300)

    # histogram of low values (x = ng, y = number of low values)
    # get the number of low values for each ng
    low_posterior_probabilities <- low_posterior_probabilities[, .(low_values = sum(low_values)), by = .(ng)]
    print(low_posterior_probabilities)
    # make histogram
    p <- ggplot(low_posterior_probabilities, aes(x = ng, y = low_values)) +
        geom_bar(stat = "identity", fill = '#333333') +
        labs(
            x = "Number of classes (trajectories) in model",
            y = "Number of models with ≥1 class with average posterior probability <0.8"
        ) +
        scale_x_continuous(breaks = seq(from = 1, to = tail(ng, n = 1), by = 1), limits = c(0.4, tail(ng, n = 1)+1), expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 10, to = 90, by = 20), limits = c(0, 98), expand = c(0, 0)) +
        theme(
            panel.background = element_rect(fill = "#ffffff"),
            axis.line = element_line(color='black'),
            axis.title.x = element_text(margin = margin(t = 12), size = 24),
            axis.title.y = element_text(margin = margin(r = 12), size = 22),
            axis.text.x = element_text(size = 22, color = "black", margin = margin(t = 6)),
            axis.text.y = element_text(size = 22, color = "black", margin = margin(r = 6)),
            axis.ticks.length = unit(0.2, "cm"),
            plot.margin = margin(t = 50, l = 50)
        )
    ggsave(paste0(output_location,"svg/","low-posterior-probabilities-",data_source,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".svg"), p, width = 16, height = 11, dpi = 300)
} else { # run this when checking average posterior probabilities in the final model
    # save class_posterior_probabilities to rds
    saveRDS(class_posterior_probabilities, paste0(output_location,"rds/","class-posterior-probabilities-",data_source,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".rds"))
    # plot class_posterior_probabilities
    p <- ggplot(class_posterior_probabilities, aes(x = class, y = posterior_probability, group = dataset)) +
        geom_line(color = '#333333') +
        geom_point(color = '#333333') +
        labs(
            x = "Class",
            y = "Average posterior probability"
        ) +
        scale_x_continuous(breaks = seq(from = 1, to = 9, by = 1), limits = c(0.9, 9.1), expand = c(0, 0)) +
        scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2), limits = c(0, 1.1), expand = c(0, 0)) +
        theme(
            panel.background = element_rect(fill = "#ffffff"),
            axis.line = element_line(color='black'),
            axis.title.x = element_text(margin = margin(t = 12), size = 24),
            axis.title.y = element_text(margin = margin(r = 12), size = 24),
            axis.text.x = element_text(size = 23, color = "black", margin = margin(t = 6)),
            axis.text.y = element_text(size = 23, color = "black", margin = margin(r = 6)),
            axis.ticks.length = unit(0.2, "cm"),
            plot.margin = margin(t = 50, l = 50)
        )
    ggsave(paste0(output_location,"svg/","class-posterior-probabilities-",data_source,"-n_iterations-",n_iterations,"-n_starts-",n_starts,"-only_positive_cases-",only_positive_cases,stratified,".svg"), p, width = 16, height = 11, dpi = 300)    
}