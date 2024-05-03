# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "poLCA", "gridExtra")
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
ng <- 9
n_iterations <- 5000
n_starts <- 100
only_positive_cases <- FALSE # whether to only include those with at least one positive outcome
stratified <- ''



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
print(paste('Imputed datasets to be assessed:', paste(imputed_i, collapse = ', ')))
print(paste('Data source:', data_source))
print(paste('Time points:', time_points))
print(paste('Number of classes to be tested:', paste(ng, collapse = ', ')))
print(paste('Number of iterations:', n_iterations))
print(paste('Number of starts:', n_starts))
print(paste('Only positive cases:', only_positive_cases))
print(paste('Stratified by:', stratified))
cat('-------- end\n\n')




# loop all values of imputed_i
for (i in imputed_i) {
    cat(paste('IMPUTED DATASET', i, '\n'))
    cat('-------- start\n')
    probs <- readRDS(paste0(output_location, "rds/", "probs-", data_source, "-dataset-", i, "-ng-", ng, "-n_iterations-", n_iterations, "-n_starts-", n_starts, "-only_positive_cases-", only_positive_cases, stratified, ".rds"))
    labels <- readRDS(paste0(output_location, "rds/", "class-labels-", data_source, "-dataset-", i, "-ng-", ng, "-n_iterations-", n_iterations, "-n_starts-", n_starts, "-only_positive_cases-", only_positive_cases, stratified, ".rds"))
    labels_n <- length(labels)
    # print(probs)
    # print(labels)
    print('dimensions of labels')
    print(dim(labels))
    
    p_list <- list()

    for (class in 1:ng) {
        if (data_source == 'survey') {
            # get asthma values
            asthma_1 <- probs$survey_asthma_1[class,2]
            asthma_2 <- probs$survey_asthma_2[class,2]
            asthma_3 <- probs$survey_asthma_3[class,2]
            asthma_4 <- probs$survey_asthma_4[class,2]
            asthma_values <- c(asthma_1, asthma_2, asthma_3, asthma_4)
            # get eczema values
            eczema_1 <- probs$survey_eczema_1[class,2]
            eczema_2 <- probs$survey_eczema_2[class,2]
            eczema_3 <- probs$survey_eczema_3[class,2]
            eczema_4 <- probs$survey_eczema_4[class,2]
            eczema_values <- c(eczema_1, eczema_2, eczema_3, eczema_4)
            # get rhinitis values
            rhinitis_2 <- probs$survey_rhinitis_2[class,2]
            rhinitis_3 <- probs$survey_rhinitis_3[class,2]
            rhinitis_4 <- probs$survey_rhinitis_4[class,2]
            rhinitis_values <- c(NA, rhinitis_2, rhinitis_3, rhinitis_4)
            # get class n and percentage
            class_n <- sum(labels == class)
            class_perc <- round(class_n / labels_n * 100, 1)
            plot_data <- data.frame(
                x = rep(c(1, 2, 3, 4), 3),
                y = c(asthma_values, eczema_values, rhinitis_values),
                condition = rep(c("Asthma", "Eczema", "Rhinitis"), each = 4)
            )
            p <- ggplot(plot_data, aes(x = x, y = round(y*100), color = condition)) +
                geom_line(lineWidth = 3.3) +
                geom_point() +
                scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("1", "4.5", "8", "12")) +
                scale_y_continuous(limits = c(0, 100)) +
                ggtitle(paste0("Class ", class, " (n = ", class_n, "; ", class_perc, "%)")) +
                scale_color_manual(values = c("#5D88C2", "#C25D5D", "#5DC273")) +
                theme(legend.position = "top right") +
                theme(
                    panel.background = element_rect(fill = "#ffffff"),
                    axis.line = element_line(color='black'),
                    axis.title.x = element_text(margin = margin(t = 12), size = 15),
                    axis.title.y = element_text(margin = margin(r = 12), size = 15),
                    axis.text.x = element_text(size = 13, color = "black", margin = margin(t = 6)),
                    axis.text.y = element_text(size = 13, color = "black", margin = margin(r = 6)),
                    axis.ticks = element_line(), 
                    axis.ticks.length = unit(0.2, "cm"),
                    legend.position = "top",
                    # legend.title = element_blank(),
                    legend.key=element_blank(),
                    legend.title = element_text(size = 13),
                    legend.text = element_text(size = 12)
                )
                xlab("Age (years)") +
                ylab("Probability of outcome")
        } else if (data_source == 'medication') {
            # get asthma values
            asthma_1 <- probs$asthma_medication1[class,2]
            asthma_2 <- probs$asthma_medication2[class,2]
            asthma_3 <- probs$asthma_medication3[class,2]
            asthma_4 <- probs$asthma_medication4[class,2]
            asthma_5 <- probs$asthma_medication5[class,2]
            asthma_6 <- probs$asthma_medication6[class,2]
            asthma_values <- c(asthma_1, asthma_2, asthma_3, asthma_4, asthma_5, asthma_6)
            # get eczema values
            eczema_1 <- probs$eczema_medication1[class,2]
            eczema_2 <- probs$eczema_medication2[class,2]
            eczema_3 <- probs$eczema_medication3[class,2]
            eczema_4 <- probs$eczema_medication4[class,2]
            eczema_5 <- probs$eczema_medication5[class,2]
            eczema_6 <- probs$eczema_medication6[class,2]
            eczema_values <- c(eczema_1, eczema_2, eczema_3, eczema_4, eczema_5, eczema_6)
            # get rhinitis values
            rhinitis_1 <- probs$rhinitis_medication1[class,2]
            rhinitis_2 <- probs$rhinitis_medication2[class,2]
            rhinitis_3 <- probs$rhinitis_medication3[class,2]
            rhinitis_4 <- probs$rhinitis_medication4[class,2]
            rhinitis_5 <- probs$rhinitis_medication5[class,2]
            rhinitis_6 <- probs$rhinitis_medication6[class,2]
            rhinitis_values <- c(rhinitis_1, rhinitis_2, rhinitis_3, rhinitis_4, rhinitis_5, rhinitis_6)
            # get class n and percentage
            class_n <- sum(labels == class)
            class_perc <- round(class_n / labels_n * 100, 1)

            # Ensure all vectors have the same length
            print(length(asthma_values))
            print(length(eczema_values))
            print(length(rhinitis_values))

            plot_data <- data.frame(
                x = rep(c(1, 2, 3, 4, 5, 6), 3),
                y = c(asthma_values, eczema_values, rhinitis_values),
                condition = rep(c("Asthma", "Eczema", "Rhinitis"), each = 6)
            )
            p <- ggplot(plot_data, aes(x = x, y = y, color = condition)) +
                geom_line(lineWidth = 3.3) +
                geom_point() +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), labels = c("2-3", "4-5", "6-7", "8-9", "10-11", "12-13")) +
                scale_y_continuous(limits = c(0, 1)) +
                ggtitle(paste0("Class ", class, " (n = ", class_n, "; ", class_perc, "%)")) +
                scale_color_manual(values = c("#5D88C2", "#C25D5D", "#5DC273")) +
                theme(legend.position = "top right") +
                theme_minimal() +
                xlab("Age (years)") +
                ylab("Probability of outcome")
        } else if (data_source == 'combination') {
            # select asthma values as per above, adding survey prefix
            survey_asthma_1 <- probs$survey_asthma_1[class,2]
            survey_asthma_2 <- probs$survey_asthma_2[class,2]
            survey_asthma_3 <- probs$survey_asthma_3[class,2]
            survey_asthma_4 <- probs$survey_asthma_4[class,2]
            survey_asthma_values <- c(survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4)
            # select eczema values as per above, adding survey prefix
            survey_eczema_1 <- probs$survey_eczema_1[class,2]
            survey_eczema_2 <- probs$survey_eczema_2[class,2]
            survey_eczema_3 <- probs$survey_eczema_3[class,2]
            survey_eczema_4 <- probs$survey_eczema_4[class,2]
            survey_eczema_values <- c(survey_eczema_1, survey_eczema_2, survey_eczema_3, survey_eczema_4)
            # select rhinitis values as per above, adding survey prefix
            survey_rhinitis_2 <- probs$survey_rhinitis_2[class,2]
            survey_rhinitis_3 <- probs$survey_rhinitis_3[class,2]
            survey_rhinitis_4 <- probs$survey_rhinitis_4[class,2]
            survey_rhinitis_values <- c(NA, survey_rhinitis_2, survey_rhinitis_3, survey_rhinitis_4)
            # select asthma values as per above, adding medication prefix
            medication_asthma_1 <- probs$asthma_medication1[class,2]
            medication_asthma_2 <- probs$asthma_medication2[class,2]
            medication_asthma_3 <- probs$asthma_medication3[class,2]
            medication_asthma_4 <- probs$asthma_medication4[class,2]
            medication_asthma_5 <- probs$asthma_medication5[class,2]
            medication_asthma_6 <- probs$asthma_medication6[class,2]
            medication_asthma_values <- c(medication_asthma_1, medication_asthma_2, medication_asthma_3, medication_asthma_4, medication_asthma_5, medication_asthma_6)
            # select eczema values as per above, adding medication prefix
            medication_eczema_1 <- probs$eczema_medication1[class,2]
            medication_eczema_2 <- probs$eczema_medication2[class,2]
            medication_eczema_3 <- probs$eczema_medication3[class,2]
            medication_eczema_4 <- probs$eczema_medication4[class,2]
            medication_eczema_5 <- probs$eczema_medication5[class,2]
            medication_eczema_6 <- probs$eczema_medication6[class,2]
            medication_eczema_values <- c(medication_eczema_1, medication_eczema_2, medication_eczema_3, medication_eczema_4, medication_eczema_5, medication_eczema_6)
            # select rhinitis values as per above, adding medication prefix
            medication_rhinitis_1 <- probs$rhinitis_medication1[class,2]
            medication_rhinitis_2 <- probs$rhinitis_medication2[class,2]
            medication_rhinitis_3 <- probs$rhinitis_medication3[class,2]
            medication_rhinitis_4 <- probs$rhinitis_medication4[class,2]
            medication_rhinitis_5 <- probs$rhinitis_medication5[class,2]
            medication_rhinitis_6 <- probs$rhinitis_medication6[class,2]
            medication_rhinitis_values <- c(medication_rhinitis_1, medication_rhinitis_2, medication_rhinitis_3, medication_rhinitis_4, medication_rhinitis_5, medication_rhinitis_6)
            # get class n and percentage
            class_n <- sum(labels == class)
            class_perc <- round(class_n / labels_n * 100, 1)
            # plot all of the above, with similar colors for survey and medication



##########

        # Create data frames
        survey_data <- data.frame(x = c(1, 4.5, 8, 12),
                                asthma = survey_asthma_values,
                                eczema = survey_eczema_values,
                                rhinitis = survey_rhinitis_values)

        medication_data <- data.frame(x = c(3, 5, 7, 9, 11, 13),
                                    asthma = medication_asthma_values,
                                    eczema = medication_eczema_values,
                                    rhinitis = medication_rhinitis_values)

        # Plotting
        p <- ggplot() +
        geom_line(data = survey_data, aes(x = x, y = asthma*100, color = "Asthma survey"), lty = 2, linewidth = 1) +
        geom_line(data = survey_data, aes(x = x, y = eczema*100, color = "Eczema survey"), lty = 2, linewidth = 1) +
        geom_line(data = survey_data, aes(x = x, y = rhinitis*100, color = "Rhinitis survey"), lty = 2, linewidth = 1) +
        geom_line(data = medication_data, aes(x = x, y = asthma*100, color = "Asthma medication"), lty = 1, linewidth = 1) +
        geom_line(data = medication_data, aes(x = x, y = eczema*100, color = "Eczema medication"), lty = 1, linewidth = 1) +
        geom_line(data = medication_data, aes(x = x, y = rhinitis*100, color = "Rhinitis medication"), lty = 1, linewidth = 1) +
        labs(
            x = "Age (years)",
            y = "Probability of outcome") +
        ggtitle(paste0("Class ", class, " (n = ", class_n, "; ", class_perc, "%)")) +
        scale_y_continuous(limits = c(0, 100), breaks = seq(from = 20, to = 100, by = 20)) +
        scale_x_continuous(breaks = seq(from = 1, to = max(medication_data$x), by = 2)) +
        scale_color_manual(values = c("Asthma survey" = "#5e8ac4", "Asthma medication" = "#305483", "Eczema survey" = "#C25D5D", "Eczema medication" = "#763030", "Rhinitis survey" = "#5DC273", "Rhinitis medication" = "#2a6839")) +
        theme(
            # panel.background = element_rect(fill = "#ffffff"),
            # axis.line = element_line(color='black'),
            # axis.title.x = element_text(margin = margin(t = 12), size = 15),
            # axis.title.y = element_text(margin = margin(r = 12), size = 15),
            # axis.text.x = element_text(size = 13, color = "black", margin = margin(t = 6)),
            # axis.text.y = element_text(size = 13, color = "black", margin = margin(r = 6)),
            # axis.ticks = element_line(), 
            # axis.ticks.length = unit(0.2, "cm"),
            # legend.position = "none",
            # legend.box = "vertical",
            # # legend.title = element_blank(),
            # legend.key=element_blank(),
            # legend.title = element_text(size = 13),
            # legend.text = element_text(size = 12)
            panel.background = element_rect(fill = "#ffffff"),
            axis.line = element_line(color='black'),
            axis.title.x = element_text(margin = margin(t = 12), size = 22),
            axis.title.y = element_text(margin = margin(r = 12), size = 22),
            axis.text.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
            axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
            axis.ticks = element_line(), 
            axis.ticks.length = unit(0.2, "cm"),
            # legend.position = "none",
            legend.position = "none",
            legend.box = "vertical",
            legend.key=element_blank(),
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 21),
            plot.title = element_text(size = 26.5, hjust = 0.5, margin = margin(b = 12)),
            plot.margin = margin(t = 50, l = 50)
        )
        p_list[[class]] <- p


##########

        }
        # save as svg with dpi 300 and a descriptive name that contains the settings
        # ggsave(
        #     filename = paste0(
        #         output_location,
        #         "svg/",
        #         "trajectories-",
        #         data_source,
        #         "-dataset-",
        #         i,
        #         "-class-",
        #         class,
        #         "-n_iterations-",
        #         n_iterations,
        #         "-n_starts-",
        #         n_starts,
        #         "-only_positive_cases-",
        #         only_positive_cases,
        #         ".svg"
        #     ),
        #     plot = p,
        #     device = "svg",
        #     dpi = 300
        # )
    }
    # save all plots in one file
    grid <- gridExtra::grid.arrange(grobs = p_list[c(1,2,3,4,5,6,7,8,9,10,11,12)], ncol = 3) # for easy class ordering
    #grid <- gridExtra::grid.arrange(grobs = p_list[c(2,4,7,1,6,5,3,9,8)], ncol = 3) # for ordered classes by disease
    ggsave(
        filename = paste0(
            output_location,
            "svg/",
            "TEST----trajectories-",
            data_source,
            "-dataset-",
            i,
            "-n_iterations-",
            n_iterations,
            "-n_starts-",
            n_starts,
            "-only_positive_cases-",
            only_positive_cases,
            stratified,
            ".svg"
        ),
        plot = grid,
        device = "svg",
        dpi = 300,
        height = 21,
        width = 21
    )
    cat('-------- end\n')
}




