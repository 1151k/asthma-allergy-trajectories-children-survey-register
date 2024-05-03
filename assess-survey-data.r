# ASSESS SURVEY DATA IN TERMS OF PATTERNS OF RESPONSE AND MISSINGNESS, AND PERFORM DESCRIPTIVE STATISTICS



# LOAD PACKAGES
packages_all = c("data.table", "ggplot2", "dplyr", "gtsummary", "flextable", "gridExtra", "naniar")
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
survey_data <- readRDS(paste0(output_location, "rds/", "survey-data-raw.rds"))
print('Loaded survey data')
# convert to data.table
data.table::setDT(survey_data)
print(paste('is a data.table:', is.data.table(survey_data)))
# get dimensions
survey_data_dim <- paste0(dim(survey_data), collapse = ',')
print(paste0('Dimensions of survey data: ', survey_data_dim))
n_total_cohort <- nrow(survey_data)
print(colnames(survey_data))
cat('-------- end\n\n')



# CHECK IF MCAR OR NOT
cat('CHECK IF MCAR OR NOT\n')
cat('-------- start\n')
mcar <- naniar::mcar_test(survey_data[, c("asthma_diagnosis_1y_ever", "asthma_treatment_1y_12m", "wheeezing_1y_12m", "asthma_diagnosis_45y_ever", "asthma_treatment_45y_12m", "wheezing_45y_12m", "asthma_diagnosis_8y_ever", "asthma_treatment_8y_12m", "wheezing_8y_12m", "asthma_diagnosis_12y_ever", "asthma_treatment_12y_12m", "wheezing_12y_12m", "eczema_1y_ever", "eczema_45y_12m", "eczema_diagnosis_45y_1_45y", "eczema_treatment_45y_12m", "eczema_8y_12m", "eczema_diagnosis_8y_ever", "eczema_treatment_8y_12m", "eczema_12y_12m", "eczema_diagnosis_12y_ever", "eczema_treatment_12y_12m", "rhinitis_diagnosis_45y_ever", "rhinitis_symptoms_45y_12m", "rhinitis_diagnosis_8y_ever", "rhinitis_treatment_8y_12m", "rhinitis_symptoms_8y_12m", "rhinitis_diagnosis_12y_ever", "rhinitis_treatment_12y_12m", "rhinitis_symptoms_12y_12m"), with = FALSE])
print(mcar)
print('p-value')
print(mcar$p.value)
cat('-------- end\n\n')



# get the proportion of subjects with 1 in survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4
cat('GET THE PROPORTION OF SUBJECTS WITH 1 IN SURVEY_ASTHMA_1, SURVEY_ASTHMA_2, SURVEY_ASTHMA_3, SURVEY_ASTHMA_4\n')
cat('-------- start\n')
# get the proportion of subjects with 1 in survey_asthma_1, survey_asthma_2, survey_asthma_3, survey_asthma_4, multiplying by 100 to get percentage, and the same for rhinitis and eczema
print(sum(survey_data$survey_asthma_1 == 1, na.rm = TRUE))
print(sum(survey_data$survey_asthma_1 == 0, na.rm = TRUE))
print(sum(survey_data$survey_asthma_1 == 0, na.rm = TRUE) + sum(survey_data$survey_asthma_1 == 1, na.rm = TRUE))
print((sum(survey_data$survey_asthma_1 == 1, na.rm = TRUE) / (sum(survey_data$survey_asthma_1 == 0, na.rm = TRUE) + sum(survey_data$survey_asthma_1 == 1, na.rm = TRUE))) * 100)
print(table(survey_data$survey_asthma_1, useNA = "always"))
survey_asthma_1_proportion <- (sum(survey_data$survey_asthma_1 == 1, na.rm = TRUE) / (sum(survey_data$survey_asthma_1 == 0, na.rm = TRUE) + sum(survey_data$survey_asthma_1 == 1, na.rm = TRUE))) * 100
survey_asthma_2_proportion <- (sum(survey_data$survey_asthma_2 == 1, na.rm = TRUE) / (sum(survey_data$survey_asthma_2 == 0, na.rm = TRUE) + sum(survey_data$survey_asthma_2 == 1, na.rm = TRUE))) * 100
survey_asthma_3_proportion <- (sum(survey_data$survey_asthma_3 == 1, na.rm = TRUE) / (sum(survey_data$survey_asthma_3 == 0, na.rm = TRUE) + sum(survey_data$survey_asthma_3 == 1, na.rm = TRUE))) * 100
survey_asthma_4_proportion <- (sum(survey_data$survey_asthma_4 == 1, na.rm = TRUE) / (sum(survey_data$survey_asthma_4 == 0, na.rm = TRUE) + sum(survey_data$survey_asthma_4 == 1, na.rm = TRUE))) * 100
asthma_survey_proportions <- c(survey_asthma_1_proportion, survey_asthma_2_proportion, survey_asthma_3_proportion, survey_asthma_4_proportion)
survey_rhinitis_2_proportion <- (sum(survey_data$survey_rhinitis_2 == 1, na.rm = TRUE) / (sum(survey_data$survey_rhinitis_2 == 0, na.rm = TRUE) + sum(survey_data$survey_rhinitis_2 == 1, na.rm = TRUE))) * 100
survey_rhinitis_3_proportion <- (sum(survey_data$survey_rhinitis_3 == 1, na.rm = TRUE) / (sum(survey_data$survey_rhinitis_3 == 0, na.rm = TRUE) + sum(survey_data$survey_rhinitis_3 == 1, na.rm = TRUE))) * 100
survey_rhinitis_4_proportion <- (sum(survey_data$survey_rhinitis_4 == 1, na.rm = TRUE) / (sum(survey_data$survey_rhinitis_4 == 0, na.rm = TRUE) + sum(survey_data$survey_rhinitis_4 == 1, na.rm = TRUE))) * 100
rhinitis_survey_proportions <- c(survey_rhinitis_2_proportion, survey_rhinitis_3_proportion, survey_rhinitis_4_proportion)
survey_eczema_1_proportion <- (sum(survey_data$survey_eczema_1 == 1, na.rm = TRUE) / (sum(survey_data$survey_eczema_1 == 0, na.rm = TRUE) + sum(survey_data$survey_eczema_1 == 1, na.rm = TRUE))) * 100
survey_eczema_2_proportion <- (sum(survey_data$survey_eczema_2 == 1, na.rm = TRUE) / (sum(survey_data$survey_eczema_2 == 0, na.rm = TRUE) + sum(survey_data$survey_eczema_2 == 1, na.rm = TRUE))) * 100
survey_eczema_3_proportion <- (sum(survey_data$survey_eczema_3 == 1, na.rm = TRUE) / (sum(survey_data$survey_eczema_3 == 0, na.rm = TRUE) + sum(survey_data$survey_eczema_3 == 1, na.rm = TRUE))) * 100
survey_eczema_4_proportion <- (sum(survey_data$survey_eczema_4 == 1, na.rm = TRUE) / (sum(survey_data$survey_eczema_4 == 0, na.rm = TRUE) + sum(survey_data$survey_eczema_4 == 1, na.rm = TRUE))) * 100
eczema_survey_proportions <- c(survey_eczema_1_proportion, survey_eczema_2_proportion, survey_eczema_3_proportion, survey_eczema_4_proportion)
# plot the proportions, asthma with #5D88C2, rhinitis with #5D88C2, and eczema with #C25D5D
individual_diseases_survey_plot <- ggplot2::ggplot() +
    geom_line(aes(x = c(1, 2, 3, 4), y = asthma_survey_proportions, color = "Asthma"), lty = 2, linewidth = 1.2) +
    geom_line(aes(x = c(2, 3, 4), y = rhinitis_survey_proportions, color = "Allergic rhinitis"), lty = 2, linewidth =1.2) +
    geom_line(aes(x = c(1, 2, 3, 4), y = eczema_survey_proportions, color = "Eczema"), lty = 2, linewidth = 1.2) +
    scale_color_manual(values = c("Asthma" = "#5e8ac4", "Allergic rhinitis" = "#5DC273", "Eczema" = "#C25D5D")) +
    scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("1", "4.5", "8", "12")) +
    labs(
        x = "Age (years)",
        y = "% of subjects with the disease",
        color = "Self-reported disease"
    ) +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5), labels = function(x) round(x)) +
    theme(
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(color='black'),
        axis.title.x = element_text(margin = margin(t = 12), size = 21),
        axis.title.y = element_text(margin = margin(r = 12), size = 21),
        axis.text.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
        axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
        axis.ticks = element_line(), 
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        # legend.title = element_blank(),
        legend.key=element_blank(),
        # legend.title = element_text(size = 13),
        legend.title = element_blank(),
        legend.text = element_text(size = 19)
    )
# save the proportions plot
# ggsave(paste0(output_location, "svg/", "individual-self-reported-disease-per-year.svg"), plot = individual_diseases_survey_plot, device = "svg", dpi = 300)



# two disease plot
survey_data$two_diseases_1 <- ifelse(survey_data$survey_asthma_1 == 1 & survey_data$survey_eczema_1 == 1, 1, 0)
print('two diseases at 1')
print(table(survey_data$two_diseases_1, useNA = "always"))
print(nrow(survey_data[survey_asthma_1 == 1]))
print(nrow(survey_data[survey_eczema_1 == 1]))
print(nrow(survey_data[survey_asthma_1 == 1 & survey_eczema_1 == 1]))
print('---')
survey_data$two_diseases_2 <- ifelse((survey_data$survey_asthma_2 == 1 & survey_data$survey_rhinitis_2 == 1) | (survey_data$survey_asthma_2 == 1 & survey_data$survey_eczema_2 == 1) | (survey_data$survey_rhinitis_2 == 1 & survey_data$survey_eczema_2 == 1), 1, 0)
survey_data$two_diseases_3 <- ifelse((survey_data$survey_asthma_3 == 1 & survey_data$survey_rhinitis_3 == 1) | (survey_data$survey_asthma_3 == 1 & survey_data$survey_eczema_3 == 1) | (survey_data$survey_rhinitis_3 == 1 & survey_data$survey_eczema_3 == 1), 1, 0)
survey_data$two_diseases_4 <- ifelse((survey_data$survey_asthma_4 == 1 & survey_data$survey_rhinitis_4 == 1) | (survey_data$survey_asthma_4 == 1 & survey_data$survey_eczema_4 == 1) | (survey_data$survey_rhinitis_4 == 1 & survey_data$survey_eczema_4 == 1), 1, 0)
two_diseases_1_proportion <- (sum(survey_data$two_diseases_1 == 1, na.rm = TRUE) / (sum(survey_data$two_diseases_1 == 0, na.rm = TRUE) + sum(survey_data$two_diseases_1 == 1, na.rm = TRUE))) * 100
two_diseases_2_proportion <- (sum(survey_data$two_diseases_2 == 1, na.rm = TRUE) / (sum(survey_data$two_diseases_2 == 0, na.rm = TRUE) + sum(survey_data$two_diseases_2 == 1, na.rm = TRUE))) * 100
two_diseases_3_proportion <- (sum(survey_data$two_diseases_3 == 1, na.rm = TRUE) / (sum(survey_data$two_diseases_3 == 0, na.rm = TRUE) + sum(survey_data$two_diseases_3 == 1, na.rm = TRUE))) * 100
two_diseases_4_proportion <- (sum(survey_data$two_diseases_4 == 1, na.rm = TRUE) / (sum(survey_data$two_diseases_4 == 0, na.rm = TRUE) + sum(survey_data$two_diseases_4 == 1, na.rm = TRUE))) * 100
two_diseases_proportions <- c(two_diseases_1_proportion, two_diseases_2_proportion, two_diseases_3_proportion, two_diseases_4_proportion)
two_diseases_survey_plot <- ggplot2::ggplot() +
    geom_line(aes(x = c(1, 2, 3, 4), y = two_diseases_proportions, color = "Parental report of two diseases"), lty = 2, linewidth = 1.2) +
    scale_color_manual(values = c("Parental report of two diseases" = "#333333")) +
    scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("1", "4.5", "8", "12")) +
    labs(
        x = "Age (years)",
        y = "% of subjects with data",
        labs = "Medication for two diseases"
    ) +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5), labels = function(x) round(x)) +
    theme(
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(color='black'),
        axis.title.x = element_text(margin = margin(t = 12), size = 21),
        # axis.title.y = element_text(margin = margin(r = 12), size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
        axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
        axis.ticks = element_line(), 
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 19),
        legend.key=element_blank()
    )
# save the proportions plot
# ggsave(paste0(output_location, "svg/", "two-diseases-per-year.svg"), plot = two_diseases_survey_plot, device = "svg", dpi = 300)




# three diseases
survey_data$three_diseases_2 <- ifelse(survey_data$survey_asthma_2 == 1 & survey_data$survey_rhinitis_2 == 1 & survey_data$survey_eczema_2 == 1, 1, 0)
survey_data$three_diseases_3 <- ifelse(survey_data$survey_asthma_3 == 1 & survey_data$survey_rhinitis_3 == 1 & survey_data$survey_eczema_3 == 1, 1, 0)
survey_data$three_diseases_4 <- ifelse(survey_data$survey_asthma_4 == 1 & survey_data$survey_rhinitis_4 == 1 & survey_data$survey_eczema_4 == 1, 1, 0)
three_diseases_2_proportion <- (sum(survey_data$three_diseases_2 == 1, na.rm = TRUE) / (sum(survey_data$three_diseases_2 == 0, na.rm = TRUE) + sum(survey_data$three_diseases_2 == 1, na.rm = TRUE))) * 100
three_diseases_3_proportion <- (sum(survey_data$three_diseases_3 == 1, na.rm = TRUE) / (sum(survey_data$three_diseases_3 == 0, na.rm = TRUE) + sum(survey_data$three_diseases_3 == 1, na.rm = TRUE))) * 100
three_diseases_4_proportion <- (sum(survey_data$three_diseases_4 == 1, na.rm = TRUE) / (sum(survey_data$three_diseases_4 == 0, na.rm = TRUE) + sum(survey_data$three_diseases_4 == 1, na.rm = TRUE))) * 100
three_diseases_proportions <- c(three_diseases_2_proportion, three_diseases_3_proportion, three_diseases_4_proportion)
three_diseases_survey_plot <- ggplot2::ggplot() +
    geom_line(aes(x = c(2, 3, 4), y = three_diseases_proportions, color = "Parental report of three diseases"), lty = 2, linewidth = 1.2) +
    scale_color_manual(values = c("Parental report of three diseases" = "#333333")) +
    scale_x_continuous(breaks = c(2, 3, 4), labels = c("4.5", "8", "12")) +
    labs(
        x = "Age (years)",
        y = "% of subjects with data",
        labs = "Parental report of three diseases"
    ) +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5), labels = function(x) round(x)) +
    theme(
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(color='black'),
        axis.title.x = element_text(margin = margin(t = 12), size = 21),
        # axis.title.y = element_text(margin = margin(r = 12), size = 21),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
        axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
        axis.ticks = element_line(), 
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 19),
        legend.key=element_blank()
    )
# save the proportions plot
# ggsave(paste0(output_location, "svg/", "three-diseases-per-year.svg"), plot = three_diseases_survey_plot, device = "svg", dpi = 300)



# combine the three figures above with gridExtra
combined_diseases_plot <- gridExtra::grid.arrange(individual_diseases_survey_plot, two_diseases_survey_plot, three_diseases_survey_plot, ncol = 3)
ggsave(paste0(output_location, "svg/", "self-reported-disease-per-year.svg"), plot = combined_diseases_plot, device = "svg", width = 20, height = 6, dpi = 300)



# summarize data
table <- survey_data %>%
    tbl_summary(
        # by = cluster
        # , statistic = list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{p}")
    ) %>%
    # add_overall() %>%
    # add_p() %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(output_location, 'docx/', 'characeristics-raw.docx'))