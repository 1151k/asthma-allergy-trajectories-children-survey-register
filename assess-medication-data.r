# TODO! ADD DESCRIPTION



# LOAD PACKAGES
packages_all = c("haven", "data.table", "dplyr", "ggplot2", "gridExtra")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# CONSTANTS AND GENERAL DEFINITIONS
input_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/input/" # path to input data
output_location <- "/Users/xlisda/Downloads/it/server/research/phd/taa/output/" # path to output data
NPDR_filename <- paste0(input_location, "SOS_grundfil_LKM20032016_181107.sav") # path and filename of input data (NPDR data)
source("ATC_CODES.R") # load ATC codes
n_total_cws <- 5654



# PRINT SETTINGS
cat('SETTINGS\n')
cat('-------- start\n')
cat('-------- end\n\n')



# LOAD DATA FROM output_location/rds/survey-data-raw.rds
cat('LOAD DATA\n')
cat('-------- start\n')
NPDR_data <- read_sav(NPDR_filename)
print('Loaded NPDR data')
data.table::setDT(NPDR_data)
dimensions <- paste(dim(NPDR_data), collapse = ',')
print(paste('Dimensions of NPDR data:', dimensions))
cat('-------- end\n\n')



# PREPROCESS DATA (MAKE VARIABLES, REMOVE UNNECESSARY VARIABLES ETC)
cat('PREPROCESS DATA\n')
cat('-------- start\n')

print('Make disease variables')
# asthma
NPDR_data[, asthma_medication := 0]
for (code in asthma_atc_codes) {
  NPDR_data[grepl(paste0("^", code), atc), asthma_medication := 1]
}
# allergic rhinitis
NPDR_data[, allergic_rhinitis_medication := 0]
for (code in rhinitis_atc_codes) {
  NPDR_data[grepl(paste0("^", code), atc), allergic_rhinitis_medication := 1]
}
# eczema
NPDR_data[, eczema_medication := 0]
for (code in eczema_atc_codes) {
  NPDR_data[grepl(paste0("^", code), atc), eczema_medication := 1]
}

print('Make year and month variables')
NPDR_data$year <- as.integer(format(as.Date(NPDR_data$EDATUM, format="%Y-%m-%d"), "%Y"))
NPDR_data$month <- as.integer(format(as.Date(NPDR_data$EDATUM, format="%Y-%m-%d"), "%m"))


print('Remove unnecessary variables')
NPDR_data <- NPDR_data[, .(lopnr, asthma_medication, allergic_rhinitis_medication, eczema_medication, year, month)]

cat('-------- end\n\n')



# PLOT PRESCRIPTION PER MONTH FOR ASTHMA MEDICATION, RHINITIS MEDICATION, AND ECZEMA MEDICATION BY SEPARATE LINES
cat('PLOT PRESCRIPTION PER MONTH\n')
cat('-------- start\n')

plot_monthly_data <- function(data, disease, color) {
    monthly_plot <- ggplot(data, aes(x = month, y = n, group = year)) +
        geom_line(color = color, linewidth = .8) +
        labs(x = "Month", y = paste0("% of subjects that dispensed ", disease, " medication")) +
        theme(
            axis.line = element_line(color='black'),
            panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_text(margin = margin(t = 12), size = 16.5),
            axis.title.y = element_text(margin = margin(r = 12), size = 15.5),
            axis.text.x = element_text(size = 14, color = "black", margin = margin(t = 6)),
            axis.text.y = element_text(size = 14, color = "black", margin = margin(r = 6)),
            axis.ticks.length = unit(0.2, "cm")
        ) +
        ylim(0, 3) +
        scale_x_continuous(breaks = c(1:12), labels = month.abb)
    return (monthly_plot)
}

# asthma
asthma_medication_per_month <- NPDR_data[asthma_medication == 1, .(n = .N/n_total_cws*100), by = .(year, month)]
asthma_medication_per_month <- asthma_medication_per_month[order(year, month)]
asthma_monthly_plot <- plot_monthly_data(asthma_medication_per_month, "asthma", "#5D88C2")
# rhinitis
rhinitis_medication_per_month <- NPDR_data[allergic_rhinitis_medication == 1, .(n = .N/n_total_cws*100), by = .(year, month)]
rhinitis_medication_per_month <- rhinitis_medication_per_month[order(year, month)]
rhinitis_monthly_plot <- plot_monthly_data(rhinitis_medication_per_month, "rhinitis", "#5DC273")
# eczema
eczema_medication_per_month <- NPDR_data[eczema_medication == 1, .(n = .N/n_total_cws*100), by = .(year, month)]
eczema_medication_per_month <- eczema_medication_per_month[order(year, month)]
eczema_monthly_plot <- plot_monthly_data(eczema_medication_per_month, "eczema", "#C25D5D")

# combine the plots with gridExtra
monthly_plots <- grid.arrange(asthma_monthly_plot, rhinitis_monthly_plot, eczema_monthly_plot, ncol = 3)
# save as svg with dpi 300
ggsave(
    paste0(output_location, "svg/medication-per-month.svg"),
    plot = monthly_plots,
    width = 21,
    height = 6,
    dpi = 300
)

cat('-------- end\n\n')



# PLOT PRESCRIPTION PATTERNS OVER THE YEARS (YEARLY PROPORTION) FOR EACH DISEASE AS PER ABOVE
cat('PLOT PRESCRIPTION PATTERNS OVER THE YEARS\n')
cat('-------- start\n')

npdr_all_years_data <- readRDS(paste0(output_location, "rds/npdr-data-all-years-long.rds"))
npdr_two_year_strata_data <- readRDS(paste0(output_location, "rds/npdr-data-6-time-points-long.rds"))
print(dim(npdr_two_year_strata_data))
print(colnames(npdr_two_year_strata_data))

# mutate a new variable called two_medications which is 1 if the individual has medication for two of the three diseases (asthma_medication, rhinitis_medication, eczema_medication) in npdr_all_years_data
npdr_two_year_strata_data <- npdr_two_year_strata_data %>%
    mutate(
        two_medications = ifelse(
            (asthma_medication + rhinitis_medication + eczema_medication) == 2,
            1,
            0
        )
    )
print('npdr_all_years_data')
print(dim(npdr_two_year_strata_data[two_medications == 1, ]))
# sort by two_medications descending
# plot age on x-axis and proportion of two_medications on y-axis
two_meds_data <- npdr_two_year_strata_data %>%
    group_by(age) %>%
    summarise(
        two_medications = 100*(sum(two_medications) / n())
    )
print('npdr_all_years_data')
# plot proportion of individuals with medication for two of the three diseases
plot_6_timepoints_medication_two <- ggplot(two_meds_data, aes(x = 2 * age + 1)) +
    geom_line(aes(y = two_medications, color = "Dispensed medication for two diseases"), linewidth = 1.2) +
    # geom_text(aes(y = two_medications, label = round(two_medications, 3)), vjust = -0.5, size = 3) +
    scale_x_continuous(breaks = seq(min(2 * two_meds_data$age + 1), max(2 * two_meds_data$age + 1), by = 2)) +
    labs(
        x = "Age (years)",
        y = "Percentage (%) of study population",
        # color = "Dispensed medication for two diseases"
    ) +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5), labels = function(x) round(x)) +
    scale_color_manual(values = c("Dispensed medication for two diseases" = "#213434")) +
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
    # + ylim(0, 11)
# # save to output/svg folder with dpi 300
# ggsave(
#     paste0(output_location, "svg/two-disease-medications-yearly-plot.svg"),
#     plot = plot_6_timepoints_medication_two,
#     width = 8,
#     height = 6,
#     dpi = 300
# )



asthma_medication_per_year <- npdr_two_year_strata_data %>%
    group_by(age) %>%
    summarise(
        asthma_medication_perc = 100*(sum(asthma_medication) / n())
    )
rhinitis_medication_per_year <- npdr_two_year_strata_data %>%
    group_by(age) %>%
    summarise(
        rhinitis_medication_perc = 100*(sum(rhinitis_medication) / n())
    )
eczema_medication_per_year <- npdr_two_year_strata_data %>%
    group_by(age) %>%
    summarise(
        eczema_medication_perc = 100*(sum(eczema_medication) / n())
    )
# plot each vector with ggplot2 in the same plot, with year on the x-axis and proportion on the y-axis
year_plot <- ggplot() +
    geom_line(data = asthma_medication_per_year, aes(x = 2 * age + 1, y = asthma_medication_perc, color = "Asthma"), linewidth=1.2) +
    geom_line(data = rhinitis_medication_per_year, aes(x = 2 * age + 1, y = rhinitis_medication_perc, color = "Allergic rhinitis"), linewidth=1.2) +
    geom_line(data = eczema_medication_per_year, aes(x = 2 * age + 1, y = eczema_medication_perc, color = "Eczema"), linewidth=1.2) +
    scale_x_continuous(breaks = seq(min(2 * asthma_medication_per_year$age + 1), max(2 * asthma_medication_per_year$age + 1), by = 2)) +
    labs(
        x = "Age (years)",
        y = "% of subjects with the disease",
        color = "Dispensed medication"
    ) +
    scale_color_manual(values = c("Asthma" = "#5e8ac4", "Allergic rhinitis" = "#5DC273", "Eczema" = "#C25D5D")) +
    theme_classic() +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5), labels = function(x) round(x)) +
    theme(
        axis.title.x = element_text(margin = margin(t = 12), size = 22),
        axis.title.y = element_text(margin = margin(r = 12), size = 22),
        axis.text.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
        axis.text.y = element_text(size = 19, color = "black", margin = margin(r = 6)),
        axis.ticks = element_line(),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position = "top",
        # legend.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 19)
    )
# # save as svg with dpi 300
# ggsave(
#     paste0(output_location, "svg/medication-per-year.svg"),
#     plot = year_plot,
#     width = 8,
#     height = 6,
#     dpi = 300
# )



# plot proportion of subjects with medication for all thre diseases
three_medications_per_year <- npdr_two_year_strata_data %>%
    group_by(age) %>%
    summarise(
        three_medications = 100*(sum(asthma_medication + rhinitis_medication + eczema_medication == 3) / n())
    )
print(three_medications_per_year)
three_medications_plot <- ggplot(three_medications_per_year, aes(x = 2 * age + 1, y = three_medications, color = "Dispensed medication for all three diseases")) +
    geom_line(linewidth = 1.2) +
    labs(
        x = "Age (years)",
        y = "% of study population"
    ) +
    scale_x_continuous(breaks = seq(min(2 * three_medications_per_year$age + 1), max(2 * three_medications_per_year$age + 1), by = 2)) +
    scale_color_manual(values = c("Dispensed medication for all three diseases" = "#333333")) +
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
# # save to output/svg folder with dpi 300
# ggsave(paste0(output_location, "svg/three-disease-medications-yearly-plot.svg"), plot = three_medications_plot, width = 8, height = 6, dpi = 300)



# merge year_plot and plot_6_timepoints_medication_two with gridExtra as a single plot
combined_plots <- grid.arrange(year_plot, plot_6_timepoints_medication_two, three_medications_plot, ncol = 3)
# save as svg with dpi 300
ggsave(
    paste0(output_location, "svg/medication-per-year.svg"),
    plot = combined_plots,
    width = 20,
    height = 6,
    dpi = 300
)