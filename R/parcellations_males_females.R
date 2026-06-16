library(dplyr)
library(ggplot2)
library(stringr)
library(ggpubr)

# NEW data format with entorhinal middletemporal posteriorcinculate parahippocampal and insula
# Define the updated list of regions
selected_regions_for_AD <- c("entorhinal", "middletemporal", "posteriorcingulate", "parahippocampal", "insula","fusiform","precuneus")

# Filter the dataset
filtered_data_5ADregions <- df_all_cortical_regions %>%
  filter(
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere %in% c("lh", "rh"),
    Region %in% selected_regions_for_AD
  )

filtered_data_lh_5AD <- filtered_data_5ADregions %>%
  filter(Hemisphere == "lh")  # Left hemisphere only

filtered_data_rh_5AD <- filtered_data_5ADregions %>%
  filter(Hemisphere == "rh")  # Left hemisphere only

ggplot(filtered_data_lh_5AD, aes(x = Region, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  facet_wrap(~ Pipeline, scales = "free_x") +
  stat_compare_means(
    aes(group = Diagnosis),
    method = "t.test",
    label = "p.format",
    label.x.npc = "center",
    label.y.npc = "top",
    size = 3
  ) +
  labs(title = "Left Hemisphere Cortical Thickness in AD vs HC Across Affected and Unaffected Regions",
       x = "Region",
       y = "ThickAvg (mm)") +
  theme_minimal() +
  scale_fill_manual(values = c("AD" = "#F8766D", "HC" = "#00BFC4")) +  # Optional: match your color scheme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

# Extract sex from subject ID
data_5AD_combined_sex <- filtered_data_5ADregions %>%
  mutate(Sex = ifelse(str_detect(subject, "_M_"), "Male",
                      ifelse(str_detect(subject, "_F_"), "Female", NA)))

# Females
filtered_5AD_data_females_rh <- data_5AD_combined_sex %>%
  filter(
    Sex == "Female",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "rh",
    Region %in% c("entorhinal", "middletemporal", "posteriorcingulate", "parahippocampal", "insula","fusiform","precuneus")
  )

# males
filtered_5AD_data_males_rh <- data_5AD_combined_sex %>%
  filter(
    Sex == "Male",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "rh",
    Region %in% c("entorhinal", "middletemporal", "posteriorcingulate", "parahippocampal", "insula","fusiform","precuneus")
  )

filtered_5AD_data_females_lh <- data_5AD_combined_sex %>%
  filter(
    Sex == "Female",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "lh",
    Region %in% c("entorhinal", "middletemporal", "posteriorcingulate", "parahippocampal", "insula","fusiform","precuneus")
  )

# males
filtered_5AD_data_males_lh <- data_5AD_combined_sex %>%
  filter(
    Sex == "Male",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "lh",
    Region %in% c("entorhinal", "middletemporal", "posteriorcingulate", "parahippocampal", "insula","fusiform","precuneus")
  )

                                                                        # SHAPIRO

# Run Shapiro-Wilk test on each subgroup RIGHT
shapiro_results_females_regions_rh <- filtered_5AD_data_females_rh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_females_regions_rh, "shapiro_results_females_rh.csv", row.names = FALSE)


# Run Shapiro-Wilk test on each subgroup LEFT
shapiro_results_females_regions_lh <- filtered_5AD_data_females_lh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_females_regions_lh, "shapiro_results_females_lh.csv", row.names = FALSE)

# MALES
# Run Shapiro-Wilk test on each subgroup LEFT
shapiro_results_males_regions_lh <- filtered_5AD_data_males_lh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_males_regions_lh, "shapiro_results_males_lh.csv", row.names = FALSE)


# RIGHT
shapiro_results_males_regions_rh <- filtered_5AD_data_males_rh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_males_regions_rh, "shapiro_results_males_rh.csv", row.names = FALSE)



                                                                            # PLOT

pvals_female <- read.csv("t-test_OR_Wilcox_7_female_regions.csv")

# Prepare the p-values for LEFT hemisphere and selected pipelines
pvals_female_lh <- pvals_female %>%
  filter(Hemisphere == "lh", Pipeline %in% c("recon-all", "clinical")) %>%
  mutate(
    Sex = "Female",
    p_label = paste0("p = ", signif(p_value, 3))
  )

# Filter plotting data for LH and relevant pipelines
filtered_plot_data <- filtered_5AD_data_females %>%
  filter(Hemisphere == "lh", Pipeline %in% c("recon-all", "clinical"))

# Plot with p-values
ggplot(filtered_plot_data, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ Region, scales = "free_y") +
  geom_text(
    data = pvals_female_lh,
    aes(x = Pipeline, y = 3.6, label = p_label),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  labs(
    title = "Left Hemisphere Cortical Thickness (Females)",
    x = "Pipeline", y = "ThickAvg (mm)"
  ) +
  scale_fill_manual(values = c("AD" = "#F8766D", "HC" = "#00BFC4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Prepare the p-values for RIGHT hemisphere and selected pipelines
pvals_female_rh <- pvals_female %>%
  filter(Hemisphere == "rh", Pipeline %in% c("recon-all", "clinical")) %>%
  mutate(
    Sex = "Female",
    p_label = paste0("p = ", signif(P_Value, 3))
  )

# Filter plotting data for LH and relevant pipelines
filtered_plot_data <- filtered_5AD_data_females_rh %>%
  filter(Hemisphere == "rh", Pipeline %in% c("recon-all", "clinical"))

# Plot with p-values
ggplot(filtered_plot_data, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ Region, scales = "free_y") +
  geom_text(
    data = pvals_female_rh,
    aes(x = Pipeline, y = 3.6, label = p_label),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  labs(
    title = "Right Hemisphere Cortical Thickness (Females)",
    x = "Pipeline", y = "ThickAvg (mm)"
  ) +
  scale_fill_manual(values = c("AD" = "#F8766D", "HC" = "#00BFC4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# MALES
pvals_male_lh <- read.csv("t-test_OR_Wilcox_7_male_regions.csv") %>%
  filter(Hemisphere == "lh", Pipeline %in% c("recon-all", "clinical")) %>%
  mutate(
    Sex = "Male",
    p_label = paste0("p = ", signif(P_Value, 3))  # or p_value if lowercase
  )

filtered_plot_data_male_lh <- filtered_7AD_data_males %>%
  filter(Hemisphere == "lh", Pipeline %in% c("recon-all", "clinical"))

ggplot(filtered_plot_data_male_lh, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7, outlier.shape = NA) +
  facet_wrap(~ Region, scales = "free_y") +
  geom_text(
    data = pvals_male_lh,
    aes(x = Pipeline, y = 3.6, label = p_label),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  labs(
    title = "Left Hemisphere Cortical Thickness (Males)",
    x = "Pipeline", y = "ThickAvg (mm)"
  ) +
  scale_fill_manual(values = c("AD" = "#F8766D", "HC" = "#00BFC4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot the box plots
ggplot(filtered_5AD_data_females, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ Region) +
  labs(title = "Cortical Thickness in RH Regions (Females, AD vs HC)",
       x = "Pipeline", y = "Average Thickness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))




# 5 random regions
# Extract sex from subject ID
data_ALL_parcellations_combined_sex <- data_ALL_parcellations_combined %>%
  mutate(Sex = ifelse(str_detect(subject, "_M_"), "Male",
                      ifelse(str_detect(subject, "_F_"), "Female", NA)))

# Females lh&rh
filtered_data_females_rh <- data_ALL_parcellations_combined_sex %>%
  filter(
    Sex == "Female",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "rh",
    Region %in% c("entorhinal", "fusiform", "precuneus", "parahippocampal", "insula")
  )

filtered_data_females_lh <- data_ALL_parcellations_combined_sex %>%
  filter(
    Sex == "Female",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "lh",
    Region %in% c("entorhinal", "fusiform", "precuneus", "parahippocampal", "insula")
  )


# Plot the box plots
ggplot(filtered_data_females_rh, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ Region) +
  labs(title = "Cortical Thickness in RH Regions (Females, AD vs HC)",
       x = "Pipeline", y = "Average Thickness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Plot the box plots
ggplot(filtered_data_females_lh, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ Region) +
  labs(title = "Cortical Thickness in RH Regions (Females, AD vs HC)",
       x = "Pipeline", y = "Average Thickness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


# Males lh&rh
filtered_data_males_rh <- data_ALL_parcellations_combined_sex %>%
  filter(
    Sex == "Male",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "rh",
    Region %in% c("entorhinal", "fusiform", "precuneus", "parahippocampal", "insula")
  )

filtered_data_males_lh <- data_ALL_parcellations_combined_sex %>%
  filter(
    Sex == "Male",
    Diagnosis %in% c("AD", "HC"),
    Pipeline %in% c("recon-all", "clinical"),
    Hemisphere == "lh",
    Region %in% c("entorhinal", "fusiform", "precuneus", "parahippocampal", "insula")
  )

# Plot the box plots
ggplot(filtered_data_males, aes(x = Pipeline, y = ThickAvg, fill = Diagnosis)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~ Region) +
  labs(title = "Cortical Thickness in RH Regions (Males, AD vs HC)",
       x = "Pipeline", y = "Average Thickness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



# CHECK THEIR p-values
# FEMALES
# Run Shapiro-Wilk test on each subgroup RIGHT
shapiro_results_females_5regions_rh <- filtered_data_females_rh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_females_5regions_rh, "shapiro_results_females_rh.csv", row.names = FALSE)


# Run Shapiro-Wilk test on each subgroup LEFT
shapiro_results_females_5regions_lh <- filtered_data_females_lh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_females_5regions_lh, "shapiro_results_females_lh.csv", row.names = FALSE)

# MALES
# Run Shapiro-Wilk test on each subgroup LEFT
shapiro_results_males_5regions_lh <- filtered_data_males_lh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_males_5regions_lh, "shapiro_results_males_lh.csv", row.names = FALSE)


# RIGHT
shapiro_results_males_5regions_rh <- filtered_data_males_rh %>%
  group_by(Region, Hemisphere, Pipeline, Diagnosis) %>%
  summarise(
    shapiro = list(shapiro.test(ThickAvg)),
    .groups = "drop"
  ) %>%
  mutate(
    W = map_dbl(shapiro, ~ .x$statistic),
    p_value = map_dbl(shapiro, ~ .x$p.value)
  ) %>%
  select(-shapiro)  # drop the test object column if not needed
write.csv(shapiro_results_males_5regions_rh, "shapiro_results_males_rh.csv", row.names = FALSE)
