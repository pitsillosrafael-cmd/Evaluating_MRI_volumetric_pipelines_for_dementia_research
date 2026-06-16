library(tidyr)
library(ggplot2)
library(dplyr)

                                                            # PARCELLATIONS

# Assuming df has columns: age, lh_entorhinal, lh_inferiorparietal, ..., rh_insula
# Select relevant columns: age + all chosen areas
selected_cols <- c("AGE", "lh_entorhinal", "lh_inferiorparietal", "lh_inferiortemporal",
                   "lh_middletemporal", "lh_parahippocampal", "lh_temporalpole", "lh_insula",
                   "rh_entorhinal", "rh_inferiorparietal", "rh_inferiortemporal",
                   "rh_middletemporal", "rh_parahippocampal", "rh_temporalpole", "rh_insula")

df_selected <- df_parcellations_longitudinal   %>% select(all_of(selected_cols))

# Pivot longer to get one thickness per row per area
df_long <- df_selected %>%
  pivot_longer(cols = -AGE, names_to = "region", values_to = "thickAvg")

# Now plot
ggplot(df_long, aes(x = factor(AGE), y = thickAvg)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Age", y = "Cortical Thickness (mm)", title = "Distribution of Thickness Across Areas (Entorhinal, Temporal, Insula, Parietal, Parahippocampal) by Age - recon-all") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# RECON-CLINICAL
file_path <- "/home/rafaelp/SUBJECTS_miriad/miriad_AD_longitudinal/recon-all-clinical/miriad_AD_longitudinal_recon_clinical_parcellation_combined.csv"
df_aparc_clinical <- read.csv(file_path)

# Define selected regions with .lh and .rh
# Define the brain region columns using _lh and _rh
selected_cols_clin <- c("AGE", 
                   "entorhinal_lh", "inferiorparietal_lh", "inferiortemporal_lh",
                   "middletemporal_lh", "parahippocampal_lh", "temporalpole_lh", "insula_lh",
                   "entorhinal_rh", "inferiorparietal_rh", "inferiortemporal_rh",
                   "middletemporal_rh", "parahippocampal_rh", "temporalpole_rh", "insula_rh")

# Subset the dataframe
df_selected_parcellations_clinical <- df_aparc_clinical %>% select(all_of(selected_cols_clin))

# Pivot longer to get region and thickness in long format
df_long_parcellations_clinical <- df_selected_parcellations_clinical %>%
  pivot_longer(cols = -AGE, names_to = "region", values_to = "thickAvg")

# Create the boxplot
ggplot(df_long_parcellations_clinical, aes(x = factor(AGE), y = thickAvg)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Age", y = "Cortical Thickness (mm)",
       title = "Distribution of Thickness Across Areas (Entorhinal, Temporal, Insula, Parietal, Parahippocampal) by Age - recon-all-clinical") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

                                                      # Selected parcellations CORRECT(entorhinal left and right ) 

setwd("/home/rafaelp/SUBJECTS_miriad/miriad_AD_longitudinal_correct")
# Load the CSV
df <- read.csv("miriad_AD_longitudinal_parcellation_combined_TP1-10.csv")

# Add a subject ID (every 10 rows is a new subject)
df <- df %>%
  mutate(
    Subject = rep(paste0("S", 1:3), each = 10),
    TimePoint = subject,
    TimePoint = factor(TimePoint, levels = paste0("tp_", 1:10))
  )

# Reshape to long format for plotting
df_long_correct <- df %>%
  select(Subject, TimePoint, entorhinal_lh, entorhinal_rh) %>%
  pivot_longer(cols = c(entorhinal_lh, entorhinal_rh),
               names_to = "Hemisphere",
               values_to = "Thickness") %>%
  mutate(
    Hemisphere = recode(Hemisphere,
                        entorhinal_lh = "Left",
                        entorhinal_rh = "Right")
  )

# Plot
# Plot: Boxplots for left & right entorhinal thickness at each time point
ggplot(df_long_correct, aes(x = TimePoint, y = Thickness, fill = Hemisphere)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(
    title = "Entorhinal Thickness Across Time Points",
    x = "Time Point",
    y = "Cortical Thickness (mm)",
    fill = "Hemisphere"
  ) +
  theme_minimal()


# for line plot
# Add numeric time for plotting continuous x-axis
df_long_correct <- df_long_correct %>%
  mutate(TimeNumeric = as.numeric(str_remove(as.character(TimePoint), "tp_")))

# Plot: all subjects, both hemispheres, linear fit per subject & hemisphere
ggplot(df_long_correct, aes(x = TimeNumeric, y = Thickness, color = Subject, linetype = Hemisphere)) +
  geom_point(size = 2) +                 # scatter points only
  geom_smooth(method = "lm", se = FALSE, size = 1.2, alpha = 0.7) +  # best-fit lines
  scale_x_continuous(breaks = 1:10, labels = paste0("tp_", 1:10)) +
  labs(
    title = "Entorhinal Cortex Thickness Over Time (with Linear Fit)",
    x = "Time Point",
    y = "Cortical Thickness (mm)",
    color = "Subject",
    linetype = "Hemisphere"
  ) +
  theme_minimal()

                                                                  # Segmentations
library(tidyr)
library(dplyr)
library(ggplot2)

# Selected regions (correct commas and spaces)
selected_small_regions_aseg <- c(
  "Left.Hippocampus",
  "Right.Hippocampus"
)

selected_big_regions_aseg <- c(
  "Left.Cerebellum.White.Matter",
  "Left.Cerebellum.Cortex",
  "Left.Thalamus",
  "Right.Cerebellum.White.Matter",
  "Right.Cerebellum.Cortex",
  "Right.Thalamus"
)

seg_df <- structure_df_longit %>%
  left_join(df_parcellations_longitudinal %>% select(subject, AGE), by = "subject")

# Select age and chosen regions
seg_selected <- seg_df %>% select(AGE, all_of(selected_small_regions_aseg))

# Pivot longer for plotting
seg_long <- seg_selected %>%
  pivot_longer(cols = -AGE, names_to = "region", values_to = "volume")

# # Calculate mean volume per age across the selected regions
# mean_volumes <- seg_long %>%
#   group_by(AGE) %>%
#   summarise(mean_volume = mean(volume, na.rm = TRUE))
# print(mean_volumes)

# Plot boxplots of volume distributions by age
ggplot(seg_long, aes(x = factor(AGE), y = volume)) +
  geom_boxplot(fill = "lightgreen") +
  labs(x = "Age", y = "Volume (mm³)", title = "Volumes of Left and Right Hippocampi by Age - Recon-all") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Linear decline - recon all-clinical fro hippocampal volume
ggplot(seg_long, aes(x = AGE, y = volume, color = region)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2) +
  labs(title = "Linear Decline of Hippocampal Volume with Age - Recon-all",
       x = "Age",
       y = "Hippocampal Volume (mm³)",
       color = "Region") +
  theme_minimal()


                                                                 # ALL 3 SUBJECTS TOGETHER

# Volumes of hippocampi of the 3 subjects longitudinal
base_dir <- "/home/rafaelp/SUBJECTS_miriad/miriad_AD_longitudinal/"

# File names
files <- c(
  "miriad_longitudinal_structures_239_combined.csv",
  "miriad_longitudinal_structures_188_combined.csv",
  "miriad_longitudinal_structures_193_combined.csv"
)

# Initialize empty data frame
hippo_data_longitudinal <- data.frame()

for (file in files) {
  full_path <- file.path(base_dir, file)
  df_longitudinal_hippo <- read_csv(full_path)
  
  lh_col <- "Left.Hippocampus"
  rh_col <- "Right.Hippocampus"
  
  subject_id <- gsub(".csv", "", basename(file))
  
  temp_df <- df_longitudinal_hippo %>%
    select(all_of(c(lh_col, rh_col))) %>%
    mutate(TimePoint = 1:nrow(df_longitudinal_hippo),
           Subject = subject_id) %>%
    rename(LH = lh_col, RH = rh_col) %>%
    pivot_longer(cols = c("LH", "RH"), names_to = "Side", values_to = "HippocampalVolume")
  
  hippo_data_longitudinal <- bind_rows(hippo_data_longitudinal, temp_df)
}

hippo_data_longitudinal$TimePoint <- factor(hippo_data_longitudinal$TimePoint)

# plot
ggplot(hippo_data_longitudinal, aes(x = TimePoint, y = HippocampalVolume, fill = Side, group = interaction(TimePoint, Side))) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6) +
  labs(
    title = "Left and Right Hippocampal Volumes Across Time Points",
    x = "MRI Time Point",
    y = "Hippocampal Volume"
  ) +
  scale_fill_manual(values = c("LH" = "lightblue", "RH" = "lightpink")) +
  theme_minimal()
