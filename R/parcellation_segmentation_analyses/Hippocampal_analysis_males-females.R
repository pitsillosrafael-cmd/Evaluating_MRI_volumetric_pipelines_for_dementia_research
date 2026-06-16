library(dplyr)

base_dir <- "/home/rafaelp/SUBJECTS_miriad"
files <- c(
  "merged_synthseg_volumes_AD.csv",
  "miriad_AD_all_structures_combined.csv",
  "miriad_HC_all_structures_combined.csv",
  "merged_synthseg_volumes_HC.csv"
)

# Function to determine source label
get_source_label <- function(filename) {
  if (grepl("miriad_.*_all_structures_combined\\.csv", filename)) {
    return("recon-all")
  } else if (grepl("merged_synthseg_volumes_.*\\.csv", filename)) {
    return("clinical")
  } else {
    return(NA)
  }
}

# Function to extract hippocampal volumes and add Source + Group
extract_hippocampus <- function(file_path, source_label) {
  df_hippo <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Find columns
  left_col <- grep("Left.*Hippocampus", names(df_hippo), ignore.case = TRUE, value = TRUE)
  right_col <- grep("Right.*Hippocampus", names(df_hippo), ignore.case = TRUE, value = TRUE)
  subject_col <- grep("subject|id", names(df_hippo), ignore.case = TRUE, value = TRUE)
  
  # Fallbacks
  if (length(subject_col) == 0) subject_col <- names(df_hippo)[1]
  if (length(left_col) == 0 || length(right_col) == 0) {
    message("Could not find hippocampal columns in ", file_path)
    return(NULL)
  }
  
  # Subset and rename
  df_subset <- df_hippo[, c(subject_col[1], left_col[1], right_col[1])]
  colnames(df_subset) <- c("subject", "Left.Hippocampus", "Right.Hippocampus")
  df_subset$Source <- source_label
  
  # Add Group column based on subject name
  df_subset$Group <- ifelse(grepl("_AD_", df_subset$subject, ignore.case = TRUE), "AD",
                            ifelse(grepl("_HC_", df_subset$subject, ignore.case = TRUE), "HC", NA))
  # Sex from subject name
  df_subset$Sex <- ifelse(grepl("_M_", df_subset$subject), "Male",
                          ifelse(grepl("_F_", df_subset$subject), "Female", NA))
  
  
  return(df_subset)
}

# Process all files
hippo_data <- lapply(files, function(f) {
  file_path <- file.path(base_dir, f)
  source_label <- get_source_label(f)
  extract_hippocampus(file_path, source_label)
}) %>%
  bind_rows()

# View result
print(hippo_data)

# Optional: write to CSV
write.csv(hippo_data, file = "/home/rafaelp/SUBJECTS_miriad/hippocampal_volumes_combined.csv", row.names = FALSE)
# write.csv(hippo_data, "combined_hippocampal_volumes_with_source_and_group.csv", row.names = FALSE)



# Shapiro males-females
# Filter for recon-all only
library(dplyr)

# Step 1: Filter recon-all source
recon_data <- hippo_data %>% filter(Source == "recon-all")

# Step 2: Function to run tests and store results
run_tests_to_csv <- function(data, sex) {
  # Subset values
  ad_vals <- data %>% filter(Group == "AD", Sex == sex) %>% pull(Right.Hippocampus)
  hc_vals <- data %>% filter(Group == "HC", Sex == sex) %>% pull(Right.Hippocampus)
  
  # Run Shapiro tests
  shapiro_ad <- shapiro.test(ad_vals)
  shapiro_hc <- shapiro.test(hc_vals)
  
  # Decide test
  is_normal <- shapiro_ad$p.value > 0.05 && shapiro_hc$p.value > 0.05
  if (is_normal) {
    comparison_test <- t.test(ad_vals, hc_vals)
    test_type <- "t-test"
  } else {
    comparison_test <- wilcox.test(ad_vals, hc_vals)
    test_type <- "Wilcoxon"
  }
  
  # Build result dataframe
  results <- data.frame(
    Sex = sex,
    Group1 = "AD",
    Group2 = "HC",
    Shapiro_p_AD = round(shapiro_ad$p.value, 5),
    Shapiro_p_HC = round(shapiro_hc$p.value, 5),
    Test = test_type,
    p_value = round(comparison_test$p.value, 5),
    Mean_AD = round(mean(ad_vals), 2),
    Mean_HC = round(mean(hc_vals), 2)
  )
  
  return(results)
}

# Step 3: Run for Male and Female, combine results
results_male <- run_tests_to_csv(recon_data, "Male")
results_female <- run_tests_to_csv(recon_data, "Female")
final_results <- bind_rows(results_male, results_female)

# Step 4: Save to CSV
write.csv(final_results, "/home/rafaelp/SUBJECTS_miriad/right_hippocampus_stats_by_group_and_sex.csv", row.names = FALSE)

# View output
print(final_results)


# Clinical
clinical_data <- hippo_data %>% filter(Source == "clinical")
# Step 2: Function to run tests and store results
run_tests_to_csv <- function(data, sex) {
  # Subset values
  ad_vals_clin <- data %>% filter(Group == "AD", Sex == sex) %>% pull(Right.Hippocampus)
  hc_vals_clin <- data %>% filter(Group == "HC", Sex == sex) %>% pull(Right.Hippocampus)
  
  # Run Shapiro tests
  shapiro_ad_clin <- shapiro.test(ad_vals_clin)
  shapiro_hc_clin <- shapiro.test(hc_vals_clin)
  
  # Decide test
  is_normal <- shapiro_ad_clin$p.value > 0.05 && shapiro_hc_clin$p.value > 0.05
  if (is_normal) {
    comparison_test <- t.test(ad_vals_clin, hc_vals_clin)
    test_type <- "t-test"
  } else {
    comparison_test <- wilcox.test(ad_vals_clin, hc_vals_clin)
    test_type <- "Wilcoxon"
  }
  
  # Build result dataframe
  results_clin <- data.frame(
    Sex = sex,
    Group1 = "AD",
    Group2 = "HC",
    Shapiro_p_AD = round(shapiro_ad_clin$p.value, 5),
    Shapiro_p_HC = round(shapiro_hc_clin$p.value, 5),
    Test = test_type,
    p_value = round(comparison_test$p.value, 5),
    Mean_AD = round(mean(ad_vals_clin), 2),
    Mean_HC = round(mean(hc_vals_clin), 2)
  )
  
  return(results_clin)
}

# Step 3: Run for Male and Female, combine results
results_male_clin <- run_tests_to_csv(clinical_data, "Male")
results_female_clin <- run_tests_to_csv(clinical_data, "Female")
final_results_clin <- bind_rows(results_male_clin, results_female_clin)

# Step 4: Save to CSV
write.csv(final_results_clin, "/home/rafaelp/SUBJECTS_miriad/right_hippocampus_stats_by_group_and_sex.csv", row.names = FALSE)

# View output
print(final_results_clin)




# plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)


# Load stats files
recon_stats <- read.csv("/home/rafaelp/SUBJECTS_miriad/males_females_tests/recon_hippocampus_stats_by_group_and_sex.csv")
clinical_stats <- read.csv("/home/rafaelp/SUBJECTS_miriad/males_females_tests/clinical_hippocampus_stats_by_group_and_sex.csv")

# Add pipeline column
recon_stats$Pipeline <- "recon-all"
clinical_stats$Pipeline <- "clinical"
all_stats <- bind_rows(recon_stats, clinical_stats)

# Load main hippocampal data (already long format)
# Assuming hippo_long is already defined in your environment

hippo_long <- hippo_long %>%
  mutate(
    Diagnosis = factor(Group, levels = c("AD", "HC")),
    Pipeline = factor(Source, levels = c("clinical", "recon-all")),
    Side = factor(Side, levels = c("Left", "Right")),
    Sex = factor(Sex)
  )

# Join p-values into the main data
hippo_long_with_p <- hippo_long %>%
  left_join(all_stats %>%
              select(Sex, Side, Pipeline, p_value),
            by = c("Sex", "Side", "Pipeline")) %>%
  mutate(p_label = ifelse(p_value < 1e-4, "p < 0.0001", paste0("p = ", signif(p_value, 3))))

# Create combined facet label
hippo_long_with_p <- hippo_long_with_p %>%
  mutate(FacetLabel = paste(Sex, Side, sep = " - "))

# Get position for p-value label (top of each facet)
p_labels_df <- hippo_long_with_p %>%
  group_by(FacetLabel, Pipeline) %>%
  summarise(p_label = first(p_label), .groups = "drop") %>%
  mutate(y_pos = max(hippo_long_with_p$Volume) * 1.05)

# Plot
ggplot(hippo_long_with_p, aes(x = Pipeline, y = Volume, fill = Diagnosis)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85, position = position_dodge(width = 0.75)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.25, size = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2,
               position = position_dodge(width = 0.75), color = "black") +
  geom_text(data = p_labels_df,
            aes(x = Pipeline, y = y_pos, label = p_label),
            inherit.aes = FALSE,
            size = 4.5) +
  scale_fill_manual(values = c("AD" = "salmon", "HC" = "skyblue")) +
  facet_wrap(~FacetLabel, scales = "fixed") +
  labs(
    title = "Hippocampal Volumes by Sex, Side, and Pipeline",
    x = "Pipeline",
    y = "Volume (mm³)",
    fill = "Diagnosis"
  ) +
  theme_minimal(base_size = 14)
