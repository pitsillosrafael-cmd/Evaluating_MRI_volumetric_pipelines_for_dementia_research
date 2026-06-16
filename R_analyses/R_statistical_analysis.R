library(irr)
# Check shapiro for AD and HC for each group
# Split by structure and group
shapiro_left_ad_recon  <- data_ALL_parcellations_combined %>% filter(Region == "entorhinal", Diagnosis == "AD", Pipeline == "recon-all") %>% pull(ThickAvg)
shapiro_left_ad_clin  <- data_ALL_parcellations_combined %>% filter(Region == "entorhinal", Diagnosis == "AD", Pipeline == "clinical") %>% pull(ThickAvg)

shapiro_left_hc_recon  <- data_ALL_parcellations_combined %>% filter(Region == "entorhinal", Diagnosis == "HC", Pipeline == "recon-all") %>% pull(ThickAvg)
shapiro_left_hc_clin  <- data_ALL_parcellations_combined %>% filter(Region == "entorhinal", Diagnosis == "HC", Pipeline == "clinical") %>% pull(ThickAvg)

shapiro_right_ad_recon <- long_df %>% filter(Structure == "Right.Hippocampus", Group == "AD", Source == "recon-all") %>% pull(Volume)
shapiro_right_ad_clin <- long_df %>% filter(Structure == "Right.Hippocampus", Group == "AD", Source == "clinical") %>% pull(Volume)

shapiro_right_hc_recon <- long_df %>% filter(Structure == "Right.Hippocampus", Group == "HC", Source == "recon-all") %>% pull(Volume)
shapiro_right_hc_clin <- long_df %>% filter(Structure == "Right.Hippocampus", Group == "HC", Source == "clinical") %>% pull(Volume)

shapiro_left_ad_recon <- long_df %>% filter(Structure == "Left.Hippocampus", Group == "AD", Source == "recon-all") %>% pull(Volume)
shapiro_left_ad_clin <- long_df %>% filter(Structure == "Left.Hippocampus", Group == "AD", Source == "clinical") %>% pull(Volume)

shapiro_left_hc_recon <- long_df %>% filter(Structure == "Left.Hippocampus", Group == "HC", Source == "recon-all") %>% pull(Volume)
shapiro_left_hc_clin <- long_df %>% filter(Structure == "Left.Hippocampus", Group == "HC", Source == "clinical") %>% pull(Volume)
# shapiro
#AD left --> recon_all
shapiro.test(shapiro_right_ad_recon)
#AD left--> clinical
shapiro.test(shapiro_right_hc_clin)
#HC left --> recon_all
shapiro.test(shapiro_left_hc_recon)
#HC left--> clinical
shapiro.test(shapiro_left_hc_clin)
#AD right --> recon_all
shapiro.test(shapiro_right_ad_recon)
#AD right--> clinical
shapiro.test(shapiro_right_ad_clin)
#HC right --> recon_all
shapiro.test(shapiro_right_hc_recon)
#HC right--> clinical
shapiro.test(shapiro_right_hc_clin)

library(dplyr)
library(writexl)

data_list <- list(
  list(Group = "AD", Side = "Right", Structure = "Hippocampus", Pipeline = "recon-all", vec = shapiro_right_ad_recon),
  list(Group = "AD", Side = "Right", Structure = "Hippocampus", Pipeline = "clinical", vec = shapiro_right_ad_clin),
  list(Group = "HC", Side = "Right", Structure = "Hippocampus", Pipeline = "recon-all", vec = shapiro_right_hc_recon),
  list(Group = "HC", Side = "Right", Structure = "Hippocampus", Pipeline = "clinical", vec = shapiro_right_hc_clin),
  list(Group = "AD", Side = "Left", Structure = "Hippocampus", Pipeline = "recon-all", vec = shapiro_left_ad_recon),
  list(Group = "AD", Side = "Left", Structure = "Hippocampus", Pipeline = "clinical", vec = shapiro_left_ad_clin),
  list(Group = "HC", Side = "Left", Structure = "Hippocampus", Pipeline = "recon-all", vec = shapiro_left_hc_recon),
  list(Group = "HC", Side = "Left", Structure = "Hippocampus", Pipeline = "clinical", vec = shapiro_left_hc_clin)
)

shapiro_hippocampus <- lapply(data_list, function(x) {
  test <- shapiro.test(x$vec)
  data.frame(
    Group     = x$Group,
    Side      = x$Side,
    Structure = x$Structure,
    Pipeline  = x$Pipeline,
    W_statistic = unname(test$statistic),
    p_value     = test$p.value,
    Normality   = ifelse(test$p.value < 0.05, "Non-Normal", "Normal")
  )
}) %>% bind_rows()

write.csv(shapiro_hippocampus, file = "shapiro_test_results.csv", row.names = FALSE)

# Check the new dataframe structure
head(long_df)

# Calculate max volume per Structure-Source group for label positioning
label_positions_df <- long_df %>%
  group_by(Structure, Source) %>%
  summarize(max_vol = max(Volume, na.rm = TRUE)) %>%
  mutate(label_y = max_vol * 1.05)

# test p-values comparing HC vs AD *for each Structure-Source combo*
library(ggpubr)
ggplot(long_df, aes(x = interaction(Structure, Source), y = Volume, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    title = "Hippocampus Volume: HC vs AD, Recon-All vs Clinical",
    x = "Structure and Source",
    y = "Volume (mm³)"
  ) +
  scale_fill_manual(values = c("HC" = "salmon", "AD" = "lightblue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

library(rstatix)

# t-test
# Clean my data and create vectors for each structure
left_clinical_ad <- long_df %>% filter(Structure == "Left.Hippocampus", Source == "clinical", Group == "AD") %>% pull(Volume)
left_clinical_hc <- long_df %>% filter(Structure == "Left.Hippocampus", Source == "clinical", Group == "HC") %>% pull(Volume)

left_recon_ad <- long_df %>% filter(Structure == "Left.Hippocampus", Source == "recon-all", Group == "AD") %>% pull(Volume)
left_recon_hc <- long_df %>% filter(Structure == "Left.Hippocampus", Source == "recon-all", Group == "HC") %>% pull(Volume)

right_clinical_ad <- long_df %>% filter(Structure == "Right.Hippocampus", Source == "clinical", Group == "AD") %>% pull(Volume)
right_clinical_hc <- long_df %>% filter(Structure == "Right.Hippocampus", Source == "clinical", Group == "HC") %>% pull(Volume)

right_recon_ad <- long_df %>% filter(Structure == "Right.Hippocampus", Source == "recon-all", Group == "AD") %>% pull(Volume)
right_recon_hc <- long_df %>% filter(Structure == "Right.Hippocampus", Source == "recon-all", Group == "HC") %>% pull(Volume)

# t-tests if distribution is canonical
# Left Hippocampus - clinical
t.test(left_clinical_ad, left_clinical_hc, var.equal = TRUE)

# Left Hippocampus - recon-all
t.test(left_recon_ad, left_recon_hc, var.equal = TRUE)
# Right Hippocampus - clinical
t.test(right_clinical_ad, right_clinical_hc, var.equal = TRUE)

# Right Hippocampus - recon-all
t.test(right_recon_ad, right_recon_hc, var.equal = TRUE)

# Wilcoxon signed-rank test 
wilcox.test(before, after, paired=TRUE)

# save them
# Create a data frame of results
t_test_summary <- data.frame(
  Structure = c("Left.Hippocampus", "Right.Hippocampus", "Right.Hippocampus"),
  Source = c("recon-all", "clinical", "recon-all"),
  Mean_AD = c(2880.987, 3095.321, 2947.02),
  Mean_HC = c(3681.041, 3736.709, 3852.95),
  Mean_Difference = c( -800.054, -641.388, -905.93),
  CI_Lower = c(-1038.47, -856.9994, -1160.8607),
  CI_Upper = c(-561.638, -425.7769, -650.9993),
  t_value = c(-6.7018, -5.941, -7.0971),
  p_value = c(5.816e-09, 1.223e-07, 1.168e-09),
  Significance = c("****", "****", "****")
)

# Save to CSV
write.csv(t_test_summary, "t_test_hippocampus_results.csv", row.names = FALSE)

cat("✅ Results saved to: t_test_hippocampus_results.csv\n")

# incorporate them into my graph
pvals_df <- data.frame(
  group1 = "HC",
  group2 = "AD",
  Structure_Source = c(
    "Left.Hippocampus.clinical",
    "Left.Hippocampus.recon-all",
    "Right.Hippocampus.clinical",
    "Right.Hippocampus.recon-all"
  ),
  p = c(4.27e-06, 5.816e-09, 1.223e-07, 1.168e-09),
  y.position = c(4500, 4800, 5100, 5400),  # adjust as needed based on your volume ranges
  label = c("****", "****", "****", "****")
)

long_df$Structure_Source <- interaction(long_df$Structure, long_df$Source)



# For multiple areas of interest
# Define regions of interest
regions <- c("fusiform", "insula", "precuneus", "parahippocampal")

# Loop through each region and assign variables
for (region in regions) {
  # LH AD
  assign(paste0("shapiro_lh_ad_recon_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "lh", Diagnosis == "AD", Pipeline == "recon-all") %>%
           pull(ThickAvg))
  
  assign(paste0("shapiro_lh_ad_clin_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "lh", Diagnosis == "AD", Pipeline == "clinical") %>%
           pull(ThickAvg))
  
  # LH HC
  assign(paste0("shapiro_lh_hc_recon_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "lh", Diagnosis == "HC", Pipeline == "recon-all") %>%
           pull(ThickAvg))
  
  assign(paste0("shapiro_lh_hc_clin_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "lh", Diagnosis == "HC", Pipeline == "clinical") %>%
           pull(ThickAvg))
  
  # RH AD
  assign(paste0("shapiro_rh_ad_recon_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "rh", Diagnosis == "AD", Pipeline == "recon-all") %>%
           pull(ThickAvg))
  
  assign(paste0("shapiro_rh_ad_clin_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "rh", Diagnosis == "AD", Pipeline == "clinical") %>%
           pull(ThickAvg))
  
  # RH HC
  assign(paste0("shapiro_rh_hc_recon_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "rh", Diagnosis == "HC", Pipeline == "recon-all") %>%
           pull(ThickAvg))
  
  assign(paste0("shapiro_rh_hc_clin_", region), data_ALL_parcellations_combined %>%
           filter(Region == region, Hemisphere == "rh", Diagnosis == "HC", Pipeline == "clinical") %>%
           pull(ThickAvg))
}





# Function to compute ICC for one group and one hemisphere
# FIRST MATCH THE CLINICAL WITH RECON ALL SUBJECTS
debug_ad_left <- combined_Lh_Rh %>%
  filter(Group == "AD") %>%
  group_by(Source) %>%
  mutate(subject_index = row_number()) %>%
  ungroup() %>%
  select(subject_index, Source, volume = Left.Hippocampus) %>%
  pivot_wider(names_from = Source, values_from = volume)

print(debug_ad_left)

debug_ad_right <- combined_Lh_Rh %>%
  filter(Group == "AD") %>%
  group_by(Source) %>%
  mutate(subject_index = row_number()) %>%
  ungroup() %>%
  select(subject_index, Source, volume = Right.Hippocampus) %>%
  pivot_wider(names_from = Source, values_from = volume)

print(debug_ad_right)


debug_hc_left <- combined_Lh_Rh %>%
  filter(Group == "HC") %>%
  group_by(Source) %>%
  mutate(subject_index = row_number()) %>%
  ungroup() %>%
  select(subject_index, Source, volume = Left.Hippocampus) %>%
  pivot_wider(names_from = Source, values_from = volume)

print(debug_hc_left)

debug_hc_right <- combined_Lh_Rh %>%
  filter(Group == "HC") %>%
  group_by(Source) %>%
  mutate(subject_index = row_number()) %>%
  ungroup() %>%
  select(subject_index, Source, volume = Right.Hippocampus) %>%
  pivot_wider(names_from = Source, values_from = volume)

print(debug_hc_right)

# ICC for AD left hippocampus using the 2 pipelines
icc_ad_left <- debug_ad_left %>%
  filter(!is.na(`recon-all`) & !is.na(clinical)) %>%
  select(`recon-all`, clinical) %>%
  icc(model = "twoway", type = "agreement", unit = "single")

print(icc_ad_left)

# ICC for AD right hippocampus using the 2 pipelines
icc_ad_right <- debug_ad_right %>%
  filter(!is.na(`recon-all`) & !is.na(clinical)) %>%
  select(`recon-all`, clinical) %>%
  icc(model = "twoway", type = "agreement", unit = "single")

# ICC for HC left hippocampus using the 2 pipelines
icc_hc_left <- debug_hc_left %>%
  filter(!is.na(`recon-all`) & !is.na(clinical)) %>%
  select(`recon-all`, clinical) %>%
  icc(model = "twoway", type = "agreement", unit = "single")

# ICC for HC right hippocampus using the 2 pipelines
icc_hc_right <- debug_hc_right %>%
  filter(!is.na(`recon-all`) & !is.na(clinical)) %>%
  select(`recon-all`, clinical) %>%
  icc(model = "twoway", type = "agreement", unit = "single")

# In one table all
icc_results <- list(
  AD_Left  = icc_ad_left,
  AD_Right = icc_ad_right,
  HC_Left  = icc_hc_left,
  HC_Right = icc_hc_right
)

# View a summary
lapply(icc_results, function(x) x$value)

# Save ICC output to a text file
sink("icc_results.txt")
print(icc_results)
sink()  # Stop capturing output

library(tibble)

# Extract ICC values from your list (assuming it's named icc_results)
icc_summary <- tibble(
  Group = c("AD_Left", "AD_Right", "HC_Left", "HC_Right"),
  ICC = c(icc_results$AD_Left$value, icc_results$AD_Right$value, icc_results$HC_Left$value, icc_results$HC_Right$value)
)

write.csv(icc_summary, "icc_summary.csv", row.names = FALSE)



