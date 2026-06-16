# Load the libary
install.packages("dplyr")
install.packages("readr")  
library(freesurferformats)
library(tidyr)
library(dplyr)
library(readr)

# Compare parcellation stats from recon all and recon all clinical
# Set directory with the data
# SIMPLE GRAPH
data_dir <- "/home/rafaelp/SUBJECTS_allen/allen_parcellation_results"

# Define subject id
subject_id <- "0006"

# Build file paths
file_recon_all <- file.path(data_dir, paste0("sub-", subject_id, "_rh_aparc.csv"))
file_clinical <- file.path(data_dir, paste0("sub-", subject_id, "-clin_rh_aparc.csv"))

# LOAD data
recon_all_df <- read.csv(file_recon_all, stringsAsFactors = FALSE)
clinical_df <- read.csv(file_clinical, stringsAsFactors = FALSE)

# Inspect columns (assuming first column is region names)
print(colnames(recon_all_df))
print(head(recon_all_df))

# Merge by region name (assuming column "Region" or similar)
merged_df <- merge(recon_all_df, clinical_df, by = "StructName", suffixes = c("_recon", "_clin"))

# Compute differences in a measure, e.g., Thickness or Volume
merged_df$SurfArea<- merged_df$SurfArea_clin - merged_df$SurfArea_recon

# Summary stats
summary(merged_df$SurfArea)

# Scatter plot to visualize similarity
library(ggplot2)
ggplot(merged_df, aes(x = SurfArea_recon, y = SurfArea_clin)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste("Surface area: Recon-all vs Clinical for subject-0006"),
       x = "Recon-all Surface area",
       y = "Clinical Surface area") +
  theme_minimal()


# BOXPLOT (already printed data and values from before)
# Select only relevant columns: StructName, SurfArea , ThickAvg
data_dir <- "/home/rafaelp/SUBJECTS_allen/allen_parcellation_results"
subject_ids <- c("0006", "1012", "1015", "1016", "2001", "2002", "2003")

# select my regions
selected_regions <- c("precuneus", "entorhinal", "parahippocampal", "fusiform", "insula", "temporal pole")

# Empty list to store data
all_data <- list()

# Loop through subjects
for (subject_id in subject_ids) {
  file_recon <- file.path(data_dir, paste0("sub-", subject_id, "_lh_aparc.csv"))
  file_clin <- file.path(data_dir, paste0("sub-", subject_id, "-clin_lh_aparc.csv"))
  
  if (file.exists(file_recon) && file.exists(file_clin)) {
    df_recon <- read_csv(file_recon, show_col_types = FALSE) %>%
      filter(StructName %in% selected_regions) %>%
      mutate(Pipeline = "Recon-all", Subject = subject_id)
    
    df_clin <- read_csv(file_clin, show_col_types = FALSE) %>%
      filter(StructName %in% selected_regions) %>%
      mutate(Pipeline = "Clinical", Subject = subject_id)
    
    all_data[[subject_id]] <- bind_rows(df_recon, df_clin)
  } else {
    warning(paste("Missing files for subject", subject_id))
  }
}

# Combine datasets
combined_df <- bind_rows(all_data)

# BoxPlot for Surface area or thickness average
ggplot(combined_df, aes(x = StructName, y = ThickAvg, fill = Pipeline)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(
    title = "Surface Area Comparison Across Allen Subjects (Left Hemisphere)",
    y = "Thickness Average (mm3)",
    x = "Brain Region",
    fill = "Pipeline"
  ) +
  scale_fill_manual(values = c("Recon-all" = "#1f78b4", "Clinical" = "#e31a1c")) +
  theme_minimal()