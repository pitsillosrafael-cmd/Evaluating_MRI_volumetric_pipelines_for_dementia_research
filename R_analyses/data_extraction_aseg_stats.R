# The read_aseg_stats function reads this corresponding file and creates a list of two different data.framesfile = file.path(fs_subj_dir(), "bert", "label", "lh.cortex.label")
file = file.path(fs_subj_dir(), "sub-0006-clin", "stats", "aseg.stats")
out = read_aseg_stats(file)
names(out)

# measures of gross anatomical structures (e.g. gray matter)
head(out$measures[, c("meaning", "value", "units")], n = 5)
#  Alternatively, the structures element corresponds to a set of measures and statistics for a set of fixed anatomical structures.
head(out$structures, n = 5)

write.csv(out$measures[, c("meaning", "value", "units")], file = file.path("allen_measures_results", "sub-0006-clin_measures.csv"), row.names = FALSE)
write.csv(out$structures, file = file.path("allen_structures_results", "sub-0006-clin_structures.csv"), row.names = FALSE)

# LOAD ALL SUBJECTS INTO TWO DATA FRAMES (all_measures & all_structures)
# MEASURES
# Get list of files
measure_files <- list.files("allen_measures_results", full.names = TRUE)
print(measure_files)
subject_ids <- gsub("_measures.csv", "", basename(measure_files))

# Read and combine
measure_df <- do.call(rbind, lapply(seq_along(measure_files), function(i) {
  df <- read.csv(measure_files[i])
  data.frame(subject = subject_ids[i], t(df$value), row.names = NULL)
}))

# Add column names
measure_names <- read.csv(measure_files[1])$meaning
colnames(measure_df)[-1] <- make.names(measure_names)

# Save measures_df
write.csv(measure_df, file = "allen_all_measures_combined.csv", row.names = FALSE)

# STRUCTURES
# Get list of files
structure_files <- list.files("allen_structures_results", full.names = TRUE)
print(structure_files)

structure_df <- do.call(rbind, lapply(seq_along(structure_files), function(i) {
  df <- read.csv(structure_files[i])
  data.frame(subject = subject_ids[i], t(df$Volume_mm3), row.names = NULL)
}))

# Add column names
structure_names <- read.csv(structure_files[1])$StructName
colnames(structure_df)[-1] <- make.names(structure_names)

# Save structures_df
write.csv(structure_df, file = "allen_all_structures_combined.csv", row.names = FALSE)

# To create a new - combined file of structures and measures
combined_df <- merge(measure_df, structure_df, by = "subject")
write.csv(combined_df, file = "allen_measures_and_satructures_combined.csv", row.names = FALSE)

# Visualize the data
library(tidyverse)
# First present the data as long format
structure_long <- structure_df %>%
  pivot_longer(
    cols = -subject,
    names_to = "structures",
    values_to = "volume"
  )

# Bar plot across subjects
ggplot(structure_long, aes(x = structures, y = volume, fill = subject)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Structure volume per subject",
    x = "Brain Structure",
    y = "Volume (mm3)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Line plot
ggplot(structure_long, aes(x = structures, y = volume, group = subject, color = subject)) +
  geom_line() + geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Here we will read a label file for the left hemisphere cortex:
#file = file.path(fs_subj_dir(), "sub-0006", "label", "lh.cortex.label")
#out = read_fs_label(file)
#head(out)