# Load data
hc_path <- "/home/rafaelp/SUBJECTS_miriad/miriad_HC/recon-all-clinical/merged_synthseg_volumes_HC.csv"
HC <- read.csv(hc_path)
print(HC)

AD_path <- "/home/rafaelp/SUBJECTS_miriad/miriad_AD/recon-all-clinical/merged_synthseg_volumes_AD.csv"
AD <- read.csv(AD_path)
print(AD)

library(reshape2)
HC$Group <- "HC"
AD$Group <- "AD"

combined <- rbind(HC, AD)
print(combined)

# Reshape data to long format for hippocampus structures
#Melt
long_df <- melt(
  combined,
  id.vars = "Group",
  measure.vars = c("left.hippocampus", "right.hippocampus"),
  variable.name = "Structure",
  value.name = "Volume"
)

# BoxPlot
boxplot(Volume ~ Structure + Group, data = long_df,
        col = c("lightblue", "salmon"),
        ylab = "Volume (mm³)", lex.order = T,
        main = "Hippocampus Volume in HC vs AD",
        las = 1)


# THEN COMBINE RECON-ALL vs RECON-ALL-CLINICAL
library(reshape2)
library(ggplot2)

# Load files
HC_recon <- read.csv("/home/rafaelp/SUBJECTS_miriad/miriad_HC/recon-all/miriad_HC_all_structures_combined.csv")
HC_clin <- read.csv("/home/rafaelp/SUBJECTS_miriad/miriad_HC/recon-all-clinical/merged_synthseg_volumes_HC.csv")

AD_recon <- read.csv("/home/rafaelp/SUBJECTS_miriad/miriad_AD/recon-all/miriad_AD_all_structures_combined.csv")
AD_clin <- read.csv("/home/rafaelp/SUBJECTS_miriad/miriad_AD/recon-all-clinical/merged_synthseg_volumes_AD.csv")

# Add group and source columns
HC_recon$Group <- "HC"
HC_recon$Source <- "recon-all"

HC_clin$Group <- "HC"
HC_clin$Source <- "clinical"

AD_recon$Group <- "AD"
AD_recon$Source <- "recon-all"

AD_clin$Group <- "AD"
AD_clin$Source <- "clinical"

# Select desired columns from each data frame
HC_recon_sub <- HC_recon[, c("Left.Hippocampus", "Right.Hippocampus")]
HC_recon_sub$Group <- "HC"
HC_recon_sub$Source <- "recon-all"

HC_clin_sub <- HC_clin[, c("Left.Hippocampus", "Right.Hippocampus")]
HC_clin_sub$Group <- "HC"
HC_clin_sub$Source <- "clinical"

AD_recon_sub <- AD_recon[, c("Left.Hippocampus", "Right.Hippocampus")]
AD_recon_sub$Group <- "AD"
AD_recon_sub$Source <- "recon-all"

AD_clin_sub <- AD_clin[, c("Left.Hippocampus", "Right.Hippocampus")]
AD_clin_sub$Group <- "AD"
AD_clin_sub$Source <- "clinical"

# Combine only those columns
combined_Lh_Rh <- rbind(HC_recon_sub, HC_clin_sub, AD_recon_sub, AD_clin_sub)

# Combine all
combined <- rbind(HC_recon, HC_clin, AD_recon, AD_clin)

# Reshape to long format for hippocampus volumes
long_df <- melt(
  combined,
  id.vars = c("Group", "Source"),
  measure.vars = c("Left.Hippocampus", "Right.Hippocampus"),
  variable.name = "Structure",
  value.name = "Volume"
)


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
