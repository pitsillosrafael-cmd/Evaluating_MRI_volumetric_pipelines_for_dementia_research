# Load the libary
library(freesurferformats)

# Load the lh and rh parcellation stats
file_lh <- file.path(fs_subj_dir(), "sub-0006-clin", "stats", "lh.aparc.stats")
file_rh <- file.path(fs_subj_dir(), "sub-0006-clin", "stats", "rh.aparc.stats")

# Read in skipping comments (lines starting with #)
aparc_lh <- read.table(file_lh, comment.char = "#", header = TRUE)
aparc_rh <- read.table(file_rh, comment.char = "#", header = TRUE)

# View
head(aparc_lh)
head(aparc_rh)

# Create a directory for saving it ONE TIME
# dir.create("allen_parcellation_results", showWarnings = FALSE)

# Change the names of the columns
colnames(aparc_lh) <- c("StructName", "NumVert", "SurfArea", "GrayVol", 
                        "ThickAvg", "ThickStd", "MeanCurv", "GausCurv", 
                        "FoldInd", "CurvInd")
colnames(aparc_rh) <- c("StructName", "NumVert", "SurfArea", "GrayVol", 
                        "ThickAvg", "ThickStd", "MeanCurv", "GausCurv", 
                        "FoldInd", "CurvInd")

# Save it to that directory
write.csv(aparc_lh[, c("StructName", "NumVert", "SurfArea", "GrayVol", "ThickAvg")],
          file = "allen_parcellation_results/sub-0006-clin_lh_aparc.csv",
          row.names = FALSE)
write.csv(aparc_rh[, c("StructName", "NumVert", "SurfArea", "GrayVol", "ThickAvg")],
          file = "allen_parcellation_results/sub-0006-clin_rh_aparc.csv",
          row.names = FALSE)

