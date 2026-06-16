library(tidyverse)
library(parallel)
        
        
# files<-dir('/home/rafaelp/SUBJECTS_miriad/miriad_193_1_MR_1/SCANS/SCAN1/',pattern = '.+\\d$',full.names = T)
# files <- dir("/home/rafaelp/SUBJECTS_miriad/miriad_AD_nonanalysed/miriad_AD_longitudinal/miriad_188_239/", pattern = '\\.nii(\\.gz)?$', full.names = TRUE, recursive = TRUE)
#files<-dir('/home/rafaelp/SUBJECTS_miriad/',pattern = '.+nii$',full.names = T,recursive = T)
# files <- dir('/home/rafaelp/SUBJECTS_cing/input', pattern = '\\.nii\\.gz$', full.names = TRUE, recursive = TRUE)

mclapply(c(1:length(files)),function(i){
#miriad 
SID <- str_match(files[i], '.+/(miriad_[^/]+)')[,2]
#cing *.nii.gz
#SID <- str_match(files[i], '.+/(sub-[^/]+)*.nii.gz')[,2]
NII<-files[i]
print(files[i])

command<-paste0('recon-all -s ',SID,' -i ',NII,' -all')
command<-gsub('//','/',command)
print(command)
system(command)

},mc.cores = 23)
