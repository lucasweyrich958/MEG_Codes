#NAME: NIFTI Zauberer
#PURPOSE:           Read NIIs and performs the following operations:
#Read NII and 2 predictor variables (XSLX); perform regression and write out t and F stat maps

#AUTHORS:            C.S.L. (i.e., C.S. Lewis or even better, Camilo, Seth, and Lucas)
#VERSION HISTORY:   05/23/2022  v1: First working version

library(oro.nifti)
library(openxlsx)
library(tidyverse)
library(readr)
library(emmeans)
library(afex)
library(ggpubr)
library(rstatix)

all_participant_files = choose.files(default = "", caption = "Select NIIs for all participants", multi = TRUE, filters = "*.nii")

predictor_xlsx = choose.files(default = "", caption = "Select the excel with the predictor variables", multi = TRUE, filters = "*.xlsx")
save_path = choose.dir(caption = 'Select the directory to save the statistical output')

#Running lm on one variable
data_predictor <- read.xlsx(predictor_xlsx, colNames = FALSE)
data_predictor1 = data_predictor$X1
data_predictor2 = data_predictor$X2

#Effectively reading and loading one file
img <- readNIfTI(all_participant_files[1])
img_data = img@.Data
hdr = nifti_header(all_participant_files[1])
dimensions = dim(img@.Data) #obtain the dimensions nicely







n_participants = length(all_participant_files)


#Preallocate 4D matrix
full_4D_matrix <- array(c(0), dim = c(dimensions[1], dimensions[2], dimensions[3], n_participants))

#Preallocate stats matrix
t_stat_map_v1 <- array(c(0), dim = c(dimensions[1], dimensions[2], dimensions[3]))
F_stat_map_v1 <- array(c(0), dim = c(dimensions[1], dimensions[2], dimensions[3]))

#Create a 4D matrix with brain data
for (i in 1:n_participants)
{
  
  img_temp <- readNIfTI(all_participant_files[i])
  img_data_temp = img_temp@.Data
  
  full_4D_matrix[ , , ,i] = img_data_temp
  
  rm(img_temp)
  
}


#Preallocate temp vector
pseudo_Ts_temp = replicate(n_participants,0)

stats_counter = 1


pb <- winProgressBar(title = "Progress bar", min = 0, max = 1, initial = 0)

for (X_current in 1: dimensions[1]) 
{
  for (Y_current in 1: dimensions[2])
  {
    for (Z_current in 1: dimensions[3])
    {
      info <- sprintf("%d%% done", round(X_current/dimensions[1]))
      setWinProgressBar(pb, X_current/dimensions[1], label=info)
      
      pseudo_Ts_temp = full_4D_matrix[X_current,Y_current,Z_current,]
      
      #To avoid NANs and make more effecient!
      if (sum(pseudo_Ts_temp) == 0) { 
        
        #dont do anything, leave the stats values at 0!
        
      } else  {
        model.brain_model = aov(pseudo_Ts_temp ~ data_predictor1) #the number indicates the column!
        #model.brain_model = lm(empty_array[X_current, Y_current, Z_current, current_subject] ~ ages[current_subject])Syntax does not allow to have brackets inside
        F_value_temp_v1 = summary(model.brain_model)[[1]][1,4]
        
        F_stat_map_v1[X_current, Y_current, Z_current] = F_value_temp_v1
        
        #only once, pull the degrees of freedom
        if (stats_counter ==  1) {
          df_1 = as.numeric(summary(model.brain_model)$fstatistic[2])
          
          stats_counter = stats_counter+1
        }
        
        rm(model.brain_model) 
      }
    }
  }
}

close(pb)


t_stat_output_name_v1 = paste('lm_regression_t_values_v1_interaction', '_df_', df_1,sep = '')

#Write out the t stats brain
pixdim = pixdim(img)
img.nifti = as.nifti(from = t_stat_map_v1, value = hdr,  verbose = FALSE) 
img.nifti@reoriented = T
img.nifti@datatype = 64
img.nifti@bitpix = 64
fname_v1 = file.path(path = save_path, t_stat_output_name_v1)
writeNIfTI(nim =img.nifti, fname_v1, gzipped = FALSE) #gzipped = false to prevent the .nz in the file

F_stat_output_name_v1 = paste('lm_regression_F_values_v1_interaction', '_df_', df_1, '_',sep = '')

#Write out the t stats brain
pixdim = pixdim(img)
img.nifti = as.nifti(from = F_stat_map_v1, value = hdr,  verbose = FALSE) 
img.nifti@reoriented = T
img.nifti@datatype = 64
img.nifti@bitpix = 64
fname_v1 = file.path(path = save_path, F_stat_output_name_v1)
writeNIfTI(nim =img.nifti, fname_v1, gzipped = FALSE) #gzipped = false to prevent the .nz in the file
