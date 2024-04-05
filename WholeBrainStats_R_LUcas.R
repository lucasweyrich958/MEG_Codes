
#PURPOSE:           Read NIIs and perform basic operations
#OUTPUT:            1) Extract peaks from specified Talirach location (working on t test?)

#FUTURE GOALS
#                   2) Create an average NII from feeded files 
#                   3) Perform statsitical operations
#
#
#
#

#AUTHOR:            Camilo A Castelblanco & Lucas Weyrich
#VERSION HISTORY:   05/17/2022  v1: First working version



#Whole brain stats 
library(oro.nifti) #Necessary to read NIIs

#Working directory of both groups
userswd = ('E:/SCAN_VisAttend/derivative/SourceSpace/ERS_Theta/NII/Users')
nonuserswd = ('E:/SCAN_VisAttend/derivative/SourceSpace/ERS_Theta/NII/NonUsers')
voxel_coordinates = c(34, -72.5, 9.5) #For now predetermined

#######################Currently working on receiving user input##############
# taking multiple inputs using braces
{
  X_Coor_str = readline("Enter 1st number : ");
  Y_Coor_str = readline("Enter 2nd number : ");
  Z_Coor_str = readline("Enter 3rd number : ");
}

# converting each value
X_Coor = as.integer(X_Coor_str);
Y_coor = as.integer(Y_Coor_str);
Z_Coor = as.integer(Z_Coor_str);




#################################################################

#Getting "dimensionality"
#Convert voxel coordinates into talairach space (based on voxel size)

#Effectively reading and loading one file

#effectively extracting the starting X, Y, Z (for now, I don't understand what this thing does)
Xstart = img@srow_x[4]
Ystart = img@srow_y[4]
Zstart = img@srow_z[4]

#Effectively substracting the coordinates (user given) from the extracted x start
voxel_coordinates[1] = voxel_coordinates[1]- Xstart
voxel_coordinates[2] = voxel_coordinates[2]- Ystart
voxel_coordinates[3] = voxel_coordinates[3]- Zstart

#Simply replicating the Matlab code!
voxel_coordinates[1] = round(voxel_coordinates[1]/img@pixdim[2]+1, digits = 1)-17
voxel_coordinates[2] = round(voxel_coordinates[2]/img@pixdim[3]+1, digits = 1)
voxel_coordinates[3] = round(voxel_coordinates[3]/img@pixdim[4]+1, digits = 1)

#############USERS##################
#extracting the names in every folder
setwd(userswd)
#Optimized by  list.files(path = 'E:/SCAN_VisAttend/derivative/SourceSpace/ERS_Theta/NII/Users', pattern = "*.nii")
users_NII <- list.files(pattern = "*.nii") #extracting the users NII
users_NII_data = lapply(users_NII, readNIfTI) #does not work :(
number_of_users = length(users_NII)
value_users = c()
for (i in 1:number_of_users)
{
  file = readNIfTI(users_NII[i])
  value_users[i] = file@.Data[voxel_coordinates[1],voxel_coordinates[2],voxel_coordinates[3]] 
  rm (file)
}
rm(i)
################NonUsers##########################################
setwd(nonuserswd)
nonusers_NII <- list.files(pattern = "*.nii") #extracting the users NII
nonusers_NII_data = lapply(nonusers_NII, readNIfTI)
number_of_nonusers = length(nonusers_NII)

value_nonusers = c()
for (i in 1:number_of_nonusers)
{
  file = readNIfTI(nonusers_NII[i])
  value_nonusers[i] = file@.Data[voxel_coordinates[1],voxel_coordinates[2],voxel_coordinates[3]] 
  rm (file)
}

#iteration through files


#COMMENTS
#summary(img) seems to indicate the descriptive stats for the image in question!
#It effectively gives you the highest, lowest, etc for pseudo-t
img@.Data[voxel_coordinates[1],voxel_coordinates[2],voxel_coordinates[3]]

#----Whole-Brain Average----
setwd('D:/DevMind MSIT/Control - Simon/Stats/Beamforming/Alpha_8-14_350-650/dat2nii/Substractions')
img <- readNIfTI("M68202383_devmind_msit_simon_1_raw_tsss_mc_CS_Alpha_350-650_ms_8-14_Hz_subtracted")
img2 = readNIfTI('M68203793_devmind_msit_simon_1_raw_tsss_mc_CS_Alpha_350-650_ms_8-14_Hz_subtracted')
hdr = nifti_header('M68203793_devmind_msit_simon_1_raw_tsss_mc_CS_Alpha_350-650_ms_8-14_Hz_subtracted')

Xstart = img@srow_x[4]
Ystart = img@srow_y[4]
Zstart = img@srow_z[4]

A <- img@.Data
B <- img2@.Data

X <- list(A, B)
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))

mean_matrix = apply(Y, c(1, 2, 3), mean, na.rm = TRUE)

pixdim = pixdim(img2)
new_nii = nifti(img = mean_matrix, datatype = 64, pixdim = pixdim)
new_nii = as.nifti(from = mean_matrix, value = hdr, verbose = FALSE)
new_nii@reoriented = T


writeNIfTI(nim = new_nii,filename = 'new_nii',gzipped = F)

#Effectively substracting the coordinates (user given) from the extracted x start
voxel_coordinates[1] = voxel_coordinates[1]- Xstart
voxel_coordinates[2] = voxel_coordinates[2]- Ystart
voxel_coordinates[3] = voxel_coordinates[3]- Zstart

#Simply replicating the Matlab code!
voxel_coordinates[1] = round(voxel_coordinates[1]/img@pixdim[2]+1, digits = 1)-17
voxel_coordinates[2] = round(voxel_coordinates[2]/img@pixdim[3]+1, digits = 1)
voxel_coordinates[3] = round(voxel_coordinates[3]/img@pixdim[4]+1, digits = 1)

img@.Data[voxel_coordinates[1],voxel_coordinates[2],voxel_coordinates[3]]
img2@.Data[voxel_coordinates[1],voxel_coordinates[2],voxel_coordinates[3]]
new_nii@.Data[voxel_coordinates[1],voxel_coordinates[2],voxel_coordinates[3]]
