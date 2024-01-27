#T1_MRI_Image_Collage
#Create MRI Collages to send out to participants
#(C) Seth Springer & Lucas Weyrich 
#V1: 05/02/2022


#Load Packages (Only needs to be done upon opening the first time each)

install.packages("oro.dicom")
install.packages("oro.nifti")
install.packages("stringr")
install.packages("patchwork")
install.packages("png") 

library(oro.dicom)
library(oro.nifti)
library(stringr)
library(png)
library(patchwork)

setwd('D:/Codes/MRI_temp/')



subjects_already_ran = read.csv(paste(getwd(),'subjects_already_ran.csv',sep = "/"))

subjects_already_ran_URSIs = subjects_already_ran$URSI
subjects_already_ran_dates = subjects_already_ran$Date
subjects_already_ran_study = subjects_already_ran$Study


n_subjects_already_ran_URSIs = length(subjects_already_ran_URSIs)



T1_target_dir = '\\\\boystown.org/btnrh/HumanNeuroscience/coins/boystown/dicom/prisma/twilson'

addImg <- function(
    obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
    x = NULL, # mid x coordinate for image
    y = NULL, # mid y coordinate for image
    width = NULL, # width of image (in x coordinate units)
    interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}


data_dir = data.frame(directories = list.dirs(path = T1_target_dir, full.names = TRUE, recursive = TRUE))


dirs_with_T1s <- data_dir[str_detect(data_dir$directories, "t1w_32ch_mpr_1mm_00"), ]  # Extract matching rows with str_detect


n_dirs_with_T1s = length(dirs_with_T1s)

dirs_with_T1s_URSIs = replicate(n_dirs_with_T1s,NA)

#Create list of URSI
for (i in 1:n_dirs_with_T1s) 
{
  one_subject_dir = dirs_with_T1s[i]
  
  #Get URSI Stuff
  URSI_find = unlist(gregexpr('Study',one_subject_dir)) #It should come before "/Study" in the full file name
  URSI_start = URSI_find-10
  URSI_end = URSI_find-2
  dirs_with_T1s_URSIs[i] = substr(one_subject_dir,URSI_start,URSI_end) #Grab the URSI from the full file name
  
}


#Now you need to see which URSIs should be ran and which need to be skipped

logical_matrix = matrix(data=0,nrow=n_subjects_already_ran_URSIs,ncol=n_dirs_with_T1s) #Preallocate. Rows are num subjects already ran, while columns are num of possible subjects to run

#Find URSIs that have already been ran
for (ii in 1:n_subjects_already_ran_URSIs)
{

  already_ran_overlap = unlist(gregexpr(subjects_already_ran_URSIs[ii],dirs_with_T1s_URSIs)) #returns 1s and -1s, 1s are where there is overlap
  already_ran_overlap[already_ran_overlap==-1] = 0
  
  
  logical_matrix[ii,] = already_ran_overlap
  
}

logical_vector = colSums(logical_matrix) #This creates ONE vector that marks the position of subjects that should be skipped
logical_vector = !(as.logical(logical_vector)) #invert the vector so it marks subjects that should be ran.



dirs_to_run_now = dirs_with_T1s[logical_vector]
URSIs_to_run_now = dirs_with_T1s_URSIs[logical_vector]
n_to_run_now = length(dirs_to_run_now)

#Preallocate date list
date_list = replicate(n_to_run_now,0)
study_list = replicate(n_to_run_now,0)


#Only run the image creating loop if there are subjects that need to be ran...
if (n_to_run_now == 0) {
  
} else {
  
for (iii in 1:n_to_run_now)
{
  
  current_sub_dir = dirs_to_run_now[iii]
  find_current_sub_dir_end_index = unlist(gregexpr('0007',current_sub_dir))+4
  
  find_current_sub_dir_study_start_index = unlist(gregexpr('twilson',current_sub_dir))+8
  find_current_sub_dir_study_end_index   = unlist(gregexpr('Study',current_sub_dir))-12
  
  current_sub_study = substr(current_sub_dir,find_current_sub_dir_study_start_index,find_current_sub_dir_study_end_index)
  
  study_list[iii] = current_sub_study
  
  
  #Get date Stuff
  date_find = unlist(gregexpr('Study',current_sub_dir)) #It should come before "/Study" in the full file name
  date_start = date_find+5
  date_end = date_find+12
  date = substr(current_sub_dir,date_start,date_end) #Grab the URSI from the full file name
  
  date_list[iii] = date
  
  
  
  #Need to convert current_sub_dir to a workable string
  current_sub_dir_char = as.character(current_sub_dir)
 
  

  
  #Read DICOM File and transfer into NIfTI
  dcm <- readDICOM('D:/Codes/MRI_temp/M68137757') #Copy and paste from location // MUST BE MATCHING
  nii <- dicom2nifti(dcm)

  
  sub_save_name = paste(study_list[iii],URSIs_to_run_now[iii],date_list[iii],sep = "_")

  
  full_save_name = paste(getwd(),'Created_Images',sub_save_name,sep = "/")
  full_save_name = paste(full_save_name,'.jpg',sep="")
  picture_title = paste(URSIs_to_run_now[iii])
#BLACK
  #Create and save image
  jpeg('M68137757.jpeg', width = 2600, height = 2000) #Change JPEG file name, can be same as input file
  image(nii, 
        z = c(87:116), #87 116         #Can change slices if needed, but 48 slices is recommended
        plot.type = "single",    
        plane = c("sagittal"),  #Plane can be changed if needed
        zlim = c(10,230)) #Changes contrast; first number is darkness, left is brightness, should not need to be changed... (10,230)
  #text(0.95, 0.85,labels = Subject_ID,cex = 1, col = 'grey')
  rasterImage(logo,xleft = 0.84, ybottom = 0.135, xright = 0.99, ytop = 0.30)
  dev.off()
  
}
}
#WHITE
#Create and save image
jpeg('whitssssssse', width = 1500, height = 1150) #Change JPEG file name, can be same as input file
image(nii, 
      z = c(75:116),          #Can change slices if needed, but 48 slices is recommended
      plot.type = "single",    
      plane = c("sagittal"),  #Plane can be changed if needed
      zlim = c(20,210),#Changes contrast; first number is darkness, left is brightness, should not need to be changed...
      bg = 'white')
text(0.95, 0.885,labels = 'M68198325',cex = 0.75, col = 'darkgrey')
rasterImage(logo,xleft = 0.85, ybottom = 0.12, xright = 1, ytop = 0.27)
dev.off()

logo = readPNG('logo.png', native = T)



#Now you have to add the subjects that you just ran to the run sheet

all_URSI = c(as.character(subjects_already_ran_URSIs),URSIs_to_run_now)


all_dates = c(as.character(subjects_already_ran_dates),date_list)

all_studies = c(as.character(subjects_already_ran_study),study_list)



csv_dir = paste(getwd(),"/Subjects_Already_Ran.csv",sep = "")




if (n_to_run_now == 0) {
  
} else {
  #Save results in a .csv file with the same name as the one you read in, thus overriding it
  
  
  df = data.frame(URSI                 = all_URSI, 
                  
                  Date                 = all_dates,
                  
                  Study                = all_studies)
  
  
  write.csv(df,csv_dir, row.names = FALSE)
  
  }





n_sub_message = paste("\n\nThe image collage function was run on ",n_to_run_now, " new subjects!\n\n",sep = "")


if (n_to_run_now == 0) {
  message("\n\nThere were no new subjects to run on!\n\n")
} else {
  message(n_sub_message)
}




