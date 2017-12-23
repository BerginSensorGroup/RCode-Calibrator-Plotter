#############################################################
# CalConst.R
#
# Uses Calibration.R and AvgTime.R by Zachary Barker
#
# Function that stores calibration constants in csv file.
#   In addition, it co-plots original data with reference
#   and calibrated data. File wiht calibration constants
#   will be stored in folder "Data>Calibration Constants>*Folder_name*".
#
#
# Inputs required are: 
#     F   = Name of folder that includes reference and dependent 
#           sensor files. 
#           e.g.) "060116"
#     Ref = Name of reference sensor. 
#           Do not include suffixes suchas "_READY" or ".csv". 
#           e.g.) "CHAMBER TEST_026"
#
#  Optional inputs are:
#     Avg   = Sampling intervals. Check AvgTime.R for specific options.
#             Default is "mins" (minutes).
#     Type  = Type of measurement. Check Calibration.R for specific options.
#             Default is "PM2.5".
#     Ti,Tf = Start and end times. 
#             Default is NA.
#   
#
# Uses Calibration.R and AvgTime.R by Zachary Barker
# By Josh Drawbaugh
# June, 2016
#
#############################################################

CALCONST = function(F,Ref,Avg="mins",Type="PM2.5",Ti=NA,Tf=NA){
  print(F)
  # Source Calibration.R (written by Zachary Barker)
  source("Calibration.R")
  # Source AvgTime.R (also written by Zachary Barker)
  source("AvgTime.R")
  
  # Create new file to store constants
  F2 = paste("Data/Calibration Constants/",F,".csv",sep="")
  F2 = file(F2,"w+")
  
  # Create header
  cat("Sensor","Slope", "Intercept",file = F2, sep=",")
  
  # Add pathways to names
  Folder = paste("Data/Ready Data/", F, sep="")
  Ref = paste(Folder,"/",Ref,"_READY.csv",sep="")
  
  # Make list of files in Folder
  FileList = list.files(Folder)
  # List of disply names
  DispName = substr(FileList,1,nchar(FileList)-10)
  
  # Read reference file
  Ref = read.csv(Ref)
  # Format Ref based on chosen sampling interval
  Ref = AVGTIME(Ref,Avg,Type)
  
  # Loop through each file to calibrate against reference file
  for(fileNum in 1:length(FileList)){
    # Add pathway to name of current dependent file
    Dep = paste(Folder,"/",FileList[fileNum],sep="")
    # Read current dependent file
    Dep = read.csv(Dep)
    # Format current dependent file based on chosen sampling interval
    Dep = AVGTIME(Dep, Avg, Type)
    # Calibration constants for this dependent sensor
    Const = CALIBRATE(Ref, Dep)
    # New line in output csv
    cat(file = F2, sep = "\n")
    # Add current constants to csv
    cat(DispName[fileNum],Const$slope,Const$intercept,file = F2, sep=",")
  }
}