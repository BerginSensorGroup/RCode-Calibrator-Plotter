####################################################################
# Based on Zachary Barker's Code: Clean.R, FormatTime.R, TimeZone.R 
#
# Function that cleans, formats, and time-shifts all files in chosen directory.
# Creates new directories to place edited files. 
#
# All files must be from the same type of sensor.
# If timeshift is used, all files will be set to same start time. It may make more
# sense to leave S (timeshift) empty (this will omit the timeshifting operation), 
# and have the sensor clocks calibrated beforehand.
#
# Inputs are: 
#   (F,T,S)
#   F = folder (like "061116" which contains "CHAMBER TEST_027.csv")
#   T = type of sensor used ("Duke" or "DustTrak") <- Discontinued
#   S = desired start ("YYYY-mm-dd HH:MM:SS")
#
# Again, based on Zachary Barker's code
# Joshua Drawbaugh
# June, 2016
#
#####################################################################

ShortCut2 = function(F, S=0){
  
  ########## Initialize #########
  
  # Pathways to each folder
  RawFolder       = paste("Data/Raw Data/", F, sep = "")
  print(RawFolder)
  CleanedFolder   = paste("Data/Cleaned Data/", F , sep = "")
  FormattedFolder = paste("Data/Formatted Data/", F, sep = "")
  ReadyFolder     = paste("Data/Ready Data/", F, sep = "")
  
  # List of raw files in chosen folder
  RawList = list.files(RawFolder)
  print(RawList)
  
  # Create directories to place edited files
  dir.create(CleanedFolder)
  dir.create(FormattedFolder)
  dir.create(ReadyFolder)
  
  # List of file types
  Types = c()
  
  for (fileNum in 1:length(RawList)){
    # Open current raw file
    F1 = file(paste(RawFolder, "/", RawList[fileNum],sep=""), open="rt")
    
    # Check what type of file the current raw file is. 
    # Creates list of file types to be used in formatting and shifting
    for(i in 1:3){
      # Read first line
      Check  = readLines(F1,i)
      # Is this line included in "Check"?
      Check2 = max(grepl("Logging",Check, fixed = T))
      # If so, it's type Duke
      if(Check2 == TRUE){
        Types = c(Types, "Duke")
        break
      }
    }
    # If not Duke, currently the only other type is Dustrak
    if (Check2==FALSE){
      Types = c(Types,"DustTrak")
    }
  }
 
  #############################################################
  ################# Clean each file ###########################
  
  for (fileNum in 1:length(RawList)){
    
    # Current Type
    T = Types[fileNum]
    print(T)
    # Open current raw file
    F1 = file(paste(RawFolder, "/", RawList[fileNum],sep=""), open="rt")
    F2 = substr(RawList[fileNum],1,nchar(RawList[fileNum])-4)      # Creates new cleaned file
    F2 = paste(CleanedFolder,"/",F2,"_CLEANED.csv",sep = "",collapse=NULL)   
    F2 = file(F2,"w+")
    ########### Clean: Duke ##########
    if (T == "Duke"){
      repeat{                                                 # Creates header that matches data recorded
        L = readLines(F1,1)
        L2 = grepl("PM2.5",L,fixed = T)
        if (L2 == TRUE){
          cat(L,file = F2, sep = ",")
          cat(file = F2, sep = "\n")
          break
        }
      }
      
      repeat{
        L = readLines(F1,1)                                   # Copies valid data from raw file to cleaned file
        if (length(L) == 0){
          break
        }
        L2 = grepl("Logging",L,fixed = T)
        if (L2 == FALSE){
          L3 = grepl("PM2.5",L,fixed = T)
          if (L3 == FALSE){
            L4 = grepl("NA",L,fixed = T)
            if (L4 == FALSE){
              L5 = grepl("Box", L, fixed = T)
              if(L5==FALSE){
              cat(L,file = F2, sep = ",")
              cat(file = F2, sep = "\n")
              }
            }
          }
        }
      }
      
    }
    #################### Clean: DustTrak ####################
    if (T == "DustTrak"){
      repeat{     
        # Creates header that matches data recorded
        L = readLines(F1,1)
        L2 = grepl("Elapsed Time",L,fixed = T)
        if (L2 == TRUE){
          cat(L,file = F2, sep = ",")
          cat(file = F2, sep = "\n")
          break
        }
      }
      repeat{
        L = readLines(F1,1)                                   # Copies valid data from raw file to cleaned file
        if (length(L) == 0){
          break
        }
        L2 = grepl("Elapsed Time",L,fixed = T)
        if (L2 == FALSE){
          L3 = grepl("PM2.5",L,fixed = T)
          if (L3 == FALSE){
            L4 = grepl("NA",L,fixed = T)
            if (L4 == FALSE){
              cat(L,file = F2, sep = ",")
              cat(file = F2, sep = "\n")
              
            }
          }
        }
      }
    }
  }
  
  ################# Format each file ###################
  
  # Make list of cleaned files
  CleanedList = list.files(CleanedFolder)
  for(fileNum in 1:length(RawList)){
    T = Types[fileNum]
    # Name of cleaned file to work with
    Current = CleanedList[fileNum]
    # Name of formatted file that will be made
    FF = paste(FormattedFolder, "/", substr(Current, 1, nchar(Current)-12), "_FORMATTED.csv", sep="")
    # Rename working cleaned file to include pathway
    Current = paste(CleanedFolder, "/", Current, sep = "")
    # Current cleaned file to work with
    CF = read.csv(Current)
    
    ########## Foramt: Duke ###########
    if (T == "Duke"){
      
      # Date and Time column
      DT = paste(CF$'yyyy.mm.dd', CF$'hh.mm.ss', sep = "")
      # Add DateTime column to CF (current working cleaned file)
      CF$DateTime = strptime(DT, format = "%Y/%m/%d %H:%M:%S")
      CF$PM2.5=CF$labPM25
    }
    
    ########## Foramt: DustTrak ###########
    if(T=="DustTrak"){
      CF$"PM2.5" = CF$"PM2.5..mg.m3."*1000
      DT = strptime("2000-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
      DUR = CF$"Elapsed.Time..s."
      CF$DateTime = DT + DUR
    }
    
    ############ Format: Write files (for all cases) #########
    # Write formatted file to formatted folder
    write.csv(CF, file = FF, row.names = FALSE, quote = FALSE)
    
  }
  
  ############## Timeshift each file (if input S != NULL) ###########
  
  # Make list of formatted files
  FormattedList = list.files(FormattedFolder)
  
  # Loop through each file
  for(fileNum in 1:length(FormattedList)){  
    # Name of formatted file to work with
    Current = FormattedList[fileNum]
    # Name of ready file that will be made
    RF = paste(ReadyFolder, "/", substr(Current, 1, nchar(Current)-14), "_READY.csv", sep = "")
    # Rename working formatted file to include pathway
    Current = paste(FormattedFolder, "/", Current, sep = "")
    # Current formatted file to work with
    FF = read.csv(Current)
    
    # Read in DateTime column from formatted file
    DT = strptime(FF$DateTime, format="%Y-%m-%d %H:%M:%S")
    # First entry in DT i.e. time of first measurement
    StartDT = strptime(FF$DateTime[1], format="%Y-%m-%d %H:%M:%S")
    # Desired start date and time
    GoalDT = strptime(S, format="%Y-%m-%d %H:%M:%S")
    # Difference between desired and current time
    Dif = GoalDT-StartDT
    # Timeshift if S value is entered
    if (S!=0){
      FF$DateTime = DT + Dif
    }
    
    # Write ready file to ReadyFolder
    write.csv(FF, file = RF, row.names = FALSE,quote = FALSE)
  }
  
  
  closeAllConnections()
}