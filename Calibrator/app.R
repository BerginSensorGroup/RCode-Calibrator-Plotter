################################################################################
# Calibrator
# 
# Heavily based on TimeSeries by Zachary Barker 
# 
# App that retrieves calibration constants for each sensor.
# In addition, plots original data of chosen sensor with reference
# and calibrated data.
#
# Heavily based on TimeSeries by Zachary Barker 
# Josh Drawbaugh
# June, 2016
################################################################################

############## Initialize ################

###### Libraries and Functions #####

library(shiny)
source ("ShortCut2.R")
source("AvgTime.R")
source("Calibration.R")
source("CalConst.R")


####### EDIT HERE #########

# Folder with files         
Folder = "072116 PM Cal 1"     # <- CHANGE HERE

# Pathway of folder. Doesn't need to be changed
Location = paste("Data/Ready Data/", Folder, sep = "") 

# Comment out next lines if you don't want to use ShortCut2.R
ShortCut2(Folder
          # Optional: Set all start times to the same. Comment out if not.
          #,"2016-06-01 15:20:00"
)


# List of file names
FileNames = list.files(Location)

# List of file names in format Location/name
FileList = paste(Location,"/",FileNames,sep="")

# Names as they are displayed in checkboxes
DispName = substr(FileNames,1,nchar(FileNames)-10)

# Set start and end times
CurrentFile = read.csv(FileList[1])
StartTime = min(strptime(CurrentFile$DateTime, format = "%Y-%m-%d %H:%M"))
EndTime = max(strptime(CurrentFile$DateTime, format = "%Y-%m-%d %H:%M"))
for (file in 1:length(FileList)){
  CurrentFile = read.csv(FileList[file])
  CurrentTimes = strptime(CurrentFile$DateTime, format = "%Y-%m-%d %H:%M")
  # Start and end times
  StartTime= min(StartTime, CurrentTimes)
  EndTime= max(EndTime, CurrentTimes)
}



#------------------------------------------------------------------------------#
############################ UI ################################################
#------------------------------------------------------------------------------#

ui <- fluidPage(
  
  fluidRow(
    column(6,
           column(6,
                  selectInput("avg", label = "Select Average",
                              c("Minutes" = "mins",
                                "Hours"= "hours",
                                "Days" = "days")),
                  
                  textInput("cal_start",label = "Enter beginning time of colocation",
                            value = StartTime),
                  
                  radioButtons("cal_x", label = "Select Reference Sensor",
                               DispName)
           ),
           column(6,
                  selectInput("pollutant", label = "Select Pollutant",
                              c("PM 2.5" = "PM2.5")),
                  
                  textInput("cal_end",label = "Enter ending time of colocation", 
                            value = EndTime),
                  
                  
                  radioButtons("cal_y", label = "Select Dependent Sensor",
                               DispName)
           )
    ),    
    column(6,
           plotOutput("cal_plot",height = "800px")
    )
  )
)


#------------------------------------------------------------------------------#
############################## SERVER ##########################################
#------------------------------------------------------------------------------#

server <- function(input, output) {
  
  
  ############# Plot Relation and Create CSV ################
  output$cal_plot <- renderPlot({
    
    # Create csv file with cal constants and in appropriate folder
    CALCONST(Folder, input$cal_x)
    
    # Determine chosen files
    for(fileNum in 1:length(DispName)){
      # Determine chosen reference sensor
      if(DispName[fileNum]==input$cal_x){
        PlotFileX = FileList[fileNum]
      }
      # Determine chosen dependent sensor
      if(DispName[fileNum]==input$cal_y){
        PlotFileY = FileList[fileNum]
      }
    }
    
    # Read chosen files
    x <- read.csv(PlotFileX)
    y <- read.csv(PlotFileY)
    
    # Format files using AvgTime.R 
    x <- AVGTIME(x,input$avg,input$pollutant)
    y <- AVGTIME(y,input$avg,input$pollutant)
    
    # Plot using Zac's Calibration.R
    CALIBRATE(x,y,input$cal_start,input$cal_end)
  })
  
}
shinyApp(ui = ui, server = server)

