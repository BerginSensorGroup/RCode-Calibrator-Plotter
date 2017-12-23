#####################################################################
# Plotter
#
# Based on Zachary Barker's
# App 2 (Data visualization app for real time air quality monitoring)
# 
# Visualizes meaurements of sensors.
#
# Joshua Drawbaugh
# June 2016
#
#####################################################################

library(shiny)
library(RColorBrewer)


################ Initialize ##################


######  Edit Here (Folder and ShortCut2) ###############

# Folder with files         
Folder = "071416 Personal PM"

# Pathway of folder. Likely doesn't need to be changed
Location = paste("Data/Ready Data/", Folder, sep = "") 

# Comment out next two lines if you don't want to use ShortCut2.R
source("ShortCut2.R")
ShortCut2(Folder
          # Optional: Set all start times to the same. Comment out if not.
          #,"2016-07-07 15:00:00"
)


# List of file names
FileNames = list.files(Location)

# List of file names in format Location/name
FileList = paste(Location,"/",FileNames,sep="")

# Names as they are displayed in checkboxes
DispName = substr(FileNames,1,nchar(FileNames)-10)

#colors for plotting
# chosen 30 colors
blues   = c(color=brewer.pal(9,"Blues"))[3:8] 
reds    = c(color=brewer.pal(9,"Reds"))[3:8]
greens  = c(color=brewer.pal(9,"Greens"))[3:8] 
oranges = c(color=brewer.pal(9,"Oranges"))[3:8]
purples = c(color=brewer.pal(9,"Purples"))[3:8]
colors = c(blues,oranges,greens,purples,reds)
# reorganize colors to pallet of dark to light, alternating blues to purples
seqnc  = seq(from=1, to=30, by=6)
order  = c(seqnc+1,seqnc+2,seqnc,seqnc+3,seqnc+4,seqnc+5)
pallet = colors[order]
# colors for pollutants
PMcolors   = pallet
O3colors   = c("dodgerblue", "seagreen1","orange","darkorchid1")
CO2colors  = c("blue4", "green4", "darkorange4", "darkorchid4")
NO2colors  = c("dodgerblue4", "seagreen4", "orange3", "magenta4")
NOcolors   = c("cornflowerblue", "palegreen2","sienna1", "maroon")
Tempcolors = c("red", "orange", "tomato", "darkorange")
RHcolors   = c("blue", "skyblue","steelblue1", "royalblue")

# Set time range, record max/min values of each measurement type
CurrentFile = read.csv(FileList[1])
StartTime = min(strptime(CurrentFile$DateTime, format = "%Y-%m-%d %H:%M"))
EndTime = max(strptime(CurrentFile$DateTime, format = "%Y-%m-%d %H:%M"))
# There's got to be a better way to do this...
MaxPM=c()
MaxTemp=c()
MinTemp=c()
MaxRH=c()
MinRH=c()
for (file in 1:length(FileList)){
  CurrentFile = read.csv(FileList[file])
  CurrentTimes = strptime(CurrentFile$DateTime, format = "%Y-%m-%d %H:%M")
  # Start and end times
  StartTime= min(StartTime, CurrentTimes)
  EndTime= max(EndTime, CurrentTimes)
  # PM2.5
  MaxPM = max(MaxPM, CurrentFile$PM2.5)
  MinPM = min(MaxPM, CurrentFile$PM2.5)
  MaxTemp = max(MaxTemp, CurrentFile$TempC)
  MinTemp = min(MaxTemp, CurrentFile$TempC)
  MaxRH = max(MaxRH, CurrentFile$Humidity)
  MinRH = min(MaxRH, CurrentFile$Humidity)
}

################################# App ###############################

####### User Interface ######

ui <- fluidPage(    
  sidebarLayout(      
    sidebarPanel(
      sliderInput(
        inputId = "Date", "Select the date range", 
        strptime(StartTime, format = "%Y-%m-%d %H:%M"),
        strptime(EndTime, format = "%Y-%m-%d %H:%M"), 
        value = c(strptime(StartTime, format = "%Y-%m-%d %H:%M"),
                  strptime(EndTime, format = "%Y-%m-%d %H:%M"))),
      
      numericInput(inputId = "ymax", "Vertical maximum for first plot:", MaxPM),
      numericInput(inputId = "ymin", "Vertical minimum for first plot:", MinPM),
      
      checkboxGroupInput("Pollutants", label = "Pollutants to be plotted in first figure:",
                         c("PM2.5", "O3", "CO2", "NO2", "NO"), 
                         selected = "PM2.5"),
      checkboxGroupInput("Sensors", label = "Sensors to be plotted in first figure:",
                         DispName,
                         selected = DispName),
      #numericInput(inputId = "ymax2", "Vertical maximum for second plot:", 65),
      #numericInput(inputId = "ymin2", "Vertical minimum for second plot:", 20),
      checkboxGroupInput("Measurements", label = "Measurements to be plotted in second figure:",
                         c("Temperature", "Humidity"), selected = c("Temperature","Humidity")),
      checkboxGroupInput("Sensors2", label = "Sensors to be plotted in second figure",
                         DispName,
                         selected = DispName)
    ),
    
    mainPanel(
      plotOutput("plot",click="plot_click",height = "700px"),
      plotOutput("plot2", height = "700px")
    )  
  )
)


####### Server #########

server <- function(input, output) {
  # Figure 1
  output$plot = renderPlot({

    plot(c(input$Date[1],input$Date[2]),
         c(0,0), 
         main = "Figure 1. Pollutants vs. Date",
         ylim = (c(input$ymin,input$ymax)), 
         xlab = "Date",
         ylab=expression(paste("(",mu, g, "/", m^3,")", sep="")), 
         col = "white")
    
    # List of Legend information: names and colors and line types
    LegendName  = c()
    LegendColor = c()
    LegendLine = c()
    
    # Loop through each sensor
    for (file in 1:length(FileList)){
      
      # Abreviated file name
      if(nchar(DispName[file])>7){
        NameShort = paste(substr(DispName[file],1,3),"...",
                          substr(DispName[file],nchar(DispName[file])-3,nchar(DispName[file])),sep="")
      }
      else{
        NameShort = DispName[file]
      }
 
      # Choose sensor file one at a time, and go through
      CurrentFile = read.csv(FileList[file])
      # x-axis
      Time = strptime(CurrentFile$DateTime, format= "%Y-%m-%d %H:%M:%S")
      
      # Show plots of only the selected sensors
      if(isTRUE(DispName[file] %in% input$Sensors))
      {
        # Show plots of only the selectred Pollutants
        # PM2.5
        if(isTRUE("PM2.5" %in% input$Pollutants)&&!is.null(CurrentFile$PM2.5)){
          lines(Time, CurrentFile$PM2.5 , type = "l", lty = 1,  col=pallet[file%%30])
          LegendName  = c(LegendName, paste(NameShort,"PM2.5"))
          LegendColor = c(LegendColor, pallet[file%%30])
          LegendLine  = c(LegendLine, 1)
        }
        # O3
        if(isTRUE("O3" %in% input$Pollutants)&&!is.null(CurrentFile$O3)){
          lines(Time, CurrentFile$O3, type = "l", lty = 2, col=pallet[file%%30])
          LegendName  = c(LegendName, paste(NameShort,"O3"))
          LegendColor = c(LegendColor, pallet[file%%30])
          LegendLine  = c(LegendLine, 2)
        }
        # CO2
        if(isTRUE("CO2" %in% input$Pollutants)&&!is.null(CurrentFile$CO2)){
          lines(Time, CurrentFile$CO2, type = "l",lty = 3, col=pallet[file%%30])
          LegendName  = c(LegendName, paste(NameShort,"CO2"))
          LegendColor = c(LegendColor, pallet[file%%30])
          LegendLine  = c(LegendLine, 3)
        }
        # NO2
        if(isTRUE("NO2" %in% input$Pollutants)&&!is.null(CurrentFile$NO2)){
          lines(Time, CurrentFile$NO2, type = "l",lty = 4, col=pallet[file%%30])
          LegendName  = c(LegendName, paste(NameShort,"NO2"))
          LegendColor = c(LegendColor, pallet[file%%30])
          LegendLine  = c(LegendLine, 4)
        }
        # NO
        if(isTRUE("NO" %in% input$Pollutants)&&!is.null(CurrentFile$NO)){
          lines(Time, CurrentFile$NO, type = "l",lty = 5, col=pallet[file%%30])
          LegendName  = c(LegendName, paste(NameShort,"NO"))
          LegendColor = c(LegendColor, pallet[file%%30])
          LegendLine  = c(LegendLine, 5)
        }
      }
    }

    # Create Legned based on LegendName, LegentLines, LegendColor
    legend("topright",LegendName,lty=LegendLine, col=LegendColor)
    
  })
  
  # Figure 2
  output$plot2 = renderPlot({
    par(mar=c(4,4,2,4))
    plot(c(input$Date[1],input$Date[2]),
         c(0,0), 
         main = "Figure 2. Temperature/Humidity vs. Date",
         ylim = (c(MinTemp-1,MaxTemp+1)), 
         xlab = "Date",
         ylab="Temperature (˚C)", 
         col = "white")
    
    # List of Legend information: names and colors
    LegendName2  = c()
    LegendColor2 = c()
    
    # PLOT TEMPERATURE
    # Loop through each sensor
    for(file in 1:length(FileList)){
      
      # Abreviated file name
      NameShort = paste(substr(DispName[file],1,3),"...",
                        substr(DispName[file],nchar(DispName[file])-3,nchar(DispName[file])),sep="")
      # Choose sensor file one at a time, and go through
      CurrentFile2 = read.csv(FileList[file])
      # x-axis
      Time2 = strptime(CurrentFile2$DateTime, format= "%Y-%m-%d %H:%M:%S")
      # Plot only selected sensors
      if(isTRUE(DispName[file] %in% input$Sensors2)){
        # Plot only selected measurements
        # Temperature
        if(isTRUE("Temperature" %in% input$Measurements)&&!is.null(CurrentFile$TempC)){
          lines(Time2,CurrentFile2$TempC, type = "l", col= c(reds,oranges)[file%%30])
          LegendName2  = c(LegendName2, paste(NameShort,"Temperature"))
          LegendColor2 = c(LegendColor2, c(reds,oranges)[file%%30])
        }

      }
    }
    # PLOT HUMIDITY
    par(new=TRUE, mar=c(4,4,2,4))
    plot(c(input$Date[1],input$Date[2]),
         c(0,0), 
         main = "Figure 2. Temperature/Humidity vs. Date",
         ylim = (c(MinRH-1,MaxRH+1)), 
         xlab = "",ylab="", col = "white", axes = FALSE)
    axis(side=4)
    mtext(side=4,line=2,"Humidity (%)")
    
    PlotFiles = c()
    
    # Loop through each file
    for(file in 1:length(FileList)){
      # Abreviated file name
      NameShort = paste(substr(DispName[file],1,3),"...",
                        substr(DispName[file],nchar(DispName[file])-3,nchar(DispName[file])),sep="")
      # Choose sensor file one at a time, and go through
      CurrentFile2 = read.csv(FileList[file])
      # x-axis
      Time2 = strptime(CurrentFile2$DateTime, format= "%Y-%m-%d %H:%M:%S")
      
      # Plot only selected sensors
      if(isTRUE(DispName[file] %in% input$Sensors2)){
        PlotFiles = c(PlotFiles, FileList[file])
        print(PlotFiles)
        # Plot only selected measurements
        # Humidity
        if(isTRUE("Humidity" %in% input$Measurements)&&!is.null(CurrentFile$Humidity)){
          lines(Time2,CurrentFile2$Humidity, type = "l", col= c(blues,greens)[file%%30])
          LegendName2  = c(LegendName2, paste(NameShort,"Humidity"))
          LegendColor2 = c(LegendColor2, c(blues,greens)[file%%30])
        }
      }
    }
    # List of legend information: lines
    LegendLine2 = c()
    for(i in 1:length(LegendName2)){
      LegendLine2 = c(LegendLine2,1)
    }
    # Create Legned based on LegendName, LegentLines, LegendColor
    legend("topright",LegendName2,lty=LegendLine2, col=LegendColor2)
  })
}

shinyApp(ui = ui, server = server)