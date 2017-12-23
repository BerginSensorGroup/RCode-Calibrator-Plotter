#############################################################
# Function that averages time series data based on a user
#     defined length of time (in hours)
# 
# Inputs are: 
#   F = cleaned data frame
#   T = period of time ("us" (microseconds), "microseconds", 
#       "ms" (milliseconds), "milliseconds", "secs" (seconds), 
#       "seconds", "mins" (minutes), "minutes", "hours", 
#       "days", "weeks", "months", "quarters", and "years")
#   P = pollutant (PM2.5, must match a heading in the file)
#
# By Zachary Barker
# March, 2016
#
#############################################################

AVGTIME = function(F,T,P){
  
  library(xts)                                                               # loads time series package
  
  if (T == "secs"){                                                          # formats the date and time as a time object
    F$DateTime = strptime(F$DateTime, format = "%Y-%m-%d %H:%M:%S")
  } else if (T == "mins"){
    F$DateTime = strptime(F$DateTime, format="%Y-%m-%d %H:%M")         
  } else if (T == "hours"){
    F$DateTime = strptime(F$DateTime, format="%Y-%m-%d %H")
  } else if (T == "days"){
    F$DateTime = strptime(F$DateTime, format="%Y-%m-%d")
  }
  
  P = noquote(P)
  F = as.xts(F[,P],order.by = F$DateTime)                                    # creates time series object
  
  ep = endpoints(F,on=T)                                                     # applies specified average
  F2 = period.apply(F,ep,mean, na.rm = TRUE)
  
  F2 <- data.frame(date=index(F2), coredata(F2))                              # converts the xts to a dataframe
  names(F2) <- c("DateTime",P)
  F2$DateTime <- as.POSIXct(F2$DateTime)
  F2
  
  # Name = substr(CF,1,nchar(CF)-4)                                          # Generates new file with suffix "AVG"
  # Name = paste(Name,"_AVG.csv",sep = "",collapse=NULL)
  # write.csv(F2,file = Name, row.names = FALSE,quote = FALSE)
  
}


