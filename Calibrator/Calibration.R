#############################################################
# Function that plots two sensors against each other and
#    fits a regression for calibration.
# Use after AvgTime.R
#    (Assumes date and time are already formated and in posix class)
#
# Inputs are: 
#   x = reference dataframe
#   y = dataframe to calibrate
#   t1= start time which to calibrate 
#   t2= end time which to calibrate
#
# By Zachary Barker
# March, 2016
#
#############################################################

CALIBRATE = function(x,y,t1=NULL,t2=NULL){
  
  x_m <- x
  y_m <- y
  
  if (!is.null(t1)){
    start <- as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S")
    x_m <- subset(x_m, x_m[,1] >= start)                                  # subsets the data so that only the
    y_m <- subset(y_m, y_m[,1] >= start)                                  #   co-located period is used
  } 
  
  if (!is.null(t2)){
    end <- as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S") 
    x_m <- subset(x_m, x_m[,1] <= end)                                    # subsets the data so that only the
    y_m <- subset(y_m, y_m[,1] <= end)                                    #   co-located period is used
  } 
  
  out <- merge(x_m,y_m,by = "DateTime",all = FALSE)                          # Creates data set from matching data
  
  x_m <- out[,2]
  y_m <- out[,3]
  reg <- lm(y_m~x_m)                                                         # Linear regression
  
  b <- as.numeric(reg$coefficients[1])                                       # calls the intercept from lm
  m <- as.numeric(reg$coefficients[2])                                       # calls the slope from the lm
  
  y_cal <- y                                                                 # creates calibrated data frame
  y_cal[,2] <- (y_cal[,2]-b)/m
  
  r2 = summary(reg)$adj.r.squared                                            # Creates legend attributes
  Leg = vector('expression',2)
  Leg[1]= paste("y = ",format(reg$coefficients[2], digits =4),"x + ",
                format(reg$coefficients[1], digits=4),sep ="")
  Leg[2]= substitute(expression(italic(R)^2 == MYVALUE),
                     list(MYVALUE = format(r2,dig=4)))[2]
  
  par(mfrow=c(2,1),mar = c(5, 3, 1, 1))
  plot(out[,2],out[,3],xlab = "Reference",ylab = "Dependent",pch = 20,       # plot regression
       cex = 1.5, cex.lab = 1.5,cex.axis = 1.5)                                                 
  abline(reg, col = "red", lwd = 2)                                          # line of best fit
  legend("topleft", legend = Leg, bty = 'n',cex = 1.5)                       # Adds legend
  
  plot(x,type = "l",main ="",cex.lab = 1.5, cex.axis = 1.5)                  # plot time series
  lines(y, col = "blue")
  lines(y_cal, col = "red")
  legend("topleft",c("Reference","Dependent","Calibrated"),
         lty = c(1,1,1),col = c("black","blue","red"),cex = 1.5,bty = 'n')
  
  cal_constants = data.frame("slope" = m, "intercept" = b)                   # creates calibration constant output
}