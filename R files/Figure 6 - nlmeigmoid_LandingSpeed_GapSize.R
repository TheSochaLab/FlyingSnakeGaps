library(nlme)
library(dplyr)
library(ggeffects)
library(ggplot2)

#load the data
filepath = paste(getwd(),"Summary Datasets/vdata.csv", sep="/")
vdata <- read.csv(filepath)
vdata = vdata[!is.na(vdata$slv),]
head(vdata) #check that everything looks good
vdata$ID <- as.factor(vdata$ID) #make sure models treat ID as a factor, not a number


#define a function relating desired variables
f1 <- slv ~ L/(1+exp(-k*(gs_bin-x0)))+b

#define the model for nlme, assigning fixed/random variables etc.
#starting values are from a fixed effects sigmoid fit of the data run in Python.
n1 <- nlme(f1,
 data = vdata,
 fixed = L + k + x0 + b ~ 1,
 random = x0 ~ 1,
 groups = ~ ID,
 start = c(L=1.8497368062508106, k=0.21699714221264196, x0= 57.50273394645001, b=0.18783532461132715)) 

plot(n1)
summary(n1)
intervals(n1,which="fixed")

#' sort the data by ascending gsr.  This just makes the lines draw from left to right
vel = vdata[order(vdata$gs_bin),]

#' change the plotting parameters to be two rows and three columns
par(mfcol = c(2,3))
#' unique(ah$ID) returns a list of the individual snakes.  The loop iterates through each 
for(i in unique(vel$ID)){
  #' filter the data (using the dplyr package) to only the observations from the snake in
  #' question
  temp = dplyr::filter(vel, ID == i)
  #' predict max velocity of that snake
  preds = predict(n1, newdata = temp)
  #' plot the observations.  pch changes point type. xlab/ylab change the axis labels, main
  #' changes the title
  plot(temp$gs_bin, temp$slv, pch = 20, xlab = "Gap Size (%SVL)", ylab = "Landing Speed (SVL/s)",
       #' paste connects two character values, here it connects "Snake" and the individual
       #' number.  The default is to put a space in between
       main = paste("Snake", i))
  #' put the line on the plot.  col changes color. lwd changes line width
  lines(temp$gsr, preds, col = 'red', lwd = 2)
}
#' change the plotting parameters back to one plot per page
par(mfcol = c(1,1))

#define function to plot only fixed effect estimate
sigmoid1 <- function(L,k,x0,b,x)
{
  val = L/(1+exp(-k*(x-x0)))+b
  return(val)
}
vsigmoid <- Vectorize(sigmoid1)

#plot overall result
plot(vel$gsr,vel$slv,pch = 20, xlab = "Gap Size (%SVL)", ylab = "Landing speed (SVL/s)")
lines(vel$gs_bin,vsigmoid(fixed.effects(n1)['L'],fixed.effects(n1)['k'],fixed.effects(n1)['x0'],fixed.effects(n1)['b'],vel$gs_bin),lty=2,col="grey",lwd=3)

#by individual
p <- ggplot(vdata, aes(x = gs_bin, y = slv, colour = ID)) +
  geom_point(data = vdata,                      # adding the raw data (scaled values)
             aes(x = gsr, y = slv, colour = ID,shape=ID)) +
  geom_line(aes(y = predict(n1)),size=1) 
print(p)

