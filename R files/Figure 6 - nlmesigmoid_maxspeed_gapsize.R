library(nlme)
library(dplyr)
library(lme4)
library(ggplot2)
library(ggeffects)

#load the data
filepath = paste(getwd(),"Summary Datasets/vdata.csv", sep="/")
vdata <- read.csv(filepath)
vdata = vdata[!is.na(vdata$mhv),]
vdata$ID <- as.factor(vdata$ID) #make sure models treat ID as a factor, not a number

head(vdata) #check that everything looks good

#define a function relating desired variables - scaled land v vs. binned gs
f1 <- smv ~ L/(1+exp(-k*(gs_bin-x0)))+b

#define the model for nlme, assigning fixed/random variables etc.
#starting values are from a fixed effects sigmoid fit of the data run in Python.
n1 <- nlme(f1,
 data = vdata,
 fixed = L + k + x0 + b ~ 1,
 random = x0 ~ 1,
 groups = ~ ID,
 start = c(L=2.6347242588091047, k=0.1895119674063019, x0=58.62341587044373, b=0.3182001099107992),
 method = "REML")

plot(n1)
summary(n1)
intervals(n1,which="fixed")

#' sort the data by ascending gsr.  This just makes the lines draw from left to right
max = vdata[order(vdata$gs_bin),]

#' change the plotting parameters to be two rows and three columns
par(mfcol = c(2,3))
#' unique(ah$ID) returns a list of the individual snakes.  The loop iterates through each 
for(i in unique(max$ID)){
  #' filter the data (using the dplyr package) to only the observations from the snake in
  #' question
  temp = dplyr::filter(max, ID == i)
  #' predict max velocity of that snake
  preds = predict(n1, newdata = temp)
  #' plot the observations.  pch changes point type. xlab/ylab change the axis labels, main
  #' changes the title
  plot(temp$gs_bin, temp$smv, pch = 20, xlab = "relative gap size", ylab = "relative max speed",
       #' paste connects two character values, here it connects "Snake" and the individual
       #' number.  The default is to put a space in between
       main = paste("Snake", i))
  #' put the line on the plot.  col changes color. lwd changes line width
  lines(temp$gs_bin, preds, col = 'red', lwd = 2)
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
plot(max$gsr,max$smv,pch = 20, xlab = "Gap Size (%SVL)", ylab = "Max Speed (SVL/s)")
lines(max$gs_bin,vsigmoid(fixed.effects(n1)['L'],fixed.effects(n1)['k'],fixed.effects(n1)['x0'],fixed.effects(n1)['b'],max$gs_bin),lty=2,col="grey",lwd=3)

#Plot 3b: untransformed data, individual lines
p <- ggplot(vdata, aes(x = gs_bin, y = smv, colour = ID)) +
  geom_point(data = vdata,                      # adding the raw data (scaled values)
             aes(x = gsr, y = smv, colour = ID,shape=ID)) +
  geom_line(aes(y = predict(n1)),size=1) 
print(p)
