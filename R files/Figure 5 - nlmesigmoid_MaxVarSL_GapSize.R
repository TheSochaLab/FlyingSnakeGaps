library(nlme)
library(dplyr)
library(ggplot2)
library(ggeffects)

#load the data
filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number
bdata = bdata[!is.na(bdata$rel_maxC),] #skip nans

head(bdata) #check that everything looks good

#define a function relating desired variables
f1 <- rel_maxC ~ L/(1+exp(-k*(gs_bin-x0)))+b

#define the model for nlme, assigning fixed/random variables etc.
#starting values are from a fixed effects sigmoid fit of the data run in Python.
n2 <- nlme(f1,
 data = bdata,
 fixed = L + k + x0 + b ~ 1,
 random = x0 ~ 1,
 groups = ~ ID,
 start = c(L=59.438025001046256, k=0.118802440119745, x0=60.55142319435201, b=9.921649650155436),
 method = "REML") 

plot(n1) #residuals
summary(n1)

#' sort the data by ascending gsr.  This just makes the lines draw from left to right
curve = bdata[order(bdata$gs_bin),]

#' change the plotting parameters to be two rows and three columns
par(mfcol = c(2,3))
#' unique(ah$ID) returns a list of the individual snakes.  The loop iterates through each 
for(i in unique(curve$ID)){
  #' filter the data (using the dplyr package) to only the observations from the snake in
  #' question
  temp = dplyr::filter(curve, ID == i)
  #' predict max velocity of that snake
  preds = predict(n1, newdata = temp)
  #' plot the observations.  pch changes point type. xlab/ylab change the axis labels, main
  #' changes the title
  plot(temp$gs_bin, temp$rel_maxC, pch = 20, xlab = "Gap Size (%SVL)", ylab = "Max Dev from a Straight Line (%SVL)",
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
plot(curve$gsr,curve$rel_maxC,pch = 20, xlab = "Gap Size (%SVL)", ylab = "Max Dev from a Straight Line (%SVL)")
lines(curve$gs_bin,vsigmoid(fixed.effects(n1)['L'],fixed.effects(n1)['k'],fixed.effects(n1)['x0'],fixed.effects(n1)['b'],curve$gs_bin),lty=2,col="grey",lwd=3)

#individual lines
p <- ggplot(bdata, aes(x = gs_bin, y = rel_maxC, colour = ID)) +
  geom_point(data = bdata,                      # adding the raw data (scaled values)
             aes(x = gsr, y = rel_maxC, colour = ID,shape=ID)) +
  geom_line(aes(y = predict(n1)),size=1) 
print(p)

