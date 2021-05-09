library(nlme)
library(segmented)
source("source-segmented.lme.r")

filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata = bdata[!is.na(bdata$over),] #skip nans
bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number

bdata<-bdata[order(bdata$ID),] #order very important to get correct plots!

bdata$over = 100*bdata$over #changing from decimal to percentage.

#check response variable distribution
#from reference: Muggo 2018. x = time in the example (predictor), y = response.
hist(bdata$over) #pretty normal; maybe a bit left.
#check response by individual
plot(groupedData(over~gs_bin|ID,data=bdata))

#first fit a standard linear model and then update with segmented
o <-lme(over~gs_bin, random=list(ID=pdDiag(~1+gs_bin)), data=bdata)

#update with random slopes but fixed breakpoints
oseg<-segmented.lme(o, Z=gs_bin, random=list(ID=pdDiag(~1+gs_bin+U)))

#random slopes and breakpoints
oseg.new<-segmented.lme(o, Z=gs_bin,random=list(ID=pdDiag(~1+gs_bin+U+G0)))

#plots
plot(oseg.new, n.plot=c(2,3), pop=TRUE,ylim=c(0,100),xlim=c(30,120),xlab='Gap Size, %SVL',ylab='Overshoot, %SVL')
plot(oseg, n.plot=c(2,3), pop=TRUE,ylim=c(0,100),xlim=c(30,120),xlab='Gap Size, %SVL',ylab='Overshoot, %SVL')

AIC(oseg, oseg.new)
#random slopes + breakpoints seems to match the data better, delta AIC = 18. Using random changepoints.
#confidence intervals for breakpoint, slope change
confint(oseg.new)
