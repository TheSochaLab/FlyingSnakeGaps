library(nlme)
library(segmented)
source("source-segmented.lme.r")

filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata = bdata[!is.na(bdata$ahl),] #skip nans

bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number

bdata<-bdata[order(bdata$ID),] #order very important to get correct plots!

#first fit a standard linear model and then update with segmented (Muggo 2016)
o <-lme(ahl~gs_bin, random=list(ID=pdDiag(~1+gs_bin)), data=bdata)

#update with random slopes but fixed breakpoints
oseg<-segmented.lme(o, Z=gs_bin, random=list(ID=pdDiag(~1+gs_bin+U)))
#random slopes and breakpoints
oseg.new<-segmented.lme(o, Z=gs_bin,random=list(ID=pdDiag(~1+gs_bin+U+G0)))

#plots
plot(oseg.new, n.plot=c(2,3), pop=TRUE,ylim=c(0,30),xlim=c(30,120),xlab='Gap Size, %SVL',ylab='Arc Height (low), %SVL')
plot(oseg, n.plot=c(2,3), pop=TRUE,ylim=c(0,30),xlim=c(30,120),xlab='Gap Size, %SVL',ylab='Arc Heigh (low), %SVL')

AIC(oseg, oseg.new)
# delta AIC is ~ 15, strong support for random breakpoints model. 

#confidence intervals for breakpoints, slope change.
confint(oseg.new)

