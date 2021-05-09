library(lme4)
library(ggplot2)
library(labeling)
library(farver)

filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
beh.data <- read.csv(filepath)
beh.data$ID <- as.factor(beh.data$ID)
head(beh.data)

logit <- glmer(beh ~ gsr + (1|ID), data = beh.data, family = "binomial")

summary(logit)

tmpdata <- beh.data
jvalues <- with(beh.data, seq(from=min(gsr), to=max(gsr), length.out=100))
tmpdata$gsr 
pp <- lapply(jvalues, function(j) {
  tmpdata$gsr <-j 
  predict(logit, newdata = tmpdata, type = "response")
  })

#get mean PPs and IQ range for specific gap sizes
test_vals <- c(55, 80) #here I'm testing the model results at 55% and 80% svl. Add or change to test different gap sizes .
pp1 <- lapply(test_vals, function(j) {
  tmpdata$gsr <-j 
  predict(logit, newdata = tmpdata, type = "response")
  })

sapply(pp1, function(x){
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
  })

plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
  }))

plotdat <- as.data.frame(cbind(plotdat, jvalues))
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "GapSize")
head(plotdat)

#plot average marginal predicted probabilities
ggplot(plotdat, aes(x = GapSize, y = PredictedProbability)) + geom_line() + ylim(c(0,1))
ggplot(plotdat, aes(x = GapSize, y = PredictedProbability)) + geom_linerange(aes(ymin = Lower, ymax = Upper)) + geom_line(size = 2) + ylim(c(0,1))
