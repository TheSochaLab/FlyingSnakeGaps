library(lme4)
library(dplyr)
library(ggplot2)
library(ggeffects)


#load the data
filepath = paste(getwd(),"Summary Datasets/tdata.csv", sep="/")
tdata <- read.csv(filepath)
tdata$ID <- as.factor(tdata$ID) #make sure models treat ID as a factor, not a number

head(tdata) #check that everything looks good
#check response variable distribution
hist(tdata$hpt) #slightly left skewed

#random intercepts model
mod1 <- lmer(hpt ~ gs_bin + (1|ID), data=tdata, REML=TRUE)
plot(mod1) #cone of variance, try log transform.


tdata$lhpt = log(tdata$hpt)
mod2 <- lmer(lhpt ~ gs_bin + (1|ID),data=tdata,REML=TRUE)
plot(mod2)
#reduced heteroskedasticity.

#test whether random slopes improves the model
mixed.ranslope <- lmer(lhpt ~ gsr+(gsr|ID),data=tdata,REML=TRUE) #singular; use reduced model.

summary(mod2)
confint(mod2)

#-----
#PlOTS

#PlOT FOR EACH INDIVIDUAL
#' sort the data by ascending gsr.  This just makes the lines draw from left to right
bod = tdata[order(tdata$gsr),]
#' change the plotting parameters to be two rows and three columns
par(mfcol = c(2,3))
#' unique(ah$ID) returns a list of the individual snakes.  The loop iterates through each 
for(i in unique(bod$ID)){
  #' filter the data (using the dplyr package) to only the observations from the snake in
  #' question
  temp = dplyr::filter(bod, ID == i)
  #' predict max velocity of that snake
  preds = predict(mod2, newdata = temp)
  #' plot the observations.  pch changes point type. xlab/ylab change the axis labels, main
  #' changes the title
  plot(temp$gsr, temp$lhpt, pch = 20, xlab = "Gap Size (%SVL)", ylab = "Log(Head Position (%GS))",
       #' paste connects two character values, here it connects "Snake" and the individual
       #' number.  The default is to put a space in between
       main = paste("Snake", i))
  #' put the line on the plot.  col changes color. lwd changes line width
  lines(temp$gsr, preds, col = 'red', lwd = 2)
}
#' change the plotting parameters back to one plot per page
par(mfcol = c(1,1))

#PLOT BY INDIVIDUAL, UNTRANSFORMED
#plot 1: by individual, untransformed data
mm_plot <- ggplot(tdata, aes(x = gs_bin, y = hpt)) +
  facet_wrap(~ID, nrow=2) +   # a panel for each snake
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(tdata, pred = exp(predict(mod2))), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
print(mm_plot)

#Plot 3a: untransformed data, summary
pred.mm <- ggpredict(mod2, terms = c("gs_bin"))  # this gives overall predictions for the model

p <- (ggplot(pred.mm) + 
    geom_line(aes(x = x, y = exp(predicted))) +          # slope
    geom_point(data = tdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = hpt, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Head Position at Transistion") + 
    theme_minimal()
)
print(p)

#Plot 3b: untransformed data, individual lines
p <- ggplot(tdata, aes(x = gs_bin, y = hpt, colour = ID)) +
  geom_point(data = tdata,                      # adding the raw data (scaled values)
             aes(x = gsr, y = hpt, colour = ID,shape=ID)) +
  geom_line(aes(y = exp(predict(mod2))),size=1) 
print(p)

