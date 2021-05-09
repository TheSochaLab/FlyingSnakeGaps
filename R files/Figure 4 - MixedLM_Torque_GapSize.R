library(lme4)
library(ggplot2)
library(ggeffects)

#load the data
filepath = paste(getwd(),"Summary Datasets/tdata.csv", sep="/")
tdata <- read.csv(filepath)
tdata$ID <- as.factor(tdata$ID) #make sure models treat ID as a factor, not a number

#check response variable distribution
hist(tdata$TNorm) #left skewed

#transform data
tdata$logT <- log(tdata$TNorm)
hist(tdata$logT) #better

#random intercepts model 
mixed.lmer <- lmer(TNorm ~ gs_bin + (1|ID), data = tdata, REML = TRUE)
summary(mixed.lmer)
plot(mixed.lmer) #Cone of variance.

mixed.lmer1 <- lmer(logT ~ gs_bin + (1|ID), data = tdata, REML = TRUE)
summary(mixed.lmer1)
plot(mixed.lmer1) #WAY better

#random slopes, fails to converge
mixed.ranslope <- lmer(logT ~ gs_bin + (1+gs_bin|ID), data = tdata, REML = TRUE)

#check for singularity
tt <- getME(mixed.ranslope,"theta")
ll <- getME(mixed.ranslope,"lower")
min(tt[ll==0]) #not the issue.

#Try restarting from previous fit 
#Restart didn’t converge in 10000 evals, so increase iterations.
ss <- getME(mixed.ranslope,c("theta","fixef"))
m2 <- update(mixed.ranslope,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4))) #fit becomes singular, suggesting reducing model complexity appropriate.

summary(mixed.lmer1)
confint(mixed.lmer1) #to see strenght of gap size effect

### PLOTS ###
#plot of full model
#plot 1: by individual, untransformed data
mm_plot <- ggplot(tdata, aes(x = gs_bin, y = TNorm)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(tdata, pred = exp(predict(mixed.lmer1))), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
print(mm_plot)

#plot 2: full graph, transformed data
pred.mm <- ggpredict(mixed.lmer1, terms = c("gs_bin"))  # this gives overall predictions for the model

#plot 2a: summary
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = tdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = logT, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Log(Normalized Transition Torque)", 
         title = "Log(Transition torque) decreases with gap size") + 
    theme_minimal()
)

#plot 2b: individual lines
p <- ggplot(tdata, aes(x = gs_bin, y = logT, colour = ID)) +
    geom_point(data = tdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = logT, colour = ID,shape=ID)) +
    geom_line(aes(y = predict(mixed.lmer1)),size=1) 
print(p)

#Plot 3a: untransformed data, summary
(ggplot(pred.mm) + 
        geom_line(aes(x = x, y = exp(predicted))) +          # slope
        geom_point(data = tdata,                      # adding the raw data (scaled values)
                   aes(x = gsr, y = TNorm, colour = ID)) + 
        labs(x = "Gap Size (%SVL)", y = "Normalized Transition Torque", 
             title = "Transition torque decreases with gap size") + 
        theme_minimal()
)
#Plot 3b: untransformed data, individual lines
p <- ggplot(tdata, aes(x = gs_bin, y = TNorm, colour = ID)) +
    geom_point(data = tdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = TNorm, colour = ID,shape=ID)) +
    geom_line(aes(y = exp(predict(mixed.lmer1))),size=1) 
print(p)



