library(lme4)
library(ggplot2)
library(ggeffects)

#load the data
filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number

#create column for relative data
bdata$relYvar <- (bdata$avg_ydev/10)/bdata$svl*100 #change mm to cm, then relative to SVL
bdata$relZvar <- (bdata$avg_zdev/10)/bdata$svl*100 #change mm to cm, then relative to SVL

#check response variable distribution
hist(bdata$relYvar) # slightly right skewed; try sqrt transformation
bdata$sqrtY = sqrt(bdata$relYvar)
hist(bdata$sqrtY)

hist(bdata$relZvar) #very right skewed; try log-log
bdata$logZ = log(bdata$relZvar)
bdata$logG = log(bdata$gs_bin)

## RANDOM EFFECT DIAGNOSTICS
#random intercepts model  - Z
mixed.lmer <- lmer(logZ ~ logG + (1|ID), data = bdata, REML = TRUE)
plot(mixed.lmer) #not ideal but not terrible?

#random intercepts model  - Y
mixed.lmer1 <- lmer(sqrtY ~ gs_bin + (1|ID), data = bdata, REML = TRUE)
plot(mixed.lmer1) #no obvious pattern.

#random slopes
mixed.lmer2 <- lmer(logZ ~ logG + (logG|ID), data = bdata, REML = TRUE) #model failrs to converge.

#check for singularity
tt <- getME(mixed.lmer2,"theta")
ll <- getME(mixed.lmer2,"lower")
min(tt[ll==0]) #not that close to 0. 

#Try restarting from previous fit 
#Restart didn’t converge in 10000 evals, so increase iterations.
ss <- getME(mixed.lmer2,c("theta","fixef"))
m2 <- update(mixed.lmer2,start=ss) #still does not converge. 

#Try another optimizer. 
m3 <- update(mixed.lmer2,start=ss,control=lmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5))) #converges :) 


mixed.lmer3 <- lmer(sqrtY ~ gs_bin + (gs_bin|ID), data = bdata, REML = TRUE) #model does not converge

#check for singularity
tt <- getME(mixed.lmer3,"theta")
ll <- getME(mixed.lmer3,"lower")
min(tt[ll==0]) #not that close to 0. 

#Try restarting from previous fit 
#Restart didn’t converge in 10000 evals, so increase iterations.
ss <- getME(mixed.lmer3,c("theta","fixef"))
m2 <- update(mixed.lmer3,start=ss) #model becomes singular. Use reduced model.  

#mixed.lmer1 for y, m3 for Z
summary(mixed.lmer1)
summary(m3)
confint(mixed.lmer1) #does not include 0 for FE estimate
confint(m3) #does not include 0 for FE estimate


# PLOTS
#plot 1: by individual
(mm_plot <- ggplot(bdata, aes(x = gs_bin, y = relZvar)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    theme_classic() +
    xlab('Gap Size, %SVL') +
    ylab("Vertical Variation, %SVL") +
    geom_line(data = cbind(bdata, pred = exp(predict(m3))), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

(mm_plot <- ggplot(bdata, aes(x = gs_bin, y = relYvar)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    theme_classic() +
    xlab('Gap Size, %SVL') +
    ylab('Horizontal Variation, %SVL') +
    geom_line(data = cbind(bdata, pred = predict(mixed.lmer1)**2), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


#plot 2: full thingy
pred.mm <- ggpredict(m3, terms = c("logG"))  # this gives overall predictions for the model

(ggplot(pred.mm) + 
    geom_line(aes(x = exp(x), y = exp(predicted))) +          # slope
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gs_bin, y = relZvar, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Vertical Variation (%SVL)", 
         title = "Average Z Devation increases with Gap Size") + 
    theme_minimal()
)

pred1.mm <- ggpredict(mixed.lmer1, terms = c("gs_bin"))  # this gives overall predictions for the model

(ggplot(pred1.mm) + 
    geom_line(aes(x = x, y = predicted**2)) +          # slope
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gs_bin, y = relYvar, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Horizontal Variation (%SVL)", 
         title = "Average Y Devation increases with Gap Size") + 
    theme_minimal()
)
#Plot 3b: untransformed data, individual lines
p <- ggplot(bdata, aes(x = gs_bin, y = relZvar, colour = ID)) +
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = relZvar, colour = ID,shape=ID)) +
    geom_line(aes(y = exp(predict(m3))),size=1) 
print(p)

p <- ggplot(bdata, aes(x = gs_bin, y = relYvar, colour = ID)) +
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = relYvar, colour = ID,shape=ID)) +
    geom_line(aes(y = predict(mixed.lmer1)**2),size=1) 
print(p)
