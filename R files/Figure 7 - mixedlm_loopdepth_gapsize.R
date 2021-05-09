library(lme4)
library(ggplot2)
library(ggeffects)

filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata = bdata[!is.na(bdata$ldl),] #skip nans
bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number

#check response variable distribution
hist(bdata$ldl) # kind of left weighted
hist(bdata$ldh) #very normal looking

## RANDOM EFFECT DIAGNOSTICS
#random intercepts model  - ldl
mixed.lmer <- lmer(ldl ~ gs_bin + (1|ID), data = bdata, REML = TRUE)
plot(mixed.lmer) #maybe slight negative pattern

#random intercepts model  - ldh
mixed.lmer1 <- lmer(ldh ~ gs_bin + (1|ID), data = bdata, REML = TRUE)
plot(mixed.lmer1) #no pattern

#random slopes for LDL, fails to converge
mixed.lmer2 <- lmer(ldl ~ gs_bin + (gs_bin|ID), data = bdata, REML = TRUE)
#Try restarting from previous fit 
#Restart didn’t converge in 10000 evals, so increase iterations.
ss <- getME(mixed.lmer2,c("theta","fixef"))
m2 <- update(mixed.lmer2,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4))) #does not converge.
#try a different optimizer.
m3 <- update(mixed.lmer2,start=ss,control=lmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5))) #converges

#random slopes for LDH, fails to converge.
mixed.lmer3 <- lmer(ldh ~ gs_bin + (gs_bin|ID), data = bdata, REML = TRUE)
#Try restarting from previous fit 
#Restart didn’t converge in 10000 evals, so increase iterations.
ss <- getME(mixed.lmer3,c("theta","fixef"))
m4 <- update(mixed.lmer3,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4))) #converges :)

#anova between the two models to determine if random slopes improves the model
anova(mixed.lmer,m3,refit=FALSE)
#Random slopes does not improve (delta AIC < 2)
anova(mixed.lmer1,m4,refit=FALSE)
#Random slopes does not improve (delta AIC < 2)

summary(mixed.lmer1)
summary(mixed.lmer)

confint(mixed.lmer1)
confint(mixed.lmer)

# PLOTS
#plot 1: by individual
(mm_plot <- ggplot(bdata, aes(x = gs_bin, y = ldl)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(bdata, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

(mm_plot <- ggplot(bdata, aes(x = gs_bin, y = ldh)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(bdata, pred = predict(mixed.lmer1)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


#plot 2: full thingy
pred.mm <- ggpredict(mixed.lmer, terms = c("gs_bin"))  # this gives overall predictions for the model

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gs_bin, y = ldl, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Loop Depth (low)", 
         title = "Loop Depth (low) increases with Gap Size") + 
    theme_minimal()
)

pred1.mm <- ggpredict(mixed.lmer1, terms = c("gs_bin"))  # this gives overall predictions for the model

(ggplot(pred1.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gs_bin, y = ldh, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Loop Dept (High)", 
         title = "Loop Depth  (High) has no relationship with Gap Size") + 
    theme_minimal()
)

#ldl with individual lines
p <- ggplot(bdata, aes(x = gs_bin, y = ldl, colour = ID)) +
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = ldl, colour = ID,shape=ID)) +
    geom_line(aes(y = predict(mixed.lmer)),size=1) 
print(p)
