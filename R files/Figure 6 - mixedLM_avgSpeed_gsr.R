library(lme4)
library(ggfortify)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(viridis)
library(ggthemes)
library(ggalt)
library(car)

filepath = paste(getwd(),"Summary Datasets/vdata.csv", sep="/")
vdata <- read.csv(filepath)
vdata$ID <- as.factor(vdata$ID) #make sure models treat ID as a factor, not a number

#check response variable distribution
hist(vdata$sav) #heavily right skewed, all positive. Examine diagnostic plots.

vdata$log.gs = log(vdata$gs_bin)
vdata$log.sav = log(vdata$sav)

##log-log transform improves normality##
gather(vdata, variable, value, sav, gs_bin, log.sav, log.gs) %>%
mutate(variable = factor(variable, levels = c("sav", 
                                              "log.sav", 
                                              "gs_bin", 
                                              "log.gs"))) %>%
ggplot(aes(x = value, fill = variable)) +
  geom_bkde() +
  geom_rug() +
  scale_fill_viridis(guide = FALSE, discrete = TRUE) +
  facet_wrap(~variable, scales = "free") +
  theme_base()

## relationship does not appear linear ##
ggplot(vdata, aes(x = gsr, y = sav)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = viridis(1, begin = 1),   se = FALSE) +
  geom_smooth(span   = 1,    color = viridis(1, begin = 0.6), se = FALSE, linetype =
                "dashed") +
  theme_base()

## relationship between logged variables does appear more linear, slope variation perhaps due to random factors##
ggplot(vdata, aes(x = log.gs, y = log.sav, group)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", color = viridis(1, begin = 1),   se = FALSE) +
  geom_smooth(span   = 1,    color = viridis(1, begin = 0.6), se = FALSE, 
              linetype = "dashed") +
  theme_base()

#mixed model - random intercepts
log.log.mm <- lmer(log.sav ~ log.gs + (1|ID), data = vdata, REML = TRUE)
plot(log.log.mm)
qqnorm(residuals(log.log.mm))  #no obvious issues with residuals


#mixed model - random random slopes
log.log.ran <- lmer(log.sav ~ log.gs + (log.gs | ID), data = vdata, REML=TRUE) #fails to converge.

#check for singularity
tt <- getME(log.log.ran,"theta")
ll <- getME(log.log.ran,"lower")
min(tt[ll==0])

#Try restarting from previous fit 
#Restart didn’t converge in 10000 evals, so increase iterations.
ss <- getME(log.log.ran,c("theta","fixef"))
m2 <- update(log.log.ran,start=ss,control=lmerControl(optCtrl=list(maxfun=2e4))) #still does not converge.

#try different optimizer
m3 <- update(log.log.ran,start=ss,control=lmerControl(optimizer="bobyqa",
                                                      optCtrl=list(maxfun=2e5))) #converges

plot(m3)
qqnorm(residuals(m3)) #no obvious residual issues.

#anova between the two dist models to determine if random slopes improves the model
anova(log.log.mm, m3,refit=FALSE)
#random slopes do improve the model fit (AIC = 7.01)

summary(m3)
confint(m3)


##PLOTS
#plot 1: by individual, log-log scale
(mm_plot <- ggplot(vdata, aes(x = log.gs, y = log.sav)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    xlab('Log of Gap Size (%SVL)')+
    ylab('Log of Avg Speed (SVL/s)')+
    theme_classic() +
    geom_line(data = cbind(vdata, pred = predict(m3)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

#plot 2: by individual, regular scale
mm_plot <- ggplot(vdata, aes(x = gs_bin, y = sav)) +
  facet_wrap(~ID, nrow=2) +   # a panel for each snake
  geom_point(alpha = 0.5) +
  xlab('Gap Size (%SVL)') +
  ylab('Average Speed (SVL/s)') +
  theme_classic() +
  geom_line(data = cbind(vdata, pred = exp(predict(m3))), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
print(mm_plot)

#plot 3: summary, regular scale
pred.mm <- ggpredict(m3, terms = c("log.gs"))  # this gives overall predictions for the model

(ggplot(pred.mm) + 
    geom_line(aes(x = exp(x), y = exp(predicted))) +          # slope
    geom_point(data = vdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = sav, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Avg Speed (SVL/s)") + 
    theme_minimal()
)

#Plot: untransformed data, individual lines
p <- ggplot(vdata, aes(x = gs_bin, y = sav , colour = ID)) +
  geom_point(data = vdata,                      # adding the raw data (scaled values)
             aes(x = gsr, y = sav, colour = ID,shape=ID)) +
  geom_line(aes(y = exp(predict(m3))),size=1) 
print(p)
