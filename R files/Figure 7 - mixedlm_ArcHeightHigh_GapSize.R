library(lme4)
library(ggplot2)
library(ggeffects)

filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata = bdata[!is.na(bdata$ahh),]
bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number

#check response variable distribution
hist(bdata$ahh) # very normal

## RANDOM EFFECT DIAGNOSTICS
#random intercepts model  - ahh
mixed.lmer <- lmer(ahh ~ gs_bin + (1|ID), data = bdata, REML = TRUE)
summary(mixed.lmer)
plot(mixed.lmer) #no pattern
qqPlot(resid(mixed.lmer)) 

#random slopes, is singular.
mixed.lmer2 <- lmer(ahh ~ gs_bin + (gs_bin|ID), data = bdata, REML = TRUE)

confint(mixed.lmer)

# PLOTS
#plot 1: by individual
(mm_plot <- ggplot(bdata, aes(x = gs_bin, y = ahh)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(bdata, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
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
               aes(x = gs_bin, y = ahh, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Arc Height (high)", 
         title = "Arc height (high point) increases with Gap Size") + 
    theme_minimal()
)

#color by individual
p <- ggplot(bdata, aes(x = gs_bin, y = ahh, colour = ID)) +
    geom_point(data = bdata,                      # adding the raw data (scaled values)
               aes(x = gsr, y = ahh, colour = ID,shape=ID)) +
    geom_line(aes(y = predict(mixed.lmer)),size=1) 
print(p)
