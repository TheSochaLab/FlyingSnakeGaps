library(lme4)
library(ggplot2)
library(ggeffects)

filepath = paste(getwd(),"Summary Datasets/bdata.csv", sep="/")
bdata <- read.csv(filepath)
bdata$ID <- as.factor(bdata$ID) #make sure models treat ID as a factor, not a number

bdata1 = bdata[!is.na(bdata$dist),] #skip nans

#check response variable distribution
hist(bdata1$dist) #somewhat bimodal

## RANDOM EFFECT DIAGNOSTICS
#random intercepts model  - dist
mixed.lmer <- lmer(dist ~ gs_bin + (1|ID), data = bdata1, REML = TRUE)
plot(mixed.lmer) #looks fine, maybe a little less distributed at end

#random slopes: is singular
mixed.lmer2 <- lmer(dist ~ gs_bin + (gs_bin|ID), data = bdata1, REML = TRUE)

summary(mixed.lmer)
confint(mixed.lmer)

# PLOTS
#plot 1: by individual, dist
(mm_plot <- ggplot(bdata1, aes(x = gs_bin, y = dist)) +
    facet_wrap(~ID, nrow=2) +   # a panel for each snake
    geom_point(alpha = 0.5) +
    xlab('Gap Size (%SVL)')+
    ylab('Distance traveled from low point to landing (%SVL)')+
    theme_classic() +
    geom_line(data = cbind(bdata1, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)


#plot 2: full thingy
pred.mm <- ggpredict(mixed.lmer, terms = c("gs_bin"))  # this gives overall predictions for the model

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = bdata1,                      # adding the raw data (scaled values)
               aes(x = gs_bin, y = dist, colour = ID)) + 
    labs(x = "Gap Size (%SVL)", y = "Distance Traveled (%SVL)", 
         title = "Snakes tailor their jump distance to the gap size") + 
    theme_minimal()
)

#color by individual
p <- ggplot(bdata1, aes(x = gs_bin, y = dist, colour = ID)) +
    geom_point(data = bdata1,                      # adding the raw data (scaled values)
               aes(x = gsr, y = dist, colour = ID,shape=ID)) +
    geom_line(aes(y = predict(mixed.lmer)),size=1) 
print(p)
