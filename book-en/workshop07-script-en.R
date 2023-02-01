##Section: 01-preparing-for-the-workshop.R 

###Notice ###
#                                                                            #
#This is an automatically generated script based on the code chunks from the #
#book for this workshop.                                                     #
#                                                                            #
#It is minimally annotated to allow participants to provide their comments:  #
#a practice that we highly encourage.                                        #
#                                                                            #
#Note that the solutions to the challenges are also included in this script. #
#When solving the challenges by yourself, attempt to not scroll and peek at  #
#the solutions.                                                              #
#                                                                            #
#Happy coding!                                                               #


install.packages(c('lme4',
                   'MASS',
                   'vcdExtra', 
                   'bbmle',
                   'MuMIn',
                   'ggplot2',
                   'DescTools',
                   'remotes',
                   'gridExtra',
                   'lattice'),
                 repos = "http://cran.us.r-project.org",
                 quiet = TRUE)

library(lme4)
library(MASS)
library(vcdExtra)
library(bbmle)
library(MuMIn)
library(ggplot2)
library(DescTools)
library(remotes)
library(gridExtra)
library(lattice)


##Section: 02-introduction-en.R 

# Load the dataset
fish.data <- read.csv('data/qcbs_w7_data.csv', stringsAsFactors = TRUE) 
# This line will vary depending on where your data is saved. 
# Check your working directory with getwd() and change it, if needed, with setwd()

# Simple theme for all ggplot figures we produce after this
fig <- theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_blank(), 
        strip.background=element_blank(), 
        strip.text.y = element_text(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        panel.border = element_rect(colour="black", fill = NA))


# Exploring the data graphically!

# Basic plot aesthetics for the relationship we care about
plot <- ggplot(aes(Fish_Length, Trophic_Pos), data = fish.data)

# Plot 1 - All data
plot + geom_point() + 
  labs(x = "Length (mm)", y = "Trophic Position", 
       title = "All Data") + 
  fig

# Plot 2 - By species
plot + geom_point() + 
  facet_wrap(~ Fish_Species) + 
  labs(x = "Length (mm)", y = "Trophic Position", 
       title = "By Species") + 
  fig

# Plot 3 – By lake
plot + geom_point() + 
  facet_wrap(~ Lake) + 
  labs(x = "Length (mm)", y = "Trophic Position", 
       title = "By Lake") + 
  fig


##Section: 03-data-exploration.R 




##Section: 04-implement-LMM.R 

# Load data in again
fish.data <- read.csv('data/qcbs_w7_data.csv')

# Look at data structure
str(fish.data)

# Look at the distribution of samples for each factor
table(fish.data[ , c("Lake", "Fish_Species")])

# Look at the distribution of continuous variables:
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
hist(fish.data$Fish_Length, xlab = "Length (mm)", main = "")
hist(fish.data$Trophic_Pos, xlab = "Trophic position", main = "")

# Standardized length, "by hand"
fish.data$Z_Length <- (fish.data$Fish_Length - mean(fish.data$Fish_Length)) / 
                      sd(fish.data$Fish_Length)

# Standardized trophic position, with the function scale
fish.data$Z_TP     <- scale(fish.data$Trophic_Pos)


lm.test <- lm(Z_TP ~ Z_Length, data = fish.data)

lm.test.resid <- rstandard(lm.test)

par(mfrow = c(1, 2))

plot(lm.test.resid ~ as.factor(fish.data$Fish_Species),
     xlab = "Species", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test.resid ~ as.factor(fish.data$Lake),
     xlab = "Lake", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

# Challenge 4: Can you re-write this code so that the slopes between trophic position and body length vary by lake and species?
lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

# Challenge 4 solution:
lmer(Z_TP ~ Z_Length + (1 + Z_Length | Lake) + (1 + Z_Length | Fish_Species),
     data = fish.data, REML = TRUE)

# Challenge 5: Make a list of 7 alternative models that could be compared to this initial model:
lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

# Challenge 5 solution: Basic linear model
M0 <- lm(Z_TP ~ Z_Length, data = fish.data)

# Challenge 5 solution, other potential models
# Note that REML = FALSE in order to compare with the basic linear model where estimation method = ML

# Basic linear model / Linear model with no random effects
M0 <- lm(Z_TP ~ Z_Length, data = fish.data)
# Full model with varying intercepts
M1 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species) + (1 | Lake), 
           data = fish.data, REML = FALSE)
# Full model with varying intercepts and slopes
M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
           data = fish.data, REML = FALSE)
# No Lake, varying intercepts only
M3 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species), data = fish.data, REML = FALSE)
# No Species, varying intercepts only
M4 <- lmer(Z_TP ~ Z_Length + (1 | Lake), data = fish.data, REML = FALSE)
# No Lake, varying intercepts and slopes
M5 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species), 
           data = fish.data, REML = FALSE)
# No Species, varying intercepts and slopes
M6 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Lake), data = fish.data, REML = FALSE)
# Full model with varying intercepts and slopes only varying by lake
M7 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species) + (1 + Z_Length | Lake),
           data = fish.data, REML = FALSE)
# Full model with varying intercepts and slopes only varying by species
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, REML = FALSE)

# Find AICc value for our first model (Basic linear model) using the package MuMIn
MuMIn::AICc(M1)

# To group all AICc values into a single table, we can use MuMIn::model.sel() to calculate AICc for each model (along with other outputs)
AIC.table  <- MuMIn::model.sel(M0, M1, M2, M3, M4, M5, M6, M7, M8)

# Then we can select only the columns of interest to print into a table 
# `df` is the degree of freedom
# `logLik` is the loglikelihood
# `delta` is the AICc difference with the lowest value
(AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")])

# For more information on the other outputs/results returned by the function `model.sel()`, see `?model.sel`.

# Let's take a closer look at M8 and M2. We can exclude other model because they have such higher AICc
# Because we are comparing two mixed effect models, we can set `REML = TRUE` when generating M8 and M2
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, REML = TRUE)

M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
           data = fish.data, REML = TRUE)
# Now let's print a table in order to compare M2 and M8 
MuMIn::model.sel(M2,M8)[ , c("df", "logLik", "AICc", "delta")]

# Let's take a look at the best model again, what is it's structure?
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, REML = FALSE)
# Both the intercepts and slopes of the relationship between trophic position and length may vary by fish species, but only the intercepts may vary by lake.

# Once the best model is selected, the estimation method must be reset to `REML = TRUE`.
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, REML = TRUE)

# Plot predicted values vs residual values
par(mar=c(4,4,.5,.5))
plot(resid(M8) ~ fitted(M8), 
     xlab = 'Predicted values', 
     ylab = 'Normalized residuals')
abline(h = 0, lty = 2)
# Homogeneous dispersion of the residuals means that the assumption is respected.

# In order to check the independence of the model residuals we need to plot residuals vs each covariate of the model
par(mfrow = c(1,3), mar=c(4,4,.5,.5))

plot(resid(M8) ~ fish.data$Z_Length, 
     xlab = "Length", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(M8) ~ Fish_Species, data = fish.data, 
        xlab = "Species", ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(M8) ~ Lake, data = fish.data, 
        xlab = "Lakes", ylab = "Normalized residuals")
abline(h = 0, lty = 2)
# Homogeneous dispersion of the residuals around 0 means no pattern of residuals depending on the variable, therefore the assumption is respected!
# Note: The clusters are due to the data structure, where fish of only 5 size classes (large, small, and three groups in between) were captured.

# Check the normality of the model residuals as residuals following a normal distribution indicate that the model is not biased.
hist(resid(M8))
# The residuals are normal! This means our model is not biased.

# Now we are ready for interpretation and visualization
# Let's take a closer look at our final model using the `summary()` function. 
(summ_M8 <- summary(M8))

# Challenge 7: *Is it possible to visualize graphically the different intercepts and slopes of the model to better interpret the results?

# Challenge 7 solution: Yes! We could do this by generating the following figures.
# a) A figure with all data grouped
# b) A figure by species
# c) A figure by lake

# To produce these figures, first we need the coefficients of the full model that are in the model summary.
summ_M8$coefficients
# Intercept = Intercept = 9.0589745 × 10^4
# Slope = 0.4222697

# We also need the coefficients for each level of the model, which can be obtained with the `coef` function
coef(M8)

# Now let's make our figures!

# a) Figure with all data grouped
# Create a simplified ggplot theme
fig <- theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_blank()) +
  theme(strip.background=element_blank(),
        strip.text.y = element_text()) +
  theme(legend.background=element_blank()) +
  theme(legend.key=element_blank()) +
  theme(panel.border = element_rect(colour="black", fill=NA))

plot <- ggplot(aes(Z_Length, Z_TP), data = fish.data)
Plot_AllData <- plot + geom_point() +
  xlab("Length (mm)") + 
  ylab("Trophic position") +
  labs(title = "All data") + fig

Plot_AllData + geom_abline(intercept = summ_M8$coefficients[1,1], 
                           slope     = summ_M8$coefficients[2,1])

# You could also write out the numbers like this:
# Plot_AllData + geom_abline(intercept = -0.0009059, slope = 0.4222697)

# b) Figure by species
# Create a table with the coefs to facilitate their manipulation
Lake.coef              <- coef(M8)$Lake
colnames(Lake.coef)    <- c("Intercept", "Slope")
Species.coef           <- coef(M8)$Fish_Species
colnames(Species.coef) <- c("Intercept", "Slope")

Plot_BySpecies <- plot + 
  geom_point(aes(colour = factor(Fish_Species)), size = 4) +
  xlab("Length (mm)") + ylab("Trophic position") +
  labs(title = "By species") + fig

# Add regression lines for each species
Plot_BySpecies +
  geom_abline(intercept = Species.coef[1,1], 
              slope     = Species.coef[1,2], col = "coral2") +
  geom_abline(intercept = Species.coef[2,1], 
              slope     = Species.coef[2,2], col = "green4") +
  geom_abline(intercept = Species.coef[3,1], 
              slope     = Species.coef[3,2], col = "blue1")


# c) Figure by lake
Plot_ByLake <- plot + 
  geom_point(aes(colour = factor(Lake)), size = 4) +
  xlab("Length (mm)") + ylab("Trophic Position") +
  labs(title = "By Lake") + fig

# Add in regression lines with the intercepts specific to each lake
Plot_ByLake +
  geom_abline(intercept = Lake.coef[1,1], 
              slope     = Lake.coef[1,2], col = "coral2") +
  geom_abline(intercept = Lake.coef[2,1], 
              slope     = Lake.coef[2,2], col = "khaki4") +
  geom_abline(intercept = Lake.coef[3,1], 
              slope     = Lake.coef[3,2], col = "green4") +
  geom_abline(intercept = Lake.coef[4,1], 
              slope     = Lake.coef[4,2], col = "darkgoldenrod") +
  geom_abline(intercept = Lake.coef[5,1], 
              slope     = Lake.coef[5,2], col = "royalblue1") +
  geom_abline(intercept = Lake.coef[6,1], 
              slope     = Lake.coef[6,2], col = "magenta3")


lmer(Biodiv ~ Productivity + (1 | Forest / Site))


##Section: 05-introducing-GLMM.R 

dat.tf <- read.csv("data/banta_totalfruits.csv")

# In this dataset, the column headers are defined as:
# popu factor with a level for each population
# gen factor with a level for each genotype
# nutrient factor with levels for low (value = 1) or high (value = 8)
# amd factor with levels for no damage or simulated herbivory
# total.fruits integer indicating the number of fruits per plant

#Before we go any further, we need to select an error distribution. This choice will be informed by the structure of our data. 
# Our response variable is count data which suggests we need a Poisson distribution (i.e. the variance is equal to the mean).
hist(dat.tf$total.fruits, breaks = 50, col = 'blue', main = '',
     xlab = 'Total fruits', ylab = 'Count')

# Let's explore the variance within our data
# Create new variables that represent every combination of variables
dat.tf <- within(dat.tf,
{
  # genotype x nutrient x clipping
  gna <- interaction(gen,nutrient,amd)
  gna <- reorder(gna, total.fruits, mean)
  # population x nutrient x clipping
  pna <- interaction(popu,nutrient,amd)
  pna <- reorder(pna, total.fruits, mean)
})

# Boxplot of total fruits vs genotype x nutrient x clipping interaction
ggplot(data = dat.tf, aes(factor(x = gna), y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", outlier.shape = 21,
  outlier.colour = "skyblue2") +
  ylab("log (Total fruits)\n") + # \n creates a space after the title
  xlab("\nGenotype x nutrient x clipping") + # space before the title
  theme_bw() + theme(axis.text.x = element_blank()) +
  stat_summary(fun = mean, geom = "point", colour = "red")


# Run a diagnostic lot of the group variances vs group means (genotype x nutrient x clipping grouping). 
# Code used to produce the plot : https://github.com/QCBSRworkshops/workshop07/blob/main/pres-fr/data/glmm_e.r
# Substantial variation among the sample variances on the transformed data
# For example, among genotypes:
grpVars <- tapply(dat.tf$total.fruits, dat.tf$gna, var)

grpMeans <- tapply(dat.tf$total.fruits,dat.tf$gna, mean)

# Quasi-Poisson
lm1 <- lm(grpVars~grpMeans-1) 
phi.fit <- coef(lm1)
# The -1 specifies a model with the intercept set to zero

# Negative binomial
lm2 <- lm(grpVars ~ I(grpMeans^2) + offset(grpMeans)-1)
k.fit <- 1/coef(lm2)
# The offset() is used to specify that we want the group means added as a term with its coefficient fixed to 1

# Non-parametric loess fit
Lfit <- loess(grpVars~grpMeans)

# The plot
plot(grpVars ~ grpMeans, xlab = "Group means", ylab = "Group variances" )
abline(a = 0, b = 1, lty = 2)
text(105,500, "Poisson")
curve(phi.fit*x, col = 2, add = TRUE)
# bquote() is used to substitute numeric values in equations with symbols
text(110,3900,
     bquote(paste("QP: ", sigma^2==.(round(phi.fit,1))*mu)), col = 2)
curve(x*(1+x/k.fit), col = 4, add = TRUE)
text(104,7200, paste("NB: k = ", round(k.fit, 1), sep = ""), col = 4)
mvec <- 0:120
lines(mvec, predict(Lfit, mvec), col = 5)
text(118, 2000, "loess", col = 5)

# Poisson GLMM
# Given the mean-variance relationship, we will most likely need a model with over-dispersion.
# To understand why, let's start with a Poisson model.
mp1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
             (1|popu)+
             (1|gen),
             data = dat.tf, family = "poisson")

# Download the glmm_funs.R code from the wiki page and source it to run the function
source(file = "data/glmm_funs.R") # This line will vary depending on where your data is saved
# Over-dispersion check
overdisp_fun(mp1)
# Ratio is significantly > 1

# Negative binomial GLMM using the function glmer.nb()
mnb1 <- glmer.nb(total.fruits ~ nutrient*amd + rack + status +
                 (1|popu)+
                 (1|gen),
                 data = dat.tf,
                 control = glmerControl(optimizer = "bobyqa"))
# Control argument specifies the way we optimize the parameter values

# Over-dispersion check
overdisp_fun(mnb1)
# Ratio is now much closer to 1 although p < 0.05

# Poisson-lognormal GLMM

# This variable is already in your data "dat.tf", but here is how we create it:
dat.tf$X <- 1:nrow(dat.tf)

# Account for over-dispersion
mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
              (1|X) +
              (1|popu)+
              (1|gen),
data = dat.tf, family = "poisson",
control = glmerControl(optimizer = "bobyqa"))

# Over-dispersion check
overdisp_fun(mpl1)
# Ratio now meets our criterion, thus, < 1

# popu only
mpl1.popu <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                     (1|X) +
                     (1|popu), 
                     data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))
 
# gen only
mpl1.gen <-glmer(total.fruits ~ nutrient*amd + rack + status + 
                   (1|X) +
                   (1|gen), 
                   data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))
 
# IC approach using AICc
ICtab(mpl1, mpl1.popu, mpl1.gen, type = c("AICc"))

# Frequentist approach using LRT
anova(mpl1,mpl1.popu)

anova(mpl1,mpl1.gen)

if (!require("coefplot2"))
  remotes::install_github("palday/coefplot2", subdir = "pkg")
library(coefplot2)

# Variance terms
coefplot2(mpl1, ptype = "vcov", intercept = TRUE, main = "Random effect variance")

# Fixed effects
coefplot2(mpl1, intercept = TRUE, main = "Fixed effect coefficient")

# dotplot code
pp <- list(layout.widths = list(left.padding = 0, right.padding = 0),
           layout.heights = list(top.padding = 0, bottom.padding = 0))
r2 <- ranef(mpl1, condVar = TRUE)
d2 <- dotplot(r2, par.settings = pp)

grid.arrange(d2$gen, d2$popu, nrow = 1)


##Section: 06-model-selection.R 

mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
              (1|X) +
              (1|popu)+
              (1|gen),
data = dat.tf, family = "poisson",
control = glmerControl(optimizer = "bobyqa"))

mpl2 <- update(mpl1, . ~ . - rack) # model without rack
mpl3 <- update(mpl1, . ~ . - status) # model without status
mpl4 <- update(mpl1, . ~ . - amd:nutrient) # without amd:nutrient interaction

aic_tab  <- MuMIn::model.sel(mpl1, mpl2, mpl3, mpl4)
(round(aic_table <- aic_tab[ , c("AICc", "delta", "df")], digits = 2))

dd_LRT <- drop1(mpl1, test = "Chisq")
(dd_AIC <- dfun(drop1(mpl1)))

mpl2 <- update(mpl1, . ~ . - amd:nutrient)

mpl3 <- update(mpl2, . ~ . - rack) # pas de rack ou d'interaction
mpl4 <- update(mpl2, . ~ . - status) # pas de status ou d'interaction
mpl5 <- update(mpl2, . ~ . - nutrient) # pas de nutrient ou d'interaction
mpl6 <- update(mpl2, . ~ . - amd) # pas d'herbivorie ou d'interaction

aic_tab2  <- MuMIn::model.sel(mpl2, mpl3, mpl4, mpl5, mpl6)
(round(aic_table2 <- aic_tab2[ , c("AICc", "delta", "df")], digits = 2))

dd_LRT2 <- drop1(mpl2, test = "Chisq")
dd_AIC2 <- dfun(drop1(mpl2))

# inverts <- read.csv('data/inverts.csv', header = TRUE)
# head(inverts)
# table(inverts$temp, inverts$feeding.type)

# mod.glm <- glm(PLD ~ temp + feeding.type, family = poisson(), data = inverts)
# summary(mod.glm)
# drop1(mod.glm, test = "Chisq")

# boxplot(PLD ~ temp,  data = inverts)
# boxplot(PLD ~ feeding.type,  data = inverts)

# boxplot(predict(mod.glm, type = "response")~inverts$temp)

# plot()

# modglm <- glm(PLD ~ temp + feeding.type, family = poisson(), data = inverts)

# r2 <- ranef(mpl1, condVar = TRUE)
# d2 <- dotplot(r2, par.settings = pp)

# plot(aggregate(PLD ~ taxon, FUN = mean, data = inverts)[,2], aggregate(PLD ~ taxon, FUN = var, data = inverts)[,2], pch = 19)
# abline(a = 0, b = 1, lty = 2)

# mod.glmer <- glmer.nb(PLD ~ temp + feeding.type + (1|taxon), data = inverts)
# mod.glm <- glm.nb(PLD ~ temp + feeding.type, family = poisson(), data = inverts)

# plot(aggregate(PLD ~ taxon, FUN = var, data = inverts)[,2], aggregate(PLD ~ taxon, FUN = mean, data = inverts)[,2])
# abline(a = 0, b = 1, lty = 2 )


##Section: 07-additional-resources.R 




##Section: 08-references-en.R 




