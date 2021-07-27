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
