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


## lmer(Biodiv ~ Productivity + (1 | Forest / Site))
