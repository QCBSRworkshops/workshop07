# QCBS R Workshop Series
# Linear Mixed Models
#Designed by Jacob Ziegler, Dalal Hanna and Catherine Baltazar

################Section 1#########################

# Remove prior commands in R
rm(list=ls()) 

# Place all workshop material in one folder on your computer
# Run the following line of code and use the browsing window to choose the QCBS_W6_Data.csv 
# file in the folder that contains the workshop material
file.choose()

# Set the working directoy to the folder which contains the lab material by copy and pasting
# all but the R file name from the output of the file.choose() command into the set working 
# directory command. 

# For example paste "/Users/ziegljac/Documents/QCBS_R/" -> include the quotations
# NOT "/Users/ziegljac/Documents/QCBS_R/Get_Data_Func.R" -> include the quotations
setwd()

# Load useful libraries and data
#Note: if you have never loaded these libraries before you will have to use 
#the "install.packages" function before the "library" function
library(ggplot2)
library(lme4)
library(arm)
library(AICcmodavg)
library(beepr)
#AICc is for small sample size corrected
#Always use this because the equation is set that the bigger the sample size gets the more it's just like AIC

data <- read.csv('qcbs_w6_data.csv')

# Used to strip down figures to make them simpler
fig <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank()) + 
  theme(strip.background=element_blank(), strip.text.y = element_text()) + theme(legend.background=element_blank()) + 
  theme(legend.key=element_blank()) + theme(panel.border = element_rect(colour="black", fill=NA))

# Make the followoing three plots to explore the data
plot <- ggplot(aes(Fish_Length,Trophic_Pos),data=data)

# Plot 1 - All Data
plot + geom_point() + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="All Data") + fig

# Plot 2 - By Speceis - BG = Bluegill, WY = Walleye, and YP = Yellow Perch
plot + geom_point() + facet_wrap(~ Fish_Species) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Species") + fig

# Plot 3 - By Lake 
plot + geom_point() + facet_wrap(~ Lake) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Lake") + fig

################Section 2#########################
#Running a mixed model in R
#Four Step Process to build a mixed model in R

#1) A priori model bulding and data exploration####
#i)  Map out the model based on a priori knowledge
#We know that we want to build a model that evaluates the relationship 
#bewteen trophic position and length while accounting for lake and species varition
#Trophic Position ~ Length + Species + Lake

#ii)Housekeeping and data exploration
#Ensure that the structure of your data is correct
str(data)
#Look at sample distribution across factors to check 
#if there are any major unequal distributions
table(data$Lake)
table(data$Fish_Species)
 
#Look at distribution of continuous variables
#Transform if necessary (will avoid future problems with homogeneity of model residuals)
hist(data$Fish_Length)
hist(data$Trophic_Pos)

#Check for colinearity between variables
plot(data)
cor(data$Fish_Length, data$Trophic_Pos)
#Note that in this data set there are not mulitple continuous variables 
#between which correlations might cause problems
#but if for example we have length and mass data
#we would not want to use both, as they would likely be highly correlated

#Consider the scales of your variables
#Note: when 2 variables have very different ranges of scale the criteria mixed models use to come up
#with parameter estimates are likely to return 'convergance errors'
#Z correcting adjusts for this scaling problem:
#What is a z correction?: (z = (x - mean(x))/sd(x))
#Z-correct Length
data$Z_Length<-(data$Fish_Length-mean(data$Fish_Length))/sd(data$Fish_Length)
#Z-correct Trophic Position
data$Z_TP<-(data$Trophic_Pos-mean(data$Trophic_Pos))/sd(data$Trophic_Pos)


#Find out if it is important to account for variation in "random effects"
#by comparing the residuals of a linear model without the random effects with 
#the potential random effects
lm.test<-lm(Z_TP~Z_Length, data=data)
lm.test.resid<-rstandard(lm.test)
#Species Effect
plot(lm.test.resid~ data$Fish_Species, xlab = "Species", ylab="Standardized residuals")
abline(0,0, lty=2)
#Lake Effect
plot(lm.test.resid~ data$Lake, xlab = "Lake", ylab="Standardized residuals")
abline(0,0, lty=2)

#2) Coding potential models and model selection####
#i) Coding all potential models
#List of all Potential models-->
#Note: you can chose to not code ones that do not make biological sense.
#Linear model with no random effects NOTE if you want to compare this model with the other models you must 
#Change REML = FALSE for all lmer models
M0<-lm(Z_TP~Z_Length,data=data)
#Full model with varying intercepts
M1<-lmer(Z_TP~Z_Length + (1|Fish_Species) + (1|Lake), data=data, REML=TRUE)
#Full model with varying intercepts and slopes
M2<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1+Z_Length|Lake), data=data, REML=TRUE)
#No Lake, varying intercepts only
M3<-lmer(Z_TP~Z_Length + (1|Fish_Species), data=data, REML=TRUE)
#No Species, varying intercepts only
M4<-lmer(Z_TP~Z_Length + (1|Lake), data=data, REML=TRUE)
#No Lake, varying intercepts and slopes
M5<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species), data=data, REML=TRUE)
#No Species, varying intercepts and slopes
M6<-lmer(Z_TP~Z_Length + (1+Z_Length|Lake), data=data, REML=TRUE)
#Full model with varying intercepts and slopes only varying by lake
M7<-lmer(Z_TP~Z_Length + (1|Fish_Species) + (1+Z_Length|Lake), data=data, REML=TRUE)
#Full model with varying intercepts and slopes only varying by species
M8<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1|Lake), data=data, REML=TRUE)


#ii) Compare models using AICc values
#Compute AICc values for each model
AICc<-c(AICc(M1), AICc(M2), AICc(M3), AICc(M4), AICc(M5), AICc(M6), AICc(M7), AICc(M8))
#Put values into one table for easy comparision
Model<-c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable
#M8 has the lowest AICc value so it has the most predictive power
#M2 is also a good fit, but all other models are not nearly as good.
#Note when you compare models with different fixed effects they must be fit by 
#Maximum Likelihood (ML) and not by Restricted Maximum Likelihood (REML)

#3) Checking model assumptions####
#Checking for M8
#A. Look at independence: plot fitted values vs residuals
E1 <- resid(M8)
F1<-fitted(M8)
plot(x = F1, 
     y = E1, 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

#B. Look at homogeneity:
# i. plot residuals vs each covariate in the model
#Fish_Length
plot(x = data$Z_Length, 
     y = E1, 
     xlab = "Z Length",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
#Note: observed groupings are created by the nature of the data because in the data set 
#we only measured individuals from 5 categories of lengths (big, small, and three groups in between)

#Species
boxplot(E1 ~ Fish_Species,   
        ylab = "Normalized residuals",
        data = data, xlab = "Species")
abline(h = 0, lty = 2)
#Lake
boxplot(E1 ~ Lake,   
        ylab = "Normalized residuals",
        data = data, xlab = "Lake")
abline(h = 0, lty = 2)

# ii. plot residuals vs each covariate not in the model
#NA in the case of this data set

#D. Look at normality: histogram
hist(E1)

#4) Interpreting results and visuzaling the model####
#Re-fit model by REML if you set REML = FALSE for model comparisions
#REML is a more conservative estimate, so the rule is to use this estimation method when
#obtaining model coefficients
M8<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1|Lake), data=data, REML=TRUE)

#Look at model summary
#This allows you to get an idea of the variance explained by the different components 
#of the model and the "significance" of fixed effects
summary(M8)

#Visualizing model results####
#There are several ways of visualizing the results of a mixed model, all of which
#involve using the coefficients generated by the model. 
#So the first step is get the model coefficients to be able to add them to the figures
coef(M8)
#Now put the coefs into dataframes to make them more easy to manipulate
Lake.coef <- as.data.frame(coef(M8)$Lake)
colnames(Lake.coef) <- c("Intercept","Slope")
Species.coef <- as.data.frame(coef(M8)$Fish_Species)
colnames(Species.coef) <- c("Intercept","Slope")

# Plot 1 - All Data
#Make a plot that includes all the data
plot <- ggplot(aes(Z_Length,Z_TP),data=data)
Plot_AllData <- plot + geom_point() + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="All Data") + fig
#Add a layer that has an abline with the intercept and slope of the relationship between length and trophic position
#Note that you can obtain the intercept and slope of the fixed factor directly from the model summary
summary(M8)
Plot_AllData + geom_abline(intercept = -0.0009059, slope =0.4222697)

# Plot 2 - By Speceis 
#Plot the data color coded by Species
Plot_BySpecies<-plot + geom_point(aes(colour = factor(Fish_Species)), size = 4) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Species") + fig
#Add the regression line with the intercepts and slopes specific to each species
Plot_BySpecies + geom_abline(intercept = Species.coef[1,1], slope =Species.coef[1,2], colour="coral2") + geom_abline(intercept = Species.coef[2,1], slope =Species.coef[2,2], colour = "green4") + geom_abline(intercept = Species.coef[3,1], slope =Species.coef[3,2], colour="blue1")

# Plot 3 - By Lake 
#Plot the data color coded by lake
Plot_ByLake<-plot + geom_point(aes(colour = factor(Lake)), size = 4) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Lake") + fig
#Add in regression lines with the intercepts specific to each lake
Plot_ByLake + geom_abline(intercept = Lake.coef[1,1], slope =Lake.coef[1,2], colour="coral2") + geom_abline(intercept = Lake.coef[2,1], slope =Lake.coef[2,2], colour="khaki4") + geom_abline(intercept = Lake.coef[3,1], slope =Lake.coef[3,2], colour="green4") + geom_abline(intercept = Lake.coef[4,1], slope =Lake.coef[4,2], colour="darkgoldenrod") + geom_abline(intercept = Lake.coef[5,1], slope =Lake.coef[5,2], colour="royalblue1") + geom_abline(intercept = Lake.coef[6,1], slope =Lake.coef[6,2], colour="magenta3")

beep(sound = 4)
beep(sound = 3)
#Thanks for attending the workshop and/or checking out this code!
#We hope that this has been helpful to you!
