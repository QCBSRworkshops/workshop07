## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = TRUE,
  fig.width = 6, fig.height = 6,
  fig.retina = 3,
  fig.align = 'center'
)
options(repos=structure(c(CRAN="http://cran.r-project.org")))


## ----install_pkgs, echo = FALSE, results = "asis"-----------------------------
cat(
  qcbsRworkshops::first_slides(6, c('ggplot2', 'lme4', 'AICcmodavg'), lang = "fr")
)


## ---- eval = TRUE-------------------------------------------------------------
data <- read.csv('data/qcbs_w6_data.csv')
head(data)


## ---- echo= FALSE-------------------------------------------------------------
library(ggplot2)
data <- read.csv('data/qcbs_w6_data.csv')

# thème simplifié
fig <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank()) +
  theme(strip.background=element_blank(), strip.text.y = element_text()) + theme(legend.background=element_blank()) +
  theme(legend.key=element_blank()) + theme(panel.border = element_rect(colour="black", fill=NA))

# Faites les trois graphiques suivants pour explorer les données
plot <- ggplot(aes(Fish_Length,Trophic_Pos),data=data)

# Graphique 1 - Toutes les données
plot + geom_point() + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="All Data") + fig


## ---- eval= FALSE-------------------------------------------------------------
## # thème simplifié
## fig <- theme_bw() + theme(panel.grid.minor=element_blank(),
##     panel.grid.major=element_blank(), panel.background=element_blank()) +
##     theme(strip.background=element_blank(), strip.text.y = element_text()) +
##     theme(legend.background=element_blank()) +
##     theme(legend.key=element_blank()) +
##     theme(panel.border = element_rect(colour="black", fill=NA))
## 
## # Faites les trois graphiques suivants pour explorer les données
## plot <- ggplot(aes(Fish_Length,Trophic_Pos),data=data)
## 
## # Graphique 1 - Toutes les données
## plot + geom_point() + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="All Data") + fig


## -----------------------------------------------------------------------------
# Graphique 2 - Par espèce
plot + geom_point() + facet_wrap(~ Fish_Species) + xlab("Length (mm)") + ylab("Trophic Position") +
   labs(title="By Species") + fig


## -----------------------------------------------------------------------------
# Graphique 3 – Par lac
plot + geom_point() + facet_wrap(~ Lake) + xlab("Length (mm)") + ylab("Trophic Position") +
   labs(title="By Lake") + fig


## -----------------------------------------------------------------------------
data <- read.csv('data/qcbs_w6_data.csv')
str(data)


## -----------------------------------------------------------------------------
table(data$Lake)
table(data$Fish_Species)


## ---- fig.width=10, fig.height=4----------------------------------------------
par(mfrow=c(1,2), mar = c(4,4,1,1))
hist(data$Fish_Length)
hist(data$Trophic_Pos)


## ---- eval = FALSE------------------------------------------------------------
## plot(data)
## 
## cor(var1, var2)


## -----------------------------------------------------------------------------
#Longueur corrigée, ""à la main"
data$Z_Length <- (data$Fish_Length-mean(data$Fish_Length))/sd(data$Fish_Length)

#Position trophique corrigée, avec scale
data$Z_TP<- scale(data$Trophic_Pos)


## -----------------------------------------------------------------------------
lm.test <- lm(Z_TP ~ Z_Length, data = data)


## -----------------------------------------------------------------------------
lm.test.resid <- rstandard(lm.test)


## ---- fig.width=10, fig.height=5, eval = FALSE--------------------------------
## par(mfrow=c(1,2))
## 
## plot(lm.test.resid ~ data$Fish_Species,
##      xlab = "Species", ylab = "Standardized residuals")
## 
## abline(0, 0, lty = 2)
## 
## plot(lm.test.resid ~ data$Lake,
##      xlab = "Lake", ylab = "Standardized residuals")
## 
## abline(0, 0, lty = 2)


## ---- fig.width=10, fig.height=5, echo = FALSE--------------------------------
par(mfrow=c(1,2), mar = c(4,4,1,1))
plot(lm.test.resid ~ data$Fish_Species,
     xlab = "Species", ylab = "Standardized residuals")
abline(0, 0, lty = 2)
plot(lm.test.resid ~ data$Lake,
     xlab = "Lake", ylab = "Standardized residuals")
abline(0, 0, lty = 2)


## ---- include = FALSE---------------------------------------------------------
library(lme4)


## ---- eval = FALSE------------------------------------------------------------
## library(lme4)
## lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
##      data = data, REML = TRUE)


## -----------------------------------------------------------------------------
lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = data, REML = TRUE)


## -----------------------------------------------------------------------------
lmer(Z_TP ~ Z_Length + (1 + Z_Length | Lake) + (1 + Z_Length | Fish_Species),
     data = data, REML = TRUE)


## ---- eval = FALSE------------------------------------------------------------
## lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
##      data = data, REML = TRUE)


## -----------------------------------------------------------------------------
M0 <- lm(Z_TP ~ Z_Length, data = data)


## -----------------------------------------------------------------------------
# Modele linéaire de base
M0 <- lm(Z_TP ~ Z_Length, data = data)
# modèle complet avec variation des intercepts
M1 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species) + (1 | Lake), data = data, REML = FALSE)
# modèle complet avec variation des intercepts et de pentes
M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
           data = data, REML = FALSE)
# Pas d'effet lac, les intercepts varient par espèces
M3 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species), data = data, REML = FALSE)
# Pas d'effet espèces, les intercepts varient par lac
M4 <- lmer(Z_TP ~ Z_Length + (1 | Lake), data = data, REML = FALSE)
# Pas d'effet de lac, les intercepts et les pentes varient par espèces
M5 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species), data = data, REML = FALSE)
# Pas d'effet de l'espèces, les intercepts et les pentes varient par lac
M6 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Lake), data = data, REML = FALSE)
# modèle complet, variation d'intercept et pente par lac
M7 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species) + (1 + Z_Length | Lake),
           data = data, REML = FALSE)
# modèle complet, variation d'intercept et pente par espèces
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = data, REML = FALSE)


## -----------------------------------------------------------------------------
library(AICcmodavg)
AICc(M1)


## -----------------------------------------------------------------------------
AICc <- c(AICc(M0), AICc(M1), AICc(M2), AICc(M3),
          AICc(M4), AICc(M5), AICc(M6), AICc(M7), AICc(M8))

Model <- c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")

AICtable <- data.frame(Model = Model, AICc = AICc)



## -----------------------------------------------------------------------------
AICtable


## ---- eval = FALSE------------------------------------------------------------
## M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
##            data = data, REML = FALSE)


## -----------------------------------------------------------------------------
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = data, REML = TRUE)

M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
           data = data, REML = TRUE)


## ---- eval = FALSE------------------------------------------------------------
## M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
##            data = data, REML = TRUE)
## 
## M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
##            data = data, REML = TRUE)


## ---- fig.width=4.5, fig.height=4.5, echo = -1--------------------------------
par(mar=c(4,4,.5,.5))
plot(resid(M8) ~ fitted(M8), xlab = 'Valeurs pérdites', ylab = 'Résidus normalisés')
abline(h = 0, lty = 2)


## ---- fig.width=11, fig.height=4.5, eval = FALSE------------------------------
## par(mfrow = c(1,3), mar=c(4,4,.5,.5))
## 
## plot(resid(M8) ~ data$Z_Length, xlab = "Longueur", ylab = "Résidus normalisés")
## abline(h = 0, lty = 2)
## 
## boxplot(resid(M8) ~ Fish_Species, data = data, xlab = "Espèces", ylab = "Résidus normalisés")
## abline(h = 0, lty = 2)
## 
## boxplot(resid(M8) ~ Lake, data = data, xlab = "Lacs", ylab = "Résidus normalisés")
## abline(h = 0, lty = 2)


## ---- fig.width=12, fig.height=4.5, echo = FALSE------------------------------
par(mfrow = c(1,3), mar=c(4,4,.5,.5), cex.lab = 1.5)
plot(resid(M8) ~ data$Z_Length, xlab = "Longueur", ylab = "Résidus normalisés")
abline(h = 0, lty = 2)
boxplot(resid(M8) ~ Fish_Species, data = data, xlab = "Espèces", ylab = "")
abline(h = 0, lty = 2)
boxplot(resid(M8) ~ Lake, data = data, xlab = "Lacs", ylab = "")
abline(h = 0, lty = 2)


## ---- fig.height=5, fig.width=5-----------------------------------------------
hist(resid(M8))


## ---- eval = FALSE------------------------------------------------------------
## summ_M8 <- summary(M8)
## summ_M8


## ---- echo = FALSE------------------------------------------------------------
summ_M8 <- summary(M8)
summary(M8)


## -----------------------------------------------------------------------------
summ_M8$coefficients


## -----------------------------------------------------------------------------
coef(M8)


## ---- eval = FALSE------------------------------------------------------------
## library(ggplot2)
## 
## # Thème ggplot simplifié
## fig <- theme_bw() +
##   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
##         panel.background=element_blank()) +
##   theme(strip.background=element_blank(), strip.text.y = element_text()) +
##   theme(legend.background=element_blank()) +
##   theme(legend.key=element_blank()) +
##   theme(panel.border = element_rect(colour = "black", fill=NA))
## 
## plot <- ggplot(aes(Z_Length, Z_TP), data = data)
## Plot_AllData <- plot + geom_point() +
##   xlab("Longueur (mm)") + ylab("Position trophique") +
##   labs(title = "Toutes les données") + fig
## 
## Plot_AllData + geom_abline(intercept = -.0009059, slope = 0.4222697)


## ---- echo = FALSE------------------------------------------------------------
plot <- ggplot(aes(Z_Length, Z_TP), data = data)
Plot_AllData <- plot + geom_point() +
  xlab("Longueur (mm)") + ylab("Position trophique") +
  labs(title = "Toutes les données") + fig

Plot_AllData + geom_abline(intercept = -.0009059, slope = 0.4222697)


## ---- eval = FALSE------------------------------------------------------------
## # mettre les coefs dans un tableau pour les rendre plus faciles à manipuler
## Lake.coef <- coef(M8)$Lake
## colnames(Lake.coef) <- c("Intercept", "Slope")
## Species.coef <- coef(M8)$Fish_Species
## colnames(Species.coef) <- c("Intercept", "Slope")
## 
## Plot_BySpecies<-plot + geom_point(aes(colour = factor(Fish_Species)), size = 4) +
##   xlab("Longueur (mm)") + ylab("Position trophique") +
##   labs(title = "Par espèce") + fig
## 
## # Ajoutez les lignes de régression pour chaque espèce
## Plot_BySpecies +
##   geom_abline(intercept = Species.coef[1,1], slope = Species.coef[1,2], col = "coral2") +
##   geom_abline(intercept = Species.coef[2,1], slope = Species.coef[2,2], col = "green4") +
##   geom_abline(intercept = Species.coef[3,1], slope = Species.coef[3,2], col = "blue1")
## 
## 


## ---- echo = F, fig.width=8---------------------------------------------------
Lake.coef <- as.data.frame(coef(M8)$Lake)
colnames(Lake.coef) <- c("Intercept", "Slope")
Species.coef <- as.data.frame(coef(M8)$Fish_Species)
colnames(Species.coef) <- c("Intercept", "Slope")

Plot_BySpecies<-plot + geom_point(aes(colour = factor(Fish_Species)), size = 4) +
  xlab("Longueur (mm)") + ylab("Position trophique") +
  labs(title = "Par espèce") + fig

# Ajoutez les lignes de régression pour chaque espèce
Plot_BySpecies +
  geom_abline(intercept = Species.coef[1,1], slope = Species.coef[1,2], col = "coral2") +
  geom_abline(intercept = Species.coef[2,1], slope = Species.coef[2,2], col = "green4") +
  geom_abline(intercept = Species.coef[3,1], slope = Species.coef[3,2], col = "blue1")



## ---- eval= FALSE-------------------------------------------------------------
## Plot_ByLake<-plot + geom_point(aes(colour = factor(Lake)), size = 4) +
##   xlab("Length (mm)") + ylab("Trophic Position") +
##   labs(title = "By Lake") + fig
## 
## # Ajouter les lignes de régression avec les intercepts spécifiques à chaque lac
## Plot_ByLake +
##   geom_abline(intercept = Lake.coef[1,1], slope = Lake.coef[1,2], col = "coral2") +
##   geom_abline(intercept = Lake.coef[2,1], slope = Lake.coef[2,2], col = "khaki4") +
##   geom_abline(intercept = Lake.coef[3,1], slope = Lake.coef[3,2], col = "green4") +
##   geom_abline(intercept = Lake.coef[4,1], slope = Lake.coef[4,2], col = "darkgoldenrod") +
##   geom_abline(intercept = Lake.coef[5,1], slope = Lake.coef[5,2], col = "royalblue1") +
##   geom_abline(intercept = Lake.coef[6,1], slope = Lake.coef[6,2], col = "magenta3")
## 


## ---- echo = F, fig.width=8---------------------------------------------------
Plot_ByLake<-plot + geom_point(aes(colour = factor(Lake)), size = 4) +
  xlab("Longueur (mm)") + ylab("Position trophique") +
  labs(title = "par lac") + fig

# Ajouter les lignes de régression avec les intercepts spécifiques à chaque lac
Plot_ByLake +
  geom_abline(intercept = Lake.coef[1,1], slope = Lake.coef[1,2], col = "coral2") +
  geom_abline(intercept = Lake.coef[2,1], slope = Lake.coef[2,2], col="khaki4") +
  geom_abline(intercept = Lake.coef[3,1], slope = Lake.coef[3,2], col="green4") +
  geom_abline(intercept = Lake.coef[4,1], slope = Lake.coef[4,2], col="darkgoldenrod") +
  geom_abline(intercept = Lake.coef[5,1], slope = Lake.coef[5,2], col="royalblue1") +
  geom_abline(intercept = Lake.coef[6,1], slope = Lake.coef[6,2], col="magenta3")



## ---- eval = FALSE------------------------------------------------------------
## lmer(Biodiv ~ Productivite + (1 | Foret / Site))


## ---- eval = FALSE------------------------------------------------------------
## lmer(Mercure ~ Longueur * Habitat + (1 | Site))

