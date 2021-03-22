##Section: 01-preparation-pour-l-atelier.R 

install.packages("lme4")
install.packages("AICcmodavg")
install.packages("MASS")
install.packages("vcdExtra")
install.packages("bbmle")
install.packages("MuMIn")
install.packages("ggplot2")
install.packages("DescTools")
install.packages("remotes")
install.packages("gridExtra")
install.packages("lattice")

library(lme4)
library(AICcmodavg)
library(MASS)
library(vcdExtra)
library(bbmle)
library(MuMIn)
library(ggplot2)
library(DescTools)
library(remotes)
library(gridExtra)
library(lattice)


##Section: 02-introduction-fr.R 

# Chargez le jeu de données
fish.data <- read.csv('data/qcbs_w7_data.csv', stringsAsFactors = TRUE) 
# Cette ligne variera en fonction de l'endroit où vos données sont enregistrées. 
#Vérifiez votre répertoire de travail avec getwd(), et changez-le avec setwd() au besoin.

# Format 'custom' pour simplifier tous les figures ggplot produites par la suite
fig <- theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_blank(), 
        strip.background=element_blank(), 
        strip.text.y = element_text(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        panel.border = element_rect(colour="black", fill = NA))

# Explorez les données graphiquement!

# Structure ggplot de base pour la relation qui nous intéresse
plot <- ggplot(aes(x = Fish_Length, y = Trophic_Pos), data = fish.data)

# Graphique 1 - Toutes les données
plot + geom_point() + 
  labs(x = "Longueur corporelle (mm)", y = "Position trophique", 
       title = "Toutes les données") + 
  fig # notre joli format ggplot!

# Graphique 2 - Par espèce
plot + geom_point() + 
  facet_wrap(~ Fish_Species) + # séparer la visualisation par espèce
  labs(x = "Longueur corporelle (mm)", y = "Position trophique", 
       title = "Par espèce") + 
  fig

# Graphique 3 – Par lac 
plot + geom_point() + 
  facet_wrap(~ Lake) + # séparer la visualisation par lac
  labs(x = "Longueur corporelle (mm)", y = "Position trophique", 
       title = "Par lac") + 
  fig


##Section: 03-exploration-des-donnees.R 




##Section: 04-implement-les-MLMs.R 

# Vérifiez la structure des données
str(fish.data)

# Observez la distribution des échantillons pour chaque facteur:
table(fish.data[ , c("Lake", "Fish_Species")])

# Regardez la distribution des variables continues.
hist(fish.data$Fish_Length, xlab = "Length (mm)", main = "")
hist(fish.data$Trophic_Pos, xlab = "Trophic position", main = "")

# Longueur corrigée, "à la main"
fish.data$Z_Length <- (fish.data$Fish_Length - mean(fish.data$Fish_Length)) / 
                      sd(fish.data$Fish_Length)

# Position trophique corrigée, avec la fonction scale
fish.data$Z_TP <- scale(fish.data$Trophic_Pos)

lm.test <- lm(Z_TP ~ Z_Length, data = fish.data)

lm.test.resid <- rstandard(lm.test)

plot(lm.test.resid ~ as.factor(fish.data$Fish_Species),
     xlab = "Species", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

plot(lm.test.resid ~ as.factor(fish.data$Lake),
     xlab = "Lake", ylab = "Standardized residuals")

abline(0, 0, lty = 2)

lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

# Défi 4: Réécrivez le code suivant de façon à ce que les **pentes** de la relation position trophique en fonction de longueur corporelle **varient par lac et par espèces**:
lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

# Défi 4 Solution:
lmer(Z_TP ~ Z_Length + (1 + Z_Length | Lake) + (1 + Z_Length | Fish_Species),
     data = fish.data, REML = TRUE)

# Défi 5 : Faites une liste de 7 modèles alternatifs qui pourraient être comparés à ce modèle initial:
lmer(Z_TP ~ Z_Length + (1 | Lake) + (1 | Fish_Species),
     data = fish.data, REML = TRUE)

# Solution du défi 5: Modèle linéaire de base
M0 <- lm(Z_TP ~ Z_Length, data = fish.data)

# Solution du défi 5, autres modèles potentiels
# Notez que REML = FALSE afin de comparer avec le modèle linéaire de base où la méthode d'estimation = ML.

# Modele linéaire de base
M0 <- lm(Z_TP ~ Z_Length, data = fish.data)
# modèle complet avec variation des ordonnées à l'origine
M1 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species) + (1 | Lake), 
           data = fish.data, REML = FALSE)
# modèle complet avec variation des ordonnées à l'origine et de pentes
M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
           data = fish.data, REML = FALSE)
# Pas d'effet lac, les ordonnées à l'origine varient par espèces
M3 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species), data = fish.data, REML = FALSE)
# Pas d'effet espèces, les ordonnées à l'origine varient par lac
M4 <- lmer(Z_TP ~ Z_Length + (1 | Lake), data = fish.data, REML = FALSE)
# Pas d'effet de lac, les ordonnées à l'origine et les pentes varient par espèces
M5 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species), 
           data = fish.data, REML = FALSE)
# Pas d'effet de l'espèces, les ordonnées à l'origine et les pentes varient par lac
M6 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Lake), data = fish.data, REML = FALSE)
# modèle complet, variation d'ordonnée à l'origine et pente par lac
M7 <- lmer(Z_TP ~ Z_Length + (1 | Fish_Species) + (1 + Z_Length | Lake),
           data = fish.data, REML = FALSE)
# modèle complet, variation d'ordonnée à l'origine et pente par espèces
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, REML = FALSE)

# Trouver la valeur AICc pour notre premier modèle (Modèle linéaire de base) avec le paquet MuMIn
MuMIn::AICc(M1)

# Pour regrouper toutes les valeurs de l'AICc dans un seul tableau, utilisez `MuMIn::model.sel()` pour calculer l'AICc pour chaque modèle (avec d'autres sorties) et ensuite sélectionnez seulement les colonnes d'intérêt pour les imprimer dans un tableau.
AIC.table  <- MuMIn::model.sel(M0, M1, M2, M3, M4, M5, M6, M7, M8)
# `df` est le degré de liberté
# `logLik` est le log de la vraisemblance
# `delta` est la différence d'AICc avec la valeur la plus petite
(AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")])

# Pour plus d'informations sur les autres sorties ou résultats de la fonction model.sel(), roulez `?model.sel`.

# Examinons de plus proche M8 and M2 (les autres modèlesont des AICc beaucoup plus élevés)
# `REML = TRUE` parce qu'on compare deux modèles avec des effets aléatoires nichés et avec la même structure d'effets fixes
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, 
           REML = TRUE) 

M2 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 + Z_Length | Lake),
           data = fish.data, 
           REML = TRUE)

# Sortonz un tableau pour comparer M2 et M8
MuMIn::model.sel(M2,M8)[ , c("df", "logLik", "AICc", "delta")]

# Regardons à nouveau le meilleur modèle, quelle est sa structure?
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, REML = FALSE)
# L'ordonnée à l'origine et l'effet de la longueur sur la position trophique peut varier selon l'espèce de poissons, mais seulement l'ordonnée à l'origine peut varier par lac

# Une fois que le meilleur modèle est sélectionné il faut remettre la méthode d'estimation a `REML = TRUE`
M8 <- lmer(Z_TP ~ Z_Length + (1 + Z_Length | Fish_Species) + (1 | Lake),
           data = fish.data, 
           REML = TRUE)

# Plotez les valeurs prédites par rapport aux valeurs résiduelles
par(mar=c(4,4,.5,.5))
plot(resid(M8) ~ fitted(M8), 
     xlab = 'Predicted values', 
     ylab = 'Normalized residuals')
abline(h = 0, lty = 2)
# La dispersion homogène des résidus signifie que l'hypothèse est respectée.

# Afin de vérifier l'indépendance des résidus du modèle, nous devons tracer les résidus en fonction de chaque covariable du modèle.
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
# La dispersion homogène des résidus autour de 0 signifie qu'il n'y a pas de modèle de résidus en fonction de la variable, donc l'hypothèse est respectée !
# Note : Les groupes sont dus à la structure des données, où les poissons de seulement 5 classes de taille (grande, petite, et trois groupes intermédiaires) ont été capturés.

# Vérifiez la normalité des résidus du modèle car des résidus suivant une distribution normale indiquent que le modèle n'est pas biaisé.
hist(resid(M8))
# Les résidus sont normaux ! Cela signifie que notre modèle n'est pas biaisé.

# Maintenant nous sommes prêts pour l'interprétation et la visualisation.
# Regardons de plus près notre modèle final en utilisant la fonction `summary()`. 
(summ_M8 <- summary(M8))

# Défi 7 : *Il est possible de visualiser graphiquement les différentes ordonnées à l'origine et pentes du modèle pour mieux interpréter les résultats ?

# Solution du défi 7 : Oui ! Nous pourrions le faire en générant les figures suivantes.
# a) Figure avec toutes les données regroupées
# b) Figure par espèce
# c) Figure par lac

# Pour produire ces figures, nous avons d'abord besoin des coefficients du modèle complet qui se trouvent dans le résumé du modèle.
summ_M8$coefficients
# Intercept = Intercept = 9.0589745 × 10^4
# Slope = 0.4222697

# Nous avons également besoin des coefficients pour chaque niveau du modèle, qui peuvent être obtenus avec la fonction `coef`.
coef(M8)

# Maintenant, faisons nos figures !

# a) Figure avec toutes les données groupées
# Créez un thème ggplot simplifié
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

# Vous pouvez également écrire les chiffres comme ceci :
# Plot_AllData + geom_abline(intercept = -0.0009059, slope = 0.4222697)

# b) Figure par espèce
# Mettre les coefs dans un tableau pour les rendre plus faciles à manipuler
Lake.coef              <- coef(M8)$Lake
colnames(Lake.coef)    <- c("Intercept", "Slope")
Species.coef           <- coef(M8)$Fish_Species
colnames(Species.coef) <- c("Intercept", "Slope")

Plot_BySpecies <- plot + 
                    geom_point(aes(colour = factor(Fish_Species)), size = 4) +
                    xlab("Longueur (mm)") + ylab("Position trophique") +
                    labs(title = "Par espèce") + fig

# Ajoutez les lignes de régression pour chaque espèce
Plot_BySpecies +
  geom_abline(intercept = Species.coef[1,1], 
              slope     = Species.coef[1,2], col = "coral2") +
  geom_abline(intercept = Species.coef[2,1], 
              slope     = Species.coef[2,2], col = "green4") +
  geom_abline(intercept = Species.coef[3,1], 
              slope     = Species.coef[3,2], col = "blue1")


# c) Figure par lac
Plot_ByLake <- plot + 
                geom_point(aes(colour = factor(Lake)), size = 4) +
                xlab("Length (mm)") + ylab("Trophic Position") +
                labs(title = "By Lake") + fig

# Ajouter les lignes de régression avec les ordonnées à l'origine spécifiques à chaque lac
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


##Section: 05-introduction-des-GLMM.R 

dat.tf <- read.csv("data/banta_totalfruits.csv")

# Dans cet ensemble de données, les en-têtes de colonne sont définis comme suit:
# popu: facteur avec un niveau pour chaque population
# gen: facteur avec un niveau pour chaque génotype
# nutrient: facteur avec niveau bas (valeur = 1) ou haut (valeur = 8)
# amd: facteur précisant l'absence ou la présence d'herbivorie
# total.fruits: nombre entier indiquant le nombre de fruits par plante

#Avant d'aller plus loin, nous devons choisir une distribution d'erreurs. Ce choix sera guidé par la structure de nos données. 
# Notre variable de réponse est une donnée de comptage, ce qui suggère que nous avons besoin d'une distribution de Poisson (c'est-à-dire que la variance est égale à la moyenne).
hist(dat.tf$total.fruits, breaks = 50, col = 'blue', main = '',
     xlab = 'Total fruits', ylab = 'Count')

# Explorons la variance dans nos données
# Créez de nouvelles variables qui représentent chaque combinaison de variables.
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
ggplot(data = dat.tf, 
       aes(factor(x = gna), y = log(total.fruits + 1))) +
  geom_boxplot(colour = "skyblue2", 
               outlier.shape = 21,
               outlier.colour = "skyblue2") +
  ylab("log (Total fruits)\n") + # \n creates a space after the title
  xlab("\nGenotype x nutrient x clipping") + # space before the title
  theme_bw() + theme(axis.text.x = element_blank()) +
  stat_summary(fun = mean, geom = "point", colour = "red")

# Figure diagnostique de variances par groupe vs moyennes par groupe (genotype x nutrient x clipping grouping). 
# Code pour créer la figure: https://github.com/QCBSRworkshops/workshop07/blob/main/pres-fr/data/glmm_e.r

# On remarque beaucoup de variation entre les variances des échantillons dans les données transformées 
# Par exemple, entre génotypes:
grpVars <- tapply(dat.tf$total.fruits, dat.tf$gna, var)

grpMeans <- tapply(dat.tf$total.fruits,dat.tf$gna, mean)

# Quasi-Poisson
lm1 <- lm(grpVars~grpMeans-1) 
phi.fit <- coef(lm1)
# Le -1 specifie un modèle avec une ordonnée à l'origine de 0

# Binomiale négative
lm2 <- lm(grpVars ~ I(grpMeans^2) + offset(grpMeans)-1)
k.fit <- 1/coef(lm2)
# offset() spécifie qu'on veut ajouter un terme pour les moyennes de groupes avec un coefficient fixé à 1

# Non-parametric loess fit
Lfit <- loess(grpVars ~ grpMeans)

# La figure
plot(grpVars ~ grpMeans, xlab = "Group means", ylab = "Group variances" )
abline(a = 0, b = 1, lty = 2)
text(105,500, "Poisson")
curve(phi.fit*x, col = 2, add = TRUE)
# bquote() remplace des valeurs numériques dans des équations avec des symboles
text(110,3900,
     bquote(paste("QP: ", sigma^2==.(round(phi.fit,1))*mu)), col = 2)
curve(x*(1+x/k.fit), col = 4, add = TRUE)
text(104,7200, paste("NB: k = ", round(k.fit, 1), sep = ""), col = 4)
mvec <- 0:120
lines(mvec, predict(Lfit, mvec), col = 5)
text(118, 2000, "loess", col = 5)

# GLMM Poisson
# Nous avons besoin d'un modèle qui tient compte de la surdispersion.
# Commençons avec un modèle Poisson
mp1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
             (1|popu)+
             (1|gen),
             data = dat.tf, family = "poisson")

# Téléchargez le code glmm_funs.R de la page wiki et exécutez la fonction dans R
source(file = "data/glmm_funs.R")
# Surdispersion?
overdisp_fun(mp1)
# Le rapport est significativement > 1

# GLMM binomial négatif utilisant la fonction glmer.nb()
mnb1 <- glmer.nb(total.fruits ~ nutrient*amd + rack + status +
                 (1|popu)+
                 (1|gen),
                 data = dat.tf,
                 control = glmerControl(optimizer = "bobyqa"))
# 'control' spécifie la façon dont nous optimisons les valeurs des paramètres

# Surdispersion?
overdisp_fun(mnb1)
# Le rapport est beaucoup plus près de 1 mais la valeur de p < 0.05

# GLMM Poisson-lognormale

# Cette variable est déjà dans vos données "dat.tf", mais voici comment la créer.
dat.tf$X <- 1:nrow(dat.tf)

# Tenir compte de la surdispersion
mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
              (1|X) +
              (1|popu)+
              (1|gen),
data = dat.tf, family = "poisson",
control = glmerControl(optimizer = "bobyqa"))

# Surdispersion?
overdisp_fun(mpl1)
# Le rapport est maintenant conforme avec notre critère, soit < 1

# Visualisons les paramètres du modèle

# Cette librairie n'est pas sur le CRAN! On utilise la librairie `remotes` pour l'installer à partir de GitHub.
if (!require("coefplot2"))
  remotes::install_github("palday/coefplot2", subdir = "pkg")
library(coefplot2)

# Effets aléatoires
coefplot2(mpl1, ptype = "vcov", intercept = TRUE, main = "Random effect variance")

# Effets fixes
coefplot2(mpl1, intercept = TRUE, main = "Fixed effect coefficient")

# dotplot code
pp <- list(layout.widths = list(left.padding = 0, right.padding = 0),
           layout.heights = list(top.padding = 0, bottom.padding = 0))
r2 <- ranef(mpl1, condVar = TRUE)
d2 <- dotplot(r2, par.settings = pp)

grid.arrange(d2$gen, d2$popu, nrow = 1)


##Section: 06-sélection-du-modèle.R 

mpl1 <- glmer(total.fruits ~ nutrient*amd + rack + status +
              (1|X) +
              (1|popu)+
              (1|gen),
data = dat.tf, family = "poisson",
control = glmerControl(optimizer = "bobyqa"))

mpl2 <- update(mpl1, . ~ . - rack) # modèle sans rack
mpl3 <- update(mpl1, . ~ . - status) # modèle sans status
mpl4 <- update(mpl1, . ~ . - amd:nutrient) # modèle sans interaction amd:nutrient

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


##Section: 07-ressources-additionnelles.R 




##Section: 08-references-fr.R 




