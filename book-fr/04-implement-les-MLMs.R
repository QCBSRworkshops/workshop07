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


## lmer(Biodiv ~ Productivity + (1 | Forest / Site))
