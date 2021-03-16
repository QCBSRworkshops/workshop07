# Ateliers R du CSBQ 
# Modèles linéaires à effets mixtes
# Développé par :  Jacob Ziegler, Dalal Hanna et Catherine Baltazar

################Section 1#########################

# Supprimer commandes antérieures en R
rm(list=ls()) 

# Placez tout le matériel de l'atelier dans un même dossier sur votre ordinateur
# Exécutez la ligne de code suivante et utilisez la fenêtre de navigation pour sélectionner QCBS_W6_Data.csv
# le fichier dans le dossier qui contient le matériel de l'atelier
file.choose()

# Réglez le répertoire de travail vers le dossier qui contient les fichiers en copiant toute 
# la sortie de la commande file.choose (), à l'exception du nom de fichier R, et collez le tout dans setwd().

# Par exemple collez "/Users/ziegljac/Documents/QCBS_R/" -> incluez les guillemets
# NON "/Users/ziegljac/Documents/QCBS_R/Get_Data_Func.R" -> incluez les guillemets 
setwd()

# Chargez les bibliothèques et les données utiles
# Si vous n’avez jamais chargé ces bibliothèques avant, vous devrez utiliser la fonction install.packages()
# avant la fonction library()
library(ggplot2)
library(lme4)
library(arm)
library(AICcmodavg)
# L'AICc corrige pour le biais créé par les faibles tailles d'échantillon quand le AIC est calculé
# Toujours utiliser l'AICc parce que l'équation est définie comme quoi que plus la taille de l'échantillon augment, le plus c'est juste comme l'AIC

data <- read.csv('qcbs_w6_data.csv')

# Utilisé pour simplifier les figures
fig <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank()) + 
  theme(strip.background=element_blank(), strip.text.y = element_text()) + theme(legend.background=element_blank()) + 
  theme(legend.key=element_blank()) + theme(panel.border = element_rect(colour="black", fill=NA))

# Faites les trois graphiques suivants pour explorer les données
plot <- ggplot(aes(Fish_Length,Trophic_Pos),data=data)

# Graphique 1 - Toutes les données
plot + geom_point() + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="All Data") + fig

# Graphique 2 - Par espèce - BG = Bluegill (crapet), WY = Walleye (doré), et YP = Yellow Perch (perchaude)
plot + geom_point() + facet_wrap(~ Fish_Species) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Species") + fig

# Graphique 3 – Par lac 
plot + geom_point() + facet_wrap(~ Lake) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Lake") + fig

################Section 2#########################
# Exécution d'un modèle mixte en R
# Processus de quatre étapes pour la construction d'un modèle mixte en R

# 1) Construction du modèle a priori et exploration des données ####
# i)  Définir un  modèle basé sur une connaissance a priori
# Nous savons que nous voulons construire un modèle qui évalue la relation entre la position trophique et 
# la longueur tout en tenant compte de la variation due au lac et à l'espèce
# Position trophique ~ Longueur + Espèce + Lac

# ii) Housekeeping et exploration des données
# Assurez-vous que la structure de vos données soit correcte
str(data)
# Regardez la distribution des échantillons de chaque facteur pour vérifier
# si le jeu de données est équilibré
table(data$Lake)
table(data$Fish_Species)
 
# Regardez la distribution des variables continues
# Transformez si nécessaire (ça évitera des problèmes d’hétérogénéité des résidus du modèle)
hist(data$Fish_Length)
hist(data$Trophic_Pos)

# Évaluer la colinéarité entre variables
plot(data)
cor(data$Fish_Length, data$Trophic_Pos)
# Dans cet exemple, il n’y a pas de risque de colinéarité avec seulement 
# une variable explicative continue

# Considérez l'échelle de vos données 
# Note : Si deux variables dans le même modèle ont des valeurs se situant sur des échelles très différentes, 
# il est probable que le modèle mixte indique un 'problème de convergence' en essayant de calculer les 
# paramètres. La correction Z standardize les variables et résout ce problème :
# Qu'est-ce qu'une correction de Z ?:  (z = (x - mean(x))/sd(x))
# Longueur corrigée
data$Z_Length<-(data$Fish_Length-mean(data$Fish_Length))/sd(data$Fish_Length)
# Position trophique corrigée
data$Z_TP<-(data$Trophic_Pos-mean(data$Trophic_Pos))/sd(data$Trophic_Pos)


# Déterminez s'il est important de tenir compte des variations dans les "effets aléatoires" en comparant 
# les résidus d'un modèle linéaire sans les effets aléatoires en fonction des effets aléatoires potentiels
lm.test<-lm(Z_TP~Z_Length, data=data)
lm.test.resid<-rstandard(lm.test)
# Effet de l’espèce
plot(lm.test.resid~ data$Fish_Species, xlab = "Species", ylab="Standardized residuals")
abline(0,0, lty=2)
# Effet du lac
plot(lm.test.resid~ data$Lake, xlab = "Lake", ylab="Standardized residuals")
abline(0,0, lty=2)

# 2) Coder les modèles potentiels et sélectionner le meilleur modèle ####
# i) Coder les modèles potentiels
# Liste de tous les modèles potentiels -->  
# Note: vous pouvez choisir de ne pas coder ceux qui n'ont pas de sens biologique.
# Construisez aussi le modèle lm() pour voir la variation dans les valeurs de AICc, mais changez la 
# méthode à ML (REML=FALSE) parce que lm() n'utilise pas la même méthode d'estimation que lmer().
# Modèle linéaire sans effets aléatoires
M0<-lm(Z_TP~Z_Length,data=data)
# Modèle complet avec différents intercepts
M1<-lmer(Z_TP~Z_Length + (1|Fish_Species) + (1|Lake), data=data, REML=FALSE)
# Modèle complet avec différents intercepts et pentes
M2<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1+Z_Length|Lake), data=data, REML=FALSE)
# Aucun effet Lac, intercept aléatoire seulement
M3<-lmer(Z_TP~Z_Length + (1|Fish_Species), data=data, REML=FALSE)
# Aucun effet Espèce, intercept aléatoire seulement
M4<-lmer(Z_TP~Z_Length + (1|Lake), data=data, REML=FALSE)
# Aucun effet Lac, intercept et pente aléatoires
M5<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species), data=data, REML=FALSE)
# Aucun effet Espèce, intercept et pente aléatoires
M6<-lmer(Z_TP~Z_Length + (1+Z_Length|Lake), data=data, REML=FALSE)
# Modèle complet avec intercepts et pentes qui variant par Lac
M7<-lmer(Z_TP~Z_Length + (1|Fish_Species) + (1+Z_Length|Lake), data=data, REML=FALSE)
# Modèle complet avec intercepts et pentes qui variant par Espèce
M8<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1|Lake), data=data, REML=FALSE)


# ii) Comparer les modèles en comparant les valeurs AICc 
# Calculer les valeurs AICc pour chaque modèle
AICc<-c(AICc(M0), AICc(M1), AICc(M2), AICc(M3), AICc(M4), AICc(M5), AICc(M6), AICc(M7), AICc(M8))
# Mettre des valeurs dans une table pour faciliter la comparaison
Model<-c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")
AICtable<-data.frame(Model=Model, AICc=AICc)
AICtable
# M8 a la plus faible valeur AICc donc le meilleur modèle
# M2 est également un bon modèle, mais tous les autres modèles ne sont pas aussi bons.

# Recoder les MLMs
# Une fois que les meilleur modèles sont sélectionnés il faut remettre REML=TRUE
M8<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1|Lake), data=data, REML=TRUE)
M2<-lmer(Z_TP~Z_Length + (1+Z_Length|Fish_Species) + (1+Z_Length|Lake), data=data, REML=TRUE)

#3) Vérification des suppositions du modèle ####
# Vérification pour M8
# A. Vérifiez l'homogénéité : graphique des valeurs prédites vs valeurs résiduelles
E1 <- resid(M8)
F1<-fitted(M8)
plot(x = F1, 
     y = E1, 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

# B. Vérifiez l’indépendance :
# i. graphique des résidus VS chaque covariable du modèle 
# Longueur corporelle des poissons
plot(x = data$Z_Length, 
     y = E1, 
     xlab = "Z Length",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)
# Note: Les regroupements de données sont dus à la structure des données, où des poissons de seulement 5 
# classes de taille  (grandes, petites, et trois groupes entre les deux) étaient capturés.

# Espèce
boxplot(E1 ~ Fish_Species,   
        ylab = "Normalized residuals",
        data = data, xlab = "Species")
abline(h = 0, lty = 2)
# Lac
boxplot(E1 ~ Lake,   
        ylab = "Normalized residuals",
        data = data, xlab = "Lake")
abline(h = 0, lty = 2)

# ii. graphique des résidus VS chaque covariable non inclus dans votre modèle
# Ne s'applique pas dans notre cas (nous avons inclus toutes les variables mesurées dans notre modèle)

#D. Vérifier la normalité : histogramme
hist(E1)

#4) Interprétation des résultats et visualisation des graphiques####
# Vérifiez le résumé du modèle
# Cela vous permet d'avoir une idée de la variance expliquée
# par les différentes composantes du modèle et la «significativité» des effets fixes 
summary(M8)

# Visualiser les résultats du modèle ####
# Il existe plusieurs façons de visualiser les résultats d'un modèle mixte, qui font tous appel au coefficients générés par le modèle.
# La première étape est d'obtenir les coefficients du modèle afin de les ajouter aux figures
coef(M8)
# Maintenant, mettez les coefs dans un tableau pour les rendre plus faciles à manipuler
Lake.coef <- as.data.frame(coef(M8)$Lake)
colnames(Lake.coef) <- c("Intercept","Slope")
Species.coef <- as.data.frame(coef(M8)$Fish_Species)
colnames(Species.coef) <- c("Intercept","Slope")

# Graphique 1 – toutes les données
# Graphique qui inclut toutes les données
plot <- ggplot(aes(Z_Length,Z_TP),data=data)
Plot_AllData <- plot + geom_point() + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="All Data") + fig
# Ajoutez un abline avec l'intercept et la pente de la relation entre la longueur et la position trophique
# Notez que vous pouvez obtenir l’origine et la pente du facteur fixe directement à partir du résumé du modèle
summary(M8)
Plot_AllData + geom_abline(intercept = -0.0009059, slope =0.4222697)

# Graphique 2 - Par Espèce 
# Colorez les données par espèce
Plot_BySpecies<-plot + geom_point(aes(colour = factor(Fish_Species)), size = 4) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Species") + fig
# Ajoutez les lignes de régression pour chaque espèce
Plot_BySpecies + geom_abline(intercept = Species.coef[1,1], slope =Species.coef[1,2], colour="coral2") + geom_abline(intercept = Species.coef[2,1], slope =Species.coef[2,2], colour = "green4") + geom_abline(intercept = Species.coef[3,1], slope =Species.coef[3,2], colour="blue1")

# Graphique 3 – Par Lac 
# Colorez les données par lac
Plot_ByLake<-plot + geom_point(aes(colour = factor(Lake)), size = 4) + xlab("Length (mm)") + ylab("Trophic Position") + labs(title="By Lake") + fig
# Ajouter les lignes de régression avec les intercepts spécifiques à chaque lac
Plot_ByLake + geom_abline(intercept = Lake.coef[1,1], slope =Lake.coef[1,2], colour="coral2") + geom_abline(intercept = Lake.coef[2,1], slope =Lake.coef[2,2], colour="khaki4") + geom_abline(intercept = Lake.coef[3,1], slope =Lake.coef[3,2], colour="green4") + geom_abline(intercept = Lake.coef[4,1], slope =Lake.coef[4,2], colour="darkgoldenrod") + geom_abline(intercept = Lake.coef[5,1], slope =Lake.coef[5,2], colour="royalblue1") + geom_abline(intercept = Lake.coef[6,1], slope =Lake.coef[6,2], colour="magenta3")

# Merci d'avoir assister à l'atelier et / ou utiliser ce code!
# On Espère que ce fu utile pour vous!