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

# popu seulement
mpl1.popu <- glmer(total.fruits ~ nutrient*amd + rack + status + 
                     (1|X) +
                     (1|popu), 
                     data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))
 
# gen seulement
mpl1.gen <-glmer(total.fruits ~ nutrient*amd + rack + status + 
                   (1|X) +
                   (1|gen), 
                   data=dat.tf, family="poisson", control=glmerControl(optimizer="bobyqa"))
 
# Approche AICc
ICtab(mpl1, mpl1.popu, mpl1.gen, type = c("AICc"))

# Approche fréquentiste (Likelihood Ratio Test)
anova(mpl1,mpl1.popu)

anova(mpl1,mpl1.gen)

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
