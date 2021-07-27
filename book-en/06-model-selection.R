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
