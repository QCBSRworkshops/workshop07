# Name: Amanda Winegardner, Xavier Giroux-Bougard, Bérenger Bourgeois, Emmanuelle Chrétien and Monica Granados
# Date: February 2016
# Description: Ordination
# Dataset: File names are "DoubsSpe.csv", "DoubsEnv.csv"
# Notes: Material in R script obtained from
#        Borcard D., Gillet F. et Legendre P., 2011. Numerical Ecology with R. Springer.
#***************************************************************************************#


# 0. Loading the data ####

rm(list=ls())
install.packages("vegan")
install.packages("gclus")
install.packages("ape")
library(vegan)
library(gclus)
library(ape)
source(file.choose()) # fichier coldiss.R

# Species community data frame (fish abundance): "DoubsSpe.csv"
spe <- read.csv(file.choose(), row.names=1)
spe <- spe[-8,] # site number 8 contains no species and must be removed

# Environmental data frame: "DoubsEnv.csv"
env <- read.csv(file.choose(), row.names=1)
env <- env[-8,]

#***************************************************************************************#

# 1.Explore the data ####

## 1.1 Species data ####
names(spe)
dim(spe)
str(spe)
head(spe)
summary(spe)

### Species distibution, all species confounded
(ab <- table(unlist(spe)))
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=grey(5:0/5))

### Number of absences
sum(spe==0)

### Proportion of zeros in the community data set
sum(spe==0)/(nrow(spe)*ncol(spe))

### Species occurences
spe.pres <- colSums(spe>0) # compute the number of sites where each species is present
hist(spe.pres, main="Species occurence", las=1, xlab="Frequency of occurences", ylab="Number of species", breaks=seq(0,30, by=5), col="grey")

### Sites richness
site.pres <- rowSums(spe>0) # compute the number of species at ecah site
hist(site.pres, main="Species richness", las=1, xlab="Frequency of sites", ylab="Number of species", breaks=seq(0,30, by=5), col="grey")

### Compute diversity indices
?diversity
(H <- diversity(spe, index="shannon")) # Shannon diversity index
(N <- diversity(spe, index="simpson")) # Simpson diversity index

## 1.2 Environmental data ####
names(env)
dim(env)
str(env)
head(env)
summary(env)
pairs(env, main="Bivariate Plots of the Environmental Data" )

# Standardization of the 11 environmental data
env.z <- decostand(env, method="standardize")
apply(env.z, 2, mean) # the data are now centered (means~0)
apply(env.z, 2, sd)   # the data are now scaled (standard deviations=1)

# Note: explanatory variables expressed in different units
#       must obligatory be standardized before computing distances measures and ordination analysis.

#***************************************************************************************#

# 2.Association measures ####

## 2.1. Distance measures ####

### Quantitative species data
?vegdist # this function compute dissimilarity indices useful for community decomposition data (see method options)
(spe.db<-vegdist(spe, method="bray")) # Bray-Curtis dissimilarity
(spe.dj<-vegdist(spe, method="jac")) # Jaccard dissimilarity
(spe.dg<-vegdist(spe, method="gower")) # Gower dissimilarity

### Presence-Absence species data
(spe.db.pa<-vegdist(spe, method="bray", binary=TRUE)) #Bray-Curtis dissimilarity
(spe.dj.pa<-vegdist(spe, method="jac", binary=TRUE)) # Jaccard dissimilarity
(spe.dg.pa<-vegdist(spe, method="gower", binary=TRUE)) # Gower dissimilarity

### Graphical display of association matrices: heat maps
windows() #use x11() or quartz() on Mac
coldiss(spe.db, byrank=FALSE, diag=TRUE) # Heat map of Bray-Curtis dissimilarity
windows()
coldiss(spe.dj, byrank=FALSE, diag=TRUE) # Heat map of Jaccard dissimilarity
windows()
coldiss(spe.dg, byrank=FALSE, diag=TRUE) # Heat map of Gower dissimilarity

### Quantitative environmental data
#### Association between environmental data (Q mode)
?dist # this function also compute dissimilarity matrix
env.de <- dist(env.z, method = "euclidean") # euclidean distance matrix of the standardized environmental variables
windows()
coldiss(env.de, diag=TRUE)

### Dependance between environmental data (R mode)
(env.pearson<-cor(env)) # Pearson r linear correlation
round(env.pearson, 2)
(env.ken<-cor(env, method="kendall")) # Kendall tau rank correlation
round(env.ken, 2)

### Mixed types of environmental variables
#### Association between mixed types of environmental data (Q mode)
##### Create a fictious data frame
var.g1 <- rnorm(30, 0, 1)
var.g2 <- runif(30, 0, 5)
var.g3 <- gl(3, 10)
var.g4 <- gl(2, 5, 30)
(dat2 <- data.frame(var.g1, var.g2, var.g3, var.g4))
str(dat2)
summary(dat2)
##### Compute the Gower dissimilarity matrix
?daisy
(dat2.dg <- daisy(dat2, metric="gower"))
coldiss(dat2.dg)

# Challenge 1 Advanced ####

# Calculate by hand the Bray-Curtis and the Gower dissimilarity of species abundance CHA, TRU and VAI for sites 1, 2 and 3
# (answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Bray-Curtis dissimilarity : d[jk] = (sum abs(x[ij]-x[ik]))/(sum (x[ij]+x[ik]))
(spe.challenge<-spe[1:3,1:3])
# Calculate the total species abundance by site
(Abund.s1<-sum(spe.challenge[1,]))
(Abund.s2<-sum(spe.challenge[2,]))
(Abund.s3<-sum(spe.challenge[3,]))
# Calculate the species abundance differences between pairs of sites for each species
Spec.s1s2<-0
Spec.s1s3<-0
Spec.s2s3<-0
for (i in 1:3) {
  Spec.s1s2<-Spec.s1s2+abs(sum(spe.challenge[1,i]-spe.challenge[2,i]))
  Spec.s1s3<-Spec.s1s3+abs(sum(spe.challenge[1,i]-spe.challenge[3,i]))
  Spec.s2s3<-Spec.s2s3+abs(sum(spe.challenge[2,i]-spe.challenge[3,i])) }
# Calculate the Bray-Curtis dissimilarity
(db.s1s2<-Spec.s1s2/(Abund.s1+Abund.s2))
(db.s1s3<-Spec.s1s3/(Abund.s1+Abund.s3))
(db.s2s3<-Spec.s2s3/(Abund.s2+Abund.s3))
# Compare your results
(spe.db.challenge<-vegdist(spe.challenge, method="bray"))

# Gower dissimilarity :    d[jk] = (1/M) sum(abs(x[ij]-x[ik])/(max(x[i])-min(x[i])))
# Calculate the number of columns in your dataset
M<-ncol(spe.challenge)
# Calculate the species abundance differences between pairs of sites for each species
Spe1.s1s2<-abs(spe.challenge[1,1]-spe.challenge[2,1])
Spe2.s1s2<-abs(spe.challenge[1,2]-spe.challenge[2,2])
Spe3.s1s2<-abs(spe.challenge[1,3]-spe.challenge[2,3])
Spe1.s1s3<-abs(spe.challenge[1,1]-spe.challenge[3,1])
Spe2.s1s3<-abs(spe.challenge[1,2]-spe.challenge[3,2])
Spe3.s1s3<-abs(spe.challenge[1,3]-spe.challenge[3,3])
Spe1.s2s3<-abs(spe.challenge[2,1]-spe.challenge[3,1])
Spe2.s2s3<-abs(spe.challenge[2,2]-spe.challenge[3,2])
Spe3.s2s3<-abs(spe.challenge[2,3]-spe.challenge[3,3])
# Calculate the range of each species abundance between sites
Range.spe1<-max(spe.challenge[,1]) - min (spe.challenge[,1])
Range.spe2<-max(spe.challenge[,2]) - min (spe.challenge[,2])
Range.spe3<-max(spe.challenge[,3]) - min (spe.challenge[,3])
# Calculate the Gower dissimilarity
(dg.s1s2<-(1/M)*((Spe2.s1s2/Range.spe2)+(Spe3.s1s2/Range.spe3)))
(dg.s1s3<-(1/M)*((Spe2.s1s3/Range.spe2)+(Spe3.s1s3/Range.spe3)))
(dg.s2s3<-(1/M)*((Spe2.s2s3/Range.spe2)+(Spe3.s2s3/Range.spe3)))
# Compare your results
(spe.db.challenge<-vegdist(spe.challenge, method="gower"))


## 2.2. Transformations for community composition data ####

?decostand # this function provide some standardiztion methods for community domposition data (see method options)
# Transform abundances to absence-presence (1-0)
(spe.pa<-decostand(spe, method="pa"))

# Hellinger transformation
(spe.hel<-decostand(spe, method="hellinger"))

# Chi.square transformation
(spe.chi<-decostand(spe, method="chi.square"))


# Challenge 2 Advanced ####

# Calculate by hand the Hellinger and the Chi-square transformed abundances data
# (answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Hellinger tranformation
# First calculate the total species abundances by site
(site.totals<-apply(spe, 1, sum))
# Scale species abundances by dividing them by site totals
(scale.spe<-spe/site.totals)
# Calculate de square root of scaled species abundances
(sqrt.scale.spe<-sqrt(scale.spe))
# Compare the results
sqrt.scale.spe
spe.hel
sqrt.scale.spe-spe.hel # or: sqrt.scale.spe/spe.hel

# Chi-squre transformation
# First calculate the total species abundances by site
(site.totals<-apply(spe, 1, sum))
# Then calculate the square root of total species abundances
(sqrt.spe.totals<-sqrt(apply(spe, 2, sum)))
# Scale species abundances by dividing them by the site totals and the species totals
scale.spe2<-spe
for (i in 1:nrow(spe)) {
  for (j in 1:ncol(spe)) {
    (scale.spe2[i,j]<-scale.spe2[i,j]/(site.totals[i]*sqrt.spe.totals[j]))   }}
#Adjust the scale abundance species by multplying by the square root of the species matrix total
(adjust.scale.spe2=scale.spe2*sqrt(sum(rowSums(spe))))
# Compare the results
adjust.scale.spe2
spe.chi
adjust.scale.spe2-spe.chi # or: adjust.scale.spe2/spe.chi



## 2.3 Clustering ####

### Calculating Hellinger distance
spe.dhel<-vegdist(spe.hel,method="euclidean") #generates the distance matrix from Hellinger transformed data

# See difference between the two matrices
head(spe.hel)# Hellinger-transformed species data
head(spe.dhel)# Hellinger distances among sites

### Comparison of single and complete linkage clustering

#Perform single linkage clustering
spe.dhel.single<-hclust(spe.dhel, method=”single”)
plot(spe.dhel.single)

#Perform complete linkage clustering
spe.dhel.complete<-hclust(spe.dhel, method=”complete”)
plot(spe.dhel.complete)

### Ward's minimum variance method

# Perform Ward minimum variance clustering
spe.dhel.ward<-hclust(spe.dhel, method=”ward.D2”)
plot(spe.dhel.ward)

# Re-plot the dendrogram by using the square roots of the fusion levels
spe.dhel.ward$height<-sqrt(spe.dhel.ward$height)
plot(spe.dhel.ward)
plot(spe.dhel.ward, hang=-1) # hang=-1 aligns all objets on the same line



#***************************************************************************************#

# 3.Unconstrained Ordination ####

## 3.1. Principal Components Analysis (PCA) ####

### PCA on hellinger-transformed species data
?rda
#### Run the PCA
spe.h.pca <- rda(spe.hel)
# Extract the results
summary(spe.h.pca)

#### Extract specific parts of the results
summary(spe.h.pca) # overall results
summary(spe.h.pca, display=NULL) # only eigenvalues and their contribution to the variance
eigen(cov(spe.hel)) # also compute the eigenvalues
(spe.scores <- scores(spe.h.pca, display="species", choices=c(1,2))) # species scores on the first two PCA axes
(site.scores <- scores(spe.h.pca, display="sites", choices=c(1,2))) # sites scores on the first two PCA axes
#Note: if you don’t specify the number of principal components to extract (e.g. choices=c(1,2) or choices=c(1:2) then all of the scores will be extracted for all of the principal components.

# Identify the significant axis using the Kaiser-Guttman criterion
ev <- spe.h.pca$CA$eig
ev[ev>mean(ev)]
n <- length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

### PCA on the environmental variables
#### Run the PCA
env.pca <- rda(env.z) # or rda(env, scale=TRUE)

#### Extract the results
summary(env.pca)
summary(env.pca, scaling=2)

# Identify the significant axis using the Kaiser-Guttman criterion
ev <- env.pca$CA$eig
ev[ev>mean(ev)]
n <- length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")


### Construct a biplot
#### Biplot of the PCA on transformed species data (scaling 1)
windows()
plot(spe.h.pca)
windows()
?biplot
biplot(spe.h.pca)
windows()
plot(spe.h.pca, scaling=1, type="none", # scaling 1 = distance biplot :
     # distances among abjects in the biplot approximate their Euclidean distances
     # but angles among descriptor vectors DO NOT reflect their correlation
     xlab=c("PC1 (%)", round((spe.h.pca$CA$eig[1]/sum(spe.h.pca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((spe.h.pca$CA$eig[2]/sum(spe.h.pca$CA$eig))*100,2)))
points(scores(spe.h.pca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.h.pca, display="species", choices=c(1), scaling=1),
     scores(spe.h.pca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(spe.h.pca, display="species", scaling=1)),
     col="red", cex=0.8)

# Add arrows for species scores
spe.sc<-scores(spe.h.pca,choices=1:2,scaling=,display="sp")
arrows(0,0,spe.sc[,1],spe.sc[,2],length=0)

#### Biplot on the environmental variables (scaling 2)
windows()
plot(env.pca)
windows()
plot(env.pca, scaling=2, type="none", # scaling 2 = correlation biplot :
     # distances among abjects in the biplot DO NOT approximate their Euclidean distances
     # but angles among descriptor vectors reflect their correlation
     xlab=c("PC1 (%)", round((env.pca$CA$eig[1]/sum(env.pca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((env.pca$CA$eig[2]/sum(env.pca$CA$eig))*100,2)),
     xlim=c(-1,1), ylim=c(-1,1))
points(scores(env.pca, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="darkgreen", cex=1.2)
text(scores(env.pca, display="species", choices=c(1), scaling=2),
     scores(env.pca, display="species", choices=c(2), scaling=2),
     labels=rownames(scores(env.pca, display="species", scaling=2)),
     col="red", cex=0.8)


#### Abundance of species TRU along an oligotrophic-eutrophic gradient
Sites_scores_Env_Axis1<- scores(env.pca, display="sites", choices=c(1), scaling=2)
spe$ANG
plot( Sites_scores_Env_Axis1, spe$TRU)
summary(lm(spe$TRU~Sites_scores_Env_Axis1))
abline(lm(spe$TRU~Sites_scores_Env_Axis1))

# Challenge 3 ####
# Run the PCA of the mite species abundances
# Which are the significant axes ?
# Which group of sites can you identify ?
# Which species are related to each group of sites
data(mite)
mite.spe <- mite #data from the vegan package
# (answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
mite.spe.hel<-decostand(mite.spe, method="hellinger")
mite.spe.h.pca<-rda(mite.spe.hel)
ev<-mite.spe.h.pca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
summary(mite.spe.h.pca, display=NULL)
windows()
plot(mite.spe.h.pca, scaling=1, type="none",
     xlab=c("PC1 (%)", round((mite.spe.h.pca$CA$eig[1]/sum(mite.spe.h.pca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((mite.spe.h.pca$CA$eig[2]/sum(mite.spe.h.pca$CA$eig))*100,2)))
points(scores(mite.spe.h.pca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(mite.spe.h.pca, display="species", choices=c(1), scaling=1),
     scores(mite.spe.h.pca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(mite.spe.h.pca, display="species", scaling=1)),
     col="red", cex=0.8)

## 3.2. Correspondence Analysis (CA) ####
?cca
### Run the CA on species abundances
spe.ca<-cca(spe)

### Identify the significant axis using the Kaiser-Guttman criterion
ev<-spe.ca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

### Extract the results
spe.ca
summary(spe.ca, display=NULL)

### Construct a biplot
par(mfrow=c(1,2))
#### scaling 1
plot(spe.ca, scaling=1, type="none", main='CA - biplot scaling 1',
     xlab=c("CA1 (%)", round((spe.ca$CA$eig[1]/sum(spe.ca$CA$eig))*100,2)),
     ylab=c("CA2 (%)", round((spe.ca$CA$eig[2]/sum(spe.ca$CA$eig))*100,2)))
points(scores(spe.ca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.ca, display="species", choices=c(1), scaling=1),
     scores(spe.ca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(spe.ca, display="species", scaling=1)),
     col="red", cex=0.8)
#### scaling 2
plot(spe.ca, scaling=1, type="none", main='CA - biplot scaling 2',
     xlab=c("CA1 (%)", round((spe.ca$CA$eig[1]/sum(spe.ca$CA$eig))*100,2)),
     ylab=c("CA2 (%)", round((spe.ca$CA$eig[2]/sum(spe.ca$CA$eig))*100,2)), ylim=c(-2,3))
points(scores(spe.ca, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.ca, display="species", choices=c(1), scaling=2),
     scores(spe.ca, display="species", choices=c(2), scaling=2),
     labels=rownames(scores(spe.ca, display="species", scaling=2)),
     col="red", cex=0.8)



# Challenge 4 ####
# Run the CA of the mite species abundances
# Which are the significant axes ?
# Which group of sites can you identify ?
# Which species are related to each group of sites ?
# (answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
mite.spe.ca<-cca(mite.spe)
ev<-mite.spe.ca$CA$eig
ev[ev>mean(ev)]
n=length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red")
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
summary(mite.spe.ca, display=NULL)
windows()
plot(mite.spe.ca, scaling=1, type="none",
     xlab=c("PC1 (%)", round((mite.spe.ca$CA$eig[1]/sum(mite.spe.ca$CA$eig))*100,2)),
     ylab=c("PC2 (%)", round((mite.spe.ca$CA$eig[2]/sum(mite.spe.ca$CA$eig))*100,2)))
points(scores(mite.spe.ca, display="sites", choices=c(1,2), scaling=1),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(mite.spe.ca, display="species", choices=c(1), scaling=1),
     scores(mite.spe.ca, display="species", choices=c(2), scaling=1),
     labels=rownames(scores(mite.spe.ca, display="species", scaling=1)),
     col="red", cex=0.8)


## 3.3. Principal Coordinate Anaysis (PCoA) ####
### Run the PCoA on hellinger transformed species abundances
?cmdscale
cmdscale(dist(spe.hel), k=(nrow(spe)-1), eig=TRUE)
?pcoa
spe.h.pcoa<-pcoa(dist(spe.hel))
### Extract the results
spe.h.pcoa
### Construct the biplot
windows()
biplot.pcoa(spe.h.pcoa, spe.hel)

### Run the PCoA on a Bray-Curtis dissimilarity matrix
spe.db
spe.bray.pcoa<-pcoa(spe.db, correction="cailliez")
spe.bray.pcoa
windows()
biplot.pcoa(spe.bray.pcoa)
# The distance measure choosen thus strongly influenced the PCoA results

# Challenge 5 ####
# Run the PCoA of the hellinger-transformed mite species abundances
# Which are the significant axes ?
# Which group of sites can you identify ?
# Which species are related to each group of sites ?
# How does the PCoA results compare with the PCA
# (answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
mite.spe.h.pcoa<-pcoa(dist(mite.spe.hel))
mite.spe.h.pcoa
windows()
biplot.pcoa(mite.spe.h.pcoa, mite.spe.hel)


## 3.4. Nonmetric Multidimensional Scaling (NMDS) ####
?metaMDS
### Run the NMDS of species abundances based on the Bray-Curtis distance in 2 dimensions
spe.nmds<-metaMDS(spe, distance='bray', k=2) # argument k precise the number of dimensions choosen (i.e. the number of ordination axis)

### Extract the results
spe.nmds
spe.nmds$stress

### Assess the goodness of fit
spe.nmds$stress
stressplot(spe.nmds, main='Shepard plot')

### Construct the biplot
windows()
plot(spe.nmds, type="none", main=paste('NMDS/Bray - Stress=', round(spe.nmds$stress, 3)),
     xlab=c("NMDS1"),
     ylab=c("NMDS2"))
points(scores(spe.nmds, display="sites", choices=c(1,2)),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(spe.nmds, display="species", choices=c(1)),
     scores(spe.nmds, display="species", choices=c(2)),
     labels=rownames(scores(spe.nmds, display="species")),
     col="red", cex=0.8)

# Challenge 6 ####
# Run the NMDS of the mite species abundances in 2 dimensions based on a Bray-Curtis distance
# Assess the goodness-of-fit of the ordination and interpret the biplot
# (answer below)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
mite.spe.nmds<-metaMDS(mite.spe, distance='bray', k=2)
### Extract the results
mite.spe.nmds

### Assess the goodness of fit
mite.spe.nmds$stress
stressplot(mite.spe.nmds, main='Shepard plot')

### Construct the biplot
windows()
plot(mite.spe.nmds, type="none", main=paste('NMDS/Bray - Stress=', round(mite.spe.nmds$stress, 3)),
     xlab=c("NMDS1"),
     ylab=c("NMDS2"))
points(scores(mite.spe.nmds, display="sites", choices=c(1,2)),
       pch=21, col="black", bg="steelblue", cex=1.2)
text(scores(mite.spe.nmds, display="species", choices=c(1)),
     scores(mite.spe.nmds, display="species", choices=c(2)),
     labels=rownames(scores(mite.spe.nmds, display="species")),
     col="red", cex=0.8)

