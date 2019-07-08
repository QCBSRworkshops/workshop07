# [QCBS R Workshop series](https://wiki.qcbs.ca/r)

This series of 10 workshops walks participants through the steps required to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current series coordinators, listed [here](https://wiki.qcbs.ca/r).

# [Série d'ateliers R du CSBQ](https://wiki.qcbs.ca/r)

Cette série de 10 ateliers guide les participants à travers les étapes requises afin de maîtriser le logiciel R pour une grande variété d’analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d’utilisateurs de R.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter les coordonnateurs actuels de la série, listés [ici](https://wiki.qcbs.ca/r).

# Workshop 6: Linear mixed effects models

[![Build Status](https://travis-ci.org/QCBSRworkshops/workshop06.svg?branch=dev)](https://travis-ci.org/QCBSRworkshops/workshop06)

Mixed effects models allow ecologists to overcome a number of limitations associated with traditional linear models. In this workshop, you will learn when it is important to use a mixed effects model to analyze your data. We will walk you through the steps to conduct a linear mixed model analysis, check its assumptions, report results, and visually represent your model in R.

# Atelier 6: Modèles linéaires à effets mixtes

[![Build Status](https://travis-ci.org/QCBSRworkshops/workshop06.svg?branch=dev)](https://travis-ci.org/QCBSRworkshops/workshop06)

Les modèles à effets mixtes permettent aux écologistes de surmonter un certain nombre de limitations liées aux modèles linéaires traditionnels. Dans cet atelier, vous apprendrez à déterminer si vous devez utiliser un modèle à effets mixtes pour analyser vos données. Nous allons vous guider à travers les étapes nécessaires pour utiliser un modèle linéaire mixte, vérifier les suppositions de base et présenter les résultats de votre modèle dans R.

# Links

#### [English](https://qcbsrworkshops.github.io/workshop06/workshop06-en/workshop06-en.html)

#### [Français](https://qcbsrworkshops.github.io/workshop06/workshop06-fr/workshop06-fr.html)

# Developers

1. Set the working directory set to this folder.
2. then use:

``` r
install.packages("remotes")
remotes::install_github("QCBSRworkshops/qcbsRworkshops")
library("qcbsRworkshops")
build_workshops()
```
