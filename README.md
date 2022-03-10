# [QCBS R Workshop Series](https://r.qcbs.ca/)

This series of 10 workshops serves as a guide to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current coordination team, listed [here](https://r.qcbs.ca/about/) or open a pull request (see contributing guidelines at <https://r.qcbs.ca/presenter-developer-protocol/developing-en.html>).

# [Série d'ateliers R du CSBQ](https://r.qcbs.ca/fr/)

Cette série de 10 ateliers servent de guide afin de maîtriser le logiciel R pour une grande variété d'analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d'utilisateurs de `R`.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter l'équipe de coordination de la série, listé [ici](https://r.qcbs.ca/fr/about/) ou ouvrez un pull request (voir les instructions <https://r.qcbs.ca/presenter-developer-protocol/developper-fr.html>).

# Workshop 7: Linear and generalized linear mixed models (LMM and GLMM)

Mixed effects models allow ecologists to overcome a number of limitations associated with traditional linear models. In this workshop, you will learn when it is important to use a mixed effects model to analyze your data. We will walk you through the steps to conduct a linear mixed model analysis, check its assumptions, report results, and visually represent your model in R.

# Atelier 7: Modèles linéaires et généralisés linéaires mixtes

Les modèles à effets mixtes permettent aux écologistes de surmonter un certain nombre de limitations liées aux modèles linéaires traditionnels. Dans cet atelier, vous apprendrez à déterminer si vous devez utiliser un modèle à effets mixtes pour analyser vos données. Nous allons vous guider à travers les étapes nécessaires pour utiliser un modèle linéaire mixte, vérifier les suppositions de base et présenter les résultats de votre modèle dans R.

# Workshop materials

Language | Slides | Bookdown | Script | GitHub 
:--------|:-------|:-----|:------ |:-------
EN | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Slides&message=07&color=red&logo=html5)](https://r.qcbs.ca/workshop07/pres-en/workshop07-pres-en.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=book&message=07&logo=github)](https://r.qcbs.ca/workshop07/book-en/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=07&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop07/book-en/workshop07-script-en.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop07) 
FR | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Diapos&message=07&color=red&logo=html5)](https://r.qcbs.ca/workshop07/pres-fr/workshop07-pres-fr.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=livre&message=07&logo=github)](https://r.qcbs.ca/workshop07/book-fr/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=07&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop07/book-fr/workshop07-script-fr.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop07) 

> *Note: The wiki for this workshop was converted to Bookdown in March 2021. <br> The wiki pages for this workshop will no longer be updated (Archive: [EN](https://wiki.qcbs.ca/r_workshop6), [FR](https://wiki.qcbs.ca/r_atelier6)).* 

# License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/).

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

# Contributions 

This workshop was originally developed by Catherine Baltazar, Dalal Hanna, Jacob Ziegler. Content about GLMMs was developed by Cédric Frenette Dussault, Vincent Fugère, Thomas Lamy, Zofia Taranu.

Since 2014, several QCBS members contributed to consistently and collaboratively develop and update this workshop, as part of the *Learning and Development Award* from the Québec Centre for Biodiversity Science. They were:

|      2022 - 2021 - 2020     |      2019 - 2018 - 2017     |      2016 - 2015 - 2014      |
|:---------------------------:|:---------------------------:|:----------------------------:|
| Maxime Fraser Franco    |     Nicolas Pinceloup   | Catherine Baltazar |
|  Hassen Allegue         |      Marie Hélène Brice  |        Dalal Hanna       |
|     Linley Sherin       |                      |       Jacob Ziegler       |
| Pedro Henrique P. Braga |                             |      Cédric Frenette Dussault  |
|   Katherine Hébert      |                             |    Vincent Fugère         |
|   Kevin Cazelles        |                             |    Thomas Lamy      |
|           |                             |    Zofia Taranu      |

# Development status

**Template** 

[![receive-from-template-and-dispatch-to-workflows](https://github.com/QCBSRworkshops/workshop07/workflows/receive-from-template-and-dispatch-to-workflows/badge.svg)](https://github.com/QCBSRworkshops/workshop07/actions?query=workflow%3Areceive-from-template-and-dispatch-to-workflows)

**Building workshop materials**

Language | Slides | Book
:------- | :----- | :-----
EN  | [![render-presentation-en](https://github.com/QCBSRworkshops/workshop07/workflows/render-presentation-en/badge.svg)](https://github.com/QCBSRworkshops/workshop07/actions?query=workflow%3Arender-presentation-en) | [![render-book-en](https://github.com/QCBSRworkshops/workshop07/workflows/render-book-en/badge.svg)](https://github.com/QCBSRworkshops/workshop07/actions?query=workflow%3Arender-book-en)
FR   | [![render-presentation-fr](https://github.com/QCBSRworkshops/workshop07/workflows/render-presentation-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop07/actions?query=workflow%3Arender-presentation-fr) | [![render-book-fr](https://github.com/QCBSRworkshops/workshop07/workflows/render-book-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop07/actions?query=workflow%3Arender-book-fr)
