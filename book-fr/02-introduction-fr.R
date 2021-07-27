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
