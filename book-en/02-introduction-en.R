# Load the dataset
fish.data <- read.csv('data/qcbs_w7_data.csv', stringsAsFactors = TRUE) 
# This line will vary depending on where your data is saved. 
# Check your working directory with getwd() and change it, if needed, with setwd()

# Simple theme for all ggplot figures we produce after this
fig <- theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.background=element_blank(), 
        strip.background=element_blank(), 
        strip.text.y = element_text(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        panel.border = element_rect(colour="black", fill = NA))


# Exploring the data graphically!

# Basic plot aesthetics for the relationship we care about
plot <- ggplot(aes(Fish_Length, Trophic_Pos), data = fish.data)

# Plot 1 - All data
plot + geom_point() + 
  labs(x = "Length (mm)", y = "Trophic Position", 
       title = "All Data") + 
  fig

# Plot 2 - By species
plot + geom_point() + 
  facet_wrap(~ Fish_Species) + 
  labs(x = "Length (mm)", y = "Trophic Position", 
       title = "By Species") + 
  fig

# Plot 3 – By lake
plot + geom_point() + 
  facet_wrap(~ Lake) + 
  labs(x = "Length (mm)", y = "Trophic Position", 
       title = "By Lake") + 
  fig
