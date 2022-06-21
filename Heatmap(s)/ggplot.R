animal <- "metazoa.csv"
mine.data <- read.csv(animal, row.names = 1)
str(mine.data)
library("tidyr")
mine.long <- pivot_longer(data = mine.data, 
                      cols = -c(Compound),
                      names_to = "Metabolito", 
                      values_to = "Abundance")
str(mine.data)

library("ggplot2")
mine.heatmap <- ggplot(data = mine.long, mapping = aes(x = Metabolito,
                                                       y = Compound,
                                                       fill = Abundance)) +
  geom_tile() +
  xlab(label = "Specie")

mine.heatmap
