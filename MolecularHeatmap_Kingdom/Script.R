#Heatmap Unclustering (red)

library(reshape2)

read.csv("molecular_count_gnps3.csv")
melt_mtcars <- read.csv("molecular_count_gnps3.csv")
melt_mtcars2 <- melt(melt_mtcars) 
melt_mtcars2$molecules <- rep(row.names(melt_mtcars2), 21)
head(melt_mtcars2)

library(ggplot2)

ggplot(melt_mtcars2, aes(variable, molecules)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red")


##Heatmap Unclustering (blue)
library(plyr)
library(scales)

#rescale values for all variables in melted data frame
melt_mtcars2 <- ddply(melt_mtcars2, .(variable), transform, rescale = rescale(value))

ggplot(melt_mtcars2, aes(variable, molecules)) +
  geom_tile(aes(fill = rescale), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")



# Interactive and clustering
library(d3heatmap)
url <- "molecular_count_gnps3.1.csv"
nba_players <- read.csv(url, row.names = 1)
d3heatmap(nba_players, scale = "column")

library(d3heatmap)
library(tidyverse)
url <- "molecular_count_gnps3.1.csv"
nba_players <- filter(read.csv(url, row.names = 1), FGA < 100)
d3heatmap(nba_players, scale = "column")

d3heatmap(nba_players, scale = "column", dendrogram = "none",
          color = "Blues")

d3heatmap(nba_players, colors = "Blues", scale = "col",
          dendrogram = "row", k_row = 3) 

d3heatmap(nba_players, colors = "Blues", scale = "col",
          dendrogram = "row", k_row = 3) 

# Unclustering and Interactive beatiful
library(d3heatmap)
library(magrittr)

d3heatmap(nba_players, dendrogram = 'none', key = TRUE, col = 'RdYlGn',
          scale = 'column', key.title = "Legend",
          notecol = 'white') %>% 
  hmAxis("x", title = "Metabolomes", location = 'bottom') %>% 
  hmAxis("y", title = "Molecules", location = 'left') %>% 
  hmCells(font.size = 8, color = 'blue') %>% 
  hmLegend(show = T, title = "Gradient", location = "tl")

# Uninteractive and clustering

library(pheatmap)
library(pheatbuilder)

read.csv("molecular_count_gnps3.1.csv")
datos <- read.csv("molecular_count_gnps3.1.csv")

pheatmap(datos)

library(magrittr)

datos %>%
  pheat() %>%
  pheat_cluster_rows() %>%
  pheat_display_main(border_color = NA)
