library(d3heatmap)
url <- "http://datasets.flowingdata.com/ppg2008.csv"
nba_players <- read.csv(url, row.names = 1)
d3heatmap(nba_players, scale = "column")


read.csv("molecular_count_gnps.csv")
interac_Count_gnps <- read.csv("molecular_count_gnps.csv", sep=",")
interaccountgnps_matrix <- data.matrix(interac_Count_gnps)
interaccountgnps_heatmap <- heatmap(interaccountgnps_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))


url <- "Molecular_unplasticcapsaicin.csv"
nba_players <- read.csv(url, row.names = 1)
d3heatmap(nba_players, scale = "column")

url <- "molecular_count_gnps3.csv"
nba_players <- read.csv(url, row.names = 1)
d3heatmap(nba_players, scale = "column")
