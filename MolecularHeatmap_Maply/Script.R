library("heatmaply")

read.csv("Archaeplastida.csv")
melt_mtcars <- read.csv("Archaeplastida.csv")

heatmaply(melt_mtcars)
