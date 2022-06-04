
#links where I took the information to build this script
https://www.rpubs.com/Knight95/volcano
https://www.biostars.org/p/214100/
https://stackoverflow.com/questions/61615727/how-can-i-add-a-legend-that-counts-points-above-or-below-a-certain-value-in-ggpl
https://stackoverflow.com/questions/59962457/volcano-plot-in-r-adding-details-coloring-common-factors-only
https://www.biostars.org/p/341420/


#First, set the working directory (WD). 
#In it, You must deposit all the files.

WD <- getwd()

#This is to read the output file from Metaboanalyst
volcano <- as.data.frame(read.csv(paste0(WD,"\\Metaboanalyst\\volcano.csv"),
	stringsAsFactors=FALSE)) 

#Load the required libraries
library(data.table)
library("purrr") #purrr is a functional programming toolkit.
library(dplyr) 
library(sqldf)
library(stringr)
library("ggplot2")
pacman::p_load(here,  
               tidyverse, 
               janitor, # Cleaning column names  
               scales, # Transform axis scales   
               ggrepel) # Optimise plot label separation  


#Set the threshold values the log fold-change of 2 is 0.5849625
#It could be replaced for other

volcano$expression = ifelse(volcano$log2.FC.> 0.5849625 ,'Up','Down')
summary <- volcano %>% count(expression)

ggplot(data=volcano, aes(log2.FC., y=-log10(raw.pval), group =  expression, colour = expression)) +
geom_point(alpha = 0.5, size = 4) +
  scale_color_manual(values=c("blue","red")) + #blue down-regulated & red up-regulated
  xlim(c(-10, 10)) +
  geom_vline(xintercept=c(-0.5849625,0.5849625),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = 1.30103,lty=4,col="black",lwd=0.8) +
  labs(x="log2(fold change)",
       y="-log10 (p-value)",
       title="XXXX")  + #This must be replaced for the tittle that you want
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5, face = "bold")) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.5, size = rel(1.5))) +
  theme(axis.text.y = element_text(size = rel(1.5))) +
  theme(axis.title = element_text(size = 20)) + #Axis Labels size
  theme(legend.text = element_text(size = 15)) + theme(legend.title = element_blank()) +
  annotate(
        geom='label',
        x=min(volcano$log2.FC.), y=0.8*max(-log10(volcano$raw.pval)),
        hjust=0,
        label=paste0('n=',summary[1,2])
    ) +
    annotate(
        geom='label',
        x=max(volcano$log2.FC.), y=0.8*max(-log10(volcano$raw.pval)),
        hjust=1,
        label=paste0('n=',summary[2,2]))

ggsave( "volcano.png",units = "px", width = 3000, height = 3000)

