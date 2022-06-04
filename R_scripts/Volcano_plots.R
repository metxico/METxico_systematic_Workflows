
#Links
https://www.rpubs.com/Knight95/volcano
https://www.biostars.org/p/214100/
https://stackoverflow.com/questions/61615727/how-can-i-add-a-legend-that-counts-points-above-or-below-a-certain-value-in-ggpl
https://stackoverflow.com/questions/59962457/volcano-plot-in-r-adding-details-coloring-common-factors-only
https://www.biostars.org/p/341420/

volcano_assoc_bean <- as.data.frame(read.csv(paste0(WD,"\\Metaboanalyst\\association_vs_bean_QCFiltered_QuantileNormalization_RawPvalue_FC1.5\\Download (1)\\volcano.csv"),
	stringsAsFactors=FALSE)) 

library("ggplot2")
pacman::p_load(here,  
               tidyverse, 
               janitor, # Cleaning column names  
               scales, # Transform axis scales   
               ggrepel) # Optimise plot label separation  


#Association vs Bean

volcano_assoc_bean$expression = ifelse(volcano_assoc_bean$log2.FC.> 0.5849625 ,'Up','Down')
summary_assoc_bean <- volcano_assoc_bean %>% count(expression)

ggplot(data=volcano_assoc_bean, aes(log2.FC., y=-log10(raw.pval), group =  expression, colour = expression)) +
geom_point(alpha = 0.5, size = 4) +
  scale_color_manual(values=c("blue","red")) +
  xlim(c(-10, 10)) +
  geom_vline(xintercept=c(-0.5849625,0.5849625),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = 1.30103,lty=4,col="black",lwd=0.8) +
  labs(x="log2(fold change)",
       y="-log10 (p-value)",
       title="Association vs Bean")  +
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
        x=min(volcano_assoc_bean$log2.FC.), y=0.8*max(-log10(volcano_assoc_bean$raw.pval)),
        hjust=0,
        label=paste0('n=',summary_assoc_bean[1,2])
    ) +
    annotate(
        geom='label',
        x=max(volcano_assoc_bean$log2.FC.), y=0.8*max(-log10(volcano_assoc_bean$raw.pval)),
        hjust=1,
        label=paste0('n=',summary_assoc_bean[2,2]))

ggsave( "volcano_AssocvsBean.png",units = "px", width = 3000, height = 3000)


#Association vs Corn
volcano_assoc_corn$expression = ifelse(volcano_assoc_corn$log2.FC.> 0.5849625 ,'Up','Down')
summary_assoc_corn <- volcano_assoc_corn %>% count(expression)

ggplot(data=volcano_assoc_corn, aes(log2.FC., y=-log10(raw.pval), group =  expression, colour = expression)) +
geom_point(alpha = 0.5, size = 4) +
  scale_color_manual(values=c("blue","red")) +
  xlim(c(-10, 10)) +
  geom_vline(xintercept=c(-0.5849625,0.5849625),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = 1.30103,lty=4,col="black",lwd=0.8) +
  labs(x="log2(fold change)",
       y="-log10 (p-value)",
       title="Association vs Corn")  +
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
        x=min(volcano_assoc_corn$log2.FC.), y=0.8*max(-log10(volcano_assoc_corn$raw.pval)),
        hjust=0,
        label=paste0('n=',summary_assoc_corn[1,2])
    ) +
    annotate(
        geom='label',
        x=max(volcano_assoc_corn$log2.FC.), y=0.8*max(-log10(volcano_assoc_corn$raw.pval)),
        hjust=1,
        label=paste0('n=',summary_assoc_corn[2,2]))

ggsave( "volcano_AssocvsCorn.png",units = "px", width = 3000, height = 3000)

#Bean vs Corn
volcano_bean_corn$expression = ifelse(volcano_bean_corn$log2.FC.> 0.5849625 ,'Up','Down')
summary_bean_corn <- volcano_bean_corn %>% count(expression)

ggplot(data=volcano_bean_corn, aes(log2.FC., y=-log10(raw.pval), group =  expression, colour = expression)) +
geom_point(alpha = 0.5, size = 4) +
  scale_color_manual(values=c("blue","red")) +
  xlim(c(-10, 10)) +
  geom_vline(xintercept=c(-0.5849625,0.5849625),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = 1.30103,lty=4,col="black",lwd=0.8) +
  labs(x="log2(fold change)",
       y="-log10 (p-value)",
       title="Bean vs Corn")  +
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
        x=min(volcano_bean_corn$log2.FC.), y=0.8*max(-log10(volcano_bean_corn$raw.pval)),
        hjust=0,
        label=paste0('n=',summary_bean_corn[1,2])
    ) +
    annotate(
        geom='label',
        x=max(volcano_bean_corn$log2.FC.), y=0.8*max(-log10(volcano_bean_corn$raw.pval)),
        hjust=1,
        label=paste0('n=',summary_bean_corn[2,2]))

ggsave( "volcano_BeanvsCorn.png",units = "px", width = 3000, height = 3000)