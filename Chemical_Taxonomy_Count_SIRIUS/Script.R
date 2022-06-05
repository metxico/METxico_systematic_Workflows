#AUTOR: Jorge Xool-Tamayo and Alan Hernández-Melgar

Chemodata (file)
library (plotly)
Username (chart_studio)
API key (chart_studio)

#Chemoinput file

chemoclassTREELIFE <- read.csv(file='chemoCLASS_treeLIFE.csv')
chemosuperclassTREELIFE <- read.csv(file='chemoSUPERCLASS_treeLIFE.csv')


#Data frames contruction
chemoclassTREELIFE <- data.frame(Class = c(chemoclassTREELIFE$Class), Annotation = c(chemoclassTREELIFE$Annotation)) #ChemoclassDataframe
chemosuperclassTREELIFE <- data.frame(Superclass = c(chemosuperclassTREELIFE$Superclass), Annotation = c(chemosuperclassTREELIFE$Annotation)) #ChemosuperclassDataframe

#Chemopie chart core
fig_chemoclasstreelife <- plot_ly(chemoclassTREELIFE, labels = ~Class, values = ~Annotation, type = 'pie', 
  textfont = list(size = 22))%>% #Text size within the slides
        layout(font=list(size = 30), #Text size of the legend box
          legend = list(title=list(text='<b> Class </b>'), y=0.5)) #Legend title
fig_chemoclasstreelife

fig_chemosuperclasstreelife <- plot_ly(chemosuperclassTREELIFE, labels = ~Superclass, values = ~Annotation, type = 'pie', 
  textfont = list(size = 23))%>% #Text size within the slides
        layout(font=list(size = 30), #Text size of the legend box
          legend = list(title=list(text='<b> Superclass </b>'), x= 100, y=0.5))
fig_chemosuperclasstreelife

#Chart studio
Sys.setenv("write the words: plotly_username"="write: your_plotly_username")
Sys.setenv("Write the words: plotly_api_key"="Write: your_api_key")

#Chemoutput file
plotly_IMAGE(fig_chemoclasstreelife, format="png", width = 1920, height = 1080, out_file="CHEMOCLASStreelife.png")
plotly_IMAGE(fig_chemosuperclasstreelife, format="png", width = 1920, height = 1080, out_file="CHEMOSUPERCLASStreelife.png")



#We open to collaboration, thanks for saw R.workflow.

