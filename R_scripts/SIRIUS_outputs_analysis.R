library(data.table)
library(Rdisop) #To use package getMolecule https://www.rdocumentation.org/packages/Rdisop/versions/1.32.0/topics/getMolecule
library(patRoon) #aims to provide comprehensive mass spectrometry based non-target analysis (NTA) workflows for environmental analysis. (Used to adducts) https://github.com/rickhelmus/patRoon
library("purrr") #purrr is a functional programming toolkit. https://www.geeksforgeeks.org/apply-function-to-every-value-in-r-dataframe/
library(dplyr) 
library("webchem")
library(sqldf)
library(RColorBrewer)
library(plotly)
library(stringr)

#First, set the working directory (WD). 
#In it, You must deposit all the output files from SIRIUS.

WD <- getwd()
canopus_summary <- as.data.frame(fread(paste0(WD, '\\canopus_summary.tsv')))
compound_identifications <- as.data.frame(fread(paste0(WD,'\\compound_identifications.tsv')))
canopus_summary_and_compound_identifications_merged <- merge(canopus_summary,compound_identifications, by.x = "name", by.y = "id")

custom_x <- function(x,y){
  calculateIonFormula(x, y)
} #Function to calculate ion formula plus adduct

MolecularFormulaIon <- mapply(custom_x, canopus_summary_and_compound_identifications_merged$molecularFormula.y, 
	canopus_summary_and_compound_identifications_merged$adduct.y)

#add column called 'MolecularFormulaIon' in a new dataframe
merged_df2 <- cbind(canopus_summary_and_compound_identifications_merged, MolecularFormulaIon)

custom_f <- function(x){
  getMass(getMolecule(x))
} #Function to calculate theoretical mass

#Insert theoretical mass to the new dataframe
merged_df2$Theoretical.mass <- map_dbl(merged_df2$MolecularFormulaIon,.f=custom_f)

custom_ppm <- function(x,y){
  ((1-(y/x))*1000000)
} #Function to calculate mass error in ppm

#Insert mass error to the new dataframe
merged_df2$Mass.accuracy..ppm.<- mapply(custom_ppm, merged_df2$ionMass, 
                                               merged_df2$Theoretical.mass)

#Retention time to minutes
custom_TimeInMinutes <- function(x) {
  (x)/60
}

TimeInMinutes <- mapply(custom_TimeInMinutes, merged_df2$retentionTimeInSeconds)

#Insert time in minutes to the new data frame
merged_df2 <- cbind(merged_df2, TimeInMinutes)

#Replace empty values from classification with "Unassigned"

merged_df2$superclass[merged_df2$superclass == ""] <- "Unassigned" #Superclass Level

merged_df2$class[merged_df2$class == ""] <- "Unassigned" #Class Level

#Plotly Module----

id <- "1Dth0s8bmGfJcdWm_hBJ6TsQ_BfcgycIx"
class_colors <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), na.strings=c("", "NA"))

id2 <- "1DuCnFfS2-KMXl1NzzE-3D2BbdEbIo6Is"
superclass_colors <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2), na.strings=c("", "NA"))


#SuperClass DF
superclass1<-data.frame(table(merged_df2$superclass)) %>% rename(Superclass = Var1)

df1_sc <- superclass1 %>% filter(Freq < 3)
df2_sc <- superclass1 %>% filter(Freq >= 3)
df3_sc<- as.data.frame(merge("Others with < 3",sum(df1_sc$Freq)))  %>% 
  rename(Superclass = x, Freq = y)
df4_sc  <- rbind(df2_sc,df3_sc)
df4_sc <- merge(superclass_colors,df4_sc, by.x = "X.canopus.Superclass.", by.y = "Superclass")  

superclass <- data.frame(df4_sc %>% select("X.canopus.Superclass.", "Freq"))
colnames(superclass) <- c("Superclass", "Freq")

colors0 <- c(df4_sc$mycolors26)

#Class DF
class1<-data.frame(table(merged_df2$class)) %>% rename(Class = Var1)

df1_c <- class1 %>% filter(Freq < 3)
df2_c <- class1 %>% filter(Freq >= 3)
df3_c<- as.data.frame(merge("Others with < 3",sum(df1_c$Freq)))  %>% 
  rename(Class = x, Freq = y)
df4_c  <- rbind(df2_c,df3_c)
df4_c <- merge(class_colors,df4_c, by.x = "X.canopus.Class.", by.y = "Class")  


class <- data.frame(df4_c %>% select("X.canopus.Class.", "Freq"))
colnames(class) <- c("Class", "Freq")

colors1 <- c(df4_c$mycolors441)


fig_superclass <- plot_ly(superclass, labels = ~Superclass, values = ~Freq, type = 'pie', 
  sort = FALSE, 
  direction = "clockwise",
  marker = list(colors= ~colors0), textfont = list(size = 40))%>%
  layout(font=list(size = 30),
         legend = list(title=list(text='<b> Superclass </b>'), y=0.5))
fig_superclass

fig_class <- plot_ly(class, labels = ~Class, values = ~Freq, type = 'pie', 
  sort = FALSE, 
  direction = "clockwise",
  marker = list(colors= ~colors1), textfont = list(size = 40))%>%
  layout(font=list(size = 30),
         legend = list(title=list(text='<b> Class </b>'), x= 100, y=0.5))
fig_class

#Set Enviroments
Sys.setenv("plotly_username" = "froz9")
Sys.setenv("plotly_api_key" = "3YcsdCmBtC8rfYLZA3PR")

plotly_IMAGE(fig_superclass, format="png", width = 2500, height = 2000, out_file="Superclass.png")
plotly_IMAGE(fig_class, format="png", width = 2500, height = 2000, out_file="Class.png")


custom_name <- function(x){
  cts_convert ((x), "PubChem CID", "Chemical Name", match = "first")
} #function to obtain Chemical name

ChemicalName <- mapply(custom_name, merged_df2$pubchemids) 

ChemicalName <- data.frame(matrix(unlist(ChemicalName))) %>% 
  rename(ChemicalName = matrix.unlist.ChemicalName..)

#Insert Chemical Name to the new data frame
merged_df2 <- cbind(merged_df2, ChemicalName) 

#Insert ID to datframe
ID_extract <- sub(".*SIRIUS_","", 
merged_df2$name) #Extract Characters After Pattern in R
merged_df2 <- cbind(merged_df2, ID_extract)

#Filter annotations with a mass desviation >15 ppm
merged_df2_15ppm <- merged_df2 %>% filter(Mass.accuracy..ppm. > -15)


#Substituted "." with "_" because sqldf not recognize dots
colnames(merged_df2_15ppm) <- gsub("\\.","_",colnames(merged_df2_15ppm))

FinalTable_15ppm <- sqldf("select ChemicalName, superclass, class, subclass, ConfidenceScore,
                    molecularFormula_y, adduct_y, Theoretical_mass, ionMass,  
                    Mass_accuracy__ppm_, pubchemids, links, TimeInMinutes from merged_df2_15ppm") #Remove pubchemids and links when approved


#This writes a CSV with all annotations, including those that have a mass deviation >15 ppm
write.csv(merged_df2,
paste0(WD, '\\Total_metabolites.csv'),
          row.names = FALSE)

#This writes a CSV with annotations that have a mass deviation <15 ppm
write.csv(FinalTable_15ppm,
paste0(WD, '\\Final_Table.csv'),
          row.names = FALSE)

#This writes a CSV with a summary of the number of elements and superclasses that appeared
write.csv(superclass1,
paste0(WD, '\\Superclass_summary.csv'),
          row.names = FALSE)

#This writes a CSV with a summary of the number of elements and classes that appeared
write.csv(class1,
paste0(WD, '\\Class_summary.csv'),
          row.names = FALSE)
