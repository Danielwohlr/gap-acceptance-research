#Tento script funguje spravne pouze pokud se spousti s prazdnym Workspace
library(readxl)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(devtools)
library(fitdistrplus)
library(optimx)
library(goftest)
library(ggpubr)
library(rapport)
library(gsubfn)

# source("D:/Intel/Documents/Simulace VU/rozdel_akcept_gamma.R")
# source("D:/Intel/Documents/Simulace VU/Boxplot_betamu.R")
# source("D:/Intel/Documents/Simulace VU/Vykreslit_graf.R")
# source("D:/Intel/Documents/Simulace VU/Pearson_test_sim.R")



#Rstudio spravne vykresli vsechny obrazky pouze s prazdnym workspace
#https://stackoverflow.com/questions/51247102/reached-elapsed-time-limit-errors-in-r
x <- ls() 
if (length(x) !=0){ 
  rm(list=ls()) 
}

#################
#################
#Cteni dat
dataset1 <- read_excel("Munich_INTSEC_01.xlsx", 
                      col_names = FALSE)
dataset2 <- read_excel("Munich_INTSEC_02.xlsx", 
                      col_names = FALSE)
dataset3 <- read_excel("Munich_INTSEC_03.xlsx", 
                      col_names = FALSE)

colnames(dataset1) <- c("Gap","k","Hustota", "Rychlost")
colnames(dataset2) <- c("Gap","k","Hustota", "Rychlost")
colnames(dataset3) <- c("Gap","k","Hustota", "Rychlost")

Gaps1 <- vector(mode = "list", length = 9)
Gaps2 <- vector(mode = "list", length = 9)
Gaps3 <- vector(mode = "list", length = 9)


for(j in 1:9){
  l<- j-1
  Gaps1[[j]] <- as.numeric(dataset1$Gap[dataset1$k == l])
  Gaps2[[j]] <- as.numeric(dataset2$Gap[dataset2$k == l])
  Gaps3[[j]] <- as.numeric(dataset3$Gap[dataset3$k == l])
}

#################
#################
#Density plots rozestupu na hlavni silnici pro ruzne datasety a pro ruzne k
#Jinymi slovy porovnani datasetu 1, 2, 3 a take porovnani hustot pro ruzne k

#k je kolik mezery akceptuji aut 
vykreslit_dplot <- function(k){
  j <- k+1
  
  prislusna_data <- c(Gaps1[[j]],Gaps2[[j]],Gaps3[[j]])
  cislo_dataset <- c(rep("První",length(Gaps1[[j]] ) ),rep("Druhá",length(Gaps2[[j]] ) ),rep("Třetí",length(Gaps3[[j]] ) )     )
  
  sloucene_gaps <- data.frame(cislo_dataset,prislusna_data)
  
  
  #Vykresleni obrazku
  porovnani_datasetu <- ggplot(sloucene_gaps, aes(x=prislusna_data,fill = cislo_dataset)) +
    geom_density(alpha=0.4 ,lwd=1) +
    labs(title=sprintf("Světlost akceptačního řádu k=%i",k),
         x="Světlost (s)", y = "Hustota (1/s)") 
    
  porovnani_datasetu +theme(legend.background = element_rect(size=0.5, linetype="solid"),
                            legend.position = c(0.9, 0.8))  +
                    scale_fill_discrete(name = "Křižovatka")
  
}

#Vykresli mi vice grafu do jednoho obrazku
figure <- ggarrange(vykreslit_dplot(0),vykreslit_dplot(1),vykreslit_dplot(2),vykreslit_dplot(3))

annotate_figure(figure )

#Vykresli pouze porovnani datasetu pro k=4
vykreslit_dplot(4)
#Z obrazku je videt ze pro k mezi 0 a 3 jsou hustoty velmi podobne, pro k = 4 je u prvniho datasetu vyrazny peak v modu distribuce,
#pro zbyvajici k je v datasetu velmi malo pozorovani


for(j in 1:4){
  
  print(gamma_test(Gaps1[[j]]))

}
