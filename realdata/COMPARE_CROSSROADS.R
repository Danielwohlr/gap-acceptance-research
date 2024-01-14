#Clear the WORKSPACE before running!
# This scripts compares densities of gaps accepted by k vehicles for k=0,1,2,3,4 for different crossroads
source("realdata/aux_scripts.R")

x <- ls() 
if (length(x) !=0){ 
  rm(list=ls()) 
}

# Main code
dataset1 <- process_dataset("data/Munich_INTSEC_01.xlsx")
dataset2 <- process_dataset("data/Munich_INTSEC_02.xlsx")
dataset3 <- process_dataset("data/Munich_INTSEC_03.xlsx")

Gaps1 <- create_gaps(dataset1)
Gaps2 <- create_gaps(dataset2)
Gaps3 <- create_gaps(dataset3)

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
