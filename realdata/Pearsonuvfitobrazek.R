#Zde vykreslim histogramy pro svetlosti radu k=0,1,2,3 na krizovatce c. 3
#prolozim je teoretickou hustotou s odhadnutymi parametry, ktere si zaznamenam

output <- Nacteni_dat()
Upload <- output[[1]]

#Indexy ktere akceptovaly prave K vozidel
k<- 0

#k je input akcept. rad, limonada je input kam az kreslit histogram(pro vyssi k ne tak daleko)
Createfig <- function(k,limonada){
  
  
  if(k==0){
    akcrad <- "Nultý"
  } else if(k==1){
    akcrad <- "První"
  } else if(k==2){
    akcrad <- "Druhý"
  }
    else if(k==3){
    akcrad <- "Třetí"
  }
  
  
  #Uprava do homogenniho vzorku dat, viz pokr_analyza
  ###############################################################################
  #Indexy pro stredne rychla auta v uploadovanych datech
  dolniR <- as.numeric(quantile(Upload$Rychlost,0.10))
  horniR <- as.numeric(quantile(Upload$Rychlost,0.90))
  Stredni_auta <- which(Upload$Rychlost >= dolniR & Upload$Rychlost <=horniR)
  
  akcept <- which(Upload$k==k)
  
  
  #Indexy pro prumernou hustotu vozidel
  dolniH <- as.numeric(quantile(Upload$Hustota,0.10))
  horniH <- as.numeric(quantile(Upload$Hustota,0.90))
  prumer <- which(Upload$Hustota >dolniH & Upload$Hustota <horniH )
  
  
  #Indexy stredne rychlych aut se stredni hustotou ktere akceptovaly k vozidel
  stredni <- intersect(intersect(Stredni_auta,akcept),prumer)
  
  #Stredne rychla vozidla akceptovana k vozidly
  Data <- Upload$Gap[stredni]
  
  
  Data <- Data/mean(Data)
  #####
  
  #Provedeni Pearsonova testu
  
  ListPearson <- resulthyp(Data,k)
  TeorHyp <- ListPearson$RozhodnutiTeor
  reportX2 <- ListPearson$Pearson
  GammaHyp <- ListPearson$RozhodnutiGamma
  
  #Pro kazdy chunk si ulozim hodnoty parametru
  alpha <- ListPearson$Parametry["alpha"]
  beta <- ListPearson$Parametry["beta"]
  lambda <- ListPearson$Parametry["lambda"]
  mu <- ListPearson$Parametry["mu"]
  
  #Predpis teoreticke hustoty
  #############################################################################
  
  suma_1 <- function(u,k){
    vysledek1 <- 0
    for(j in 0:(alpha-1)){
      mezivysledek <- (u*beta)^j/factorial(k*alpha+j)
      vysledek1 <- vysledek1 + mezivysledek
    }
    vysledek1
  }
  
  #druha suma
  suma_2 <- function(k){
    vysledek2 <- 0
    for(j in 0:(alpha-1)){
      mezivysledek <- beta^j/(beta+mu)^j*
        gamma(k*alpha+j+lambda)/
        factorial(k*alpha + j)
      vysledek2 <- vysledek2 + mezivysledek
    }
    vysledek2
  }
  
  
  Odvozena_hustota <- function(u,k){
    (beta + mu)^(k*alpha +lambda)*u^(k*alpha +lambda -1)*exp(-(beta+mu)*u)*
      suma_1(u,k)/suma_2(k)
  }
  #####
  
  #x-ova souradnice
  sourx = seq(from=0,to=limonada, length.out=10000)
  #Hodnoty teoreticke hustoty
  hustota <- sapply(sourx,Odvozena_hustota,k=k)
  
  histogramek <- ggplot() +
    geom_histogram( aes(x=Data, y = ..density..),alpha=0.7,fill = "green",
                    color = "black", bins = 20, boundary = 0 ) +
    labs(title = sprintf("%s akceptační řád",akcrad),
         y = 'Hustota (1/s)', x='Škálovaná světlost (s)')+
    theme(axis.text.x = element_text(size=13),
          axis.text.y = element_text(size=13),
          axis.title.x = element_text(  size = 16,face="bold",vjust=-0.35),
          axis.title.y = element_text(  size = 16,face="bold",vjust=0.85),
          plot.title = element_text(size=15))
  
  #Finalni obrazek histogram+gamma+teoreticka
  histogramek + geom_line( aes(x=sourx,y=hustota),lwd=1.4,color="orangered")
  
  
}




# figure <- ggarrange(Createfig(0,3),Createfig(1,2.5),Createfig(2,2),Createfig(3,2))
# 
# annotate_figure(figure)
