#data jsou realne gaps akceptovane prave k vozidly
#pro data vykresli histogram, z prislusneho Pearsonovo testu vezme odhadnute parametry a prolozi teoretickou hustotou
#dale odhaduji gamma fit, ten take prolozim

#typ je bud 1 - data s nejlepsi shodou, a vypíše se do obrazku NEJLEPSI, anebo 0 - data s nejhorsi shodou, vypise se nejhorsi
Obrazekmocpekny <- function(data,k,typdat){
  
  if(typdat == 1){
    bestorworst <- "nejlepší"
  } else{
    bestorworst <- "nejhorší"
  }
  
  #NESKALUJI UZ DATA
  scudaje <- data
  
  #Znovu otestuji hypotezu kvuli inicializaci promene
  BestPearson <- resulthyp(scudaje,k)
  
  #Ukladam si hodnoty parametru pro ktere chci vykreslit teoretickou hustotu
  alpha <- BestPearson$Parametry["alpha"]
  beta <- BestPearson$Parametry["beta"]
  lambda <- BestPearson$Parametry["lambda"]
  mu <- BestPearson$Parametry["mu"]
  
  
  #Odhadovani parametru pro fitovani gamma distribuce
  fit.gamma <- scudaje %>%
    fitdist(distr= 'gamma', method = 'mle')
  
  shape_est <- fit.gamma$estimate["shape"]
  rate_est <- fit.gamma$estimate["rate"]
  
  #x-ova souradnice
  sour_1 = seq(from=0,to=3, length.out=100)
  
  #Funkce gamma fit, ktere pak davam ggplotu na vykresleni porovnani s histogramem
  gamma_hustota <- dgamma(sour_1,shape = shape_est, rate = rate_est)
  gamma_hodnoty <- data.frame(sour_1,gamma_hustota)
  
  Gamma_hustota_est <- geom_line(data = gamma_hodnoty, aes(x=sour_1,y=gamma_hustota,colour = "Gamma"), lwd=1.2)
  
  
  
  
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
  
  #Hodnoty teoreticke hustoty
  hustota <- sapply(sour_1,Odvozena_hustota,k=k)
  
  
  #Histogram z realnych dat
  histogramek <- ggplot() +
    geom_histogram( aes(x=scudaje, y = ..density..),alpha=0.6,fill = "green",
                    color = "black", bins = 20, boundary = 0 ) +
    labs(title = sprintf("Úsek dat s %s shodou",bestorworst),
         y = 'Hustota (1/s)', x='Škálovaná světlost (s)')+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title.x = element_text(  size = 17,face="bold",vjust=-0.35),
          axis.title.y = element_text(  size = 17,face="bold",vjust=0.85),
          plot.title = element_text(size=17))
  
  
  
  
  #Finalni obrazek histogram+gamma+teoreticka
  obrazecek <- histogramek + geom_line( aes(x=sour_1,y=hustota,colour = "Teoretická"),lwd=1.2) +
    Gamma_hustota_est +
    scale_color_manual(values = c(
      "Teoretická" = 'blue',
      "Gamma" = 'red')) +
    labs(color = 'Typ distribuce') +
    theme(legend.position = c(0.8, 0.8),legend.text = element_text(size=14),
          legend.key.size = unit(1, 'cm'),
          legend.title = element_text(size=17,face="bold"))
  
  obrazecek
  

  
}
