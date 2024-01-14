#Vykresli histogram, gamma fit a teoretickou hustotu + take provede CVM test,
#tj GoF test, zda lze data povazovat z Gamma rozdeleni


#Pro porovnani si generuji teoreticke rozdeleni
#sour_1 je x-ova souradnice, hustota_k jsou y-ove souradnice distribuci,
#ktere akceptuji prave k vozidel

sour_1 = seq(from=0,to=3, length.out=1000)
hustota_0 <- sapply(sour_1,Odvozena_hustota,k=0)
hustota_1 <- sapply(sour_1,Odvozena_hustota,k=1)
hustota_2 <- sapply(sour_1,Odvozena_hustota,k=2)
hustota_3 <- sapply(sour_1,Odvozena_hustota,k=3)
hustota_4 <- sapply(sour_1,Odvozena_hustota,k=4)

dist_hodnoty <- cbind(sour_1,hustota_0,hustota_1,hustota_2,hustota_3,hustota_4)


#k je pocet akcept aut do jedne mezery na X
Vykreslit_graf <- function(k){
  
  #posouvam k jako index o +1 ptz v R indexuje od 1 a ne 0
  j <- k+1
  hist_data <- Sloucena_data[,j]
  hist_data <- hist_data[!is.na(hist_data)]
  
  #Sklonovani slova vozidlo v popisku grafu
  if (k==0 | k >5){
    vozidlo <- "vozidel"
  } else if (k==1){
    vozidlo <- "vozidlo"
  } else if (k > 1 & k <5){
    vozidlo <- "vozidla"
  }
  
  #Vytvoreni histogramu
  histogramek1 <- ggplot() +
    geom_histogram( aes(x=hist_data, y = ..density..),fill = "green", 
                    color = "black", bins=20, boundary = 0 ) +
    labs(title = sprintf("Rozdělení akceptující %i %s",k,vozidlo),
         y = 'Hustota (1/s)', x='Časová světlost (s)')+
    theme(plot.title = element_text(size=15))
  
  #Odhadovani parametru pro fitovani gamma distribuce
  fit.gamma <- hist_data %>%
    fitdist(distr= 'gamma', method = 'mle')

  shape_est <- fit.gamma$estimate["shape"]
  rate_est <- fit.gamma$estimate["rate"]

  gamma_hustota <- dgamma(sour_1,shape = shape_est, rate = rate_est)
  gamma_hodnoty <- data.frame(sour_1,gamma_hustota)
  
  #Plot (histogram,teoreticka distribuce, odhadnuta gamam distribuce)
  obrazecek <- histogramek1 + 
    geom_line(data = gamma_hodnoty, aes(x=sour_1,y=gamma_hustota,colour = "Gamma"),lwd=1.5) +
    geom_line( aes(x=sour_1,y=dist_hodnoty[,j+1],colour = "Teoretická"),lwd=1) +
    scale_color_manual(values = c(
      "Teoretická" = 'blue',
      "Gamma" = 'red')) +
    labs(color = 'Typ distribuce') +
    theme(legend.title = element_text(size=14, face = "bold"),
          legend.position = c(0.75, 0.7),
        axis.title.x = element_text( size = 14,face="bold", vjust=-0.35),
        axis.title.y = element_text(  size = 14,face="bold",vjust=0.85),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=13),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14) )+
    xlim(0,2.5)
    
  obrazecek
}

#Zkouska
Vykreslit_graf(1)

# Graf0 <- Vykreslit_graf(0)
# Graf1 <- Vykreslit_graf(1)
# Graf2 <- Vykreslit_graf(2)
# Graf3 <- Vykreslit_graf(3)
# 
# 
# figure <- ggarrange(Graf0,Graf1,Graf2,Graf3)
# 
# annotate_figure(figure)
# 
