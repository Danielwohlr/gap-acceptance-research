#Vykresli pouze hustoty


sour_1 = seq(from=0,to=3, length.out=1000)
alpha <- 2
beta <- 5
lambda <- 4
mu <- 6
hustota_list <- list()

# Use a loop to calculate the different values of k
for (k in 0:6) {
  hustota_list[[paste0("hustota_", k)]] <- sapply(sour_1, Odvozena_hustota, k = k)
}

dist_hodnoty <- data.frame("x" = sour_1, hustota_0,hustota_1,hustota_2,hustota_3,hustota_4,hustota_5,hustota_6)

mdf <- melt(dist_hodnoty,id = "x")

#k je pocet akcept aut do jedne mezery na X
  
  
  #Vytvoreni histogramu
  # histogramek1 <- ggplot() +
  #   geom_histogram( aes(x=hist_data, y = ..density..),alpha=0.6,fill = "grey", 
  #                   color = "black", bins = 100, boundary = 0 ) +
  #   labs(title = sprintf("Rozdělení akceptující právě %i %s",k,vozidlo),
  #        subtitle = 'Gamma rozdělení v obou proudech',
  #        y = 'Hustota', x='Délka mezery')
  # 
  #Odhadovani parametru pro fitovani gamma distribuce
  
  #Plot (histogram,teoreticka distribuce, odhadnuta gamam distribuce)
  A <-  ggplot(mdf,aes(x=x,y=value,color = variable)  ) + 
    geom_line( lwd=1.5)  +
    theme(legend.title = element_blank(),
          legend.position = c(0.85, 0.75),
          axis.title.x = element_text( size = 20,face="bold",vjust=-0.35),
          axis.title.y = element_text( size = 20,face="bold", vjust=0.85),
          legend.text = element_text(size=20),
          legend.key.size = unit(1, 'cm'),
          axis.text.x = element_text(face="bold",size=14),
          axis.text.y = element_text(face="bold",size=14) ) +
    labs(x="Časová světlost (s)",y= "Hustota pravděpodobnosti (1/s)")+
    scale_color_hue(labels = c("k=0", "k=1","k=2", "k=3","k=4", "k=5","k=6"))
  
  A
  
  