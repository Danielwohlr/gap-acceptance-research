#Závislost alpha
###############################################################################
sour_1 = seq(from=0,to=5, length.out=1000)
alpha <- 1
beta <- 5
lambda <- 4
mu <- 6
hustota_0 <- sapply(sour_1,Odvozena_hustota,k=3)
alpha <- 2
hustota_1 <- sapply(sour_1,Odvozena_hustota,k=3)
alpha <- 4
hustota_2 <- sapply(sour_1,Odvozena_hustota,k=3)
alpha <- 6
hustota_3 <- sapply(sour_1,Odvozena_hustota,k=3)
alpha <- 8
hustota_4 <- sapply(sour_1,Odvozena_hustota,k=3)
alpha <- 10
hustota_5 <- sapply(sour_1,Odvozena_hustota,k=3)
alpha <- 15
hustota_6 <- sapply(sour_1,Odvozena_hustota,k=3)


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
        legend.position = c(0.75, 0.65),
        axis.title.x = element_text(  size = 20,face="bold",vjust=-0.35),
        axis.title.y = element_text(  size = 20,face="bold",vjust=0.85),
        legend.text = element_text(size=20),
        legend.key.size = unit(1, 'cm'),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14) ) +
  labs(x="Časová světlost (s)",y= "Hustota pravděpodobnosti (1/s)")+
  scale_color_hue(labels = c("alpha=1", "alpha=2","alpha=4", "alpha=6","alpha=8", "alpha=10","alpha=15"))

A

#####
#zavislost beta
################################################################################

sour_1 = seq(from=0,to=1.5, length.out=1000)
alpha <- 2
beta <- 1
lambda <- 4
mu <- 6
hustota_0 <- sapply(sour_1,Odvozena_hustota,k=0)
beta <- 2
hustota_1 <- sapply(sour_1,Odvozena_hustota,k=0)
beta <- 4
hustota_2 <- sapply(sour_1,Odvozena_hustota,k=0)
beta <- 6
hustota_3 <- sapply(sour_1,Odvozena_hustota,k=0)
beta <- 8
hustota_4 <- sapply(sour_1,Odvozena_hustota,k=0)
beta <- 10
hustota_5 <- sapply(sour_1,Odvozena_hustota,k=0)
beta <- 15
hustota_6 <- sapply(sour_1,Odvozena_hustota,k=0)


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
B <-  ggplot(mdf,aes(x=x,y=value,color = variable)  ) + 
  geom_line( lwd=1.5)  +
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.65),
        axis.title.x = element_text( size = 20,face="bold",vjust=-0.35),
        axis.title.y = element_text(  size = 20,face="bold", vjust=0.85),
        legend.text = element_text(size=20),
        legend.key.size = unit(1, 'cm'),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14) ) +
  labs(x="Časová světlost (s)",y= "Hustota pravděpodobnosti (1/s)")+
  scale_color_hue(labels = c("beta=1", "beta=2","beta=4", "beta=6","beta=8", "beta=10","beta=15"))

B
#####
#zavislost na lambda
###############################################################################
sour_1 = seq(from=0,to=2.5, length.out=1000)
alpha <- 2
beta <- 5
lambda <- 1
mu <- 6
hustota_0 <- sapply(sour_1,Odvozena_hustota,k=0)
lambda <- 2
hustota_1 <- sapply(sour_1,Odvozena_hustota,k=0)
lambda <- 4
hustota_2 <- sapply(sour_1,Odvozena_hustota,k=0)
lambda <- 6
hustota_3 <- sapply(sour_1,Odvozena_hustota,k=0)
lambda <- 8
hustota_4 <- sapply(sour_1,Odvozena_hustota,k=0)
lambda <- 10
hustota_5 <- sapply(sour_1,Odvozena_hustota,k=0)
lambda <- 15
hustota_6 <- sapply(sour_1,Odvozena_hustota,k=0)


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
C <-  ggplot(mdf,aes(x=x,y=value,color = variable)  ) + 
  geom_line( lwd=1.5)  +
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.65),
        axis.title.x = element_text(  size = 20,face="bold",vjust=-0.35),
        axis.title.y = element_text(  size = 20,face="bold",vjust=0.85),
        legend.text = element_text(size=20),
        legend.key.size = unit(1, 'cm'),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14) ) +
  labs(x="Časová světlost (s)",y= "Hustota pravděpodobnosti (1/s)")+
  scale_color_hue(labels = c("lambda=1", "lambda=2","lambda=4", "lambda=6","lambda=8", "lambda=10","lambda=15"))

C
#####
#zavislost na mu
###############################################################################
sour_1 = seq(from=0,to=2, length.out=1000)
alpha <- 2
beta <- 5
lambda <- 4
mu <- 1
hustota_0 <- sapply(sour_1,Odvozena_hustota,k=0)
mu <- 2
hustota_1 <- sapply(sour_1,Odvozena_hustota,k=0)
mu <- 4
hustota_2 <- sapply(sour_1,Odvozena_hustota,k=0)
mu <- 6
hustota_3 <- sapply(sour_1,Odvozena_hustota,k=0)
mu <- 8
hustota_4 <- sapply(sour_1,Odvozena_hustota,k=0)
mu <- 10
hustota_5 <- sapply(sour_1,Odvozena_hustota,k=0)
mu <- 15
hustota_6 <- sapply(sour_1,Odvozena_hustota,k=0)


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
D <-  ggplot(mdf,aes(x=x,y=value,color = variable)  ) + 
  geom_line( lwd=1.5)  +
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.65),
        axis.title.x = element_text( size = 20,face="bold", vjust=-0.35),
        axis.title.y = element_text(  size = 20,face="bold",vjust=0.85),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=20),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14) ) +
  labs(x="Časová světlost (s)",y= "Hustota pravděpodobnosti (1/s)")+
  scale_color_hue(labels = c("mu=1", "mu=2","mu=4", "mu=6","mu=8", "mu=10","mu=15"))
  

D


