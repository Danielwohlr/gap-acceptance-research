#Nacteni dat, napr 
#Dataset1 <- output[[1]]
#Dataset3 <- output[[3]]
source("realdata/aux_scripts.R")
output <- Nacteni_dat()

#Do promene Total_data muzu ulozit data bud z mereni 1,2,3 a nebo vsechny sloucene, tj index 4
Total_data <- output [[1]]

summary(Total_data)

#Rychla auta jsou nad 61.10 km/h
#Pomala auta jsou pod 33.10 km/h
#Stredne rychla auta jsou mezi 45.10 a 50 km/h
Rychle_auta <- which(Total_data$Rychlost >61.10 )
Pomale_auta <- which(Total_data$Rychlost <33.10 )
Stredni_auta <- which(Total_data$Rychlost >=45.10 & Total_data$Rychlost <=50)

##############################################
##############################################
#Počet akceptovaných aut v závislosti na rychlosti auta

temp <- Total_data$k[Rychle_auta]
tempo <- Total_data$k[Pomale_auta]
tempor <- Total_data$k[Stredni_auta]


#Ulozim si histogram pro pomala auta
rychlost_auta = rep("slow",length(tempo))
acc_auta <- tempo
rychl_acc <- data.frame(rychlost_auta,acc_auta)
hist1 <-ggplot(rychl_acc,aes(x=acc_auta,fill=rychlost_auta))+
  geom_bar(aes(y = ..count../sum(..count..)))+ 
  labs(x="Počet akceptovaných aut", y = "PMF",title="Pomalá auta")+
  theme(legend.position='none')

#Ulozim si histogram pro rychla auta
rychlost_auta = rep("fast",length(temp))
acc_auta <- temp
rychl_acc <- data.frame(rychlost_auta,acc_auta)
hist2 <-ggplot(rychl_acc,aes(x=acc_auta,fill=rychlost_auta))+
  geom_bar(aes(y = ..count../sum(..count..)))+ 
  labs(x="Počet akceptovaných aut", y = "PMF",title="Rychlá auta")+
  theme(legend.position='none') 

#Ulozim si histogram pro stredne rychla auta
rychlost_auta = rep("normal",length(tempor))
acc_auta <- tempor
rychl_acc <- data.frame(rychlost_auta,acc_auta)
hist3 <-ggplot(rychl_acc,aes(x=acc_auta, fill=rychlost_auta))+
  geom_bar(aes(y = ..count../sum(..count..)) ) + 
  labs(x="Počet akceptovaných aut", y = "PMF",title="Středně rychlá auta")+
  theme(legend.position='none')


#######################################
#######################################
#Rozdeleni velikosti mezer v zavislosti na rychlosti auta


Gap_pomale <- Total_data$Gap[Pomale_auta]
Gap_rychle <- Total_data$Gap[Rychle_auta]
Gap_stredni <- Total_data$Gap[Stredni_auta]


Gap_pomale <-data.frame("Gap"=Gap_pomale)
Gap_rychle <-data.frame("Gap"=Gap_rychle)
Gap_stredni <-data.frame("Gap"=Gap_stredni)

A<-ggplot(Gap_pomale,aes(x=Gap,y=..density..))+
  geom_histogram(color="black",fill="magenta",binwidth = 1) + 
  labs(x="Velikost mezery (s)", y = "Hustota",title="Pomalá auta")+
  theme(legend.position='none')

B<-ggplot(Gap_rychle,aes(x=Gap,y=..density..))+
  geom_histogram(color="black",fill="magenta",binwidth = 1) + 
  labs(x="Velikost mezery (s)", y = "Hustota",title="Rychlá auta")+
  theme(legend.position='none')

C<-ggplot(Gap_stredni,aes(x=Gap,y=..density..))+
  geom_histogram(color="black",fill="magenta",binwidth = 1) + 
  labs(x="Velikost mezery (s)", y = "Hustota",title="Středně rychlá auta")+
  theme(legend.position='none')


#Vizualizace (histogramy a boxplot)
#################

##Vykreslim 3 histogramy do jednoho obrazku

figure1 <- ggarrange(hist1,hist2,hist3)

 annotate_figure(figure1, top = text_grob("Počet akceptovaných aut v závislosti na rychlosti auta",
                                          color = "red", face = "bold", size = 14) )




 figure <- ggarrange(A,B,C)

 annotate_figure(figure, top = text_grob("Rozdělení mezer na hlavní silnici v závislosti na rychlosti vozidla",
                                          color = "red", face = "bold", size = 14) )

#Jasne z popisu boxplotu o co jde

 ggboxplot(Total_data,x= "k",y="Gap" ) +
   labs(title="Velikost rozestupů na hlavní silnici přijatých různými počty vozidel",
        x="Počet akceptovaných vozidel", y = "Velikost rozestupu (s)")
#####
 
 
 
 #CVM test - Gamma a exp rozdeleni gaps na hlavni silnici
 ###############################################################################
 
 #Odhadovani parametru pro fitovani gamma distribuce
 Test_set <- Total_data$Gap[Stredni_auta]


 #Gamma
 fit.gamma <- as.numeric(Test_set) %>%
   fitdist(distr= 'gamma', method = 'mle')

 shape_est <- fit.gamma$estimate["shape"]
 rate_est <- fit.gamma$estimate["rate"]

 CVMGAMMA <- cvm.test(Test_set,"pgamma",shape_est,rate_est,estimated=TRUE)
 CVMGAMMA

 #Exponencialni
 fit.exp <- as.numeric(Test_set) %>%
   fitdist(distr= 'exp', method = 'mle')


 shape_est <- 1
 rate_est <- fit.exp$estimate["rate"]

 CVMEXP <- cvm.test(Test_set,"pgamma",shape_est,rate_est,estimated=TRUE)
 CVMEXP
 
 #####
 
 
 