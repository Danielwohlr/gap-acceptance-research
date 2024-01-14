# load libraries and data
#############################################################################
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(devtools)
library(fitdistrplus)
library(optimx)
library(goftest)
library(binr)
library(reshape2)
library(ggthemes)
library(ggpubr)
library(goft)
library(rapport)
library(gsubfn)
library(KScorrect)
library(writexl)
#####


#Predpis hustoty rozdeleni akceptujici prave k vozidel
###############################################################################
#Prvni suma ktera figuruje v predpisu hustoty
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

#u predstavuje x-ovou souradnici
Odvozena_hustota <- function(u,k){
  (beta + mu)^(k*alpha +lambda)*u^(k*alpha +lambda -1)*exp(-(beta+mu)*u)*
    suma_1(u,k)/suma_2(k)
}


#####


#Simulace
###################################################

#num of gaps generated in main lane
#Pro n=10e7 tento script bezi tak minutu, ale ve vysledku mam cca 1000 odhadu pro boxplot,
#takze se to da prekousnout 
n <- 1000000

#gamma parameters for main lane

#shape, skaluji hlavni silnici na 1
lambda <- 4
#rate
mu <- 6

#gamma parameters for side lane

#shape
alpha <- 2

#rate
beta <- 5


#generated gaps main lane
X_real <- rgamma(n,lambda,mu)

#TBS: num_car[i] = kolik aut akceptovalo mezeru X_real[i]
num_car <- rep(NULL,n)

#Simuluji kolik aut ze side lane akceptovala jednotlive gaps 
for(i in 1:n) {
  
  Y <- -0
  j <- -1
  #while probehne aspon jednou s pravedp=1
  while (Y < X_real[i]) {
    #Y_real jsou generovane mezery z vedlejsi silnice
    Y_real <- rgamma(1,alpha,beta)
    Y <- Y + Y_real
    j <- j + 1
  }
  
  num_car[i] <- j
}

#X_k jsou realizace veliciny X, ktere byly akceptovany k auty ze side lane
X_0 <- X_real[acc0 = which(num_car == 0)]
X_1 <- X_real[acc1 = which(num_car == 1)]
X_2 <- X_real[acc2 = which(num_car == 2)]
X_3 <- X_real[acc3 = which(num_car == 3)]
X_4 <- X_real[acc4 = which(num_car == 4)]

#Doplnim si vektory dat hodnotami NA, aby byly stejne dlouhe a mohl jsem je sloucit
n <- max(length(X_0),length(X_1),
         length(X_2),length(X_3), length(X_4) )

#Delam duplikaty, ktere pak budou mit stejnou length (hodi se mi to do nejakych funkci)
X_same_0 <- X_0
X_same_1 <- X_1
X_same_2 <- X_2
X_same_3 <- X_3
X_same_4 <- X_4


length(X_same_0) <- n
length(X_same_1) <- n
length(X_same_2) <- n
length(X_same_3) <- n
length(X_same_4) <- n

Sloucena_data <- cbind(X_same_0,X_same_1,X_same_2,X_same_3,X_same_4)

###################################################
###################################################
#Vizualice



 # Vykreslit_graf(0)
 # Vykreslit_graf(1)
 # Vykreslit_graf(2)
 # Vykreslit_graf(3)
 # Vykreslit_graf(4)

###################################################
###################################################
#Odhad parametru 

#Odhad_parametru(1)
