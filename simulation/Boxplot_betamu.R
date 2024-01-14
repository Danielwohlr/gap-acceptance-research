# This scripts estimates beta and mu parameters for the scaled case
# Alpha is known and set at the beginning of the first script



#Analyza odhadovani parametru beta, mu
#Alpha je znamy parametr a stejne jako ostatni se nastavuje na zacatku 1. scriptu
#Lambda je nastaven na stejnou hodnotu jako mu, pak je hlavni silnice skalovana na 1
###################################################
###################################################
#Priprava dat


#Vezmu nejmensi pocet pozorovani dle k =0,1,2,3
#Chci udelat stejny pocet odhadu pro vsechna k
# Helper function to truncate and split data
process_observations <- function(data, num_estimates) {
  trunc_data <- data[1:(num_estimates*100)]
  split_data <- split(trunc_data, ceiling(seq_along(trunc_data)/100))
  return(split_data)
}

# Main code
data_list <- list(X_0, X_1, X_2, X_3)
nejmensi_rozmer <- min(sapply(data_list, length))

#jeden odhad pouzije 500 hodnot, proto promena pocet_odhadu bude toto cislo
pocet_odhadu <- floor(nejmensi_rozmer/100)

# Process each dataset
split_data_list <- lapply(data_list, process_observations(), num_estimates = pocet_odhadu)

#Alokuji misto na ukladani odhadu
odhad_beta <- rep(NULL,pocet_odhadu)
odhad_mu <- rep(NULL,pocet_odhadu)
CVM_test <- rep(NULL,pocet_odhadu)
shape_est <- rep(NULL,pocet_odhadu)
rate_est <- rep(NULL,pocet_odhadu)

#Zde volim pro ktere k chci odhadovat parametry
k <- 3

#Podle zvoleneho k vyse rozhodne ktere data pouziju v hlavnim for loop
Moje_data <- split_data_list[[k+1]]
###################################################
###################################################
#Odhady parametru, vizualizace


#pro kazdej 500chunk v danym datasetu se mi odhadnou 3 parametry a ulozi se mi to
#Pozn. pro k=0 se mi to casto zkazi, for se zastavi driv nez pred koncem, protoze 
#nedava konecne hodnoty ve verohodnostni funkci nebo co

#Predpis negloglike
########################################################################
#VYPOCET PRVNI SUMY/CTVRTY CLEN
ctvrtyclen <- function(alpha, beta){
  ejkej <- function(j) (pozorovani*beta)^j/factorial(k*alpha +j)
  
  vysledek =0
  
  #pro pevny index pozorovani mam pevne cislo vysledek
  for(j in 0:(alpha-1)){
    vysledek = vysledek + ejkej(j) 
  }
  sum(log(vysledek))
}

#VYPOCET DRUHE SUMY/PATY CLEN
patyclen <- function(alpha, beta, lambda, mu){
  brekeke <- function(j) (beta/(beta +mu) )^j * gamma(k*alpha +lambda +j)/factorial(k*alpha+j)
  
  mezivysledek <- 0
  
  for(j in 0:(alpha-1)){
    mezivysledek = mezivysledek + brekeke(j) 
  }
  length(pozorovani)*log(mezivysledek )
}



#Zaporna log-verohodnostni funkce, argumenty muzu upravit podle toho ktery chci zrovna odhadnout
#Za parametr lambda je dosazen mu. TO odpovida skalovani na hlavni silnici na EX=1
neg_log_like <- function(Params){
  beta <- Params[1]
  mu <- Params[2]
  
  -(
    (k*alpha + mu)*length(pozorovani)*log(beta +mu) +
      (k*alpha + mu -1)*sum(log(pozorovani)) -
      (beta+mu)*sum(pozorovani) +
      ctvrtyclen(alpha, beta)  -
      patyclen(alpha, beta, mu, mu) )
  
}
#####


  for(j in 1:pocet_odhadu ){
      print(j)
      pozorovani <- Moje_data[[j]]
     
      
      try(
      minimalizace_negloglike <- nlm(neg_log_like, c(beta+runif(1,-1,1),mu+runif(1,-1,1)) )
      )
      
      
      odhad_beta[j] <- minimalizace_negloglike$estimate[1]
      
      odhad_mu[j] <- minimalizace_negloglike$estimate[2]
  
      if(odhad_mu[j] <0){
        odhad_mu[j] = 0
      } 
  }




odhad_teor_hustoty <- data.frame(odhad_beta,odhad_mu)


#Vykresleni boxplotu pro dane k, ktere je volene pred hlavnim for loop
boxplot(odhad_teor_hustoty,names=c("Odhad beta","Odhad mu"),
        main= sprintf("Odhady parametrů teoretické hustoty pro škálovaný případ, k = %i",k) )



Sada3k3 <-odhad_teor_hustoty


summary(odhad_teor_hustoty)

#Pro pripomenuti skutecnych hodnot
beta
mu
