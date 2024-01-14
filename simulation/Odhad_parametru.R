#k je pocet akcept aut do jedne mezery na X
Odhad_parametru <- function(k){
  
  #posouvam k jako index o +1 ptz v R indexuje od 1 a ne 0
  m <- k+1
  #Sloucena_data viz hlavni script "rozdel_akcept_gamma"
  pozorovani <- Sloucena_data[,m]
  pozorovani <- pozorovani[!is.na(pozorovani)]
  n <- length(pozorovani)
  
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
    n*log(mezivysledek )
  }
  
  
  #Zaporna log-verohodnostni funkce, argumenty muzu upravit podle toho ktery chci zrovna odhadnout
  neg_log_like <- function(Params){
    beta <- Params[1]
    lambda <- Params[2]
    mu <- Params[3]
    
    -(
      (k*alpha + lambda)*n*log(beta +mu) +
      (k*alpha + lambda -1)*sum(log(pozorovani)) -
      (beta+mu)*sum(pozorovani) +
       ctvrtyclen(alpha, beta)  -
       patyclen(alpha, beta, lambda, mu) )
    
  }
  
  
  nlm(neg_log_like, c(4,5,3) )
}

#Zkouska
#Odhad_parametru(1)
