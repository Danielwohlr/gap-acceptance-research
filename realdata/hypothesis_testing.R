#Pearsonovo X2 testu hypotezy, zda chunkofdata pochazi z teoreticke distribuce z odhadnutymi parametry
#Funkce vrati list s parametry odpovidajici nejlepsi volbe alpha (nejlepsi se mysli nejnizsi hodnota pearsonovy stat)
#dale pak take vraci nejlepsi hodnotu Pearsonovy statistiky a vysledek hypotezy(0-zamitnuto, 1-nezamitnuto) 
library(goft)
resulthyp <- function(chunkofdata,k){
  
  #scaling data
  scdata<-chunkofdata
  
  #v GoF test, ze data jsou gamma s odhadnutymi parametry
  rozhodnutiGamma <- gamma_test(scdata)$p.value
  
  if(rozhodnutiGamma >= 0.05){
    rozhodnutiGamma = 1
  } else{
    rozhodnutiGamma = 0
  }
  
  
  #Předpis NEGLOGLIKE
  ##################################################
  #VYPOCET PRVNI SUMY/CTVRTY CLEN V NEGLOGLIKE
  ctvrtyclen <- function(alpha, beta){
    
    ejkej <- function(j) (scdata*beta)^j/factorial(k*alpha +j)
    
    vysledek =0
    
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
    length(scdata)*log(mezivysledek )
  }
 
  #Zaporna log-verohodnostni funkce
  negloglike <- function(Params){
    beta <- Params[1]
    mu <- Params[2]
    
    -(
      (k*alpha + mu)*length(scdata)*log(beta +mu) +
        (k*alpha + mu -1)*sum(log(scdata)) -
        (beta+mu)*sum(scdata) +
        ctvrtyclen(alpha, beta)  -
        patyclen(alpha, beta, mu, mu) )
  }
  
  #Predpis teoreticke hustoty, CDF. ICDF 
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
  
  Kumulativni <- function(x,k){
    integrate(Odvozena_hustota,0,x,k=k)$value
  }
  
  #Definice ICDF
  Quantilova <- function(p,k){
    uniroot( function(x) {Kumulativni(x,k)-p},lower=0,upper=20 )$root
  }
  
  #Pevna volba rozmezi pro alpha, pro kterou se bude zkouset odhadnout parametry beta, mu (=lambda)
  maxalpha <- 15
  
  #Sem se budou ukladat hodnoty Pearsonovy statistiky pro ruzne alpha
  X2_stat <- rep(NA,maxalpha)
  
  #sem se bude ukladat vysledek testu.. 0 pokud se zamitne, 1 nezamitne
  vysledek <- rep(NA,maxalpha)
  
  #Zde si budu zaznamenavat prubezne ostatni hodnoty parametru, abych si je pak mohl precist pro nejnizsi hodnotu
  #Pearsonovy statistiky
  allbeta <- rep(NA,maxalpha)
  allmu <- rep(NA,maxalpha)
  alllambda <- rep(NA,maxalpha)
  
  #For loop pro kazdou hodnotu alphy in 1:maxalpha provede Pearsonuv-X2 test s odhadnutymi parametry,
  #vrati bestalpha a zda byla hypoteza pro bestalpha zamitnuta(0)/nezamitnuta(1)
  #POZOR!!!!! MUSI SE ZACINAT VE FOR LOOPU OD 1, JINAK MI PAK NA KONCI TOHOTO SKRIPTU NEFUNGUJE NALEZENI
  #TOHO NEJLEPSIHO ALPHA atd
  for(chi in 1:maxalpha){
    
    alpha <-chi
    
    #Odhadovani a ukladani odhadu
    try(
    min_negloglike <- nlm(negloglike, c(10+runif(1,-2,2),6+runif(1,-2,2) ) )
    )
    
    #Odhady pro teoretickou hustotu, pripominam, ze mam skalovani mu=lambda
    odhad_beta <- min_negloglike$estimate[1]
    odhad_mu <- min_negloglike$estimate[2]
    
    beta <- odhad_beta
    lambda <- odhad_mu
    mu <- odhad_mu
    #Pearson X2 test zda jsou data z teoreticke distribuce
    ####################################################
    
    #pocet binu na ktery rozsekam obor hodnot
    numbins <- 10
    
    #Probability that X falls inside bin
    prob <- rep(NA,numbins)
    
    #Expected num of realizations in bin
    exp <- rep(NA,numbins)
    
    #edges for bins
    edges <- rep(NA,numbins)
    
    #posledni bin s cislem [Numbins] bude ten od max(data) do infty
    edges[1] <- 0
    prob[1] <- 1/numbins
    
    for(omega in 1:(numbins-1) ){
      edges[omega+1] = Quantilova(omega/numbins,k)
      prob[omega+1] = 1/numbins
    }

    #Actually observed X inside bins
    obs <- rep(NA,numbins)
    
    #Ulozim kolik hodnot spadlo mezi hranice kazdeho binu az na posledni, ten udelam zvlast
    for(q in 1:numbins){
      obs[q] <- which(edges[q] < scdata & scdata <= edges[q+1]) %>%
        length()
    }
    
    #zvlast posledni bin
    obs[numbins] <- which(edges[numbins] < scdata) %>%
      length()
    
    #Spocteni Pearsonovy statistiky, vyhodnoceni testu
    ###########################################################################
    
    exp <- length(scdata)*prob
    
    #Vypocitani Pearsonovy statistiky
    X2_stat[chi] <- sum( (exp -obs)^2/exp )
    
    
    #length(obs) je pocet binu, 2 jsem odhadoval parametry
    quant_chi <- qchisq(0.95,numbins-2-1)
    
    if(X2_stat[chi] >= quant_chi){
      #pokud se zamitne
      vysledek[chi] <- 0
    } else{
      #nezamitne
      vysledek[chi] <- 1
    }
    #####
    
    #chci si pamatovat vsechny parametry v kazdem cyklu, protoze pak hledam ty prislusne k nejlepsi hodnote Pearsonovy stat
    alllambda[chi] <- lambda
    allbeta[chi] <- beta
    allmu[chi] <- mu
    
  }
  
  #Pro ktere alpha byla dosazena nejnizsi hodnota pearsonovy statistiky. Pokud jich bude víc, vezmu ten s mensim alpha
  X2_min <- min(which(X2_stat == min(X2_stat) ) )
  
  
  #Parametry odpovidajici nejlepsimu alpha
  bestalpha <- X2_min
  bestlambda <- alllambda[X2_min]
  bestbeta <- allbeta[X2_min]
  bestmu <- allmu[X2_min]
  
  #odpovidajici nejlepsi Pearsonova statistika
  bestX2 <- X2_stat[X2_min]
  
  
  #Byla pro nejlepsi alpha hypoteza zamitnuta/nezamitnuta
  rozhodnuti <- vysledek[bestalpha]
  
  
  bestparams <- c("alpha"=bestalpha, "beta" = bestbeta,
                  "lambda"=bestlambda, "mu"=bestmu)
  
  
  OUT_resulthyp <- list("Parametry" = bestparams, "Pearson"=bestX2,"RozhodnutiTeor" = rozhodnuti,
                        "RozhodnutiGamma" = rozhodnutiGamma)
  OUT_resulthyp
  
}
#Run like this:
#resulthyp(chunkofdata,0)
