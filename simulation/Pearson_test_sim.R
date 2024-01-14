#Pearsonův X2 test dobre shody, jestli data pochazeji z teoreticke distribuce

#dejta napr ve tvaru X_0, k je odpovidajici pocet akcept aut
PearsonX2 <- function(k,dejta){
  
  ########################
  #Rozsekani dat na chunks
  
  pocet_odhadu <- floor(length(dejta)/100)
  
  trunc_data <- dejta[1:(pocet_odhadu*100)]
  
  split_data <- split(trunc_data, ceiling(seq_along(trunc_data)/100) )
  
  #Zde se budou ukladat vysledky zamitnuti/nezamitnuti
  X2test <- rep(NA,pocet_odhadu)
  #Zde ukladam vysledky pro gamma test
  rozhodnutiGamma <-rep(NA,pocet_odhadu)
  
  #pocet binu na ktery rozsekam definicni obor
  numbins <- 10
  
  
  #Predpis teoreticke hustoty, CDF. ICDF 
  #####################################
  
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
    uniroot( function(x) {Kumulativni(x,k)-p},lower=0,upper=10 )$root
  }
  
  
  #####
  
  
  
  
  ###############################
  #Pro kazdy kus dat provede test
  
  for( j in 1: pocet_odhadu){
    
    #Do data se ulozi chunk dat
    data <- split_data[[j]]
    
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
      obs[q] <- which(edges[q] < data & data <= edges[q+1]) %>%
        length()
    }
    
    #zvlast posledni bin
    obs[numbins] <- which(edges[numbins] < data) %>%
      length()
    
    
    exp <- length(data)*prob
    
    #Vypocitani Pearsonovy statistiky
    X2_stat <- sum( (exp -obs)^2/exp )
    
    #length(obs) je pocet binu, 2 jsem odhadoval parametry
    quant_chi <- qchisq(0.95,numbins-1)
    
    
    
    if(X2_stat >= quant_chi){
      #pokud se zamitne
      vysledek <- 0
    } else{
      #nezamitne
      vysledek <- 1
    }
    
    X2test[j] <- vysledek
    
    # GoF test, ze data jsou gamma s odhadnutymi parametry
    vysledekGamma <- gamma_test(data)$p.value
    
    if(vysledekGamma >= 0.05){
      rozhodnutiGamma[j] = 1
    } else{
      rozhodnutiGamma[j] = 0
    }
    
    
  }
  
  #Ulozim si jednicky a nuly jako vysledky hypotez, rozhoduji podle p-value
  #1 odpovida nezamitnuti hypotezy, 0 zamitnuti
  
  #Vysledky_X2 <- table(X2test)
  
  #Barplot pro znazorneni pomeru zamitnuti/nezamitnuti hypotezy, ze data jsou z gamma
  # barplot_hypoteza <- barplot(Vysledky_X2, 
  #                             main ="Poměr zamítnutí/nezamítnutí hypotéz, že data pocházejí z odvozeného rozdělení",
  #                             names.arg = c("Zamítnutí","Nezamítnutí"),
  #                             col =c("mistyrose","lavender")
  #                             )
  # #Napise do grafu pocet zamitnuti, nezamitnuti
  # text(barplot_hypoteza,0,round(Vysledky_X2,1),cex=5,pos=3 )
  
  #Podil nezamitnuti/celkovy pocet testu
  Vysledky_X2 <- as.integer(X2test)
  #Podil pearson
  ratioP <- 1-sum(Vysledky_X2)/length(Vysledky_X2)
  #podil Gamma
  ratioG <- 1-sum(rozhodnutiGamma)/length(rozhodnutiGamma)
  
  OutputTest <- list("Pearson" = ratioP,
                        "Gamma" = ratioG)
  OutputTest
  
}
#Zkouska
Ambrosie <- PearsonX2(4,X_4)
Ambrosie$Pearson
Ambrosie$Gamma


#X_4 <- X_4[1:100000]
