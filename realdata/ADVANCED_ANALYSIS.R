#Komentar ke skriptu
###########################
#Tento skript nacte data z krizovatek, pak si vyberu konkretni krizovatku,
#na ni si vyberu homogenni data (tj vymazu extremni hustoty a rychlosti) a zvolim k (od 0 do 4 by to melo fungovat)
#Pokud dat je dost, tak je rozdelim na kusy po s a s kazdym "chunk"-em testuji Pearsonovym GOF testem
#zda jsou z teoretickeho rozdeleni, kde jsem odhadnul parametry,
#pricemz parametr alpha zkousim pro kazdy chunk mezi 1 az 10. Za finalni odhadnute parametry pak povazuji ty,
#ktere mely nejmensi hodnotu Pearsonovo statistiky. 
#Ze vsechn chunks prislusneho datasetu vyberu ten s nejmensi hodnotou pearsonovo statistiky behem jeho testu
#a prislusny histogram dat vykreslim na obrazek spolecne s gamma fitem(ktery na konci skriptu hodnotim dle CVM) a
#teoretickou distribuci s prislusnymi odhadnutymi parametry
#####

#Pokud je po spusteni error v nlm() funkci, zkus prejit do skriptu "resulthyp.R" 
#a uprav pocatecni odhady. Mozna to pak ten error uz neda


#Nacteni dat a nejaka inicializace, Vybiram zde cislo krizovatky, k,...
###############################################################################

source("realdata/aux_scripts.R")

output <- Nacteni_dat()
Upload <- output[[3]]

#Indexy pro stredne rychla auta v uploadovanych datech
dolniR <- as.numeric(quantile(Upload$Rychlost,0.10))
horniR <- as.numeric(quantile(Upload$Rychlost,0.90))
Stredni_auta <- which(Upload$Rychlost >= dolniR & Upload$Rychlost <=horniR)

#Indexy ktere akceptovaly prave K vozidel
k<- 0

akcept <- which(Upload$k==k)


#Indexy pro prumernou hustotu vozidel
dolniH <- as.numeric(quantile(Upload$Hustota,0.10))
horniH <- as.numeric(quantile(Upload$Hustota,0.90))
prumer <- which(Upload$Hustota >dolniH & Upload$Hustota <horniH )


#Indexy stredne rychlych aut se stredni hustotou ktere akceptovaly k vozidel
stredni <- intersect(intersect(Stredni_auta,akcept),prumer)

#Stredne rychla vozidla akceptovana k vozidly
Data <- Upload$Gap[stredni]
#####

Data <- Data/mean(Data)
#Testovani
############################################################################

#Kolik hodnot chci mit v jednom chunk
s <- 100

#Rozkouskuji data na chunks po s, pokud to lze, pokud ne, tak vypisu warning a pokracuji jakoby nic
#(nechapu proc se nevypise)
if(length(Data)<s){
  ListPearson <- resulthyp(Data,k)
  TeorHyp <- ListPearson$RozhodnutiTeor
  reportX2 <- ListPearson$Pearson
  GammaHyp <- ListPearson$RozhodnutiGamma
  
  #Pro kazdy chunk si ulozim hodnoty parametru
  alphy <- ListPearson$Parametry["alpha"]
  bety <- ListPearson$Parametry["beta"]
  lambdy <- ListPearson$Parametry["lambda"]
  muy <- ListPearson$Parametry["mu"]
  
  
  
  
} else {
  
  #Rozsekani dat na kousky po s
  pocet_chunks <- floor(length(Data)/s)
  
  trunc_data <- Data[1:(pocet_chunks*s)]
  
  split_data <- split(trunc_data, ceiling(seq_along(trunc_data)/s) )
  
  #Sem budu ukladat vysledky Pearsonovych testu pro kazdej chunk (0 zamitnuto, 1 nezamitnuto)
  TeorHyp <- rep(NA,pocet_chunks)
  
  #Sem budu ukladat vysledky Gamam fit testu pro kazdej chunk (0 zamitnuto, 1 nezamitnuto)
  GammaHyp <- rep(NA,pocet_chunks)
  
  #Sem budu ukladat hodnoty Pearsonovych statistik, pak vyberu tu nejnizsi a pro tu vykreslim obrazek
  reportX2 <- rep(NA,pocet_chunks)
  
  
  #Sem si budu ukladat hodnoty parametru
  alphy <- rep(NA,pocet_chunks)
  bety <- rep(NA,pocet_chunks)
  lambdy <- rep(NA,pocet_chunks)
  muy <- rep(NA,pocet_chunks)
  
  
  for(iota in 1:pocet_chunks){
    print(iota)
    sprintf("%i out of %i",iota,pocet_chunks)
    #Provedu test, ulozim si vystup
    ListPearson <- resulthyp(split_data[[iota]],k)
    
    TeorHyp[iota] <- ListPearson$RozhodnutiTeor
    
    GammaHyp[iota] <- ListPearson$RozhodnutiGamma
    
    reportX2[iota] <- ListPearson$Pearson
   
    alphy[iota] <- ListPearson$Parametry["alpha"]
    bety[iota] <- ListPearson$Parametry["beta"]
    lambdy[iota] <- ListPearson$Parametry["lambda"]
    muy[iota] <- ListPearson$Parametry["mu"]
    
  }
  
}
#####

#Grafy hustot,histogramy
###############################################################################

#index chunku s nejmensi hodnotou Pearsonovy stat, pro nej kreslim
Bestfitidx <- min( which(reportX2 == min(reportX2) ) )

#Index "nejhorsiho" chunku
Worstfitidx <- min( which(reportX2 == max(reportX2) ) )

#udaje = synonymum pro data
udajeBest <- split_data[[Bestfitidx]]

udajeWorst <- split_data[[Worstfitidx]]

#Vykresli histogram + gamma fit + odhadnute teoreticke s nejlepsim a nejhorsim Pearsonem na teto krizovatce a specifickym k
A <- Obrazekmocpekny(udajeBest,k,1)
B <- Obrazekmocpekny(udajeWorst,k,0)


#Vykresli mi nejhorsi a nejlepsi shodu na jeden obrazek
figure <- ggarrange(A,B)

annotate_figure(figure )



#Vypise ve vektoru vysledky hypotez (0-zamitnuto,1-nezamitnuto)
TeorHyp
sprintf("Bylo provedených %i Pearsonovo-X2 testů. Z toho byla hypotéza nezamítnuta %i-krát, což činí poměr zamítnutí %f",
        length(TeorHyp),sum(TeorHyp),1-sum(TeorHyp)/length(TeorHyp))


GammaHyp
sprintf("Bylo provedených %i testů hypotézy, že škálované data jsou z gamma distribuce. Z toho byla hypotéza nezamítnuta %i-krát, což činí poměr zamítnutí %f",
        length(GammaHyp),sum(GammaHyp),1-sum(GammaHyp)/length(GammaHyp))
#####

#Boxploty pro parametry
#################################################################################
#postupne jsem ukladal odhadnute parametry s datasetu pro ruzne k, ktere jsem menil nahore ve skriptu rucne
# 
# boxplot(alphy1,alphy2,alphy3,alphy4,names=c("k=0","k=1","k=2","k=3"),
#        main = "Odhady parametru alpha (vedlejší silnice)")
# 
# boxplot(bety1,bety2,bety3,bety4,names=c("k=0","k=1","k=2","k=3"),
#         main = "Odhady parametru beta (vedlejší silnice)")
# 
# boxplot(lambdy1,lambdy2,lambdy3,lambdy4,names=c("k=0","k=1","k=2","k=3"),
#         main = "Odhady parametru lambda=mu (hlavní silnice)")
