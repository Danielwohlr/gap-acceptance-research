

vnitrnisuma <- function(t,alpha,beta,uplim,l){
  vysledek <- 0
  for(k in 1:uplim){
    mezivysledek = k * (beta*t)^(k*alpha+l)/factorial(k*alpha+l)
    vysledek = vysledek + mezivysledek
  }
  vysledek
}

vnejsisuma <- function(t,alpha,beta,uplim){
  vysledek <- 0
  for(l in 0:(alpha-1) ){
    mezivysledek = vnitrnisuma(t,alpha,beta,uplim,l)
    vysledek = vysledek + mezivysledek
  }
  vysledek
}



siegloch <- function(t,alpha,beta,uplim){
  exp(-beta*t)*vnejsisuma(t,alpha,beta,uplim)
}
  
sour = seq(from=0,to=0.75, length.out=1000)

sieg1 <- sapply(sour,siegloch,alpha=1,beta=5,uplim=50)
sieg2 <- sapply(sour,siegloch,alpha=2,beta=2,50)
sieg3 <- sapply(sour,siegloch,alpha=2,beta=10,50)
sieg4 <- sapply(sour,siegloch,alpha=8,beta=20,20)
sieg5 <- sapply(sour,siegloch,alpha=2,beta=5,50)
sieg6 <- sapply(sour,siegloch,alpha=5,beta=10,20)
sieg7 <- sapply(sour,siegloch,alpha=2,beta=6,50)


sloucenesiegl<- data.frame(sour,sieg1,sieg2,sieg3,sieg4,sieg6,sieg7)

mdf <- melt(sloucenesiegl,id = "sour")

#vytvoreni ggplot objeckutu

A <-  ggplot(mdf,aes(x=sour,y=value,color = variable)  ) + 
  geom_line( lwd=1.5)  +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.65),
        axis.title.x = element_text(  size = 20,face="bold",vjust=-0.35),
        axis.title.y = element_text(  size = 20,face="bold",vjust=0.85),
        legend.text = element_text(size=20),
        legend.key.size = unit(1, 'cm'),
        axis.text.x = element_text(face="bold",size=14),
        axis.text.y = element_text(face="bold",size=14) ) +
  labs(x="t",y= "s(t)")
A

