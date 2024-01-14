
mydf1 <- data.frame(Sada1k0,Sada1k1,Sada1k3)
mydf2 <- data.frame(Sada2k0,Sada2k1,Sada2k3)
mydf3 <- data.frame(Sada3k0,Sada3k1,Sada3k3)



mdf <- melt(mydf1)


BPsada <- ggplot(mdf, aes(x=variable,y=value,group=variable)  ) + 
  geom_boxplot(aes(fill= factor(variable) )) +
  theme(legend.position="right",legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size=17,face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(  size = 17,face="bold",vjust=-0.35),
        axis.title.y = element_text(  size = 17,face="bold",vjust=0.85)) +
  labs( y = "Hodnota")+
  scale_fill_manual(values=c("#999999","#999999","#E69F00","#E69F00","#56B4E9","#56B4E9"),name="Akceptační \nřád",
                    labels = c("k=0", "k=1", "k=3"))+
  scale_x_discrete(name ="Odhady parametrů", 
                 labels=c("odhad_beta"="beta","odhad_mu"="mu",
                          "odhad_beta.1"="beta","odhad_mu.1"="mu",
                          "odhad_beta.2"="beta","odhad_mu.2"="mu"))
  


BPsada

