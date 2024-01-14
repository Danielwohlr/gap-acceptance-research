# Helper function to create a data frame
create_df <- function(prefix) {
  df <- data.frame(get(paste0(prefix, "k0")),
                   get(paste0(prefix, "k1")),
                   get(paste0(prefix, "k3")))
  names(df) <- c("k0", "k1", "k3")
  return(df)
}

# Main code
mydf1 <- create_df("Sada1")
mydf2 <- create_df("Sada2")
mydf3 <- create_df("Sada3")

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

