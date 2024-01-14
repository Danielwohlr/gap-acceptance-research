#obrazek, ze svetlosti nejsou exponencialni

output <- Nacteni_dat()

#Do promene Total_data muzu ulozit data bud z mereni 1,2,3 a nebo vsechny sloucene, tj index 4
Total_data <- output [[3]]


mydata <- Total_data$Gap

#Gamma fit
fit.gamma <- mydata %>%
  fitdist(distr= 'gamma', method = 'mle')

shape_est <- fit.gamma$estimate["shape"]
rate_est <- fit.gamma$estimate["rate"]

#Exp fit
fit.exp <- mydata %>%
  fitdist(distr= 'exp', method = 'mle')

exp_rate_est <- fit.exp$estimate["rate"]


#Kresleni gamma a exp
sourx = seq(from=0,to=20, length.out=10000)
#gamma
gammahustota <- dgamma(sourx,shape = shape_est, rate = rate_est)
gammahodnoty <- data.frame(sourx,gammahustota)
#exp
exphustota <- dexp(sourx,rate=exp_rate_est)
exphodnoty <- data.frame(sourx,exphustota)

mydf <- data.frame(mydata)

nejsouexp <- ggplot() +
  geom_histogram( aes(x=mydata, y = ..density..),fill = "green", 
                  color = "black", bins=20, boundary = 0 ) +
  labs(title = "Světlosti na třetí křižovatce",
       y = 'Hustota (1/s)', x='Časová světlost (s)')+
  theme(plot.title = element_text(size=15))


histogr <- nejsouexp + 
  geom_line(data = gammahodnoty, aes(x=sourx,y=gammahustota,colour = "Gamma"),lwd=1.3) +
  geom_line( data = exphodnoty,aes(x=sourx,y=exphustota,colour = "Exponenciální"),lwd=1.3) +
  scale_color_manual(values = c(
    "Exponenciální" = 'blue',
    "Gamma" = 'red')) +
  labs(color = 'Typ distribuce') +
  theme(legend.title = element_text(size=14, face = "bold"),
        legend.position = c(0.75, 0.7),
        axis.title.x = element_text( size = 14,face="bold", vjust=-0.35),
        axis.title.y = element_text(  size = 14,face="bold",vjust=0.85),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=13),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14) )+
  xlim(0,20)

histogr

porov  <- ggarrange(histogr1,ggarrange(histogr2,histogr3,ncol = 2),nrow = 2)

annotate_figure(porov)



