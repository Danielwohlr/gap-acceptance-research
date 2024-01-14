# This script creates histogram of clearances of k vehicles for k=0,1,2,3 at crossroad no. 3
# Also theoretical density is plotted with estimated parameters which are saved
source("realdata/aux_scripts.R")
output <- Nacteni_dat()
Upload <- output[[1]]


#Indexy ktere akceptovaly prave K vozidel
k<- 0
figure <- ggarrange(Createfig(0,3),Createfig(1,2.5),Createfig(2,2),Createfig(3,2))
# 
annotate_figure(figure)


Createfig(k,3)
