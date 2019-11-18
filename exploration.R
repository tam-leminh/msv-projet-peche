
#Exploration des données

#150 différentes espèces
especes <- names(lanquant.co)[8:ncol(lanquant.co)]
length(especes) #150

nrow(lanquant.co)
nrow(disquant.co)

##################################
#Poisson 1 : Acantholabrus palloni

p <- 1
nrow(lanquant.co[lanquant.co[,especes[p]] > 0,c(1:7,p+7)]) #0

nrow(disquant.co[disquant.co[,especes[p]] > 0,c(1:7,p+7)]) #1
disquant.co[disquant.co[,especes[p]] > 0,c(1:7,p+7)]

#Pas exploitable...

##################################
#Poisson 2 : Alloteuthis

p <- 2
nrow(lanquant.co[lanquant.co[,especes[p]] > 0,c(1:7,p+7)]) #0

nrow(disquant.co[disquant.co[,especes[p]] > 0,c(1:7,p+7)]) #27
disquant.co[disquant.co[,especes[p]] > 0,c(1:7,p+7)]

##################################
#Pour chaque poisson, nombre de présences en LAN et en DIS

recap <- data.frame(#Espece = especes,
                              LAN.presences = apply(mat.oc.lanquant, 2, function(x) sum(x>0)),
                              DIS.presences = apply(mat.oc.disquant, 2, function(x) sum(x>0)),
                              LAN.masse = apply(mat.oc.lanquant, 2, sum),
                              DIS.masse = apply(mat.oc.disquant, 2, sum))

LAN.maver <- recap$LAN.masse/recap$LAN.presences
DIS.maver <- recap$DIS.masse/recap$DIS.presences

#recap <- cbind(recap, LAN.maver, DIS.maver)

#le "2" dans apply : calcul sur les colonnes

recap
summary(recap[,1]) #LAN
summary(recap[,2]) #DIS
summary(recap[,3]) #LAN masse
summary(recap[,4]) #DIS masse

#NB : espèce accessible avec :
row.names(recap[1,])

#histogrammes de ces valeurs
library(ggplot2)

ggplot(recap,aes(x = LAN.presences))+geom_histogram()
ggplot(recap,aes(x = DIS.presences))+geom_histogram()
ggplot(recap,aes(x = log10(LAN.masse)))+geom_histogram()
ggplot(recap,aes(x = log10(DIS.masse)))+geom_histogram()

summary(recap[,5])
summary(recap[,6])

#En gros les volumes de LAN et DIS sont à peu près équivalents. Pas très intéressant.



