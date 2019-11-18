library(dplyr)
library(tidyverse)

# load all data
setwd("C:/Users/Moi/Desktop/4A MSV/Projet Stratégies de pêche")
load("Agro_OTBCRU20132017.rdata")

OTBCRU %>%glimpse()
OTBCRU <-  OTBCRU %>% mutate(CAP=LAN+DIS)
OTBCRU %>% dplyr::select("CAP") %>% is.na() %>% sum()

print("Résumé du jeu de données complet :")
summary(OTBCRU)

OTBCRU_OPECH <- OTBCRU %>% dplyr::filter(catReg=="All")


## debarques en ligne les operations de peche, en colonne les quantites (en kilo) debarquees de chaque espece
lanquant.co <- OTBCRU %>% dplyr::select(catReg, trpCode,staNum,spp,LAN,rect,month,year,lonIni,latIni) %>% spread(key=spp, value=LAN) 
lanquant.co <- lanquant.co %>% dplyr::filter(catReg=="All")

print("Résumé du jeu de données réduit aux OP echantillonées :")
#summary(OTBCRU_OPECH)

for (i in 9:dim(lanquant.co)[2]){
  lanquant.co[is.na(lanquant.co[,i]),i] <- 0
}
mat.oc.lanquant <- as.matrix(lanquant.co[,9:dim(lanquant.co)[2]])

## Rejetes en ligne les operations de peche, en colonne les quantites (en kilo) debarquees de chaque espece
disquant.co <- OTBCRU %>% dplyr::select(catReg, trpCode,staNum,spp,DIS,rect,month,year,lonIni,latIni) %>% spread(key=spp, value=DIS) 
disquant.co <- disquant.co %>% dplyr::filter(catReg=="All")

for (i in 9:dim(disquant.co)[2]){
  disquant.co[is.na(disquant.co[,i]),i] <- 0
}
mat.oc.disquant <- as.matrix(disquant.co[,9:dim(disquant.co)[2]])


