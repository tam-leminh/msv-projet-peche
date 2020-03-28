rm(list=objects())
library(dplyr)
library(tidyverse)

# load all data
load("Agro_OTBCRU20132017.rdata")

OTBCRU %>%glimpse()
OTBCRU <-  OTBCRU %>% mutate(CAP=LAN+DIS)
OTBCRU %>% dplyr::select("CAP") %>% is.na() %>% sum()
OTBCRU_OPECH <- OTBCRU %>% dplyr::filter(catReg=="All")

print("Résumé du jeu de données complet :")
summary(OTBCRU)
print("Résumé du jeu de données réduit aux OP echantillonées :")
summary(OTBCRU_OPECH)



## debarques en ligne les operations de peche, en colonne les quantites (en kilo) debarquees de chaque estpece
lanquant.co <- OTBCRU_OPECH %>% dplyr::select(trpCode,staNum,spp,LAN,rect,month,year,lonIni,latIni) %>% spread(key=spp, value=LAN) 


for (i in 8:dim(lanquant.co)[2]){
  lanquant.co[is.na(lanquant.co[,i]),i] <- 0
}
mat.oc.lanquant <- as.matrix(lanquant.co[,8:dim(lanquant.co)[2]])

## Rejetes en ligne les operations de peche, en colonne les quantites (en kilo) debarquees de chaque espece
disquant.co <- OTBCRU_OPECH %>% dplyr::select(trpCode,staNum,spp,DIS,rect,month,year,lonIni,latIni) %>% spread(key=spp, value=DIS) 

for (i in 8:dim(disquant.co)[2]){
  disquant.co[is.na(disquant.co[,i]),i] <- 0
}
mat.oc.disquant <- as.matrix(disquant.co[,8:dim(disquant.co)[2]])


