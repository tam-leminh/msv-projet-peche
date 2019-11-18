
#clustering des espèces

recap_clus <- recap[recap$LAN.presences+recap$DIS.presences>0,1:4]
nrow(recap_clus) #150
nrow(recap) #164

library(ggplot2)

ggplot(recap_clus)+geom_point(aes(x = recap_clus$LAN.presences, y = recap_clus$DIS.presences))

ggplot(recap_clus)+geom_point(aes(x = recap_clus$LAN.masse, y = recap_clus$DIS.masse))
ggplot(recap_clus)+geom_point(aes(x = log(recap_clus$LAN.masse), y = log(recap_clus$DIS.masse)))

clust <- kmeans(recap_clus[1:2], 3)
clust$cluster

ggplot(recap_clus)+geom_point(aes(x = recap_clus$LAN.presences, y = recap_clus$DIS.presences,
                                  color = as.factor(as.vector(clust$cluster))))+
                  scale_color_discrete(name = "Clusters")+xlab("#Débarquée")+ylab("#Rejetée")+ggtitle("Espèces")

kmeansAIC <- function(cluster){
  return(cluster$tot.withinss+2*nrow(cluster$centers)*ncol(cluster$centers))
}

#Erreur selon le nombre de clusters

opti_clusters <- NULL
for (k in c(1:25)) {
  cluster_k <- kmeans(recap_clus[1:2], k, nstart = 20)
  err <- cluster_k$tot.withinss
  aic <- kmeansAIC(cluster_k)
  opti_clusters <- rbind(opti_clusters, data.frame(k = k, AIC = aic, err = err))
}

ggplot(data = opti_clusters, aes(x = opti_clusters$k, y = opti_clusters$err))+geom_point()+
  ggtitle("Erreur selon le nombre de clusters")+xlab("k clusters")+ylab("Erreur")

opti_clusters$deriv_err <- NULL
for (i in 2:(nrow(opti_clusters)-1)){
  opti_clusters[i, "deriv_err"] <- 0.5*(opti_clusters[i+1, "err"]-opti_clusters[i-1, "err"])
}

ggplot(data = opti_clusters, aes(x = opti_clusters$k, y = opti_clusters$deriv_err))+geom_point()

for (k in 2:9) {
  c <- kmeans(recap_clus[1:2], k, nstart = 20)
  tailles <- paste(c$size, collapse = ', ')
  print(ggplot(recap_clus)+geom_point(aes(x = recap_clus$LAN.presences, y = recap_clus$DIS.presences,
                                    color = as.factor(as.vector(c$cluster))))+
    scale_color_discrete(name = "Clusters")+xlab("Occurrences Débarquement")+ylab("Occurrences Rejet")+
    ggtitle(paste0("Espèces - ", k, " clusters (", tailles, ")")))
  #ggsave(paste0("clusters_", k,".png"))
}

# Autres variables d'abondance/fréquence

recap_clus$Abondance <- recap_clus$LAN.presences+recap_clus$DIS.presences
mx <- max(recap_clus$Abondance)
recap_clus$FreqRejet <- recap_clus$DIS.presences/recap_clus$Abondance
recap_clus$Abondance <- recap_clus$Abondance/mx

ggplot(recap_clus)+geom_point(aes(x = recap_clus$FreqRejet, y = recap_clus$Abondance))

opti_clusters <- NULL
for (k in c(1:25)) {
  cluster_k <- kmeans(recap_clus[5:6], k, nstart = 20)
  err <- cluster_k$tot.withinss
  opti_clusters <- rbind(opti_clusters, data.frame(k = k, err = err))
}

ggplot(data = opti_clusters, aes(x = opti_clusters$k, y = opti_clusters$err))+geom_point()+
  ggtitle("Erreur selon le nombre de clusters - AbFreq")+xlab("k clusters")+ylab("Erreur")

opti_clusters$deriv_err <- NULL
for (i in 2:(nrow(opti_clusters)-1)){
  opti_clusters[i, "deriv_err"] <- 0.5*(opti_clusters[i+1, "err"]-opti_clusters[i-1, "err"])
}

ggplot(data = opti_clusters, aes(x = opti_clusters$k, y = opti_clusters$deriv_err))+geom_point()

for (k in 2:9) {
  c <- kmeans(recap_clus[5:6], k, nstart = 20)
  tailles <- paste(c$size, collapse = ' ')
  print(ggplot(recap_clus)+geom_point(aes(x = recap_clus$FreqRejet, y = recap_clus$Abondance,
                                          color = as.factor(as.vector(c$cluster))))+
          scale_color_discrete(name = "Clusters")+xlab("Fréquence rejet")+ylab("Abondance")+
          ggtitle(paste0("Espèces - ", k, " clusters (", tailles, ")")))
  #ggsave(paste0("clusters_", k,".png"))
}

clustabfreq4 <- kmeans(recap_clus[5:6], 4, nstart = 20, iter.max = 20)
tailles <- paste(clustabfreq4$size, collapse = ', ')
ggplot(recap_clus)+geom_point(aes(x = recap_clus$FreqRejet, y = recap_clus$Abondance,
                                  color = as.factor(as.vector(clustabfreq4$cluster))))+
  scale_color_discrete(name = "Clusters")+xlab("Fréquence rejet")+ylab("Abondance")+
  ggtitle(paste0("Espèces - ", 4, " clusters (", tailles, ")"))

#pour chaque opération de pêche, sommer selon les clusters
spp <- OTBCRU_OPECH[-which(duplicated(OTBCRU_OPECH$spp)),]$spp

OPclust <- lanquant.co[,2:8] #seulement info de l'OP
OPclust$LAN1 <- 0
OPclust$LAN2 <- 0
OPclust$LAN3 <- 0
OPclust$LAN4 <- 0
OPclust$DIS1 <- 0
OPclust$DIS2 <- 0
OPclust$DIS3 <- 0
OPclust$DIS4 <- 0

for(i in 1:nrow(OTBCRU_OPECH)) {
  lan <- OPclust[OPclust$trpCode == as.character(OTBCRU_OPECH[i, "trpCode"]) & OPclust$staNum == as.double(OTBCRU_OPECH[i, "staNum"]),paste0("LAN", clustabfreq4$cluster[as.character(OTBCRU_OPECH[i, "spp"])])]
  dis <- OPclust[OPclust$trpCode == as.character(OTBCRU_OPECH[i, "trpCode"]) & OPclust$staNum == as.double(OTBCRU_OPECH[i, "staNum"]),paste0("DIS", clustabfreq4$cluster[as.character(OTBCRU_OPECH[i, "spp"])])]
  OPclust[OPclust$trpCode == as.character(OTBCRU_OPECH[i, "trpCode"]) & OPclust$staNum == as.double(OTBCRU_OPECH[i, "staNum"]),paste0("LAN", clustabfreq4$cluster[as.character(OTBCRU_OPECH[i, "spp"])])] <- lan + OTBCRU_OPECH[i, "LAN"]
  OPclust[OPclust$trpCode == as.character(OTBCRU_OPECH[i, "trpCode"]) & OPclust$staNum == as.double(OTBCRU_OPECH[i, "staNum"]),paste0("DIS", clustabfreq4$cluster[as.character(OTBCRU_OPECH[i, "spp"])])] <- dis + OTBCRU_OPECH[i, "DIS"]
}

#"long" car pas propre, à refaire en vectorisé

vec <- as.vector(names(OPclust[,8:ncol(OPclust)]))
vec <- vec[-which(vec == "DIS1")]
par <- paste(vec, collapse ="+")
frm <- as.formula(paste("DIS1 ~ ", par, sep = ""))
test1 <- lm(frm, data = OPclust)
summary(test1)

# généraliser : pour chaque DIS, une valeur (faire un frame propre...)

vec <- as.vector(names(OPclust[,8:ncol(OPclust)]))
vec <- vec[-which(vec == "DIS2")]
par <- paste(vec, collapse ="+")
frm <- as.formula(paste("DIS2 ~ ", par, sep = ""))
test2 <- lm(frm, data = OPclust)
summary(test2)

vec <- as.vector(names(OPclust[,8:ncol(OPclust)]))
vec <- vec[-which(vec == "DIS3")]
par <- paste(vec, collapse ="+")
frm <- as.formula(paste("DIS3 ~ ", par, sep = ""))
test3 <- lm(frm, data = OPclust)
summary(test3)

vec <- as.vector(names(OPclust[,8:ncol(OPclust)]))
vec <- vec[-which(vec == "DIS4")]
par <- paste(vec, collapse ="+")
frm <- as.formula(paste("DIS4 ~ ", par, sep = ""))
test4 <- lm(frm, data = OPclust)
summary(test4)

print(test1)
print(test2)
print(test3)
print(test4)

#en vue d'obtenir un syst linéaire :

Inter <- c(test1$coefficients[1], test2$coefficients[1], test3$coefficients[1], test4$coefficients[1])
#intercept pour DIS1,2,3,4

L <- rbind(test1$coefficients[2:5],test2$coefficients[2:5], test3$coefficients[2:5], test4$coefficients[2:5])
#qtés landed

D <- rbind(c(1, (-1)*test1$coefficients[6:8]),
           c((-1)*test2$coefficients[6], 1, (-1)*test2$coefficients[7:8]),
           c((-1)*test3$coefficients[6:7], 1, (-1)*test3$coefficients[8]),
           c((-1)*test4$coefficients[6:8],1))
colnames(D)[1] <- "DIS1"
#qtés discarded

PredictMassClust <- function(trp, sta) {
  X <- t(as.matrix(OPclust[OPclust$trpCode == trp & OPclust$staNum == sta, 8:11]))
  B <- L%*%X+Inter
  print(B)
  Y <- solve(D, B)
  return(Y)
}

#essai
trp <- as.character(OPclust[1, "trpCode"])
sta <- as.double(OPclust[1, "staNum"])

y1 <- PredictMassClust(trp, sta)

#globalement

DISclust <- t(PredictMassClust(OPclust$trpCode, OPclust$staNum))

s <- 0
for (i in 1:ncol(DISclust)) {
  for (j in 1:nrow(OPclust)) {
    s <- s+ (OPclust[j, 11+i]-DISclust[j, i])^2
  }
}

#PCA : adegenet

