

setwd("C:/Users/Moi/Desktop/4A MSV/Projet Stratégies de pêche")
load("Agro_OTBCRU20132017.rdata")
source("Miseenformedonnees.r")
source("fonctions Sylvain.r")

library(glmnet)
library(data.table)
library(ggplot2)
# library(dplyr)
# library(tidyr)

#### Organisation du script ####
#
# Le script utilise les quantités et fonctions introduites
# dans Miseenformedonnees.r et fonctions Sylvain.r.
# Pas besoin de manipuler ces fichiers a priori.
#
# Dans ce script :

# Modèles de régression step (simple/moyenne/jointe)
# pour le modèle basique et pour le modèle à 2 étapes
#
# Figures et autres annexes
#
# Etudes des clusters
#

#### quantités auxiliaires utiles ####

recap <- data.frame(LAN.presences = apply(mat.oc.lanquant, 2, function(x) sum(x>0)),
                    DIS.presences = apply(mat.oc.disquant, 2, function(x) sum(x>0)),
                    LAN.masse = apply(mat.oc.lanquant, 2, sum),
                    DIS.masse = apply(mat.oc.disquant, 2, sum))
spp <- rownames(recap)

vraies_val <- disquant.co[,8:ncol(disquant.co)]

spplan <- rownames(recap[recap$LAN.presences > 0,])
lan.aux <- lanquant.co[,8:ncol(lanquant.co)]
lan.aux <- lan.aux[,spplan]
lan.aux$cosm <- cos(pi/6*as.integer(lanquant.co$month))
lan.aux$sinm <- sin(pi/6*as.integer(lanquant.co$month))
rectLat <- setDT(lanquant.co)[,mean(latIni), by="rect"]
rectLon <- setDT(lanquant.co)[,mean(lonIni), by="rect"]
lan.aux$rectLat <- rectLat$V1[match(lanquant.co$rect, rectLat$rect)]
lan.aux$rectLon <- rectLon$V1[match(lanquant.co$rect, rectLon$rect)]
colnames(lan.aux) <- paste0("c", 1:ncol(lan.aux))

#### Préparation Validation Croisée ####

set.seed(123)

tr <- create_train_test(disquant.co[,8:ncol(disquant.co)], 0.7)

dis_train <- tr$train
dis_test <- tr$test
op_train <- tr$train_ind

cmp_train <- colSums(dis_train>0)
defaillant <- spp[which(cmp_train < 5)]

spp_ok <- spp[which(cmp_train >= 5)]
ld <- length(spp_ok)
lan_train <- lan.aux[op_train,]

dis_tout <- disquant.co[,8:ncol(disquant.co)]

dis.aux <- dis_train[,spp_ok]
colnames(dis.aux) <- paste0("d", 1:ncol(dis.aux))

lmla <- NULL

#### Prédiction des rejets avec modèle de régression simple ####

Y_reg_simple <- reg_lin_simple()

calc_err_CV(Y_reg_simple, title = "Modèle simple")

#Coefficients de régression non nuls :
sum(lmla!=0)
#Soit en % :
sum(lmla!=0)/(dim(lmla)[1]*dim(lmla)[2])

#Matrice de confusion :
table(zeros(Y_reg_simple))


#### Prédiction des rejets avec modèle moyenne ####

Y_moy <- reg_lin_simple(moyenne = TRUE)

calc_err_CV(Y_moy, title = "Moyenne")

#Matrice de confusion :
table(zeros(Y_moy))


#### Modèle de régression jointe ####

lmdis <- NULL

Y_joint <- reg_lin_jointe()

calc_err_CV(Y_joint, title = "Modèle joint")

#Coefficients de régression non nuls(débarquements)
sum(lmla != 0)
sum(lmla != 0)/(dim(lmla)[1]*dim(lmla)[2])

#Coefficients de régression non nuls(rejets)
(sum(lmdis!=0)-length(spp_ok)) #enlever les 62 "1" sur la diagonale
(sum(lmdis!=0)-length(spp_ok))/(dim(lmdis)[1]*dim(lmdis)[2])

#Matrice de confusion
table(zeros(Y_joint))


#### Prédiction de présence : régression logistique ####

aux <- NULL

pred <- pred_pres()


#### Prédiction à 2 étapes ####

Y_simple_2step <- reg_2step_simple(pred)
calc_err_CV(Y_simple_2step, title = "Modèle de régression simple à 2 étapes")
table(zeros(Y_simple_2step))

sum(lmla!=0)
sum(lmla!=0)/(dim(lmla)[1]*dim(lmla)[2])

Y_moy_2step <- reg_2step_simple(pred, moyenne = TRUE)
calc_err_CV(Y_moy_2step, title = "Prédiction de la moyenne à 2 étapes")
table(zeros(Y_moy_2step))

lmdis <- NULL

Y_jointe_2step <- reg_2step_jointe(pred)
calc_err_CV(Y_jointe_2step, title = "Modèle de régression jointe à 2 étapes")
table(zeros(Y_jointe_2step))

sum(lmla!=0)
sum(lmla!=0)/(dim(lmla)[1]*dim(lmla)[2])

(sum(lmdis!=0)-length(spp_ok)) #enlever les 62 "1" sur la diagonale
(sum(lmdis!=0)-length(spp_ok))/(dim(lmdis)[1]*dim(lmdis)[2])

#### Figures et output ####

xmoy <- c()
for (i in 1:nrow(recap)) {
  if (recap[i,]$LAN.presences > 0) {
    co <- mat.oc.lanquant[,i]
    xmoy <- c(xmoy, mean(co[co>0]))
  }
  else {xmoy <- c(xmoy, 0)}
}

ymoy <- c()
for (i in 1:nrow(recap)) {
  if (recap[i,]$DIS.presences > 0) {
    co <- mat.oc.disquant[,i]
    ymoy <- c(ymoy, mean(co[co>0]))
  }
  else {ymoy <- c(ymoy, 0)}
}

rec <- data.frame(Y_pres = apply(mat.oc.disquant, 2, function(x) sum(x>0)),
                  Y_moy_rejet = trunc(ymoy),
                  Y_max = apply(mat.oc.disquant, 2, max),
                  X_pres = apply(mat.oc.lanquant, 2, function(x) sum(x>0)),
                  X_moy_debarq = trunc(xmoy),
                  X_max = apply(mat.oc.lanquant, 2, max))

#### Figure 2.1 : Heatmap (Corrélation entre rejet et débarq pour 56 espèces) ####

lan <- lanquant.co[,c(8:157)]
dis <- disquant.co[,c(8:157)]
colnames(lan) <- paste0("X",c(1:150))
colnames(dis) <- paste0("Y",c(1:150))

non = which(colSums(lan)!=0 & colSums(dis)!=0)

corrmat <- cor(subset(lan, select=c(non)), 
               subset(dis, select=c(non)))

x <- rownames(corrmat)
y <- colnames(corrmat)
data <- expand.grid(X=x, Y=y)
z <- NULL
for (i in 1:length(x)){
  z <- c(z, corrmat[,i])}
data$Z <- z

ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile()+scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                   limits = c(-1,1), midpoint = 0) +
  theme(axis.text.x = element_text(size=6, angle = 90),
        axis.text.y = element_text(size=6))+ggtitle("Corrélations des espèces rejetées et débarquées")+xlab("Débarquements")+
  ylab("Rejets")
# x11(width=20, height = 20) #Pour augmenter la taille de l'image

#### Figure 2.4 : Résidus du modèle linéaire ####
plerr <- NULL
for (i in 1:length(spp)){
  aux <- data.frame(err = (vraies_val[,i]-Y_reg_simple[,i])^2, val = vraies_val[,i])
  colnames(aux) <- c("err", "val")
  plerr <- rbind(plerr, aux)
}

ggplot(data = plerr)+geom_point(aes(x = val, y = err))+
  ggtitle("Résidus du modèle de régression simple")+
  xlab("Valeur réelle du rejet")+
  ylab("(Rejet observé - rejet estimé)²")

#### Figures A1 - A2 : Quantité en fonction du nb de rejets/débarq ####

ggplot(rec)+geom_point(aes(x = log10(1+Y_pres), y = log10(Y_moy_rejet+1)))+
  ggtitle("Quantités rejetées en fonction du nombre de rejets")

ggplot(rec)+geom_point(aes(x = log10(1+X_pres), y = log10(X_moy_debarq+1)))+
  ggtitle("Qtés débarquées en fonction du nombre de débarq.")

#### Figures A3 et A4 : Variabilité mensuelle ####

#Rejets
mrej <- c()
mqr <- c()
mespr <- c()
for (m in c(paste0("0", as.character(1:9)), as.character(10:12))) {
  maux <- mat.oc.disquant[disquant.co$month == m,]
  mrej <- c(mrej, sum(maux>0)/dim(maux)[1])
  mqr <- c(mqr, mean(maux[maux>0]))
  x <- colSums(maux)
  mespr <- c(mespr, length(which(x>0)))
}

ggplot(data = NULL, aes(c(paste0("0", as.character(1:9)), as.character(10:12)), mrej))+
  geom_bar(stat = "identity")+
  xlab("Mois")+ylab("Nombre moyen de rejets par opération")+
  ggtitle("Variabilité mensuelle des rejets - Occurences")

ggplot(data = NULL, aes(c(paste0("0", as.character(1:9)), as.character(10:12)), mqr))+
  geom_bar(stat = "identity")+
  xlab("Mois")+ylab("Quantité moyenne rejetée par opération")+
  ggtitle("Variabilité mensuelle des rejets - Quantités")

ggplot(data = NULL, aes(c(paste0("0", as.character(1:9)), as.character(10:12)), mespr))+
  geom_bar(stat = "identity")+
  xlab("Mois")+ylab("Nombre d'espèces rejetées au moins une fois")+
  ggtitle("Variabilité mensuelle des rejets - Espèces")

#Débarquements
mrej <- c()
mqr <- c()
mespr <- c()
for (m in c(paste0("0", as.character(1:9)), as.character(10:12))) {
  maux <- mat.oc.lanquant[lanquant.co$month == m,]
  mrej <- c(mrej, sum(maux>0)/dim(maux)[1])
  mqr <- c(mqr, mean(maux[maux>0]))
  x <- colSums(maux)
  mespr <- c(mespr, length(which(x>0)))
}

ggplot(data = NULL, aes(c(paste0("0", as.character(1:9)), as.character(10:12)), mrej))+
  geom_bar(stat = "identity")+
  xlab("Mois")+ylab("Nombre moyen de débarquements par opération")+
  ggtitle("Variabilité mensuelle des débarquements - Occurences")

ggplot(data = NULL, aes(c(paste0("0", as.character(1:9)), as.character(10:12)), mqr))+
  geom_bar(stat = "identity")+
  xlab("Mois")+ylab("Quantité moyenne débarquée par opération")+
  ggtitle("Variabilité mensuelle des débarquements - Quantités")

ggplot(data = NULL, aes(c(paste0("0", as.character(1:9)), as.character(10:12)), mespr))+
  geom_bar(stat = "identity")+
  xlab("Mois")+ylab("Nombre d'espèces débarquées au moins une fois")+
  ggtitle("Variabilité mensuelle des débarquements - Espèces")

#### Figures A5 et A6 : Variabilité spatiale ####

#Débarquements
rrej <- c()
rqr <- c()
respr <- c()

for (m in unique(lanquant.co$rect)) {
  maux <- mat.oc.lanquant[lanquant.co$rect == m,]
  rqr <- c(rqr, mean(maux[maux>0]))
  if (m != "19E8" & m != "24E4") { #Pour ces rectangles, une seule OP
    x <- colSums(maux)
    rrej <- c(rrej, sum(maux>0)/dim(maux)[1])
  }
  else {
    x <- sum(maux)
    rrej <- c(rrej, sum(maux>0))
  }
  respr <- c(respr, length(which(x>0)))
}

ggplot(data = NULL, aes(unique(lanquant.co$rect), rrej))+
  geom_bar(stat = "identity")+
  xlab("Rectangle de pêche")+ylab("Nombre moyen de débarquements par opération")+
  ggtitle("Variabilité spatiale des débarquements - Occurences")

ggplot(data = NULL, aes(unique(lanquant.co$rect), rqr))+
  geom_bar(stat = "identity")+
  xlab("Rectangle de pêche")+ylab("Quantité moyenne débarquée par opération")+
  ggtitle("Variabilité spatiale des débarquements - Quantités")

ggplot(data = NULL, aes(unique(lanquant.co$rect), respr))+
  geom_bar(stat = "identity")+
  xlab("Rectangle de pêche")+ylab("Nombre d'espèces débarquées au moins une fois")+
  ggtitle("Variabilité spatiale des débarquements - Espèces")

#Rejets
rrej <- c()
rqr <- c()
respr <- c()

for (m in unique(disquant.co$rect)) {
  maux <- mat.oc.disquant[disquant.co$rect == m,]
  rqr <- c(rqr, mean(maux[maux>0]))
  if (m != "19E8" & m != "24E4") {
    x <- colSums(maux)
    rrej <- c(rrej, sum(maux>0)/dim(maux)[1])
  }
  else {
    x <- sum(maux)
    rrej <- c(rrej, sum(maux>0))
  }
  respr <- c(respr, length(which(x>0)))
}

ggplot(data = NULL, aes(unique(lanquant.co$rect), rrej))+
  geom_bar(stat = "identity")+
  xlab("Rectangle de pêche")+ylab("Nombre moyen de rejets par opération")+
  ggtitle("Variabilité spatiale des rejets - Occurences")

ggplot(data = NULL, aes(unique(lanquant.co$rect), rqr))+
  geom_bar(stat = "identity")+
  xlab("Rectangle de pêche")+ylab("Quantité moyenne rejetée par opération")+
  ggtitle("Variabilité spatiale des rejets - Quantités")

ggplot(data = NULL, aes(unique(lanquant.co$rect), respr))+
  geom_bar(stat = "identity")+
  xlab("Rectangle de pêche")+ylab("Nombre d'espèces rejetées au moins une fois")+
  ggtitle("Variabilité spatiale des rejets - Espèces")

#### Annexe B : Recap rares ####

rec[defaillant,]
#write.csv(rec[defaillant,], "C:/Users/Moi/Desktop/recap_rares.csv")


#### Clustering et prédiction des espèces rares ####

#### Visualisation des espèces rares ####

ggplot(recap, aes(x = recap$DIS.presences, y = log10(recap$DIS.masse+1)))+geom_point()

recapdis <- recap[recap$DIS.presences > 0,]

kmeansAIC <- function(cluster){
  return(cluster$tot.withinss+2*nrow(cluster$centers)*ncol(cluster$centers))
}
#Erreur selon le nombre de clusters

recapdis$DIS.masselog10 <- log10(1+recapdis$DIS.masse)

#### Clustering sur le nombre de rejets > 0 ####

opti_clusters <- NULL
for (k in c(2:9)) {
  cluster_k <- kmeans(recapdis[,c("DIS.presences", "DIS.masselog10")], k, nstart = 20)
  cluster_k <- kmeans(recapdis[,c("DIS.presences")], k, nstart = 20)
  # print(ggplot(recapdis)+geom_point(aes(x = recapdis$DIS.presences, y = recapdis$DIS.masselog10,
  #                                       color = as.factor(as.vector(cluster_k$cluster))))+
  #         scale_color_discrete(name = "Clusters")+xlab("Présences DIS")+ylab("Masse DIS")+ggtitle(paste0("Espèces - ", k, " clusters")))
  err <- cluster_k$tot.withinss
  aic <- kmeansAIC(cluster_k)
  opti_clusters <- rbind(opti_clusters, data.frame(k = k, AIC = aic, err = err))
}

ggplot(data = opti_clusters, aes(x = opti_clusters$k, y = opti_clusters$err))+geom_point()+
  ggtitle("Erreur selon le nombre de clusters")+xlab("k clusters")+ylab("Erreur")

k <- 6
cluster_rare <- kmeans(recapdis[,c("DIS.presences")], k, nstart = 20)
ggplot(recapdis)+geom_point(aes(x = recapdis$DIS.presences, y = recapdis$DIS.masselog10,
                                color = as.factor(as.vector(cluster_rare$cluster))))+
  scale_color_discrete(name = "Clusters")+xlab("Présences dans les rejets")+ylab("Masse totale rejetée (log) (non clusterisée)")+
  ggtitle(paste0(k, " clusters kmeans sur le nombre de rejets"))

ind <- cluster_rare$cluster[1]
#1 = position de Acantholabrus palloni présent une seule fois dans les DIS

#### Clustering des espèces rares sur les quantités moyennes rejetées ####

nrow(recapdis) #nb d'espèces avec au moins 1 rejet > 0
recapdis[cluster_rare$cluster == ind,]
nrow(recapdis[cluster_rare$cluster == ind,]) #nb d'espèces rarement rejetées
rares <- rownames(recapdis[cluster_rare$cluster == ind,])
nrow(recapdis[cluster_rare$cluster != ind,]) #nb d'espèces souvent rejetées
max(recapdis[cluster_rare$cluster == ind,]$DIS.presences)
min(recapdis[cluster_rare$cluster != ind,]$DIS.presences)

recap.rare <- recapdis[cluster_rare$cluster == ind,]
recap.rare$DIS.massemoy <- recap.rare$DIS.masse/recap.rare$DIS.presences

opti_clusters <- NULL
for (k in c(2:10)) {
  cluster_k <- kmeans(log10(recap.rare$DIS.massemoy), k, nstart = 20)
  # print(ggplot(recap.rare)+geom_point(aes(x = recap.rare$DIS.presences, y = recap.rare$DIS.massemoy,
  #                                         color = as.factor(as.vector(cluster_k$cluster))))+
  #         scale_color_discrete(name = "Clusters")+xlab("Présences DIS")+ylab("Masse DIS moy")+ggtitle(paste0("Espèces - ", k, " clusters")))
  err <- cluster_k$tot.withinss
  aic <- kmeansAIC(cluster_k)
  opti_clusters <- rbind(opti_clusters, data.frame(k = k, AIC = aic, err = err))
}

ggplot(data = opti_clusters, aes(x = opti_clusters$k, y = opti_clusters$err))+geom_point()+
  ggtitle("Erreur selon le nombre de clusters - masse")+xlab("k clusters")+ylab("Erreur")

k <- 5
clust.rare.moy <- kmeans(log10(recap.rare[,"DIS.massemoy"]), k, nstart = 20)
ggplot(recap.rare)+geom_point(aes(x = recap.rare$DIS.presences, y = log10(recap.rare$DIS.massemoy),
                                  color = as.factor(as.vector(clust.rare.moy$cluster))))+
  scale_color_discrete(name = "Clusters")+xlab("Présences dans les rejets")+
  ylab("log10(Masse moyenne rejetée)")+ggtitle(paste0("74 espèces rares - ", k, " clusters"))

clust.rare.moy

recap.rare$clust <- clust.rare.moy$cluster
recap$clust <- -1
for (i in 1:nrow(recap)) {
  n <- rownames(recap[i,])
  if (n %in% rownames(recap.rare)) {
    clu <- recap.rare[n, "clust"]
    recap[i, "clust"] <- clu
  }
}

table(recap$clust)
#le cluster -1 correspond aux espèces abondantes ou complétement absentes

#On supprime les deux espèces singulières évoquées dans le rapport :
# Deltentosteus quadrimaculatus 17
# Raja undulata 59
rares <- rares[-c(17, 59)]

sppdis2 <- c(rownames(recap), "cluster1", "cluster2", "cluster3", "cluster4", "cluster5")
sppdis2 <- sppdis2[!(sppdis2 %in% rares)]
ldis2 <- length(sppdis2) #46 abondantes + 2 singulières + 5 clusters + 30 absentes

dis.aux <- disquant.co[,8:ncol(disquant.co)]
c1 <- matrix(rep(0, len = nrow(disquant.co)), nrow = nrow(disquant.co))
c2 <- matrix(rep(0, len = nrow(disquant.co)), nrow = nrow(disquant.co))
c3 <- matrix(rep(0, len = nrow(disquant.co)), nrow = nrow(disquant.co))
c4 <- matrix(rep(0, len = nrow(disquant.co)), nrow = nrow(disquant.co))
c5 <- matrix(rep(0, len = nrow(disquant.co)), nrow = nrow(disquant.co))
colcl <- cbind(c1, c2, c3, c4, c5)
colnames(colcl) <- c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5")
for (s in rares){
  cl <- recap[s, "clust"]
  nom_clust <- paste0("cluster", cl)
  df <- data.frame(colcl[,which(colnames(colcl)==nom_clust)], disquant.co[,s])
  somme <- rowSums(df, na.rm=T)
  colcl[,which(colnames(colcl)==nom_clust)] <- somme
}
dis.aux <- cbind(dis.aux, colcl)
dis.aux <- dis.aux[,sppdis2]

#### Régressions en utilisant les clusters ####

spp<- colnames(dis.aux)

dis_train <- dis.aux[op_train,]
dis_test <- dis.aux[-op_train,]

cmp_train <- colSums(dis_train>0)
defaillant <- spp[which(cmp_train < 5)]
defaillant <- c(defaillant, rownames(recap[recap$DIS.presences==0,]))
print(defaillant)

spp_ok <- spp[which(cmp_train >= 5)]
ld <- length(spp_ok)
lan_train <- lan.aux[op_train,]

dis_tout <- dis.aux

dis.aux <- dis_train[,spp_ok]
colnames(dis.aux) <- paste0("d", 1:ncol(dis.aux))

dim(dis.aux)

Ycl <- reg_lin_simple()
sppdis2 <- c(rownames(recap), "cluster1", "cluster2", "cluster3", "cluster4", "cluster5")
sppdis2 <- sppdis2[!(sppdis2 %in% rares)]
Ycl <- Ycl[,sppdis2]

#l'affichage graphique de l'erreur pour les clusters n'a pas été codé
calc_err_CV(Ycl, vrv = dis_tout, title = "Régression simple avec clusters")

Yclmoy <- reg_lin_simple(moyenne = TRUE)
#l'affichage graphique de l'erreur pour les clusters n'a pas été codé
calc_err_CV(Yclmoy, vrv = dis_tout, title = "moyenne")


#### Clusters hiérarchiques ####

disquant.rare <- disquant.co[,rares]
pres.rare <- disquant.rare>0
rowSums(disquant.rare>0)

#proxim ci-dessous donne une idée des corrélations entre espèces rejetées
#Décompte du nb d'OP où 2 espèces sont rejetées simultanément
proxim <- matrix(rep(0, len = length(rares)^2), nrow = length(rares))
for (i in 1:(length(rares)-1)){
  for (j in (i+1):length(rares)){
    x <- sum(pres.rare[,i]&pres.rare[,j])
    proxim[i,j] <- x
    proxim[j,i] <- x
  }
}
rowSums(proxim)

#prox <- dist(t(disquant.rare), method = "euclidean")
#prox <- dist(t(disquant.rare), method = "maxim")
#prox <- dist(t(disquant.rare), method = "man")
prox <- dist(t(disquant.rare), method = "can")
#prox <- dist(t(disquant.rare), method = "binary")
#prox <- dist(t(disquant.rare), method = "minkowski")

#x11(width=20, height = 20)
cah <- hclust(prox, method = "ward.D")
plot(cah, cex = 0.6)
abline(h = 412.5, col = "red")

mycl <- cutree(cah, h=412.5)
unique(mycl) #11 clusters

# cah <- hclust(prox3, method = "ward.D2")
# plot(cah, cex = 0.6)
# cah <- hclust(prox3, method = "complete")
# plot(cah, labels = FALSE)

# prox_pres <- dist(t(pres.rare))
# cah2 <- hclust(prox_pres)
# plot(cah2)

recap$clust <- -1

for (i in 1:nrow(recap)) {
  n <- rownames(recap[i,])
  if (n %in% rares) {
    clu <- mycl[which(names(mycl)==n)]
    recap[i, "clust"] <- clu
  }
}

dis.aux <- disquant.co[,8:ncol(disquant.co)]

spp<- colnames(dis.aux)

dis_train <- dis.aux[op_train,]
dis_test <- dis.aux[-op_train,]

cmp_train <- colSums(dis_train>0)
defaillant <- spp[which(cmp_train < 5 & recap$clust == -1)]
#print : Clusters regroupant trop peu de rejets
for (i in 1:11){
  s <- rownames(recap[recap$clust == i,])
  cmp <- 0
  for (ss in s){
    cmp <- cmp + cmp_train[ss]
  }
  if (cmp <5){
    defaillant <- c(defaillant, s)
    print(i)
  }
}

spp_ok <- c()
for (s in spp){
  if (!(s %in% defaillant)) {spp_ok <- c(spp_ok, s)}
}
ld <- length(spp_ok)
lan_train <- lan.aux[op_train,]

dis_tout <- dis.aux

dis.aux <- dis_train[,spp_ok]
colnames(dis.aux) <- paste0("d", 1:ncol(dis.aux))

dim(dis.aux)

Y_hcl <- reg_simple_clusters()
calc_err_CV(Y_hcl, vrv = dis_tout, title = "Régression simple avec clusters hiérarchiques")

#### Fin ####
