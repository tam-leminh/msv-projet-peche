#Cross Validation

aux <- cbind(OTBCRU, key = paste0(OTBCRU$trpCode, OTBCRU$staNum))
aux <- aux[-which(duplicated(aux$key)), 1:7]
nrow(aux)

tides <- aux[-which(duplicated(aux$trpCode)),1]
length(tides)

library("glmnet")

#Input : Landed pour une OP donnée pour tous les poissons
#Output : Discarded pour cette même OP

#Si on ne regarde que les poissons pêchés (pas marée, area etc)

X <- mat.oc.lanquant

lanquant <- OTBCRU %>% dplyr::select(catReg,trpCode,staNum,spp,LAN,rect,month,year,lonIni,latIni) %>% spread(key=spp, value=LAN) 
to_pred <- lanquant %>% dplyr::filter(catReg=="Lan")

for (i in 9:dim(to_pred)[2]){
  to_pred[is.na(to_pred[,i]),i] <- 0
}
to_pred <- as.matrix(to_pred[,9:dim(to_pred)[2]])

Ypred <- data.frame(Init = vector(length = nrow(to_pred)))

#for (j in (1:ncol(mat.oc.disquant))) {
for (j in (20:20)) {
  Y <- mat.oc.disquant[,j]
  cv <- cv.glmnet(X, Y)

  print(j)
  Ynouveau <- predict.cv.glmnet(cv, to_pred)
  Ypred <- cbind(Ypred, Ynouveau)
}

Ypred <- Ypred[,-1]
head(Ypred)






