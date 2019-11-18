

#Régression linéaire avec toutes les variables combinées comme dans clustering
lm.lan <- NULL
lm.dis <- NULL
intercpt <- NULL
#spp <- OTBCRU_OPECH[-which(duplicated(OTBCRU_OPECH$spp)),]$spp
spp <- NULL
defaillant <- c()

approx <- function(log = FALSE){
  spp <<- rownames(recap[recap$DIS.presences > 0,])
  dis.aux <- disquant.co[,9:ncol(disquant.co)]
  lan.aux <- lanquant.co[,9:ncol(lanquant.co)]
  quant.co <<- cbind(dis.aux, lan.aux)
  quant.co <<- quant.co[,which(colnames(quant.co) %in% spp)]
  if (log) {
    quant.co <<- quant.co + 1
    quant.co <<- log10(quant.co)
  }
  colnames(quant.co) <<- paste0("c",as.character(1:ncol(quant.co)))
  
  lm.lan <<- matrix(rep(0, len=length(spp)^2), nrow = length(spp))
  lm.dis <<- matrix(rep(0, len=length(spp)^2), nrow = length(spp))
  intercpt <<- rep(0, len = length(spp))
  
  defaillant <<- c()
  debut <- TRUE
  while(debut | length(defaillant) > 0) {
    if (debut) {
      debut <- FALSE
      print("let's go !")
    }
    else {
      print(defaillant)
      intercpt <<- intercpt[-defaillant]
      lm.lan <<- lm.lan[-defaillant,]
      lm.dis <<- lm.dis[-defaillant, -defaillant]
      quant.co <<- quant.co[,-defaillant]
      spp <<- spp[-defaillant]
      defaillant <<- c()
    }
    for (i in (1:length(spp))){
      correct <- FALSE
      while (not(correct)){
        frm <- as.formula(paste(colnames(quant.co)[i], "~",
                                paste(colnames(quant.co)[-i], collapse = "+"),sep = ""))
        lmspp <- lm(frm, data = quant.co)
        #raffiner en ne gardant que les paramètres pertinents
        pval <- summary(lmspp)$coefficients[2:length(spp),4]
        var.bis <- names(pval[which(pval<0.05)])
      }
      
      if (length(var.bis) > 0){
        frm <- paste(var.bis, collapse = "+")
        frm <- as.formula(paste(colnames(quant.co)[i], "~",
                                frm,sep = ""))
        lmspp <- lm(frm, data = quant.co)
        #éventuellement : faire une boucle pour réitérer... Et gérer intercept
        intercpt[i] <<- lmspp$coefficients[1]
        ind <- names(pval[pval<0.05])
        ind <- as.integer(substr(ind, 2, nchar(ind)))
        ind.dis <- ind[ind <= length(spp)]
        ind.lan <- ind[ind > length(spp)] - length(spp)
        if (length(ind.dis) > 0) {
          lm.dis[i, ind.dis] <<- lmspp$coefficients[2:(length(ind.dis)+1)]
        }
        if (length(ind.lan) > 0) {
          lm.lan[i, ind.lan] <<- lmspp$coefficients[(length(ind.dis)+2):(length(lmspp$coefficients))]
        }
      }
      else {
        defaillant <<- c(defaillant, i)
      }
    }
  }
}

approx()

#In summary.lm(lmspp) :
#essentially perfect fit: summary may be unreliable

OP <- lanquant.co[,2:8] #seulement info de l'OP
#intercpt <- intercpt[-defaillant]
#lm.lan <- lm.lan[-defaillant,]
#lm.dis <- lm.dis[-defaillant, -defaillant]
lm.dis <- (-1)*lm.dis
for (i in 1:nrow(lm.dis)){
  lm.dis[i,i] <- 1
}
X <- as.matrix(lanquant.co[,9:ncol(lanquant.co)])
X <- X[,which(colnames(X) %in% rownames(recap[recap$DIS.presences > 0,]))]
#X <- X+1
#X <- log10(X)

PredictMassClust <- function() {
  B <- lm.lan%*%t(X)+intercpt
  Y <- solve(lm.dis, B)
  Y[Y<0] <- 0
  Y <- floor(Y)
  return(Y)
}
#à voir : système non inversible. Il faut certainement virer des quantités à prédire mais que dans dis
DIS <- t(PredictMassClust())

x <- disquant.co[1,9:ncol(disquant.co)]
cbind(DIS[1,],t(x[,which(colnames(x) %in% spp)]))


#pour l'étude en log

floor(10^(DIS))
y <- disquant.co[,9:ncol(disquant.co)]
y[,spp]
qr(lm.dis)$rank
dim(lm.dis)


#il faudra faire de la cross validation pour s'assurer que c'est OK
#une fois qu'on aura vérifié que le résultat est correct...

#Si vraiment pas ouf : filtrer les espèces pour ne garder que celles pour lesquelles ça marche

#peut être fait sur des grandeurs transformées tq log
#ou encore 