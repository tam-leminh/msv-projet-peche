

#### Fonctions projet Stratégies de pêche ####

#### Fonctions variées ####

create_train_test <- function(data, train_ratio) {
  #Fonction de Tâm
  train_ind = sample(seq_len(nrow(data)), size = floor(train_ratio * nrow(data)))
  train = data[train_ind, ]
  test = data[-train_ind, ]
  return(list("train"=train, "test"=test, "train_ind"=train_ind))
}

zeros <- function(est, vrai = vraies_val) {
  zer <- matrix(rep(-1, len=ncol(est)*nrow(est)), nrow = nrow(est))
  zero <- function(a, b) {
    if (a == 0) {
      if (b == 0) {return("VN")}
      else {return("FN")}
    }
    else {
      if (b == 0) {return("FP")}
      else {return("VP")}
    }
  }
  for (i in 1:ncol(est)) {
    zer[,i] <- mapply(zero, est[,i], as.data.frame(vrai[,i])[,1])
  }
  return(zer)
}

metrique <- function(a, b) {
  x <- log10((a+1)/(b+1))
  return(abs(x))
}

calc_err_CV <- function(DIS, vrv = vraies_val,
                        title = "Erreur (log) selon abondance (log)", rec = recap) {
  #calcul et représentation de l'erreur
  erreur <- metrique(DIS[,spp[1]],vrv[,spp[1]])
  for (i in spp[-1]){
    x <- metrique(DIS[,i],vrv[,i])
    erreur <- cbind(erreur, x)
  }
  erreur <- t(erreur)
  erreur_train <- erreur[,op_train]
  erreur_test <- erreur[,-op_train]
  d <- rowMeans(erreur_train)
  d <- as.data.frame(cbind(d, recap[names(d),"DIS.presences"]))
  colnames(d) <- c("errlog", "abondance")
  d <- cbind(d, log10(1+d$abondance))
  colnames(d)[3] <- "logab"
  pl <- ggplot(data = d)+
    geom_point(aes(x = d$logab, y = d$errlog))+
    ggtitle(paste0(title, " - Entraînement"))+
    ylab("Erreur (log10)") +
    xlab("Abondance (rejet)")
  print(pl)
  print("Entrainement")
  print(summary(rowMeans(erreur_train)))
  print("_____________")
  
  d <- rowMeans(erreur_test)
  d <- as.data.frame(cbind(d, recap[names(d),"DIS.presences"]))
  colnames(d) <- c("errlog", "abondance")
  d <- cbind(d, log10(1+d$abondance))
  colnames(d)[3] <- "logab"
  pl <- ggplot(data = d)+
    geom_point(aes(x = d$logab, y = d$errlog))+
    ggtitle(paste0(title, " - Test"))+
    ylab("Erreur (log10)") +
    xlab("Abondance (rejet)")
  print(pl)
  print("Test")
  print(summary(rowMeans(erreur_test)))
  print("_____________")
}

#### Régression logistique ####

pred_pres <- function(seuil = 0.5){
  pred <- matrix(rep(FALSE, len=ld*nrow(lan.aux)), nrow = ld)
  
  for (i in (1:length(spp_ok))){
    print("__________________________________________________________________")
    print(paste(i, spp_ok[i]))
    frm <- as.formula(paste0("as.factor(d",i, ") ~ 1"))
    aux <<- cbind(dis.aux[,i], lan_train)
    print(paste0("rejets entrainement : ",sum(aux[,1]>0)))
    colnames(aux)[1] <<- paste0("d",i)
    
    lmspp <- glm(frm, data = aux, family = binomial)
    frm2 <- as.formula(paste("~",paste(colnames(aux)[-1], collapse = "+"),sep = ""))
    lmspp <- step(lmspp, scope = frm2, trace = FALSE)
    aux <<- cbind(dis_tout[,i], lan.aux)
    colnames(aux)[1] <<- paste0("d",i)
    proba <- lmspp %>% predict(aux, type = "response")
    pred[i,] <- ifelse(proba > seuil, TRUE, FALSE)
    print(summary(pred[i,]))
  }
  rownames(pred) <- spp_ok
  
  preddef <- matrix(rep(FALSE, len=length(defaillant)*nrow(lan.aux)), nrow = length(defaillant))
  rownames(preddef) <- defaillant
  
  pred <- rbind(pred, preddef)
  pred <- pred[order(rownames(pred)), ]
  pred <- t(pred)
  return(pred)
}

#### Fonctions de régression ####
# Les fonctions sont toutes similaires (même base)

reg_lin_simple <- function(moyenne = FALSE){
  
  # Matrice des coefficients de régression :
  lm.lan <- matrix(rep(0, len=ld*(4+length(spplan))), nrow = ld) #+4 var environnement
  colnames(lm.lan) <- c(spplan, "cosm", "sinm", "rectLat", "rectLon")
  intercpt <- rep(0, len = ld)
  
  refnames <- data.frame(code = colnames(lan.aux),
                         spp = c(spplan, "cosm", "sinm", "rectLat", "rectLon"))
  
  for (i in (1:length(spp_ok))){
    print("__________________________________________________________________")
    print(paste(i, spp_ok[i]))
    frm <- as.formula(paste0("d",i, " ~ 1"))
    a <- (dis.aux[,i])
    aux <- cbind(a, lan_train)
    colnames(aux)[1] <- paste0("d",i)
    print(paste0("rejets entrainement : ",sum(aux[,1]>0)))
    lmspp <- lm(frm, data = aux)
    if (!moyenne) {
      frm2 <- as.formula(paste("d",i, "~",paste(colnames(lan_train), collapse = "+"),sep = ""))
      lmspp <- step(lmspp, scope = frm2, trace = FALSE)
    }
    coef <- lmspp$coefficients
    coef[is.na(coef)] <- 0
    
    #Compléter la matrice de régression :
    names_fact <- names(coef)
    char.aux <- substr(names_fact, 1, 1)
    if (char.aux[1] == "(") { #(Intercept)
      intercpt[i] <- coef[1]
      coef <- coef[-1]
      names_fact <- names(coef)
      char.aux <- substr(names_fact, 1, 1)
    }
    indC <- which(char.aux == "c")
    if (length(indC) > 0) {
      ref <- refnames[as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC]))),"spp"]
      s <- as.character(ref)
      lm.lan[i, s] <- coef
    }
  }
  #Calcul de la prédiction pour toutes les OP
  DIS <- lm.lan%*%t(lan.aux)+intercpt
  DIS[DIS<0] <- 0
  lmla <<- lm.lan
  rownames(DIS) <- spp_ok
  
  # Compléter avec les espèces trop rares
  DISdef <- matrix(rep(0, len=length(defaillant)*nrow(lan.aux)), nrow = length(defaillant))
  rownames(DISdef) <- defaillant
  
  DIS <- rbind(DIS, DISdef)
  DIS <- DIS[order(rownames(DIS)), ]
  DIS <- t(DIS)
  return(DIS)
}

reg_lin_jointe <- function(){
  
  lm.lan <- matrix(rep(0, len=ld*(4+length(spplan))), nrow = ld) #+4 var environnement
  colnames(lm.lan) <- c(spplan, "cosm", "sinm", "rectLat", "rectLon")
  
  lm.dis <- matrix(rep(0, len=ld^2), nrow = ld)
  colnames(lm.dis) <- spp_ok
  
  intercpt <- rep(0, len = ld)
  
  aux <<- cbind(dis.aux, lan_train)
  colnames(aux) <<- paste0("c", 1:ncol(aux))
  
  refnames <- data.frame(code = colnames(aux),
                         spp = c(spp_ok, spplan, "cosm", "sinm", "rectLat", "rectLon"))
  
  for (i in (1:length(spp_ok))){
    print("__________________________________________________________________")
    print(paste(i, spp_ok[i]))
    print(paste0("rejets entrainement : ",sum(aux[,i]>0)))
    frm <- paste0(colnames(aux)[i], "~ 1")
    lmspp <- lm(frm, data = aux)
    frm2 <- as.formula(paste("~",paste(colnames(aux)[-i], collapse = "+"),sep = ""))
    lmspp <- step(lmspp, scope = frm2, trace = FALSE)
    coef <- lmspp$coefficients
    coef[is.na(coef)] <- 0
    names_fact <- names(coef)
    char.aux <- substr(names_fact, 1, 1)
    #Compléter la matrice des coef de régression
    if (char.aux[1] == "(") { #(Intercept)
      intercpt[i] <- coef[1]
      coef <- coef[-1]
      names_fact <- names(coef)
      char.aux <- substr(names_fact, 1, 1)
    }
    indC <- which(char.aux == "c")
    if (length(indC) > 0) {
      ref <- data.frame(code = names_fact[indC],
                        dis = (as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC])))<=ld),
                        spp = refnames[as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC]))),"spp"])
      d <- nrow(ref[ref$dis,])
      if (d > 0) {
        s <- as.character(ref[ref$dis,]$spp)
        lm.dis[i, s] <- coef[1:d]
      }
      if (nrow(ref[!ref$dis,]) > 0) {
        s <- as.character(ref[!ref$dis,]$spp)
        lm.lan[i, s] <- coef[(d+1):nrow(ref)]
      }
    }
  }
  lm.dis <- (-1)*lm.dis
  for (i in 1:nrow(lm.dis)){
    lm.dis[i,i] <- 1
  }
  B <- lm.lan%*%t(lan.aux)+intercpt
  Y <- solve(lm.dis, B)
  Y[Y<0] <- 0
  Y <- floor(Y)
  lmdis <<- lm.dis
  lmla <<- lm.lan
  rownames(Y) <- spp_ok
  Ydef <- matrix(rep(0, len=length(defaillant)*nrow(lan.aux)), nrow = length(defaillant))
  rownames(Ydef) <- defaillant
  
  Y <- rbind(Y, Ydef)
  Y <- Y[order(rownames(Y)), ]
  Y <- t(Y)
  return(Y)
}

reg_2step_simple <- function(pred, moyenne = FALSE) {
  lm.lan <- matrix(rep(0, len=ld*(4+length(spplan))), nrow = ld) #+4 var environnement
  colnames(lm.lan) <- c(spplan, "cosm", "sinm", "rectLat", "rectLon")
  
  intercpt <- rep(0, len = ld)
  
  aux <<- cbind(lan_train)
  colnames(aux) <<- paste0("c", 1:ncol(aux))
  
  refnames <- data.frame(code = colnames(aux),
                         spp = c(spplan, "cosm", "sinm", "rectLat", "rectLon"))
  predexact <<- matrix(rep(FALSE, len=ld*length(op_train)), nrow = length(op_train))
  for (i in 1:nrow(dis.aux)){
    for (j in 1:ncol(dis.aux)){
      predexact[i,j] <<- (dis.aux[i,j] > 0)
    }
  }
  for (i in (1:length(spp_ok))){
    print("__________________________________________________________________")
    print(paste(i, spp_ok[i]))
    print(paste0("rejets entrainement : ",length(data.matrix(aux[predexact[,i],i]))))
    frm <- as.formula(paste0("d",i, " ~ 1"))
    aux <<- cbind(dis.aux[,i], lan_train)
    colnames(aux)[1] <<- paste0("d", i)
    aux <<- aux[predexact[,i],]
    lmspp <- lm(frm, data = aux)
    if (!moyenne) {
      frm2 <- as.formula(paste("~",paste(colnames(lan.aux), collapse = "+"),sep = ""))
      lmspp <- step(lmspp, scope = frm2, trace = FALSE)
    }
    coef <- lmspp$coefficients
    coef[is.na(coef)] <- 0
    names_fact <- names(coef)
    char.aux <- substr(names_fact, 1, 1)
    if (char.aux[1] == "(") { #(Intercept)
      intercpt[i] <- coef[1]
      coef <- coef[-1]
      names_fact <- names(coef)
      char.aux <- substr(names_fact, 1, 1)
    }
    indC <- which(char.aux == "c")
    if (length(indC) > 0) {
      ref <- refnames[as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC]))),"spp"]
      s <- as.character(ref)
      lm.lan[i, s] <- coef
    }
  }
  Y <- lm.lan%*%t(lan.aux)+intercpt
  Y[Y<0] <- 0
  lmla <<- lm.lan
  rownames(Y) <- spp_ok
  Y <- t(Y)
  for (i in 1:nrow(Y)) {
    for (j in 1:ncol(Y)) {
      if (!pred[i,j]) {
        Y[i,j] <- 0
      }
    }
  }
  Ydef <- matrix(rep(0, len=length(defaillant)*nrow(lan.aux)), nrow = nrow(lan.aux))
  colnames(Ydef) <- defaillant
  Y <- cbind(Y, Ydef)
  Y <- Y[,order(colnames(Y))]
  
  return(Y)
}

reg_2step_jointe <- function(pred) {
  lm.lan <- matrix(rep(0, len=ld*(4+length(spplan))), nrow = ld) #+4 var environnement
  colnames(lm.lan) <- c(spplan, "cosm", "sinm", "rectLat", "rectLon")
  
  lm.dis <- matrix(rep(0, len=ld^2), nrow = ld)
  colnames(lm.dis) <- spp_ok
  
  intercpt <- rep(0, len = ld)
  
  aux <<- cbind(dis.aux, lan_train)
  colnames(aux) <<- paste0("c", 1:ncol(aux))
  
  refnames <- data.frame(code = colnames(aux),
                         spp = c(spp_ok, spplan, "cosm", "sinm", "rectLat", "rectLon"))
  
  predexact <<- matrix(rep(FALSE, len=ld*length(op_train)), nrow = length(op_train))
  for (i in 1:nrow(dis.aux)){
    for (j in 1:ncol(dis.aux)){
      predexact[i,j] <<- (dis.aux[i,j] > 0)
    }
  }
  for (i in (1:length(spp_ok))){
    print("__________________________________________________________________")
    print(paste(i, spp_ok[i]))
    print(paste0("rejets entrainement : ",length(data.matrix(aux[predexact[,i],i]))))
    frm <- paste0(colnames(aux)[i], "~ 1")
    lmspp <- lm(frm, data = aux[predexact[,i],])
    frm2 <- as.formula(paste("~",paste(colnames(aux)[-i], collapse = "+"),sep = ""))
    lmspp <- step(lmspp, scope = frm2, trace = FALSE)
    coef <- lmspp$coefficients
    coef[is.na(coef)] <- 0
    names_fact <- names(coef)
    char.aux <- substr(names_fact, 1, 1)
    if (char.aux[1] == "(") { #(Intercept)
      intercpt[i] <- coef[1]
      coef <- coef[-1]
      names_fact <- names(coef)
      char.aux <- substr(names_fact, 1, 1)
    }
    indC <- which(char.aux == "c")
    if (length(indC) > 0) {
      ref <- data.frame(code = names_fact[indC],
                        dis = (as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC])))<=ld),
                        spp = refnames[as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC]))),"spp"])
      d <- nrow(ref[ref$dis,])
      if (d > 0) {
        s <- as.character(ref[ref$dis,]$spp)
        lm.dis[i, s] <- coef[1:d]
      }
      if (nrow(ref[!ref$dis,]) > 0) {
        s <- as.character(ref[!ref$dis,]$spp)
        lm.lan[i, s] <- coef[(d+1):nrow(ref)]
      }
    }
  }
  lm.dis <- (-1)*lm.dis
  for (i in 1:nrow(lm.dis)){
    lm.dis[i,i] <- 1
  }
  
  lmla <<- lm.lan
  lmdis <<- lm.dis
  
  B <- lm.lan%*%t(lan.aux)+intercpt
  Y <- solve(lm.dis, B)
  Y[Y<0] <- 0
  Y <- floor(Y)
  Y <- t(Y)

  for (i in 1:nrow(Y)) {
    for (j in 1:ncol(Y)) {
      if (!pred[i,j]) {
        Y[i,j] <- 0
      }
    }
  }
  Ydef <- matrix(rep(0, len=length(defaillant)*nrow(lan.aux)), nrow = nrow(lan.aux))
  colnames(Ydef) <- defaillant
  Y <- cbind(Y, Ydef)
  Y <- Y[,order(colnames(Y))]
  return(Y)
}

a<-NULL
reg_simple_clusters <- function(moyenne = FALSE){
  clust_deja_faits <- c()
  lm.lan <- matrix(rep(0, len=ld*(4+length(spplan))), nrow = ld) #+4 var environnement
  colnames(lm.lan) <- c(spplan, "cosm", "sinm", "rectLat", "rectLon")
  intercpt <- rep(0, len = ld)
  
  refnames <- data.frame(code = colnames(lan.aux),
                         spp = c(spplan, "cosm", "sinm", "rectLat", "rectLon"))
  
  for (i in (1:length(spp_ok))){
    print("__________________________________________________________________")
    print(paste(i, spp_ok[i]))
    frm <- as.formula(paste0("d",i, " ~ 1"))
    if (recap[spp_ok[i],"clust"] == -1) {
      a <<- (dis.aux[,i])
      aux <- cbind(a, lan_train)
      colnames(aux)[1] <- paste0("d",i)
      lmspp <- lm(frm, data = aux)
      if (!moyenne) {
        frm2 <- as.formula(paste("~",paste(colnames(lan_train), collapse = "+"),sep = ""))
        lmspp <- step(lmspp, scope = frm2, trace = FALSE)
      }
      coef <- lmspp$coefficients
      coef[is.na(coef)] <- 0
      names_fact <- names(coef)
      char.aux <- substr(names_fact, 1, 1)
      if (char.aux[1] == "(") { #(Intercept)
        intercpt[i] <- coef[1]
        coef <- coef[-1]
        names_fact <- names(coef)
        char.aux <- substr(names_fact, 1, 1)
      }
      indC <- which(char.aux == "c")
      if (length(indC) > 0) {
        ref <- refnames[as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC]))),"spp"]
        s <- as.character(ref)
        lm.lan[i, s] <- coef
      }
    }
    else {
      cl <- recap[spp_ok[i],"clust"]
      print(paste0("cluster ", cl))
      if (!cl %in% clust_deja_faits) {
        clust_deja_faits <- c(clust_deja_faits, cl)
        a <<- (dis.aux[,i])
        aux <- cbind(a, lan_train)
        colnames(aux)[1] <- paste0("d",i)
        for (j in 1:length(spp_ok)) {
          if (spp_ok[j] != spp_ok[i] & recap[spp_ok[j], "clust"] == cl) {
            a2 <- cbind(dis.aux[,j], lan_train)
            colnames(a2)[1] <- paste0("d", i)
            aux <<- rbind(aux, a2)
          }
        }
        colnames(aux)[1] <- paste0("d",i)
        lmspp <- lm(frm, data = aux)
        if (!moyenne) {
          frm2 <- as.formula(paste("~",paste(colnames(lan_train), collapse = "+"),sep = ""))
          lmspp <- step(lmspp, scope = frm2, trace = FALSE)
        }
        coef <- lmspp$coefficients
        coef[is.na(coef)] <- 0
        names_fact <- names(coef)
        char.aux <- substr(names_fact, 1, 1)
        if (char.aux[1] == "(") { #(Intercept)
          for (j in 1:length(spp_ok)) {
            if (recap[spp_ok[j], "clust"] == cl) {
              intercpt[j] <- coef[1]
            }
          }
          coef <- coef[-1]
          names_fact <- names(coef)
          char.aux <- substr(names_fact, 1, 1)
        }
        indC <- which(char.aux == "c")
        if (length(indC) > 0) {
          ref <- refnames[as.integer(substr(names_fact[indC], 2, nchar(names_fact[indC]))),"spp"]
          s <- as.character(ref)
          for (j in 1:length(spp_ok)) {
            if (recap[spp_ok[j], "clust"] == cl) {
              lm.lan[j, s] <- coef
            }
          }
        }
      }
    }
  }
  DIS <- lm.lan%*%t(lan.aux)+intercpt
  DIS[DIS<0] <- 0
  lmla <<- lm.lan
  rownames(DIS) <- spp_ok
  
  DISdef <- matrix(rep(0, len=length(defaillant)*nrow(lan.aux)), nrow = length(defaillant))
  rownames(DISdef) <- defaillant
  
  DIS <- rbind(DIS, DISdef)
  DIS <- DIS[order(rownames(DIS)), ]
  
  DIS <- t(DIS)
  
  return(DIS)
}

