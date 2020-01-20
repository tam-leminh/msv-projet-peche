library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(glmnet)
library(glmnetUtils)

#Keep only fish data

lanfish = lanquant.co[c(8:157)]
disfish = disquant.co[c(8:157)]
colnames(lanfish) = paste0("X",c(1:150))
colnames(disfish) = paste0("Y",c(1:150))
lanfish[lanfish > 0] = 1
disfish[disfish > 0] = 1
lanzeros = which(colSums(lanfish)!=0)
diszeros = which(colSums(disfish)!=0)
lanfish = subset(lanfish, select=c(lanzeros))
disfish = subset(disfish, select=c(diszeros))

#lanfish$month = lanquant.co$month
#lanfish$rect = lanquant.co$rect

xnames = colnames(lanfish)
ynames = colnames(disfish)
data = data.frame(lanfish,disfish)
nobs = dim(data)[1]

strsumx = paste(xnames, collapse= "+")
rates = data.frame(matrix(ncol = 4, nrow = 0))
layer = data.frame(matrix(ncol = length(ynames), nrow = nobs))
colnames(rates) = c("tn", "fn", "fp", "tp")
colnames(layer) = ynames

#Build train and test sets

set.seed(55)

train_ind = sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
train = data[train_ind, ]
test = data[-train_ind, ]
layer_train = layer[train_ind, ]
layer_test = layer[-train_ind, ]
n_train = dim(train)[1]
n_test = dim(test)[1]

rates_train = data.frame(matrix(ncol = 4, nrow = 0))
rates_test = data.frame(matrix(ncol = 4, nrow = 0))
colnames(rates_train) = c("tn", "fn", "fp", "tp")
colnames(rates_test) = c("tn", "fn", "fp", "tp")

dec_thr = 0.5
for (yname in ynames) {
  if (sum(train[[yname]])==0) {
    layer_train[[yname]][is.na(layer_train[[yname]])] = 0
    layer_test[[yname]][is.na(layer_test[[yname]])] = 0
  }
  else {
    fmla = as.formula(paste0(yname, "~", strsumx))
    #model = glm(fmla, family="binomial", data=train)
    cv.model = cv.glmnet(x=data.matrix(train[c(xnames)]), y=  data.matrix(train[[yname]]), family="binomial")
    #pred_train = predict(model, train, type = "response")
    pred_train = predict(model, newx = data.matrix(train[c(xnames)]), type="response", s = 0)
    pred_train = ifelse(pred_train > dec_thr,1,0)
    
    #pred_test = predict(model, test, type = "response")
    pred_test = predict(model, newx = data.matrix(test[c(xnames)]), type="response", s = 0)
    pred_test = ifelse(pred_test > dec_thr,1,0)
    
    layer_train[[yname]] = pred_train
    layer_test[[yname]] = pred_test
    
    rates_train[yname,] = as.vector(table(train[[yname]], pred_train))
    rates_test[yname,] = as.vector(table(test[[yname]], pred_test))
  }
}




lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]
colnames(lanfish) <- paste0("X",c(1:150))
colnames(disfish) <- paste0("Y",c(1:150))
lanzeros = which(colSums(lanfish)!=0)
diszeros = which(colSums(disfish)!=0)
lanfish <- subset(lanfish, select=c(lanzeros))
disfish <- subset(disfish, select=c(diszeros))

#lanfish$month = lanquant.co$month
#lanfish$rect = lanquant.co$rect

xnames = colnames(lanfish)
ynames = colnames(disfish)
data = data.frame(lanfish,disfish)
train = data[train_ind, ]
test = data[-train_ind, ]

scores = data.frame(matrix(ncol = 2, nrow = 0))
colnames(scores) = c("train", "test")
base_train_mse = 0
base_test_mse = 0
rf_train_mse = 0
rf_test_mse = 0
rftr_train_mse = 0
rftr_test_mse = 0
rfnz_train_mse = 0
rfnz_test_mse = 0
lm_train_mse = 0
lm_test_mse = 0
lmtr_train_mse = 0
lmtr_test_mse = 0
lmnz_train_mse = 0
lmnz_test_mse = 0

for (yname in ynames) {
  baseline = mean(train[[yname]])
  base_train_mse = base_train_mse + mean((train[[yname]] - baseline)^2)
  base_test_mse = base_test_mse + mean((test[[yname]] - baseline)^2)
  
  fmla = as.formula(paste0(yname, "~", strsumx))
  
  rf_mod <- randomForest(fmla, data=train, ntree=100, mtry=40)
  pred_train = predict(rf_mod, train)
  pred_test = predict(rf_mod, test)
  rf_train_mse = rf_train_mse + mean((train[[yname]] - pred_train)^2)
  rf_test_mse = rf_test_mse + mean((test[[yname]] - pred_test)^2)
  rftr_train_mse = rftr_train_mse + mean((train[[yname]] - pred_train*layer_train[[yname]])^2)
  rftr_test_mse = rftr_test_mse + mean((test[[yname]] - pred_test*layer_test[[yname]])^2)
  
  lm_mod <- lm(fmla, data=train)
  pred_train = predict(lm_mod, train)
  pred_test = predict(lm_mod, test)
  lm_train_mse = lm_train_mse + mean((train[[yname]] - pred_train)^2)
  lm_test_mse = lm_test_mse + mean((test[[yname]] - pred_test)^2)
  lmtr_train_mse = lmtr_train_mse + mean((train[[yname]] - pred_train*layer_train[[yname]])^2)
  lmtr_test_mse = lmtr_test_mse + mean((test[[yname]] - pred_test*layer_test[[yname]])^2)
  
  #S'il n'y a pas que des zéros dans le train (sinon on prédit zéro pour tout le vecteur -> prendre le coût du vecteur)
  if (sum(train[yname])!=0) {
    rfnz_mod <- randomForest(fmla, data=train[which(train[[yname]]!=0),], ntree=100, mtry=40)
    lmnz_mod <- lm(fmla, data=train[which(train[[yname]]!=0),])
    
    nb_1_train = sum(layer_train[[yname]])
    nb_1_test = sum(layer_test[[yname]])
    nb_0_train = sum(layer_train[[yname]]==0)
    nb_0_test = sum(layer_test[[yname]]==0)
    
    #Si le filtre ne détecte pas que des zéros dans le train/test, il faut faire des prédictions
    if (nb_1_train != 0) {
      pred_train = predict(rfnz_mod, train[which(layer_train[[yname]]!=0),])
      rfnz_train_mse = rfnz_train_mse + sum((train[which(layer_train[[yname]]!=0),][[yname]] - pred_train)^2)/n_train
      
      pred_train = predict(lmnz_mod, train[which(layer_train[[yname]]!=0),])
      lmnz_train_mse = lmnz_train_mse + sum((train[which(layer_train[[yname]]!=0),][[yname]] - pred_train)^2)/n_train
    }
    if (nb_1_test != 0) {
      pred_test = predict(rfnz_mod, test[which(layer_test[[yname]]!=0),])
      rfnz_test_mse = rfnz_test_mse + sum((test[which(layer_test[[yname]]!=0),][[yname]] - pred_test)^2)/n_test
    
      pred_test = predict(lmnz_mod, test[which(layer_test[[yname]]!=0),])
      lmnz_test_mse = lmnz_test_mse + sum((test[which(layer_test[[yname]]!=0),][[yname]] - pred_test)^2)/n_test
    }
    
    #Si le filtre détecte des zéros dans le train/test, ajouter le coût des faux zéros
    if (nb_0_train != 0) {
      rfnz_train_mse = rfnz_train_mse + sum((train[which(layer_train[[yname]]==0),][[yname]])^2)/n_train
      lmnz_train_mse = lmnz_train_mse + sum((train[which(layer_train[[yname]]==0),][[yname]])^2)/n_train
    }
    if (nb_0_train != 0) {
      rfnz_test_mse = rfnz_test_mse + sum((test[which(layer_test[[yname]]==0),][[yname]])^2)/n_test
      lmnz_test_mse = lmnz_test_mse + sum((test[which(layer_test[[yname]]==0),][[yname]])^2)/n_test
    } 
    
  } 
  else {
    rfnz_train_mse = rfnz_train_mse + mean((train[[yname]])^2)
    rfnz_test_mse = rfnz_test_mse + mean((test[[yname]])^2)
    lmnz_train_mse = lmnz_train_mse + mean((train[[yname]])^2)
    lmnz_test_mse = lmnz_test_mse + mean((test[[yname]])^2)
  }
  print(yname)
  print(rfnz_train_mse)
  print(rfnz_test_mse)
  print(lmnz_train_mse)
  print(lmnz_test_mse)
}
scores['base',] = as.vector(c(sqrt(base_train_mse), sqrt(base_test_mse)))
scores['rf',] = as.vector(c(sqrt(rf_train_mse), sqrt(rf_test_mse)))
scores['lm',] = as.vector(c(sqrt(lm_train_mse), sqrt(lm_test_mse)))
scores['rf_tr',] = as.vector(c(sqrt(rftr_train_mse), sqrt(rftr_test_mse)))
scores['lm_tr',] = as.vector(c(sqrt(lmtr_train_mse), sqrt(lmtr_test_mse)))
scores['rf_nz',] = as.vector(c(sqrt(rfnz_train_mse), sqrt(rfnz_test_mse)))
scores['lm_nz',] = as.vector(c(sqrt(lmnz_train_mse), sqrt(lmnz_test_mse)))