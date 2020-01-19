library(dplyr)
library(tidyverse)
library(ggplot2)

k = 89

lanfish = lanquant.co[c(8:157)]
disfish = disquant.co[c(k+7)]
colnames(lanfish) = paste0("X",c(1:150))
colnames(disfish) = paste0("Y",c(k))
lanfish[lanfish > 0] = 1
disfish[disfish > 0] = 1
lanzeros = which(colSums(lanfish)!=0)
lanfish = subset(lanfish, select=c(lanzeros))

xnames = colnames(lanfish)
yname = colnames(disfish)
data = data.frame(lanfish,disfish)
nobs = dim(data)[1]

strsumx = paste(xnames, collapse= "+")
layer = data.frame(matrix(ncol = 1, nrow = nobs))
colnames(layer) = yname

set.seed(56)

train_ind = sample(seq_len(nrow(data)), size = floor(0.8 * nrow(data)))
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

fmla = as.formula(paste0(yname, "~", strsumx))
model = glm(fmla, family=binomial(link='logit'), data=train)

pred_train = predict(model, train, type = "response")
pred_train = ifelse(pred_train > dec_thr,1,0)

pred_test = predict(model, test, type = "response")
pred_test = ifelse(pred_test > dec_thr,1,0)

layer_train = pred_train
layer_test = pred_test

rates_train[yname,] = as.vector(table(train[[yname]], pred_train))
rates_test[yname,] = as.vector(table(test[[yname]], pred_test))

lanfish = lanquant.co[c(8:157)]
#disfish = disquant.co[c(k+7)]
disfish = log(disquant.co[c(k+7)] + 0.01)
colnames(lanfish) = paste0("X",c(1:150))
colnames(disfish) = paste0("Y",c(k+7))
lanzeros = which(colSums(lanfish)!=0)
lanfish = subset(lanfish, select=c(lanzeros))

#lanfish$month = lanquant.co$month
#lanfish$rect = lanquant.co$rect

xnames = colnames(lanfish)
yname = colnames(disfish)
data = data.frame(lanfish,disfish)
train = data[train_ind, ]
test = data[-train_ind, ]


baseline = mean(train[[yname]])
base_train_mse = base_train_mse + mean((train[[yname]] - baseline)^2)
base_test_mse = base_test_mse + mean((test[[yname]] - baseline)^2)
lmnz_train_mse = 0
lmnz_test_mse = 0

fmla = as.formula(paste0(yname, "~", strsumx))

#S'il n'y a pas que des zéros dans le train (sinon on prédit zéro pour tout le vecteur -> prendre le coût du vecteur)
if (sum(train[yname])!=0) {
  #lmnz_mod <- lm(fmla, data=train[which(train[[yname]]!=0),])
  lmnz_mod <- lm(fmla, data=train[which(train[[yname]]>0),])
  
  nb_1_train = sum(layer_train)
  nb_1_test = sum(layer_test)
  nb_0_train = sum(layer_train==0)
  nb_0_test = sum(layer_test==0)
  
  #Si le filtre ne détecte pas que des zéros dans le train/test, il faut faire des prédictions
  if (nb_1_train != 0) {
    pred_train = predict(lmnz_mod, train[which(layer_train!=0),])
    lmnz_train_mse = lmnz_train_mse + sum((train[which(layer_train!=0),][[yname]] - pred_train)^2)/n_train
    plot(pred_train, train[which(layer_train!=0),][[yname]] - pred_train, xlab = "fitted", ylab = "residuals")
  }
  if (nb_1_test != 0) {
    pred_test = predict(lmnz_mod, test[which(layer_test!=0),])
    lmnz_test_mse = lmnz_test_mse + sum((test[which(layer_test!=0),][[yname]] - pred_test)^2)/n_test
  }
  
  #Si le filtre détecte des zéros dans le train/test, ajouter le coût des faux zéros
  if (nb_0_train != 0) {
    lmnz_train_mse = lmnz_train_mse + sum((train[which(layer_train==0),][[yname]])^2)/n_train
  }
  if (nb_0_train != 0) {
    lmnz_test_mse = lmnz_test_mse + sum((test[which(layer_test==0),][[yname]])^2)/n_test
  } 
  
} else {
  lmnz_train_mse = lmnz_train_mse + mean((train[[yname]])^2)
  lmnz_test_mse = lmnz_test_mse + mean((test[[yname]])^2)
}

