library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)

#Keep only fish data

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]
colnames(lanfish) <- paste0("X",c(1:150))
colnames(disfish) <- paste0("Y",c(1:150))
lanzeros = which(colSums(lanfish)!=0)
diszeros = which(colSums(disfish)!=0)
lanfish <- subset(lanfish, select=c(lanzeros))
disfish <- subset(disfish, select=c(diszeros))

lanfish$month = lanquant.co$month
lanfish$rect = lanquant.co$rect

xnames = colnames(lanfish)
ynames = colnames(disfish)
data = data.frame(lanfish,disfish)

#Build train and test sets

set.seed(55)

train_ind <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
train <- data[train_ind, ]
test <- data[-train_ind, ]

strsumx = paste(xnames, collapse= "+")

fmla = as.formula(paste0(yname, "~", strsumx))
mlist = seq(10, length(xnames), 10)
mres = numeric(length(mlist))
for (k in c(1:length(mlist))) {
  print(mlist[k])
  rf_mod <- randomForest(fmla, data=train, ntree=100, mtry=mlist[k])
  mres[k] = rmse(rf_mod, test)
}

scores = data.frame(matrix(ncol = 2, nrow = 0))
colnames(scores) = c("train", "test")
base_train_mse = 0
base_test_mse = 0
rf_train_mse = 0
rf_test_mse = 0
lm_train_mse = 0
lm_test_mse = 0
for (yname in ynames) {
  print(yname)
  
  baseline = mean(train[[yname]])
  base_train_mse = base_train_mse + mean((train[[yname]] - baseline)^2)
  base_test_mse = base_train_mse + mean((test[[yname]] - baseline)^2)
  
  fmla = as.formula(paste0(yname, "~", strsumx))
  
  rf_mod <- randomForest(fmla, data=train, ntree=100, mtry=40)
  rf_train_mse = rf_train_mse + mse(rf_mod, train)
  rf_test_mse = rf_test_mse + mse(rf_mod, test)
  
  lm_mod <- lm(fmla, data=train)
  lm_train_mse = lm_train_mse + mse(lm_mod, train)
  lm_test_mse = lm_train_mse + mse(lm_mod, test)
}
scores['base',] = as.vector(c(sqrt(base_train_mse), sqrt(base_test_mse)))
scores['rf',] = as.vector(c(sqrt(rf_train_mse), sqrt(rf_test_mse)))
scores['lm',] = as.vector(c(sqrt(lm_train_mse), sqrt(lm_test_mse)))


