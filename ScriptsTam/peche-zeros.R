library(dplyr)
library(tidyverse)
library(ggplot2)

#Keep only fish data

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]
colnames(lanfish) <- paste0("X",c(1:150))
colnames(disfish) <- paste0("Y",c(1:150))
lanfish[lanfish > 0] = 1
disfish[disfish > 0] = 1
lanzeros = which(colSums(lanfish)!=0)
diszeros = which(colSums(disfish)!=0)
lanfish <- subset(lanfish, select=c(lanzeros))
disfish <- subset(disfish, select=c(diszeros))

lanfish$month = lanquant.co$month
lanfish$rect = lanquant.co$rect

xnames = colnames(lanfish)
ynames = colnames(disfish)
data = data.frame(lanfish,disfish)

#lanfish[] <- lapply(lanfish, factor)
#disfish[] <- lapply(disfish, factor)


set.seed(55)

train_ind <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
train <- data[train_ind, ]
test <- data[-train_ind, ]

strsumx = paste(xnames, collapse= "+")
fmla = as.formula(paste("Y150 ~", strsumx))
model = lm(fmla, data=train)
lm_1_train_pred = rmse(model, train)
lm_1_test_pred = rmse(model, test)
predictions = predict(model, test)
predictions[predictions>=0.5] = 1
predictions[predictions<0.5] = 0
predictions
which(predictions==1)
which(test['Y150']==1)


train_ind <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
train <- data[train_ind, ]
test <- data[-train_ind, ]

fmla = as.formula(paste0("Y150", "~", strsumx))
model = glm(fmla, family=binomial(link='logit'), data=data)
summary(model)
predictions = predict(model, data,type = "response")
predictions <- ifelse(predictions > 0.4,1,0)
which(predictions>0.5)
which(data['Y150']>0)
table(Truth=data[['Y150']], Prediction=predictions)

strsumx = paste(xnames, collapse= "+")
rates = data.frame(matrix(ncol = 4, nrow = 0))
colnames(rates) = c("tn", "fn", "fp", "tp")

for (yname in ynames) {
  fmla = as.formula(paste0(yname, "~", strsumx))
  model = glm(fmla, family=binomial(link='logit'), data=data)
  predictions = predict(model, data,type = "response")
  predictions <- ifelse(predictions > 0.5,1,0)
  rates[yname,] = as.vector(table(Truth=data[[yname]], Prediction=predictions))
}

rates$sens = rates$tp/(rates$tp + rates$fn)
rates$spec = rates$tn/(rates$tn + rates$fp)

colSums(rates)
