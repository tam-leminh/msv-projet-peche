library(dplyr)
library(tidyverse)
library(ggplot2)

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

lanfish$month = lanquant.co$month
lanfish$rect = lanquant.co$rect

xnames = colnames(lanfish)
ynames = colnames(disfish)
data = data.frame(lanfish,disfish)
nobs = dim(data)[1]

#lanfish[] = lapply(lanfish, factor)
#disfish[] = lapply(disfish, factor)

set.seed(55)

strsumx = paste(xnames, collapse= "+")
rates = data.frame(matrix(ncol = 4, nrow = 0))
layer = data.frame(matrix(ncol = length(ynames), nrow = nobs))
colnames(rates) = c("tn", "fn", "fp", "tp")
colnames(layer) = ynames

for (yname in ynames) {
  fmla = as.formula(paste0(yname, "~", strsumx))
  model = glm(fmla, family=binomial(link='logit'), data=data)
  pred = predict(model, data,type = "response")
  pred = ifelse(pred > 0.5,1,0)
  rates[yname,] = as.vector(table(Truth=data[[yname]], Prediction=pred))
  layer[[yname]] = pred
}

rates$sens = rates$tp/(rates$tp + rates$fn)
rates$spec = rates$tn/(rates$tn + rates$fp)

colSums(rates)
