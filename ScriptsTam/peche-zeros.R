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
pred = data.frame(matrix(ncol = length(ynames), nrow = nobs))
rates = data.frame(matrix(ncol = 4, nrow = 0))
layer = data.frame(matrix(ncol = length(ynames), nrow = nobs))

thresholds = data.frame(matrix(ncol = 4, nrow = 0))

colnames(rates) = c("tn", "fn", "fp", "tp")
colnames(layer) = ynames

for (yname in ynames) {
  fmla = as.formula(paste0(yname, "~", strsumx))
  model = glm(fmla, family=binomial(link='logit'), data=data)
  pred[[yname]] = predict(model, data,type = "response")
}
for (threshold in seq(0.0, 1.0, by=0.1)) {
  for (yname in ynames) {
    decision = ifelse(pred[[yname]] > threshold,1,0)
    rates[yname,] = as.vector(table(Truth=data[[yname]], Prediction=decision))
  }
  sum_rates = colSums(rates)
  thresholds[threshold,] = c(tn = sum_rates['tn'], tn = sum_rates['fn'], tn = sum_rates['fp'], tn = sum_rates['tp'])
}
for (yname in ynames) {
  decision = ifelse(pred[[yname]] > 0.5,1,0)
  rates[yname,] = as.vector(table(Truth=data[[yname]], Prediction=decision))
  layer[[yname]] = decision
}

rates$sens = rates$tp/(rates$tp + rates$fn)
rates$spec = rates$tn/(rates$tn + rates$fp)

colSums(rates)['fn']

plot(rates$tp + rates$fn, rates$sens, xlab="Vrai nombre de positifs", ylab="Sensitivité")
plot(rates$fp + rates$tn, rates$spec, xlab="Vrai nombre de négatifs", ylab="Spécificité")
hist(rates$sens)
hist(rates$spec)
plot(rates$spec, rates$sens, xlab="Spécificité", ylab="Sensitivité")
plot(rates$tp + rates$fn, rates$tp/(rates$tp + rates$fp), xlab="Vrai nombre de positifs", ylab="Précision")
plot(rates$fp + rates$tn, rates$tn/(rates$tn + rates$fn), xlab="Vrai nombre de négatifs", ylab="Valeur prédictive négative")
