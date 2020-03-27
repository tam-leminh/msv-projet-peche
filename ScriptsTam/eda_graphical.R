#Correlation matrices and graphical models
library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glasso)
library(igraph)
library(qgraph)
library(Matrix)
library(sfsmisc)
library(huge)

source("miseenformedonnees.R")

lan <- lanquant.co[c(8:157)]
dis <- disquant.co[c(8:157)]
colnames(lan) <- paste0("X",c(1:150))
colnames(dis) <- paste0("Y",c(1:150))

nonlan = which(colSums(lan)!=0)
nondis = which(colSums(dis)!=0)
lannz <- subset(lan, select=c(nonlan))
disnz <- subset(dis, select=c(nondis))

#Correlation matrix
corrmat <- cor(lannz, disnz)
col<- colorRampPalette(c("blue", "white", "red"))(256)
heatmap(x = corrmat, col = col, Colv = NA, Rowv=NA, cexRow = 0.5, cexCol = 0.5)

fishnodes <- data.frame(lannz, disnz)
covXY <- cov(fishnodes, fishnodes)
names <- rownames(covXY)
symCov <- posdefify(covXY)
colnames(symCov) <- names
rownames(symCov) <- names

#Graphical lasso rho=0.1
a <- glasso(symCov, rho=0.1, nobs=362, trace=TRUE)
qgraph(a, labels=names, layout="spring")

#Graphical lasso with BIC
b <- EBICglasso(symCov, n=362, 0, threshold=TRUE)
summary(b)
qgraph(b, layout="spring")

#Non-isolated nodes only
b_non <- which(colSums(b)!=0)
b_names <- names[b_non]
b_simple <- b[b_non, b_non]
qgraph(b_simple, labels=b_names, layout="spring")

#Graphical lasso with EBIC 0.5
e <- EBICglasso(symCov, n=362, 0.5, threshold=TRUE)
summary(e)
qgraph(e, layout="spring")

#Non-parametric transformation to gaussian
symCov.npn <- huge.npn(symCov)
h <- huge(symCov.npn, method="glasso")
plot(h)

h_out <- huge.select(h, criterion="stars")
summary(h_out)
plot(h_out)

h_out_n <- huge.select(h, criterion="ric")
summary(h_out_n)
plot(h_out_n)

hist(symCov.npn[,1])
