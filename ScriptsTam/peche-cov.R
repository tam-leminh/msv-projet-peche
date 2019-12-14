library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glasso)
library(igraph)
library(qgraph)
library(blockmodels)
library(Matrix)
library(sfsmisc)
library(flare)
library(huge)

#Keep only fish data

lan <- lanquant.co[c(8:157)]
dis <- disquant.co[c(8:157)]
colnames(lan) <- paste0("X",c(1:150))
colnames(dis) <- paste0("Y",c(1:150))
fishnodes <- data.frame(lan, dis)
nonlan = which(colSums(fishnodes)!=0)
nonlan
fishnodes <- subset(fishnodes, select=c(nonlan))
S <- cov(fishnodes, fishnodes)
names <- rownames(S)
s <- posdefify(S)
colnames(s) <- names
rownames(s) <- names
a <- glasso(s, rho=0.1, nobs=362, trace=TRUE)
qgraph(a, labels=names, layout="spring")


b <- EBICglasso(s, n=362, 0, threshold=TRUE)
summary(b)
qgraph(b, layout="spring")
b_non <- which(colSums(b)!=0)
b_names <- names[b_non]
b_simple <- b[b_non, b_non]
qgraph(b_simple, labels=b_names, layout="spring")


e <- EBICglasso(s, n=362, 0.5, threshold=TRUE)
summary(e)
qgraph(e, layout="spring")

s.npn <- huge.npn(s)
h <- huge(s.npn, method="glasso")
plot(h)
h_out_n <- huge.select(h, criterion="ric")
h_out <- huge.select(h, criterion="stars")
summary(h_out_n)
summary(h_out)
plot(h_out_n)
plot(h_out)

hist(s.npn[,1])
