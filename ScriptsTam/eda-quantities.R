#General quantity exploration with species clustering
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)

source("miseenformedonnees.R")
lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

#Clustering with logsums of landed and discarded fish
logdisfish <- log10(colSums(disfish)+10)
loglanfish <- log10(colSums(lanfish)+10)
loglandis <- data.frame(logdisfish, loglanfish)

nzc_lanfish <- log10(colSums(lanfish!=0)+10)
nzc_disfish <- log10(colSums(disfish!=0)+10)
nzc_landis <- data.frame(nzc_disfish, nzc_lanfish)

#CV for number of logclusters
max_clusters = 20
ratio_ss <- data.frame(cluster = seq(from = 1, to = max_clusters, by = 1)) 
for (k in 1:max_clusters) {
  km_model <- kmeans(loglandis, k, nstart=20)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
}
ggplot(ratio_ss, aes(cluster, ratio)) + 
  geom_line() +
  geom_point()

#Build logclusters
nb_logclusters = 8
km_model <- kmeans(loglandis, centers = nb_logclusters, nstart=20)
loglandis$cluster <- km_model$cluster
nzc_landis$cluster <- km_model$cluster
p_sum <- ggplot(loglandis, aes(logdisfish, loglanfish, col = factor(cluster))) + geom_point(size = 2) +
  xlab("Total discarded fish (log10)") + ylab("Total landed fish (log10)")
p_freq <- ggplot(nzc_landis, aes(nzc_disfish, nzc_lanfish, col = factor(cluster))) + geom_point(size = 2) +
  xlab("Number of times discarded (log10)") + ylab("Number of times landed (log10)")
plot_grid(p_sum, p_freq, labels=c("A", "B"), ncol = 2, nrow = 1)
