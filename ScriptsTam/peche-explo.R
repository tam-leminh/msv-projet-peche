rm(list=objects())
library(dplyr)
library(tidyverse)
library(ggplot2)

# load all data
source("miseenformedonnees.R")

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

#MONTHS AND YEAR
par(mfrow=c(1,2))
months = table(lanquant.co$month)
barplot(months, xlab="Mois", ylab="Fréquence")
years = table(lanquant.co$year)
barplot(years, xlab="Année", ylab="Fréquence")

months
years

p1<-ggplot(data=lanquant.co, aes(x=month)) +
  geom_bar(stat="count", fill="darkorchid4")

p1

p2<-ggplot(data=lanquant.co, aes(x=year)) +
  geom_bar(stat="count", fill="darkolivegreen")

p2

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)

#MAP
lan_coord <- lanquant.co[c("lonIni", "latIni")]

world_map <- map_data("world")
p <- ggplot() + coord_fixed(xlim=c(-5.5,-1), ylim=c(45,48)) +
  xlab("") + ylab("")
base_france <- p + geom_polygon(data=subset(world_map, world_map$region=="France"), aes(x=long, y=lat, group=group), 
                                colour="black", fill="coral2")

base_france

map_data <- 
  base_france +
  geom_point(data=lanquant.co, 
             aes(x=lonIni, y=latIni), colour="darkcyan", size=2, alpha=I(0.9)) +
  xlab("lon") + ylab("lat")

map_data

#QUANTITIES
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
p1 <- ggplot(loglandis, aes(logdisfish, loglanfish, col = factor(cluster))) + geom_point(size = 2) +
  xlab("Total discarded fish (log10)") + ylab("Total landed fish (log10)")
p2 <- ggplot(nzc_landis, aes(nzc_disfish, nzc_lanfish, col = factor(cluster))) + geom_point(size = 2) +
  xlab("Number of times discarded (log10)") + ylab("Number of times landed (log10)")
plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)