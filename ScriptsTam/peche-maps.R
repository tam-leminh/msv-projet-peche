library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(tmap)
library(ggmap)
library(mclust)

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

lan_coord <- lanquant.co[c("lonIni", "latIni")]

world_map <- map_data("world")
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")
base_france <- p + geom_polygon(data=subset(world_map, world_map$region=="France"), aes(x=long, y=lat, group=group), 
                                     colour="black", fill="darkslategray3")

base_france

map_data <- 
  base_france +
  geom_point(data=lanquant.co, 
             aes(x=lonIni, y=latIni), colour="firebrick", 
             fill="Pink",pch=21, size=5, alpha=I(0.7))

map_data

moi = '10'

lanquant_moi <- lanquant.co[ lanquant.co$month == moi ,]
lanquant_moi

map_data <- 
  base_france +
  geom_point(data=lanquant_moi, 
             aes(x=lonIni, y=latIni), colour="Deep Pink", 
             fill="Pink",pch=21, size=5, alpha=I(0.7))

map_data

max_clusters = 20

ratio_ss <- data.frame(cluster = seq(from = 1, to = max_clusters, by = 1)) 

for (k in 1:max_clusters) {
  
  km_model <- kmeans(subset(lan_coord, select=c("lonIni", "latIni")), k, nstart=20)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
}

ggplot(ratio_ss, aes(cluster, ratio)) + 
  geom_line() +
  geom_point()

nb_clusters = 3

km_model <- kmeans(subset(lan_coord, select=c("lonIni", "latIni")), centers = nb_clusters)
lan_coord$cluster <- km_model$cluster

p <- ggplot(lan_coord, aes(x=lonIni, y=latIni, col=factor(cluster))) + geom_point()
p

gmm <- Mclust(subset(lan_coord, select=c("lonIni", "latIni")))
plot(gmm)
