library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(tmap)
library(ggmap)

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

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

