#General space-time exploration for all species
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)

source("miseenformedonnees.R")

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

#MONTHS AND YEAR
par(mfrow=c(1,1))

p_month <- ggplot(data=lanquant.co, aes(x=month)) +
  geom_bar(stat="count", fill="darkorchid4")

p_year <- ggplot(data=lanquant.co, aes(x=year)) +
  geom_bar(stat="count", fill="darkolivegreen")

plot_grid(p_month, p_year, labels=c("A", "B"), ncol = 2, nrow = 1)

#MAP
base <- ggplot() + coord_fixed(xlim=c(-5.5,-1), ylim=c(45,48))
world_map <- map_data("world")
base_france <- base + geom_polygon(data=subset(world_map, world_map$region=="France"), aes(x=long, y=lat, group=group), 
                                colour="black", fill="coral2")
map_obs <- base_france +
  geom_point(data=lanquant.co, 
             aes(x=lonIni, y=latIni), colour="darkcyan", size=2, alpha=I(0.9)) +
  xlab("lon") + ylab("lat")

map_obs
