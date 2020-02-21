library(dplyr)
library(tidyverse)
library(ggplot2)

# load all data
load("Agro_OTBCRU20132017.rdata")

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]



summary(disquant.co)


all_lan = colSums(lanfish)
all_dis = colSums(disfish)
typeof(all_lan)

all_lan_and_dis <- data.frame(all_lan, all_dis)

all_lan_and_dis$all_lan

filtered <- all_lan_and_dis[ all_lan_and_dis$all_dis > 50000 & all_lan_and_dis$all_lan > 50000 ,]
as.matrix(all_lan_and_dis)

barplot(t(as.matrix(filtered)), beside=TRUE)

colSums(lanfish!=0)
nonlan = which(colSums(disfish)!=0)

