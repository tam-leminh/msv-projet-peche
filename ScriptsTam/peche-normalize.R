library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(cowplot)

#Keep only fish data
lanfish = lanquant.co[c(8:157)]
disfish = disquant.co[c(8:157)]
colnames(lanfish) = paste0("X",c(1:150))
colnames(disfish) = paste0("Y",c(1:150))
xnames = colnames(lanfish)
ynames = colnames(disfish)

disfish_10 = disfish[colSums(disfish > 0) > 90]
hist(log(disfish_10$Y89[disfish_10$Y89 > 0]), breaks=15)
lapply(disfish[1:4], FUN=hist)

list <-lapply(1:ncol(disfish_10),
              function(col) ggplot2::qplot(disfish_10[[col]][disfish_10[col] > 0],
                                           geom = "histogram", bins = 20) + ggplot2::xlab(colnames(disfish_10)[col]))
cowplot::plot_grid(plotlist = list) 

list <-lapply(1:ncol(disfish_10),
              function(col) ggplot2::qplot(log(disfish_10[[col]][disfish_10[col] > 0]),
                                           geom = "histogram", bins = 20) + ggplot2::xlab(colnames(disfish_10)[col]))

cowplot::plot_grid(plotlist = list) 

