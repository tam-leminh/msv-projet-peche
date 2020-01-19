library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(cowplot)
library(e1071)

#Keep only fish data
lanfish = lanquant.co[c(8:157)]
disfish = disquant.co[c(8:157)]
colnames(lanfish) = paste0("X",c(1:150))
colnames(disfish) = paste0("Y",c(1:150))
xnames = colnames(lanfish)
ynames = colnames(disfish)


lanfish_10 = lanfish[colSums(lanfish > 0) > 125]
hist(lanfish_10$X69[lanfish_10$X69 > 0], breaks=15)
list <-lapply(1:ncol(lanfish_10),
              function(col) ggplot2::qplot(lanfish_10[[col]][lanfish_10[col] > 0],
                                           geom = "histogram", bins = 20) + ggplot2::xlab(colnames(lanfish_10)[col]))
cowplot::plot_grid(plotlist = list) 

list <-lapply(1:ncol(lanfish_10),
              function(col) ggplot2::qplot(log(lanfish_10[[col]][lanfish_10[col] > 0]),
                                           geom = "histogram", bins = 20) + ggplot2::xlab(colnames(lanfish_10)[col]))

cowplot::plot_grid(plotlist = list) 

skewv = vector()
skewv0 = vector()
for xname in xnames {
  if !is.nan(skewness(lanfish[[xname]])) {
    skewv = skewness(lanfish$X22)
    skewv0 = skewness(lanfish$X22[lanfish$X22>0])
  }
}
is.nan(skewness(lanfish$X20))

hist(apply(lanfish[which(colSums(lanfish)>0)], FUN=skewness, MARGIN=2), breaks = 10, xlab="Skewness", main="Skewness per landed species")
apply(disfish[which(colSums(disfish)>0)], FUN=skewness, MARGIN=2)
hist(rowSums(lanfish), xlab="Sum of landed fish", main="Distribution of the sum of landed fish")

disfish_10 = disfish[colSums(disfish > 0) > 160]
hist(log(disfish_10$Y89[disfish_10$Y89 > 0]), breaks=15)

list <-lapply(1:ncol(disfish_10),
              function(col) ggplot2::qplot(disfish_10[[col]][disfish_10[col] > 0],
                                           geom = "histogram", bins = 20) + ggplot2::xlab(colnames(disfish_10)[col]))
cowplot::plot_grid(plotlist = list) 

list <-lapply(1:ncol(disfish_10),
              function(col) ggplot2::qplot(log(disfish_10[[col]][disfish_10[col] > 0]),
                                           geom = "histogram", bins = 20) + ggplot2::xlab(colnames(disfish_10)[col]))

cowplot::plot_grid(plotlist = list) 

hist(apply(disfish[which(colSums(disfish)>0)], FUN=skewness, MARGIN=2), breaks = 10, xlab="Skewness", main="Skewness per discarded species")
hist(apply(log(disfish[which(colSums(disfish)>0)]), FUN=skewness, MARGIN=2), breaks = 10, xlab="Skewness", main="Skewness per discarded species")
     