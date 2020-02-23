library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(glmnet)
library(glmnetUtils)
library(data.table)
library(glmnet)
library(gplots)
library(RColorBrewer)

format_data <- function(month=FALSE, rect=FALSE, log=FALSE, binary=FALSE, nz_y=FALSE) {
  lanfish = lanquant.co[c(8:157)]
  disfish = disquant.co[c(8:157)]
  if (binary) {
    if (log) {
      stop("Check arguments: Cannot binary AND log")
    }
    disfish[disfish > 0] = 1
  }
  colnames(lanfish) = paste0("X",c(1:150))
  colnames(disfish) = paste0("Y",c(1:150))
  lan_nz = which(colSums(lanfish)!=0)
  dis_nz = which(colSums(disfish)!=0)
  lanfish = subset(lanfish, select=c(lan_nz))
  disfish = subset(disfish, select=c(dis_nz))
  if (log) {
    disfish = log10(disfish + 10)
  }
  if (month) {
    lanfish$sinmonth = sin(as.numeric(lanquant.co$month)*pi/6)
    lanfish$cosmonth = cos(as.numeric(lanquant.co$month)*pi/6)
  }
  if (rect) {
    rectLat <- setDT(lanquant.co)[,mean(latIni), by="rect"]
    rectLon <- setDT(lanquant.co)[,mean(lonIni), by="rect"]
    lanfish$rectLat <- rectLat$V1[match(lanquant.co$rect, rectLat$rect)]
    lanfish$rectLon <- rectLon$V1[match(lanquant.co$rect, rectLon$rect)]
  }
  
  xnames = colnames(lanfish)
  ynames = colnames(disfish)
  data = data.frame(lanfish,disfish)
  return(list("data"=data, "xnames"=xnames, "ynames"=ynames))
}

create_train_test <- function(data, train_ratio) {
  train_ind = sample(seq_len(nrow(data)), size = floor(train_ratio * nrow(data)))
  train = data[train_ind, ]
  test = data[-train_ind, ]
  return(list("train"=train, "test"=test, "train_ind"=train_ind))
}
