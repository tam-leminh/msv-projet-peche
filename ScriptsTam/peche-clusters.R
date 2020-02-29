library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)

#Keep only fish data

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

#CLUSTERING

#Clustering with logsums of landed and discarded fish

logdisfish <- log10(colSums(disfish)+10)
loglanfish <- log10(colSums(lanfish)+10)

logdisfish[is.infinite(logdisfish)] <- 0
loglanfish[is.infinite(loglanfish)] <- 0

loglandis <- data.frame(logdisfish, loglanfish)
loglandis
p1 <- ggplot(loglandis, aes(logdisfish, loglanfish)) + geom_point(size = 2, colour="darkcyan")
p1

nzc_lanfish <- log10(colSums(lanfish!=0)+10)
nzc_disfish <- log10(colSums(disfish!=0)+10)
nzc_landis <- data.frame(nzc_disfish, nzc_lanfish)
p2 <- ggplot(nzc_landis, aes(nzc_disfish, nzc_lanfish)) + geom_point(size = 2, colour="darkcyan")
p2

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)

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

summary(km_model)

#Geographical clusters

geolandis <- lanquant.co[c("lonIni", "latIni")]

#CV for number of geoclusters

max_clusters = 20

ratio_ss <- data.frame(cluster = seq(from = 1, to = max_clusters, by = 1)) 

for (k in 1:max_clusters) {
  
  km_model <- kmeans(subset(geolandis, select=c("lonIni", "latIni")), k, nstart=20)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
}

ggplot(ratio_ss, aes(cluster, ratio)) + 
  geom_line() +
  geom_point()

#Build geoclusters

nb_geoclusters = 6

km_model <- kmeans(subset(geolandis, select=c("lonIni", "latIni")), centers = nb_geoclusters)
geolandis$cluster <- km_model$cluster
ggplot(geolandis, aes(x=lonIni, y=latIni, col=factor(cluster))) + geom_point()

#Build data frame

data <- lanquant.co

data = subset(data, select=-c(1,2,3,5,6,7))

metrics <- data.frame(algo=character(), train_mse=double(), test_mse=double(), stringsAsFactors=FALSE)

#Add cluster to data frame

for (k in 1:nb_logclusters) {
  data[paste0("mass_cluster_", k)] <- rowSums(data[, rownames(loglandis[ loglandis$cluster == k ,])])
}
data["geocluster"] <- as.factor(geolandis$cluster)

#Add sums of landed and discarded fish

data$lanquant_sum <- rowSums(lanquant.co[8:157])
data$disquant_sum <- rowSums(disquant.co[8:157])

#Remove individual fish counts

data = subset(data, select=-c(2:151))

#Remove clusters of fish which are never landed

nonlan = which(colSums(data[ sapply(data, is.numeric) ])==0) + 1
nonlan
data = subset(data, select=-c(nonlan))

#Add cross, squared and log features
data_num = data[sapply(data, is.numeric)]
k = dim(data_num)[2]-1
colnames(data_num)[k]

for (i in 1:(k-1)) {
  for (j in (i+1):k) {
    data[paste0(colnames(data_num)[i], "*", colnames(data_num)[j])] <- data_num[i]*data_num[j]
  }
}

for (i in 2:k) {
  data[paste0(colnames(data_num)[i], "_sq")] <- data_num[i]^2
}

for (i in 2:k) {
  new_col = log(data_num[i])
  new_col[sapply(new_col, is.infinite)] <- 0
  data[paste0(colnames(data_num)[i], "_log")] <- new_col
}

data_num <- data[sapply(data, is.numeric)]

corrmat <- cor(data_num)
col<- colorRampPalette(c("blue", "white", "red"))(256)
heatmap(x = corrmat, col = col, symm = TRUE)

#Scale features

scaled_data <- data.frame(data$month, data$geocluster, 
                          lapply(data_num[ , !names(data_num)=="disquant_sum" ], function(x) c(scale(x))), data$disquant_sum)

#Build train and test sets

set.seed(55)

train_ind <- sample(seq_len(nrow(scaled_data)), size = floor(0.7 * nrow(scaled_data)))
train <- scaled_data[train_ind, ]
test <- scaled_data[-train_ind, ]

#Predict mean as a baseline
baseline = mean(train$data.disquant_sum)
baseline_train_MSE = sqrt(mean((train$data.disquant_sum - baseline)^2))
baseline_test_MSE = sqrt(mean((test$data.disquant_sum - baseline)^2))
metrics[nrow(metrics) + 1,] = list("baseline_mean", baseline_train_MSE, baseline_test_MSE)

#Linear model without cross terms

mod_1 <- lm(data.disquant_sum ~ ., data=train)
summary(mod_1)
lm_1_train_pred = rmse(mod_1, train)
lm_1_test_pred = rmse(mod_1, test)
metrics[nrow(metrics) + 1,] = list("lm_all", lm_1_train_pred, lm_1_test_pred)

mod_aic = step(mod_1)
summary(mod_aic)
lm_aic_train_pred = rmse(mod_aic, train)
lm_aic_test_pred = rmse(mod_aic, test)
metrics[nrow(metrics) + 1,] = list("lm_aic", lm_aic_train_pred, lm_aic_test_pred)

colnames(train[1:k])[1]
form <- "data.disquant_sum ~ "
for (i in 1:k) {
  form <- paste0(form, " + ", colnames(train)[i])
}

mod_2 <- lm(form, data=train)
summary(mod_2)
lm_2_train_pred = rmse(mod_2, train)
lm_2_test_pred = rmse(mod_2, test)
metrics[nrow(metrics) + 1,] = list("lm_simple", lm_2_train_pred, lm_2_test_pred)

#Lasso

fit <- glmnet(x=data.matrix(train[c(1:dim(train)[2]-1)]), y=data.matrix(train[c(dim(train)[2])]))
plot(fit, xvar="lambda", label=TRUE)
print(fit)
cvfit <- cv.glmnet(x=data.matrix(train[c(2:dim(train)[2]-1)]), y=data.matrix(train[c(dim(train)[2])]), relax=TRUE)
plot(cvfit)
print(cvfit)
lasso_train_pred = predict(cvfit, newx = data.matrix(train[c(2:dim(train)[2]-1)]), 
                           lambda = cvfit$lambda.min)
lasso_test_pred = predict(cvfit, newx = data.matrix(test[c(2:dim(train)[2]-1)]), 
                          lambda = cvfit$lambda.min)
lasso_train_MSE = sqrt(mean((train[[c(dim(train)[2])]] - lasso_train_pred)^2))
lasso_test_MSE = sqrt(mean((test[[c(dim(train)[2])]] - lasso_test_pred)^2))
coef(cvfit, s=cvfit$lambda.min)
metrics[nrow(metrics) + 1,] = list("lasso", lasso_train_MSE, lasso_test_MSE)

#Decision tree

tree_mod <- rpart(data.disquant_sum ~ ., data=train, control = rpart.control(minsplit = 20))
plotcp(tree_mod)
printcp(tree_mod)
tree_train_pred = rmse(tree_mod, train)
tree_test_pred = rmse(tree_mod, test)
prp(tree_mod)
metrics[nrow(metrics) + 1,] = list("tree", tree_train_pred, tree_test_pred)

rf_mod <- randomForest(data.disquant_sum ~ ., data=train, ntree=10000)
rf_train_pred = rmse(rf_mod, train)
rf_test_pred = rmse(rf_mod, test)
metrics[nrow(metrics) + 1,] = list("rf", rf_train_pred, rf_test_pred)
