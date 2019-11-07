library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glmnet)
library(rpart)
library(rpart.plot)

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

data <- lanquant.co

data$lanquant_sum <- rowSums(lanquant.co[8:157])
data$disquant_sum <- rowSums(disquant.co[8:157])
summary(data$disquant_sum)
normal_dis = data$disquant_sum/max(data$disquant_sum)
summary(normal_dis)
log(normal_dis)
hist(log(normal_dis+0.001))

data$log_disquant_sum <- log(1 + data$disquant_sum)

normal_logdis = data$log_disquant_sum/max(data$log_disquant_sum)

hist(normal_logdis)
logdisfish <- log(colSums(disfish))
loglanfish <- log(colSums(lanfish))

logdisfish[is.infinite(logdisfish)] <- 0
loglanfish[is.infinite(loglanfish)] <- 0

plot(logdisfish, loglanfish)

loglandis <- data.frame(logdisfish, loglanfish)

plot(loglandis)

ratio_ss <- data.frame(cluster = seq(from = 1, to = 20, by = 1)) 

for (k in 1:20) {
  
  km_model <- kmeans(loglandis, k, nstart=20)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
}

ggplot(ratio_ss, aes(cluster, ratio)) + 
  geom_line() +
  geom_point()

nb_clusters = 6

km_model <- kmeans(loglandis, centers = nb_clusters, nstart=20)
loglandis$cluster <- km_model$cluster
ggplot(loglandis, aes(logdisfish, loglanfish, col = factor(cluster))) + geom_point(size = 2)

summary(km_model)

loglandis

data <- lanquant.co

data = subset(data, select=-c(1,2,3,5,6,7))

for (k in 1:nb_clusters) {
  data[paste0("mass_cluster_", k)] <- rowSums(data[, rownames(loglandis[ loglandis$cluster == k ,])])
}

data$lanquant_sum <- rowSums(lanquant.co[8:157])
data$disquant_sum <- rowSums(disquant.co[8:157])

data = subset(data, select=-c(2:151))

nonlan = which(colSums(data[c(2:dim(data)[2])])==0) + 1
nonlan
data = subset(data, select=-c(nonlan))
scaled_data <- data.frame(data$month, lapply(data[c(2:dim(data)[2])], function(x) c(scale(x))))
scaled_data$disquant_sum <- data$disquant_sum

set.seed(55)
train_ind <- sample(seq_len(nrow(scaled_data)), size = floor(0.7 * nrow(scaled_data)))

train <- scaled_data[train_ind, ]
test <- scaled_data[-train_ind, ]

baseline = mean(train$disquant_sum)
baseline_train_MSE = sqrt(mean((train$disquant_sum - baseline)^2))
baseline_test_MSE = sqrt(mean((test$disquant_sum - baseline)^2))

mod <- lm(disquant_sum ~ ., data=train)
summary(mod)
lm_train_pred = rmse(mod, train)
lm_test_pred = rmse(mod, test)

fit <- glmnet(x=data.matrix(train[c(1:dim(train)[2]-1)]), y=data.matrix(train[c(dim(train)[2])]))
plot(fit, xvar="lambda", label=TRUE)
print(fit)
cvfit <- cv.glmnet(x=data.matrix(train[c(2:dim(train)[2]-1)]), y=data.matrix(train[c(dim(train)[2])]))
plot(cvfit)
lasso_train_pred = predict(cvfit, newx = data.matrix(train[c(2:dim(train)[2]-1)]), cvfit$lambda.min)
lasso_test_pred = predict(cvfit, newx = data.matrix(test[c(2:dim(train)[2]-1)]), cvfit$lambda.min)
lasso_train_MSE = sqrt(mean((train[[c(dim(train)[2])]] - lasso_train_pred)^2))
lasso_test_MSE = sqrt(mean((test[[c(dim(train)[2])]] - lasso_test_pred)^2))

tree_mod <- rpart(disquant_sum ~ ., data=train, control = rpart.control(minsplit = 20))
plotcp(tree_mod)
printcp(tree_mod)
tree_train_pred = rmse(tree_mod, train)
tree_test_pred = rmse(tree_mod, test)
prp(tree_mod)

