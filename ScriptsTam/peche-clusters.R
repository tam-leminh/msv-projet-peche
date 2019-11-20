library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glmnet)
library(rpart)
library(rpart.plot)

#Keep only fish data

lanfish <- lanquant.co[c(8:157)]
disfish <- disquant.co[c(8:157)]

data <- lanquant.co

#CLUSTERING

#Clustering with logsums of landed and discarded fish

logdisfish <- log(colSums(disfish))
loglanfish <- log(colSums(lanfish))

logdisfish[is.infinite(logdisfish)] <- 0
loglanfish[is.infinite(loglanfish)] <- 0

loglandis <- data.frame(logdisfish, loglanfish)

plot(loglandis)

#CV for number of clusters

max_clusters = 20

ratio_ss <- data.frame(cluster = seq(from = 1, to = max_clusters, by = 1)) 

for (k in 1:max_clusters) {
  
  km_model <- kmeans(loglandis, k, nstart=20)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
}

ggplot(ratio_ss, aes(cluster, ratio)) + 
  geom_line() +
  geom_point()

#Build clusters

nb_clusters = 6

km_model <- kmeans(loglandis, centers = nb_clusters, nstart=20)
loglandis$cluster <- km_model$cluster
ggplot(loglandis, aes(logdisfish, loglanfish, col = factor(cluster))) + geom_point(size = 2)

summary(km_model)

#Build data frame

data <- lanquant.co

data = subset(data, select=-c(1,2,3,5,6,7))

#Add cluster masses to data frame

for (k in 1:nb_clusters) {
  data[paste0("mass_cluster_", k)] <- rowSums(data[, rownames(loglandis[ loglandis$cluster == k ,])])
}

#Add sums of landed and discarded fish

data$lanquant_sum <- rowSums(lanquant.co[8:157])
data$disquant_sum <- rowSums(disquant.co[8:157])

#Remove individual fish counts

data = subset(data, select=-c(2:151))

#Remove clusters of fish which are never landed

nonlan = which(colSums(data[c(2:dim(data)[2])])==0) + 1
nonlan
data = subset(data, select=-c(nonlan))

#Add cross, squared and log features

k = dim(data)[2]-1
colnames(data)[k]

for (i in 2:(k-1)) {
  for (j in (i+1):k) {
    data[paste0(colnames(data)[i], "*", colnames(data)[j])] <- data[i]*data[j]
  }
}

for (i in 2:k) {
  data[paste0(colnames(data)[i], "_sq")] <- data[i]^2
}

for (i in 2:k) {
  new_col = log(data[i])
  new_col[sapply(new_col, is.infinite)] <- 0
  data[paste0(colnames(data)[i], "_log")] <- new_col
}

data <- subset(data, select=c(1:k, (k+2):(dim(data)[2]), (k+1)))

#Scale features

scaled_data <- data.frame(data$month, lapply(data[c(2:(dim(data)[2]-1))], function(x) c(scale(x))), data$disquant_sum)
scaled_data$data.disquant_sum <- data$disquant_sum-mean(data$disquant_sum)

#Build train and test sets

set.seed(55)

train_ind <- sample(seq_len(nrow(scaled_data)), size = floor(0.7 * nrow(scaled_data)))
train <- scaled_data[train_ind, ]
test <- scaled_data[-train_ind, ]

#Predict mean as a baseline
baseline = mean(train$data.disquant_sum)
baseline_train_MSE = sqrt(mean((train$data.disquant_sum - baseline)^2))
baseline_test_MSE = sqrt(mean((test$data.disquant_sum - baseline)^2))

#Linear model without cross terms

mod_1 <- lm(data.disquant_sum ~ ., data=train)
summary(mod_1)
lm_1_train_pred = rmse(mod_1, train)
lm_1_test_pred = rmse(mod_1, test)

colnames(train[1:k])[1]
form <- "data.disquant_sum ~ "
for (i in 1:k) {
  form <- paste0(form, " + ", colnames(train)[i])
}

mod_2 <- lm(form, data=train)
summary(mod_2)
lm_2_train_pred = rmse(mod_2, train)
lm_2_test_pred = rmse(mod_2, test)

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

#Decision tree

tree_mod <- rpart(data.disquant_sum ~ ., data=train, control = rpart.control(minsplit = 20))
plotcp(tree_mod)
printcp(tree_mod)
tree_train_pred = rmse(tree_mod, train)
tree_test_pred = rmse(tree_mod, test)
prp(tree_mod)

