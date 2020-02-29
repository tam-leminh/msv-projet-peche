library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)
library(caTools)
library(glmnet)
library(cowplot)

par(mfrow=c(1,2))
months = table(lanquant.co$month)
barplot(months, xlab="Mois", ylab="Fréquence")
years = table(lanquant.co$year)
barplot(years, xlab="Année", ylab="Fréquence")

months
years

p1<-ggplot(data=lanquant.co, aes(x=month)) +
  geom_bar(stat="count", fill="darkorchid4")

p1

p2<-ggplot(data=lanquant.co, aes(x=year)) +
  geom_bar(stat="count", fill="darkolivegreen")

p2

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)

data <- lanquant.co

data$lanquant_sum <- rowSums(lanquant.co[8:157])
data$disquant_sum <- rowSums(disquant.co[8:157])

summary(data$lanquant_sum)
summary(data$disquant_sum)

data = subset(data, select=-c(1,2,3,5,6,7))


set.seed(51)
train_ind <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))

train <- data[train_ind, ]
test <- data[-train_ind, ]

baseline = mean(train$disquant_sum)
baseline_train_MSE = sqrt(mean((train$disquant_sum - baseline)^2))
baseline_test_MSE = sqrt(mean((test$disquant_sum - baseline)^2))

mod <- lm(disquant_sum ~ lanquant_sum, data=train)
summary(mod)
lm_train_pred = rmse(mod, train)
lm_test_pred = rmse(mod, test)

fit <- glmnet(x=data.matrix(train[c(2:152)]), y=data.matrix(train[c(153)]), lambda.min=0.05)
plot(fit, xvar="lambda", label=TRUE)
print(fit)
cvfit <- cv.glmnet(x=data.matrix(train[c(2:152)]), y=data.matrix(train[c(153)]), lambda.min=0.05)
plot(cvfit)
lasso_train_pred = predict(cvfit, newx = data.matrix(train[c(2:152)]), cvfit$lambda.min)
lasso_test_pred = predict(cvfit, newx = data.matrix(test[c(2:152)]), cvfit$lambda.min)
lasso_train_MSE = sqrt(mean((train[[c(153)]] - lasso_train_pred)^2))
lasso_test_MSE = sqrt(mean((test[[c(153)]] - lasso_test_pred)^2))

