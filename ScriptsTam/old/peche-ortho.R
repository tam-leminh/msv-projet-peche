X = data.matrix(train[c(2:151)])
A = t(X) %*% X
diag(A) <- 0
diag(A)
A
sum(A)
nnzero(A)
nnzero(rowSums(A))
B = which(rowSums(A)!=0)

data = subset(data, select=-c(1,2,3,5,6,7))
O = subset(data, select=-c(B+1))

set.seed(51)
train_ind <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))

o_train <- O[train_ind, ]
o_test <- O[-train_ind, ]

baseline = mean(o_train$disquant_sum)
baseline_train_MSE = sqrt(mean((o_train$disquant_sum - baseline)^2))
baseline_test_MSE = sqrt(mean((o_test$disquant_sum - baseline)^2))

mod <- lm(disquant_sum ~ lanquant_sum, data=o_train)
summary(mod)
lm_train_pred = rmse(mod, o_train)
lm_test_pred = rmse(mod, o_test)

fit <- glmnet(x=data.matrix(o_train[c(1:70)]), y=data.matrix(o_train[c(71)]), lambda.min=0.05)
plot(fit, xvar="lambda", label=TRUE)
print(fit)
cvfit <- cv.glmnet(x=data.matrix(o_train[c(1:70)]), y=data.matrix(o_train[c(71)]), lambda.min=0.05)
plot(cvfit)
lasso_train_pred = predict(cvfit, newx = data.matrix(o_train[c(1:70)]), cvfit$lambda.min)
lasso_test_pred = predict(cvfit, newx = data.matrix(o_test[c(1:70)]), cvfit$lambda.min)
lasso_train_MSE = sqrt(mean((o_train[[c(71)]] - lasso_train_pred)^2))
lasso_test_MSE = sqrt(mean((o_test[[c(71)]] - lasso_test_pred)^2))
